-- | Best-effort extraction of a low-cardinality query summary, following the
-- @db.query.summary@ attribute of the OpenTelemetry semantic conventions for
-- database spans.
module Database.PostgreSQL.PQTypes.OpenTelemetry.QuerySummary
  ( QuerySummary (..)
  , renderQuerySummary
  , querySummaryCollection
  , querySummaryStoredProcedure
  , parseQuerySummary
  ) where

import Data.Char
import Data.Containers.ListUtils
import Data.IntMap.Strict qualified as IM
import Data.HashMap.Strict qualified as H
import Data.Set qualified as S
import Data.Text qualified as T

-- | Summary of a query: the operations it performs and the tables they target.
--
-- For example, the summary of
--
-- > INSERT INTO high_value_customers (id, name)
-- > SELECT id, name FROM customers WHERE total_spend > 10000
--
-- is
--
-- @
-- 'QuerySummary'
--   { 'qsOperationName' = Just \"INSERT\"
--   , 'qsOperations' = [(\"INSERT\", [\"high_value_customers\"]), (\"SELECT\", [\"customers\"])]
--   }
-- @
data QuerySummary = QuerySummary
  { qsOperationName :: !(Maybe T.Text)
  -- ^ The outermost operation of the query, e.g. @INSERT@ above.
  , qsOperations :: ![(T.Text, [T.Text])]
  -- ^ Operations of the query in the order they appear in it, each with the
  -- tables it targets.
  }
  deriving (Eq, Ord, Show)

-- | Render a 'QuerySummary' as text, e.g.
-- @INSERT high_value_customers SELECT customers@. The result is truncated to
-- 255 characters, as the OpenTelemetry semantic conventions recommend.
renderQuerySummary :: QuerySummary -> T.Text
renderQuerySummary =
  T.take 255 . T.unwords . concatMap (\(op, targets) -> op : targets) . qsOperations

-- | The only table the query targets, if there is exactly one.
querySummaryCollection :: QuerySummary -> Maybe T.Text
querySummaryCollection qs
  | Just _ <- querySummaryStoredProcedure qs = Nothing
  | otherwise = case nubOrd . concatMap snd $ qsOperations qs of
      [table] -> Just table
      _ -> Nothing

-- | The stored procedure that the query calls, e.g. @some_procedure@ of
-- @CALL some_procedure($1)@.
querySummaryStoredProcedure :: QuerySummary -> Maybe T.Text
querySummaryStoredProcedure qs = case qsOperations qs of
  [(op, [procedure])] | T.toUpper op == "CALL" -> Just procedure
  _ -> Nothing

----------------------------------------

data Token
  = TWord !T.Text !Keyword
  -- ^ An unquoted identifier or a keyword, as written and classified.
  | TQuoted !T.Text
  -- ^ A quoted identifier, including the quotes.
  | TOpen
  | TClose
  | TComma
  | TDot
  | TOther
  deriving (Eq, Show)

-- | Split a query into tokens. Comments, whitespace and the contents of string
-- literals are dropped.
tokenize :: T.Text -> [Token]
tokenize t = case T.uncons t of
  Nothing -> []
  Just (c, rest)
    | isSpace c -> tokenize $ T.dropWhile isSpace rest
    | c == '-', Just ('-', rest') <- T.uncons rest -> tokenize $ T.dropWhile (/= '\n') rest'
    | c == '/', Just ('*', rest') <- T.uncons rest -> tokenize $ skipBlockComment (1 :: Int) rest'
    | c == '\'' -> TOther : tokenize (skipString False rest)
    | c `elem` ['e', 'E'], Just ('\'', rest') <- T.uncons rest -> TOther : tokenize (skipString True rest')
    | c == '"' -> case spanQuoted rest of
        (ident, rest') -> TQuoted ("\"" <> ident <> "\"") <:> rest'
    | c == '$', Just tag <- dollarTag rest -> TOther : tokenize (skipDollarString tag rest)
    | isAlpha c || c == '_' -> case T.span isIdentChar t of
        (word, rest') -> TWord word (classifyWord word) <:> rest'
    | c == '(' -> TOpen : tokenize rest
    | c == ')' -> TClose : tokenize rest
    | c == ',' -> TComma : tokenize rest
    | c == '.' -> TDot : tokenize rest
    | isDigit c || c == '$' -> TOther : tokenize (T.dropWhile isIdentChar rest)
    | otherwise -> TOther : tokenize rest
  where
    -- Tokens are evaluated at once, so that they don't build up thunks. The rest
    -- is lazy, so that the parser consumes tokens as they are produced.
    tok <:> rest = tok `seq` (tok : tokenize rest)
    infixr 5 <:>

    -- ASCII is checked first, as Unicode properties are slow to look up.
    isIdentChar x
      | isAscii x = isAsciiLower x || isAsciiUpper x || isDigit x || x == '_' || x == '$'
      | otherwise = isAlphaNum x

    -- Block comments can be nested.
    skipBlockComment depth s0 = case T.uncons $ T.dropWhile (\x -> x /= '*' && x /= '/') s0 of
      Nothing -> T.empty
      Just ('*', s')
        | Just ('/', s'') <- T.uncons s' ->
            if depth == 1 then s'' else skipBlockComment (depth - 1) s''
      Just ('/', s')
        | Just ('*', s'') <- T.uncons s' -> skipBlockComment (depth + 1) s''
      Just (_, s') -> skipBlockComment depth s'

    -- Skip the rest of a string literal. Two consecutive quotes are an escaped
    -- quote, and in an escape string a backslash escapes the next character.
    skipString escapes s0 = case T.uncons $ T.dropWhile (\x -> x /= '\'' && x /= '\\') s0 of
      Nothing -> T.empty
      Just ('\\', s') | escapes -> skipString escapes (T.drop 1 s')
      Just ('\'', s')
        | Just ('\'', s'') <- T.uncons s' -> skipString escapes s''
        | otherwise -> s'
      Just (_, s') -> skipString escapes s'

    spanQuoted s =
      let (ident, s') = T.break (== '"') s
      in case T.uncons s' of
          Just ('"', s'')
            | Just ('"', s''') <- T.uncons s'' ->
                let (ident', rest) = spanQuoted s'''
                in (ident <> "\"\"" <> ident', rest)
            | otherwise -> (ident, s'')
          _ -> (ident, T.empty)

    -- The tag of a dollar-quoted string, e.g. @$tag$@ or @$$@. Parameter
    -- placeholders such as @$1@ are not tags.
    dollarTag s =
      let (tag, s') = T.span (\x -> isAlphaNum x || x == '_') s
      in case T.uncons s' of
          Just ('$', _)
            | T.null tag || not (isDigit $ T.head tag) -> Just $ "$" <> tag <> "$"
          _ -> Nothing

    skipDollarString tag s =
      let body = T.drop (T.length tag - 1) s
      in T.drop (T.length tag) . snd $ T.breakOn tag body

----------------------------------------

-- | Keywords that the parser recognizes. Words are classified once by the
-- tokenizer, so that the parser compares constructors instead of text.
data Keyword
  = NotKeyword
  | K_ALTER
  | K_AS
  | K_CALL
  | K_CONCURRENTLY
  | K_CONFLICT
  | K_CREATE
  | K_CROSS
  | K_DEFAULT
  | K_DELETE
  | K_DISTINCT
  | K_DO
  | K_DROP
  | K_EXCEPT
  | K_EXISTS
  | K_FETCH
  | K_FOR
  | K_FROM
  | K_FULL
  | K_GLOBAL
  | K_GROUP
  | K_HAVING
  | K_IF
  | K_INNER
  | K_INSERT
  | K_INTERSECT
  | K_INTO
  | K_JOIN
  | K_KEY
  | K_LATERAL
  | K_LEFT
  | K_LIMIT
  | K_LOCAL
  | K_LOCK
  | K_MATERIALIZED
  | K_MERGE
  | K_NATURAL
  | K_NO
  | K_NOT
  | K_OFFSET
  | K_ON
  | K_ONLY
  | K_OR
  | K_ORDER
  | K_OUTER
  | K_RECURSIVE
  | K_REPLACE
  | K_RETURNING
  | K_RIGHT
  | K_SELECT
  | K_SET
  | K_SHARE
  | K_TABLE
  | K_TEMP
  | K_TEMPORARY
  | K_THEN
  | K_TRUNCATE
  | K_UNION
  | K_UNIQUE
  | K_UNLOGGED
  | K_UPDATE
  | K_USING
  | K_VALUES
  | K_WHEN
  | K_WHERE
  | K_WINDOW
  | K_WITH
  deriving (Eq, Ord, Enum, Bounded, Show)

classifyWord :: T.Text -> Keyword
classifyWord w
  -- No keyword is longer, so long identifiers skip upper-casing.
  | T.compareLength w maxKeywordLength == GT = NotKeyword
  -- The map has upper-cased and lower-cased keywords, so only words in mixed
  -- case need to be converted.
  | Just k <- H.lookup w keywordMap = k
  | T.any isAsciiUpper w && T.any isAsciiLower w =
      H.lookupDefault NotKeyword (T.toUpper w) keywordMap
  | otherwise = NotKeyword

keywordMap :: H.HashMap T.Text Keyword
keywordMap =
  H.fromList
    [ (caseOf name, k)
    | k <- [succ NotKeyword ..]
    , let name = T.pack . drop 2 $ show k
    , caseOf <- [id, T.toLower]
    ]
{-# NOINLINE keywordMap #-}

maxKeywordLength :: Int
maxKeywordLength = maximum . map T.length $ H.keys keywordMap
{-# NOINLINE maxKeywordLength #-}

-- | Keywords that end a list of tables, e.g. @WHERE@.
isClauseKeyword :: Keyword -> Bool
isClauseKeyword = \case
  K_WHERE -> True
  K_GROUP -> True
  K_HAVING -> True
  K_WINDOW -> True
  K_ORDER -> True
  K_LIMIT -> True
  K_OFFSET -> True
  K_FETCH -> True
  K_FOR -> True
  K_RETURNING -> True
  K_SET -> True
  K_UNION -> True
  K_INTERSECT -> True
  K_EXCEPT -> True
  K_WHEN -> True
  K_CONFLICT -> True
  K_DEFAULT -> True
  _ -> False

-- | Keywords that can't be a name of a table.
isReservedKeyword :: Keyword -> Bool
isReservedKeyword k = isClauseKeyword k || case k of
  K_SELECT -> True
  K_INSERT -> True
  K_UPDATE -> True
  K_DELETE -> True
  K_MERGE -> True
  K_VALUES -> True
  K_FROM -> True
  K_JOIN -> True
  K_USING -> True
  K_INTO -> True
  K_ON -> True
  K_AS -> True
  K_ONLY -> True
  K_LATERAL -> True
  K_WITH -> True
  K_RECURSIVE -> True
  K_LEFT -> True
  K_RIGHT -> True
  K_INNER -> True
  K_OUTER -> True
  K_FULL -> True
  K_CROSS -> True
  K_NATURAL -> True
  _ -> False

----------------------------------------

-- | State of a parenthesized level of the query.
data Frame = Frame
  { frStatement :: !Bool
  -- ^ Whether an operation appeared at this level, so that keywords such as
  -- @FROM@ introduce tables (unlike e.g. in @EXTRACT(YEAR FROM ts)@).
  , frInFrom :: !Bool
  -- ^ Whether the level is in a @FROM@ clause, so that a comma introduces
  -- another table.
  , frExpect :: !(Maybe Expect)
  -- ^ Whether a table name comes next.
  , frWith :: !Bool
  -- ^ Whether the level is in a @WITH@ clause, before its main statement.
  , frExpectCte :: !Bool
  -- ^ Whether the name of a common table expression comes next.
  , frOperation :: !(Maybe Int)
  -- ^ Position of the operation that the tables of this level belong to.
  }

data Expect
  = ExpectFromItem
  -- ^ An item of a @FROM@ clause. A name followed by a parenthesis is a
  -- function, not a table.
  | ExpectTarget
  -- ^ The target of e.g. @INSERT INTO@ or @UPDATE@.
  | ExpectUsing
  -- ^ A table after @USING@ of @DELETE@ or @MERGE@, or the parenthesized
  -- columns of a join.
  deriving (Eq)

emptyFrame :: Frame
emptyFrame = Frame False False Nothing False False Nothing

data ParseState = ParseState
  { psFrames :: ![Frame]
  , psOperations :: !(IM.IntMap (T.Text, [T.Text]))
  -- ^ Operations by their position, each with targets in reverse order.
  , psOperationCount :: !Int
  -- ^ Number of operations, i.e. the position of the next one.
  , psOperationName :: !(Maybe T.Text)
  , psCtes :: !(S.Set T.Text)
  , psPrevious :: !Keyword
  -- ^ The previous keyword.
  }

-- | Extract a 'QuerySummary' from the text of a query on a best-effort basis.
-- The parser recognizes common statements and the tables or stored procedures
-- they target, and ignores everything else. Operations and targets are kept as
-- written, without case normalization, as the OpenTelemetry semantic
-- conventions recommend.
parseQuerySummary :: T.Text -> QuerySummary
parseQuerySummary query = case tokenize query of
  [] -> QuerySummary Nothing []
  tokens@(TWord w k : rest)
    | k `elem` dmlKeywords -> parseDml tokens
    | k `elem` [K_CREATE, K_DROP, K_ALTER] -> parseDdl w rest
    | k `elem` [K_TRUNCATE, K_LOCK] -> simple w . readNames $ dropKeywords [K_TABLE, K_ONLY] rest
    -- A stored procedure, e.g. CALL some_procedure($1).
    | k == K_CALL -> simple w . maybe [] (pure . fst) $ readName rest
    | otherwise -> simple w []
  tokens -> parseDml tokens
  where
    simple op targets = QuerySummary (Just op) [(op, targets)]

    dmlKeywords = [K_SELECT, K_INSERT, K_UPDATE, K_DELETE, K_MERGE, K_WITH, K_VALUES, K_TABLE]

    parseDdl op tokens =
      let tokens' = dropKeywords ddlModifiers tokens
      in case tokens' of
          TWord object _ : rest ->
            let op' = op <> " " <> object
                rest' = dropKeywords [K_CONCURRENTLY, K_IF, K_NOT, K_EXISTS, K_ONLY] rest
                targets = case rest' of
                  -- An index without a name, e.g. CREATE INDEX ON table.
                  TWord _ K_ON : rest'' -> readNames rest''
                  _ -> readNames rest'
                QuerySummary _ ops = case dropUntilAs rest' of
                  [] -> QuerySummary Nothing []
                  body -> parseDml body
            in QuerySummary (Just op') ((op', targets) : ops)
          _ -> simple op []

    ddlModifiers =
      [K_OR, K_REPLACE, K_TEMP, K_TEMPORARY, K_UNLOGGED, K_UNIQUE, K_MATERIALIZED, K_GLOBAL, K_LOCAL]

    -- The query of e.g. CREATE TABLE ... AS SELECT.
    dropUntilAs = go (0 :: Int)
      where
        go depth = \case
          [] -> []
          TOpen : rest -> go (depth + 1) rest
          TClose : rest -> go (depth - 1) rest
          TWord _ K_AS : rest | depth == 0 -> rest
          _ : rest -> go depth rest

dropKeywords :: [Keyword] -> [Token] -> [Token]
dropKeywords keywords = \case
  TWord _ k : rest | k `elem` keywords -> dropKeywords keywords rest
  tokens -> tokens

-- | Read comma separated names.
readNames :: [Token] -> [T.Text]
readNames tokens = case readName tokens of
  Just (name, TComma : rest) -> name : readNames rest
  Just (name, _) -> [name]
  Nothing -> []

-- | Read a possibly qualified name, e.g. @schema.table@.
readName :: [Token] -> Maybe (T.Text, [Token])
readName tokens = case tokens of
  TWord w _ : rest -> Just $ go w rest
  TQuoted w : rest -> Just $ go w rest
  _ -> Nothing
  where
    go name = \case
      TDot : TWord w _ : rest -> go (name <> "." <> w) rest
      TDot : TQuoted w : rest -> go (name <> "." <> w) rest
      rest -> (name, rest)

parseDml :: [Token] -> QuerySummary
parseDml = finish . go initialState
  where
    initialState = ParseState [emptyFrame] IM.empty 0 Nothing S.empty NotKeyword

    finish ps =
      QuerySummary
        { qsOperationName = case psOperationName ps of
            Nothing -> fst . snd <$> IM.lookupMin (psOperations ps)
            name -> name
        , qsOperations = map (fmap (nubOrd . reverse)) . IM.elems $ psOperations ps
        }

    go :: ParseState -> [Token] -> ParseState
    go ps = \case
      [] -> ps
      TOpen : rest ->
        let frame = case top ps of
              -- A parenthesized item of a FROM clause, e.g. a join or a
              -- subquery, continues the clause.
              Just f
                | frExpect f == Just ExpectFromItem ->
                    emptyFrame
                      { frStatement = True
                      , frInFrom = True
                      , frExpect = Just ExpectFromItem
                      , frOperation = frOperation f
                      }
              _ -> emptyFrame
            frames = frame : modifyTop (\f -> f {frExpect = Nothing}) (psFrames ps)
        in go (ps {psFrames = frames, psPrevious = NotKeyword}) rest
      TClose : rest -> case psFrames ps of
        _ : frames@(_ : _) -> go (ps {psFrames = frames, psPrevious = NotKeyword}) rest
        _ -> go ps rest
      TComma : rest -> go (withTop ps comma) rest
      tokens@(t : _)
        | Just f <- top ps
        , frExpectCte f
        , not (isKeyword t)
        , Just (name, rest') <- readName tokens ->
            go (withTop ps (\fr -> fr {frExpectCte = False})) {psCtes = S.insert (T.toLower name) (psCtes ps)} rest'
        | Just f <- top ps
        , Just expect <- frExpect f
        , Just (name, rest') <- readName tokens
        , not (isKeyword t) ->
            let isFunction = expect == ExpectFromItem && take 1 rest' == [TOpen]
                ps' = withTop ps (\fr -> fr {frExpect = Nothing})
            in go (if isFunction then ps' else addTarget name ps') rest'
      TWord w k : rest -> go (keyword w k ps) {psPrevious = k} rest
      _ : rest -> go ps {psPrevious = NotKeyword} rest

    comma f
      | frWith f && not (frStatement f) = f {frExpectCte = True}
      | frInFrom f = f {frExpect = Just ExpectFromItem}
      | otherwise = f

    -- The word as written and classified.
    keyword w k ps = case k of
      K_WITH | onTop (not . frStatement) -> withTop ps (\f -> f {frWith = True, frExpectCte = True})
      K_RECURSIVE -> ps
      _
        | k `elem` [K_SELECT, K_INSERT, K_UPDATE, K_DELETE, K_MERGE, K_VALUES, K_TABLE]
        , psPrevious ps `notElem` [K_FOR, K_KEY, K_DO, K_THEN, K_ON, K_SHARE, K_NO] ->
            operation w k ps
      -- IS [NOT] DISTINCT FROM is a comparison.
      _
        | (k == K_FROM || k == K_JOIN) && onTop frStatement && psPrevious ps /= K_DISTINCT ->
            withTop ps (\f -> f {frInFrom = True, frExpect = Just ExpectFromItem})
      K_USING
        | onTop frStatement ->
            withTop ps (\f -> f {frInFrom = True, frExpect = Just ExpectUsing})
      K_INTO
        | onTop frStatement ->
            withTop ps (\f -> f {frExpect = Just ExpectTarget})
      K_ONLY -> ps
      K_LATERAL -> ps
      _
        | isClauseKeyword k ->
            withTop ps (\f -> f {frInFrom = False, frExpect = Nothing})
      -- Anything else, e.g. an alias or ON of a join, ends the table name.
      _ -> withTop ps (\f -> f {frExpect = Nothing})
      where
        onTop p = maybe False p (top ps)

    operation w k ps =
      let outermost = case psFrames ps of
            [_] -> True
            _ -> False
          expect = if k == K_UPDATE || k == K_TABLE then Just ExpectTarget else Nothing
          ps' =
            withTop
              ps
                { psOperations = IM.insert (psOperationCount ps) (w, []) (psOperations ps)
                , psOperationCount = psOperationCount ps + 1
                , psOperationName = case psOperationName ps of
                    Nothing | outermost -> Just w
                    name -> name
                }
              ( \f ->
                  f
                    { frStatement = True
                    , frInFrom = False
                    , frExpect = expect
                    , frWith = False
                    , frExpectCte = False
                    , frOperation = Just $ psOperationCount ps
                    }
              )
      -- VALUES of e.g. INSERT is not a separate operation.
      in if k == K_VALUES && maybe False frStatement (top ps) then ps else ps'

    -- Add a table to the operation of the current level.
    addTarget name ps
      | T.toLower name `S.member` psCtes ps = ps
      | Just i <- frOperation =<< top ps =
          ps {psOperations = IM.adjust (fmap (name :)) i (psOperations ps)}
      | otherwise = ps

    top = \ps -> case psFrames ps of
      f : _ -> Just f
      [] -> Nothing

    withTop ps f = ps {psFrames = modifyTop f (psFrames ps)}

    modifyTop f = \case
      fr : frames -> f fr : frames
      [] -> []

    isKeyword = \case
      TWord _ k -> isReservedKeyword k
      _ -> False
