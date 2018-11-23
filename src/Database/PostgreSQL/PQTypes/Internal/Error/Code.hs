module Database.PostgreSQL.PQTypes.Internal.Error.Code (
    ErrorCode(..)
  , stringToErrorCode
  ) where

-- | SQL error code. Reference:
-- <http://www.postgresql.org/docs/devel/static/errcodes-appendix.html>.
data ErrorCode
  -- Class 00 — Successful Completion
  = SuccessfulCompletion
  -- Class 01 — Warning
  | Warning
  | DynamicResultSetsReturned
  | ImplicitZeroBitPadding
  | NullValueEliminatedInSetFunction
  | PrivilegeNotGranted
  | PrivilegeNotRevoked
  | StringDataRightTruncation_01
  | DeprecatedFeature
  -- Class 02 — No Data (this is also a warning class per the SQL standard)
  | NoData
  | NoAdditionalDynamicResultSetsReturned
  -- Class 03 — SQL Statement Not Yet Complete
  | SqlStatementNotYetComplete
  -- Class 08 — Connection Exception
  | ConnectionException
  | ConnectionDoesNotExist
  | ConnectionFailure
  | SqlclientUnableToEstablishSqlconnection
  | SqlserverRejectedEstablishmentOfSqlconnection
  | TransactionResolutionUnknown
  | ProtocolViolation
  -- Class 09 — Triggered Action Exception
  | TriggeredActionException
  -- Class 0A — Feature Not Supported
  | FeatureNotSupported
  -- Class 0B — Invalid Transaction Initiation
  | InvalidTransactionInitiation
  -- Class 0F — Locator Exception
  | LocatorException
  | InvalidLocatorSpecification
  -- Class 0L — Invalid Grantor
  | InvalidGrantor
  | InvalidGrantOperation
  -- Class 0P — Invalid Role Specification
  | InvalidRoleSpecification
  -- Class 0Z — Diagnostics Exception
  | DiagnosticsException
  | StackedDiagnosticsAccessedWithoutActiveHandler
  -- Class 20 — Case Not Found
  | CaseNotFound
  -- Class 21 — Cardinality Violation
  | CardinalityViolation
  -- Class 22 — Data Exception
  | DataException
  | ArraySubscriptError
  | CharacterNotInRepertoire
  | DatetimeFieldOverflow
  | DivisionByZero
  | ErrorInAssignment
  | EscapeCharacterConflict
  | IndicatorOverflow
  | IntervalFieldOverflow
  | InvalidArgumentForLogarithm
  | InvalidArgumentForNtileFunction
  | InvalidArgumentForNthValueFunction
  | InvalidArgumentForPowerFunction
  | InvalidArgumentForWidthBucketFunction
  | InvalidCharacterValueForCast
  | InvalidDatetimeFormat
  | InvalidEscapeCharacter
  | InvalidEscapeOctet
  | InvalidEscapeSequence
  | NonstandardUseOfEscapeCharacter
  | InvalidIndicatorParameterValue
  | InvalidParameterValue
  | InvalidRegularExpression
  | InvalidRowCountInLimitClause
  | InvalidRowCountInResultOffsetClause
  | InvalidTimeZoneDisplacementValue
  | InvalidUseOfEscapeCharacter
  | MostSpecificTypeMismatch
  | NullValueNotAllowed_22
  | NullValueNoIndicatorParameter
  | NumericValueOutOfRange
  | StringDataLengthMismatch
  | StringDataRightTruncation_22
  | SubstringError
  | TrimError
  | UnterminatedCString
  | ZeroLengthCharacterString
  | FloatingPointException
  | InvalidTextRepresentation
  | InvalidBinaryRepresentation
  | BadCopyFileFormat
  | UntranslatableCharacter
  | NotAnXmlDocument
  | InvalidXmlDocument
  | InvalidXmlContent
  | InvalidXmlComment
  | InvalidXmlProcessingInstruction
  -- Class 23 — Integrity Constraint Violation
  | IntegrityConstraintViolation
  | RestrictViolation
  | NotNullViolation
  | ForeignKeyViolation
  | UniqueViolation
  | CheckViolation
  | ExclusionViolation
  -- Class 24 — Invalid Cursor State
  | InvalidCursorState
  -- Class 25 — Invalid Transaction State
  | InvalidTransactionState
  | ActiveSqlTransaction
  | BranchTransactionAlreadyActive
  | HeldCursorRequiresSameIsolationLevel
  | InappropriateAccessModeForBranchTransaction
  | InappropriateIsolationLevelForBranchTransaction
  | NoActiveSqlTransactionForBranchTransaction
  | ReadOnlySqlTransaction
  | SchemaAndDataStatementMixingNotSupported
  | NoActiveSqlTransaction
  | InFailedSqlTransaction
  -- Class 26 — Invalid SQL Statement Name
  | InvalidSqlStatementName
  -- Class 27 — Triggered Data Change Violation
  | TriggeredDataChangeViolation
  -- Class 28 — Invalid Authorization Specification
  | InvalidAuthorizationSpecification
  | InvalidPassword
  -- Class 2B — Dependent Privilege Descriptors Still Exist
  | DependentPrivilegeDescriptorsStillExist
  | DependentObjectsStillExist
  -- Class 2D — Invalid Transaction Termination
  | InvalidTransactionTermination
  -- Class 2F — SQL Routine Exception
  | SqlRoutineException
  | FunctionExecutedNoReturnStatement
  | ModifyingSqlDataNotPermitted_2F
  | ProhibitedSqlStatementAttempted_2F
  | ReadingSqlDataNotPermitted_2F
  -- Class 34 — Invalid Cursor Name
  | InvalidCursorName
  -- Class 38 — External Routine Exception
  | ExternalRoutineException
  | ContainingSqlNotPermitted
  | ModifyingSqlDataNotPermitted_38
  | ProhibitedSqlStatementAttempted_38
  | ReadingSqlDataNotPermitted_38
  -- Class 39 — External Routine Invocation Exception
  | ExternalRoutineInvocationException
  | InvalidSqlstateReturned
  | NullValueNotAllowed_39
  | TriggerProtocolViolated
  | SrfProtocolViolated
  -- Class 3B — Savepoint Exception
  | SavepointException
  | InvalidSavepointSpecification
  -- Class 3D — Invalid Catalog Name
  | InvalidCatalogName
  -- Class 3F — Invalid Schema Name
  | InvalidSchemaName
  -- Class 40 — Transaction Rollback
  | TransactionRollback
  | TransactionIntegrityConstraintViolation
  | SerializationFailure
  | StatementCompletionUnknown
  | DeadlockDetected
  -- Class 42 — Syntax Error or Access Rule Violation
  | SyntaxErrorOrAccessRuleViolation
  | SyntaxError
  | InsufficientPrivilege
  | CannotCoerce
  | GroupingError
  | WindowingError
  | InvalidRecursion
  | InvalidForeignKey
  | InvalidName
  | NameTooLong
  | ReservedName
  | DatatypeMismatch
  | IndeterminateDatatype
  | CollationMismatch
  | IndeterminateCollation
  | WrongObjectType
  | UndefinedColumn
  | UndefinedFunction
  | UndefinedTable
  | UndefinedParameter
  | UndefinedObject
  | DuplicateColumn
  | DuplicateCursor
  | DuplicateDatabase
  | DuplicateFunction
  | DuplicatePreparedStatement
  | DuplicateSchema
  | DuplicateTable
  | DuplicateAlias
  | DuplicateObject
  | AmbiguousColumn
  | AmbiguousFunction
  | AmbiguousParameter
  | AmbiguousAlias
  | InvalidColumnReference
  | InvalidColumnDefinition
  | InvalidCursorDefinition
  | InvalidDatabaseDefinition
  | InvalidFunctionDefinition
  | InvalidPreparedStatementDefinition
  | InvalidSchemaDefinition
  | InvalidTableDefinition
  | InvalidObjectDefinition
  -- Class 44 — WITH CHECK OPTION Violation
  | WithCheckOptionViolation
  -- Class 53 — Insufficient Resources
  | InsufficientResources
  | DiskFull
  | OutOfMemory
  | TooManyConnections
  | ConfigurationLimitExceeded
  -- Class 54 — Program Limit Exceeded
  | ProgramLimitExceeded
  | StatementTooComplex
  | TooManyColumns
  | TooManyArguments
  -- Class 55 — Object Not In Prerequisite State
  | ObjectNotInPrerequisiteState
  | ObjectInUse
  | CantChangeRuntimeParam
  | LockNotAvailable
  -- Class 57 — Operator Intervention
  | OperatorIntervention
  | QueryCanceled
  | AdminShutdown
  | CrashShutdown
  | CannotConnectNow
  | DatabaseDropped
  -- Class 58 — System Error (errors external to PostgreSQL itself)
  | SystemError
  | IoError
  | UndefinedFile
  | DuplicateFile
  -- Class F0 — Configuration File Error
  | ConfigFileError
  | LockFileExists
  -- Class HV — Foreign Data Wrapper Error (SQL/MED)
  | FdwError
  | FdwColumnNameNotFound
  | FdwDynamicParameterValueNeeded
  | FdwFunctionSequenceError
  | FdwInconsistentDescriptorInformation
  | FdwInvalidAttributeValue
  | FdwInvalidColumnName
  | FdwInvalidColumnNumber
  | FdwInvalidDataType
  | FdwInvalidDataTypeDescriptors
  | FdwInvalidDescriptorFieldIdentifier
  | FdwInvalidHandle
  | FdwInvalidOptionIndex
  | FdwInvalidOptionName
  | FdwInvalidStringLengthOrBufferLength
  | FdwInvalidStringFormat
  | FdwInvalidUseOfNullPointer
  | FdwTooManyHandles
  | FdwOutOfMemory
  | FdwNoSchemas
  | FdwOptionNameNotFound
  | FdwReplyHandle
  | FdwSchemaNotFound
  | FdwTableNotFound
  | FdwUnableToCreateExecution
  | FdwUnableToCreateReply
  | FdwUnableToEstablishConnection
  -- Class P0 — PL/pgSQL Error
  | PlpgsqlError
  | RaiseException
  | NoDataFound
  | TooManyRows
  -- Class XX — Internal Error
  | InternalError
  | DataCorrupted
  | IndexCorrupted
  -- Unknown error code
  | UnknownErrorCode String
  deriving (Eq, Ord, Show)

-- | Convert 'String' to corresponding 'ErrorCode'.
stringToErrorCode :: String -> ErrorCode
stringToErrorCode code = case code of
  -- Class 00 — Successful Completion
  "00000" -> SuccessfulCompletion
  -- Class 01 — Warning
  "01000" -> Warning
  "0100C" -> DynamicResultSetsReturned
  "01008" -> ImplicitZeroBitPadding
  "01003" -> NullValueEliminatedInSetFunction
  "01007" -> PrivilegeNotGranted
  "01006" -> PrivilegeNotRevoked
  "01004" -> StringDataRightTruncation_01
  "01P01" -> DeprecatedFeature
  -- Class 02 — No Data (this is also a warning class per the SQL standard)
  "02000" -> NoData
  "02001" -> NoAdditionalDynamicResultSetsReturned
  -- Class 03 — SQL Statement Not Yet Complete
  "03000" -> SqlStatementNotYetComplete
  -- Class 08 — Connection Exception
  "08000" -> ConnectionException
  "08003" -> ConnectionDoesNotExist
  "08006" -> ConnectionFailure
  "08001" -> SqlclientUnableToEstablishSqlconnection
  "08004" -> SqlserverRejectedEstablishmentOfSqlconnection
  "08007" -> TransactionResolutionUnknown
  "08P01" -> ProtocolViolation
  -- Class 09 — Triggered Action Exception
  "09000" -> TriggeredActionException
  -- Class 0A — Feature Not Supported
  "0A000" -> FeatureNotSupported
  -- Class 0B — Invalid Transaction Initiation
  "0B000" -> InvalidTransactionInitiation
  -- Class 0F — Locator Exception
  "0F000" -> LocatorException
  "0F001" -> InvalidLocatorSpecification
  -- Class 0L — Invalid Grantor
  "0L000" -> InvalidGrantor
  "0LP01" -> InvalidGrantOperation
  -- Class 0P — Invalid Role Specification
  "0P000" -> InvalidRoleSpecification
  -- Class 0Z — Diagnostics Exception
  "0Z000" -> DiagnosticsException
  "0Z002" -> StackedDiagnosticsAccessedWithoutActiveHandler
  -- Class 20 — Case Not Found
  "20000" -> CaseNotFound
  -- Class 21 — Cardinality Violation
  "21000" -> CardinalityViolation
  -- Class 22 — Data Exception
  "22000" -> DataException
  "2202E" -> ArraySubscriptError
  "22021" -> CharacterNotInRepertoire
  "22008" -> DatetimeFieldOverflow
  "22012" -> DivisionByZero
  "22005" -> ErrorInAssignment
  "2200B" -> EscapeCharacterConflict
  "22022" -> IndicatorOverflow
  "22015" -> IntervalFieldOverflow
  "2201E" -> InvalidArgumentForLogarithm
  "22014" -> InvalidArgumentForNtileFunction
  "22016" -> InvalidArgumentForNthValueFunction
  "2201F" -> InvalidArgumentForPowerFunction
  "2201G" -> InvalidArgumentForWidthBucketFunction
  "22018" -> InvalidCharacterValueForCast
  "22007" -> InvalidDatetimeFormat
  "22019" -> InvalidEscapeCharacter
  "2200D" -> InvalidEscapeOctet
  "22025" -> InvalidEscapeSequence
  "22P06" -> NonstandardUseOfEscapeCharacter
  "22010" -> InvalidIndicatorParameterValue
  "22023" -> InvalidParameterValue
  "2201B" -> InvalidRegularExpression
  "2201W" -> InvalidRowCountInLimitClause
  "2201X" -> InvalidRowCountInResultOffsetClause
  "22009" -> InvalidTimeZoneDisplacementValue
  "2200C" -> InvalidUseOfEscapeCharacter
  "2200G" -> MostSpecificTypeMismatch
  "22004" -> NullValueNotAllowed_22
  "22002" -> NullValueNoIndicatorParameter
  "22003" -> NumericValueOutOfRange
  "22026" -> StringDataLengthMismatch
  "22001" -> StringDataRightTruncation_22
  "22011" -> SubstringError
  "22027" -> TrimError
  "22024" -> UnterminatedCString
  "2200F" -> ZeroLengthCharacterString
  "22P01" -> FloatingPointException
  "22P02" -> InvalidTextRepresentation
  "22P03" -> InvalidBinaryRepresentation
  "22P04" -> BadCopyFileFormat
  "22P05" -> UntranslatableCharacter
  "2200L" -> NotAnXmlDocument
  "2200M" -> InvalidXmlDocument
  "2200N" -> InvalidXmlContent
  "2200S" -> InvalidXmlComment
  "2200T" -> InvalidXmlProcessingInstruction
  -- Class 23 — Integrity Constraint Violation
  "23000" -> IntegrityConstraintViolation
  "23001" -> RestrictViolation
  "23502" -> NotNullViolation
  "23503" -> ForeignKeyViolation
  "23505" -> UniqueViolation
  "23514" -> CheckViolation
  "23P01" -> ExclusionViolation
  -- Class 24 — Invalid Cursor State
  "24000" -> InvalidCursorState
  -- Class 25 — Invalid Transaction State
  "25000" -> InvalidTransactionState
  "25001" -> ActiveSqlTransaction
  "25002" -> BranchTransactionAlreadyActive
  "25008" -> HeldCursorRequiresSameIsolationLevel
  "25003" -> InappropriateAccessModeForBranchTransaction
  "25004" -> InappropriateIsolationLevelForBranchTransaction
  "25005" -> NoActiveSqlTransactionForBranchTransaction
  "25006" -> ReadOnlySqlTransaction
  "25007" -> SchemaAndDataStatementMixingNotSupported
  "25P01" -> NoActiveSqlTransaction
  "25P02" -> InFailedSqlTransaction
  -- Class 26 — Invalid SQL Statement Name
  "26000" -> InvalidSqlStatementName
  -- Class 27 — Triggered Data Change Violation
  "27000" -> TriggeredDataChangeViolation
  -- Class 28 — Invalid Authorization Specification
  "28000" -> InvalidAuthorizationSpecification
  "28P01" -> InvalidPassword
  -- Class 2B — Dependent Privilege Descriptors Still Exist
  "2B000" -> DependentPrivilegeDescriptorsStillExist
  "2BP01" -> DependentObjectsStillExist
  -- Class 2D — Invalid Transaction Termination
  "2D000" -> InvalidTransactionTermination
  -- Class 2F — SQL Routine Exception
  "2F000" -> SqlRoutineException
  "2F005" -> FunctionExecutedNoReturnStatement
  "2F002" -> ModifyingSqlDataNotPermitted_2F
  "2F003" -> ProhibitedSqlStatementAttempted_2F
  "2F004" -> ReadingSqlDataNotPermitted_2F
  -- Class 34 — Invalid Cursor Name
  "34000" -> InvalidCursorName
  -- Class 38 — External Routine Exception
  "38000" -> ExternalRoutineException
  "38001" -> ContainingSqlNotPermitted
  "38002" -> ModifyingSqlDataNotPermitted_38
  "38003" -> ProhibitedSqlStatementAttempted_38
  "38004" -> ReadingSqlDataNotPermitted_38
  -- Class 39 — External Routine Invocation Exception
  "39000" -> ExternalRoutineInvocationException
  "39001" -> InvalidSqlstateReturned
  "39004" -> NullValueNotAllowed_39
  "39P01" -> TriggerProtocolViolated
  "39P02" -> SrfProtocolViolated
  -- Class 3B — Savepoint Exception
  "3B000" -> SavepointException
  "3B001" -> InvalidSavepointSpecification
  -- Class 3D — Invalid Catalog Name
  "3D000" -> InvalidCatalogName
  -- Class 3F — Invalid Schema Name
  "3F000" -> InvalidSchemaName
  -- Class 40 — Transaction Rollback
  "40000" -> TransactionRollback
  "40002" -> TransactionIntegrityConstraintViolation
  "40001" -> SerializationFailure
  "40003" -> StatementCompletionUnknown
  "40P01" -> DeadlockDetected
  -- Class 42 — Syntax Error or Access Rule Violation
  "42000" -> SyntaxErrorOrAccessRuleViolation
  "42601" -> SyntaxError
  "42501" -> InsufficientPrivilege
  "42846" -> CannotCoerce
  "42803" -> GroupingError
  "42P20" -> WindowingError
  "42P19" -> InvalidRecursion
  "42830" -> InvalidForeignKey
  "42602" -> InvalidName
  "42622" -> NameTooLong
  "42939" -> ReservedName
  "42804" -> DatatypeMismatch
  "42P18" -> IndeterminateDatatype
  "42P21" -> CollationMismatch
  "42P22" -> IndeterminateCollation
  "42809" -> WrongObjectType
  "42703" -> UndefinedColumn
  "42883" -> UndefinedFunction
  "42P01" -> UndefinedTable
  "42P02" -> UndefinedParameter
  "42704" -> UndefinedObject
  "42701" -> DuplicateColumn
  "42P03" -> DuplicateCursor
  "42P04" -> DuplicateDatabase
  "42723" -> DuplicateFunction
  "42P05" -> DuplicatePreparedStatement
  "42P06" -> DuplicateSchema
  "42P07" -> DuplicateTable
  "42712" -> DuplicateAlias
  "42710" -> DuplicateObject
  "42702" -> AmbiguousColumn
  "42725" -> AmbiguousFunction
  "42P08" -> AmbiguousParameter
  "42P09" -> AmbiguousAlias
  "42P10" -> InvalidColumnReference
  "42611" -> InvalidColumnDefinition
  "42P11" -> InvalidCursorDefinition
  "42P12" -> InvalidDatabaseDefinition
  "42P13" -> InvalidFunctionDefinition
  "42P14" -> InvalidPreparedStatementDefinition
  "42P15" -> InvalidSchemaDefinition
  "42P16" -> InvalidTableDefinition
  "42P17" -> InvalidObjectDefinition
  -- Class 44 — WITH CHECK OPTION Violation
  "44000" -> WithCheckOptionViolation
  -- Class 53 — Insufficient Resources
  "53000" -> InsufficientResources
  "53100" -> DiskFull
  "53200" -> OutOfMemory
  "53300" -> TooManyConnections
  "53400" -> ConfigurationLimitExceeded
  -- Class 54 — Program Limit Exceeded
  "54000" -> ProgramLimitExceeded
  "54001" -> StatementTooComplex
  "54011" -> TooManyColumns
  "54023" -> TooManyArguments
  -- Class 55 — Object Not In Prerequisite State
  "55000" -> ObjectNotInPrerequisiteState
  "55006" -> ObjectInUse
  "55P02" -> CantChangeRuntimeParam
  "55P03" -> LockNotAvailable
  -- Class 57 — Operator Intervention
  "57000" -> OperatorIntervention
  "57014" -> QueryCanceled
  "57P01" -> AdminShutdown
  "57P02" -> CrashShutdown
  "57P03" -> CannotConnectNow
  "57P04" -> DatabaseDropped
  -- Class 58 — System Error (errors external to PostgreSQL itself)
  "58000" -> SystemError
  "58030" -> IoError
  "58P01" -> UndefinedFile
  "58P02" -> DuplicateFile
  -- Class F0 — Configuration File Error
  "F0000" -> ConfigFileError
  "F0001" -> LockFileExists
  -- Class HV — Foreign Data Wrapper Error (SQL/MED)
  "HV000" -> FdwError
  "HV005" -> FdwColumnNameNotFound
  "HV002" -> FdwDynamicParameterValueNeeded
  "HV010" -> FdwFunctionSequenceError
  "HV021" -> FdwInconsistentDescriptorInformation
  "HV024" -> FdwInvalidAttributeValue
  "HV007" -> FdwInvalidColumnName
  "HV008" -> FdwInvalidColumnNumber
  "HV004" -> FdwInvalidDataType
  "HV006" -> FdwInvalidDataTypeDescriptors
  "HV091" -> FdwInvalidDescriptorFieldIdentifier
  "HV00B" -> FdwInvalidHandle
  "HV00C" -> FdwInvalidOptionIndex
  "HV00D" -> FdwInvalidOptionName
  "HV090" -> FdwInvalidStringLengthOrBufferLength
  "HV00A" -> FdwInvalidStringFormat
  "HV009" -> FdwInvalidUseOfNullPointer
  "HV014" -> FdwTooManyHandles
  "HV001" -> FdwOutOfMemory
  "HV00P" -> FdwNoSchemas
  "HV00J" -> FdwOptionNameNotFound
  "HV00K" -> FdwReplyHandle
  "HV00Q" -> FdwSchemaNotFound
  "HV00R" -> FdwTableNotFound
  "HV00L" -> FdwUnableToCreateExecution
  "HV00M" -> FdwUnableToCreateReply
  "HV00N" -> FdwUnableToEstablishConnection
  -- Class P0 — PL/pgSQL Error
  "P0000" -> PlpgsqlError
  "P0001" -> RaiseException
  "P0002" -> NoDataFound
  "P0003" -> TooManyRows
  -- Class XX — Internal Error
  "XX000" -> InternalError
  "XX001" -> DataCorrupted
  "XX002" -> IndexCorrupted
  -- Unknown error code
  _       -> UnknownErrorCode code
