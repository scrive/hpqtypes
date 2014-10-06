#include <libpq-fe.h>

// Needed by bindings for passing this in case
// input ByteString is null.
const char pqt_hs_null_string_ptr[1];

// Workaround for bug in GHC FFI that causes foreign
// pointer finalizers to be run more than once under
// random circumstances - PQfinish invoked on already
// finished PGconn causes segmentation fault, therefore
// we introduce another level of indirection and set
// finished PGconn object to NULL so that subsequent
// calls to this function are safe.
void PQfinishPtr(PGconn **conn)
{
  PQfinish(*conn);
  *conn = NULL;
}
