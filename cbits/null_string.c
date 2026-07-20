// A non-null pointer to an empty C string. Needed for passing an empty value
// to libpq in case the input ByteString is backed by a null pointer, which
// libpq would interpret as SQL NULL.
const char hpqtypes_null_string_ptr[1];
