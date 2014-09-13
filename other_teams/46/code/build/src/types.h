#ifndef types_h
#define types_h

#include <openssl/sha.h>

#define SECERR_STR "security error"
#define INVALID_STR "invalid"
#define INTERR_STR "integrity violation"

#define NO_ROOM -1

#define SEK_SZ SHA256_DIGEST_LENGTH
#define HMAC_SZ SHA256_DIGEST_LENGTH
#define NNC_SZ 16

#define ENTER_GALLERY 0
#define ENTER_ROOM 1
#define LEAVE_ROOM 2
#define LEAVE_GALLERY 3

#endif
