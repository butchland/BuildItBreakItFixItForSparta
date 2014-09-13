#ifndef xhmac_h
#define xhmac_h

#include <openssl/evp.h>
#include <openssl/hmac.h>
#include <string>

using namespace std;

class XHMAC {
public:
    XHMAC();
    ~XHMAC();
    void init(const char* key, size_t len);
    void update(const char* bytes, size_t len);
    void digest(char* digest);
private:
    HMAC_CTX ctx;
};

#endif
