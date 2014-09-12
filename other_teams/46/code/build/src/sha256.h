#ifndef sha256_h
#define sha256_h

#include <openssl/sha.h>
#include <string>

using namespace std;

class XSHA256 {
public:
    XSHA256();
    ~XSHA256();
    void init();
    void update(const char* bytes, size_t len);
    void digest(char* digest);
private:
    SHA256_CTX ctx;
};

#endif
