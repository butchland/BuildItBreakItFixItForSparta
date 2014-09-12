#include "hmac.h"
#include <iostream> // KAI

using namespace std;

XHMAC::XHMAC() {
    HMAC_CTX_init(&ctx);
}
XHMAC::~XHMAC() {
    HMAC_CTX_cleanup(&ctx);
} 

void XHMAC::init(const char* key, size_t len) {
    HMAC_Init_ex(&ctx, key, len, EVP_sha256(), NULL);
}

void XHMAC::update(const char* bytes, size_t len) {
    HMAC_Update(&ctx, (unsigned char*)bytes, len);
}

void XHMAC::digest(char* digest) {
    unsigned int len;
    HMAC_Final(&ctx, (unsigned char*)digest, &len);
}
