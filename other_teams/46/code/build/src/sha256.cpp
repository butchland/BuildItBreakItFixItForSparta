#include "sha256.h"

using namespace std;

XSHA256::XSHA256() {
    SHA256_Init(&ctx);
}

XSHA256::~XSHA256() {} 

void XSHA256::init() {}

void XSHA256::update(const char* bytes, size_t len) {
    SHA256_Update(&ctx, (unsigned char*)bytes, len);
}

void XSHA256::digest(char* digest) {
    SHA256_Final((unsigned char*)digest, &ctx);
}
