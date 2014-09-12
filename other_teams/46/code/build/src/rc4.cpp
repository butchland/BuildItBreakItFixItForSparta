#include "rc4.h"

using namespace std;

XRC4::XRC4() {}

XRC4::~XRC4() {}

void XRC4::init(const char* k, size_t ksz) {
    RC4_set_key(&rc4_key, ksz, (const unsigned char*)k);
}

char* XRC4::encrypt(char *ptext, size_t len) {
    RC4(&rc4_key, len, (const unsigned char*)ptext, (unsigned char*)ptext);
    return ptext;
}

char* XRC4::decrypt(char *ctext, size_t len) {
    return encrypt(ctext, len);
}
