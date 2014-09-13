#ifndef xrc4_h
#define xrc4_h

#include <string.h>
#include <iostream>
#include <openssl/rc4.h>

class XRC4 {
public:
    XRC4();
    ~XRC4();
    void init(const char* k, size_t ksz);
    char *encrypt(char *ptext, size_t len);
    char *decrypt(char *ctext, size_t len);
private:
    RC4_KEY rc4_key;
};

#endif
