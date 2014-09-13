#include "rand.h"

using namespace std;

bool Rand::bytes(char* buf, size_t len) {
    fstream ur;
    ur.open("/dev/urandom", ios_base::binary | ios_base::in);
    if(!ur) {
        return false;
    }
    ur.read(buf, len);
    if(!ur) {
        return false;
    }
    ur.close();
    return true;
}
