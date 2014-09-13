#ifndef rand_h
#define rand_h

#include <fstream>

using namespace std;

class Rand {
public:
    static bool bytes(char* buf, size_t len);
};

#endif
