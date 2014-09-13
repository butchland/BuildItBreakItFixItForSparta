#ifndef parse_h
#define parse_h

#include <string>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include "state.h"
#include "util.h"
#include "types.h"

using namespace std;

struct Entry {
    unsigned int time;
    signed int room;
    bool is_employee;
    int type;
    string name;
};

class Parse {
public:
    static string serialize(const Entry* e);
    static bool deserialize(const string& s, Entry* e);
    static void delimit(istringstream& stream, string& token);
    static bool insert(State& state, Entry* e);
};

#endif




















