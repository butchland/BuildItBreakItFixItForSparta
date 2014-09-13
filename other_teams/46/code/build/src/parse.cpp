#include "parse.h"
using namespace std;


string Parse::serialize(const Entry* e){
    try {
        ostringstream oss;
        oss << e->time;
        oss << ":";
        oss << e->room;
        oss << ":";
        oss << e->is_employee;
        oss << ":";
        oss << e->type;
        oss << ":";
        oss << e->name;
    
        return oss.str();
    } catch(...) {
        return nullptr;
    }
    
}

bool Parse::deserialize(const string& s, Entry* entry){
    string token = "";
    istringstream stream(s);
    int x;

    delimit(stream, token);
    entry->time = strtoul(token.c_str(), NULL, 0);
    
    delimit(stream, token);
    x = strtol (token.c_str(), NULL, 0);
    entry->room = NO_ROOM;
    if(x >= 0) {
        entry->room = x;
    }
    
    delimit(stream, token);
    x = strtoul (token.c_str(), NULL, 0);
    if(x == 0) {
        entry->is_employee = false;
    } else if(x == 1) {
        entry->is_employee = true;
    } else {
        return false;
    }
    
    delimit(stream, token);
    x = strtoul (token.c_str(), NULL, 0);
    if(x < 0 || x > 4) {
        return false;
    }
    entry->type = x;
    
    delimit(stream, token);
    if(token.length() == 0 || !is_alpha(token)) {
        return false;
    }
    entry->name = token;
    
    return true;
}

void Parse::delimit(istringstream& stream, string& token){
    char inp;
    token = "";

    while(stream >> inp){
        if(inp == ':')
            break;
        token += inp;
    }
}

bool Parse::insert(State& state, Entry *e){
    int x = e->type;
    bool ret = false;

    switch (x) {
        case ENTER_GALLERY:
            ret = state.enterGallery(e->time, e->name, e->is_employee);
            break;
        case ENTER_ROOM:
            ret = state.enterRoom(e->time, e->name, e->is_employee, e->room);
            break;
        case LEAVE_ROOM:
            ret = state.exitRoom(e->time, e->name, e->is_employee, e->room);
            break;
        case LEAVE_GALLERY:
            ret = state.exitGallery(e->time, e->name, e->is_employee);
            break;
    }
    return ret;
}

















