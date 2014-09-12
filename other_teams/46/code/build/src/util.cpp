#include "util.h"

bool person_cmp(Person* a, Person* b) { return a->name.compare(b->name) < 0; }
bool room_cmp(Room* a, Room* b) { return a->room < b->room; }

void sort_people(vector<Person*>& people) {
    std::sort(people.begin(), people.end(), person_cmp);
}

void sort_rooms(vector<Room*>& rooms) {
    std::sort(rooms.begin(), rooms.end(), room_cmp);
}

void sort_ints(vector<int>& ints) {
    std::sort(ints.begin(), ints.end());
}

bool is_alpha(const string& str) {
    for(size_t i = 0; i < str.length(); ++i) {
        if(!isalpha(str[i])) {
            return false;
        }
    }
    return true;
}

bool is_numeric(const string& str) {
    for(size_t i = 0; i < str.length(); ++i) {
        if(!isdigit(str[i])) {
            return false;
        }
    }
    return true;
}

bool is_alphanumeric(const string& str) {
    for(size_t i = 0; i < str.length(); ++i) {
        if(!isalpha(str[i]) && !isdigit(str[i])) {
            return false;
        }
    }
    return true;
}

bool unsigned_overflow(string number){
    unsigned long int hippo = strtoul(number.c_str(), NULL, 0);
    return (hippo == ULONG_MAX);
}

bool signed_overflow(string number){
    long hippo = strtol(number.c_str(), NULL, 0);
    return (hippo == LONG_MAX);
}
