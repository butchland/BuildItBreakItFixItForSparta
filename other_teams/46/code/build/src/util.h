#ifndef util_h
#define util_h

#include "state.h"
#include <ctype.h>
#include <algorithm>
#include <climits>

void sort_people(vector<Person*>& people);
void sort_rooms(vector<Room*>& rooms);
void sort_ints(vector<int>& ints);
bool is_alpha(const string& str);
bool is_numeric(const string& str);
bool is_alphanumeric(const string& str);
bool unsigned_overflow(string number);
bool signed_overflow(string number);

#endif
