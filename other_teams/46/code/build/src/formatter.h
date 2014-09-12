#ifndef formatter_h
#define formatter_h

#include <string>
#include <iostream>
#include "state.h"
#include "util.h"
using namespace std;

class Formatter {
public:
    virtual ~Formatter();
    virtual void printState(const State& st) = 0;
    virtual void printRooms(Person* p) = 0;
    virtual void printTime(State& st, Person* p) = 0;
    virtual void printEmployees(vector<Person*>& e) = 0;
    virtual void printRoomVector(const vector<int>& v) = 0;
};

class Html : public Formatter {
public:
    ~Html();
    void header();
    void footer();
    void printState(const State& st);
    void printRooms(Person* p);
    void printTime(State& st, Person* p);
    void printEmployees(vector<Person*>& e);
    void printRoomVector(const vector<int>& v);
};

class Plain : public Formatter {
public:
    ~Plain();
    void printState(const State& st);
    void printRooms(Person* p);
    void printTime(State& st, Person* p);
    void printEmployees(vector<Person*>& e);
    void printRoomVector(const vector<int>& v);
};


#endif
