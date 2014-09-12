#ifndef state_h
#define state_h

#include <iostream>
#include <string>
#include <vector>
#include "types.h"
using namespace std;

struct Person {
    Person(const string& name, bool is_employee);
    string name;
    vector<int> rooms;
    bool is_employee;
    bool in_gallery;
    int curr_room;
    vector<unsigned int> gallery_times;
    vector<unsigned int> room_times;
};

class Room {
public:
    Room(int room);
    void enter(Person* p);
    void exit(Person* p);

    int room;
    vector<Person*> people;
};

class State {
public:
    State();
    ~State();
    bool enterGallery(unsigned int time, const string& name, bool is_employee);
    bool exitGallery(unsigned int time, const string& name, bool is_employee);
    bool enterRoom(unsigned int time, const string& name, bool is_employee, int room);
    bool exitRoom(unsigned int time, const string& name, bool is_employee, int room);
    Room* getRoom(int room);
    Person* getPerson(const string& name, bool is_employee);
    void addPerson(Person* p);
    bool inGallery(int low, int high, Person* p);

    vector<Person*> employees;
    vector<Person*> guests;
    vector<Room*> rooms;
    unsigned int last_time;
};

#endif
