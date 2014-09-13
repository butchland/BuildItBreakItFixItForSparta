#include "state.h"
using namespace std;

Person::Person(const string& name, bool is_employee):
name(name), is_employee(is_employee), in_gallery(false), curr_room(NO_ROOM) {}

Room::Room(int room) :
room(room) {}

void Room::enter(Person* p) {
    p->curr_room = room;
    p->rooms.push_back(room);
    people.push_back(p);
}


// FIXME: Inefficient
void Room::exit(Person* p) {
    p->curr_room = NO_ROOM;
    for(vector<Person*>::iterator pit = people.begin(); pit != people.end(); ++pit) {
        if(*pit != p) {
            continue;
        }
        *pit = *(--people.end());
        people.pop_back();
        return;
    }
}

State::State():
last_time(0) {}

State::~State() {
    for(vector<Person*>::iterator eit = employees.begin(); eit != employees.end(); ++eit) {
        delete (*eit);
    }
    for(vector<Person*>::iterator git = guests.begin(); git != guests.end(); ++git) {
        delete (*git);
    }
    for(vector<Room*>::iterator rit = rooms.begin(); rit != rooms.end(); ++rit) {
        delete (*rit);
    }
}

// Set Persons gallery field to True : Must not have already entered gallery
bool State::enterGallery(unsigned int time, const string& name, bool is_employee) {
    if(time < last_time) {
        return false;
    }
    // No rejoining the gallery
    Person* old_p = getPerson(name, is_employee);
    if(old_p != nullptr) {
        return false;
    }
    Person* p = new Person(name, is_employee);
    p->in_gallery = true;
    p->gallery_times.push_back(time);
    addPerson(p);
    last_time = time;
    return true;
}

// Set Persons gallery field to False : Must leave room before exiting gallery
bool State::exitGallery(unsigned int time, const string& name, bool is_employee) {
    if(time < last_time) {
        return false;
    }
    Person* p = getPerson(name, is_employee);
    if(p == nullptr) {
        return false;
    }
    if(p->curr_room != NO_ROOM) {
        return false;
    }
    if(!p->in_gallery) {
        return false;
    }
    p->in_gallery = false;
    p->gallery_times.push_back(time);
    last_time = time;
    return true;
}

bool State::enterRoom(unsigned int time, const string& name, bool is_employee, int room) {
    if(time < last_time) {
        return false;
    }
    Person* p = getPerson(name, is_employee);
    if(p == nullptr) {
        return false;
    }
    if(!p->in_gallery) {
        return false;
    }
    if(room < 0) {
        return false;
    }
    if(p->curr_room != NO_ROOM) {
        return false;
    }
    Room* r = getRoom(room);
    if(r == nullptr) {
        r = new Room(room);
        rooms.push_back(r);
    }
    r->enter(p);
    p->room_times.push_back(time);
    last_time = time;
    return true;
}

// Can only leave current room
bool State::exitRoom(unsigned int time, const string& name, bool is_employee, int room) {
    if(time < last_time) {
        return false;
    }
    Person* p = getPerson(name, is_employee);
    if(p == nullptr) {
        return false;
    }
    if(!p->in_gallery) {
        return false;
    }
    if(room < 0) {
        return false;
    }
    if(p->curr_room != room) {
        return false;
    }
    
    Room* r = getRoom(room);
    if(r == nullptr) {
        return false;
    }
    r->exit(p);
    last_time = time;
    p->room_times.push_back(time);
    return true;
}

void State::addPerson(Person* p) {
    if(p->is_employee) {
        employees.push_back(p);
    } else {
        guests.push_back(p);
    }
}

// FIXME: Inefficient
Room* State::getRoom(int room) {
    for(vector<Room*>::iterator rit = rooms.begin(); rit != rooms.end(); ++rit) {
        if((*rit)->room != room) {
            continue;
        }
        return *rit;
    }
    return nullptr;
}

// FIXME: Inefficient
Person* State::getPerson(const string& name, bool is_employee) {
    Person* p = nullptr;
    if(is_employee) {
        for(vector<Person*>::iterator eit = employees.begin(); eit != employees.end(); ++eit) {
            p = *eit;
            if(name != p->name) {
                continue;
            }
            return p;
        }
    } else {
        for(vector<Person*>::iterator git = guests.begin(); git != guests.end(); ++git) {
            p = *git;
            if(name != p->name) {
                continue;
            }
            return p;
        }
    }
    return nullptr;
}

// FIXME: Inefficient
bool State::inGallery(int low, int high, Person* p){
    bool low_bound = false, high_bound = false;
    for (int i = 0; i < p->gallery_times.size(); ++i) {
        if(p->gallery_times[i] <= high){
            low_bound = true;
        }
        if(p->gallery_times[i] >= low){
            high_bound = true;
        }
    }
    if(p->in_gallery){
        if(last_time <= high){
            low_bound = true;
        }
        if(last_time >= low){
            high_bound = true;
        }
    }
    return (low_bound && high_bound);
}

