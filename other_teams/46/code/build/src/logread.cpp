#include "types.h"
#include "logfile.h"
#include "state.h"
#include "parse.h"
#include "formatter.h"
#include "util.h"
#include <unistd.h>
#include <iostream>
#include <map>

using namespace std;

extern char *optarg;
extern int optind;
extern int opterr;

int find_together(LogFile& lf, State& st, vector<string>& names, vector<bool>& types, bool html) {
    map<int,int> room_map;
    vector<int> interesting_rooms;

    // Make sure there are no duplicates
    map<string, unsigned int> people_map;
    for(size_t i = 0; i < names.size(); ++i) {
        unsigned int interesting_bit = 1 << ((unsigned int)types[i]);
        if(people_map[names[i]] & interesting_bit) {
            cout << INVALID_STR <<endl;
            return -1;
        }
        people_map[names[i]] |= interesting_bit;
    }

    int total_interesting_people = names.size();
    while(lf.hasNext()) {
        string buf = lf.readEntry();
        Entry e;
        if(!Parse::deserialize(buf, &e)) {
            cout << INTERR_STR << endl;
            return -1;
        }
        if(!Parse::insert(st, &e)) {
            cout << INTERR_STR << endl;
            return -1;
        }

        // We only care about room events
        if(e.room == NO_ROOM) {
            continue;
        }

        // We only care about interesting people
        bool interesting_person = false;
        for(int i = 0; i < names.size(); ++i){
            if(e.is_employee == types[i] && e.name == names[i]) {
                interesting_person = true;
                break;
            }
        }

        if(!interesting_person) {
            continue;
        }

        // Look for the room
        int delta = e.type == ENTER_ROOM ? 1:-1;
        room_map[e.room] += delta;

        if(room_map[e.room] == total_interesting_people) {
            interesting_rooms.push_back(e.room);
        }
    } // while end

    sort(interesting_rooms.begin(), interesting_rooms.end());

    Formatter* fmt;
    if(html) {
        fmt = new Html();
    } else {
        fmt = new Plain();
    }
    fmt->printRoomVector(interesting_rooms);
    
    delete fmt;
    return 0;
}


int main(int argc, char **argv) {
    opterr = 0;
    string fn="", key="", lower1="", lower2="", upper1="", upper2="";
    unsigned int lower1_u=0, lower2_u=0, upper1_u=0, upper2_u=0;
    
    int c;
    bool state=false, room=false, time=false, together=false, alist=false, base=false,
    employee=false, guest=false,
    html=false;
    bool key_b=false, room_b=false, html_b=false, state_b=false;
    int lower_b=0, upper_b=0;
    
    vector<string> names;
    vector<bool> types;
    // logread -K <token> [-H] -S <log>
    // logread -K <token> [-H] -R (-E <name> | -G <name>) <log>
    // logread -K <token> -T (-E <name> | -G <name>) <log>
    // logread -K <token> [-H] -I (-E <name> | -G <name>) [(-E <name> | -G <name>) ...] <log>
    // logread -K <token> [-H] -A -L <lower> -U <upper> <log>
    // logread -K <token> [-H] -B -L <lower1> -U <upper1> -L <lower2> -U <upper2> <log>
    while ((c = getopt(argc, argv, "K:HSRTIABE:G:L:U:")) != -1) {
        switch (c) {
            case 'T':
                if(time) {
                    cout << INVALID_STR << endl;
                    return -1;
                }
                time = true;
                break;
            case 'B':
                if(base) {
                    cout << INVALID_STR << endl;
                    return -1;
                }
                base = true;
                break;
            case 'K':
                if(key_b) {
                    cout << INVALID_STR << endl;
                    return -1;
                }
                key_b = true;
                key = optarg;
                break;
            case 'E':
                employee = true;
                names.push_back(optarg);
                types.push_back(true);
                break;
            case 'G':
                guest = true;
                names.push_back(optarg);
                types.push_back(false);
                break;
            case 'A':
                if(alist) {
                    cout << INVALID_STR << endl;
                    return -1;
                }
                alist = true;
                break;
            case 'L':
                if (lower_b == 0) {
                    lower1 = optarg;
                } else if (lower_b == 1) {
                    lower2 = optarg;
                } else{
                    cout << INVALID_STR << endl;
                    return -1;
                }
                ++lower_b;
                break;
            case 'U':
                if (upper_b == 0) {
                    upper1 = optarg;
                } else if (upper_b == 1) {
                    upper2 = optarg;
                } else {
                    cout << INVALID_STR << endl;
                    return -1;
                }
                ++upper_b;
                break;
            case 'R':
                if(room) {
                    cout << INVALID_STR << endl;
                    return -1;
                }
                room = true;
                break;
            case 'H':
                if(html) {
                    cout << INVALID_STR << endl;
                    return -1;
                }
                html = true;
                break;
            case 'S':
                if(state) {
                    cout << INVALID_STR << endl;
                    return -1;
                }
                state = true;
                break;
            case 'I':
                if(together) {
                    cout << INVALID_STR << endl;
                    return -1;
                }
                together = true;
                break;
            default:
                cout << INVALID_STR << endl;
                return -1;
        }
    }
    
    // ** Existence checks **
    
    // Add the file name
    if(optind < argc) {
        fn = argv[optind];
    }
    
    // Check for required params
    if(key.empty() || fn.empty()) {
        cout << INVALID_STR << endl;
        return -1;
    }

    // Require at least 1 command
    if(!state && !room && !time && !together && !alist && !base) {
        cout << INVALID_STR << endl;
        return -1;
    }

    bool range1 = !lower1.empty() || !upper1.empty();
    bool range2 = !lower2.empty() || !upper2.empty();
    bool range = range1 || range2;
    bool all_range1 = !lower1.empty() && !upper1.empty();
    bool all_range = all_range1 && !lower2.empty() && !upper2.empty();
    bool people = names.size() > 1;
    
    // -S, no arguments
    if(state && (room || time || together || alist || base || employee || guest || people || range)) {
        cout << INVALID_STR << endl;
        return -1;
    }
    
    // -K, (-E|-G)
    if(room && (guest == employee || state || time || together || alist || base || people || range)) {
        cout << INVALID_STR << endl;
        return -1;
    }
    // -T, (-E|-G) !-H
    if(time && (guest == employee || state || room || together || alist || base || people || range || html)) {
        cout << INVALID_STR << endl;
        return -1;
    }
    
    // -I, (-E|-G)+
    if(together && ((!employee && !guest) || state || room || time || alist || base || range)) {
        cout << INVALID_STR << endl;
        return -1;
    }
    
    // -A, -L -U
    if(alist && (state || room || time || together || base || employee || guest || people || !all_range1 || range2)) {
        cout << INVALID_STR << endl;
        return -1;
    }
    
    // -B, -L -U -L -U
    if(base && (state || room || time || together || alist || employee || guest || people || !all_range)) {
        cout << INVALID_STR << endl;
        return -1;
    }
    
    
    // ** Validity checks **
    
    // Ensure key is alphanum
    if(!is_alphanumeric(key)){
        cout << INVALID_STR << endl;
        return -1;
    }
    
    // Check all names (if any) are alpha
    //  Can always do this because if statements will filter out invalid cmds
    for(vector<string>::iterator nit = names.begin(); nit != names.end(); ++nit) {
        if(!is_alpha(*nit)) {
            cout << INVALID_STR << endl;
            return -1;
        }
    }
    
    // Check first range is numeric and not backwards
    if(alist || base) {
        lower1_u = strtoul(lower1.c_str(), NULL, 0);
        upper1_u = strtoul(upper1.c_str(), NULL, 0);

        //Check for overflows
        if(unsigned_overflow(lower1) || unsigned_overflow(upper1)) {
            cout << INVALID_STR << endl;
            return -1;
        }

        if(!is_numeric(lower1) || !is_numeric(upper1) || lower1_u >= upper1_u) {
            cout << INVALID_STR << endl;
            return -1;
        }
    }
    
    // Check second range is numeric and not backwards
    if(base) {
        lower2_u = strtoul(lower2.c_str(), NULL, 0);
        upper2_u = strtoul(upper2.c_str(), NULL, 0);

        //Check for overflows
        if(unsigned_overflow(lower2) || unsigned_overflow(upper2)){
            cout << INVALID_STR << endl;
            return -1;
        }

        if(!is_numeric(lower2) || !is_numeric(upper2) || lower2 >= upper2) {
            cout << INVALID_STR << endl;
            return -1;
        }
    }
    
    
    // ** Processing **
    
    LogFile lf;
    int x = lf.open(fn, key, true);
    if(!x) {
        cout << INVALID_STR << endl;
        return -1;
    }
    
    if(x == -1) {
        cout << SECERR_STR << endl;
        return -1;
    }
    
    State st;
    
    // -I
    if(together) {
        return find_together(lf, st, names, types, html);
    }
    
    // Update to newest state from log
    while(lf.hasNext()) {
        string buf = lf.readEntry();
        Entry e;
        if(!Parse::deserialize(buf, &e)) {
            cout << INTERR_STR << endl;
            return -1;
        }
        if(!Parse::insert(st, &e)) {
            cout << INTERR_STR << endl;
            return -1;
        }
    }
    
    Formatter* fmt;
    if(html) {
        fmt = new Html();
    } else {
        fmt = new Plain();
    }
    int ret = 0;
    
    // ** Output **
    
    // -S
    if (state) {
        // Print current state of log
        fmt->printState(st);
        // -R
    } else if(room) {
        // Print all rooms a person has been in
        Person* p = st.getPerson(names[0], types[0]);
        fmt->printRooms(p);
        // -T
    } else if(time) {
        // Print how much time a person has spent in the gallery
        Person* p =  st.getPerson(names[0], types[0]);
        fmt->printTime(st, p);
        // -A
    } else if(alist) {
        // Print employees in gallery during certain time frame
        vector<Person*> people;
        for(vector<Person*>::const_iterator eit = st.employees.begin(); eit != st.employees.end(); ++eit) {
            if(st.inGallery(lower1_u, upper1_u, *eit)) {
                people.push_back(*eit);
            }
        }

       fmt->printEmployees(people);
        // -B
    } else if(base) {
        vector<Person*> people;
        for(vector<Person*>::const_iterator eit = st.employees.begin(); eit != st.employees.end(); ++eit) {
            if(st.inGallery(lower1_u, upper1_u, *eit) && !st.inGallery(lower2_u, upper2_u, *eit)) {
                people.push_back(*eit);
            }
        }

        fmt->printEmployees(people);
    }
    
    delete fmt;
    return ret;
}
