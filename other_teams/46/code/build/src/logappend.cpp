#include "types.h"
#include "logfile.h"
#include "state.h"
#include "parse.h"
#include "util.h"
#include <unistd.h>
#include <iostream>
#include <sstream>

using namespace std;

extern char *optarg;
extern int optind;
extern int opterr;

bool generate_args(int* argc_, char*** argv_, const string& line) {
    char* ctx;
    char* next;
    int argc__;
    char** argv__;

    size_t max_entries = 20;
    argv__ = (char**)malloc(sizeof(char**) * max_entries);
    if(argv__ == nullptr) {
        return false;
    }
    char* arg0 = strdup("");
    if(arg0 == nullptr) {
        free(argv__);
        return false;
    }

    argv__[0] = arg0;
    argv__[1] = nullptr;
    argc__ = 1;
    for(next = strtok_r((char*)line.c_str(), " ", &ctx); next != nullptr; next = strtok_r(NULL, " ", &ctx)) {
        ++argc__;
        if(argc__ >= max_entries - 1) {
            max_entries += 20;
            char** tmp = (char**)realloc(argv__, sizeof(char**) * max_entries);
            // Realloc failed, we have data still, so finish up...
            if(tmp == nullptr) {
                break;
            } else {
                argv__ = tmp;
            }
        }
        char* cpy = strdup(next);
        if(cpy == nullptr) {
            break;
        }
        (argv__)[argc__ - 1] = cpy;
        (argv__)[argc__    ] = nullptr;
    }

    *argc_ = argc__;
    *argv_ = argv__;
    return true;
}

void cleanup_args(int* argc_, char*** argv_) {
    for(size_t i = 0; i < *argc_; ++i) {
        free( (*argv_)[i] );
    }
    free(*argv_);
    *argv_ = nullptr;
    *argc_ = 0;
}

int process(int argc, char** argv) {
    optind = 1;
    string timestamp="", key="", name="", room="", fn="";

    int c;
    bool employee=false, guest=false, arrival=false, leaving=false;
    bool room_b=false, timestamp_b=false, key_b=false, arrival_b=false, leaving_b=false;
    // logappend -T <timestamp> -K <token> (-E <employee-name> | -G <guest-name>) (-A | -L) [-R <room-id>] <log>
    while ((c = getopt(argc, argv, "T:K:E:G:ALR:")) != -1) {
        switch (c) {
            case 'T':
                if(timestamp_b) {
                    cout << INVALID_STR << endl;
                    return -1;
                }
                timestamp_b = true;
                timestamp = optarg;
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
                if(employee) {
                    cout << INVALID_STR << endl;
                    return -1;
                }
                employee = true;
                name = optarg;
                break;
            case 'G':
                if(guest) {
                    cout << INVALID_STR << endl;
                    return -1;
                }
                guest = true;
                name = optarg;
                break;
            case 'A':
                if(arrival) {
                    cout << INVALID_STR << endl;
                    return -1;
                }
                arrival = true;
                break;
            case 'L':
                if(leaving) {
                    cout << INVALID_STR << endl;
                    return -1;
                }
                leaving = true;
                break;
            case 'R':
                if (room_b) {
                    cout << INVALID_STR << endl;
                    return -1;
                }
                room_b = true;
                room = optarg;
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
    if(timestamp.empty() || key.empty() || name.empty() || fn.empty()) {
        cout << INVALID_STR << endl;
        return -1;
    }

    // Check for mutually exclusive options
    // After this point, one of each pair is set
    if((employee == guest) || (arrival == leaving)) {
        cout << INVALID_STR << endl;
        return -1;
    }

    // If a room operation, check for a room!
    if(room_b && room.empty()) {
        cout << INVALID_STR << endl;
        return -1;
    }


    // ** Validity checks **

    // Ensure timestamp is numeric
    if(!is_numeric(timestamp)) {
        cout << INVALID_STR << endl;
        return -1;
    }

    // Ensure key is alphanum
    if(!is_alphanumeric(key)){
        cout << INVALID_STR << endl;
        return -1;
    }

    // Ensure the name is alpha
    if(!is_alpha(name)){
        cout << INVALID_STR << endl;
        return -1;
    }

    // Ensure room is numeric
    if(!is_numeric(room)) {
        cout << INVALID_STR << endl;
        return -1;
    }

    //Check for overflows
    if(unsigned_overflow(timestamp)){
        cout << INVALID_STR << endl;
        return -1;
    }

    if(room_b){
        if(signed_overflow(room)){
            cout << INVALID_STR << endl;
            return -1;
        }
    }


    // ** Processing **

    LogFile lf;
    int x = lf.open(fn, key, false);
    if(!x){
       cout << INVALID_STR << endl;
        return -1;
    }

    if(x == -1){
        cerr << SECERR_STR << endl;
        return -1;
    }

    State st;

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


    Entry new_e;
    new_e.time = strtoul(timestamp.c_str(), NULL, 0);
    new_e.room = NO_ROOM;
    new_e.is_employee = guest ? false:true;
    new_e.type = -1;
    new_e.name = name;

    if (arrival) {
        // Room operation, else gallery operation
        if(room_b) {
            new_e.type = ENTER_ROOM;
            // Signed <- Unsigned
            new_e.room = strtoul(room.c_str(), NULL, 0);
        } else{
            new_e.type = ENTER_GALLERY;
        }
    }

    if (leaving) {
        // Room operation, else gallery operation
        if(room_b) {
            new_e.type = LEAVE_ROOM;
            // Signed <- Unsigned
            new_e.room = strtoul(room.c_str(), NULL, 0);
        } else{
            new_e.type = LEAVE_GALLERY;
        }
    }

    if(!Parse::insert(st, &new_e)) {
        cout << INVALID_STR << endl;
        return -1;
    } else {
        string logger = Parse::serialize(&new_e);
        lf.appendEntry(logger);
        lf.close();
    }

    return 0;
}

int process_batch(char* fn){
    ifstream file;
    file.open(fn);
    if (!file) {
        cout << INVALID_STR << endl;
        return -1;
    }

    string line;
    vector<string*> commands;
    while(getline(file, line)) {
        int argc_;
        char** argv_;
        if(!generate_args(&argc_, &argv_, line)) {
            continue;
        }

        process(argc_, argv_);
        cleanup_args(&argc_, &argv_);
    }

    return 0;
}

int main(int argc, char** argv) {
    opterr = 0;
    int c = 0;
    bool batch = false;
    char* batch_file = nullptr;

    // logappend -B <file>
    while ((c = getopt(argc, argv, "B:T:K:E:G:ALR:")) != -1) {
        switch (c) {
            case 'B':
                batch = true;
                batch_file = optarg;
                break;
            default:
                if(batch) {
                    cerr << INVALID_STR << endl;
                    return -1;
                }
        }
    }
    if(batch) {
        return process_batch(batch_file);
    } else {
        return process(argc, argv);
    }
}
