#ifndef logfile_h
#define logfile_h

#include <fstream>
#include <string>
#include <vector>
#include <iostream>
#include <sstream>
#include <openssl/hmac.h>
#include <string.h>
#include "rc4.h"
#include "hmac.h"
#include "sha256.h"
#include "types.h"
#include "rand.h"

#define CHUNK_SZ 2048LL

using namespace std;

class LogFile {
public:
    LogFile();
    ~LogFile();
    // Open a file and verify integrity
    // @return success
    int open(const string& fn, const string& sc, bool ro);
    
    // Read a single entry
    string readEntry();
    
    // Whether another entry is available
    // @return available
    bool hasNext();
    
    // Append an entry
    void appendEntry(const string& entry);
    
    // Generate a new hmac and close the file
    void close();
    
private:
    XRC4 rc4;
    XHMAC hmac;
    fstream file;

    string filename;

    stringstream data;
    string next;
    bool has_next;

    bool dirty;
    bool readonly;
};

#endif







