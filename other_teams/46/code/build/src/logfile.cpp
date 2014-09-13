#include "logfile.h"

using namespace std;

LogFile::LogFile():
    dirty(false), has_next(false) {}

LogFile::~LogFile() {
    close();
}

void LogFile::appendEntry(const string& entry) {
    if(readonly) {
        return;
    }

    size_t len = entry.length() + 1;
    char* buf = (char*)malloc(len);
    if(buf == nullptr) {
        return;
    }
    memcpy(buf, entry.c_str(), len - 1);
    buf[len - 1] = '\n';

    // Encrypts in place
    rc4.encrypt(buf, len);

    hmac.update(buf, len);
    file.tellp();
    file.write(buf, len);
    free(buf);
    dirty = true;
}

// next is prepopulated to allow hasNext to work
string LogFile::readEntry() {
    if(!has_next) {
        return "";
    }
    string old = next;
    has_next = (bool)getline(data, next);
    return old;
}

// Increment count ensure less than log size
// count can now be used to access current element of the vector
bool LogFile::hasNext() {
    return has_next;
}

/**
 * Read log in as an encrypted blob
 * decrypt then hmac to ensure all is well
 * then file is read line by line
 * into the complete log vector for further processing
 *
 * Return 1:success, 0:err, -1:secerr
 */
int LogFile::open(const string& fn, const string& sc, bool ro) {
    char computed_hmac_buf[HMAC_SZ];
    char file_hmac_buf[HMAC_SZ];
    char nnc_buf[NNC_SZ];
    char secret[SEK_SZ];
    bool exists = false;

    filename = fn;
    readonly = ro;

    // Check if the file exists, open anyway for ro == false
    file.open(filename, fstream::binary | fstream::in);
    if(!file) {
        if(readonly) {
            return 0;
        }
    } else {
        file.close();
        exists = true;
    }

    ios_base::openmode flags = fstream::binary | fstream::in;
    if(!readonly) {
        flags |= fstream::out;
        if(!exists) {
            flags |= fstream::trunc;
        }
    }
    file.open(filename, flags);

    if(!sc.length()) {
        return 0;
    }

    // Generate secret(1)
    XSHA256 sha256;
    sha256.init();
    sha256.update(sc.c_str(), sc.length());

    // Determine if new file
    file.seekg(0, ios_base::end);
    streampos total_sz = file.tellg();
    streampos sz = 0;
    file.seekg(0);

    // Obtain file hmac and nnc
    if(total_sz != 0) {
        if(total_sz < HMAC_SZ + NNC_SZ) {
            return 0;
        }
        sz = total_sz - (long long)(HMAC_SZ + NNC_SZ);
        file.read(nnc_buf, NNC_SZ);
        if(!file) {
            return 0;
        }

        // Ready the ptr for reading
        file.seekg(-HMAC_SZ, ios_base::end);
        file.read(file_hmac_buf, HMAC_SZ);
        if(!file) {
            return 0;
        }
        file.seekg(NNC_SZ, ios_base::beg);
    } else {
        // Reading an empty file is an error
        if(readonly) {
            return 0;
        }

        if(!Rand::bytes(nnc_buf, NNC_SZ)) {
            return 0;
        }
        file.write(nnc_buf, NNC_SZ);
        if(!file) {
            return 0;
        }

        // We want to make sure we write the hmac. Otherwise, we create a corrupted file
        dirty = true;

        // No entries to read, but we have to initialize our crypto helpers ->
    }

    // Initialize crypto with the secret
    sha256.update(nnc_buf, NNC_SZ);
    sha256.digest(secret);
    hmac.init(secret, SEK_SZ);
    rc4.init(secret, SEK_SZ);

    // <- Now we return!
    if(total_sz == 0) {
        return 1;
    }

    // Read data, hmac, decrypt
    XHMAC in_hmac;
    in_hmac.init(secret, SEK_SZ);

    streampos i = 0;
    char file_buf[CHUNK_SZ];

    while (i < sz) {
        size_t chunk_sz = i + CHUNK_SZ < sz ? CHUNK_SZ:sz - i;
        file.read(file_buf, chunk_sz);
        if(!file) {
            return 0;
        }

        i += chunk_sz;
        hmac.update(file_buf, chunk_sz);
        in_hmac.update(file_buf, chunk_sz);

        // Decrypts in place
        rc4.decrypt(file_buf, chunk_sz);
        data.write(file_buf, chunk_sz);
    }
    data.seekg(0);
    file.seekp(-HMAC_SZ, ios_base::end);

    // Generate HMAC
    in_hmac.digest(computed_hmac_buf);

    if(memcmp(file_hmac_buf, computed_hmac_buf, HMAC_SZ) != 0) {
        return -1;
    }

    has_next = (bool)getline(data, next);

    return 1;
}

void LogFile::close() {
    if(!file.is_open()) {
        return;
    }

    if(dirty) {
        char hmac_buf[HMAC_SZ];
        hmac.digest(hmac_buf);
        file.write(hmac_buf, HMAC_SZ);
    }
    file.close();
}

