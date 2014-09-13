/*
 * functions.h
 *
 *  Created on: Aug 31, 2014
 *      Author: kevin
 */

#ifndef FUNCTIONS_H_
#define FUNCTIONS_H_

#include "dbg.h"
#include "args.h"
#include "definitions.h"
#include "hash.h"
#include "argv.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <openssl/md5.h>
#include <stdlib.h>
#include <openssl/evp.h>
#include <openssl/aes.h>

extern HT* allMahHashes_guests;
extern HT* allMahHashes_employees;

off_t fsize(const char *filename);
int check_logic(logappend_args * args);
int do_crypt(FILE *in, FILE *out, int do_encrypt, unsigned char *key_data,
		int key_data_len, unsigned char *salt);
void invalid();
void printMD5(char * toPrint);
void invalid_token();
void nameOpt(char * input);
void tokenOpt(char * input);
void numOpt(char * input);
void invalid_check(logappend_args * args);
void cryptWrapper(logappend_args * args, int32_t type);

#endif /* FUNCTIONS_H_ */
