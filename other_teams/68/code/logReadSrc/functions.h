/*
 * functions.h
 *
 *  Created on: Sep 1, 2014
 *      Author: kevin
 */

#ifndef FUNCTIONS_H_
#define FUNCTIONS_H_

#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <openssl/md5.h>
#include <stdlib.h>
#include <openssl/evp.h>
#include <openssl/aes.h>
#include "definitions.h"

void printMD5(char * toPrint);
off_t fsize(const char *filename);
int do_crypt(FILE *in, FILE *out, int do_encrypt, unsigned char *key_data,
		int key_data_len, unsigned char *salt);
void invalid();
void invalid_0();
void invalid_token();
void nameOpt(char * input);
void numOpt(char * input);
void tokenOpt(char * input);
void invalid_check(logread_args * args);

#endif /* FUNCTIONS_H_ */
