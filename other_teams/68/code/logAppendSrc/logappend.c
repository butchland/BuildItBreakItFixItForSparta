#include "dbg.h"
#include "args.h"
#include "definitions.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <openssl/md5.h>
#include <stdlib.h>
#include <openssl/evp.h>
#include <openssl/aes.h>
#include "hash.h"
#include "argv.h"
#include "functions.h"

#define DECRYPT 0
#define ENCRYPT 1

void batch(logappend_args args);
void processLine(logappend_args args, int32_t CheckAndHashBool);
void inter(logappend_args args);

#define NULL_CHECK(val)  if (val == NULL) invalid_check(&args);

MD5_CTX oldMD5;
MD5_CTX currentMD5;
MD5_CTX newMD5;

unsigned char oldMD5String[MD5_DIGEST_LENGTH + 1];
unsigned char currentMD5String[MD5_DIGEST_LENGTH + 1];
unsigned char newMD5String[MD5_DIGEST_LENGTH + 1];

uint32_t oldHashExists;
uint32_t firstRun;

char * password;

HT* allMahHashes_guests = NULL;
HT* allMahHashes_employees = NULL;

int main(int argc, char * argv[]) {
	if (argc < 3)
		invalid();
	oldTime = -1;
	password = NULL;
	firstRun = 1;
	logappend_args args = opt_parser(argc, argv, 1);
	allMahHashes_employees = ht_create(65536);
	allMahHashes_guests = ht_create(65536);

	if (args.returnStatus == -1)
		invalid();

	int32_t fileSize = fsize(args.logName);

	if (args.batchFile) {
		isBatch = 1;
		batch(args);
	} else {
		cryptWrapper(&args, DECRYPT);
		isBatch = 0;
		inter(args);
		if (check_logic(&args) == -1)
			invalid_check(&args);
		processLine(args, 1);
		cryptWrapper(&args, ENCRYPT);
	}

	return 0;
}

void inter(logappend_args args) {
	int32_t fileSize = 0;
	FILE* mahFile = NULL;
	size_t bytes = 0;
	ssize_t read = 0;
	char * line = NULL;
	char interString[MAX * 4];

	fileSize = fsize(args.logName);
	if (fileSize < 10) {
		return;
	} else {
		mahFile = fopen(args.logName, "r");
	}

	while ((read = getline(&line, &bytes, mahFile)) != -1 && fileSize > 10) {

		int len = strlen(line);
		fileSize = fileSize - len;
		bzero(interString, MAX * 4);
		// RERUN COMMANDS CAUZE LOGIC!
		sprintf(interString, "./logappend %s", line);
		int tempc;
		char ** tempv = argv_split(interString, &tempc);
		logappend_args temp = opt_parser(tempc, tempv, 0);
		if (check_logic(&temp) == -1)
			continue;
		argv_free(tempv);
		// FINISH LOGICZ
	}
	fclose(mahFile);
}

void batch(logappend_args args) {
	FILE* batchFile = NULL;
	int32_t fileSize = 0;
	size_t bytes = 0;
	ssize_t read = 0;
	char * line = NULL;
	char interString[MAX * 4];
	logappend_args temp;
	fileSize = fsize(args.batchFile);
	if (fileSize < 10)
		invalid_check(&args);
	batchFile = fopen(args.batchFile, "r+");

	while ((read = getline(&line, &bytes, batchFile)) != -1 && fileSize > 10) {

		int len = strlen(line);
		fileSize = fileSize - len;
		// RERUN COMMANDS CAUZE LOGIC!
		sprintf(interString, "./logappend %s", line);
		int tempc;
		char ** tempv = argv_split(interString, &tempc);
		temp = opt_parser(tempc, tempv, 1);

		if (temp.batchFile) {
			firstRun = 0;
			fprintf(stderr, "invalid\n");
			continue;
		}
		if (firstRun) {
			cryptWrapper(&temp, DECRYPT);
			password = temp.token;
			inter(temp);
		} else if (strcmp(password, temp.token)) {
			fprintf(stderr, "security error\n");
			continue;
		}

		if (check_logic(&temp) == -1) {
			firstRun = 0;
			fprintf(stderr, "invalid\n");
			continue;
		}
		processLine(temp, fileSize < 10 ? 1 : 0);
		firstRun = 0;
		bzero(interString, MAX * 4);
		argv_free(tempv);
		// FINISH LOGICZ
	}
	cryptWrapper(&temp, ENCRYPT);
}

void processLine(logappend_args args, int32_t isLastLine) {
	int32_t fileSize = 0;
	FILE* mahFile = NULL;
	//Make sure it is a good new first line

	fileSize = fsize(args.logName);
	if (fileSize < 10) {
		mahFile = fopen(args.logName, "w+");
	} else {
		mahFile = fopen(args.logName, "r+");
	}
	NULL_CHECK(mahFile)
	//Write the current line to the file
	if (fileSize < 16) {
		fwrite(args.toString, sizeof(char), strlen(args.toString), mahFile);
	} else {
		fseek(mahFile, 0, SEEK_END);
		fwrite(args.toString, sizeof(char), strlen(args.toString), mahFile);
	}
	fclose(mahFile);
}

