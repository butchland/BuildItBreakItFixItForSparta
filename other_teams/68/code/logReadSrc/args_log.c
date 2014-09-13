#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <string.h>
#include <unistd.h>
#include "dbg.h"
#include "args_log.h"
#include "functions.h"
#include "definitions.h"

logappend_args opt_parser_log(int32_t argc, char **argv) {
	int32_t index;
	int32_t c;
	logappend_args args;
	args.token = NULL;
	args.employeeName = NULL;
	args.guestName = NULL;
	args.logName = NULL;
	args.batchFile = NULL;
	args.eventArrival = -1;
	args.eventDeparture = -1;
	args.roomID = -1;
	args.timestamp = -1;
	args.toString = NULL;
	opterr = 0;
	int32_t len = 0;
	optind = 0;  //This must occur to have getopt back in its correct state!

//No need to regex parse stored names/numbers since they would only have been stored if they were valid

	while ((c = getopt(argc, argv, "T:B:K:E:G:ALR:")) != -1) {
		//debug("%c", c);
		switch (c) {
		case 'T':
			//numOpt(optarg);
			args.timestamp = atoi(optarg);
			break;
		case 'B':
			len = MIN(strlen(optarg), MAX_ONE);
			args.batchFile = calloc(MAX, 1);
			nameOpt(optarg);
			strncpy(args.batchFile, optarg, len);
			break;
		case 'K':
			len = MIN(strlen(optarg), MAX_ONE);
			args.token = calloc(MAX, 1);
			//nameOpt(optarg);
			strncpy(args.token, optarg, len);
			break;
		case 'E':
			len = MIN(strlen(optarg), MAX_ONE);
			args.employeeName = calloc(MAX, 1);
			//nameOpt(optarg);
			strncpy(args.employeeName, optarg, len);
			break;
		case 'G':
			len = MIN(strlen(optarg), MAX_ONE);
			args.guestName = calloc(MAX, 1);
			//nameOpt(optarg);
			strncpy(args.guestName, optarg, len);
			break;
		case 'A':
			args.eventArrival = 1;
			break;
		case 'L':
			args.eventDeparture = 1;
			break;
		case 'R':
			numOpt(optarg);
			args.roomID = atoi(optarg);
			break;
		case '?':
			invalid();
			break;
		default:
			invalid();
		}
	}

	index = optind;

	if (args.batchFile != NULL) {

	} else if (args.timestamp
			== -1|| args.token == NULL || args.logName == NULL) {

	} else if ((args.eventArrival == -1 && args.eventDeparture == -1)
			|| (args.eventArrival == 1 && args.eventDeparture == 1)) {
		invalid();
	} else if ((args.employeeName == NULL && args.guestName == NULL)
			|| (args.employeeName != NULL && args.guestName != NULL)) {
		invalid();
	} else if (args.roomID == -1
			&& !(args.eventDeparture == 1 || args.eventArrival == 1)) {
		invalid();
	} else {
		args.returnStatus = 0;
	}
	toString(&args);
	return args;
}

void * toString(logappend_args* args) {
	char * string = calloc(MAX * 4, 1);
	char str[15];
	if (args->batchFile != NULL) {
		sprintf(string, "-B %s ", args->batchFile);
	} else {
		sprintf(string, "-T %d ", args->timestamp);
		if (args->eventArrival == -1) {
			strcat(string, "-L ");
		} else {
			strcat(string, "-A ");
		}
		if (args->employeeName == NULL) {
			strcat(string, "-E ");
			strcat(string, args->guestName);
		} else {
			strcat(string, "-G ");
			strcat(string, args->employeeName);
		}
		if (args->roomID != -1) {
			strcat(string, " -R ");
			sprintf(str, "%d", args->roomID);
			strcat(string, str);
		}
	}
	strcat(string, "\n");
	args->toString = string;
	return 0;
}
