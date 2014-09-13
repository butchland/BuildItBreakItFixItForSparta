#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <string.h>
#include <unistd.h>
#include "dbg.h"
#include "args.h"
#include "definitions.h"
#include "functions.h"

logread_args opt_parser(int32_t argc, char **argv, int32_t checkInput) {
	int32_t index;
	int32_t c;
	logread_args args;
	args.token = NULL;
	args.employeeName = NULL;
	args.guestName = NULL;
	args.logName = NULL;
	args.inHTML = 0;
	args.currentState = 0;
	args.totalTime = 0;
	args.listAllRooms_R = 0;
	args.printSpecificRooms_I = 0;
	args.listEmployeesWithTime = 0;
	args.listEmployeesWithoutTime = 0;
	args.bounds = malloc(sizeof(times));
	args.bounds->lower = 0;
	args.bounds->upper = 0;
	args.bounds->lower1 = 0;
	args.bounds->upper1 = 0;
	args.peoples_I = NULL;
	opterr = 0;
	int32_t len = 0;
	optind = 0;  //This must occur to have getopt back in its correct state!

	while ((c = getopt(argc, argv, "K:BHSRTAIL:U:E:G:")) != -1) {
		switch (c) {
		case 'H':
			args.inHTML = 1;
			break;
		case 'S':
			args.currentState = 1;
			break;
		case 'R':
			args.listAllRooms_R = 1;
			break;
		case 'T':
			args.totalTime = 1;
			break;
		case 'I':
			args.printSpecificRooms_I = 1;
			break;
		case 'A':
			args.listEmployeesWithTime = 1;
			break;
		case 'B':
			args.listEmployeesWithoutTime = 1;
			break;
		case 'K':
			len = MIN(strlen(optarg), MAX_ONE);
			if (checkInput)
				tokenOpt(optarg);
			args.token = calloc(MAX, 1);
			strncpy(args.token, optarg, len);
			break;
		case 'E':
			len = MIN(strlen(optarg), MAX_ONE);
			if (checkInput)
				nameOpt(optarg);
			if (args.printSpecificRooms_I) {
				person * currPerson = calloc(1, sizeof(person));
				strncpy(currPerson->name, optarg, len);
				currPerson->isEmployee = 1;
				stack_push(&args.peoples_I, currPerson);
			} else {
				args.employeeName = calloc(MAX, 1);
				strncpy(args.employeeName, optarg, len);
			}
			break;
		case 'G':
			len = MIN(strlen(optarg), MAX_ONE);
			if (checkInput)
				nameOpt(optarg);
			if (args.printSpecificRooms_I) {
				person * currPerson = calloc(1, sizeof(person));
				strncpy(currPerson->name, optarg, len);
				currPerson->isEmployee = 0;
				stack_push(&args.peoples_I, currPerson);
			} else {
				args.guestName = calloc(MAX, 1);
				strncpy(args.guestName, optarg, len);
			}
			break;
		case 'L':
			if (checkInput)
				numOpt(optarg);
			if (args.listEmployeesWithoutTime
					&& args.bounds->upper > args.bounds->lower) {
				args.bounds->lower1 = atoi(optarg);
			} else {
				args.bounds->lower = atoi(optarg);
			}
			break;
		case 'U':
			if (checkInput)
				numOpt(optarg);
			if (args.listEmployeesWithoutTime
					&& args.bounds->upper > args.bounds->lower) {
				args.bounds->upper1 = atoi(optarg);
			} else {
				args.bounds->upper = atoi(optarg);
			}
			break;
		case '?':
			invalid();
			break;
		default:
			invalid();
		}
	}

	index = optind;
	if (index < argc) {
		args.logName = argv[index];
	}
	int32_t exclusive_options = args.currentState + args.listAllRooms_R
			+ args.totalTime + args.printSpecificRooms_I
			+ args.listEmployeesWithTime + args.listEmployeesWithoutTime;
	if (exclusive_options > 1 || exclusive_options < 1)
		invalid();

	if(args.listAllRooms_R == 1 && args.employeeName == NULL && args.guestName == NULL)
		invalid();

	//Regex will filter out any negative numbers.  Hurrah
	if (args.listEmployeesWithTime && args.bounds->lower >= args.bounds->upper)
		invalid();
	if (args.listEmployeesWithoutTime
			&& args.bounds->lower1 >= args.bounds->upper1)
		invalid();

	return args;
}

