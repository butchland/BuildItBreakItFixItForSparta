/*
 * definitions.h
 *
 *  Created on: Sep 1, 2014
 *      Author: kevin
 */

#ifndef DEFINITIONS_H_
#define DEFINITIONS_H_

#include <stdint.h>
#include "hash.h"
#include "list.h"
#include <regex.h>

#define MAX 256
#define MAX_ONE 255

#define MIN(a,b) (((a)<(b))?(a):(b))

#define TRY do{ jmp_buf ex_buf__; if( !setjmp(ex_buf__) ){
#define CATCH } else {
#define ETRY } }while(0)
#define THROW longjmp(ex_buf__, 1)

regex_t regex;
int reti;


typedef struct Person {
	uint32_t isEmployee;
	int32_t roomID;
	uint32_t enterTime;
	int32_t leaveTime;
	uint32_t inBuilding;
	Node * rooms;
	char name[MAX];

} person;

typedef struct{
	uint32_t lower;
	uint32_t upper;
	uint32_t lower1;
	uint32_t upper1;
}times;

typedef struct {
	uint32_t inHTML;
	uint32_t currentState;
	uint32_t totalTime;
	uint32_t listAllRooms_R;
	uint32_t printSpecificRooms_I;
	uint32_t listEmployeesWithTime;
	uint32_t listEmployeesWithoutTime;
	times * bounds;
	char * token;
	char * employeeName;
	char * guestName;
	char * logName;
	Node * peoples_I;

} logread_args;

typedef struct {
	/*
	 *   Positive number of seconds since gallery opened.  Time should always increase.
	 *   Invoking logappend with event prior to time of most recent log is an error
	 */
	int32_t timestamp;
	/*
	 *	Token used to authenticate log.  Consists of arbitrary-sized string of alphanumeric (a-aA-Z0-9) characters.
	 *	Once a log is created, subsequent logs must use same token.
	 */
	char * token;
	/*
	 * Upper and lower case letters only.  No spaces.  This applies to both guestName and employeeName which are mutually exclusive.
	 */
	char * employeeName;
	char * guestName;
	/*
	 * Can be used with the name options as well as the room ID options.  In no room ID is defined, assume -A
	 * means that they arrived at the gallery as a whole. Should never enter a room without having left the last room they entered.
	 */
	int32_t eventArrival;
	/*
	 * Can be used with the name options as well as the room ID options.  In no room ID is defined, assume -L
	 * means that they left the building. Cannot leave the gallery without last leaving the room they entered.
	 */
	int32_t eventDeparture;
	/*
	 * None-negative integer characters with no spaces.  A
	 */
	int32_t roomID;
	/*
	 * Path to file containing log
	 */
	char * logName;
	/*
	 * Path to file containing list of commands to run
	 */
	char * batchFile;

	int returnStatus;

	//String representation
	char * toString;

} logappend_args;

#endif /* DEFINITIONS_H_ */
