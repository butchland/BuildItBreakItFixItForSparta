/*
 * definitions.h
 *
 *  Created on: Aug 29, 2014
 *      Author: kevin
 */

#ifndef DEFINITIONS_H_
#define DEFINITIONS_H_

#include <unistd.h>
#include <stdint.h>
#include <regex.h>

#define MIN(a,b) (((a)<(b))?(a):(b))

#define MAX 256
#define MAX_ONE 255

#define TRY do{ jmp_buf ex_buf__; if( !setjmp(ex_buf__) ){
#define CATCH } else {
#define ETRY } }while(0)
#define THROW longjmp(ex_buf__, 1)

regex_t regex;
int reti;

int32_t encrypted_state;

typedef struct
{
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

}logappend_args;

typedef struct{
	uint32_t roomID;
	uint32_t inRoom;
	uint32_t inBuilding;
	char name[256];
}logicUser;

typedef enum
{
	FUNC_OK,
	INVALID_INPUT,
	INVALID_SIGNATURE,
	INVALID_MOVEMENT
} error_t;


int32_t oldTime;
int32_t isBatch;


#endif /* DEFINITIONS_H_ */
