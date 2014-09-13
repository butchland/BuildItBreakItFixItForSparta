#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <openssl/md5.h>
#include <stdlib.h>
#include <openssl/evp.h>
#include <openssl/aes.h>
#include "args.h"
#include "definitions.h"
#include "functions.h"
#include "args_log.h"
#include "argv.h"
#include "hash.h"
#include "dictionary.h"
#include "htmlPrint.h"

#define DECRYPT 0
#define ENCRYPT 1

#define NULL_CHECK(val)  if (val == NULL) invalid_0();
#define NULL_CHECK_INV(val)  if (val == NULL) invalid_check(args);

void buildDataStructs(logappend_args *temp);
void doBadThings(logread_args* args);
void sortLinkedList_Names(Node *head);
void whyIsTheHTMLFormatDifferent_S(logread_args* args);
void sortLinkedList_Nums(Node *head);

HT* allMahHashes_employees;
HT* allMahHashes_guests;
Node *peopleHead;
int32_t highestRoomNum;
int32_t lastTime;

int main(int argc, char * argv[]) {
	peopleHead = NULL;
	allMahHashes_employees = ht_create(65536);
	allMahHashes_guests = ht_create(65536);
	int32_t fileSize = 0;
	FILE* file = NULL;
	size_t bytes = 0;
	ssize_t read = 0;
	char * line = NULL;
	char interString[MAX * 4];

	//get Logreader arguements
	logread_args args = opt_parser(argc, argv, 1);
	//Verify integrity and hopefully the syntax should be right

	fileSize = fsize(args.logName);
	if (fileSize > 15) {
		unsigned int salt[] = { 12345, 54321 };
		FILE * encrypted_file = fopen(args.logName, "r");
		FILE * decrypted = fopen("tempblahman", "w+");
		do_crypt(encrypted_file, decrypted, DECRYPT, args.token,
				strlen(args.token), (unsigned char *) salt);
		rename("tempblahman", args.logName);
	} else {
		invalid();
	}

	file = fopen(args.logName, "r");
	//Line by line apply the options
	while ((read = getline(&line, &bytes, file)) != -1 && fileSize > 10) {

		int len = strlen(line);
		fileSize = fileSize - len;
		// RERUN COMMANDS CAUZE LOGIC!
		sprintf(interString, "./logappend %s", line);
		int tempc;
		char ** tempv = argv_split(interString, &tempc);
		logappend_args temp = opt_parser_log(tempc, tempv);
		buildDataStructs(&temp);
		bzero(interString, MAX * 4);
		argv_free(tempv);
		// FINISH LOGICZ
	}
	doBadThings(&args);

	unsigned int salt[] = { 12345, 54321 };
	FILE * decrypted_file = fopen(args.logName, "r");
	FILE * encrypted = fopen("tempblahman", "w+");
	do_crypt(decrypted_file, encrypted, ENCRYPT, args.token, strlen(args.token),
			(unsigned char *) salt);
	rename("tempblahman", args.logName);

	return 0;
}

void buildDataStructs(logappend_args *temp) {
	int32_t isemployee = 0;
	char * name = NULL;
	person* currPerson;
	if (temp->employeeName == NULL) {
		name = temp->guestName;
		currPerson = ht_get(allMahHashes_guests, name);
	} else {
		name = temp->employeeName;
		currPerson = ht_get(allMahHashes_employees, name);
		isemployee = 1;
	}

	if (currPerson == NULL) {
		currPerson = calloc(1, sizeof(person));
		currPerson->rooms = NULL;
		strcpy(currPerson->name, name);
		currPerson->isEmployee = isemployee;
		currPerson->roomID = -1;
		currPerson->inBuilding = 1;
		currPerson->leaveTime = -1;
		currPerson->enterTime = temp->timestamp;
		if (isemployee) {
			ht_put(allMahHashes_employees, currPerson->name, currPerson);
		} else {
			ht_put(allMahHashes_guests, currPerson->name, currPerson);
		}
		stack_push(&peopleHead, currPerson);
	} else if (temp->eventArrival == 1 && temp->roomID != -1) {
		currPerson->roomID = temp->roomID;
		int32_t * tempNum = calloc(1, sizeof(int32_t));
		*tempNum = temp->roomID;
		stack_push(&currPerson->rooms, tempNum);
		if (currPerson->roomID > highestRoomNum)
			highestRoomNum = currPerson->roomID;
	} else if (temp->eventDeparture == 1 && temp->roomID == -1) {
		currPerson->leaveTime = temp->timestamp;
		currPerson->inBuilding = 0;
	} else {
		currPerson->roomID = -1;
	}
	lastTime = temp->timestamp;

}

void doBadThings(logread_args* args) {
	sortLinkedList_Names(peopleHead);
	int32_t currRoom;
	uint32_t isFirst = 1;
	if (args->currentState) {
		if (args->inHTML) {
			whyIsTheHTMLFormatDifferent_S(args);
			return;
		}
		Node* temp = peopleHead;
		while (temp) {
			person* tempP = (person *) (temp->data);
			if (tempP->isEmployee && tempP->inBuilding) {
				if (!isFirst)
					printf(",");
				isFirst = 0;
				printf("%s", tempP->name);
			}
			temp = temp->next;

		}
		isFirst = 1;
		temp = peopleHead;
		printf("\n");
		while (temp) {
			person* tempP = (person *) (temp->data);
			if (!tempP->isEmployee && tempP->inBuilding) {
				if (!isFirst)
					printf(",");
				isFirst = 0;
				printf("%s", tempP->name);
			}
			temp = temp->next;
		}
		printf("\n");
		for (currRoom = 0; currRoom <= highestRoomNum; currRoom++) {
			isFirst = 1;
			Node* temp = peopleHead;
			while (temp) {
				person* tempP = (person *) (temp->data);
				if (tempP->roomID == currRoom && tempP->inBuilding) {
					if (isFirst)
						printf("%d: ", currRoom);
					if (!isFirst)
						printf(",");
					isFirst = 0;
					printf("%s", tempP->name);

				}
				temp = temp->next;
			}
			if (!isFirst)
				printf("\n");
		}

	} else if (args->listAllRooms_R) {
		person* blahzz;
		if (args->employeeName != NULL) {
			blahzz = ht_get(allMahHashes_employees, args->employeeName);
		} else {
			blahzz = ht_get(allMahHashes_guests, args->guestName);
		}
		Node* temp = blahzz == NULL ? NULL : blahzz->rooms;
		if (temp)
			reverse(&temp);
		uint32_t isFirst = 1;
		if (args->inHTML)
			printHeader();
		while (temp) {
			int32_t* num = (int32_t*) (temp->data);
			if (args->inHTML) {
				if (isFirst)
					init_R();
				print_R_element(num);
			} else {
				if (!isFirst)
					printf(",");

				printf("%d", *num);
			}
			isFirst = 0;
			temp = temp->next;
		}
		if (args->inHTML)
			printFooter();
	} else if (args->totalTime) {

		if (args->inHTML)
			invalid_check(args);
		person* blahzz;
		if (args->employeeName != NULL) {
			blahzz = ht_get(allMahHashes_employees, args->employeeName);
		} else {
			blahzz = ht_get(allMahHashes_guests, args->guestName);
		}

		int32_t timespent;
		if (blahzz != NULL && blahzz->leaveTime == -1) {
			timespent = lastTime - blahzz->enterTime;
		} else {
			timespent = blahzz->leaveTime - blahzz->enterTime;
		}
		if (blahzz != NULL)
			printf("%d", timespent);

	} else if (args->listEmployeesWithTime
			&& args->bounds->upper > args->bounds->lower) {
		Node* temp = peopleHead;
		if (args->inHTML) {
			printHeader();
			printf("<tr>\n<th>Employees</th>\n</tr>\n");
		}
		while (temp) {
			person* tempP = (person *) (temp->data);
			if (tempP->isEmployee
					&& (tempP->enterTime <= args->bounds->upper
							&& (tempP->leaveTime >= args->bounds->lower
									|| tempP->leaveTime == -1))) {
				if (!isFirst && !args->inHTML) {
					printf(",");
				}

				isFirst = 0;
				if (!args->inHTML) {
					printf("%s", tempP->name);
				} else {
					print_AB_element(tempP->name);
				}

			}
			temp = temp->next;

		}
		if (args->inHTML)
			printFooter();
	} else if (args->listEmployeesWithoutTime
			&& args->bounds->upper > args->bounds->lower
			&& args->bounds->upper1 > args->bounds->lower1) {
		Node* temp = peopleHead;
		if (args->inHTML) {
			printHeader();
			printf("<tr>\n<th>Employees</th>\n</tr>\n");
		}
		while (temp) {
			person* tempP = (person *) (temp->data);
			if (tempP->isEmployee
					&& (tempP->enterTime <= args->bounds->upper
							&& (tempP->leaveTime >= args->bounds->lower
									|| tempP->leaveTime == -1))
					&& ((tempP->enterTime > args->bounds->upper1
							&& tempP->leaveTime > args->bounds->upper1)
							|| (tempP->leaveTime < args->bounds->lower1
									&& tempP->enterTime < args->bounds->lower1))) {
				if (!isFirst && !args->inHTML) {
					printf(",");
				}

				isFirst = 0;
				if (!args->inHTML) {
					printf("%s", tempP->name);
				} else {
					print_AB_element(tempP->name);
				}

			}
			temp = temp->next;

		}
		if (args->inHTML)
			printFooter();
	} else if (args->printSpecificRooms_I) {
		Node * temp = args->peoples_I;
		Node * roomList = NULL;
		Node * oldList = NULL;
		while (temp) {
			person * tempPerson = (person *) temp->data;
			person* currPerson =
					tempPerson->isEmployee ?
							ht_get(allMahHashes_employees, tempPerson->name) :
							ht_get(allMahHashes_guests, tempPerson->name);

			if (currPerson) {
				if (isFirst) {
					roomList = currPerson->rooms;
					sortLinkedList_Nums(roomList);
				} else {
					oldList = roomList;
					roomList = NULL;
					while (oldList && currPerson->rooms) {
						int32_t * tempAA = oldList->data;
						int32_t * tempBB = currPerson->rooms->data;
						if (*tempAA == *tempBB) {
							int32_t * tempNum = calloc(1, sizeof(int32_t));
							*tempNum = *tempAA;
							stack_push(&roomList, tempNum);
							oldList = oldList->next;
							currPerson->rooms = currPerson->rooms->next;
						} else if (*tempAA < *tempBB) {
							oldList = oldList->next;
						} else {
							currPerson->rooms = currPerson->rooms->next;
						}
					}
				}
			}
			temp = temp->next;
		}
		isFirst = 1;
		if (args->inHTML) {
			printHeader();
			printf("<tr>\n<th>Rooms</th>\n</tr>\n");
		}
		while (roomList) {
			if (!isFirst && !args->inHTML)
				printf(",");
			int32_t * tempNum = roomList->data;
			if (!args->inHTML) {
				printf("%d", *tempNum);
			} else {
				print_I_element(tempNum);
			}

			isFirst = 0;
			roomList = roomList->next;
		}
		if (args->inHTML)
			printFooter();
	}
	fflush(stdout);

}

void columns(Node* temp_E, Node* temp_G) {
	person* tempE = temp_E == NULL ? NULL : (person *) (temp_E->data);
	person* tempG = temp_G == NULL ? NULL : (person *) (temp_G->data);
	if (!temp_E && !temp_G) {
		return;
	} else if (temp_E && !tempE->isEmployee) {
		columns(temp_E->next, temp_G);
	} else if (temp_G && tempG->isEmployee) {
		columns(temp_E, temp_G->next);
	} else if (temp_E == NULL && temp_G && !tempG->isEmployee) {
		printGallery(tempE, tempG);
		if (temp_G->next != NULL)
			printEndTableNewTable();
		printEndTableNewTable();
		columns(temp_E, temp_G->next);
	} else if (temp_G == NULL && temp_E && tempE->isEmployee) {
		printGallery(tempE, tempG);
		if (temp_E->next != NULL)
			printEndTableNewTable();
		columns(temp_E->next, temp_G);
	} else {
		printGallery(tempE, tempG);
		if (temp_E->next != NULL && temp_G->next != NULL)
			printEndTableNewTable();
		columns(temp_E->next, temp_G->next);
	}
}

void whyIsTheHTMLFormatDifferent_S(logread_args* args) {
	Node* temp_E = peopleHead;
	Node* temp_G = peopleHead;
	columns(temp_E, temp_G);
	uint32_t currRoom;
	printSetup_S_2();
	uint32_t isFirst;
	for (currRoom = 0; currRoom <= highestRoomNum; currRoom++) {
		isFirst = 1;
		Node* temp = peopleHead;
		while (temp) {
			person* tempP = (person *) (temp->data);
			if (tempP->roomID == currRoom) {
				if (isFirst)
					printf("<tr>\n\t<td>%d</td>\n\t<td>", currRoom);
				if (!isFirst)
					printf(",");
				isFirst = 0;
				printf("%s", tempP->name);

			}
			temp = temp->next;
		}
		if (!isFirst)
			printf("</td>\n</tr>\n");
	}

	printFooter();

}

void sortLinkedList_Names(Node *head) {
	Node *temp;
	int i;
	/* since the compare dereferences temp->next you'll have to verify that it is not NULL */
	for (i = 0; i < stack_len(head); i++) {
		for (temp = head; temp && temp->next; temp = temp->next) {
			person* blah = (person *) temp->data;
			person * blahNext = (person *) temp->next->data;
			if (strcmp(blah->name, blahNext->name) > 0) {
				/* no need for a whole node, since you only copy a pointer */
				person *cp;
				cp = temp->data;
				temp->data = temp->next->data;
				temp->next->data = cp;
			}

		}
	}

}

void sortLinkedList_Nums(Node *head) {
	Node *temp;
	int i;
	/* since the compare dereferences temp->next you'll have to verify that it is not NULL */
	for (i = 0; i < stack_len(head); i++) {
		for (temp = head; temp && temp->next; temp = temp->next) {
			int32_t* blah = (int32_t *) temp->data;
			int32_t * blahNext = (int32_t *) temp->next->data;
			if (*blah > *blahNext) {
				/* no need for a whole node, since you only copy a pointer */
				int32_t *cp;
				cp = temp->data;
				temp->data = temp->next->data;
				temp->next->data = cp;
			}

		}
	}

}
