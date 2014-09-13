
#include "htmlPrint.h"

void printHeader(){
	printf("<html>\n<body>\n<table>\n");
}
void printFooter(){
	printf("</table>\n</body>\n</html>\n");
}

void printEndTableNewTable(){
	printf("</table>\n<table>\n");
}

void printGallery(person * tempE, person * tempG){
	printf("<tr>\n\t<td>%s</td>\n\t<td>%s</td>\n</tr>\n", tempE==NULL? "":tempE->name, tempG==NULL? "":tempG->name);
}

void init_R(){
	printf("<tr>\n<th>Rooms</th>\n</tr>\n");
}
void print_R_element(int32_t * element){
	printf("<tr>\n<th>%d</th>\n</tr>\n", *element);
}
void print_I_element(int32_t * element){
	printf("<tr>\n<td>%d</td>\n</tr>\n", *element);
}
void print_AB_element(char * element){
	printf("<tr>\n<td>%s</td>\n</tr>\n", element);
}
void printSetup_S_2(){
	printf("<table>\n<tr>\n\t<th>Room ID</th>\n\t<th>Occupants</th>\n</tr>\n");
}

