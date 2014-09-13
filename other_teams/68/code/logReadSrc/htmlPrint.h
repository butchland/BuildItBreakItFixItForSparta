/*
 * htmlPrint.h
 *
 *  Created on: Sep 2, 2014
 *      Author: kevin
 */

#ifndef HTMLPRINT_H_
#define HTMLPRINT_H_

#include "definitions.h"
#include <stdio.h>


void printHeader();
void printFooter();
void printEndTableNewTable();
void printGallery(person * tempE, person * tempG);
void init_R();
void print_R_element(int32_t * element);
void print_AB_element(char * element);
void printSetup_S_2();
void print_I_element(int32_t * element);

#endif /* HTMLPRINT_H_ */
