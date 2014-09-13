/*
 * argv.h
 *
 *  Created on: Aug 31, 2014
 *      Author: kevin
 */

#ifndef ARGV_H_
#define ARGV_H_

int count_argc(const char *str);
void argv_free(char **argv);
char **argv_split(const char *str, int *argcp);

#endif /* ARGV_H_ */
