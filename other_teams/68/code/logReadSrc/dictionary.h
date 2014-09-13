/*
 * dictionary.h
 *
 *  Created on: Sep 1, 2014
 *      Author: kevin
 */

#ifndef DICTIONARY_H_
#define DICTIONARY_H_

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

typedef struct tree_node {
	const char *word;
	struct tree_node *left;
	struct tree_node *right;
} tree_node;

void print_tree(tree_node *dictionary);
int find_word(tree_node *dictionary, char *word);
void insert(tree_node *root, char *word);
char *strip(char **word);

#endif /* DICTIONARY_H_ */
