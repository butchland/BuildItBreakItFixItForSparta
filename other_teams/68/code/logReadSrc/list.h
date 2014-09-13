/*
 * list.h
 *
 *  Created on: Sep 1, 2014
 *      Author: kevin
 */

#ifndef LIST_H_
#define LIST_H_

#include <stdio.h>
#include <stdlib.h>


typedef void * stack_data; //stack data type
typedef struct stack Node; //short name for the stack type
struct stack //stack structure format
{
    stack_data data;
    Node *next;
};

int stack_len(Node *node_head); //stack length
void reverse(Node** head_ref);
void stack_push(Node **node_head, stack_data d); //pushes a value d onto the stack
stack_data stack_pop(Node **node_head); //removes the head from the stack & returns its value
void stack_print(Node **node_head); //prints all the stack data
void stack_clear(Node **node_head); //clears the stack of all elements
void stack_snoc(Node **node_head, stack_data d); //appends a node
int stack_elem(Node **node_head, stack_data d); //checks for an element

#endif /* LIST_H_ */
