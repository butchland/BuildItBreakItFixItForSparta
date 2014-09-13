#include "list.h"

//stack length
int stack_len(Node *node_head)
{
    Node *curr = node_head;
    int len = 0;

    while(curr)
    {
        ++len;
        curr = curr -> next;
    }
    return len;
}

/* Function to reverse the linked list */
void reverse(Node** head_ref)
{
	Node* prev   = NULL;
	Node* current = *head_ref;
	Node* next;
    while (current != NULL)
    {
        next  = current->next;
        current->next = prev;
        prev = current;
        current = next;
    }
    *head_ref = prev;
}


//pushes a value d onto the stack
void stack_push(Node **node_head, stack_data d)
{
    Node *node_new = malloc(sizeof(Node));

    node_new -> data = d;
    node_new -> next = *node_head;
    *node_head = node_new;
}

//removes the head from the stack & returns its value
stack_data stack_pop(Node **node_head)
{
    Node *node_togo = *node_head;
    stack_data d;

    if(node_head)
    {
        d = node_togo -> data;
        *node_head = node_togo -> next;
        free(node_togo);
    }
    return d;
}

//prints all the stack data
void stack_print(Node **node_head)
{
    Node *node_curr = *node_head;

    if(!node_curr)
        puts("the stack is empty");
    else
    {
        while(node_curr)
        {
            printf("%d ", node_curr -> data); //set for integers, modifiable
            node_curr = node_curr -> next;
        }
        putchar('\n');
    }
}
//clears the stack of all elements
void stack_clear(Node **node_head)
{
    while(*node_head)
        stack_pop(node_head);
}

//appends a node
void stack_snoc(Node **node_head, stack_data d)
{
    Node *node_curr = *node_head;

    if(!node_curr)
        stack_push(node_head, d);
    else
    {
        //find the last node
        while(node_curr -> next)
            node_curr = node_curr -> next;
        //build the node after it
        stack_push(&(node_curr -> next), d);
    }
}
//checks for an element
int stack_elem(Node **node_head, stack_data d)
{
    Node *node_curr = *node_head;

    while(node_curr)
    {
        if(node_curr -> data == d) //set for numbers, modifiable
            return 1;
        else
            node_curr = node_curr -> next;
    }
    return 0;
}
