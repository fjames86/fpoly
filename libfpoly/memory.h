
/*
 * Memory management functions for GPUCAS
 * Frank James October 2012
 */
 
#ifndef MEMORY_H
#define MEMORY_H

#include <stdio.h>
#include <stdlib.h>

/* ***************  the stack data structure ******************** */

/* a variable sized array of void pointers */
typedef struct {
	void **data;
	int i;
	int n;
} stack;

stack *make_stack (int n);
void free_stack (stack *s);
void stack_push (stack **s, void *x);
void *stack_pop (stack *s);

#endif
