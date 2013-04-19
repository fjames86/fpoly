
#include "memory.h"

stack *make_stack (int n) {
	stack *ret;

	ret = (stack *)malloc (sizeof(stack));
	ret->data = (void **)malloc(sizeof(void *)*n);
	ret->i = 0;
	ret->n = n;

	return ret;
}

void free_stack (stack *s) {
	free(s->data);
	free(s);
}

void stack_push (stack **st, void *x) {
	stack *s, *t;
	int i;
	
	s = *st;
	if (s->i == s->n) {
		/* stack exhausted, need to allocate a bigger one */
		t = make_stack (2*s->n);
		/* copy the data across */
		for(i=0; i < s->n; i++) {
			t->data[i] = s->data[i];
		}
		t->data[s->n] = x;
		/* set the offset counter */
		t->i = s->n+1;

		/* free the old one and replace it with the new one */
		free_stack (*st);
		*st = t;
	} else {
		s->data[s->i] = x;
		s->i++;
	}
}

void *stack_pop (stack *s) {
	void *ret;
	/* if the stack is empty return NULL */
	if (s->i > 0) {
		s->i--;
		ret = s->data[s->i];
	} else {
		ret = NULL;
	}
	return ret;
}

