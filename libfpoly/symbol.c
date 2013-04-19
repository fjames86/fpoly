
#include "symbol.h"

void symbol_init (int n) {

	if (n < SYMBOL_MINIMUM_STACK) {
		symbol_pool = make_stack (SYMBOL_MINIMUM_STACK);
	} else {
		symbol_pool = make_stack (n);
	}

	symbol_nil = intern ("NIL");
}

/*
 * take a character string and look it up in our list of symbols that we've already seen.
 * if it's not yet interned then we add it to our dictionary and return it
 */
symbol intern (char *str) {
	symbol s;
	char *c;
	int i, len;
	
	s = lookup (str);
	if (s == NULL) {
		/* this is a new symbol, so we need to add it to our stack */
		len = strlen (str);
		c = (char *)malloc (sizeof (char)*(len+1));
		for (i=0; i < len; i++) c[i] = str[i]; /* toupper (str[i]);*/
		c[len] = '\0';
		s = (symbol)c;
		stack_push (&symbol_pool, s);
	}
	
	return s;
}

/*
 * go through the symbol stack, comparing the interned symbols. If we find this one then it's already
 * been interned and therefore we simply return the old one
 * else we just return NULL, indicating that it's new
*/
symbol lookup (char *str) {
	int i;
	for (i=0; i<symbol_pool->i; i++) {
		if (strcmp (((char *)symbol_pool->data[i]), str) == 0) {
			return (symbol)symbol_pool->data[i];
		}
	}
	return NULL;
}

