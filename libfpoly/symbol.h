

#ifndef SYMBOL_H
#define SYMBOL_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "memory.h"

#define SYMBOL_MINIMUM_STACK        10

typedef char * symbol;

stack *symbol_pool;
symbol symbol_nil;

void symbol_init (int n);
symbol intern (char *str);
symbol lookup (char *str);

#endif
