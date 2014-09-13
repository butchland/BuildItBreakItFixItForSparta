#ifndef ARGS_H_
#define ARGS_H_


#include "definitions.h"
#include <stdint.h>

logappend_args opt_parser(int32_t argc, char **argv, int32_t checkInput);
void * toString(logappend_args* args);

#endif /* ARGS_H_ */
