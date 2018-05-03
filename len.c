#include <stdio.h>
#include <stdint.h>

typedef struct int_array {
	int length;
	int *arr;
} int_array;

int len(void *a) {
	struct int_array *inp_arr = *(int_array**)(a);
	int size = inp_arr->length;
    return size;
}
