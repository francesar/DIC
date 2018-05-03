#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct int_array {
	int length;
	int *arr;
} int_array;

int len(void *a) {
	struct int_array *inp_arr = *(int_array**)(a);
	int size = inp_arr->length;

    return size;
}

int_array* append(void *a, int *new_element) {
    struct int_array *passed_in_arr = *(int_array**)(a);
    struct int_array new_struct;

    int old_size = passed_in_arr->length;

    new_struct.length = old_size + 1;

    for(int i = 0; i < old_size; i++) {
        int val = *(passed_in_arr->arr);
        new_struct.arr = &val;

        new_struct.arr++;
        passed_in_arr->arr++;
    }

    new_struct.arr = new_element;

    return &new_struct;
}