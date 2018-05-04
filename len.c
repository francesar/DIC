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

int_array* append(void *a, void *new_element) {
    struct int_array *passed_in_arr = *(int_array**)(a);
    struct int_array *new_struct = (struct int_array*) malloc(sizeof(struct int_array));

    int old_size = passed_in_arr->length;

    new_struct->length = old_size + 1;

    new_struct->arr = malloc(new_struct->length);

    for(int i = 0; i < old_size; i++) {
        int val = passed_in_arr->arr[i];
        new_struct->arr[i] = val;
    }
    
    int new_int = *(int *)new_element;
    int last_pos = new_struct->length-1;

    new_struct->arr[last_pos] = new_int;

    return new_struct;
}