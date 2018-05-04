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

int_array* add_list(void *e1, void *e2) {
	struct int_array *e1_t = *(int_array**)(e1);
	struct int_array *e2_t = *(int_array**)(e2);
	struct int_array *new_struct = (struct int_array*) malloc (sizeof(struct int_array));
	int size = e2_t->length;
	int x;
	new_struct->arr = malloc(size);
	for (x = 0; x < size; x++) {
		*((new_struct->arr) + x) = *((e1_t->arr) + x) + *((e2_t->arr) + x);
	} 
	return new_struct;
}