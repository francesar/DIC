#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct int_array {
	int length;
	int *arr;
} int_array;


typedef struct float_array {
	int length;
	double *arr;
} float_array;

typedef struct int_mat {
	int length;
	struct int_array *arr;
} int_mat;

typedef struct float_mat {
	int length;
	struct float_array *arr;
} float_mat;

int len(void *a) {
	struct int_array *inp_arr = *(int_array**)(a);
	int size = inp_arr->length;

    return size;
}

int_array* len_mat(void *a) {
	struct int_mat *inp_mat = *(int_mat**)(a);
	int outer_size = inp_mat->length;
	struct int_array *inp_array = inp_mat->arr;
	int inner_size = inp_array->length;
	struct int_array *new_struct = (struct int_array*) malloc(sizeof(struct int_array));
	new_struct->length = 2;
	new_struct->arr = malloc(2);
	// new_struct[0] = outer_size;
	// new_struct[1] = inner_size;
	return new_struct;
}

int_array* append(void *a, void *new_element) {
    struct int_array *passed_in_arr = *(int_array**)(a);
    struct int_array *new_struct = (struct int_array*) malloc(sizeof(struct int_array));

    int old_size = passed_in_arr->length;

    new_struct->length = old_size + 1;

    new_struct->arr = malloc(new_struct->length);
    int i;
    for(i = 0; i < old_size; i++) {
        int val = passed_in_arr->arr[i];
        new_struct->arr[i] = val;
    }
    
    int new_int = *(int *)new_element;
    int last_pos = new_struct->length-1;

    new_struct->arr[last_pos] = new_int;

    return new_struct;
}

int_array* add_list_int(int_array* e1, int_array* e2) {
	// struct int_array *e1_t = *(int_array**)(e1);
	// struct int_array *e2_t = *(int_array**)(e2);
	struct int_array *new_struct = (struct int_array*) malloc (sizeof(struct int_array));
	int size = e2->length;
	int x;
	new_struct->arr = malloc(size);
	for (x = 0; x < size; x++) {
		*((new_struct->arr) + x) = *((e1->arr) + x) + *((e2->arr) + x);
	} 
	return new_struct;
}

int_array* sub_list_int(int_array* e1, int_array* e2) {
	struct int_array *new_struct = (struct int_array*) malloc (sizeof(struct int_array));
	int size = e2->length;
	int x;
	new_struct->arr = malloc(size);
	for (x = 0; x < size; x++) {
		*((new_struct->arr) + x) = *((e1->arr) + x) - *((e2->arr) + x);
	} 
	return new_struct;
}

float_array* add_list_float(float_array* e1, float_array* e2) {
	// struct int_array *e1_t = *(int_array**)(e1);
	// struct int_array *e2_t = *(int_array**)(e2);
	struct float_array *new_struct = (struct float_array*) malloc (sizeof(struct float_array));
	int size = e2->length;
	int x;
	new_struct->arr = malloc(size);
	for (x = 0; x < size; x++) {
		*((new_struct->arr) + x) = *((e1->arr) + x) + *((e2->arr) + x);
	} 
	return new_struct;
}

float_array* sub_list_float(float_array* e1, float_array* e2) {
	// struct int_array *e1_t = *(int_array**)(e1);
	// struct int_array *e2_t = *(int_array**)(e2);
	struct float_array *new_struct = (struct float_array*) malloc (sizeof(struct float_array));
	int size = e2->length;
	int x;
	new_struct->arr = malloc(size);
	for (x = 0; x < size; x++) {
		*((new_struct->arr) + x) = *((e1->arr) + x) - *((e2->arr) + x);
	} 
	return new_struct;
}
