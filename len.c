#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

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
	int inner_size = len(inp_array);
	struct int_array *new_struct = (struct int_array*) malloc(sizeof(struct int_array));
	new_struct->length = 2;
	new_struct->arr = malloc(2 * sizeof(int));
	new_struct->arr[0] = outer_size;
	new_struct->arr[1] = inner_size;
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


/**************** LIST OPERATIONS ****************/
bool is_square(int_mat *a) {
    // int outer_dim = a->length;
    // int inner_dim = a->arr->length;

    int_array* size = len_mat(a);
    int outer_dim = size->arr[0];
    int inner_dim = size->arr[1];
    // bool *r = malloc(sizeof(bool));
    bool r;
    if(outer_dim != inner_dim) {
        r = 0;
        return r;
    } else {
        r = 1;
        return r;
    }
}

int_array* add_list_int(int_array* e1, int_array* e2) {
	// struct int_array *e1_t = *(int_array**)(e1);
	// struct int_array *e2_t = *(int_array**)(e2);
	
	
	struct int_array *new_struct = (struct int_array*) malloc (sizeof(struct int_array));
	
	int size = e2->length;
	int x;
	
	new_struct->arr = malloc(size);
	// printf("%d\n", size);
	
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

/**************** MATRIX OPERATIONS ****************/
int_mat* add_mat_int(void* e1, void* e2) {
	// printf("%s", "HERE");
	struct int_mat *e1_ = *(int_mat**)(e1);
	struct int_mat *e2_ = *(int_mat**)(e2);

	int_array* size = len_mat(&e1_);
	
	int outer_size = size->arr[0];
	int inner_size = size->arr[1];
	
	struct int_mat *new_struct = (int_mat*) malloc (sizeof(int_mat));
	new_struct->length = outer_size;
	new_struct->arr = malloc(outer_size * sizeof(int_array));
	
		// struct int_array *new_array = *(int_array**)(new_struct->arr + x);
	int x;
	for (x = 0; x < outer_size; x++) {
		struct int_array *tmp = (int_array*) malloc(sizeof(int_array));
		tmp->arr = malloc(inner_size * sizeof(int));
		tmp = add_list_int(*((int_array**)(e1_->arr) + x), *((int_array**)(e2_->arr) + x));
		// struct int_array *new_array = ;
		// memcpy(((new_struct->arr) + x), tmp, inner_size*sizeof(int));
		*((new_struct->arr) + x) = *tmp;
		printf("%d\n", ((new_struct->arr) + x)->arr[0]);
		printf("%d\n", ((new_struct->arr) + x)->arr[1]);
	}
	*new_struct->arr->arr = 5;
	printf("%d\n", *new_struct->arr->arr);
	return *(int_mat**) new_struct;
}
