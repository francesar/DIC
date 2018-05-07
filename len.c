#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>



/**************** STRUCT DEFINITIONS ****************/
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


int_array* len_mat(void *a);


/**************** PRINT FUNCTIONS ****************/
void printlist(void *e) {
	struct int_array *e1_t = *(int_array**)(e);

	printf("%d\n", e1_t->length);
}

/**************** LIST OPERATIONS ****************/
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

int len(void *a) {
	struct int_array *inp_arr = *(int_array**)(a);
	int size = inp_arr->length;
    return size;
}


/**************** MATRIX OPERATIONS ****************/
int_mat* add_mat_int(void* e1, void* e2) {
	struct int_mat *e1_ = *(int_mat**)(e1);
	struct int_mat *e2_ = *(int_mat**)(e2);
	struct int_mat *new_struct = (struct int_mat*) malloc (sizeof(struct int_mat));
	int size = e2_->length;
	int x;
	new_struct->arr = malloc(size * sizeof(int_array));
	for (x = 0; x < size; x++) {
		struct int_array *tmp = (struct int_array*) malloc (sizeof(struct int_array));
		struct int_array *t1 = *((int_array**)(e1_->arr) + x);
		struct int_array *t2 = *((int_array**)(e2_->arr) + x);
		int size = t2->length;
		int z;
		tmp->arr = malloc(size);
		for (z = 0; z < size; z++) {
			*((tmp->arr) + z) = *((t1->arr) + z) + *((t2->arr) + z);
		}
		*((int_array**)(new_struct->arr) + x) = tmp;
		
	}
	return new_struct;
}

int_mat* sub_mat_int(void* e1, void* e2) {
	struct int_mat *e1_ = *(int_mat**)(e1);
	struct int_mat *e2_ = *(int_mat**)(e2);
	struct int_mat *new_struct = (struct int_mat*) malloc (sizeof(struct int_mat));
	int size = e2_->length;
	int x;
	new_struct->arr = malloc(size * sizeof(int_array));
	for (x = 0; x < size; x++) {
		struct int_array *tmp = (struct int_array*) malloc (sizeof(struct int_array));
		struct int_array *t1 = *((int_array**)(e1_->arr) + x);
		struct int_array *t2 = *((int_array**)(e2_->arr) + x);
		int size = t2->length;
		int z;
		tmp->arr = malloc(size);
		for (z = 0; z < size; z++) {
			*((tmp->arr) + z) = *((t1->arr) + z) - *((t2->arr) + z);
		}
		*((int_array**)(new_struct->arr) + x) = tmp;
	}
	return new_struct;
}

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
