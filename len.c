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



/**************** FUNCTION DECLARATIONS ****************/
void print_intlist_return (void *e, bool pretty);
void print_intlist(void *e);
void print_floatlist_return (void *e, bool pretty);
void print_floatlist(void *e);
void print_intmat(void *e, bool pretty);
void print_floatmat(void *e, bool pretty);

int_array* add_list_int(int_array* e1, int_array* e2);
int_array* sub_list_int(int_array* e1, int_array* e2);
float_array* add_list_float(float_array* e1, float_array* e2);
float_array* sub_list_float(float_array* e1, float_array* e2);
int_array* append(void *a, void *new_element);
int len(void *a);

int_mat* add_mat_int(void* e1, void* e2);
int_mat* sub_mat_int(void* e1, void* e2);
int_mat* mult_mat_int(void* e1, void* e2);
float_mat* add_mat_float(void* e1, void* e2);
float_mat* sub_mat_float(void* e1, void* e2);
bool is_square(int_mat *a);
int_array* len_mat(void *a);
int** store_array(void *e1);
int_mat* transpose_int (void *e);
int determinant_int(void *e1);
int det_helper(int n, int a[][n]);

/**************** PRINT FUNCTIONS ****************/
void print_intlist_return (void *e, bool pretty) {
	struct int_array *e_ = *(int_array**)(e);
	int size = e_->length;
	int x;
	printf("%s", "[");
	printf("%d", *((e_->arr)));
	for (x = 1; x < size; x++) {
		if (pretty)
			printf("%s", "\t");
		else
			printf("%s", ", ");
		printf("%d", *((e_->arr) + x));
	}
	printf("%s", "]");
}

void print_intlist(void *e) {
	print_intlist_return(e, false);
	printf("\n");	
}


void print_floatlist_return (void *e, bool pretty) {
	struct float_array *e_ = *(float_array**)(e);
	int size = e_->length;
	int x;
	printf("%s", "[");
	printf("%f", *((e_->arr)));
	for (x = 1; x < size; x++) {
		if (pretty)
			printf("%s", "\t");
		else
			printf("%s", ", ");
		printf("%f", *((e_->arr) + x));
	}
	printf("%s", "]");
}

void print_floatlist(void *e) {
	print_floatlist_return(e, false);
	printf("\n");
}


void print_intmat(void *e, bool pretty) {
	struct int_mat *e_ = *(int_mat**)(e);
	int size = e_->length;
	int x;
	if (!pretty) {
		printf("%s", "[");	
		print_intlist_return(((int_array**)(e_->arr)), false);
	} else {
		print_intlist_return(((int_array**)(e_->arr)), true);
	}
	
	if (pretty) printf("\n");
	for (x = 1; x < size; x++) {		
		
		struct int_array *e1_ = *((int_array**)(e_->arr) + x);
		
		if (pretty) {
			print_intlist_return(((int_array**)(e_->arr) + x), true);
			printf("\n");			
		} else {
			printf("%s", ", ");
			print_intlist_return(((int_array**)(e_->arr) + x), false);	
		}
		
	}
	if (!pretty) printf("%s", "]\n"); else printf("\n");
}


void print_floatmat(void *e, bool pretty) {
	struct float_mat *e_ = *(float_mat**)(e);
	int size = e_->length;
	int x;
	if (!pretty) {
		printf("%s", "[");
		print_floatlist_return(((float_array**)(e_->arr)), false);
	} else {
		print_floatlist_return(((float_array**)(e_->arr)), true);
	}
	if (pretty) printf("\n");
	for (x = 1; x < size; x++) {		
		struct float_array *e1_ = *((float_array**)(e_->arr) + x);

		if (pretty) {
			print_floatlist_return(((float_array**)(e_->arr) + x), true);
			printf("\n");
		} else {
			printf("%s", ", ");
			print_floatlist_return(((float_array**)(e_->arr) + x), false);
		}
	}
	if (!pretty) printf("%s", "]\n"); else printf("\n");
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
	new_struct->length = size;
	new_struct->arr = malloc(size * sizeof(int_array));
	for (x = 0; x < size; x++) {
		struct int_array *tmp = (struct int_array*) malloc (sizeof(struct int_array));
		struct int_array *t1 = *((int_array**)(e1_->arr) + x);
		struct int_array *t2 = *((int_array**)(e2_->arr) + x);
		int inner_size = t2->length;
		int z;
		tmp->length = inner_size;
		tmp->arr = malloc(inner_size);
		for (z = 0; z < inner_size; z++) {
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
	new_struct->length = size;
	new_struct->arr = malloc(size * sizeof(int_array));
	for (x = 0; x < size; x++) {
		struct int_array *tmp = (struct int_array*) malloc (sizeof(struct int_array));
		struct int_array *t1 = *((int_array**)(e1_->arr) + x);
		struct int_array *t2 = *((int_array**)(e2_->arr) + x);
		int size = t2->length;
		int z;
		tmp->length = size;
		tmp->arr = malloc(size);
		for (z = 0; z < size; z++) {
			*((tmp->arr) + z) = *((t1->arr) + z) - *((t2->arr) + z);
		}
		*((int_array**)(new_struct->arr) + x) = tmp;
	}
	return new_struct;
}

int_mat* mult_mat_int(void* e1, void* e2) {
	// struct int_mat *e1_llvm = *(int_mat**)(e1);
	// struct int_mat *e2_ = *(int_mat**)(e2);

	// Create C arrays for e1 and e2
	int **e1_ = store_array(e1);
	int **e2_ = store_array(e2);

	int_array *e1_sizes = len_mat(e1);
	int_array *e2_sizes = len_mat(e2);
	int m1 = e1_sizes->arr[0];
	int m2 = e1_sizes->arr[1];
	int n1 = e2_sizes->arr[0];
	int n2 = e2_sizes->arr[1];

	// Create the return array
	struct int_mat *new_struct = (struct int_mat*) malloc (sizeof(struct int_mat));
	new_struct->length = m1;
	new_struct->arr = malloc(m1 * sizeof(int_array));
	

	// Code taken from: https://www.geeksforgeeks.org/c-program-multiply-two-matrices/
	int res[m1][n2];
	int x, i, j;
    for (i = 0; i < m1; i++) {
        for (j = 0; j < n2; j++) {
        	res[i][j] = 0;	
            for (x = 0; x < m2; x++) {
                *(*(res + i) + j) += ( *(*(e1_ + i) + x) *
                                     *(*(e2_ + x) + j));
            }

        }
    }
    printf("%d\n", m2);
    for (i = 0; i < m1; i++) {
    	struct int_array *tmp = (struct int_array*) malloc (sizeof(struct int_array));
    	tmp->length = n2;
    	tmp->arr = malloc(n2);
    	for (j = 0; j < n2; j++) {
    		*((tmp->arr) + j) = res[i][j];
    	}
    	*((int_array**)(new_struct->arr) + i) = tmp;
    }

	
	return new_struct;
}


float_mat* add_mat_float(void* e1, void* e2) {
	struct float_mat *e1_ = *(float_mat**)(e1);
	struct float_mat *e2_ = *(float_mat**)(e2);
	struct float_mat *new_struct = (struct float_mat*) malloc (sizeof(struct float_mat));
	int size = e2_->length;
	int x;
	new_struct->length = size;
	new_struct->arr = malloc(size * sizeof(float_array));
	for (x = 0; x < size; x++) {
		struct float_array *tmp = (struct float_array*) malloc (sizeof(struct float_array));
		struct float_array *t1 = *((float_array**)(e1_->arr) + x);
		struct float_array *t2 = *((float_array**)(e2_->arr) + x);
		int size = t2->length;
		int z;
		tmp->length = size;
		tmp->arr = malloc(size);
		for (z = 0; z < size; z++) {
			*((tmp->arr) + z) = *((t1->arr) + z) + *((t2->arr) + z);
		}
		*((float_array**)(new_struct->arr) + x) = tmp;
		
	}
	return new_struct;
}

float_mat* sub_mat_float(void* e1, void* e2) {
	struct float_mat *e1_ = *(float_mat**)(e1);
	struct float_mat *e2_ = *(float_mat**)(e2);
	struct float_mat *new_struct = (struct float_mat*) malloc (sizeof(struct float_mat));
	int size = e2_->length;
	new_struct->length = size;
	int x;
	new_struct->arr = malloc(size * sizeof(float_array));
	for (x = 0; x < size; x++) {
		struct float_array *tmp = (struct float_array*) malloc (sizeof(struct float_array));
		struct float_array *t1 = *((float_array**)(e1_->arr) + x);
		struct float_array *t2 = *((float_array**)(e2_->arr) + x);
		int size = t2->length;
		int z;
		tmp->length = size;
		tmp->arr = malloc(size);
		for (z = 0; z < size; z++) {
			*((tmp->arr) + z) = *((t1->arr) + z) - *((t2->arr) + z);
		}
		*((float_array**)(new_struct->arr) + x) = tmp;
		
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


int** store_array(void *e) {
	struct int_mat *e_ = *(int_mat**)(e);
	
	void *tmp = e;
	int_array *sizes = len_mat(e);
	
	int outer_size = sizes->arr[0];
	int inner_size = sizes->arr[1];

	// int return_val[outer_size][inner_size];
	int **return_val = (int **)malloc(outer_size * sizeof(int*));
    int z;
    for (z=0; z<outer_size; z++)
         return_val[z] = (int *)malloc(inner_size * sizeof(int));


	int c_array[outer_size][inner_size];
	int x; 
	
	for (x = 0; x < outer_size; x++) {
		
		struct int_array *e1 = *((int_array**)(e_->arr) + x);
		int y;
	
		for (y = 0; y < inner_size; y++) {
			return_val[x][y] = e1->arr[y];
		}
	}
	return return_val;
}


int_mat* transpose_int ( void*e) {
	int **output = store_array(e);
	struct int_mat *e1_ = *(int_mat**)(e);
	struct int_mat *new_struct = (struct int_mat*) malloc (sizeof(struct int_mat));
	int size = e1_->length;
	int x;
	new_struct->length = size;
	new_struct->arr = malloc(size * sizeof(int_array));
	for (x = 0; x < size; x++) {
		struct int_array *tmp = (struct int_array*) malloc (sizeof(struct int_array));
		struct int_array *t1 = *((int_array**)(e1_->arr) + x);
		int inner_size = t1->length;
		int z;
		tmp->length = inner_size;
		tmp->arr = malloc(inner_size);
		for (z = 0; z < inner_size; z++) {
			*((tmp->arr) + z) = output[z][x];
		}
		*((int_array**)(new_struct->arr) + x) = tmp;
		
	}
	return new_struct;
}


int determinant_int(void *e) {
	
	if (is_square(e)) {
		//int_mat *tmp = *(int_mat **) e;
		int_array *sizes = len_mat(e);
		int n = sizes->arr[0];
		
		int **output = store_array(e);
		int tmp[n][n];
		int x;
		for (x = 0; x < n; x++) {
			int y;
			for (y = 0; y < n; y++) {
				tmp[x][y] = output[x][y];
			}
		}
		int value =det_helper(n, tmp);
		
		return value;
	}
	return 0;
}


/*

	From: https://stackoverflow.com/questions/42802208/code-for-determinant-of-n-x-n-matrix

 */
int det_helper(int n, int a[][n]) {
	if(n<=0) return 0;                                 // stop recursion
    if(n==1) return a[0][0];                           // stop recursion
    if(n==2) return a[0][0]*a[1][1] - a[0][1]*a[1][0]; // stop recursion
    	
    int i,aj,bj,k,p,sign,b[n-1][n-1];
    
    for (p=0,  sign=+1, k = 0; k < n ; k++, sign=-sign)
        {
        for (i=1; i<n; i++)
            {
            for (aj=0,bj=0 ; aj<n; aj++)
             if (aj!=k) 
                {
                b[i-1][bj]=a[i][aj];
                ++bj;
                }
            }
        p= p + (sign*a[0][k]*det_helper(n-1, b)); // here you had aj instead of k causing problems !!!
        }
    return p;
}