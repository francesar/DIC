#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct float_array {
	int length;
	double *arr;
} float_array;

typedef struct float_mat {
	int length;
	struct float_array *arr;
} float_mat;

void write_string_to_file(char *file_path, char * data) {
	fprintf(stderr, "%s", data);
    FILE *fp;
    fp = fopen(file_path, "w");
    fprintf(fp, "%s\n", data);
    fclose(fp);
}

// void parseline(char *line, float_array *inp_row) {
//     char *tok;
//     char *og;
//     strcpy(og, line);
//     int colcount = 0;

//     tok = strtok(line, ",");
//     while(tok != NULL) {
//         colcount++;
//         tok = strtok(NULL, ",");
//     }

//     row->arr = malloc(colcount * sizeof(double));

//     char* ltok = strtok(og, ",");
//     double *el;
//     int i = 0;
//     while(ltok != NULL) {
//         double itok;
//         sscanf(ltok, "%lf", &itok);

//         row->arr[i] = itok;
//         ltok = strtok(NULL, ",");
//     }

//     printf("%lf\n", row->arr[0]);

//     row->length = colcount;
// }

int main() {
    FILE *fp;
    fp = fopen("lol.csv", "r");

    char *line;
    size_t len = 0;
    ssize_t read;
    
    int rcount = 0;

    while((read = getline(&line, &len, fp) != -1)) {
        rcount++;
    }

    struct float_mat *mat = (struct float_mat *) malloc(sizeof(struct float_mat));
    mat->arr = malloc(rcount * sizeof(struct float_array));
	mat->length = rcount;
    fp = fopen("lol.csv", "r");

    
    int i = 0;
    while((read = getline(&line, &len, fp) != -1)) {
        struct float_array *row = (struct float_array *) malloc(sizeof(struct float_array));
        char *tok;
        char *og;
        strcpy(og, line);
        int colcount = 0;

        tok = strtok(line, ",");
        while(tok != NULL) {
            colcount++;
            tok = strtok(NULL, ",");
        }

        row->arr = malloc(colcount * sizeof(double));

        char* ltok = strtok(og, ",");
        double *el;
        int i = 0;
        while(ltok != NULL) {
            double itok;
            sscanf(ltok, "%lf", &itok);

            row->arr[i] = itok;
        	
            ltok = strtok(NULL, ",");
        }
        

        //printf("%lf\n", row->arr[0]);
	    
        row->length = colcount;

        *((float_array**)(mat->arr) + i) = row;
        
        i++;
    }

    struct float_array *r = *(float_array**)(mat->arr);
    
    float x = r->arr[0];
    
    printf("%d\n", mat->length);
    printf("%lf\n", x);
}