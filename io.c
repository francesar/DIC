#include <stdio.h>

void f_write(char *file_path, char * data) {
    FILE *fp;
    fp = fopen(file_path, "w");
    fprintf(fp, "%s\n", data);
    fclose(fp);
}