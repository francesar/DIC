#include <stdio.h>

void write_string_to_file(char *file_path, char * data) {
	fprintf(stderr, "%s", data);
    FILE *fp;
    fp = fopen(file_path, "w");
    fprintf(fp, "%s\n", data);
    fclose(fp);
}