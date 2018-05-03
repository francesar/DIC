#include <stdio.h>
#include <stdint.h>


int len(int *a) {
	// int* int_arr = (int*)a;
	// int size = sizeof(a) / sizeof();
	printf("%s\n", "sizeof: ");
	printf("%d\n", sizeof(a));
	printf("%s\n", "size: ");
	// printf("%d\n", size);
	printf("%s\n", "*a: ");
    printf("%d\n", *a);
	printf("%s\n", "*a+1: ");
    printf("%d\n", *(a+1));

	printf("%s\n", "*a+2: ");
    printf("%d\n", *(a+2));
    // printf("%d\n", **int_arr);
    // printf("%d\n", ***int_arr);
    return 0;
}

// int main() {
//     int i[] = {1};
//     len(i);
//     return 0;
// }