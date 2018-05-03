#include <stdio.h>

int len(void **e) {
    printf("%p", e);
    return 0;
}

// int main() {
//     int i[] = {1};
//     len(i);
//     return 0;
// }