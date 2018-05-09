#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

// long long sum = 0;

// void* sum_runner(void* arg) {
//     long long *limit_ptr = (long long *) arg;
//     long long limit = *limit_ptr;

//     long long i;
//     for(i = 0; i <= limit; i++) {
//         sum+=i;
//     }

//     pthread_exit(0);
// }

// int main() {
//     long long limit = 10000000000000000;

//     pthread_t tid;
//     pthread_attr_t attr;
//     pthread_attr_init(&attr);

//     pthread_create(&tid, &attr, sum_runner, &limit);

//     pthread_join(tid, NULL);

//     printf("%lld", sum);
// }

void* start_thread(void *func) {
    pthread_t tid;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_create(&tid, &attr, func, NULL);

    pthread_join(tid, NULL);
}