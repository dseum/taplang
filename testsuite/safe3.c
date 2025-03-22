#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main() {
    int* y = malloc(sizeof(int));
    *y = 1;
    int* x = y;
    int z = *x;
    free(x);
    printf("%i\n", z);
    return 0;
}