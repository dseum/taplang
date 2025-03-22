#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main() {
    int* y = malloc(sizeof(int));
    *y = 1;
    int* x = y;
    free(y);
    // Use after free
    printf("%i\n", *x);
    return 0;
}