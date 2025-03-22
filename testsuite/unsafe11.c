#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main() {
    // Use of uninitialized memory
    int* x = malloc(sizeof(int));
    printf("%i\n", *x);
    return 0;
}