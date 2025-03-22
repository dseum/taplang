#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main() {
    int* y = malloc(sizeof(int));
    int* z = y;
    free(y);
    // double free
    free(z);
}