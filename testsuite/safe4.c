#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main() {
    int* y = malloc(sizeof(int));
    int* z = malloc(sizeof(int));
    free(y);
    free(z);
}