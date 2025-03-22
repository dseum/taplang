#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main() {
    int* y = malloc(sizeof(int));
    *y = 2;
    // Unsafe: Frees then uses y
    if(*y == 2)  {
        free(y);
    }
    *y = 1;
    printf("%i\n", *y);
    return 0;
}