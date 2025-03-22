#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main() {
    int* y = malloc(sizeof(int));
    *y = 1;
    if(*y == 2)  {
        free(y);
    }
    *y = 2;
    printf("%i\n", *y);
    return 0;
}