#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main() {
    int x[10];
    // Accessing out of bounds
    printf("%i\n", x[1000000]);
    return 0;
}