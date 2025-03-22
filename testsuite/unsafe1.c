#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main() {
    char buffer[10];
    // Unsafe: Buffer overflow because null character
    strcpy(buffer, "abcdefghij");
    printf("%s\n", buffer);
    return 0;
}