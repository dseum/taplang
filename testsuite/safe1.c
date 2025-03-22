#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main() {
    char buffer[10];
    strcpy(buffer, "abcdefghi");
    printf("%s\n", buffer);
    return 0;
}