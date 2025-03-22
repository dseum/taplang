#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main() {
    int check[3] = { 1, 2, 3 };
    for(int i = 0; i < 5; i++)  {
        // Accesses beyond the bounds of the array
        for(int j = 0; j <= 3; j++)  {
            if(check[j] == i)  {
                printf("%i is in the array\n", i);
                break;
            }
        }
    }
}