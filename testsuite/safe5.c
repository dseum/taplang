#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct s {
    int x;
    long y;
};

struct t {
    long y;
    int x;
};

int main() {
    struct s *x = malloc(sizeof(struct s*));
    x->x = 5;
    printf("%i\n", x->x);
    free(x);
}