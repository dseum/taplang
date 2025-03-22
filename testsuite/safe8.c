#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct s {
    int val;
    struct s* ptr;
};

int main() {
    struct s* x = malloc(sizeof(struct s));
    struct s* y = malloc(sizeof(struct s));
    struct s* z = malloc(sizeof(struct s));

    x->ptr = y;
    y->ptr = z;
    z->ptr = x;

    x->val = 1;
    y->val = 2;
    z->val = 3;

    // Circular, but there is no memory issues here.
    printf("%i\n", x->ptr->ptr->ptr->val);
    free(y);
    printf("%i\n", z->ptr->val);
    free(x);
    printf("%i\n", z->val);
    free(z);
    return 0;
}