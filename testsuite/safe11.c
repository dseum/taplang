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

    printf("%i\n", x->ptr->ptr->val);
    free(x->ptr->ptr);
    printf("%i\n", x->ptr->val);
    free(y);
    printf("%i\n", x->val);
    free(x);
    return 0;
}