#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

struct s {
    int x;
    int y;
    int z;
};

void shuffle(struct s* t)  {
    int temp = t->x;
    t->x = t->y;
    t->y = t->z;
    t->z = temp;
}

int main() {
    struct s* x = malloc(sizeof(struct s));
    x->x = 3;
    x->y = 4;
    x->z = 5;
    shuffle(x);
    assert(x->x == 4 && x->y == 5 && x->z == 3);
    free(x);
    return 0;
}