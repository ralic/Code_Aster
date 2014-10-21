#include <stdio.h>

#include "MFrontBehaviour.h"

int main(int argc, char *argv[])
{
    if ( argc - 1 != 3 ) {
        printf("exactly three arguments are expected!\n");
        return 1;
    }

    unsigned int i, size;
    char **props;
    props = getMaterialPropertiesNames(argv[1], argv[2], argv[3], &size);
    for (i = 0; i < size; ++i) {
        printf("%s\n", props[i]);
    }
    return 0;
}
