mkdir app
cd app
echo "
#include <stdio.h> 
int main(void) { 
    printf(\"hello world!!!\\n\"); 
    return 0; 
} 
" > start.c
nios2-app-generate-makefile --bsp-dir ../bsp --elf-name start.elf --src-dir .
cd ..

