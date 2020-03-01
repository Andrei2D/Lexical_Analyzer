#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "StateAutomata.h"

#define IN_FILE "./res/in.txt"
#define OUT_FILE "./res/out.txt"

char* getInput() {
    FILE* in = fopen(IN_FILE, "r");
    int size, ind = 0;
    char* inputString;
    char chr;
    
    fseek(in, 0L, SEEK_END);
    size = ftell(in);
    fseek(in, 0L, SEEK_SET);

    inputString = (char*)malloc (size + 1);
    
    chr = fgetc(in);
    while(chr != EOF) {
        inputString[ind] = chr;
        ind ++;
        chr = fgetc(in);
    }

    fclose(in);

    return inputString;
}

void putOutput(const char* output) {
    FILE* out = fopen(OUT_FILE, "w");
    int ind;
    
    for(ind = 0; ind < strlen(output); ind++) {
        fputc(output[ind], out);
    }
    
    fclose(out);
}

int main() {
    char* inputString;
    TokenList tokList;
    
    inputString = getInput();
    tokList = scanner(inputString);
    ltokFPrint(&tokList, OUT_FILE);

    return 0;
}