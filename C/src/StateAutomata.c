#include "StateAutomata.h"

#define ARR_SIZE(array) (sizeof(array) / sizeof(array[0]))
#define ALFABET "abcdefghijklmnopqrstuvwxyzABCDEFGHIJLKLMNOPQRSTUVWXYZ"
#define NUMBERS "0123456789"

char statesNames[][20] = {
    "InitialState",

    "ForbidState",
    "ErrorState",

    "statesDelimiter", // v- final | ^- non final

    "Identifier",
    "LiteralIntegr",
    "LiteralFloat",
    "LiteralCaract",
    "LiteralString",

    "Operator",
    "Separator",
    "Comment",

    "TabSpace",
    "NewLine",
    "Space",
    "Delim",

    "stLastState"
};

corespondence lambdaCoresp[] = {

};

corespondence simpleCoresp[] = {
    {stInit,            ';',    stDelim},
    {stInit,            ' ',    stSpace}
};

corespondences multipleCoresp[] = {
    {stInit,            ALFABET,    stIdentifier},
    {stIdentifier,      ALFABET,    stIdentifier},
    {stIdentifier,      NUMBERS,    stIdentifier},

    {stInit,            NUMBERS,    stLiteralIntegr},
    {stLiteralIntegr,   NUMBERS,    stLiteralIntegr},
    {stLiteralIntegr,   ALFABET,    stForbidState},

    {stInit,            "[]{}()",   stSeparator}
    
};


// ------- Token related functons ----------

Token* tokCreate(int type, const char* str) {
    Token* tok = malloc(sizeof(Token));
    
    tok->type = type;
    tok->string = malloc(strlen(str) + 1);
    strcpy(tok->string, str);
    return tok;
}

void tokFPrint(Token* tok, FILE* out) {
    fprintf(out,"<%s, \"%s\">\n", 
        statesNames[tok->type], 
        tok->string
    );
}

void tokDelete(Token* tok) {
    free(tok->string);
    free(tok);
}

// ---------Tokens list related functions--------

TokenList ltokInit() {
    TokenList tokList = {0, NULL};
    return tokList;
}

void ltokAdd(TokenList* tokList, int tokType, const char* tokStr) {
    size_t newSize = (tokList->size + 1) * sizeof(Token*);
    tokList->array = (tokList->array == NULL)
        ? malloc (sizeof(Token*)) 
        : realloc(tokList->array, newSize);
    tokList->array[tokList->size] = tokCreate(tokType, tokStr);
    tokList->size += 1;
}

void ltokFPrint(TokenList* tokList, const char* filename) {
    FILE* out = fopen(filename, "w");
    int ind;
    for (ind = 0; ind < tokList->size; ind++) {
        tokFPrint(tokList->array[ind], out);
    }
    fclose(out);
}

void ltokFree(TokenList* tokList) {
    int ind;
    for(ind = 0; ind < tokList->size; ind++) {
        tokDelete(tokList->array[ind]);
    }
    free(tokList->array);

    *tokList = ltokInit();
}


// -----------Scanner related functions----------
int lambdaTranz (int state) {
    int ind;
    for(ind = 0; ind < ARR_SIZE(lambdaCoresp); ind++) {
        if (state == lambdaCoresp[ind].start_state)
            return lambdaCoresp[ind].dest_state;
    }
    return state;
}

bool satisSim(int corespIndex, int state, char symbol) {
    return state == simpleCoresp[corespIndex].start_state && 
        symbol == simpleCoresp[corespIndex].symbol;
}

bool satisMlt(int corespIndex, int state, char symbol) {
    bool isIn = false;
    int ind;
    if (state != multipleCoresp[corespIndex].start_state)
        return false;

    char* symbArray = multipleCoresp[corespIndex].symbols;
    for (ind = 0; ind < strlen(symbArray); ind++) {
        if (symbol == symbArray[ind]) {
            isIn = true;
            break;
        }
    }
    return isIn;
}

int stateSearch(int currState, char symbol) {
    int ind, defaultState = stErrorState;

    for (ind = 0; ind < ARR_SIZE(simpleCoresp); ind++) {
        if(satisSim(ind, currState, symbol)) {
            return simpleCoresp[ind].dest_state;
        }
    }
    for (ind = 0; ind < ARR_SIZE(multipleCoresp); ind++) {
        if(satisMlt(ind, currState, symbol)) {
            return multipleCoresp[ind].dest_state;
        }
    }
    return defaultState;
}

bool stateIsFinal(int state) {
    return state > statesDelimiter && state < stLastState;
}

char* chrAppend (char* string, char chr) {
    if(NULL == string) {
        string = malloc(2);
        string[0] = chr;
        string[1] = '\0';
    }
    else {
        int lastInd = strlen(string);        
        string = realloc (string, lastInd + 2);
        string[lastInd] = chr;
        string[lastInd+1] = '\0';

    }
    
    return string;
}

char* strAppend(char* dest, const char* src) {
    if(NULL == dest) {
        dest = malloc(strlen(src) + 1);
        strcpy(dest, src);
    }
    else {
        int offset = strlen(dest);
        dest = realloc(dest, offset + strlen(src) + 1);
        strcpy((dest + offset), src);
    }
    
    return dest;
}

TokenList scanner(const char* string) {
    TokenList lt = ltokInit();
    int ind, prevState = stInit, currState;
    int foff = 0, loff = 0;
    char* word = NULL;

    for(ind = 0; ind < strlen(string); ind++) {
        currState = stateSearch(prevState, string[ind]);
        switch (currState)
        {
            case stErrorState: {
                if (stateIsFinal(prevState)) {
                    ltokAdd(&lt, prevState, word);
                    free(word); word = NULL;
                    word = chrAppend(word, string[ind]);
                    break;
                }
                int lmbState = lambdaTranz(prevState);
                if (stateIsFinal(lmbState)) {
                    ltokAdd(&lt, lmbState, word);
                    free(word); word = NULL;
                    word = chrAppend(word, string[ind]);
                    break;
                }
                char* errMsg = NULL;
                errMsg = strAppend(errMsg, statesNames[prevState]);
                errMsg = strAppend(errMsg, ": ");
                errMsg = strAppend(errMsg, chrAppend(word, string[ind]));
                ltokAdd(&lt, stErrorState, errMsg);
                goto end;
            }

            case stForbidState: {
                char* errMsg = NULL;
                errMsg = strAppend(errMsg, statesNames[prevState]);
                errMsg = strAppend(errMsg, ": ");
                errMsg = strAppend(errMsg, chrAppend(word, string[ind]));
                ltokAdd(&lt, stErrorState, errMsg);
                free(errMsg);
                goto end;
            }
            
            default: {
                prevState = currState;
                word = chrAppend (word, string[ind]);
                break;
            }

        }
    }
    end:
    free(word);
    return lt;
}