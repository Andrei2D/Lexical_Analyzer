#include "StateAutomata.h"

#define ARR_SIZE(array) (sizeof(array) / sizeof(array[0]))
#define ANYTHING " `~1234567890-=!?@#$%^&*()_+[]{}<>;:,.'/|\"\\\tqwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"
#define ALFABET "abcdefghijklmnopqrstuvwxyzABCDEFGHIJLKLMNOPQRSTUVWXYZ"
#define NUMBERS "0123456789"

char statesNames[][20] = {
    "InitialState",

    "PlusInterm",
    "MinusInterm",
    "SlashInterm",
    "EqualInterm",
    "stBarInterm",
    "stAndInterm",

    "PowInsertInteger",
    "PowInsertFloat",
    "PowInteger",
    "stFltNrLoop",  
    "stFltExpLoop",  
    "stFltPoint",

    "stZeroNr",
    "stBinIns",       
    "stBinLoop",      
    "stHexIns",       
    "stHexLoop",      

    "stLineCommLoop",
    "stComStar",
    "stComLopp",

    "stChrStart",
    "stChrEnd",
    "stChrBack",

    "stStrEnd",
    "stStrLoop",
    "stStrSlh",

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

    "Delim",
    "Space",
    "TabSpace",
    "NewLine",

    "stLastState"
};

// ----------------------------------------
// @TODO: Comentarii, siruri, caracter
// ----------------------------------------

corespondence lambdaCoresp[] = {
    
    // Number transitions
    {stPowIntegr,   LMB, stLiteralIntegr},
    {stFltNrLoop,   LMB, stLiteralFloat},
    {stFltExpLoop,   LMB, stLiteralFloat},
    {stBinLoop,     LMB, stLiteralIntegr},
    {stHexLoop,     LMB, stLiteralIntegr},
    {stZeroNr,      LMB, stLiteralIntegr},

    // Operators transitions
    {stPlusInterm,  LMB, stOperator},
    {stMinusInterm, LMB, stOperator},
    {stSlashInterm, LMB, stOperator},
    {stEqualInterm, LMB, stOperator},
    {stAndInterm,   LMB, stOperator},
    {stBarInterm,   LMB, stOperator},

    // Others
    {stLineCommLoop, LMB, stComment}
};

corespondence simpleCoresp[] = {
    // Delimiters related
    {stInit,            ';',    stDelim},
    {stInit,            ' ',    stSpace},
    {stInit,           '\n',    stNewLine},
    {stInit,            '0',    stZeroNr},

    // Numbers related
    {stLiteralIntegr,   '.',    stFltPoint},
    {stZeroNr,          '.',    stFltPoint},
    {stZeroNr,          'b',    stBinIns},
    {stZeroNr,          'x',    stHexIns},
    
    // Exponential numbers
    {stLiteralIntegr,   'e',    stPowInsInt},
    {stPowInsInt,       '+',    stPowIntegr},
    {stLiteralFloat,    'e',    stPowInsFlt},
    {stPowInsInt,       '-',    stFltExpLoop},
    {stFltNrLoop,       'e',    stPowInsFlt},
    {stPowIntegr,       '.',    stForbidState},
    {stFltExpLoop,      '.',    stForbidState},
    

    // Comment
    {stSlashInterm,     '/',    stLineCommLoop},
    {stSlashInterm,     '*',    stComLopp},
    {stComLopp,         '*',    stComStar},
    {stComStar,         '*',    stComStar},
    {stComStar,         '/',    stComment},
    {stComStar,         '\n',   stComLopp},
    {stComLopp,         '\n',   stComLopp},

    // Strings and characters
    {stInit,            '\'',   stChrStart},
    {stChrEnd,          '\'',   stLiteralCaract},
    {stChrStart,        '\\',   stChrBack},
    {stInit,            '"',    stStrLoop},
    {stStrLoop,         '\\',   stStrSlh},
    {stStrLoop,         '"',    stLiteralString},
    {stStrSlh,          '"',    stStrLoop},
    {stStrLoop,         '\n',   stStrLoop},

    // Intermediary operators
    {stInit,            '-',    stMinusInterm},
    {stInit,            '+',    stPlusInterm},
    {stInit,            '/',    stSlashInterm},
    {stInit,            '&',    stAndInterm},
    {stInit,            '|',    stBarInterm},

    // Composed operators
    {stAndInterm,       '&',    stOperator},
    {stAndInterm,       '=',    stOperator},
    {stBarInterm,       '|',    stOperator},
    {stBarInterm,       '=',    stOperator},
    {stMinusInterm,     '-',    stOperator},
    {stMinusInterm,     '=',    stOperator},
    {stPlusInterm,      '+',    stOperator},
    {stPlusInterm,      '=',    stOperator},
    {stSlashInterm,     '=',    stOperator},
    {stEqualInterm,     '=',    stOperator}
};

corespondences multipleCoresp[] = {
    // Identfiers
    {stInit,            ALFABET,    stIdentifier},
    {stIdentifier,      ALFABET,    stIdentifier},
    {stIdentifier,      NUMBERS,    stIdentifier},

    // Literals
    {stInit,            NUMBERS,    stLiteralIntegr},
    {stLiteralIntegr,   NUMBERS,    stLiteralIntegr},
    {stLiteralFloat,    NUMBERS,    stLiteralFloat},
    {stLiteralIntegr,   ALFABET,    stForbidState},
    {stLiteralFloat,    ALFABET,    stForbidState},

    // Weird literals
    {stFltPoint,        NUMBERS,    stFltNrLoop},

    // Hex and Bin
    {stBinIns,          "01",       stBinLoop},
    {stBinLoop,         "01",       stBinLoop},
    {stBinLoop,         NUMBERS,    stForbidState},
    {stBinLoop,         ALFABET,   stForbidState},
    {stHexIns,          NUMBERS,    stHexLoop},
    {stHexIns,          "abcdef",   stHexLoop},
    {stHexIns,          "ABCDEF",   stHexLoop},
    {stHexLoop,         NUMBERS,    stHexLoop},
    {stHexLoop,         "abcdef",   stHexLoop},
    {stHexLoop,         "ABCDEF",   stHexLoop},
    {stHexLoop,         ALFABET,    stForbidState},

    // Exponential literals
    {stPowInsInt,       NUMBERS,    stPowIntegr},
    {stPowIntegr,       NUMBERS,    stPowIntegr},

    {stPowInsFlt,       "+-",       stFltExpLoop},
    {stPowInsFlt,       NUMBERS,    stFltExpLoop},
    {stFltExpLoop,      NUMBERS,    stFltExpLoop},
    {stFltExpLoop,      ALFABET,    stForbidState},
    
    // Comments
    {stLineCommLoop,    ANYTHING,   stLineCommLoop},
    {stComLopp,         ANYTHING,   stComLopp},
    {stComStar,         ANYTHING,   stComLopp},

    // Strings and charactes
    {stChrStart,        ANYTHING,   stChrEnd},
    {stChrBack,         "'nt0",     stChrEnd},
    {stStrLoop,         ANYTHING,   stStrLoop},
    {stStrSlh,          ANYTHING,   stStrLoop},

    // Operators and separators
    {stInit,            "-*=%^><!", stEqualInterm},
    {stInit,            "#,.:?",     stOperator},

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
    char* toPrint = strcmp("\n", tok->string) == 0 
        ? "\\n" : tok->string;
    fprintf(out,"<%s, \"%s\">\n", 
        statesNames[tok->type], 
        toPrint
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
    if(symbol == '\0') return currState;

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
    if('\0' == chr)
        return string;
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

    for(ind = 0; ind <= strlen(string); ind++) {
        restart:
        currState = stateSearch(prevState, string[ind]);
        printf("%s - %s -| %s ^ %c\n",
            statesNames[prevState], statesNames[currState],
            word, string[ind]
        );
        switch (currState)
        {
            case stErrorState: {
                if (stateIsFinal(prevState)) {
                    ltokAdd(&lt, prevState, word);
                    free(word); word = NULL;
                    prevState = stInit;
                    goto restart;
                }
                int lmbState = lambdaTranz(prevState);
                if (stateIsFinal(lmbState)) {
                    ltokAdd(&lt, lmbState, word);
                    free(word); word = NULL;
                    prevState = stInit;
                    goto restart;
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
    prevState = lambdaTranz(prevState);
    if(stateIsFinal(prevState)) {
        ltokAdd(&lt, prevState, word);
    }
    else {
        char* errMsg = NULL;
        errMsg = strAppend(errMsg, statesNames[prevState]);
        errMsg = strAppend(errMsg, ": ");
        errMsg = strAppend(errMsg, word);
        ltokAdd(&lt, stErrorState, errMsg);
        free(errMsg);
    }

    end:
    free(word);
    return lt;
}