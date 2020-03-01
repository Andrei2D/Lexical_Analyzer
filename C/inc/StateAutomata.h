#ifndef STATE_AUTOMATA_H_
#define STATE_AUTOMATA_H_
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define AR_SIZE(array) ((int)(sizeof(array)/sizeof(array[0])))
#define LMB '`'
//--------------Data structures--------------

struct _corespondence {
    int start_state;
    char symbol;
    int dest_state;
};
typedef struct _corespondence corespondence;


struct _corespondences {
    int start_state;
    char* symbols;
    int dest_state;
};
typedef struct _corespondences corespondences;


struct _Token {
    int type;
    char* string;
};
typedef struct _Token Token;

struct _TokenList {
    int size;
    Token** array;
};
typedef struct _TokenList TokenList;


//-------Enum and arrays declarations----------

enum StatesList {
    stInit,

    stPlusInterm,   // ++ +=
    stMinusInterm,  // -- -=
    stSlashInterm,  // /* /=
    stEqualInterm,  // == and *=

    stPowInsInt,     //12e..
    stPowInsFlt,     //12.3e..
    stPowIntegr,     //13e12 
    stFltNrLoop,      //2.3e-8, 0.3
    stFltPoint,

    stZeroNr,       // 0, 0b.., 0x.., 0. ..
    stBinIns,       // 0b..
    stBinLoop,      // 0b101..
    stHexIns,       // 0x..
    stHexLoop,      // 0xa5F..

    stLineCommLoop, // //...
    stComStar,      // /*******
    stComLopp,      // /* .....

    stForbidState,
    stErrorState,

    statesDelimiter,

    stIdentifier,
    stLiteralIntegr,
    stLiteralFloat,
    stLiteralCaract,
    stLiteralString,

    stOperator,
    stSeparator,
    stComment,
    
    stDelim,
    stSpace,
    stTabSpace,
    stNewLine,

    stLastState
};

char statesNames[][20];

corespondence lambdaCoresp[];
corespondence simpleCoresp[];
corespondences multipleCoresp[];

//--------------Function declaration--------------

bool satisSim(int corespIndex, int state, char symbol);
bool satisMult(int corespIndex, int state, char symbol);

Token* tokCreate(int type, const char* str);
void tokFPrint(Token* tok, FILE* out);
void tokDelete(Token* tok);

TokenList ltokInit();
void ltokAdd(TokenList* tokList, int tokType, const char* tokStr);
void ltokFPrint(TokenList* tokList, const char* filename);
void ltokFree(TokenList* tokList);


char* chrAppend (char* string, char chr);
char* strAppend(char* dest, const char* src);

int stateSearch(int currState, char symbol);

TokenList scanner(const char* string);

#endif /* STATE_AUTOMATA_H_ */

/*
data ClsType = Identif | LitIntr | LitFlot | LitBin |
    LitCrc | LitStr | Operator | Separator | Comment |
    TabSpace | NewLine | Space | Delim
    deriving (Show, Eq)

data Interm = None | FrontSlh | PlusInterm | EqualInterm | 
    ComLoop | ComEnd | LineComm |
    FltExp | FltExpPow | NumbRepr | BinRepr | HexRepr |
    ChrInsInterm | ChrEndInterm | ChrQuotInterm | 
    StrgInterm | StrgQuotInterm
*/