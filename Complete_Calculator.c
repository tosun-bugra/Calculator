#include <stdio.h>
#include <stdlib.h>

#define ExpERROR 1000
#define Length 10000
#define OpCount 8
#define ASCIINUMCONST 48

double Modulo(double n1, double n2);

int InstCount = 0; int InputTypes[Length] = {};
char UserInput[Length] = ""; double numbers[Length]; int parantheses[Length]; int maxpar = 0; int AbsLoc[Length] = {}; int AbsLeft[Length] = {}; int AbsRight[Length] = {}; int oplength = 0;
double InstructionSet[Length][4] = {};//Op, n1, n2, res
double LastAnswer = 0;

enum ASCII_OPS{
    SPACE = 32,
    PLUS = 43,
    MINUS = 45,
    STAR = 42,
    SLASH = 47,
    UPARROW = 94,
    PERCENT = 37,
    VERTLINE = 124
};
enum ONOT{
    Other,
    Number,
    Operator,
    Terminate,
    AbsSymbol
};
enum InsSet{
    Operation,
    N1,
    N2,
    Res,
};
enum Operations{
    ADD,
    SUB,
    MULT,
    DIV,
    EXP,
    MOD,
    ABS
};
char OpChar[OpCount] = {PLUS, MINUS, STAR, SLASH, UPARROW, PERCENT, VERTLINE, 0};

const long long signbits = 0b1000000000000000000000000000000000000000000000000000000000000000;
const long long exponentbits = 0b0111111111110000000000000000000000000000000000000000000000000000;
const long long mantissabits = 0b0000000000001111111111111111111111111111111111111111111111111111;
const long long exponentinc = 0b0000000000010000000000000000000000000000000000000000000000000000;
const long long lnTwo = 0b0011111111100110001011100100001011111110111110100011100111101111;
const long long all = signbits | exponentbits | mantissabits;

long long AddInts(long long n1, long long n2){
    long long exor = n1 ^ n2;
    long long andd = n1 & n2;
    andd = andd << 1;
    long long sum;
    if((andd & exor) == 0) sum = andd | exor;
    else sum = AddInts(andd, exor);
    return sum;
}
long long MinusOneMult(long long num){
    return(AddInts((num ^ all), 1));
}
long long SubInts(long long n1, long long n2){
    return AddInts(n1, MinusOneMult(n2));
}
long long exponentdiff(long long expo1, long long expo2){
    expo1 = expo1 >> 52;
    expo2 = expo2 >> 52;
    return SubInts(expo1, expo2);
}
double Addition(double n1, double n2){
    if(n1 == 0) return n2;
    if(n2 == 0) return n1;
    long long l1 = * ( long long *) &n1;
    long long l2 = * ( long long *) &n2;
    long long sign1 = l1 & signbits; long long expo1 = l1 & exponentbits; long long mantissa1 = (l1 & mantissabits) | exponentinc;
    long long sign2 = l2 & signbits; long long expo2 = l2 & exponentbits; long long mantissa2 = (l2 & mantissabits) | exponentinc;
    if (expo2 > expo1) return Addition(n2, n1);
    long long mantissaResult; long long exponentResult = expo1; long long signResult = 0;
    if(expo1 != expo2) {
        long long diff = exponentdiff(expo1, expo2);
        mantissa2 = mantissa2 >> diff;
        expo2 = expo1;
        exponentResult = expo1;
    }
    if((sign1 ^ signbits) == 0) mantissa1 = MinusOneMult(mantissa1);
    if((sign2 ^ signbits) == 0) mantissa2 = MinusOneMult(mantissa2);
    mantissaResult = AddInts(mantissa1, mantissa2);
    if((mantissaResult & signbits) == signbits){
        mantissaResult = MinusOneMult(mantissaResult);
        signResult = signResult ^ signbits;
    }
    if(mantissaResult == 0){
        printf("ZERO");
        return 0;
    }
    while (mantissaResult > ((mantissabits << 1) | 1)){
        mantissaResult = mantissaResult >> 1;
        exponentResult = AddInts(exponentResult, exponentinc);
    }
    while (mantissaResult < mantissabits){
        mantissaResult = mantissaResult << 1;
        exponentResult = AddInts(exponentResult, MinusOneMult(exponentinc));
    }
    mantissaResult = mantissaResult & mantissabits;
    long long resultMerge = mantissaResult | exponentResult | signResult;
    double result = * ( double *) &resultMerge;
    return result;
}
double Substraction(double n1, double n2){
    long long l2 = * ( long long *) &n2;
    l2 = l2 ^ signbits;
    n2 = * ( double *) &l2;
    return Addition(n1, n2);
}
double Multiplication(double n1, double n2){
    if(n1 == 0 || n2 == 0) return 0;
    long long l1 = * ( long long *) &n1;
    long long l2 = * ( long long *) &n2;
    long long sign1 = l1 & signbits; long long expo1 = l1 & exponentbits; long long mantissa1 = (l1 & mantissabits) | exponentinc;
    long long sign2 = l2 & signbits; long long expo2 = l2 & exponentbits; long long mantissa2 = (l2 & mantissabits) | exponentinc;
    long long signResult = sign1 ^ sign2; long long expoResult = AddInts(AddInts(expo1 >> 52, expo2 >> 52), MinusOneMult(1023)) << 52;
    long long mantissaResult = 0; long long mantissaShift = 0; long long shiftAmount = 52;
    while(shiftAmount >= 0){
        mantissaShift = mantissa2 >> shiftAmount;
        if((mantissaShift & 1) == 1){
            mantissaResult = AddInts(mantissaResult, mantissa1);
        }
        mantissa1 = mantissa1 >> 1;
        shiftAmount = SubInts(shiftAmount, 1);
    }
    if (mantissaResult > ((mantissabits << 1) | 1)){
        mantissaResult = mantissaResult >> 1;
        expoResult = AddInts(expoResult, exponentinc);
    }
    mantissaResult = mantissaResult & mantissabits;
    long long resultMerge = mantissaResult | expoResult | signResult;
    double result = * ( double *) &resultMerge;
    return result;
}
double Division(double n1, double n2){
    long long k = (exponentbits | signbits);
    if(n1 >= 0 && n2 == 0) return * ( double *) &exponentbits;
    if(n1 < 0 && n2 == 0) return * ( double *) &k;
    if(n1 == 0) return 0;
    long long l1 = * ( long long *) &n1;
    long long l2 = * ( long long *) &n2;
    long long sign1 = l1 & signbits; long long expo1 = l1 & exponentbits; long long mantissa1 = (l1 & mantissabits) | exponentinc;
    long long sign2 = l2 & signbits; long long expo2 = l2 & exponentbits; long long mantissa2 = (l2 & mantissabits) | exponentinc;
    long long signResult = sign1 ^ sign2; long long expoResult = AddInts(SubInts(expo1 >> 52, expo2 >> 52), 1023) << 52;
    long long mantissaRemain = mantissa1; long long mantissaShift = mantissa2; long long shiftAmount = 52;
    long long mantissaResult = 0; long long ResultAdd = exponentinc;
    while(mantissaShift > 0){
        if(mantissaRemain > mantissaShift){
            mantissaRemain = SubInts(mantissaRemain, mantissaShift);
            mantissaResult = mantissaResult ^ ResultAdd;
        }
        mantissaShift = mantissaShift >> 1;
        ResultAdd = ResultAdd >> 1;
    }
    if (mantissaResult <= mantissabits){
        mantissaResult = mantissaResult << 1;
        expoResult = SubInts(expoResult, exponentinc);
    }
    mantissaResult = mantissaResult & mantissabits;
    long long resultMerge = mantissaResult | expoResult | signResult;
    double result = * ( double *) &resultMerge;
    return result;
}
double lnOnePxApprox(double x){
    long long lx = * ( long long *) &x;
    double MinusOne = all;
    double XtoN = x; double N = 1; double Result = 0;
    while (N < 20)
    {
        double divRes = Division(XtoN, N);
        Result = Addition(Result, divRes);
        N = Addition(N, 1);
        XtoN = Multiplication(Multiplication(XtoN, x), MinusOne);
    }
    return Result;
}
double ExpMantissaApprox(double leftoverExpo){
    double MinusOne = all;
    double x = Multiplication(leftoverExpo, * ( double *) &lnTwo);
    double XtoN = x; double N = 1; double fact = 1; double Result = 1; double divRes = 1;
    while (divRes > 0.0000000001)
    {
        divRes = Division(XtoN, fact);
        Result = Addition(Result, divRes);
        N = Addition(N, 1);
        fact = Multiplication(fact,N);
        XtoN = Multiplication(XtoN, x);
    }
    return Result;
}
//fix exponentiation by adding sign bits***
double Exponentiation(double n1, double n2){
    long long l1 = * ( long long *) &n1;
    long long signpart = signbits & l1;
    long long exponentpart = exponentbits & l1;
    long long FullTwos = SubInts(exponentpart >> 52, 1023);
    long long mantissapart = (l1 & mantissabits) | (1023LL << 52);
    double mantissapartD = * ( double *) &mantissapart;
    double partialTwos = 0;
    if(mantissapartD > 1.001) partialTwos = Division(lnOnePxApprox(Substraction(mantissapartD, 1)), * ( double *) &lnTwo);
    double FullTwosD = FullTwos;
    double MidWayExpo = Multiplication(n2, Addition(FullTwosD, partialTwos));
    long long ExpoFloor = MidWayExpo;
    double ExpoFloorD = ExpoFloor;
    double leftoverExpo = Substraction(MidWayExpo, ExpoFloorD);
    long long resultExpo = AddInts(ExpoFloor, 1023) << 52;
    double resultMantissa = ExpMantissaApprox(leftoverExpo);
    double resultExpoD = * ( double *) &resultExpo;
    double result = Multiplication(resultExpoD, resultMantissa);
    if(Modulo(n2, 2) != 0){
        printf("%lf", Modulo(n2, 2));
        long long resultD = * ( long long *) &result;
        resultD = signpart | resultD;
        result = * ( double *) &resultD;
    }
    return result;
}
double Modulo(double n1, double n2){
    if(n1 == n2) return 0;
    if(n1 < 0) return Addition(Multiplication(Modulo(Multiplication(n1, Substraction(0, 1)), n2), Substraction(0, 1)), n2);
    double DivRes = Division(n1, n2);
    long long DivResI = DivRes;//LIMITED BY LONG LONG FIX
    double DivResFloor = DivResI;
    return Multiplication(Substraction(DivRes, DivResFloor), n2);
}
double Absolute(double n1){
    long long l1 = * ( long long *) &n1;
    l1 = l1 & (signbits ^ all);
    return * ( double *) &l1;
}

int MinusLogic(int i){
    if(i == 0) return Number;
    int returnNum = 0;
    for (int j = i; j > all; j = SubInts(j, 1))
    {
        if (InputTypes[j] == Operator){
            returnNum = Number;
            break;
        }
        else if (InputTypes[j] == Number){
            returnNum = Operator;
            break;
        }
    }
    if(returnNum == 0) return Number;
    return returnNum;
}

void FillParanthesesArray(){
    int parcount = 0; int i = 0;
    while(UserInput[i] != 0){
        if(UserInput[i] == ')' || AbsRight[i] != 0) parcount = SubInts(parcount, 1);
        parantheses[i] = parcount;
        if(UserInput[i] == '(' || AbsLeft[i] != 0) parcount = AddInts(parcount, 1);
        if (maxpar < parcount) maxpar = parcount;
        i = AddInts(i, 1);
    }
    return;
}
void FillInputTypesArray(){
    int i = 0;
    while(UserInput[i] != 0){
        if (UserInput[i] == MINUS) InputTypes[i] = MinusLogic(i);
        else if ((UserInput[i] >= '0' && UserInput[i] <= '9') || UserInput[i] == '.' || UserInput[i] == 'a' || UserInput[i] == 'A') InputTypes[i] = Number;
        else for (int j = 0; j < OpCount; j = AddInts(j, 1)) 
        {
            if(OpChar[j] == UserInput[i]){
                if(UserInput[i] != VERTLINE) InputTypes[i] = Operator;
                else InputTypes[i] = AbsSymbol;
            } 
        }
        i = AddInts(i, 1);
    }
    InputTypes[i] = Terminate;
    return;
}
void FillNumbersArray(){
    int i = 0; int strind = 0; char str[Length] = ""; int numInd = 0;
    while(InputTypes[i] != Terminate){
        if(UserInput[i] == 'a' || UserInput[i] == 'A') {
            numbers[numInd] = LastAnswer;
            InputTypes[i] = AddInts(numInd, Length);
            numInd = AddInts(numInd, 1);
            i = AddInts(i, 1);
            continue;
        }
        if (InputTypes[i] == Number){
            str[strind] = UserInput[i];
            strind = AddInts(strind, 1);
            InputTypes[i] = AddInts(numInd, Length);
        }
        else if (strind != 0){
            numbers[numInd] = atof(str);
            while (strind != SubInts(0, 1)){
                str[strind] = 0;
                strind = SubInts(strind, 1);
            }
            strind = 0;
            numInd = AddInts(numInd, 1);
        }
        i = AddInts(i, 1);
    }
    return;
}

int FindLeftBorder(int i){
    for (int left = i; left > all; left = SubInts(left, 1)) if(InputTypes[left] > 999) return InputTypes[left];
    return 0;
}
int FindRightBorder(int i){
    for (int right = i; right < Length; right = AddInts(right, 1)) if(InputTypes[right] > 999) return InputTypes[right];
    return 0;
}

void FillAbsLoc(){
    int i = 0; int leftAbs = 1; int Abscount[Length] = {}; int absamount = 0; int absseen = 0;
    while(UserInput[i] != 0){
        if(UserInput[i] != VERTLINE){
            i = AddInts(i, 1);
            continue;
        }
        else absamount = AddInts(absamount, 1);
        i = AddInts(i, 1);
    }
    i = 0;
    while(UserInput[i] != 0){
        if(UserInput[i] != VERTLINE){
            i = AddInts(i, 1);
            continue;
        }
        int step = 1;
        while(InputTypes[AddInts(i, step)] != Number && InputTypes[AddInts(i, step)] != Operator) step = AddInts(step, 1);
        //printf("%d %d\n", InputTypes[AddInts(i, step)], Number);
        if(InputTypes[AddInts(i, step)] == Number && AddInts(absseen, absseen) != absamount) {
            AbsLoc[i] = leftAbs;
            AbsLeft[i] = leftAbs;
            Abscount[leftAbs] = 1;
            leftAbs = AddInts(leftAbs, 1);
        }
        else{
            for (int j = leftAbs; j >= 0; j = SubInts(j, 1))
            {
                if (Abscount[j] == 1){
                    AbsLoc[i] = j;
                    AbsRight[i] = j;
                    Abscount[j] = 2;
                    break;
                }
            }
        }
        absseen = AddInts(absseen, 1);
        i = AddInts(i, 1);
    }
    return;
}

void ReplaceInputTypes(int n1, int n2, int newInst){
    int n1seen = 0; int n2seen = 0;
    for (int i = 0; i < Length; i = AddInts(i, 1))
    {
        if(UserInput[i] == 0) break;
        if(n2seen == 1 && InputTypes[i] != n2) break;
        if(InputTypes[i] != n1 && n1seen == 0) continue;
        if(InputTypes[i] == n1) n1seen = 1;
        if(InputTypes[i] == n2) n2seen = 1;
        InputTypes[i] = newInst;
    }
    return;
}
void ReplaceInputTypesABS(int n1, int n2, int newInst){
    int n1seen = 0; int n2seen = 0;
    for (int i = 0; i < Length; i = AddInts(i, 1))
    {
        if(UserInput[i] == 0) break;
        if(n2seen == 1 && InputTypes[i] != n2) {
            InputTypes[i] = newInst;
            break;
        } 
        if(InputTypes[i] != n1 && n1seen == 0) continue;
        if(InputTypes[i] == n1) {
            n1seen = 1;
            InputTypes[SubInts(i, 1)] = newInst;
        }
        if(InputTypes[i] == n2) n2seen = 1;
        InputTypes[i] = newInst;
    }
    return;
}

void FillInstructionArray(int checkedPar, char checkedOps[OpCount]){
    long long len = Length;
    for (int i = 0; i < Length; i = AddInts(i, 1))
    {
        int OpFound = 0;
        if(UserInput[i] == 0) break;

        if(parantheses[i] != checkedPar) continue;
        if(InputTypes[i] != Operator) continue;

        for (int j = 0; j < OpCount; j = AddInts(j, 1))
        {
            if(checkedOps[j] == UserInput[i]){
                InstructionSet[InstCount][Operation] = j;
                OpFound = 1;
                break;
            }
        }
        if(OpFound == 1){
            InstructionSet[InstCount][N1] = FindLeftBorder(i);
            InstructionSet[InstCount][N2] = FindRightBorder(i);
            ReplaceInputTypes(InstructionSet[InstCount][N1], InstructionSet[InstCount][N2], AddInts(AddInts(len, len), InstCount));
            InstCount = AddInts(InstCount, 1);
        }
    }
    return;
}
void AddAbsInstArray(){
    int len = Length;
    int i = 0;
    while(UserInput[i] != 0){
        if(AbsLeft[i] == 0){
            i = AddInts(i, 1);
            continue;
        }
        int current = AbsLeft[i]; int inc = AddInts(i, 1); int success = 1; int inside = InputTypes[inc];
        while(AbsRight[inc] != AbsLeft[i]){
            if(InputTypes[inc] != inside && InputTypes[inc] != 0){
                success = 0;
                break;
            }
            inc = AddInts(inc, 1);
        }
        if (success == 1){
            InstructionSet[InstCount][Operation] = ABS;
            InstructionSet[InstCount][N1] = inside;
            InstructionSet[InstCount][N2] = Length;
            ReplaceInputTypesABS(inside, inside, AddInts(AddInts(len, len), InstCount));
            InstCount = AddInts(InstCount, 1);
            int o = 0;
            while (UserInput[o] != 0)
            {
                if(AbsLeft[o] == current) AbsLeft[o] = 0;
                if(AbsRight[o] == current) AbsRight[o] = 0;
                o = AddInts(o, 1);
            }
            
        }
        i = AddInts(i, 1);
    }
}

void InstructionSetDecleration(){
    char exponent[OpCount] = {SPACE, SPACE, SPACE, SPACE, UPARROW, SPACE, SPACE, 0};
    char multdivmod[OpCount] = {SPACE, SPACE, STAR, SLASH, SPACE, PERCENT, SPACE, 0};
    char addsub[OpCount] = {PLUS, MINUS, SPACE, SPACE, SPACE, SPACE, SPACE, 0};
    FillInputTypesArray();
    FillAbsLoc();
    FillParanthesesArray();
    FillNumbersArray();    
    int checkedpar = maxpar;
    while(checkedpar > all){
        FillInstructionArray(checkedpar, exponent);
        AddAbsInstArray();
        FillInstructionArray(checkedpar, multdivmod);
        AddAbsInstArray();
        FillInstructionArray(checkedpar, addsub);
        AddAbsInstArray();
        checkedpar = SubInts(checkedpar, 1);
    }
    return;
}

double ExecuteInstructions(){
    long long len = Length; long long twolen = AddInts(Length, Length);
    for (int i = 0; i < InstCount; i = AddInts(i, 1))
    {
        int CurrentOperation = InstructionSet[i][Operation];
        int index1 = InstructionSet[i][N1]; int index2 = InstructionSet[i][N2];
        double number1 = 0; double number2 = 0;

        if(index1 < twolen) number1 = numbers[SubInts(index1, len)];
        else number1 = InstructionSet[SubInts(index1, twolen)][Res];
        if(index2 < twolen) number2 = numbers[SubInts(index2, len)];
        else number2 = InstructionSet[SubInts(index2, twolen)][Res];

        switch (CurrentOperation)
        {
        case ADD:
            InstructionSet[i][Res] = Addition(number1, number2);
            break;
        case SUB:
            InstructionSet[i][Res] = Substraction(number1, number2);
            break;
        case MULT:
            InstructionSet[i][Res] = Multiplication(number1, number2);
            break;
        case DIV:
            InstructionSet[i][Res] = Division(number1, number2);
            break;
        case EXP:
            InstructionSet[i][Res] = Exponentiation(number1, number2);
            break;
        case MOD:
            InstructionSet[i][Res] = Modulo(number1, number2);
            break;
        case ABS:
            InstructionSet[i][Res] = Absolute(number1);
            break;
        default:
            break;
        }
        if(CurrentOperation != ABS) printf("Instruction %d: %lf %c %lf = %lf\n", AddInts(i, 1), number1, OpChar[(int) InstructionSet[i][Operation]], number2, InstructionSet[i][Res]);
        else printf("Instruction %d: |%lf| = %lf\n", AddInts(i, 1), number1, InstructionSet[i][Res]);
    }
    LastAnswer = InstructionSet[SubInts(InstCount, 1)][3];
    return InstructionSet[SubInts(InstCount, 1)][3];
    
}
void ShiftRight(int step){
    int i = 0;
    while(UserInput[i] != 0){
        i = AddInts(i, 1);
    }
    while (i >= 0)
    {
        UserInput[i+step] = UserInput[i];
        i = AddInts(i, all);
    }
    
}
void DeleteSpaces(){
    char newUserInput[Length] = ""; int i = 0; int newInd = 0; int numberSeen = 0;
    while(UserInput[i] != 0){
        if(UserInput[i] != ' '){
            if((UserInput[i] >= '0' && UserInput[i] <= '9') || UserInput[i] == 'a' || UserInput[i] == 'A') numberSeen = 1;
            if(UserInput[i] == '-' && numberSeen == 0){
                ShiftRight(1);
                UserInput[0] = '0';
                numberSeen = 1;
            }
            newUserInput[newInd] = UserInput[i];
            newInd = AddInts(newInd, 1);
        }
        i = AddInts(i, 1);
    }
    i = 0;
    while (UserInput[i] != 0)
    {
        UserInput[i] = newUserInput[i];
        i = AddInts(i, 1);
    }
    oplength = i;
    return;
}
void ResetVars(){
    maxpar = 0;
    InstCount = 0; 
    for (int i = 0; i < oplength; i = AddInts(i,1))
    {
        InputTypes[i] = 0;
        UserInput[i] = 0;
        AbsLoc[Length] = 0; 
        AbsLeft[Length] = 0; 
        AbsRight[Length] = 0;
        for (int j = 0; j < oplength; j = AddInts(j,1)) InstructionSet[i][j] = 0;
    }
}
//Ans cacığını ekle*** 
int main(){
    char quitchar = 'a';
    while (quitchar != 'q' && quitchar != 'Q')
    {
        printf("Availible Operations: ");
        for (int i = 0; i < 7; i = AddInts(i, 1))
        {
            printf(" %c", OpChar[i]);
        }
        if(LastAnswer != 0) printf(" (A for Last Answer)");
        printf("\n");
        fgets(UserInput, Length, stdin);
        DeleteSpaces();
        InstructionSetDecleration();

        printf("= %lf\n", ExecuteInstructions());
        printf("q to Exit, any other key to continue: ");
        scanf("%c", &quitchar);
        ResetVars();
    }
    return 0;
}