from pj import *

class MJ(enum.Enum):
    INT, BOOLEAN, STRING = 'int', 'boolean', 'String'
    OOTV, OZATV, UOTV, UZATV, VOTV, VZATV = '()[]{}'
    NEG, MINUS, PLUS, PUTA, JEDNAKO, MANJE, TOČKA, TOČKAZ = '!-+*=<.;'
    AND = '&&'
    IF, ELSE, WHILE, RETURN, NEW = 'if', 'else', 'while', 'return', 'new'
    CLASS, PUBLIC, STATIC, VOID, MAIN = 'class', 'public', 'static', 'void', 'main'

    class BROJ(Token):
        def vrijednost(self, mem): return int(self.sadržaj)

    class IME(Token):
        def vrijednost(self, mem): return pogledaj(mem, self)

    class LKONST(Token):
        def vrijednost(self, mem): return self.sadržaj == 'true'

def minijava_lexer(program):
    lex = Tokenizer(program)
    for znak in iter(lex.čitaj, ''):
        if znak.isspace(): lex.zanemari()
        elif znak.isalpha():
            lex.zvijezda(identifikator)
            if lex.sadržaj in {'true', 'false'}: yield lex.token(MJ.LKONST)
            else: yield lex.literal(MJ.IME)
        elif znak.isdigit():
            lex.zvijezda(str.isdigit)
            yield lex.token(MJ.BROJ)
        else: yield lex.literal(MJ)
    
### Beskontekstna gramatika:
# CLASS_IME, ARG_IME, EXTENDS_IME, METHOD_IME, TIP_IME, VARIJABLA_IME
# RETURN_TIP
# RETURN_IZRAZ, UVJET_IZRAZ, INDEKS_IZRAZ, VELIČINA_IZRAZ
# TRUE_NAREDBA, FALSE_NAREDBA
# PRINT = System.out.println
# program -> MainClass ClassDeclarations
# MainClass -> CLASS IME VOTV PUBLIC STATIC VOID MAIN OOTV STRING UOTV UZATV
#               IME OZATV VOTV naredbe VZATV VZATV
# ClassDeclarations -> ClassDeclaration | ClassDeclaration ClassDeclarations
# ClassDeclaration -> CLASS IME (OOTV EXTENDS IME OZATV)? VOTV
#                       VarDeclarations MethodDeclarations VZATV
# VarDeclarations -> VarDeclaration | VarDeclaration VarDeclarations
# VarDeclaration -> tip IME TOČKAZ
# MethodDeclarations -> MethodDeclaration | MethodDeclaration MethodDeclarations
# MethodDeclaration -> PUBLIC tip IME OOTV (parametri)? OZATV VOTV
#                       VarDeclarations naredbe RETURN izraz TOČKAZ VZATV
# parametri -> tip IME (ZAREZ parametri)?
# tip -> primitivni | lista | složeni
# primitivni -> INT | BOOLEAN
# lista -> INT UOTV UZATV
# složeni -> IME
# naredbe -> naredba | naredba naredbe
# naredba -> blok | if | while | print | pridruživanje
# blok -> VOTV naredbe VZATV
# if -> IF OOTV izraz OZATV naredbe ELSE naredba
# while -> WHILE OOTV izraz OZATV naredba
# print -> PRINT OOTV izraz OZATV TOČKAZ
# pridruživanje -> IME (UOTV izraz UZATV)? JEDNAKO izraz TOČKAZ
# izrazi -> izraz (ZAREZ izrazi)?
# izraz -> konjunkt | izraz AND konjunkt
# konjunkt -> uspoređen | konjunkt MANJE uspoređen
# uspoređen -> član | uspoređen (PLUS | MINUS) član
# član -> faktor | član PUTA faktor
# faktor -> MINUS faktor | NEG faktor | postfix | primarni | grupa
# postfix -> indeksiranje | pozivmetode | length
# indeksiranje ->
# FALI!
# grupa -> OOTV izraz OZATV
# primarni -> BROJ | LKONST | THIS | konstruktor | IME
# konstruktor -> NEW INT UOTV izraz UZATV | NEW IME OOTV OZATV

class MiniJavaParser(Parser):
    def program(self):
        mainclass = self.mainclass()
        self.klase = []
        while not self >> E.KRAJ:
            klasa = self.classdeclaration()
            imek = klasa.ime
            if imek in self.klase: SemantičkaGreška('Dvaput definirana klasa '
                                                      + imek.sadržaj)
            self.klase[imek] = klasa
        return Program(mainclass, self.klase)

    def mainclass(self):
        self.pročitaj(MJ.CLASS)
        ime = self.pročitaj(MJ.IME)
        self.pročitaj(MJ.VOTV)
        self.pročitaj(MJ.PUBLIC)
        self.pročitaj(MJ.STATIC)
        self.pročitaj(MJ.VOID)
        self.pročitaj(MJ.MAIN)
        self.pročitaj(MJ.OOTV)
        self.pročitaj(MJ.STRING)
        self.pročitaj(MJ.UOTV)
        self.pročitaj(MJ.UZATV)
        arg = self.pročitaj(MJ.IME)
        self.pročitaj(MJ.OZATV)
        self.pročitaj(MJ.VOTV)
        naredbe = self.naredbe() # implementirati naredbe!
        self.pročitaj(MJ.VZATV)
        self.pročitaj(MJ.VZATV)
        return MainClass(ime, arg, naredbe)

    def classdeclaration(self):
        self.pročitaj(MJ.CLASS)
        ime = self.pročitaj(MJ.IME)
        if self >> MJ.OOTV:
            extends = self.pročitaj(MJ.EXTENDS)
            imeroditelja = self.pročitaj(MJ.IME)
            self.pročitaj(MJ.OZATV)
        else:
            extends = None
        self.pročitaj(MJ.VOTV)
        vardeklaracije = []
        while not self >> {MJ.PUBLIC, MJ.VZATV}:
            vardeklaracije.append(self.vardeclaration())
        if self.zadnji ^ MJ.PUBLIC:
            self.vrati()
        else: return ClassDeclaration(ime, extends, vardeklaracije, None)
        metdeklaracije = []
        while not self >> MJ.VZATV:
            metdeklaracije.append(self.methoddeclaration())
        return ClassDeclaration(ime, extends, vardeklaracije, metdeklaracije)

    def vardeclaration(self):
        tip = self.tip() # implementirati tip!
        ime = self.pročitaj(MJ.IME)
        self.pročitaj(MJ.TOČKAZ)
        return VarDeclaration(tip, ime)

# MethodDeclarations -> MethodDeclaration | MethodDeclaration MethodDeclarations
# MethodDeclaration -> PUBLIC tip IME OOTV (parametri)? OZATV VOTV
#                       VarDeclarations naredba RETURN izraz TOČKAZ VZATV

    def methoddeclaration(self):
        self.pročitaj(MJ.PUBLIC)
        returntip = self.tip() # i tu je tip
        ime = self.pročitaj(MJ.IME)
        self.pročitaj(MJ.OOTV)
        if not self >> MJ.OZATV:
            parametri = self.parametri() # implementirati parametri!
            self.pročitaj(MJ.OZATV)
        else: parametri = None
        self.pročitaj(MJ.VOTV)
        vardeklaracije = []
        while self >> {MJ.INT, MJ.BOOLEAN, MJ.IME}:
            if self.zadnji ^ MJ.IME:
                if self >> MJ.IME:
                    if not self >> MJ.TOČKAZ:
                        self.vrati()
                        self.vrati()
                        break;
                    else:
                        self.vrati()
                        self.vrati()
            self.vrati()
            vardeklaracije.append(self.vardeclaration())
        naredbe = []
        while not self >> MJ.RETURN:
            naredbe.append(self.naredba())
        returns = self.izraz()
        self.pročitaj(MJ.TOČKAZ)
        self.pročitaj(MJ.VZATV)
        return MethodDeclaration(returntip, ime, parametri, vardeklaracije, naredbe, returns)

    start = program

### Apstraktna sintaksna stabla:
# Program(MainClass klase) (klase: lista ClassDeclaration)
# MainClass(ime arg naredbe)
# ClassDeclaration(ime extends vardeklaracije metdeklaracije)
# PrintStatement(izraz)
# MethodCallExpresion(objekt ime arg)
# ConstructorExpression(veličina ime)
# Izraz() VRATITI SE NA OVO! BinOp, Unarna, IdentifierExpression(ime),
#                               GroupExpression, ThisExpression, IntegerExpression
# VarDeclaration(tip ime)
# MethodDeclaration(returntip ime parametri vardeklaracije naredbe returns)
# Parametar(tip ime)
# If(uvjet naredba inače) Uvjet(lhs rhs)
# Pridruživanje(varijabla indeks izraz)
# BinOp(op lijevo desno)
# Unarna(op ispod)

program = '''
class Factorial{
    public static void main(String[] a){
        System.out.println(new Fac().ComputeFac(10));
    }
}

class Fac {
    public int ComputeFac(int num){
        int num_aux ;
        if (num < 1)
            num_aux = 1 ;
        else
            num_aux = num * (this.ComputeFac(num-1)) ;
        return num_aux ;
    }
}
'''

minijava = list(minijava_lexer(program))
print(*minijava)
