from pj import *

class MJ(enum.Enum):
    CLASS, STRING, RETURN = 'class', 'String', 'return'
    PUBLIC, STATIC, VOID, MAIN = 'public', 'static', 'void', 'main'
    PRINT, NEW = 'System.out.println', 'new'
    INT, BOOL, ARRAY = 'int', 'bool', 'int[]'
    IF, ELSE = 'if', 'else'
    PUTA, PLUS, MINUS, JEDNAKO, MANJE, TOČKAZ, TOČKA = '*+-=<;.'
    OOTV, OZATV, UOTV, UZATV, VOTV, VZATV = '()[]{}'
        
    class BROJ(Token):
        def vrijednost(self, mem): return int(self.sadržaj)

    class IME(Token):
        def vrijednost(self, mem): return pogledaj(mem, self)

    class LKONST(Token):
        def vrijednost(self, mem): return self.sadržaj == 'true'

class Tip(enum.Enum):
    INT = MJ.INT
    BOOLEAN = MJ.BOOL
    INTA = MJ.ARRAY
    IME = MJ.IME

def minijava_lexer(program):
    lex = Tokenizer(program)
    for znak in iter(lex.čitaj, ''):
        if znak.isspace(): lex.zanemari()
        elif znak.isalpha():
            lex.zvijezda(identifikator)
            if lex.sadržaj == 'int':
                if lex.slijedi('['):
                    if lex.slijedi(']'): yield lex.token(MJ.ARRAY)
                    else:
                        lex.vrati()
                        lex.vrati()
                        yield lex.token(MJ.INT)
                else: yield lex.token(MJ.INT)
            elif lex.sadržaj in {'true', 'false'}: yield lex.token(MJ.LKONST)
#            elif lex.sadržaj == 'System' and lex.slijedi('.'):
#                lex.zvijezda(str.isalpha)
#                lex.pročitaj('.')
#                lex.zvijezda(str.isalpha)
#                if lex.sadržaj == 'System.out.println': yield lex.token(MJ.PRINT)
            else: yield lex.literal(MJ.IME)
        elif znak.isdigit():
            lex.zvijezda(str.isdigit)
            yield lex.token(MJ.BROJ)
        else: yield lex.literal(MJ)

### Beskontekstna gramatika:
# program -> MainClass | MainClass ClassDeclarations
# ClassDeclarations -> ClassDeclaration | ClassDeclaration ClassDeclarations
# MainClass -> CLASS IME VOTV PUBLIC STATIC VOID MAIN OOTV STRING UOTV UZATV
#               IME OZATV VOTV naredbe VZATV VZATV
# ClassDeclaration -> CLASS IME (OOTV EXTENDS IME OZATV)? VOTV
#                       VarDeclarations MethodDeclarations VZATV
# VarDeclarations -> VarDeclaration | VarDeclaration VarDeclarations
# MethodDeclarations -> MethodDeclaration | MethodDeclaration MethodDeclarations
# VarDeclaration -> (INT | BOOL | ARRAY | IME) IME TOČKAZ
# MethodDeclaration -> PUBLIC (INT | BOOL | ARRAY | IME) IME OOTV
#                       (parametri)? OZATV VOTV VarDeclarations naredbe
#                       RETURN izraz TOČKA VZATV
# parametri -> (INT | BOOL | ARRAY | IME) IME (ZAREZ parametri)?
# naredba -> VOTV naredbe VZATV | IF OOTV izraz OZATV naredba ELSE naredba |
#           WHILE OOTV izraz OZATV naredba | PRINT OOTV izraz OZATV TOČKAZ |
#           IME JEDNAKO izraz TOČKAZ | IME UOTV izraz UZATV JEDNAKO izraz TOČKAZ
# naredbe -> naredba | naredba naredbe
# izrazi -> izraz (ZAREZ izrazi)?
# izraz -> konjukt | izraz AND konjukt
# konjukt -> logizraz | logizraz MANJE logizraz
# logizraz -> član | logizraz (PLUS | MINUS) član
# član -> faktor | član PUTA faktor
# faktor -> MINUS faktor | OOTV logizraz OZATV | BROJ | LKONST | IME |
#           izraz UOTV izraz UZATV | izraz TOČKA LENGTH |
#           izraz TOČKA ime OOTV (izrazi)? OZATV | THIS |
#           NEW INT UOTV izraz UZATV | NEW IME UOTV UZATV

class MiniJavaParser(Parser):
    def program(self):
        self.klase = []
        klasa = self.mainclass()
        imek = klasa.ime
        self.klase[imek] = klasa
        while not self >> E.KRAJ:
            klasa = self.klasa()
            imek = klasa.ime
            if imek in self.klase: raise SemantičkaGreška('Dvaput definirana klasa '
                                                      + imek.sadržaj)
            self.klase[imek] = klasa
        return self.klase

    def mainclass(self):
        self.pročitaj(MJ.CLASS)
        ime = self.pročitaj(MJ.IME)
        self.pročitaj(MJ.VOTV)
        funkcija = self.mainfunkcija()
        self.pročitaj(MJ.VZATV)
        return Klasa(ime, [], funkcije)

    def klasa(self):
        self.pročitaj(MJ.CLASS)
        ime = self.pročitaj(MJ.IME)
        # implementirati nasljeđivanje
        self.pročitaj(MJ.VOTV)
        atributi = [self.vardeclaration()]
        while not self >> MJ.PUBLIC:
            atributi.append(self.vardeclaration())
        funkcije = [self.funkcija()]
        while not self >> MJ.VZATV:
            funkcije.append(self.funkcija())
        self.pročitaj(MJ.VZATV)
        return Klasa(ime, atributi, funkcije)
    
    def mainfunkcija(self):
        self.pročitaj(MJ.PUBLIC)
        self.pročitaj(MJ.STATIC)
        self.pročitaj(MJ.VOID)
        self.pročitaj(MJ.MAIN)
        self.pročitaj(MJ.OOTV)
        self.pročitaj(MJ.STRING)
        self.pročitaj(MJ.UOTV)
        self.pročitaj(MJ.UZATV)
        parametar = self.pročitaj(MJ.IME)
        self.pročitaj(MJ.OZATV)
        self.pročitaj(MJ.VOTV)
        #naredbe = [self.naredba()]
        #while not self >> MJ.VZATV:
        #    naredbe.append(self.naredba())
        return Funkcija(Token(MJ.IME, 'main'), parametar, self.naredba())

    def funkcija(self):
        tip = self.pročitaj(MJ.INT, MJ.BOOL, MJ.ARRAY, MJ.IME)
        ime = self.pročitaj(MJ.IME)
        self.pročitaj(MJ.OOTV)
        parametri = self.parametri()
        self.pročitaj(MJ.OZATV)
        self.pročitaj(MJ.VOTV)
        varijable = []
        while self >> (MJ.INT, MJ.BOOL, MJ.ARRAY, MJ.IME):
            tipv = self.zadnji
            imev = self.pročitaj(MJ.IME)
            if not self >> MJ.TOČKAZ:
                self.vrati()
                self.vrati()
                break
            else: varijable.append([tipv, imev])
        #naredbe = [self.naredba()]
        #while not self >> MJ.VZATV:
        #    naredbe.append(self.naredba())
        return Funkcija(tip, ime, varijable, self.naredba())

    def parametri(self):
        parametri = []
        prvi = True
        while not self >= MJ.OZATV:
            if not prvi:
                self.pročitaj(MJ.ZAREZ)
            tipv = self.pročitaj(MJ.INT, MJ.BOOL, MJ.ARRAY, MJ.IME)
            imev = self.pročitaj(MJ.IME)
            parametri.append([tipv, imev])
        return parametri

    def naredbe(self):
        naredbe = [self.naredba()]
        while self >> MJ.TOČKAZ:
            if self >= PSK.VZATV: return Blok(naredbe)
            naredbe.append(self.naredba())
        return Blok(naredbe)

    def naredba(self):
        if self >> {MJ.IF, MJ.WHILE}:
            petlja = self.zadnji ^ MJ.WHILE
            uvjet, naredba = self.izraz(), self.naredba()
            if petlja: return Petlja(uvjet, naredba)
            inače = self.naredba() if self >> MJ.ELSE else Blok([])
            return Grananje(uvjet, naredba, inače)
        elif self >> MJ.VOTV:
            if self >> MJ.VZATV: return Blok([])
            u_zagradi = self.naredbe()
            self.pročitaj(MJ.ZATV)
            return u_zagradi
        elif self >> MJ.IME:
            ime = self.zadnji
            if ime.sadržaj == 'System':
                if self >> MJ.TOČKA:
                    out = self.pročitaj(MJ.IME)
                    if out.sadržaj != 'out':
                        raise self.greška()
                    self.pročitaj(MJ.TOČKA)
                    println = self.pročitaj(MJ.IME)
                    if println.sadržaj != 'println':
                        raise self.greška()
                    self.pročitaj(MJ.OOTV)
                    izraz = self.izraz()
                    self.pročitaj(MJ.OZATV)
                    return Print(izraz)
            self.pročitaj(PSK.JEDNAKO)
            vrijednost = self.izraz()
            return Pridruživanje(ime, vrijednost)
        elif self >> MJ.RETURN:
            return Return(self.izraz())
        else: raise self.greška()

    def izraz(self):
        konjukti = [self.konjukt()]
        while self >> MJ.AND: konjukti.append(self.konjukt())
        return konjukti[0] if len(konjukti) == 1 else Konjukcija(konjukti)

    def konjukt(self):
        logizrazi = [self.logizraz()]
        while self >> MJ.MANJE: logizrazi.append(self.logizraz())
        return logizrazi[0] if len(logizrazi) == 1 else Usporedba(logizrazi)

    def logizraz(self):
        članovi = [self.član()]
        while self >> {MJ.PLUS, MJ.MINUS}:
            operator = self.zadnji
            dalje = self.član()
            članovi.append(dalje if operator ^ MJ.PLUS else Suprotan(dalje))
        return članovi[0] if len(članovi) == 1 else Zbroj(članovi)

    def član(self):
        faktori = [self.faktor()]
        while self >> MJ.PUTA: faktori.append(self.faktor())
        return faktori[0] if len(faktori) == 1 else Umnožak(faktori)

    def faktor(self):
        if self >> MJ.MINUS: return Suprotan(self.faktor())
        elif self >> MJ.OTV:
            u_zagradi = self.logizraz()
            self.pročitaj(PSK.ZATV)
            return u_zagradi
        elif self >> MJ.BROJ: return self.zadnji
        elif self >> MJ.LKONST: return self.zadnji
        #treba ostatak

    start = program


### Apstraktna sintaksna stabla
# Klasa: ime atributi funkcije
# Funkcije: ime parametri naredba
# Blok: naredbe
#




##program = '''
##class Factorial{
##    public static void main(String[] a){
##        System.out.println(new Fac().ComputeFac(10));
##    }
##}
##
##class Fac {
##    public int ComputeFac(int num){
##        int num_aux ;
##        if (num < 1)
##            num_aux = 1 ;
##        else
##            num_aux = num * (this.ComputeFac(num-1)) ;
##        return num_aux ;
##    }
##}
##'''
##tokeni = list(minijava_lexer(program))
##print(*tokeni)

program = '''
class MyClass{
    public static void main(String[] a){
        System.out.println(5);
    }
}
'''
minijava = list(minijava_lexer(program))
print(*minijava)
