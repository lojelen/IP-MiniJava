from pj import *

class MJ(enum.Enum):
    INT, BOOLEAN, STRING = 'int', 'boolean', 'String'
    OOTV, OZATV, UOTV, UZATV, VOTV, VZATV = '()[]{}'
    NEG, MINUS, PLUS, PUTA, JEDNAKO, MANJE, TOČKA, TOČKAZ, ZAREZ = '!-+*=<.;,'
    AND = '&&'
    IF, ELSE, WHILE, RETURN, NEW, THIS, LENGTH = 'if', 'else', 'while', 'return', 'new', 'this', 'length'
    CLASS, PUBLIC, STATIC, VOID, MAIN = 'class', 'public', 'static', 'void', 'main'
    ARRAY = 'int[]'

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
# RETURN_IZRAZ, UVJET_IZRAZ, INDEKS_IZRAZ, VELIČINA_IZRAZ, ARGUMENT_IZRAZ
# TRUE_NAREDBA, FALSE_NAREDBA
# PRINT = System.out.println
# program -> MainClass | MainClass ClassDeclarations
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
# naredba -> blok | ako | dok | ispis | pridruživanje | return
# return -> RETURN izraz TOČKAZ
# blok -> VOTV naredbe VZATV
# ako -> IF OOTV izraz OZATV naredbe ELSE naredba
# dok -> WHILE OOTV izraz OZATV naredba
# ispis -> PRINT OOTV izraz OZATV TOČKAZ
# pridruživanje -> IME (UOTV izraz UZATV)? JEDNAKO izraz TOČKAZ
# izrazi -> izraz (ZAREZ izrazi)?
# izraz -> konjunkt | izraz AND konjunkt
# konjunkt -> uspoređen | konjunkt MANJE uspoređen
# uspoređen -> član | uspoređen (PLUS | MINUS) član
# član -> faktor | član PUTA faktor
# faktor -> MINUS faktor | NEG faktor | postfix | primarni | grupa
# postfix -> indeksiranje | pozivmetode | length
# indeksiranje -> IME UOTV izraz UZATV
# pozivmetode -> konstruktor TOČKA IME OOTV izrazi? OZATV | IME TOČKA IME OOTV izrazi? OZATV <-- Čitaj izraze u pozivmetode!
#               | THIS TOČKA IME OOTV izrazi? OZATV
# Nije desno pravilo konstruktora!
# length -> konstruktor TOČKA LENGTH | IME TOČKA LENGTH <--- vratiti se na ovo
# grupa -> OOTV izraz OZATV
# primarni -> BROJ | LKONST | THIS | konstruktor | IME
# konstruktor -> NEW INT UOTV izraz UZATV | NEW IME OOTV OZATV

class MiniJavaParser(Parser):
    def program(self):
        mainclass = self.mainclass()
        self.klase = {}
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
        naredbe = []
        while not self >> MJ.VZATV:
            naredbe.append(self.naredba())
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
            extends = False
        self.pročitaj(MJ.VOTV)
        vardeklaracije = []
        while not self >> {MJ.PUBLIC, MJ.VZATV}:
            vardeklaracije.append(self.vardeclaration())
        if self.zadnji ^ MJ.PUBLIC:
            self.vrati()
        else: return ClassDeclaration(ime, extends, vardeklaracije, False)
        metdeklaracije = []
        while not self >> MJ.VZATV:
            metdeklaracije.append(self.methoddeclaration())
        return ClassDeclaration(ime, extends, vardeklaracije, metdeklaracije)

    def vardeclaration(self):
        tip = self.tip()
        ime = self.pročitaj(MJ.IME)
        self.pročitaj(MJ.TOČKAZ)
        return VarDeclaration(tip, ime)

    def methoddeclaration(self):
        self.pročitaj(MJ.PUBLIC)
        returntip = self.tip()
        ime = self.pročitaj(MJ.IME)
        self.pročitaj(MJ.OOTV)
        if not self >> MJ.OZATV:
            parametri = self.parametri()
            self.pročitaj(MJ.OZATV)
        else: parametri = False
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

    def parametri(self):
        parametri = []
        tip = self.tip()
        ime = self.pročitaj(MJ.IME)
        parametri.append(Parametar(tip, ime))
        while self >> MJ.ZAREZ:
            tip = self.tip()
            ime = self.pročitaj(MJ.IME)
            parametri.append(Parametar(tip, ime))
        return parametri

    def tip(self):
        if self >> MJ.INT:
            tip = self.zadnji
            if self >> MJ.UOTV:
                self.pročitaj(MJ.UZATV)
                return Token(MJ.ARRAY, 'int[]')
            else: return tip
        elif self >> MJ.BOOLEAN:
            return self.zadnji
        self.pročitaj(MJ.IME)
        return self.zadnji
    
    def naredba(self):
        if self >> MJ.VOTV:
            if self >> MJ.VZATV: return Blok([])
            naredbe = [self.naredba()]
            while not self >> MJ.VZATV:
                naredbe.append(self.naredba())
            return Blok(naredbe)
        elif self >> MJ.IF:
            return self.ako()
        elif self >> MJ.WHILE:
            return self.dok()
        ime = self.pročitaj(MJ.IME)
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
                self.pročitaj(MJ.TOČKAZ)
                return PrintStatement(izraz)
            else: raise self.greška()
        else:
            if self >> MJ.UOTV:
                indeks = self.izraz()
                self.pročitaj(MJ.UZATV)
            else: indeks = False
            self.pročitaj(MJ.JEDNAKO)
            izraz = self.izraz()
            self.pročitaj(MJ.TOČKAZ)
            return Pridruživanje(ime, indeks, izraz)

    def ako(self):
        self.pročitaj(MJ.OOTV)
        uvjet = self.izraz()
        self.pročitaj(MJ.OZATV)
        naredba = self.naredba()
        self.pročitaj(MJ.ELSE)
        inače = self.naredba()
        return Ako(uvjet, naredba, inače)

    def dok(self):
        self.pročitaj(MJ.OOTV)
        uvjet = self.izraz()
        self.pročitaj(MJ.OZATV)
        naredba = self.naredba()
        return Dok(uvjet, naredba)

    def izraz(self):
        konjukti = [self.konjukt()]
        while self >> MJ.AND: konjukti.append(self.konjukt())
        return konjukti[0] if len(konjukti) == 1 else Konjukcija(konjukti)

    def konjukt(self):
        uspoređeni = [self.uspoređen()]
        while self >> MJ.MANJE: uspoređeni.append(self.uspoređen())
        return uspoređeni[0] if len(uspoređeni) == 1 else Usporedba(uspoređeni)

    def uspoređen(self):
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
        elif self >> MJ.NEG: return Negacija(self.faktor())
        elif self >> MJ.OOTV:
            u_zagradi = self.izraz()
            self.pročitaj(MJ.OZATV)
            return u_zagradi
        elif self >> MJ.BROJ: return self.zadnji
        elif self >> MJ.LKONST: return self.zadnji
        elif self >> MJ.THIS:
            objekt = self.zadnji
            if self >> MJ.TOČKA:
                return self.pozivmetode(objekt)
            else: return objekt
        elif self >> MJ.NEW:
            objekt =  self.konstruktor()
            if self >> MJ.TOČKA:
                if self >> MJ.LENGTH: return Length(objekt)
                else:
                    return self.pozivmetode(objekt)
        objekt = self.pročitaj(MJ.IME) # ne moramo raiseati grešku
        if self >> MJ.UOTV:
            izraz = self.izraz()
            self.pročitaj(MJ.UZATV)
            return Indeksiranje(objekt, izraz)
        elif self >> MJ.TOČKA:
            if self >> MJ.LENGTH:
                return Length(objekt)
            return self.pozivmetode(objekt)
        else: return objekt

    def pozivmetode(self, objekt):
        metoda = self.pročitaj(MJ.IME)
        self.pročitaj(MJ.OOTV)
        if self >> MJ.OZATV:
            argumenti = False
        else:
            argumenti = [self.izraz()]
            while self >> MJ.ZAREZ:
                argumenti.append(self.izraz())
        self.pročitaj(MJ.OZATV)
        return MethodCallExpression(objekt, metoda, argumenti)

    def konstruktor(self):
        if self >> MJ.INT:
            self.pročitaj(MJ.UOTV)
            veličina = self.izraz()
            self.pročitaj(MJ.UZATV)
            return ConstructorExpression(veličina, False) # False = int
        ime = self.pročitaj(MJ.IME)
        self.pročitaj(MJ.OOTV)
        self.pročitaj(MJ.OZATV)
        return ConstructorExpression(False, ime) # False = objekt složenog tipa
    
    start = program

class Program(AST('mainclass klase')): pass

class MainClass(AST('ime arg naredbe')): pass

class ClassDeclaration(AST('ime extends vardeklaracije metdeklaracije')): pass

class PrintStatement(AST('izraz')): pass

class MethodCallExpression(AST('objekt ime arg')): pass

class ConstructorExpression(AST('veličina ime')): pass

class Indeksiranje(AST('varijabla veličina')): pass

class Length(AST('varijabla')): pass

class VarDeclaration(AST('tip ime')): pass

class MethodDeclaration(AST('returntip ime parametri vardeklaracije naredbe returns')): pass

class Parametar(AST('tip ime')): pass

class Ako(AST('uvjet naredba inače')):
    def izvrši(self, mem):
        if self.uvjet.vrijednost(mem): self.naredba.izvrši(mem) 
        else: self.inače.izvrši(mem)

class Dok(AST('uvjet naredba')):
    def izvrši(self, mem):
        while self.uvjet.vrijednost(mem): self.naredba.izvrši(mem)

#ovo je bezveze
class Pridruživanje(AST('varijabla indeks izraz')):
    def izvrši(self, mem):
        if self.indeks:
            mem[self.varijabla.sadržaj][self.indeks.sadržaj] = self.izraz.vrijednost(mem)
        else:
            mem[self.varijabla.sadržaj] = self.izraz.vrijednost(mem)

class Konjunkcija(AST('konjukti')):
    def vrijednost(self, mem):
        return all(konjukt.vrijednost(mem) for konjukt in self.konjukti)

class Usporedba(AST('uspoređeni')):
    def vrijednost(self, mem):
        l, d, *rest = self.uspoređeni
        vr = l.vrijednost(mem) < d.vrijednost(mem)
        for uspoređen in rest :
            vr = vr < uspoređen.vrijednost(mem)
        return vr 

class Zbroj(AST('članovi')):
    def vrijednost(self, mem):
        return sum(članovi.vrijednost(mem) for član in self.članovi)

class Umnožak(AST('faktori')):
    def vrijednost(self, mem):
        f = 1
        for faktor in self.faktori: f *= faktor.vrijednost(mem)
        return f

class Suprotan(AST('dolje')):
    def vrijednost(self, mem):
        return -self.dolje.vrijednost(mem)

class Negacija(AST('dolje')):
    def vrijednost(self, mem):
        return not(self.dolje.vrijednost(mem))

class Blok(AST('naredbe')):
    def izvrši(self, mem):
        for naredba in self.naredbe: naredba.izvrši(mem)

### Apstraktna sintaksna stabla:
# Program(MainClass klase) (klase: lista ClassDeclaration)
# MainClass(ime arg naredbe)
# ClassDeclaration(ime extends vardeklaracije metdeklaracije)
# PrintStatement(izraz)
# MethodCallExpresion(objekt ime arg)
# ConstructorExpression(veličina ime)
# Indeksiranje(varijabla veličina)
# Length(varijabla)
# Izraz() VRATITI SE NA OVO! BinOp, Unarna, IdentifierExpression(ime),
#                               GroupExpression, ThisExpression, IntegerExpression
# VarDeclaration(tip ime)
# MethodDeclaration(returntip ime parametri vardeklaracije naredbe returns)
# Parametar(tip ime)
# Ako(uvjet naredba inače) Uvjet(lhs rhs)
# Dok(uvjet naredba)
# Pridruživanje(varijabla indeks izraz)
# BinOp(op članovi) --> Konjunkcija, Usporedba, Zbroj, Umnožak
# Unarna(op ispod) --> Suprotan, Negacija
# Blok(naredbe)

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
            num_aux = num * (this.ComputeFac(num-1));
        return num_aux ;
    }
}
'''

tokeni = list(minijava_lexer(program))
print(*tokeni)

ast = MiniJavaParser.parsiraj(tokeni)
print(ast)