from pj import *

class MJ(enum.Enum):
    INT, BOOLEAN, STRING = 'int', 'boolean', 'String'
    OOTV, OZATV, UOTV, UZATV, VOTV, VZATV = '()[]{}'
    NEG, MINUS, PLUS, PUTA, JEDNAKO, MANJE, TOČKA, TOČKAZ, ZAREZ = '!-+*=<.;,'
    AND = '&&'
    IF, ELSE, WHILE, RETURN, NEW, THIS, LENGTH = 'if', 'else', 'while', 'return', 'new', 'this', 'length'
    CLASS, PUBLIC, STATIC, VOID, MAIN, EXTENDS = 'class', 'public', 'static', 'void', 'main', 'extends'
    ARRAY = 'int[]'

    class BROJ(Token):
        def vrijednost(self, mem, lokalni): return int(self.sadržaj)
        def provjeri_tip(self, mem, lokalni): return MJ.INT

    class IME(Token):
        def vrijednost(self, mem, lokalni): return pogledaj(lokalni, self)
        def provjeri_tip(self, mem, lokalni):
            return pogledaj(lokalni[0], self)[0]

    class LKONST(Token):
        def vrijednost(self, mem, lokalni): return self.sadržaj == 'true'
        def provjeri_tip(self, mem, lokalni): return MJ.BOOLEAN

def minijava_lexer(program):
    lex = Tokenizer(program)
    for znak in iter(lex.čitaj, ''):
        if znak.isspace(): lex.zanemari()
        elif znak == '/':
            if lex.slijedi('/'):
                lex.pročitaj_do('\n')
                lex.zanemari()
            else:
                lex.pročitaj('*')
                while(1):
                    lex.pročitaj_do('*')
                    if lex.slijedi('/'):
                        lex.zanemari()
                        break
        elif znak == '&':
            lex.pročitaj('&')
            yield lex.literal(MJ.AND)
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
# pozivmetode -> konstruktor TOČKA IME OOTV izrazi? OZATV | IME TOČKA IME OOTV izrazi? OZATV
#               | THIS TOČKA IME OOTV izrazi? OZATV
# Nije desno pravilo konstruktora!
# length -> konstruktor TOČKA LENGTH | IME TOČKA LENGTH
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
        klase = []
        for ključ in self.klase:
            klase.append(self.klase[ključ])
        return Program(mainclass, klase)

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
            naredbe.append(self.naredba(ime))
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
            imeroditelja = False
        self.pročitaj(MJ.VOTV)
        vardeklaracije = []
        while not self >> {MJ.PUBLIC, MJ.VZATV}:
            vardeklaracije.append(self.vardeclaration())
        if self.zadnji ^ MJ.PUBLIC:
            self.vrati()
        else: return ClassDeclaration(ime, extends, vardeklaracije, False)
        metdeklaracije = []
        while not self >> MJ.VZATV:
            metdeklaracije.append(self.methoddeclaration(ime))
        return ClassDeclaration(ime, imeroditelja, vardeklaracije, metdeklaracije)

    def vardeclaration(self):
        tip = self.tip()
        ime = self.pročitaj(MJ.IME)
        self.pročitaj(MJ.TOČKAZ)
        return VarDeclaration(tip, ime)

    def methoddeclaration(self, parent):
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
        while self >> {MJ.INT, MJ.BOOLEAN, MJ.IME, MJ.ARRAY}:
            zadnji = self.zadnji
            if zadnji ^ MJ.IME:
                if zadnji.sadržaj not in self.klase:
                    self.vrati()
                    break
            self.vrati()
            vardeklaracije.append(self.vardeclaration())
        naredbe = []
        while not self >> MJ.RETURN:
            naredbe.append(self.naredba(parent))
        returns = self.izraz(parent)
        self.pročitaj(MJ.TOČKAZ)
        self.pročitaj(MJ.VZATV)
        return MethodDeclaration(parent, returntip, ime, parametri, vardeklaracije, naredbe, returns)

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
    
    def naredba(self, parent):
        if self >> MJ.VOTV:
            if self >> MJ.VZATV: return Blok([])
            naredbe = [self.naredba(parent)]
            while not self >> MJ.VZATV:
                naredbe.append(self.naredba(parent))
            return Blok(naredbe)
        elif self >> MJ.IF:
            return self.ako(parent)
        elif self >> MJ.WHILE:
            return self.dok(parent)
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
                izraz = self.izraz(parent)
                self.pročitaj(MJ.OZATV)
                self.pročitaj(MJ.TOČKAZ)
                return PrintStatement(izraz)
            else: raise self.greška()
        else:
            if self >> MJ.UOTV:
                indeks = self.izraz(parent)
                self.pročitaj(MJ.UZATV)
            else: indeks = False
            self.pročitaj(MJ.JEDNAKO)
            izraz = self.izraz(parent)
            self.pročitaj(MJ.TOČKAZ)
            return Pridruživanje(ime, indeks, izraz)

    def ako(self, parent):
        self.pročitaj(MJ.OOTV)
        uvjet = self.izraz(parent)
        self.pročitaj(MJ.OZATV)
        naredba = self.naredba(parent)
        self.pročitaj(MJ.ELSE)
        inače = self.naredba(parent)
        return Ako(uvjet, naredba, inače)

    def dok(self, parent):
        self.pročitaj(MJ.OOTV)
        uvjet = self.izraz(parent)
        self.pročitaj(MJ.OZATV)
        naredba = self.naredba(parent)
        return Dok(uvjet, naredba)

    def izraz(self, parent):
        konjunkti = [self.konjunkt(parent)]
        while self >> MJ.AND: konjunkti.append(self.konjunkt(parent))
        return konjunkti[0] if len(konjunkti) == 1 else Konjunkcija(konjunkti)

    def konjunkt(self, parent):
        uspoređeni = [self.uspoređen(parent)]
        while self >> MJ.MANJE: uspoređeni.append(self.uspoređen(parent))
        return uspoređeni[0] if len(uspoređeni) == 1 else Usporedba(uspoređeni)

    def uspoređen(self, parent):
        članovi = [self.član(parent)]
        while self >> {MJ.PLUS, MJ.MINUS}:
            operator = self.zadnji
            dalje = self.član(parent)
            članovi.append(dalje if operator ^ MJ.PLUS else Suprotan(dalje))
        return članovi[0] if len(članovi) == 1 else Zbroj(članovi)

    def član(self, parent):
        faktori = [self.faktor(parent)]
        while self >> MJ.PUTA: faktori.append(self.faktor(parent))
        return faktori[0] if len(faktori) == 1 else Umnožak(faktori)

    def faktor(self, parent):
        if self >> MJ.MINUS: return Suprotan(self.faktor())
        elif self >> MJ.NEG: return Negacija(self.faktor())
        elif self >> MJ.OOTV:
            u_zagradi = self.izraz(parent)
            self.pročitaj(MJ.OZATV)
            return u_zagradi
        elif self >> MJ.BROJ: return self.zadnji
        elif self >> MJ.LKONST: return self.zadnji
        elif self >> MJ.THIS:
            objekt = self.zadnji
            if self >> MJ.TOČKA:
                return self.pozivmetode(objekt, parent)
            else: return objekt
        elif self >> MJ.NEW:
            objekt =  self.konstruktor(parent)
            if self >> MJ.TOČKA:
                if self >> MJ.LENGTH: return Length(objekt)
                else:
                    return self.pozivmetode(objekt, parent)
            else: return objekt
        objekt = self.pročitaj(MJ.IME)
        if self >> MJ.UOTV:
            izraz = self.izraz(parent)
            self.pročitaj(MJ.UZATV)
            return Indeksiranje(objekt, izraz)
        elif self >> MJ.TOČKA:
            if self >> MJ.LENGTH:
                return Length(objekt)
            return self.pozivmetode(objekt)
        else: return objekt

    def pozivmetode(self, objekt, parent):
        if objekt ^ MJ.THIS:
               objekt = parent
        metoda = self.pročitaj(MJ.IME)
        self.pročitaj(MJ.OOTV)
        if self >> MJ.OZATV:
            argumenti = False
        else:
            argumenti = [self.izraz(parent)]
            while self >> MJ.ZAREZ:
                argumenti.append(self.izraz(parent))
        self.pročitaj(MJ.OZATV)
        return MethodCallExpression(parent, objekt, metoda, argumenti)

    def konstruktor(self, parent):
        if self >> MJ.INT:
            self.pročitaj(MJ.UOTV)
            veličina = self.izraz(parent)
            self.pročitaj(MJ.UZATV)
            return ConstructorExpression(veličina, False) # False = int
        ime = self.pročitaj(MJ.IME)
        self.pročitaj(MJ.OOTV)
        self.pročitaj(MJ.OZATV)
        return ConstructorExpression(False, ime) # False = objekt složenog tipa
    
    start = program

class Program(AST('mainclass klase')):
    def izvrši(self):
        memorija = {} # globalna memorija, klase + njihovi namespaceovi i metode + njihovi namespaceovi
        symtab = {} # tablica tipova
        memorija[self.mainclass.ime.sadržaj] = [self.mainclass.arg.sadržaj]
        symtab[self.mainclass.arg.sadržaj] = [MJ.STRING, None] # vrijednosti u symtab su liste tipova + veličina

        for klasa in self.klase:
            memorija[klasa.ime.sadržaj] = [] # inicijaliziramo na prazno polje
            klasa.dekl(memorija)
        self.mainclass.dekl(memorija, symtab) # izvrši se main funkcija

class MainClass(AST('ime arg naredbe')):
    def dekl(self, mem, symtab):
        for naredba in self.naredbe:
            lokalni = {}
            lokalni[0] = symtab
            lokalni[self.arg.sadržaj] = False
            naredba.izvrši(mem, lokalni)

class ClassDeclaration(AST('ime extends vardeklaracije metdeklaracije')):
    def dekl(self, mem):
        if self.extends:
            lokalni = mem[self.extends.sadržaj].copy() # nasljeđuje namespace klase roditelja
        else:
            lokalni = {}
            lokalni[0] = {}
        for ključ in lokalni:
            if isinstance(lokalni[ključ], MethodDeclaration):
                mem[self.ime.sadržaj + ključ] = mem[self.extends.sadržaj + ključ]
        lokalni[self.ime.sadržaj] = [True, 1]
        symtab = {}
        for vardekl in self.vardeklaracije:
            if vardekl.tip ^ MJ.ARRAY:
                lokalni[vardekl.dekl(mem, symtab).sadržaj] = {}
            else:
                lokalni[vardekl.dekl(mem, symtab).sadržaj] = None
        lokalni[0].update(symtab)
        for metdekl in self.metdeklaracije:
            lokalni[metdekl.ime.sadržaj] = metdekl # spremamo sve metode u lokalnu memoriju
        for metdekl in self.metdeklaracije:
            metdekl.dekl(mem, lokalni) # šaljemo lokalnu memoriju metode
        mem[self.ime.sadržaj] = lokalni

class PrintStatement(AST('izraz')):
    def izvrši(self, mem, lokalni):
        print(self.izraz.vrijednost(mem, lokalni))

class MethodCallExpression(AST('parent objekt ime arg')): # poziv metode
    def vrijednost(self, mem, lokalni):
        if isinstance(self.objekt, ConstructorExpression):
            ime = self.objekt.ime
        else:
            if not lokalni[self.objekt.sadržaj][0]:
                raise SemantičkaGreška("Nije instanciran objekt klase")
            ime = self.parent # dobili smo ime klase
        namespace = pogledaj(mem, ime) # namespace klase
        metoda = pogledaj(namespace, self.ime) # metoda (MethodDeclaration)
        namespace_metode = pogledaj(mem, Token(MJ.IME, ime.sadržaj + self.ime.sadržaj)) # lok. memorija
        for parametar, argument in zip(metoda.parametri, self.arg):
            if (parametar.tip ^ argument.provjeri_tip(mem, lokalni)):
               namespace_metode[parametar.ime.sadržaj] = argument.vrijednost(mem, lokalni)
            else: raise parametar.tip.krivi_tip(parametar.tip.tip, argument.provjeri_tip(mem, lokalni))
        for naredba in metoda.naredbe:
            naredba.izvrši(mem, namespace_metode)
        if not metoda.returntip ^ metoda.returns.provjeri_tip(mem, namespace_metode):
            raise (metoda.returntip).krivi_tip(metoda.returntip.tip, metoda.returns.provjeri_tip(mem, namespace_metode))
        return metoda.returns.vrijednost(mem, namespace_metode)
    
    def provjeri_tip(self, mem, lokalni):
        if isinstance(self.objekt, ConstructorExpression):
            ime = self.objekt.ime
        else: ime = self.objekt
        namespace = pogledaj(mem, ime) # namespace klase
        metoda = pogledaj(namespace, self.ime)
        return metoda.returntip.tip

class ConstructorExpression(AST('veličina ime')):
    def vrijednost(self, mem, lokalni):
        if (self.ime == False):
            return {}
        else:
            return True

    def provjeri_tip(self, mem, lokalni):
        if (self.ime == False):
            return MJ.ARRAY
        else:
            return ime.sadržaj

class Indeksiranje(AST('varijabla veličina')):
    def vrijednost(self, mem, lokalni):
        return pogledaj(lokalni, self.varijabla)[str(self.veličina.vrijednost(mem, lokalni))]

    def provjeri_tip(self, mem, lokalni):
        return MJ.INT

class Length(AST('varijabla')):
    def vrijednost(self, mem, lokalni):
        return pogledaj(lokalni[0], self.varijabla)[1]

    def provjeri_tip(self, mem, lokalni):
        return MJ.INT

class VarDeclaration(AST('tip ime')):
    def dekl(self, mem, symtab):
        if self.tip ^ MJ.IME:
            symtab[self.ime.sadržaj] = [self.tip.sadržaj, 1]
        else:
            symtab[self.ime.sadržaj] = [self.tip.tip, 1]
        return self.ime

# deklaracija metode
class MethodDeclaration(AST('parent returntip ime parametri vardeklaracije naredbe returns')):
    def dekl(self, mem, lokalni): # lokalni je lokalna memorija klase
        metlokalni = lokalni.copy()
        for parametar in self.parametri:
            if parametar.tip ^ MJ.IME:
                metlokalni[0][parametar.ime.sadržaj] = [parametar.tip.sadržaj, 1]
            else:
                metlokalni[0][parametar.ime.sadržaj] = [parametar.tip.tip, 1]
        symtab = {}
        for vardekl in self.vardeklaracije:
            if vardekl.tip ^ MJ.ARRAY:
                metlokalni[vardekl.dekl(mem, symtab).sadržaj] = {}
            else:
                metlokalni[vardekl.dekl(mem, symtab).sadržaj] = None
        metlokalni[0].update(symtab)
        mem[self.parent.sadržaj + self.ime.sadržaj] = metlokalni

class Parametar(AST('tip ime')): pass #

class Ako(AST('uvjet naredba inače')):
    def izvrši(self, mem, lokalni):
        if self.uvjet.vrijednost(mem, lokalni): self.naredba.izvrši(mem, lokalni) 
        else: self.inače.izvrši(mem, lokalni)

class Dok(AST('uvjet naredba')):
    def izvrši(self, mem, lokalni):
        while self.uvjet.vrijednost(mem, lokalni): self.naredba.izvrši(mem, lokalni)

class Pridruživanje(AST('varijabla indeks izraz')):
    def izvrši(self, mem, lokalni):
        if self.indeks:
            if not self.izraz.provjeri_tip(mem, lokalni) == MJ.INT:
                raise Token(self.izraz.provjeri_tip(mem, lokalni), '').krivi_tip(self.izraz.provjeri_tip(mem, lokalni), MJ.INT)
            lokalni[self.varijabla.sadržaj][self.indeks.sadržaj] = self.izraz.vrijednost(mem, lokalni)
        else:
            if not pogledaj(lokalni[0], self.varijabla)[0] == self.izraz.provjeri_tip(mem, lokalni):
                raise Token(pogledaj(lokalni[0], self.varijabla)[0],
                      '').krivi_tip(pogledaj(lokalni[0], self.varijabla)[0], self.izraz.provjeri_tip(mem, lokalni))
            lokalni[self.varijabla.sadržaj] = self.izraz.vrijednost(mem, lokalni)
            if lokalni[self.varijabla.sadržaj] == {}:
                lokalni[0][self.varijabla.sadržaj][1] = self.izraz.veličina.sadržaj

class Konjunkcija(AST('konjunkti')):
    def vrijednost(self, mem, lokalni):
        return all(konjunkt.vrijednost(mem, lokalni) for konjunkt in self.konjunkti)

    def provjeri_tip(self, mem, lokalni):
        return MJ.BOOLEAN

class Usporedba(AST('uspoređeni')):
    def vrijednost(self, mem, lokalni):
        l, d, *rest = self.uspoređeni
        vr = l.vrijednost(mem, lokalni) < d.vrijednost(mem, lokalni)
        for uspoređen in rest :
            vr = vr < uspoređen.vrijednost(mem, lokalni)
        return vr

    def provjeri_tip(self, mem, lokalni):
        return MJ.BOOLEAN

class Zbroj(AST('članovi')):
    def vrijednost(self, mem, lokalni):
        return sum(član.vrijednost(mem, lokalni) for član in self.članovi)

    def provjeri_tip(self, mem, lokalni):
        return MJ.INT

class Umnožak(AST('faktori')):
    def vrijednost(self, mem, lokalni):
        f = 1
        for faktor in self.faktori: f *= faktor.vrijednost(mem, lokalni)
        return f

    def provjeri_tip(self, mem, lokalni):
        return MJ.INT

class Suprotan(AST('dolje')):
    def vrijednost(self, mem, lokalni):
        return -self.dolje.vrijednost(mem, lokalni)

    def provjeri_tip(self, mem, lokalni):
        return MJ.INT

class Negacija(AST('dolje')):
    def vrijednost(self, mem, lokalni):
        return not(self.dolje.vrijednost(mem, lokalni))

    def provjeri_tip(self, mem, lokalni):
        return MJ.BOOLEAN

class Blok(AST('naredbe')):
    def izvrši(self, mem, lokalni):
        for naredba in self.naredbe: naredba.izvrši(mem, lokalni)

### Apstraktna sintaksna stabla:
# Program(MainClass klase) (klase: lista ClassDeclaration)
# MainClass(ime arg naredbe)
# ClassDeclaration(ime extends vardeklaracije metdeklaracije)
# PrintStatement(izraz)
# MethodCallExpresion(objekt ime arg)
# ConstructorExpression(veličina ime)
# Indeksiranje(varijabla veličina)
# Length(varijabla)
# VarDeclaration(tip ime)
# MethodDeclaration(parent returntip ime parametri vardeklaracije naredbe returns)
# Parametar(tip ime)
# Ako(uvjet naredba inače)
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

program_komentari = '''
class Factorial{
    public static void main(String[] a){
        System.out.println(new Fac().ComputeFac(10));
    }
}

// sada slijedi klasa Fac
class Fac {
    public int ComputeFac(int num){ // metoda unutar klase Fac
        int num_aux ;
        if (num < 1)
            num_aux = 1 ; /* ovdje pridružujemo jedinicu num_aux
        završio je if dio
        // sada slijedi else
        u njemu ćemo množiti: num * nešto */
        else
            num_aux = num * (this.ComputeFac(num-1));
        return num_aux ;
    }
}
'''

program2 = '''
class Main{
    public static void main(String[] a){
        System.out.println(new Klasa().Metoda(5, 10));
    }
}
class Klasa{
    public int Metoda(int a, int b){
        while (a < 10 && b < 13) {
            if (a < 8)
                a = a + 2;
            else
                a = a + 1;
            b = b + 1;
            System.out.println(a);                          
        }
        return b;
    }
}
'''

program_nasljeđivanje = '''
class Main{
    public static void main(String[] a){
        System.out.println(new Dijete().DjetetovaMetoda(15));
    }
}

class Roditelj{
    int a;
    int b;

    public int RoditeljevaMetoda(int c){
        if (c < 10) {
            a = c;
            b = c;
        }
        else {
            a = c - 5;
            b = c - 10;
        }
        return (a + b);
    }
}

class Dijete (extends Roditelj){
    int d;

    public int DjetetovaMetoda(int d){
        int e;
        e = this.RoditeljevaMetoda(d + 3);
        return e;
    }
}
'''

program_nasljeđivanje_krivi_tipovi = '''
class Main{
    public static void main(String[] a){
        System.out.println(new Dijete().DjetetovaMetoda(15));
    }
}

class Roditelj{
    int a;
    int b;

    public int RoditeljevaMetoda(int c){
        if (c < 10) {
            a = c;
            b = c;
        }
        else {
            a = c - 5;
            b = c - 10;
        }
        return (a + b);
    }
}

class Dijete (extends Roditelj){
    int d;

    public int DjetetovaMetoda(int d){
        int e;
        int[] f;
        e = this.RoditeljevaMetoda(d + 3);
        f[0] = e;
        return f;
    }
}
'''

program4 = '''
class Main{
    public static void main(String[] a){
        System.out.println(new Klasa().Metoda(15));
    }
}

class Klasa{
    int[] a;
    public int Metoda(int x){
        a = new int[2];
        a[0] = 1;
        a[1] = x;
        System.out.println(a.length);
        return a[1];
    }
}
'''

program_nasljeđivanje2 = '''
class Main{
    public static void main(String[] a){
        System.out.println(new Dijete().DjetetovaMetoda(15));
        System.out.println(new Dijete().RoditeljevaMetoda(15));
        System.out.println(new Roditelj().RoditeljevaMetoda(15));
    }
}

class Roditelj{
    int a;
    int b;

    public int RoditeljevaMetoda(int c){
        if (c < 10) {
            a = c;
            b = c;
        }
        else {
            a = c - 5;
            b = c - 10;
        }
        return (a + b);
    }
}

class Dijete (extends Roditelj){
    int d;

    public int RoditeljevaMetoda(int c){
        a = c;
        b = c + 1;
        return (a + b);
    }

    public int DjetetovaMetoda(int d){
        int e;
        e = this.RoditeljevaMetoda(d + 3);
        return e;
    }
}
'''

tokeni = list(minijava_lexer(program_nasljeđivanje_krivi_tipovi))
#print(*tokeni)

ast = MiniJavaParser.parsiraj(tokeni)
#print(ast)

ast.izvrši()
