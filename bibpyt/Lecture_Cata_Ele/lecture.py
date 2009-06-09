#@ MODIF lecture Lecture_Cata_Ele  DATE 08/06/2009   AUTEUR DELMAS J.DELMAS 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE VABHHTS J.PELLET
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
# (AT YOUR OPTION) ANY LATER VERSION.
#
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.
#
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
# ======================================================================
import string,copy,traceback,re
from Lecture_Cata_Ele import spark
GenericScanner       =spark.GenericScanner
GenericASTBuilder    =spark.GenericASTBuilder
GenericASTTraversal  =spark.GenericASTTraversal
GenericASTMatcher    =spark.GenericASTMatcher

from Lecture_Cata_Ele import utilit
ut=utilit
ERR=ut.ERR


#######################################################################################################
# Fonction principale :
#######################################################################################################

def lire_cata(nomfic,format='cata'):
#==================================


#   lire un fichier .cata et construire  un catalogue  python (capy)
        global spark
        spark.FIC_CATA = nomfic
        fcata = open(nomfic,"r")
        if format == 'cata' :
            try :
                t0=scan(fcata)
                fcata.close()
                t1=parse(t0)
                ast2=creer_capy(t1)
                capy=ast2.ast
                del ast2
                detruire_kids(capy)
            except :
                fcata = open(nomfic,"r")
                print 80*'/'
                print "Erreur de lecture d'un morceau contenant des catalogues"
                print "Impression du morceau (avec ses numéros de lignes):"
                ilig=0
                for line in fcata :
                    ilig=ilig+1; print "%i %s" % (ilig,line)
                print 80*'/'
                raise "Erreur_Fatale"
        else :
            raise "Erreur_Fatale","format inconnu: "+format
        return capy


#######################################################################################################
# sous_fonctions :
#######################################################################################################


debug_scan=0
debug_parse=0
debug_constr=0

# --------------------------------------------------------------------------------
# définition des classes Token et AST :
# --------------------------------------

class Token:
        def __init__(self, type, lineno, attr=''):
                self.type = type
                self.attr = attr
                self.lineno = lineno
                if debug_scan : print self

        #  __cmp__      required for GenericParser, required for
        #                       GenericASTMatcher only if your ASTs are
        #                       heterogeneous (i.e., AST nodes and tokens)
        #  __repr__     recommended for nice error messages in GenericParser
        #  __getitem__  only if you have heterogeneous ASTs
        #
        def __cmp__(self, o):
                return cmp(self.type, o)
        def __repr__(self):
                return ' Token ligne:' + str(self.lineno)  \
                     + ' type: "' + self.type  + '" valeur: "' + self.attr +'"'
        #def __getitem__(self, i):
        #       raise IndexError

class AST:
        def __init__(self, type):
                self.type = type
                self._kids = []

        #
        #  Not all these may be needed, depending on which classes you use:
        #
        #  __getitem__          GenericASTTraversal, GenericASTMatcher
        #  __len__              GenericASTBuilder
        #  __setslice__         GenericASTBuilder
        #  __cmp__              GenericASTMatcher
        #
        def __getitem__(self, i):
                return self._kids[i]
        def __len__(self):
                try :
                    return len(self._kids)
                except : return 1
        def __repr__(self):
                try :
                   return ' type_AST: "' + self.type + '" ligne: ' + str(self.lineno)
                except :
                   return ' type_AST: effacé.'
        def __setslice__(self, low, high, seq):
                self._kids[low:high] = seq
        def __cmp__(self, o):
                return cmp(self.type, o)

# --------------------------------------------------------------------------------
#       SCANNING
#

class MonScanner(GenericScanner):
    def __init__(self):
        GenericScanner.__init__(self)
        self.lineno=1

    def tokenize(self, input):
        self.rv = []
        GenericScanner.tokenize(self, input)
        return self.rv

    def t_espace(self, s):
        r'[ \t\r]+'
        pass

    def t_commentaire(self, s):
        r'%.*'
        pass

    def t_chaine(self, s):
        r'\'[a-z0-9_ ]*\''
        t = Token(type="chaine", lineno=self.lineno, attr=string.upper(s))
        self.rv.append(t)

    def t_ident(self, s):
        r'[a-z][a-z0-9_]*(\[[0-9]+\])*'
        t = Token(type="ident", lineno=self.lineno, attr=string.upper(s))
        self.rv.append(t)

    def t_entier(self, s):
        r'(\+|\-)?\d+'
        t = Token(type='entier', lineno=self.lineno, attr=s)
        self.rv.append(t)

    def t_ponctuation(self, s):
        r'\(|\)|:|='
        t = Token(type=s, lineno=self.lineno)
        self.rv.append(t)

# une sous_classe de MonScanner pour "forcer" certains tokens :
class Mon2Scanner(MonScanner):
    def __init__(self):
        MonScanner.__init__(self)

    def t_nl(self, s):  # pour compter les lignes du fichier.
        r'\n'
        self.lineno = self.lineno +1

    def t_cmodif(self, s):
        r'%&[ \t]+(MODIF|AJOUT)[ \t].*'
        t = Token(type='cmodif', lineno=self.lineno, attr=s)
        self.rv.append(t)

#   comlibr= commentaire libre compris entre "<<" et ">>" :
    def t_comlibr(self, s):
        r'<<(.|\n)*?>>'
        t = Token(type='comlibr', lineno=self.lineno, attr=s)
        nlig=len(string.split(s,"\n"))
        self.lineno = self.lineno +nlig-1
        self.rv.append(t)

    def t_reserve(self, s):
        r'[a-z][a-z0-9_]*__'
        t = Token(type=string.upper(s), lineno=self.lineno)
        self.rv.append(t)

def scan(f):
    ERR.mess('I',"Début de l'analyse lexicale")
    input = f.read()
    scanner = Mon2Scanner()
    l= scanner.tokenize(input)
    ERR.mess('I',"Fin de l'analyse lexicale")
    return l


# --------------------------------------------------------------------------------
#       PARSING : construction de l'arbre syntaxique
# --------------------------------------------------------------------------------


class MonParser(GenericASTBuilder):
    def __init__(self, AST, start='catalo'):
                GenericASTBuilder.__init__(self, AST, start)


#   définition de la structure générale :
#   --------------------------------------
    def p_cata(self, args):
        '''
        catalo      ::=  l_cata
        l_cata      ::=  l_cata cata
        l_cata      ::=  cata
        cata        ::=  cata_tg
        cata        ::=  cata_te
        cata        ::=  cata_op
        cata        ::=  cata_tm
        cata        ::=  cata_gd
        cata        ::=  cata_ph
        cata_op     ::=  cmodif ident  OPTION__  IN__      l_opin1    OUT__      l_opou1
        cata_op     ::=  cmodif ident  comlibr   OPTION__  IN__       l_opin1    OUT__     l_opou1
        cata_tm     ::=  cmodif TYPE_MAILLE__ l_tyma
        cata_gd     ::=  cmodif GRANDEUR_SIMPLE__  l_gdsimp GRANDEUR_ELEMENTAIRE__  l_gdelem
        cata_tg     ::=  cmodif ident TYPE_GENE__  l_entete  modes_locaux options
        cata_te     ::=  cmodif ident TYPE_ELEM__  entete    modes_locaux options
        cata_ph     ::=  cmodif PHENOMENE_MODELISATION__ l_pheno
        '''


#   définition des utilitaires : listes, ... :
#   ----------------------------------------------
    def p_utili(self, args):
        '''
        l_ident     ::=  l_ident  ident
        l_ident     ::=  ident
        l_entier    ::=  l_entier  entier
        l_entier    ::=  entier
        '''

#   définition supplémentaires pour les cata_tm :
#   ----------------------------------------------
    def p_cata0(self, args):
        '''
        l_tyma      ::=  l_tyma tyma
        l_tyma      ::=  tyma
        fampg       ::=  FAMILLE__  ident  entier
        l_fampg     ::=  l_fampg fampg
        l_fampg     ::=  fampg
        elrefe      ::=  ELREFE__  ident  l_fampg
        l_elrefe    ::=  l_elrefe elrefe
        l_elrefe    ::=  elrefe
        tyma        ::=  MAILLE__ ident entier  DIM__ entier  CODE__ chaine  l_elrefe
        '''

#   définitions supplémentaires pour les cata_gd  :
#   ----------------------------------------------------------
    def p_cata1(self, args):
        '''
        l_gdsimp   ::= l_gdsimp  gdsimp
        l_gdsimp   ::= gdsimp

        gdsimp     ::= ident      =        ident     l_ident
        gdsimp     ::= ident      =        UNION__   l_ident
        gdsimp     ::= comlibr    ident    =         ident      l_ident
        gdsimp     ::= comlibr    ident    =         UNION__    l_ident
        l_gdelem   ::= l_gdelem  gdelem
        l_gdelem   ::= gdelem
        gdelem     ::= ident entier l_ident
        '''


#   définitions supplémentaires pour les cata_op  :
#   ----------------------------------------------------------
    def p_cata2(self, args):
        '''
        l_opin1    ::= l_opin1  opin1
        l_opin1    ::= opin1
        l_opou1    ::= l_opou1  opou1
        l_opou1    ::= opou1

        opin1      ::= ident   ident
        opin1      ::= ident   ident   comlibr
        opou1      ::= ident   ident   typ_out
        opou1      ::= ident   ident   typ_out  comlibr
        typ_out    ::= ELEM__
        typ_out    ::= ELGA__
        typ_out    ::= ELNO__
        typ_out    ::= RESL__
        '''


#   définitions supplémentaires pour les cata_te et cata_tg :
#   ---------------------------------------------------------
    def p_cata3(self, args):
        '''
        l_entete    ::=  l_entete  entete
        l_entete    ::=  entete

        entete      ::=  entet1    l_decl_en   l_decl_opt
        entete      ::=  entet1                l_decl_opt
        entete      ::=  entet1    l_decl_en
        entete      ::=  entet1

        entet1      ::=  ENTETE__  ELEMENT__ ident MAILLE__ ident  l_elref1
        entet1      ::=  ENTETE__  ELEMENT__ ident MAILLE__ ident  l_elref1  ATTRIBUT__ l_attr_val

        l_elref1  ::=  l_elref1  elref1
        l_elref1  ::=  elref1
        elref1    ::=  ELREFE__ ident
        elref1    ::=  ELREFE__ ident gauss1

        gauss1    ::=  GAUSS__  l_attr_val
        gauss1    ::=  GAUSS__  l_attr_val  l_fpg_liste
        l_attr_val  ::=  l_attr_val  attr_val
        l_attr_val  ::=  attr_val
        attr_val    ::=  ident =  ident

        l_fpg_liste  ::=  l_fpg_liste  fpg_liste
        l_fpg_liste  ::=  fpg_liste
        fpg_liste    ::=  FPG_LISTE__   ident = ( l_ident )

        l_decl_opt  ::=  l_decl_opt  decl_opt
        l_decl_opt  ::=  decl_opt
        decl_opt    ::=  OPTION__  ident   entier


        l_decl_en   ::=  l_decl_en   decl_en
        l_decl_en   ::=  decl_en
        decl_en     ::=  ENS_NOEUD__  ident = l_entier

        modes_locaux ::=  MLOC   MLVE   MLMA
        options     ::=  OPTION__
        options     ::=  OPTION__   l_opt
        MLOC     ::= MODE_LOCAL__   l_moloc
        MLVE     ::= VECTEUR__      l_molove
        MLVE     ::= VECTEUR__
        MLMA     ::= MATRICE__      l_moloma
        MLMA     ::= MATRICE__
        l_moloc  ::= l_moloc    moloc
        l_moloc  ::= moloc
        moloc    ::= molocc
        moloc    ::= molocn
        moloc    ::= moloce
        molocc   ::= ident =  ident  ELEM__  point
        molocn   ::= ident =  ident  ELNO__  IDEN__   point
        molocn   ::= ident =  ident  ELNO__  DIFF__   l_point
        moloce   ::= ident =  ident  ELGA__  ident    point
        l_point  ::= l_point ident point
        l_point  ::= ident point
        point    ::= ( l_ident )
        point    ::= ( )
        lcmp     ::= l_ident

        l_molove ::= l_molove molove
        l_molove ::= molove
        molove   ::= ident = ident ident
        l_moloma ::= l_moloma moloma
        l_moloma ::= moloma
        moloma   ::= ident = ident ident ident

        l_opt    ::= l_opt     opt
        l_opt    ::= opt
        opt      ::= ident entier IN__  l_ident  OUT__     l_ident
        opt      ::= ident entier IN__  OUT__    l_ident
        opt      ::= ident entier IN__  OUT__
        '''

#   définitions supplémentaires pour le cata_ph  :
#   ----------------------------------------------------------
    def p_cata4(self, args):
        '''
        l_pheno     ::=  l_pheno pheno
        l_pheno     ::=  pheno
        pheno       ::=  PHENOMENE__ ident CODE__ chaine  l_modeli
        l_modeli    ::=  l_modeli modeli
        l_modeli    ::=  modeli
        modeli      ::=  MODELISATION__ chaine DIM__ entier entier CODE__ chaine l_affe_te
        modeli      ::=  MODELISATION__ chaine DIM__ entier entier CODE__ chaine  ATTRIBUT__ l_attr_val l_affe_te
        l_affe_te   ::=  l_affe_te  affe_te
        l_affe_te   ::=  affe_te
        affe_te     ::=  MAILLE__ ident ELEMENT__ ident
        '''

    def terminal(self, token):
        #
        #  Homogeneous AST.
        #
        if debug_parse : print 'AJACO terminal :',token.type,' ',token.attr
        rv = AST(token.type)
        rv.attr = token.attr
        rv.lineno = token.lineno
        return rv

    def nonterminal(self, type, args):
        #
        #  Flatten AST a bit by not making nodes if there's only
        #  one child.   NON !!!!!
        #
        if debug_parse : print 'AJACO non-terminal :',type,' ',args
        #  if len(args) == 1:
        #      return args[0]
        #  else :
        nt = GenericASTBuilder.nonterminal(self, type, args)
        nt.lineno=args[0].lineno
        return nt

def parse(tokens):
    ERR.mess('I',"Début de l'analyse syntaxique")
    parser = MonParser(AST)
    t= parser.parse(tokens)
    ERR.mess('I',"Fin de l'analyse syntaxique")
    return t


# --------------------------------------------------------------------------------
#       construction du catalogue  capy
# --------------------------------------------------------------------------------

class creer_capy(GenericASTTraversal):

    def __init__(self, ast):
        GenericASTTraversal.__init__(self, ast)
        self.ast.gd=None
        self.ast.tm=None
        self.ast.mp=None
        self.ast.ph=None
        ERR.mess('I',"Début de la construction du catalogue 'Python'")
        self.postorder()
        ERR.mess('I',"Fin de la construction du catalogue 'Python'")


#   pour construire le noeud "sommet" (catalo) :
#   ---------------------------------------------------------
#       catalo      ::=  l_cata
#       l_cata      ::=  l_cata cata
#       l_cata      ::=  cata
#       cata        ::=  cata_tg
#       cata        ::=  cata_te
#       cata        ::=  cata_op
#       cata        ::=  cata_tm
#       cata        ::=  cata_gd
#       cata        ::=  cata_ph
    def n_catalo(self, node):
        node.dicop={}; node.dicte={}; node.dictg={}
        node.op=[];node.te=[];node.tg=[];nb_op=0;nb_te=0;nb_tg=0
        for cata in node[0].l_cata:
            type=cata.type
            del cata._kids

            if type=='cata_gd':
                node.gd=cata

            if type=='cata_tm':
                node.tm=cata

            if type=='cata_ph':
                node.ph=cata

            if type=='cata_op':
                nom=cata.cata_op[0]
                iex=ERR.veri_new_key("E",nom,node.dicop)
                if iex == 0 :
                    node.dicop[nom]=nb_op
                    node.op.append(cata)
                    nb_op=nb_op+1

            if type=='cata_te':
                nom=cata.cata_te[0][0]
                iex=ERR.veri_new_key("E",nom,node.dicte)
                if iex == 0 :
                    node.dicte[nom]=nb_te
                    node.te.append(cata)
                    nb_te=nb_te+1

            if type=='cata_tg':
                nom=cata.cata_tg[0]
                iex=ERR.veri_new_key("E",nom,node.dictg)
                if iex == 0 :
                    node.dictg[nom]=nb_tg
                    node.tg.append(cata)
                    nb_tg=nb_tg+1


        # on met les options ,les type_elem et les type_gene dans l'ordre alphabétique:
        # -----------------------------------------------------------------------------
        likeys= node.dicop.keys(); likeys.sort(); liste2=[]; dico2={};k=0
        for ke in likeys:
           liste2.append(node.op[node.dicop[ke]])
           dico2[ke]=k; k=k+1
        node.op=liste2 ; node.dicop=dico2

        likeys= node.dicte.keys(); likeys.sort(); liste2=[]; dico2={};k=0
        for ke in likeys:
           liste2.append(node.te[node.dicte[ke]])
           dico2[ke]=k; k=k+1
        node.te=liste2 ; node.dicte=dico2

        likeys= node.dictg.keys(); likeys.sort(); liste2=[]; dico2={};k=0
        for ke in likeys:
           liste2.append(node.tg[node.dictg[ke]])
           dico2[ke]=k; k=k+1
        node.tg=liste2 ; node.dictg=dico2



    def n_l_cata(self, node):
        node.l_cata=[]
        if len(node) == 2 :
           node.l_cata.extend(node[0].l_cata)
           node.l_cata.append(node[1].cata)
        if len(node) == 1 : node.l_cata.append(node[0].cata)

    def n_cata(self, node):
        node.cata=node[0]


#   Utilitaires pour construire les listes :
#   ---------------------------------------------------------
    def n_l_ident(self, node):
        #  l_ident   ::= l_ident ident
        #  l_ident   ::= ident
        node.l_ident=[]
        if len(node) == 2 :
           node.l_ident.extend(node[0].l_ident)
           match=re.search('([a-z][a-z0-9_]*)\[([0-9]*)\]',node[1].attr,re.IGNORECASE)
           if match is not None :
             prefix=match.group(1)
             lonlist=match.group(2)
             ident_list=[]
             for i in range(1,int(lonlist)+1) :
               ident_list.append(prefix+str(i))
             node.l_ident.extend(ident_list)
           else :
             node.l_ident.append(node[1].attr)
        else :
           match=re.search('([a-z][a-z0-9_]*)\[([0-9]*)\]',node[0].attr,re.IGNORECASE)
           if match is not None :
             prefix=match.group(1)
             lonlist=match.group(2)
             ident_list=[]
             for i in range(1,int(lonlist)+1) :
               ident_list.append(prefix+str(i))
             node.l_ident.extend(ident_list)
           else :
             node.l_ident.append(node[0].attr)
        del node._kids

    def n_l_entier(self, node):
        #  l_entier   ::= l_entier entier
        #  l_entier   ::= entier
        node.l_entier=[]
        if len(node) == 2 :
           node.l_entier.extend(node[0].l_entier)
           node.l_entier.append(int(node[1].attr))
        else :
           node.l_entier.append(int(node[0].attr))
        del node._kids


#   pour construire le catalogue de TYPE_MAILLE__ :
#   ---------------------------------------------------------
    def n_l_tyma(self, node):
        #    l_tyma      ::=  l_tyma tyma
        #    l_tyma      ::=  tyma
        node.l_tyma=[]
        if len(node) == 2 :
           node.l_tyma.extend(node[0].l_tyma)
           node.l_tyma.append(node[1].tyma)
        else :
           node.l_tyma.append(node[0].tyma)
        del node._kids

    def n_tyma(self, node):
        #                     0       1     2      3     4       5       6      7
        #  tyma      ::=  MAILLE__  ident entier  DIM__ entier  CODE__ chaine  l_elrefe
           node.tyma=(node[1].attr,node[2].attr,node[4].attr,node[6].attr,node[7].l_elrefe)
           if len(node[6].attr) != 5 :
              ERR.mess('E',"le code d'un type de maille doit avoir 3 caractères exactement."+node.code)


    def n_fampg(self, node):
        #  fampg       ::=  FAMILLE__  ident  entier
           node.fampg=(node[1].attr,node[2].attr,)

    def n_l_fampg(self, node):
        #    l_fampg      ::=  l_fampg fampg
        #    l_fampg      ::=  fampg
        node.l_fampg=[]
        if len(node) == 2 :
           node.l_fampg.extend(node[0].l_fampg)
           node.l_fampg.append(node[1].fampg)
        else :
           node.l_fampg.append(node[0].fampg)
        del node._kids

    def n_elrefe(self, node):
        #  elrefe      ::=  ELREFE__  ident  l_fampg
           node.elrefe=(node[1].attr,node[2].l_fampg)

    def n_l_elrefe(self, node):
        #    l_elrefe      ::=  l_elrefe elrefe
        #    l_elrefe      ::=  elrefe
        node.l_elrefe=[]
        if len(node) == 2 :
           node.l_elrefe.extend(node[0].l_elrefe)
           node.l_elrefe.append(node[1].elrefe)
        else :
           node.l_elrefe.append(node[0].elrefe)
        del node._kids

    def n_cata_tm(self, node):
        #   cata_tm     ::=  cmodif  TYPE_MAILLE__ l_tyma
        ERR.contexte("Définition des types de maille.")
        node.cmodif=node[0].attr
        node.ltm=node[2].l_tyma

        # vérification de l'unicité des noms des types de maille et de leurs codes :
        dico={} ; dico_code={}
        for k in  range(len(node.ltm)) :
           if dico.has_key(node.ltm[k][0]) : ERR.mess('E',"erreur : le type de maille: "+node.ltm[k][0]+" est déjà défini.")
           if dico_code.has_key(node.ltm[k][3]) : ERR.mess('E',"erreur : le type de maille: "+node.ltm[k][0]+" a un CODE__ déjà utilisé:"+node.ltm[k][3])
           dico[node.ltm[k][0]]=1
           dico_code[node.ltm[k][3]]=1

        # vérification de l'unicité des noms des ELREFE :
        dico={}
        for tm in node.ltm :
           nom_tm=tm[0]
           for elrefe in tm[4] :
              nom_elrefe=elrefe[0]
              if not dico.has_key(nom_elrefe) :
                 dico[nom_elrefe]=nom_tm
              else :
                 if dico[nom_elrefe]!=nom_tm :
                    ERR.mess('E',"erreur : le nom d'ELREFE__ "+nom_elrefe+\
                    " est défini dans plusieurs MAILLE__ : "+nom_tm+" "+dico[nom_elrefe])
        ERR.contexte("","RAZ")


#   pour construire le catalogue de PHENOMENE_MODELISATION__ :
#   ---------------------------------------------------------
    def n_affe_te(self, node):
#       affe_te     ::=  MAILLE__ ident ELEMENT__ ident
        node.affe_te=(node[1].attr,node[3].attr)

    def n_l_affe_te(self, node):
#       l_affe_te   ::=  l_affe_te  affe_te
#       l_affe_te   ::=  affe_te
        node.l_affe_te=[]
        if len(node) == 1 : node.l_affe_te.append(node[0].affe_te)
        if len(node) == 2 :
           node.l_affe_te.extend(node[0].l_affe_te)
           node.l_affe_te.append(node[1].affe_te)

    def n_modeli(self, node):
#                             0           1     2      3      4     5       6       7           8          9
#       modeli      ::=  MODELISATION__ chaine DIM__ entier entier CODE__ chaine l_affe_te
#       modeli      ::=  MODELISATION__ chaine DIM__ entier entier CODE__ chaine  ATTRIBUT__ l_attr_val l_affe_te
        ERR.contexte('Définition de la modélisation: '+node[1].attr)
        if len(node)== 8 :
           node.modeli=(node[1].attr,node[7].l_affe_te,node[6].attr,(node[3].attr,node[4].attr),None)
        else :
           node.modeli=(node[1].attr,node[9].l_affe_te,node[6].attr,(node[3].attr,node[4].attr),node[8].l_attr_val)

        if len(node[6].attr) != 5 :
           ERR.mess('E',"le code d'une modélisation doit avoir 3 caractères exactement: "+node[6].attr)
        if not node[3].attr in ("0","1","2","3")  :
           ERR.mess('E',"la 1eme dimension doit etre celle des élements 'principaux' (0,1,2, ou 3) : "+node[3].attr)
        if not node[4].attr in ("2","3")  :
           ERR.mess('E',"la 2eme dimension doit etre celle de l'espace (2 ou 3) : "+node[4].attr)
        ERR.contexte("","RAZ")

    def n_l_modeli(self, node):
#       l_modeli   ::=  l_modeli  modeli
#       l_modeli   ::=  modeli
        node.l_modeli=[]
        if len(node) == 1 : node.l_modeli.append(node[0].modeli)
        if len(node) == 2 :
           node.l_modeli.extend(node[0].l_modeli)
           node.l_modeli.append(node[1].modeli)

    def n_pheno(self, node):
#                             0      1       2      3        4
#       pheno       ::=  PHENOMENE__ ident CODE__ chaine  l_modeli
        ERR.contexte('Définition du phénomène: '+node[1].attr)
        node.pheno=(node[1].attr,node[4].l_modeli,node[3].attr)
        if len(node[3].attr) != 4 :
           ERR.mess('E',"le code d'un phénomène doit avoir 2 exactement: "+node[3].attr)

        # vérification de l'unicité des noms des modélisations (et de leurs codes) d'un phénomène:
        dico={}; dico_code={}
        for modeli in node[4].l_modeli :
           if dico.has_key(modeli[0]): ERR.mess('E',"La modélisation: "+modeli[0]+" est définie plusieurs fois.")
           if dico_code.has_key(modeli[2]): ERR.mess('E',"La modélisation: "+modeli[0]+" a un CODE__ déjà utilisé:"+modeli[2])
           dico[modeli[0]]=1
           dico_code[modeli[2]]=1
        ERR.contexte("","RAZ")



    def n_l_pheno(self, node):
#      l_pheno     ::=  l_pheno pheno
#      l_pheno     ::=  pheno
        node.l_pheno=[]
        if len(node) == 1 : node.l_pheno.append(node[0].pheno)
        if len(node) == 2 :
           node.l_pheno.extend(node[0].l_pheno)
           node.l_pheno.append(node[1].pheno)

    def n_cata_ph(self, node):
#                           0              1                2
#       cata_ph      ::=  cmodif PHENOMENE_MODELISATION__ l_pheno
        node.cmodif=node[0].attr
        node.l_pheno=node[2].l_pheno




#   pour construire les catalogues d'OPTION__ :
#   ---------------------------------------------------------

    def n_typ_out(self, node):
#       typ_out    ::= ELEM__
#       typ_out    ::= ELGA__
#       typ_out    ::= ELNO__
#       typ_out    ::= RESL__
        node.typ_out=node[0].type

    def n_opin1(self, node):
#                          0      1      2
#         opin1      ::= ident   ident
#         opin1      ::= ident   ident   comlibr
        if len(node)   == 2 :  node.opin1=(node[0].attr,node[1].attr,None)
        elif len(node) == 3 :  node.opin1=(node[0].attr,node[1].attr,node[2].attr)

    def n_opou1(self, node):
#                          0      1      2        3
#         opou1      ::= ident   ident   typ_out
#         opou1      ::= ident   ident   typ_out  comlibr
        if len(node)   == 3 :  node.opou1=(node[0].attr,node[1].attr,node[2].typ_out,None)
        elif len(node) == 4 :  node.opou1=(node[0].attr,node[1].attr,node[2].typ_out,node[3].attr)

    def n_l_opin1(self, node):
#          l_opin1   ::= l_opin1 opin1
#          l_opin1   ::= opin1
        node.l_opin1=[]
        if len(node) == 2 :
           node.l_opin1.extend(node[0].l_opin1)
           node.l_opin1.append(node[1].opin1)
        else :
           node.l_opin1.append(node[0].opin1)
        del node._kids

    def n_l_opou1(self, node):
#          l_opou1   ::= l_opou1 opou1
#          l_opou1   ::= opou1
        node.l_opou1=[]
        if len(node) == 2 :
           node.l_opou1.extend(node[0].l_opou1)
           node.l_opou1.append(node[1].opou1)
        else :
           node.l_opou1.append(node[0].opou1)
        del node._kids

    def n_cata_op(self, node):
#                        0      1      2         3         4          5          6         7
#       cata_op     ::=  cmodif ident  OPTION__  IN__      l_opin1    OUT__      l_opou1
#       cata_op     ::=  cmodif ident  comlibr   OPTION__  IN__       l_opin1    OUT__     l_opou1
        node.cmodif=node[0].attr
        if len(node) == 7 :
           node.cata_op=(node[1].attr,node[4].l_opin1,node[6].l_opou1,None)
        elif len(node) == 8 :
           node.cata_op=(node[1].attr,node[5].l_opin1,node[7].l_opou1,node[2].attr)


#   pour construire le catalogue des GRANDEUR :
#   ---------------------------------------------------------
    def n_gdsimp(self, node):
#                       0         1        2         3          4
#       gdsimp     ::= ident      =        ident     l_ident
#       gdsimp     ::= ident      =        UNION__   l_ident
#       gdsimp     ::= comlibr    ident    =         ident      l_ident
#       gdsimp     ::= comlibr    ident    =         UNION__    l_ident

        if len(node) == 5 :
           node.comlibr=node[0].attr; decal=1
        else :
           node.comlibr=None ; decal=0
        node.nom=node[0+decal].attr
        if node[2+decal].type == "UNION__" :
           node.union="OUI"
           node.lgd=node[3+decal].l_ident
        else :
           node.union=None
           node.tscal=node[2+decal].attr
           node.lcmp=node[3+decal].l_ident

    def n_l_gdsimp(self, node):
#       l_gdsimp   ::= l_gdsimp  gdsimp
#       l_gdsimp   ::= gdsimp
        node.l_gd=[]
        if len(node) == 1 : node.l_gd.append(node[0])
        if len(node) == 2 :
           node.l_gd.extend(node[0].l_gd)
           node.l_gd.append(node[1])

    def n_gdelem(self, node):
#       gdelem   ::= ident entier l_ident
        node.nom=node[0].attr
        node.gdelem=node[2].l_ident

    def n_l_gdelem(self, node):
#       l_gdelem   ::= l_gdelem  gdelem
#       l_gdelem   ::= gdelem
        node.l_gd=[]
        if len(node) == 1 : node.l_gd.append(node[0])
        if len(node) == 2 :
           node.l_gd.extend(node[0].l_gd)
           node.l_gd.append(node[1])


    def n_cata_gd(self, node):
#                           0        1              2              3                4
#       cata_gd     ::=  cmodif GRANDEUR_SIMPLE__  l_gdsimp GRANDEUR_ELEMENTAIRE__  l_gdelem
        node.cmodif=node[0].attr
        node[2].l_gd.sort(ut.cmp_gd);    node.l_gdsimp=node[2].l_gd
        node[4].l_gd.sort(ut.cmp_gd);    node.l_gdelem=node[4].l_gd





#   pour construire les modes_locaux :
#   ----------------------------------
    def n_modes_locaux(self, node):
        #                     0      1      2
        #  modes_locaux ::=  MLOC   MLVE   MLMA
        node[0].MLOC.sort(ut.cmp_tuple_1);node[1].MLVE.sort(ut.cmp_tuple_1);node[2].MLMA.sort(ut.cmp_tuple_1)
        node.modes_locaux=(node[0].MLOC,node[1].MLVE,node[2].MLMA)

    def n_MLOC(self, node):
        #  MLOC     ::= MODE_LOCAL__   l_moloc
        node.MLOC=node[1].l_moloc

    def n_MLVE(self, node):
        #  MLVE     ::= VECTEUR__      l_molove
        #  MLVE     ::= VECTEUR__
        if len(node)==2 : node.MLVE=node[1].l_molove
        if len(node)==1 : node.MLVE=[]

    def n_MLMA(self, node):
        #  MLMA     ::= MATRICE__      l_moloma
        #  MLMA     ::= MATRICE__
        if len(node)==2 : node.MLMA=node[1].l_moloma
        if len(node)==1 : node.MLMA=[]

    def n_l_molove(self, node):
        #  l_molove  ::= l_molove    molove
        #  l_molove  ::= molove
        node.l_molove=[]
        if len(node)==1 : node.l_molove.append(node[0].molove)
        if len(node)==2 :
            node.l_molove.extend(node[0].l_molove)
            node.l_molove.append(node[1].molove)

    def n_molove(self, node):
        #  molove   ::= ident = ident ident
        node.molove =(node[0].attr,node[2].attr,node[3].attr)

    def n_l_moloma(self, node):
        #  l_moloma  ::= l_moloma    moloma
        #  l_moloma  ::= moloma
        node.l_moloma=[]
        if len(node)==1 : node.l_moloma.append(node[0].moloma)
        if len(node)==2 :
            node.l_moloma.extend(node[0].l_moloma)
            node.l_moloma.append(node[1].moloma)

    def n_moloma(self, node):
        #  moloma   ::= ident = ident ident ident
        node.moloma =(node[0].attr,node[2].attr,node[3].attr,node[4].attr)

    def n_l_moloc(self, node):
        #  l_moloc  ::= l_moloc    moloc
        #  l_moloc  ::= moloc
        node.l_moloc=[]
        if len(node)==1 : node.l_moloc.append(node[0].moloc)
        if len(node)==2 :
            node.l_moloc.extend(node[0].l_moloc)
            node.l_moloc.append(node[1].moloc)


    def n_moloc(self, node):
        node.moloc =node[0].moloc

    def n_molocc(self, node):
        #                 0   1   2       3      4
        #  molocc   ::= ident =  ident  ELEM__  point
        node.moloc =(node[0].attr,node[2].attr,node[3].type,"0","IDEN",node[4].point)

    def n_molocn(self, node):
        #                 0   1   2       3         4        5
        #  molocn   ::= ident =  ident  ELNO__    IDEN__   point
        #  molocn   ::= ident =  ident  ELNO__    DIFF__   l_point
        if node[4].type[0:4]=="IDEN" :
            node.moloc =(node[0].attr,node[2].attr,node[3].type,"0",node[4].type[0:4],node[5].point)
        else:
            node.moloc =(node[0].attr,node[2].attr,node[3].type,"0",node[4].type[0:4],node[5].l_point)

    def n_moloce(self, node):
        #                 0   1   2       3         4       5
        #  moloce   ::= ident =  ident  ELGA__   ident     point
        node.moloc =(node[0].attr,node[2].attr,node[3].type,node[4].attr,"IDEN",node[5].point)

    def n_point(self,node):
        # point    ::= ( l_ident  )
        # point    ::= ( )
        if len(node) == 3 :
           node.point=node[1].l_ident
        else :
           node.point=[]

    def n_l_point(self, node):
        #  l_point  ::= ident  point
        #  l_point  ::= l_point ident point
        node.l_point=[]
        if len(node) == 2 : node.l_point.append((node[0].attr,node[1].point))
        if len(node) == 3 :
            node.l_point.extend(node[0].l_point)
            node.l_point.append((node[1].attr,node[2].point))



#   pour construire les entetes de TYPE_ELEM__  et TYPE_GENE__ :
#   -----------------------------------------------------------
    def n_decl_opt(self, node):
#       decl_opt     ::=  OPTION__  ident   entier
        node.decl_opt=(node[1].attr,node[2].attr)

    def n_l_decl_opt(self, node):
#       l_decl_opt  ::=  l_decl_opt  decl_opt
#       l_decl_opt  ::=  decl_opt
        node.l_decl_opt=[]
        if len(node)==1 : node.l_decl_opt.append(node[0].decl_opt)
        if len(node)==2 :
            node.l_decl_opt.extend(node[0].l_decl_opt)
            node.l_decl_opt.append(node[1].decl_opt)

    def n_attr_val(self, node):
#       attr_val    ::=  ident =  ident
        node.attr_val=(node[0].attr,node[2].attr)

    def n_l_attr_val(self, node):
#       l_attr_val  ::=  l_attr_val  attr_val
#       l_attr_val  ::=  attr_val
        node.l_attr_val=[]
        if len(node)==1 : node.l_attr_val.append(node[0].attr_val)
        if len(node)==2 :
            node.l_attr_val.extend(node[0].l_attr_val)
            node.l_attr_val.append(node[1].attr_val)

    def n_gauss1(self, node):
#       gauss1    ::=  GAUSS__  l_attr_val
#       gauss1    ::=  GAUSS__  l_attr_val  fpg_liste
        if len(node)==2 :
           node.gauss1=(node[1].l_attr_val,None)
        if len(node)==3 :
           node.gauss1=(node[1].l_attr_val,node[2].l_fpg_liste)

    def n_elref1(self, node):
#       elref1    ::=  ELREFE__ ident
#       elref1    ::=  ELREFE__ ident gauss1
        if len(node)==2 : node.elref1=(node[1].attr,None)
        if len(node)==3 : node.elref1=(node[1].attr,node[2].gauss1)

    def n_l_elref1(self, node):
#       l_elref1  ::=  l_elref1  elref1
#       l_elref1  ::=  elref1
        node.l_elref1=[]
        if len(node)==1 : node.l_elref1.append(node[0].elref1)
        if len(node)==2 :
            node.l_elref1.extend(node[0].l_elref1)
            node.l_elref1.append(node[1].elref1)

    def n_fpg_liste(self, node):
#       fpg_liste    ::=  FPG_LISTE__   ident = ( l_ident )
        node.fpg_liste=(node[1].attr,node[4].l_ident)


    def n_l_fpg_liste(self, node):
#       l_fpg_liste  ::=  l_fpg_liste  fpg_liste
#       l_fpg_liste  ::=  fpg_liste
        node.l_fpg_liste=[]
        if len(node)==1 : node.l_fpg_liste.append(node[0].fpg_liste)
        if len(node)==2 :
            node.l_fpg_liste.extend(node[0].l_fpg_liste)
            node.l_fpg_liste.append(node[1].fpg_liste)

    def n_decl_en(self, node):
#       decl_en     ::=  ENS_NOEUD__  ident = l_entier
        node.decl_en=(node[1].attr,node[3].l_entier)

    def n_l_decl_en(self, node):
#       l_decl_en  ::=  l_decl_en  decl_en
#       l_decl_en  ::=  decl_en
        node.l_decl_en=[]
        if len(node)==1 : node.l_decl_en.append(node[0].decl_en)
        if len(node)==2 :
            node.l_decl_en.extend(node[0].l_decl_en)
            node.l_decl_en.append(node[1].decl_en)

    def n_entet1(self, node):
#                          0         1         2      3       4       5         6          7
#       entet1      ::=  ENTETE__  ELEMENT__ ident MAILLE__ ident  l_elref1
#       entet1      ::=  ENTETE__  ELEMENT__ ident MAILLE__ ident  l_elref1  ATTRIBUT__ l_attr_val
        if len(node)==6 :
           node.entet1=(node[2].attr,node[4].attr,node[5].l_elref1,None)
        else :
           node.entet1=(node[2].attr,node[4].attr,node[5].l_elref1,node[7].l_attr_val)


    def n_entete(self, node):
#                          0         1            2
#       entete      ::=  entet1    l_decl_en   l_decl_opt
#       entete      ::=  entet1                l_decl_opt
#       entete      ::=  entet1    l_decl_en
#       entete      ::=  entet1
        if len(node)==3:
            node.entete=node[0].entet1 +(node[1].l_decl_en,node[2].l_decl_opt)
        elif len(node)==1:
            node.entete=node[0].entet1 +(None,None)
        if len(node)==2:
            if node[1].type=="l_decl_en":
                node.entete=node[0].entet1 +(node[1].l_decl_en,None)
            elif node[1].type=="l_decl_opt":
                node.entete=node[0].entet1 +(None,node[1].l_decl_opt)

    def n_l_entete(self, node):
#       l_entete  ::=  l_entete  entete
#       l_entete  ::=  entete
        node.l_entete=[]
        if len(node)==1 : node.l_entete.append(node[0].entete)
        if len(node)==2 :
            node.l_entete.extend(node[0].l_entete)
            node.l_entete.append(node[1].entete)



#   pour construire les catalogues de TYPE_ELEM__  et TYPE_GENE__ :
#   -----------------------------------------------------------

    def n_cata_tg(self, node):
#                          0      1      2            3            4         5
#       cata_tg     ::=  cmodif ident  TYPE_GENE__  l_entete  modes_locaux options
#       cata_tg     ::=  cmodif ident  TYPE_GENE__  l_entete  modes_locaux options
        node.cmodif=node[0].attr
        if len(node)== 6 : node.cata_tg=(node[1].attr,node[3].l_entete,node[4].modes_locaux,node[5].options)

    def n_cata_te(self, node):
#                          0      1      2           3           4         5
#       cata_te     ::=  cmodif ident TYPE_ELEM__  entete   modes_locaux options
#       cata_te     ::=  cmodif ident TYPE_ELEM__  entete   modes_locaux options
        node.cmodif=node[0].attr
        if node[1].attr!= node[3].entete[0] : ERR.mess("E","le nom du TYPE_ELEM n'est pas cohérent: "+node[1].attr+" != "+node[3].entete[0])
        if len(node)== 6 : node.cata_te=(node[3].entete,node[4].modes_locaux,node[5].options)

    def n_options(self, node):
#       options     ::=  OPTION__   l_opt
#       options     ::=  OPTION__
        if len(node)== 2 : node.options=node[1].l_opt
        if len(node)== 1 : node.options=None

    def n_l_opt(self, node):
        #  l_opt  ::= l_opt    opt
        #  l_opt  ::= opt
        node.l_opt=[]
        if len(node)==1 : node.l_opt.append(node[0].opt)
        if len(node)==2 :
            node.l_opt.extend(node[0].l_opt)
            node.l_opt.append(node[1].opt)

    def n_opt(self, node):
#                      0     1     2      3       4          5
#       opt      ::= ident entier IN__  l_ident  OUT__     l_ident
#       opt      ::= ident entier IN__  OUT__    l_ident
#       opt      ::= ident entier IN__  OUT__
        ERR.contexte("Définition de l'option:"+node[0].attr + " vers la ligne: "+str(node[0].lineno))
        if len(node)==6 :
           node.opt =(node[0].attr,node[1].attr,trie_en_2(node[3].l_ident),trie_en_2(node[5].l_ident))
        if len(node)==5 :
           node.opt =(node[0].attr,node[1].attr,[],trie_en_2(node[4].l_ident))
        if len(node)==4 :
           node.opt =(node[0].attr,node[1].attr,[],[])
        ERR.contexte("","RAZ")



#   utilitaires ...
# -------------------------------------------------------------------

def detruire_kids(ast):
    # but : détruire dans l'arbre produit tout l'arbre syntaxique (.kids, .lineno, .type).
    for k in ast.__dict__.keys():
        if k=='_kids' :
           del ast._kids
        elif k=='type' :
           del ast.type
        elif k=='lineno' :
           del ast.lineno
        else :
           try :
              detruire_kids(ast.__dict__[k])
           except: pass

           try :
              for elt in ast.__dict__[k]:
                 detruire_kids(elt)
           except: pass


def trie_en_2(liste):
    # ordonne la liste de paires (a1,b1,a2,b2,...) en classant par ordre alphabétique des bi :
    if len(liste)%2 != 0 : ERR.mess('E',"Erreur la liste d'identificateurs doit etre une liste de paires:\n\t "+str(liste))
    l1=[];l2=[];lr=[]
    for k in range(len(liste)/2) :
       l1.append(liste[2*(k-1)])
       l2.append(liste[2*(k-1)+1])
    l2_apres=copy.deepcopy(l2)
    l2_apres.sort()
    for x2 in l2_apres :
       k= l2.index(x2)
       lr.append(l1[k])
       lr.append(l2[k])
    return lr



