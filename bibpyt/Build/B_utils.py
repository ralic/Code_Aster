#@ MODIF B_utils Build  DATE 28/01/2013   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE COURTOIS M.COURTOIS
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#
#
# ======================================================================


"""
"""
# Modules Python
import sys

# Modules Eficas
from Noyau.N_ASSD import ASSD
from Noyau.N_types import is_sequence

def RETLIST(v, mxval):
    """Cette fonction retourne un tuple dont le premier element est sa longueur reelle
       eventuellement negative si supérieure a mxval
       et le deuxieme la liste complete qui sera tronquee a mxval dans le fortran
    """
    if type(v) is tuple and len(v) == 3 and v[0] in ("RI","MP"):
       # On a affaire a un complexe isole
       length = 1
       val = (v, )
    elif is_sequence(v):
       # Il s'agit d'un tuple de longueur quelconque
       length = len(v)
       val = v
    elif v != None:
       # Il s'agit d'un objet isole
       length = 1
       val = (v, )
    else: # Le reste : None,...
       return 0, ()
    if length > mxval:
       length = -length
    return length, val


def TraceGet( nom_fonction , nom_motfac , iocc , nom_motcle , tup , sortie=sys.stdout ) :
    """
       Fonction TraceGet
       Auteur : Antoine Yessayan
       Intention : affiche sur la sortie standard, le resultat retourne par une methode
                   getxxxx
    """
    sortie.write( "\t"+nom_fonction+" :" )
    assert(nom_motfac!=None)
    sortie.write( " nom_motfac='"+nom_motfac+"'" )
    sortie.write( " nom_motcle='"+nom_motcle+"'" )
    sortie.write( " iocc= "+`iocc`+' :' )
    if tup[0] == 0 :
       sortie.write( 'None' )
       sortie.write( '\n' )
    else :
       for e in tup[1] :
          sortie.write( ' ' )
          if type(e) == str:
             sortie.write( '"'+e+'"' )
          else :
             sortie.write( `e` )
          sortie.write( '\n' )

    return

def CONVID (v):
   """
      Cette fonction convertit un concept ou une liste de concept
      en chaine de caracteres ou liste de chaines
   """
   if type(v) in (list, tuple):
     l=()
     for e in v:
       l=l+(convid(e),)
     return l
   else:
     return convid(v)

def ast_name(concept):
   return concept.get_name().ljust(8)

def convid(e):
   if isinstance(e,ASSD):
      return ast_name(e)
   return e

def ReorganisationDe( texte , LongueurSousChaine=80 ) :
        """
           INTENTION : retourne la reorganisation de texte en une concatenation de
                       sous chaines de longueur strictement egale a LongueurSousChaine
                       caracteres. L'ensemble est mis entre parentheses.
                       Chaque caractere '\n', de texte, est supprime et eventuellement
                       la sous-chaine (utile) qui le contient est completee par
                       des espaces blancs pour faire une longueur de LongueurSousChaine.
        """
        assert(type(texte) == str)

        # on change les chaines '**' (puissance), 'E+' 'E-' (puissance de 10) pour les
        # proteger lors du decoupage suivant les separateurs
        texte=texte.replace('**','#1#')
        texte=texte.replace('E+','#2#')
        texte=texte.replace('E-','#3#')
        texte=texte.replace('e+','#4#')
        texte=texte.replace('e-','#5#')
        texte=texte.replace('D+','#6#')
        texte=texte.replace('D-','#7#')
        texte=texte.replace('d+','#8#')
        texte=texte.replace('d-','#9#')
        # conversion de texte en une liste de sous-chaines
        liste = ('(' + texte.strip() + ')').split('\n')
        # dans le cas d'une chaine trop longue (>LongueurSousChaine) :
        # on eclate suivant les separateurs de la liste l_separ
        l_separ=['+','-','/','*','(',')']
        liste2=[]
        LongueurSousChaine=LongueurSousChaine/2
        for sous_chaine in liste :
            uneChaine = sous_chaine.strip() # elimination des blancs inutiles.
            if ( len(uneChaine) > LongueurSousChaine ) :
               liste3=[]
               ttt=uneChaine[:LongueurSousChaine]
               j=0
               while(ttt.rfind(l_separ[j])<0):j=j+1
               liste3.append(ttt[:ttt.rfind(l_separ[j])])
               reste=ttt[ttt.rfind(l_separ[j]):]
               i=0
               for i in range(1,len(uneChaine)/LongueurSousChaine) :
                   ttt=uneChaine[i*LongueurSousChaine:(i+1)*LongueurSousChaine]
                   j=0
                   while(ttt.find(l_separ[j])<0):j=j+1
                   liste3.append(reste+ttt[:ttt.find(l_separ[j])])
                   k=0
                   while(ttt.rfind(l_separ[k])<0):k=k+1
                   liste3.append(ttt[ttt.find(l_separ[j]):ttt.rfind(l_separ[k])])
                   reste=ttt[ttt.rfind(l_separ[k]):]
               ttt=uneChaine[(i+1)*LongueurSousChaine:]
               liste3.append(reste+ttt)
               liste2=liste2+liste3
            else : liste2.append(uneChaine)

        # Construction dans apres de texte modifie.
        apres = ''
        LongueurSousChaine=LongueurSousChaine*2
        for sous_chaine in liste2 :
                uneChaine = sous_chaine.strip() # elimination des blancs inutiles.
                uneChaine = uneChaine.replace('#1#','**')
                uneChaine = uneChaine.replace('#2#','E+')
                uneChaine = uneChaine.replace('#3#','E-')
                uneChaine = uneChaine.replace('#4#','e+')
                uneChaine = uneChaine.replace('#5#','e-')
                uneChaine = uneChaine.replace('#6#','D+')
                uneChaine = uneChaine.replace('#7#','D-')
                uneChaine = uneChaine.replace('#8#','d+')
                uneChaine = uneChaine.replace('#9#','d-')
                if ( len(uneChaine) > LongueurSousChaine ) :
                        sys.stderr.write("ERREUR detectee dans Decoupe\n" )
                        print ">",uneChaine,"<"
                        raise Exception("Decoupe : chaine depassant la limite de %s caractères"
                                        % LongueurSousChaine)
                apres = apres + uneChaine.ljust(LongueurSousChaine)

        return apres

def Typast(ty):
   """
      Cette fonction retourne une chaine de caracteres indiquant le type
      du concept ou de la liste de concepts passé en argument (ty)
   """
   if type(ty) in (list, tuple):
#      t=ty[0]
      return [Typast(elem) for elem in ty]
   else :
      t=ty
   if t is None : return ""
   if t == 'I'  : return "IS "
   if t == 'R'  : return "R8 "
   if t == 'C'  : return "C8 "
   if t == 'TXM': return "TX "
   if issubclass(t, ASSD):
      if t.__name__ == 'reel'    :return "R8 "
      if t.__name__ == 'entier'  :return "IS "
      if t.__name__ == 'complexe':return "C8 "
      if t.__name__ == 'chaine'  :return "TX "
      return t.__name__
   return ""

# utilisés pour l'évaluation des formules
def miss_dble(list1, list2):
    """Retourne 3 objets set :
    - miss = elements de list1 absents de list2,
    - inter = intersection de list1 et list2,
    - dble = elements de list2 presents dans list1, fournis plusieurs fois."""
    s_ini = set(list1)
    inter = s_ini.intersection(list2)
    miss = s_ini.copy()
    dble = set()
    for para in list2:
        try:
            miss.remove(para)
        except KeyError:
            if set(para).issubset(s_ini) and para.strip() != '':
                dble.add(para)
    return miss, inter, dble
