#@ MODIF B_utils Build  DATE 20/06/2002   AUTEUR D6BHHJP J.P.LEFEBVRE 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
import types,sys,string

# Modules Eficas
from Noyau.N_ASSD import ASSD

def RETLIST(v,mxval):
   """ 
       Cette fonction retourne un tuple dont le premier element est sa longueur reelle
       eventuellement negative si supérieure a mxval
       et le deuxieme la liste complete qui sera tronquee a mxval dans le fortran
   """
   if type(v) in (types.ListType, types.TupleType):
     if len(v) == 3 and v[0] in ("RI","MP"): # On a affaire a un complexe isole
       ll=1
       l=(v,)
     else: # Il s'agit d'un tuple de longueur quelconque
       ll= len(v)
       l=v
   elif v != None: # Il s'agit d'un objet isole
     ll=1
     l=(v,)
   else: # Le reste : None,...
     return 0,()
   if ll > mxval:ll= -ll
   return ll,l


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
          if type(e) == types.StringType :
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
   if type(v) in (types.ListType, types.TupleType):
     l=()
     for e in v:
       l=l+(convid(e),)
     return l
   else:
     return convid(v)

def ast_name(concept):
   return string.ljust(concept.get_name(),8)

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
           AUTEUR : AY sur la base d'un developpement de CCar
        """
        assert(type(texte)==types.StringType)

        # conversion de texte en une liste de sous-chaines
        liste=string.split('('+string.strip(texte)+')','\n')

        # Construction dans apres de texte modifie.
        apres = ''
        for sous_chaine in liste :
                uneChaine = string.strip( sous_chaine) # elimination des blancs inutiles.
                if ( len(uneChaine) > LongueurSousChaine ) :
                        sys.stderr.write("ERREUR detectee dans Decoupe\n" )
                        print ">",uneChaine,"<"
                        raise "Decoupe : chaine depassant la limite de "+`LongueurSousChaine`+\
                        ' caracteres'
                apres = apres + string.ljust(uneChaine,LongueurSousChaine)

        return apres

def Typast(ty):
   """
      Cette fonction retourne une chaine de caracteres indiquant le type
      du concept ou de la liste de concepts passé en argument (ty)
   """
   if type(ty) == types.TupleType : 
      t=ty[0]
   else : 
      t=ty
   if t == 'I': return "IS "
   if t == 'R': return "R8 "
   if t == 'C': return "C8 "
   if t == 'TXM': return "TX "
   if type(t)==types.ClassType :
      if t.__name__ == 'reel':return "R8 "
      if t.__name__ == 'entier':return "IS "
      if t.__name__ == 'complexe':return "C8 "
      if t.__name__ == 'chaine':return "TX "
      return t.__name__
   return None

