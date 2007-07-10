#@ MODIF utilitai8 Messages  DATE 10/07/2007   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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

def _(x) : return x

cata_msg={
1: _("""
 rien que des constantes pour une nappe
 nombre de fonctions constantes %(i1)d 
"""),

2: _("""
 parametres differents
 fonction %(k1)s de parametre %(k2)s au lieu de %(k3)s 
"""),

3: _("""
  le nombre de parametres  %(i1)d  est different du nombre de fonctions  %(i2)d 
"""),

4: _("""
 il n'y a pas un nombre pair de valeurs, "defi_fonction" occurence  %(i1)d 
"""),

5: _("""
 les abscisses de la fonction  %(k1)s ont ete reordonnees.
"""),

6: _("""
 l ordre des abscisses de la fonction numero  %(i1)d a ete inverse .
"""),

7: _("""
 appel errone
  archivage numero :  %(i1)d
  code retour de rsexch :  %(i2)d 
"""),

8: _("""
 lecture des champs:
"""),

9: _("""
   numero d'ordre :  %(i1)d             inst :  %(r1)f 
"""),

10: _("""
 
"""),

11: _("""
 erreur de programmation
"""),

12: _("""
 erreur de programmation
"""),

13: _("""
 trop grande valeur pour i2
"""),

14: _("""
 champ  inexistant  %(k1)s 
"""),

15: _("""
  ou  %(k1)s 
"""),

16: _("""
  nume_ordre  %(i1)d  on ne calcule pas l'option  %(k1)s 
"""),

17: _("""
 pas nume_ordre trouve pour le numero  %(i1)d 
"""),

18: _("""
 pas de champs trouve pour l'instant  %(r1)f 
"""),

19: _("""
   plusieurs pas de temps trouves  dans l'intervalle de precision
  autour de l'instant  %(r1)f 
 nombre de pas de temps trouves  %(i1)d 
 modifier le parametre precision
"""),

20: _("""
 erreur dans les donnees
 parametre existe deja:  %(k1)s  dans la table:  %(k2)s 
"""),

21: _("""
 erreur dans les donnees
 type de parametre:  %(k1)s 
  different pour le parametre:  %(k2)s 
  et le parametre:  %(k3)s 
"""),

22: _("""
  Valeur de M maximale atteinte pour résoudre F(M)=0,
  verifiez vos listes d'instants de rupture, M maximal admissible =  %(r1)f  
"""),

23: _("""
  Valeur de M minimale atteinte pour résoudre F(M)=0,
  verifiez vos listes d'instants de rupture, valeur de M =  %(r1)f  
"""),

24: _("""
 le champ demande est incompatible avec le type de resultat
  type de resultat : %(k1)s 
      nom du champ : %(k2)s 
 
"""),

25: _("""
 
 le nombre d'asterisques pour les noms de fichiers ensight de pression est trop grand il est limite a 7
  nombre d'asterisques : %(i1)d 
 
"""),

26: _("""
 appel errone  resultat :  %(k1)s   archivage numero :  %(i1)d 
   code retour de rsexch :  %(i2)d 
   probleme champ :  %(k2)s 
 
"""),

27: _("""
 appel errone  resultat :  %(k1)s   archivage numero :  %(i1)d 
   code retour de rsexch :  %(i2)d 
   probleme champ :  %(k2)s 
 
"""),

28: _("""
 fin de fichier dans la lecture des fichiers ensight
"""),

29: _("""
 erreur dans la lecture du fichier ensight
"""),

30: _("""
  probleme pour le fichier:  %(k1)s 
"""),

31: _("""
  option deja calculee:  option  %(k1)s  nume_ordre  %(i1)d 
  on la recalcule car les donnees peuvent etre differentes 
 
"""),

32: _("""
 l'extrapolation ne peut etre faite a gauche (interdit).
"""),

33: _("""
 l'extrapolation ne peut etre faite a droite (interdit).
"""),

34: _("""
 l'interpolation ne peut etre faite car aucun champ de : %(k1)s n'est calcule.
"""),

35: _("""
 la variable d'acces %(k1)s est invalide pour une interpolation.
"""),

36: _("""
 ce nom de champ est interdit : %(k1)s pour une interpolation.
"""),

37: _("""
 resultat: %(k1)s nom_cham: %(k2)s  variable d'acces: %(k3)s valeur: %(r1)f 
 
"""),




38: _("""
 Plusieurs champs correspondant a l'acces demande.resultat  %(k1)s 
"""),

39: _("""
 acces %(k1)s : %(i1)d 
"""),

40: _("""
 acces %(k1)s : %(r1)f 
"""),

41: _("""
 acces %(k1)s  : %(k1)s 
"""),




46: _("""
  nombre : %(i1)d nume_ordre retenus : %(i2)d, %(i3)d, %(i4)d
"""),

47: _("""
 pas de champ correspondant a un acces demande.resultat  %(k1)s 
"""),




55: _("""
 
"""),

56: _("""
 pas de champs pour l'acces  %(k1)s de valeur  %(r1)f 
"""),

57: _("""
 plusieurs champs correspondant a l'acces demande.resultat  %(k1)s 
 - acces "inst" : %(r1)f 
 - nombre : %(i1)d 
 
"""),

58: _("""
 pas de champs pour l'acces  %(k1)s de valeur  %(r1)f 
"""),

59: _("""
 plusieurs champs correspondant a l'acces demande.resultat  %(k1)s 
 - acces "freq" : %(r1)f 
 - nombre : %(i1)d 
 
"""),

60: _("""
 erreur dans les donnees pour le champ  %(k1)s 
"""),

61: _("""
 aucuns noeuds ne supportent
"""),

62: _("""
 aucunes mailles ne supportent
"""),

63: _("""
  les composantes  %(k1)s, %(k2)s, %(k3)s, %(k4)s, ...  
"""),


}
