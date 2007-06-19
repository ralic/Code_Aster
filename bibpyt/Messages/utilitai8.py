#@ MODIF utilitai8 Messages  DATE 19/06/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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

}
