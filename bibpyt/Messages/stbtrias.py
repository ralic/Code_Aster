#@ MODIF stbtrias Messages  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
 le fichier ideas est vide, ou ne contient pas de datatset traite par l'interface
"""),

2: _("""
 couleur inconnu
"""),

3: _("""
  attention le dataset 2420 apparait plusieurs fois.
"""),

4: _("""
  attention le dataset 18 apparait plusieurs fois.
"""),

5: _("""
 groupe  %(k1)s  de longueur superieure a 8 (troncature du nom)
"""),

6: _("""
 le nom du groupe est invalide:  %(k1)s  : non traite
"""),

7: _("""
 le nom du groupe  %(k1)s  est tronque :  %(k2)s 
"""),

8: _("""
 le nom du groupe ne peut commencer par coul_ : non traite
"""),

9: _("""
  aucun systeme de coordonnes n'est defini
"""),

10: _("""
  attention systeme de coordonnes autre que cartesien non relu dans aster.
"""),

11: _("""
  attention votre maillage utiliseplusieurs systemes de coordonnees.verifier qu'ils sont tous identiques car asterne gere qu'un systeme de coordonne cartesien unique.
"""),
}
