#@ MODIF precalcul Messages  DATE 11/09/2007   AUTEUR DURAND C.DURAND 
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
Le type du parametre CARA_ELEM nomme <%(k1)s> est inconnu
Contactez le support
"""),

2: _("""
Le type du parametre XFEM nomme <%(k1)s> est inconnu
Contactez le support
"""),

11: _("""
Le type de paramètre pour le champ IN de température est inconnu (ni scalaire, ni fonction mais <%(k1)s>)
Contactez le support
"""),

20: _("""
Le type de calcul du chargement est invalide :  %(k1)s
Contactez le support
"""),

50: _("""
Dépassement de la capacité pour les tableaux de champs d'entrée de CALCUL
Contactez le support
"""),

51: _("""
On tente d'écraser le paramètre de champ d'entrée de CALCUL déjà existant nommé <%(k1)s>
par un autre paramètre d'entree nommé <%(k2)s>
Contactez le support
"""),

52: _("""
On tente d'écraser le champ d'entrée de CALCUL deja existant nomme <%(k1)s> par un autre champ d'entrée nomme <%(k2)s>
Contactez le support
"""),

60: _("""
Appel à CALCUL
Le nom du paramètre de champ d'entree numéro %(i1)d est vide.
"""),

61: _("""
Appel à CALCUL
Le champ d'entrée numéro %(i1)d est vide.
"""),

62: _("""
Appel à CALCUL. Le nom du paramètre de champ de sortie numéro %(i1)d est vide.
"""),

63: _("""
Appel à CALCUL. Le champ de sortie numéro %(i1)d est vide.
"""),


}
