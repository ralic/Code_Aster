#@ MODIF calcessai0 Messages  DATE 28/06/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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

# RESPONSABLE BODEL C.BODEL

def _(x) : return x

cata_msg={
1: _("""
Le modèle mesuré doit etre un concept de type DYNA_HARMO ou MODE_MECA.
"""),
3: _("""
Calcul de MAC impossible : bases incompatibles.
"""),
4: _("""
Problème inverse impossible : problème de cohérence entre les données.
"""),
5: _("""
Problème de NUME_DDL dans MACRO_EXPANS : il est possible de le preciser
a l'appel de la macro. Conséquence : erreur fatale possible dans les
operations ulterieures (notamment l'operateur MAC_MODE)
"""),
6: _("""
Si vous n'avez pas selectionne de NUME_ORDRE ou de NUME_MODE dans %(k1)s.
Il ne faut pas declarer de concept en sortie de type %(k2)s.
Cela risque de causer une erreur fatale par la suite.
"""),
7: _("""
Erreur dans MACRO_EXPANS
"""),
8: _("""
Impossible de trouver le modele associe a la base de modes %(k1)s.
Cela peut empecher certains calculs de se derouler normalement.
"""),
9: _("""
Les mots-clés MATR_A et MATR_B n'ont pas été renseignés dans OBSERVATION.
Cela peut s'avérer indispensable pour la suite des calculs (les calculs
d'expansion et de MAC ne seront pas possibles).
"""),
10: _("""
Le modèle associé aux matrices MATR_A et MATR_B doit être le même que MODELE_2.
"""),
13: _("""
Le résultat expérimental est un dyna_harmo : il n'est pas possible d'en extraire
des numéros d'ordre avec MACRO_EXPANS. Le mots-clés NMUE_MODE et NUME_ORDRE
sont ignorés.
"""),
14: _("""
Erreur dans le calcul de MAC : le nume_ddl associé à la base %(k1)s
n'existe pas. Si cette base a été créée avec PROJ_CHAMP, ne pas oublier
de mentionner explicitement le nume_ddl de la sd résultat avec le mot-clé
NUME_DDL.
"""),


}
