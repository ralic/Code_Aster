#@ MODIF meidee0 Messages  DATE 21/10/2008   AUTEUR NISTOR I.NISTOR 
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
Le modèle mesuré doit etre un concept de type DYNA_HARMO ou MODE_MECA.
"""),
2: _("""
Le maillage et les déformées ne sont pas compatibles.
"""),
3: _("""
Calcul de MAC impossible : bases incompatibles.
"""),
4: _("""
Problème inverse impossible : problème de cohérence entre les données.
"""),
5: _("""
Problème de NUME_DDL dans MACRO_EXPANS : il est possible de le preciser
a l'appel de la macro. Si vous utilisez MACRO_EXPANS par l'intermediaire d'une IHM,
contactez l'assistance technique.
"""),
6: _("""
Si vous n'avez pas selectionne de NUME_ORDRE ou de NUME_MODE dans %(k1)s.
Il ne faut pas declarer de concept en sortie de type %(k2)s.
Cela risque de causzer une erreur fatale par la suite.
"""),
7: _("""
Erreur dans MACRO_EXPANS
"""),


}
