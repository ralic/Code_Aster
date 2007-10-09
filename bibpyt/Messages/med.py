#@ MODIF med Messages  DATE 09/10/2007   AUTEUR COURTOIS M.COURTOIS 
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
 Absence de localisation de points de Gauss dans le fichier MED
 pour l'élément de référence %(k1)s.
 On suppose que l'ordre des points de Gauss est celui d'Aster.
 Risque de résultats faux.
"""),

2: _("""
 Le nombre de points de Gauss est différent entre le fichier med et Aster:
 - nombre de points de Gauss contenu dans le fichier MED : %(i2)d
 - nombre de points de Gauss défini  dans Aster          : %(i1)d
"""),

3: _("""
Risque de résultats faux à cause d'une incompatibilité de points de Gauss
entre Med et Aster.
"""),

4: _("""

Point De Gauss : %(i1)d              MED               ASTER
"""), 

5: _("""
   %(k1)s                          %(r1)f          %(r2)f
"""), 

6: _("""
Une ou plusieurs permutations ont été effectuées sur l'ordre des points de Gauss
pour que la localisation Med corresponde à celle d'Aster.

"""),
}
