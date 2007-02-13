#@ MODIF prepost5 Messages  DATE 13/02/2007   AUTEUR PELLET J.PELLET 
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
 L'option %(k1)s est deja calculee pour le numero d'ordre %(k2)s.
 On la recalcule car les donnees peuvent etre differentes.
"""),

2: _("""
Champ inexistant SIEF_ELGA ou SIEF_ELGA_DEPL numero d'ordre %(k2)s pour le calcul de l'option %(k3)s
"""),

3: _("""
Champ inexistant DEPL numero d'ordre %(k2)s pour le calcul de l'option %(k3)s
"""),

4: _("""
Champ inexistant %(k1)s numero d'ordre %(k2)s pour le calcul de l'option %(k3)s
"""),

5: _("""
Option illicite pour le resultat %(k1)s numero d'ordre %(k2)s pour le calcul de l'option %(k3)s
"""),

6: _("""
Numero d'ordre trop grand %(k1)s pour le calcul de l'option %(k2)s
"""),

7: _("""
Option illicite pour le resultat %(k1)s numero d'ordre trop grand %(k2)s pour le calcul de l'option %(k3)s
"""),


}
