#@ MODIF test0 Messages  DATE 19/11/2007   AUTEUR COURTOIS M.COURTOIS 
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
1 : _("""
Expression régulière invalide : %(k2)s

Exception retournée :
   %(k1)s
"""),

2 : _("""
Le fichier n'a pas été fermé : %(k1)s
"""),

3 : _("""
TEST_FICHIER impossible, fichier inexistant : %(k1)s
"""),

4 : _("""

     Nom du fichier   : %(k3)s

                       -----------------------------------------------------------------------
                       |             FICHIER              |            REFERENCE             |
   -------------------------------------------------------------------------------------------
   | Nombre de valeurs |            %(i1)6d                |            %(i2)6d                |
   | Somme des valeurs |      %(r1)20.13e        |      %(k4)20s        |
   | md5sum des textes | %(k1)-32s | %(k2)-32s |
   -------------------------------------------------------------------------------------------

"""),

}



