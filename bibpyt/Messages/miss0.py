#@ MODIF miss0 Messages  DATE 30/06/2010   AUTEUR DELMAS J.DELMAS 
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
# RESPONSABLE DELMAS J.DELMAS

def _(x) : return x

cata_msg={
1: _("""
Longueur de LFREQ_LISTE incorrecte.
"""),

2: _("""
Longueur de CONTR_LISTE incorrecte.
"""),

3 : _("""
Il faut une et une seule couche avec SUBSTRATUM="OUI".
"""),

4 : _("""
La définition de la couche numéro %(i1)d est incorrecte :
Il y a %(i2)d matériaux, or NUME_MATE=%(i3)d.
"""),

5 : _("""
La numérotation des couches est incorrectes.
"""),

6 : _("""
Erreur lors de la copie de fichier pour MISS :
  source      : %(k1)s
  destination : %(k2)s
"""),

7 : _("""
Erreur lors de la lecture du fichier de résultat Aster
à la ligne numéro %(i1)d.

Message d'erreur :
%(k1)s
"""),

8 : _("""
Les données lues dans le fichier de résultat Aster ne sont pas cohérentes.
La trace ci-dessous doit montrer l'incohérence relevée.

Message d'erreur :
%(k1)s
"""),

}

