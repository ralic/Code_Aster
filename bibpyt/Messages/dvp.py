#@ MODIF dvp Messages  DATE 23/10/2007   AUTEUR COURTOIS M.COURTOIS 
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

"""
   Messages à l'attention des développeurs, type "assert"
   Lorsque l'utilisateur tombe sur un tel message, tout ce qu'il a à faire,
   c'est de rapporter le bug, le texte du message devant aider le
   développeur à faire le diagnostic.
   
   "UTPRIN" ajoute automatiquement ces deux lignes :
      Il y a probablement une erreur dans la programmation.
      Veuillez contacter votre assistance technique.
"""

cata_msg={

1 : _("""
   Erreur de programmation : condition non respectée.
"""),

2 : _("""
   Erreur numérique (floating point exception).
"""),

3 : _("""
   Erreur de programmation : Nom de grandeur inattendu : %(k1)s
   Routine : %(k2)s
"""),

4 : _("""
   On ne sait pas traiter ce type d'élément : %(k1)s
"""),



98 : _("""
   Erreur lors du traitement du contact.
"""),

99 : _("""
   Le calcul de l'option %(k1)s n'est pas prévue avec Arlequin.
"""),

}
