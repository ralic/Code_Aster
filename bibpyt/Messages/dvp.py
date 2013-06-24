# coding=utf-8
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
# person_in_charge: josselin.delmas at edf.fr

"""
   Messages à l'attention des développeurs, type "ASSERT"
   Lorsque l'utilisateur tombe sur un tel message, tout ce qu'il a à faire,
   c'est de rapporter le bug, le texte du message devant aider le
   développeur à faire le diagnostic.
   
   "UTPRIN" ajoute automatiquement ces deux lignes :
      Il y a probablement une erreur dans la programmation.
      Veuillez contacter votre assistance technique.
"""

cata_msg={

1 : _(u"""
   Erreur de programmation : condition non respectée.
"""),

2 : _(u"""
   Erreur numérique (floating point exception).
"""),

3 : _(u"""
   Erreur de programmation : Nom de grandeur inattendu : %(k1)s
   Routine : %(k2)s
"""),

4 : _(u"""
   On ne sait pas traiter ce type d'élément : %(k1)s
"""),

5 : _(u"""
 Erreur de programmation :
  On ne trouve pas le triplet ( %(k1)s )
  correspondant à (nomte elrefe famille).
 Conseils :
  Vérifiez le catalogue d'éléments.
  L'elrefe ou la famille de points de Gauss ne sont pas définis.
"""),

9 : _(u"""
   Erreur de programmation dans un module Python. 
   Condition non respectée : %(k2)s

      %(k1)s
"""),

97 : _(u"""
   Erreur signalée dans la bibliothèque MED
     nom de l'utilitaire : %(k1)s
             code retour : %(i1)d
"""),

}
