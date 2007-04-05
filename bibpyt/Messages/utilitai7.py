#@ MODIF utilitai7 Messages  DATE 06/04/2007   AUTEUR PELLET J.PELLET 
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







2: _("""
 erreur dans les donneespas de tri sur les complexes,  parametre:  %(k1)s 
"""),








4: _("""
  numero d''occurrence invalide   %(i1)d pour le mot cle facteur %(k1)s 
"""),

5: _("""
 le numero  de la composante (pour vari_r) est trop grand.maille: %(k1)s 
 num. cmp maxi: %(i1)d 
 num. cmp demandee: %(i2)d 
"""),

6: _("""
 Le schéma d'intégration temporelle %(k1)s et le paramètre %(k2)s sont incompatibles.
"""),

7: _("""
 Le paramètre %(k1)s ne fait pas partie des choix possibles.
"""),

99: _("""
 Arret dans le programme %(k1)s.
"""),

}
