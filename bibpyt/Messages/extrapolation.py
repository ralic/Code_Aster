#@ MODIF extrapolation Messages  DATE 12/10/2011   AUTEUR COURTOIS M.COURTOIS 
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
# RESPONSABLE DELMAS J.DELMAS

# Pour la méthode EXTRAPOLATION de DEFI_LIST_INST

cata_msg={

1: _(u"""
    On tente une extrapolation linéaire sur les résidus.
"""),

2: _(u"""
    L'extrapolation sur les résidus n'est pas possible.
    On utilise un autre résidu que RESI_GLOB_RELA ou RESI_GLOB_MAXI pour l'évaluation de la convergence.
    Ce n'est pas prévu.
"""),

3: _(u"""
    L'extrapolation sur les résidus n'est pas possible car il n'y a pas assez de valeurs pour la faire.
"""),

10: _(u"""
    L'extrapolation sur les résidus a échoué.
"""),

11: _(u"""
    L'extrapolation sur les résidus a réussi.
"""),

}

