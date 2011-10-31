#@ MODIF matrice0 Messages  DATE 31/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

cata_msg={
1: _(u"""
 Cas fluides multiples : précisez le GROUP_MA dans lequel vous affectez la masse volumique RHO.
"""),

2: _(u"""
 PRES_FLUIDE obligatoire une fois.
"""),

3: _(u"""
 Amortissement ajouté sur modèle généralisé non encore implanté.
"""),

4: _(u"""
 Rigidité ajouté sur modèle généralisé non encore implanté.
"""),

9: _(u"""
 Une des options doit être RIGI_MECA ou RIGI_THER ou RIGI_ACOU ou RIGI_MECA_LAGR.
"""),

10: _(u"""
 Pour calculer RIGI_MECA_HYST, il faut avoir calculé RIGI_MECA auparavant (dans le même appel).
"""),

11: _(u"""
 Pour calculer AMOR_MECA, il faut avoir calculé RIGI_MECA et MASS_MECA auparavant (dans le même appel).
"""),

}
