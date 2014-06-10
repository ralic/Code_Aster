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

cata_msg={
1: _(u"""
GROUP_MA et GROUP_MA_BORD incohérents.
"""),

2: _(u"""
GROUP_MA et NOEUD incohérents.
"""),

3: _(u"""
Il faut donner un noeud unique.
"""),

4: _(u"""
Poutre circulaire à variation de section homothétique.

Le rapport d'homothétie est assez différent entre les rayons et les épaisseurs :
    - rayon 1 = %(r1)16.9g
    - rayon 2 = %(r2)16.9g
        `- rapport d'homothétie = %(r5)16.9g

    - épaisseur 1 = %(r3)16.9g
    - épaisseur 2 = %(r4)16.9g
        `- rapport d'homothétie = %(r6)16.9g

La différence entre les rapports d'homothétie est supérieure à 1%%.
Les hypothèses du modèle de poutre à variation homothétique ne sont donc pas
respectées (consultez la documentation de référence des éléments poutre).


Risques et conseil:
    - Les calculs seront inexacts.
    - Raffiner le maillage permet de minimiser les écarts.
"""),

}
