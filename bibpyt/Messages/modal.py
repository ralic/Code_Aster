#@ MODIF modal Messages  DATE 16/10/2012   AUTEUR ALARCON A.ALARCON 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
Arrêt du calcul des modes : pas de mode propre dans la bande de fréquence demandée.
"""),
2: _(u"""
Opérateur MACRO_MODE_MECA
Pas de test de Sturm demandé (VERI_MODE=_F(STURM='NON')).
Donc, à l'issu de chaque calcul modal sur une sous-bande, on vérifie seulement que:
   - la norme de l'erreur est bien valide (via le paramètre VERI_MODE/SEUIL),
   - chaque fréquence est bien incluse dans la bande spécifiée (VERI_MODE/PREC_SHIFT).
Pas de test de Sturm local ou global.
"""),
3: _(u"""
Opérateur MACRO_MODE_MECA
Test de Sturm local demandé (VERI_MODE=_F(STURM='LOCAL')).
Donc, à l'issu de chaque calcul modal sur une sous-bande, on vérifie que:
    - la norme de l'erreur est bien valide (via le paramètre paramètre VERI_MODE/SEUIL),
    - chaque fréquence est bien incluse dans la bande spécifiée (VERI_MODE/PREC_SHIFT),
    - le test de Sturm est valide.
"""),
4: _(u"""
Opérateur MACRO_MODE_MECA
Test de Sturm global demandé (VERI_MODE=_F(STURM='GLOBAL')).
Donc, à l'issu de chaque calcul modal sur une sous-bande, on vérifie que:
    - la norme de l'erreur est bien valide (via le paramètre paramètre VERI_MODE/SEUIL),
    - chaque fréquence est bien incluse dans la bande spécifiée (VERI_MODE/PREC_SHIFT),
    - pas de test de Sturm local.
Puis, on réunit les fréquences calculées sur toutes les sous-bandes et on fait un test
de Sturm global:
  Dans l'intervalle (%(r1)f,%(r2)f), il y a théoriquement %(i1)d fréquence(s) et on
  en a bien calculé %(i2)d.
"""),
5: _(u"""
Opérateur MACRO_MODE_MECA, test de Sturm global:
  Dans l'intervalle (%(r1)f,%(r2)f), il y a théoriquement %(i1)d fréquence(s) et on
  en a calculé %(i2)d.

Conseil:
Vous pouvez relancer le calcul en demandant à faire le test de Sturm de post-vérification
sur chaque sous-bande (VERI_MODE=_F(STURM='LOCAL')). Cela peut aider à trouver le problème:
mode multiple sauté, borne trop proche d'un mode multiple, mode de corps rigide...
Vous pouvez aussi relancez un INFO_MODE et/ou des MODE_ITER_SIMULT sur une sous-partie pour
corroborer (ou non) les résultats précédent.
"""),
6: _(u"""
Opérateur MACRO_MODE_MECA, la sous-bande n %(i1)d est vide, on passe à la suivante.
"""),
7: _(u"""
Opérateur MACRO_MODE_MECA: Test de Sturm global.
  On n'a pas pu faire ce test car la bande de fréquence demandée (%(r1)f,%(r2)f) est vide !
"""),
8: _(u"""
Opérateur MACRO_MODE_MECA:
  Pas de mode propre dans la bande de fréquence demandée (%(r1)f,%(r2)f) !
  Le concept résultat sera vide.
"""),
}
