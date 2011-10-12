#@ MODIF observation Messages  DATE 12/10/2011   AUTEUR COURTOIS M.COURTOIS 
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


cata_msg = {

3 : _(u"""
 Observations: il y aura, au maximum, %(i1)d observations à chaque instant sélectionné pour cela.
"""),

5 : _(u"""
On ne peut faire plus de %(i1)d SUIVI_DDL.
Le nombre de SUIVI_DDL permis dépend de l'affichage du tableau de convergence et donc des fonctionnalités activées.
"""),

6 : _(u"""
 Erreur dans les données d'observation
 pour "NOM_CHAM"  %(k1)s , il faut renseigner  %(k2)s ou  %(k3)s 
"""),


8 : _(u"""
 Variation de la déformation supérieure au seuil fixé :
    seuil en valeur relative : %(r1)f
    entite : %(k1)s
    composante : %(k2)s
    numéro ordre : %(i1)d
"""),

37 : _(u"""
  Observations: %(i1)d réalisations pour ce pas de temps
"""),

38 : _(u"""
  Observations: une réalisation pour ce pas de temps
"""),

39 : _(u"""
  Observations: pas de réalisation pour ce pas de temps
"""),


}
