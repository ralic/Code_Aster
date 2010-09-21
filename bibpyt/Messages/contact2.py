#@ MODIF contact2 Messages  DATE 20/09/2010   AUTEUR DESOZA T.DESOZA 
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

1 : _("""
   Echec lors du traitement du contact.
"""),

2 : _("""
   Echec : la matrice de contact est singulière.
"""),

3 : _("""
  Toutes vos zones de contact sont en mode RESOLUTION='NON'.
  Le mode REAC_GEOM = 'SANS' est forcé. 
"""),

11 : _("""
Au moins une des mailles de contact que vous avez définies est de dimension %(i1)i, or la dimension de votre problème est : %(i2)i.
Cette maille n'est donc pas une maille de bord. Il doit y avoir une erreur dans votre mise en données.

Conseil :
Vérifiez votre AFFE_MODELE et le type de vos mailles dans la définition des surfaces de contact. 
"""),

12 : _("""
Contact avec formulation continue. 
Votre modèle contient des surfaces de contact qui s'appuient sur un mélange d'éléments axisymétriques et non axisymétriques.
Cela n'a pas de sens. Toute la modélisation doit être axisymétrique.

Conseil :
Vérifiez votre AFFE_MODELE et le type de vos mailles dans la définition des surfaces de contact. 
"""),

13 : _("""
Contact méthodes maillées. Il existe une zone de contact dans laquelle un noeud est commun aux surfaces maîtres et
esclaves. Vérifiez la définition de vos surfaces de contact ou bien renseignez le mot-clé SANS_GROUP_NO.
"""),

14 : _("""
Contact méthode continue. 
  -> Une zone de contact est définie sur une modélisation axisymétrique. Le Jacobien
     est nul car un noeud de la surface de contact esclave appartient à l'axe.
     La pression de contact (degré de liberté LAGS_C) risque d'être erronée.
  -> Conseil :
     Il faut changer de schéma d'intégration et utiliser 'GAUSS'.
"""),


}

