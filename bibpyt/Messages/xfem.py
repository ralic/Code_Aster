#@ MODIF xfem Messages  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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

1: _("""
Pour le DVP : écrasement des valeurs nodales dans xconno.f
Pour l'utilisateur : les fissures X-FEM sont surement trop proches.
                     il faut au minimum 2 mailles entre les fissures.
                     veuillez raffiner le maillage entre les fissures (ou écarter les fissures). 
"""),

2: _("""
 Le nombre de fissures autorisées avec X-FEM est limité à (i1)i
"""),

3: _("""
 Echec de copier le champ de %(i1)d éléments du CHAM_ELEM_S au CHAM_ELEM 
"""),

4: _("""
 Il est interdit de melanger dans un modèle les fissures X-FEM avec et sans
 contact. Veuillez rajouter les mots clé CONTACT manquants 
 dans DEFI_FISS_XFEM.
"""),

5: _("""
La valeur du parametre %(k1)s (%(i1)d) de la fissure %(k2)s 
a été changé à 
%(i2)d (valeur maximale de toutes les fissures du modèle)
"""),


}
