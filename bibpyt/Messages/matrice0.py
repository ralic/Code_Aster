#@ MODIF matrice0 Messages  DATE 16/10/2012   AUTEUR ALARCON A.ALARCON 
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

5: _(u"""
 La construction d'un nouveau concept NUME_DDL ne peut se faire qu'en présence du mot clé MATR_ASSE avec une des options
 suivantes: RIGI_MECA, RIGI_THER, RIGI_ACOU ou RIGI_MECA_LAGR.
 Attention: si vous cherchez à assembler des vecteurs seulement, le concept NUME_DDL doit être construit préalablement. 
"""),

6: _(u"""
  Attention: le mot-clé CHARGE définissant les conditions de Dirichlet n'a pas été renseigné. 
  Pour l'assemblage d'un vecteur selon une numérotation imposée (NUME_DDL), le mot-clé CHARGE 
  doit être renseigné à l'identique que lors de la création du NUME_DDL, sous risque d'assemblage erroné. 
  Cependant, si votre modèle ne contient aucune condition de Dirichlet votre syntaxe est correcte.
"""),

7: _(u"""
  Le mot-clé CHAM_MATER est obligatoire pour la construction d'un vecteur assemblé avec l'option CHAR_ACOU. 
"""),

8: _(u"""
  Pour la construction d'un vecteur assemblé il faut renseigner au moins une charge.
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

12: _(u"""
 Une des charges renseignées pour l'assemblage des vecteurs est déjà présente dans le mot-clé 
 CHARGE définissant les conditions de Dirichlet. Il est interdit de renseigner plus d'une fois la même charge.
"""),

}
