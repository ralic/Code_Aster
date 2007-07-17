#@ MODIF aspic0 Messages  DATE 17/07/2007   AUTEUR REZETTE C.REZETTE 
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
Vous affectez plus d'un materiau contenant l'option rccm.
"""),

2: _("""
Pour les piquages sains, TUBULURE doit etre renseigné.
"""),

3: _("""
EQUILIBRE[NOEUD] : on attend P1_CORP ou P2_CORP.
"""),

4: _("""
Il faut préciser un noeud pour EFFE_FOND.
"""),

5: _("""
PRES_REP[NOEUD] : on attend P1_CORP ou P2_CORP.
"""),

6: _("""
On ne peut appliquer un EFFE_FOND sur PRES_REP[NOEUD] car ce noeud est bloqué"
"""),

7: _("""
TORS_CORP[NOEUD] : on attend P1_CORP ou P2_CORP.
"""),

8: _("""
On ne peut appliquer un torseur sur TORS_CORP[NOEUD] car ce noeud est bloqué.
"""),

9: _("""
Si TYPE_MAILLAGE sain : mécanique de la rupture impossible.
"""),

10: _("""
Mot-clef <BORNES> obligatoire avec cette option.
"""),

11: _("""
Impression de résultats demandée sans préciser le nom des champs cf. la documentation utilisateur : U4.PC.20.
"""),

12: _("""
Les piquages pénetrants sont autorisés uniquement avec les soudures de type 1.
"""),

13: _("""
 Erreur donnees
 Z_MAX fournie   : %(r1)f
 Z_MAX calculee  : %(r2)f
"""),

14: _("""
 Erreur donnees
 Dans le cas de fissures inclinees debouchant en peau interne avec
 piquage penetrant, le jeu doit etre nul.
"""),

15: _("""
 Erreur donnees
 Dans le cas de fissures internes (NON_DEB) le ligament inférieur est obligatoire.
"""),

16: _("""
Dans le cas de fissures internes (NON_DEB) le ligament est trop petit.
"""),

17: _("""
Dans le cas de fissures internes (NON_DEB) le ligament est trop grand.
"""),

18: _("""
Dans le cas de fissures courte il faut préciser la longueur.
"""),

19: _("""
Dans le cas de la fissure longue il faut préciser la longueur ou axis=oui.
"""),

20: _("""
Fissure axisymetrique : le mot clef <LONGUEUR> ne doit pas etre renseigné.
"""),

21: _("""
Seuls gibi98 et gibi2000 sont appelables.
"""),

}
