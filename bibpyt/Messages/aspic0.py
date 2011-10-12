#@ MODIF aspic0 Messages  DATE 12/10/2011   AUTEUR COURTOIS M.COURTOIS 
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
Vous affectez plus d'un materiau contenant l'option rccm.
"""),

2: _(u"""
Pour les piquages sains, TUBULURE doit etre renseigné.
"""),

3: _(u"""
EQUILIBRE[NOEUD] : on attend P1_CORP ou P2_CORP.
"""),

4: _(u"""
Il faut préciser un noeud pour EFFE_FOND.
"""),

5: _(u"""
PRES_REP[NOEUD] : on attend P1_CORP ou P2_CORP.
"""),

6: _(u"""
On ne peut appliquer un EFFE_FOND sur PRES_REP[NOEUD] car ce noeud est bloqué"
"""),

7: _(u"""
TORS_CORP[NOEUD] : on attend P1_CORP ou P2_CORP.
"""),

8: _(u"""
On ne peut appliquer un torseur sur TORS_CORP[NOEUD] car ce noeud est bloqué.
"""),

9: _(u"""
Si TYPE_MAILLAGE sain : mécanique de la rupture impossible.
"""),

10: _(u"""
Mot-clef <BORNES> obligatoire avec cette option.
"""),

11: _(u"""
Impression de résultats demandée sans préciser le nom des champs cf. la documentation utilisateur : U4.PC.20.
"""),

12: _(u"""
Les piquages pénetrants sont autorisés uniquement avec les soudures de type 1.
"""),

13: _(u"""
 La valeur de Z_MAX (cote maximale de la tubulure) est inférieure à la longueur 
 d'amortissement calculée :
 Z_MAX fournie   : %(r1)f
 Z_MAX calculee  : %(r2)f
-> Risque et Conseil :
 La longueur d'amortissement est liée à l'onde de flexion se propageant depuis le piquage.
 Si la longueur de la tubulure est inférieure à cette longueur, le calcul des contraintes 
 dans le piquage ne sera pas indépendant du mode d'application des conditions aux limites.
"""),

14: _(u"""
 Erreur donnees
 Dans le cas de fissures inclinees debouchant en peau interne avec
 piquage penetrant, le jeu doit etre nul.
"""),

15: _(u"""
 Erreur donnees
 Dans le cas de fissures internes (NON_DEB) le ligament inférieur est obligatoire.
"""),

16: _(u"""
Dans le cas de fissures internes (NON_DEB) le ligament est trop petit.
"""),

17: _(u"""
Dans le cas de fissures internes (NON_DEB) le ligament est trop grand.
"""),

18: _(u"""
Dans le cas de fissures courte il faut préciser la longueur.
"""),

19: _(u"""
Dans le cas de la fissure longue il faut préciser la longueur ou axis=oui.
"""),

20: _(u"""
Fissure axisymetrique : le mot clef <LONGUEUR> ne doit pas etre renseigné.
"""),

21: _(u"""
Seuls gibi98 et gibi2000 sont appelables.
"""),

22: _(u"""
Une interpénétration des lèvres est détectée pour le numéro d'ordre %(i1)d : sur les
%(i2)d noeuds de chaque lèvre, %(i3)d noeuds s'interpénètrent.
-> Risque et Conseil :
Le contact n'est pas pris en compte dans le calcul. Le taux de restitution de l'énergie G
est donc positif y compris là où la fissure tend à se refermer, ce qui peut conduire à
des résultats trop pénalisants.
Pour prendre en compte le contact entre les lèvres, il faut lancer le calcul hors macro.
"""),

23: _(u"""
 La valeur de X_MAX (cote maximale du corps) est inférieure à la longueur d'amortissement 
 calculée :
 X_MAX fournie   : %(r1)f
 X_MAX calculee  : %(r2)f
-> Risque et Conseil :
 La longueur d'amortissement est liée à l'onde de flexion se propageant depuis le piquage.
 Si la longueur ddu corps est inférieure à cette longueur, le calcul des contraintes 
 dans le piquage ne sera pas indépendant du mode d'application des conditions aux limites.
"""),

}
