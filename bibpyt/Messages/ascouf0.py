#@ MODIF ascouf0 Messages  DATE 30/06/2010   AUTEUR DELMAS J.DELMAS 
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
1: _("""
La condition aux limites sur bol à section conique 
est ignorée pour un coude avec sous-épaisseurs.
"""),

2: _("""
Mot-clé AZIMUT non autorisé dans le cas d'un coude sain"
"""),

3: _("""
POSI_ANGUL POSI_CURV_LONGI est obligatoire.
"""),

4: _("""
Il faut renseigner : ANGLE, R_CINTR et POSI_ANGUL ou ANGLE, R_CINTR et POSI_CURV_LONGI.
"""),

5: _("""
ANGL_COUDE et ANGL_SOUS_EPAI sont inutiles dans ce cas.
"""),

6: _("""
 ASCSEP valeur hors domaine
 sous-epaisseur numero : %(i1)d
 taille axe circonferentiel : %(r1)f
 bord plaque : %(r2)f
"""),

7: _("""
 ASCSEP cas de symetrie :
 la sous-epaisseur doit etre dans la section mediane du coude !
"""),


9: _("""
 Valeur hors domaine :
 sous-epaisseur numero :%(i1)d
 absc. curv. circonf. :%(r1)f
 bord plaque :%(r2)f
"""),

10: _("""
 Valeur hors domaine :
 sous-epaisseur numero :%(i1)d
 absc. curv. longit.  :%(r1)f
 bord plaque :%(r2)f
"""),

11: _("""
 valeur hors domaine :
 sous-epaisseur numero :%(i1)d
 bord inferieur  :%(r1)f
 bord plaque :%(r2)f
"""),


13: _("""
 Les quart et demi structure ne peuvent etre réalisees 
 sur un modèle comportant une transition d'épaisseur.
"""),

14: _("""
 Les deux embouts doivent etre de meme longueur pour les cas de symétrie.
"""),

15: _("""
 Longueur d'embout P1 inférieure a la longueur d'amortissement = %(r1)f
"""),

16: _("""
 Longueur d'embout P2 inférieure à la longueur d'amortissement = %(r1)f
"""),

17: _("""
 La condition aux limites raccord 3d-poutre appliquée avec la macro de calcul
 ascouf n'est pas licite avec un embout de type conique.
"""),

18: _("""
 Le nombre d'elements dans l'epaisseur du coude n'est pas parametrable pour
 un coude avec fissure.
 Le mot-cle NB_ELEM_EPAIS est ignoré.
"""),

19: _("""
 Pour les fissures non axisymetriques, la longueur doit etre spécifiée.
"""),

20: _("""
 La fissure est axisymetrique : on ne tient pas compte de la longueur spécifiée.
"""),

21: _("""
 Avec une transition d'épaisseur,la fissure doit obligatoirement etre transverse.
"""),

23: _("""
 L'orientation de la fissure doit etre transverse (orien : 90.) pour modéliser
 un quart ou une demi structure.
"""),

24: _("""
 La fissure est axisymetrique : son orientation doit etre transverse (ORIEN : 90.)
"""),

25: _("""
 Il ne peut pas y avoir plusieurs sous-épaisseurs en meme temps
 qu'une transition d'épaisseur: 
 si une seule sous-épaisseur, alors utiliser SOUS_EPAIS_COUDE.
"""),

26: _("""
 Avec une transition d'épaisseur,il doit obligatoirement y avoir un défaut,
 soit une fissure  soit une sous-épaisseur.
"""),

27: _("""
 Ne modéliser qu'une seule sous-épaisseur pour un quart ou demi-coude.
"""),

28: _("""
 Vous ne pouvez déclarer la sous-epaisseur comme axisymetrique et donner
 une taille d'axe circonferentiel.
"""),

29: _("""
 Vous devez donner une taille d'axe circonférentiel pour une sous-épaisseur
 de type elliptique.
"""),

30: _("""
 Valeur hors domaine de validité :
 sous-épaisseur numéro :%(i1)d
 abscisse curv. longit. :%(r1)f
 valeur maximale autorisée :%(r2)f
"""),

31: _("""
 Valeur hors domaine de validité :
 sous-épaisseur numéro :%(i1)d
 position angulaire centre sous-ep :%(r1)f
 valeur limite autorisée :%(r2)f
"""),

32: _("""
 Valeur hors domaine de validité : 
 sous-épaisseur numero :%(i1)d
 abscisse curv. circonf. :%(r1)f
 valeur limite autorisée :%(r2)f
"""),

33: _("""
 Le centre d'une sous-épaisseur axisymétrique est imposé en intrados (pi*RM).
"""),

34: _("""
 Le centre d'une sous-épaisseur axisymétrique est imposé en intrados.
 L'azimut est fixé à 180 degrés.
"""),

35: _("""
 Le nombre d'élements dans l'épaisseur du coude n'est pas parametrable pour
 la version 2 de la procedure de plaque avec sous-épaisseur : 
 mot-cle NB_ELEM_EPAIS ignoré.
"""),

36: _("""
 Valeur hors domaine de validité :
 surépaisseur :%(i1)d
 valeur limite autorisée (RM-EP1/2) :%(r1)f
"""),

37: _("""
 Valeur hors domaine de validité :
 le rayon de cintrage : %(r1)f
 doit etre supérieur a (RM+EP1/2) :%(r2)f
"""),

38: _("""
 Valeur hors domaine de validité (5,50)
 rapport RM/EP1 : %(r1)f
"""),

39: _("""
 Valeur hors domaine de validité :
 abscisse curviligne centre fissure :%(r1)f
 valeur limite autorisée :%(r2)f
"""),

40: _("""
 Valeur hors domaine de validité : 
 nombre de tranches :%(i1)d
"""),

41: _("""
 Valeur hors domaine de validité :
 position angulaire  centre fissure : %(r1)f
 posi_angul doit etre >= 0 et <= %(r2)f 
"""),

42: _("""
 Valeur hors domaine de validité : 
 début transition d'épaisseur :%(r1)f
 valeur minimale autorisée :%(r2)f
 valeur maximale autorisée :%(r3)f
"""),

43: _("""
 Valeur hors domaine de validité :
 angle de transition TETA1 : %(r1)f
 valeur minimale autorisée : 0.
 valeur maximale autorisée : 30.
"""),

44: _("""
 Valeur hors domaine de validité :
 épaisseur avant la transition :%(r1)f
 valeur minimale autorisée : 12
 valeur maximale autorisée : 80
"""),

45: _("""
 Valeur hors domaine de validité : 
 épaisseur apres la transition :%(r1)f
 valeur minimale autorisée : 20
 valeur maximale autorisée : 110
"""),

46: _("""
 L'épaisseur avant la transition doit etre inférieure
 à celle apres la transition.
"""),

47: _("""
 Valeur hors domaine de validité :
 fin transition d'épaisseur :%(r1)f
 valeur limite autorisée :%(r2)f
"""),

48: _("""
 Valeur hors domaine de validité :
 diam ext du tube avant transition:%(r1)f
 valeur minimum autorisée : 112.
 valeur maximum autorisée : 880.
"""),

49: _("""
 Valeur hors domaine de validité :
 angle de transition TETA2: %(r1)f
 valeur minimum autorisée : 0.
 valeur maximum autorisée : 45.
"""),

50: _("""
 Valeur hors domaine de validité :
 epaisseur avant 1ere transition:%(r1)f
 valeur minimum autorisee : 7.
 valeur maximum autorisee : 35.
"""),

51: _("""
 Valeur hors domaine de validité :
 epaisseur avant 2eme transition:%(r1)f
 valeur minimum autorisee : 15.
 valeur maximum autorisee : 40.
"""),

52: _("""
 Valeur hors domaine de validité :
 épaisseur intermediaire:%(r1)f
 valeur minimum autorisée : 15.
 valeur maximum autorisée : 40.
"""),

53: _("""
 Valeur hors domaine de validité.
 L'épaisseur avant la transition doit etre inférieure
 à l'épaisseur intermediaire.
"""),

54: _("""
 Valeur hors domaine de validité.
 L'épaisseur après la transition doit etre inférieure
 à l'épaisseur intermediaire.
"""),

55: _("""
 Valeur hors domaine de validité :
 fin transition d'épaisseur:%(r1)f
 valeur limite autorisée :%(r2)f
"""),

56: _("""
 Valeur hors domaine de validité :
 diam ext du tube avant transition:%(r1)f
 valeur minimum autorisée : 77.
 valeur maximum autorisée : 355.
"""),

57: _("""
Seuls gibi98 et gibi2000 sont appelables.
"""),

58: _("""
Une interpénétration des lèvres est détectée pour le numéro d'ordre %(i1)d : sur les
%(i2)d noeuds de chaque lèvre, %(i3)d noeuds s'interpénètrent.
-> Risque et Conseil :
Le contact n'est pas pris en compte dans le calcul. Le taux de restitution de l'énergie G
est donc positif y compris là où la fissure tend à se refermer, ce qui peut conduire à
des résultats trop pénalisants.
Pour prendre en compte le contact entre les lèvres, il faut lancer le calcul hors macro.
"""),

}
