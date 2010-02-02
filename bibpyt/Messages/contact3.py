#@ MODIF contact3 Messages  DATE 02/02/2010   AUTEUR DESOZA T.DESOZA 
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

cata_msg = {

2 : _("""
  La maille de fond de fissure de type POI1, introduite par le mot-clef MAILLE_FOND ou GROUP_MA_FOND,
ne correspond pas à une extrémité du segment toucahnt le fond de fisssure. 
  
"""),

3 : _("""
  Pour la formulation de contact < %(k1)s > le couple:
  ALGO_CONT: < %(k2)s >
  ALGO_FROT: < %(k3)s >  
  N'est pas possible.
"""),

4 : _("""
  Le mot-clef < %(k1)s > doit avoir la meme valeur sur toutes les zones
  de contact
"""),


13 : _("""
L'algorithme de Newton a échoué lors de la projection du point de contact de coordonnées
  (%(r1)s,%(r2)s,%(r3)s)
sur la maille %(k1)s.
Erreur de définition de la maille ou projection difficile. Contactez l'assistance dans ce dernier cas.  
"""),

14 : _("""
Les vecteurs tangents sont nuls au niveau du projeté du point de contact de coordonnées
  (%(r1)s,%(r2)s,%(r3)s) 
sur la maille %(k1)s, 
Erreur de définition de la maille ou projection difficile. Contactez l'assistance dans ce dernier cas.
"""),

15 : _("""
La direction d'appariement fixe données par le vecteur DIRE_APPA est nulle !
"""),

16 : _("""
Contact méthode continue.
La méthode d'intégration n'est pas NOEUD, le champ VALE_CONT n'est pas créé.
"""),

17 : _("""
Contact méthode continue.
Avec un FOND-FISSURE, il est impossible d'utiliser les options suivantes:
- LISSAGE = 'OUI' ou/et
- NORMALE = 'ESCL' ou
- NORMALE = 'MAIT_ESCL'
"""),


23 : _("""
Le vecteur normal est nul au niveau du projeté du point de contact de coordonnées
  (%(r1)s,%(r2)s,%(r3)s) 
sur la maille %(k1)s, 
Erreur de définition de la maille ou projection difficile. Contactez l'assistance dans ce dernier cas.
"""),

26 : _("""
Le vecteur normal est nul au niveau du noeud %(k1)s.
"""),

27 : _("""
Le vecteur normal est nul au niveau de la maille %(k1)s.
"""),

28 : _("""
Le statut du contact a changé %(i1)d fois au point de contact numéro %(i2)d sur la maille esclave %(k1)s
Présence de flip-flop. 
"""),

29 : _("""
Nom de la maille : %(k1)s 
"""),

30 : _("""
Le couple de surfaces de contact %(i1)s pour l'appariement nodal est mal défini.
Il faut moins de noeuds esclaves que de noeuds maitres pour respecter l'injectivité.
Or ici:
Nombre de noeuds maitres : %(i2)s
Nombre de noeuds esclaves: %(i3)s
Conseil: intervertissez les deux surfaces maitres et esclaves
"""),

31 : _("""
Les vecteurs tangents sont nuls au niveau quand on projette le noeud esclave
%(k1)s sur la maille %(k2)s
"""),

32: _("""
 Le mot-clef DIST_POUT ne fonctionne qu'avec des sections circulaires définies dans AFFE_CARA_ELEM.
 """),

33 : _("""
L'erreur suivante est arrivée lors du pré-calcul des normales aux noeuds activées par les options
- LISSAGE = 'OUI' ou/et
- NORMALE = 'ESCL' ou
- NORMALE = 'MAIT_ESCL'
"""),

34 : _("""
Echec de l'orthogonalisation du repère tangent construit au niveau du projeté du point de contact de coordonnées
  (%(r1)s,%(r2)s,%(r3)s) 
sur la maille %(k1)s, 
Erreur de définition de la maille ou projection difficile. Contactez l'assistance dans ce dernier cas.
"""),

35 : _("""
Les vecteurs tangents sont nuls au niveau quand on projette le noeud esclave
%(k1)s sur la noeud maitre %(k2)s
"""),

36 : _("""
La maille %(k1)s est de type 'POI1', ce n'est pas autorisé sur une maille maitre. 
"""),

37: _("""
 La section de la poutre n'est pas constante sur l'élément. On prend la moyenne.
 """),

38 : _("""
La maille %(k1)s est de type poutre et sa tangente est nulle.
Vérifiez votre maillage.
"""),

39: _("""
Problème pour récupérer l'épaisseur de la coque pour la maille  %(k1)s
"""),

40: _("""
L'excentricité de la coque pour la maille %(k1)s ne peut pas etre traitée
"""),

41: _("""
Problème pour récupérer l'excentricité de la coque pour la maille  %(k1)s
"""),

43 : _("""
La normale que vous avez prédéfinie par (VECT_* = 'VECT_Y') n'est pas utilisable en 2D.
Utilisez plutot (ou Dingo) VECT_* = 'FIXE'
"""),


50: _("""
Avec l'option VECT_MAIT = 'FIXE', seule l'option NORMALE = 'MAIT' est possible.
"""),

51: _("""
Avec l'option VECT_MAIT = 'VECT_Y', seule l'option NORMALE = 'MAIT' est possible.
"""),

52: _("""
Avec l'option VECT_ESCL = 'FIXE', seule l'option NORMALE = 'ESCL' est possible.
"""),

53: _("""
Avec l'option VECT_ESCL = 'VECT_Y', seule l'option NORMALE = 'ESCL' est possible.
"""),

54: _("""
Le LISSAGE n'est possible qu'avec des normales automatiques VECT_ESCL='AUTO' et/ou VECT_MAIT='AUTO'.
"""),

54: _("""
Le LISSAGE n'est possible qu'avec des normales automatiques VECT_ESCL='AUTO' et/ou VECT_MAIT='AUTO'.
"""),

60 : _("""
La maille %(k1)s est de type 'POI1', elle nécessite l'utilisation de l'option
NORMALE='FIXE' avec une normale non-nulle. 
"""),

81 : _("""
Contact.
  -> Il y a trop de réactualisations géométriques.
  -> Risque & Conseil :
     Le paramètre ITER_GEOM_MAXI est trop faible.
     Votre maillage est incorrect ou le glissement relatif des deux
     surfaces est trop important.
     Tentez de découper plus finement en temps votre problème ou augmenter ITER_GEOM_MAXI.
 
"""),

85 : _("""
Contact méthode continue. 
  -> Il y a échec de la boucle des contraintes actives lors du traitement
     du contact
  -> Risque & Conseil :
     Vérifier votre modèle ou augmenter ITER_CONT_MAXI/ITER_CONT_MULT.
"""),

86 : _("""
Contact méthode continue. 
  -> Il y a convergence forcée sur la boucle des contraintes actives lors du traitement
     du contact.
  -> Risque & Conseil :
     La convergence forcée sur les statuts de contact se déclenche lorsque le problème a du mal à converger.
     Il y a des risques que le problème soit un peu moins bien traité.
     Vérifiez bien que vous n'avez pas d'interpénétration entre les mailles.
     S'il y a des interpénétrations intempestives, tentez de découper plus finement en temps votre problème."""),

87 : _("""
Contact méthode continue. 
  -> Il y a trop de réactualisations pour le seuil de frottement.
  -> Risque & Conseil :
     Le paramètre ITER_FROT_MAXI est trop faible.
     La condition de frottement de Coulomb est peut être mal prise en compte, il y a donc un risque de 
     résultats faux sur les forces d'adhérence.
     Essayez de découper plus finement en temps votre problème."""),


96 : _("""
Contact.
    -> Les surfaces en contact ont bougé de plus de 1%% depuis la dernière réactualisation.
       Or vous n'avez pas activé la réactualisation géométrique (REAC_GEOM) automatique ou
       vous utilisez le mode 'CONTROLE'
    -> Risque & Conseil : Vos résultats risquent d'etre faux, les mailles ne
       seront peut-etre pas correctement appariées et donc la condition de contact sera peut
       etre fausse.
       Si vous avez volontairement négligé la non-linéarité géométrique de contact (pour des raisons
       de performance), nous vous invitons à vérifier visuellement qu'il n'y a effectivement
       pas interpénétration.
"""),

}
