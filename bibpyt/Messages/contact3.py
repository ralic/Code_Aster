#@ MODIF contact3 Messages  DATE 05/05/2009   AUTEUR DESOZA T.DESOZA 
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

1 : _("""
Le mot-clef < %(k1)s > est inconnu dans AFFE_CONTACT. Contactez les développeurs.
Note DVP: erreur de cohérence fortran/catalogue. 
"""),

2 : _("""
Le mot-clef < %(k1)s > n'est pas renseigné dans AFFE_CONTACT alors qu'il est obligatoire. Contactez les développeurs.
Note DVP: erreur de cohérence fortran/catalogue. 
"""),

3 : _("""
L'option < %(k1)s > ne correspond a aucune option permise par le mot-clef < %(k2)s > dans AFFE_CONTACT. Contactez les développeurs.
Note DVP: erreur de cohérence fortran/catalogue.            
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

25 : _("""
L'élément porté par la maille esclave %(k1)s n'est pas du bon type pour un fond de fissure, elle est de type  %(k2)s 
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


38 : _("""
La maille %(k1)s est de type poutre et sa tangente est nulle.
Vérifiez votre maillage.
"""),

39: _("""
Problème pour récupérer l'épaisseur de la coque pour la maille  %(k1)s
"""),

40: _("""
L'excentricité de la coque pour la maille %(k1)s n'est pas traitée
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

85 : _("""
Contact méthode continue. 
  -> Il y a échec de la boucle contraintes actives lors du traitement
     du contact
  -> Risque & Conseil :
     Vérifier votre maillage ou augmenter ITER_CONT_MAX.
"""),

86 : _("""
Contact méthode continue. 
  -> Il y a convergence forcée sur boucle contraintes actives lors du traitement
     du contact.
  -> Risque & Conseil :
     La convergence forcée se déclenche lorsque le problème a du mal à converger. Il y a des risques que le problème 
     soit un peu moins bien traité. Vérifiez bien que vous n'avez pas d'interpénétration entre les mailles. S'il y des 
     interpénétrations intempestives, tentez de découper plus finement en temps votre problème.
"""),

87 : _("""
Contact méthode continue. 
  -> Il y a convergence forcée sur boucle seuil frottement lors du traitement du
     contact.
  -> Risque & Conseil :
     La convergence forcée se déclenche lorsque le problème a du mal à converger. Il y a des risques que le problème 
     soit un peu moins bien traité. La condition de frottement de Coulomb est peut etre mal prise en compte. Risque de 
     résultats faux sur les forces d'adhérence. Essayez de découper plus finement en temps votre problème.
"""),

88 : _("""
Contact méthode continue. 
  -> Il y a convergence forcée sur boucle de géométrie lors du traitement du
     contact.
  -> Risque & Conseil :
     La convergence forcée se déclenche lorsque le problème a du mal à converger
     lors de grands glissements relatifs entre deux surfaces de contact.
     Il y a des risques que le problème soit un peu moins bien traité.
     Vérifiez bien que vous n'avez pas d'interpénétration entre les mailles.
     S'il y des interpénétrations intempestives, tentez de découper plus finement en temps votre problème.
"""),

}
