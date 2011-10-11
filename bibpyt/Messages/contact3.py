#@ MODIF contact3 Messages  DATE 11/10/2011   AUTEUR DESOZA T.DESOZA 
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

def _(x) : return x

cata_msg = {

1 : _("""
Formulation continue.
Vous avez activé le frottement de Coulomb (FROTTEMENT='COULOMB') pourtant toutes les zones de contact
portent un coefficient de frottement nul.

Le frottement est donc désactivé.

Conseil : vérifiez que vous avez correctement défini le coefficient de frottement (mot-clé COULOMB) dans chaque zone. 
"""),

2 : _("""
  La maille de fond de fissure de type POI1, introduite par le mot-clef MAILLE_FOND ou GROUP_MA_FOND,
ne correspond pas à une extrémité du segment toucahnt le fond de fisssure. 
  
"""),

3 : _("""
  Pour la formulation de contact < %(k1)s > le couple :
  ALGO_CONT : < %(k2)s >
  ALGO_FROT : < %(k3)s >  
  n'est pas permis.
  
  Conseil : consultez la documentation pour connaître les couples autorisés.
"""),

4 : _("""
  Le mot-clef < %(k1)s > doit avoir la meme valeur sur toutes les zones
  de contact
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
Avec un FOND_FISSURE, il est impossible d'utiliser les options suivantes:
- LISSAGE = 'OUI' ou/et
- NORMALE = 'ESCL' ou
- NORMALE = 'MAIT_ESCL'
"""),

18 : _("""
Contact méthode continue.
La direction d'exclusion du frottement fournie pour la zone de contact numéro %(i1)s (%(r1)s,%(r2)s,%(r3)s) est perpendiculaire au plan de contact sur la maille %(k1)s.

Conseil :
   - Vérifiez le vecteur DIRE_EXCL_FROT. Sa projection sur le plan tangent de contact doit exister
     pour indiquer une direction à exclure.

"""),

23 : _("""
Le vecteur normal est nul au niveau du projeté du point de contact de coordonnées
  (%(r1)s,%(r2)s,%(r3)s) 
sur la maille %(k1)s, 
Erreur de définition de la maille ou projection difficile. Contactez l'assistance dans ce dernier cas.
"""),

24 : _("""
Le vecteur normal est nul sur la maille %(k1)s, 
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

31 : _("""
Les vecteurs tangents sont nuls au niveau quand on projette le noeud esclave
%(k1)s sur la maille %(k2)s
"""),

32: _("""
 Le mot-clef DIST_POUT ne fonctionne qu'avec des sections circulaires définies dans AFFE_CARA_ELEM.
 """),

35 : _("""
Les vecteurs tangents sont nuls au niveau quand on projette le noeud esclave
%(k1)s sur la noeud maitre %(k2)s
"""),


37: _("""
 La section de la poutre n'est pas constante sur l'élément. On prend la moyenne.
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
La normale que vous avez prédéfinie par (VECT_* = 'VECT_Y') sur la maille %(k1)s n'est pas utilisable en 2D.
Utilisez plutôt VECT_* = 'FIXE'
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

61 : _("""
La maille %(k1)s est de type poutre, elle nécessite la définition d'une base locale.
Utilisez NORMALE='FIXE' ou NORMALE='VECT_Y' dans DEFI_CONTACT.
"""),

75 : _("""
La maille %(k1)s est de type 'POI1', elle ne peut pas être une maille maître.
"""),

81 : _("""
Contact.
  -> Il y a trop de réactualisations géométriques.
  -> Conseils :
     - Augmentez le mot-clé ITER_GEOM_MAXI dans la commande DEFI_CONTACT.
     - Vérifiez votre maillage (orientation des surfaces, définition des zones de contact).
     - Découpez plus finement le pas de temps.
"""),

85 : _("""
Contact méthode continue. 
  -> Il y a échec de la boucle des contraintes actives lors du traitement du contact.
  -> Conseil :
     - Augmentez le mot-clé ITER_CONT_MAXI (ou ITER_CONT_MULT) dans la commande DEFI_CONTACT.
"""),

86 : _("""
Contact méthode continue. 
  -> Il y a convergence forcée sur la boucle des contraintes actives lors du traitement du contact.
  -> Risque & conseil :
     La convergence forcée sur les statuts de contact se déclenche lorsque le problème a du mal à converger.
     Il y a des risques que le problème soit un peu moins bien traité.
     Vérifiez bien que vous n'avez pas d'interpénétration au niveau des zones de contact.
     S'il y a des interpénétrations intempestives, découpez plus finement le pas de temps."""),

87 : _("""
Contact méthode continue. 
  -> Il y a trop de réactualisations pour le seuil de frottement.
  -> Conseils :
     - Augmentez le mot-clé ITER_FROT_MAXI dans la commande DEFI_CONTACT.
     - Découpez plus finement le pas de temps."""),


96 : _("""
Contact.
    -> Les surfaces en contact ont bougé de plus de 1%% depuis la dernière réactualisation.
       Or vous n'avez pas activé la réactualisation géométrique automatique dans la commande DEFI_CONTACT
       (REAC_GEOM='AUTOMATIQUE') ou bien vous utilisez le mode 'CONTROLE'
    -> Risque & conseil : 
       Vos résultats risquent d'être faux, les mailles ne seront peut-être pas correctement appariées
       et des interpénétrations pourraient apparaître.
       Si vous avez volontairement négligé la non-linéarité géométrique de contact (pour des raisons
       de performance), nous vous invitons à vérifier visuellement qu'il n'y a effectivement
       pas d'interpénétrations.
"""),

97 : _("""
Contact formulation continue.
    -> Le seuil de frottement a bougé de plus de 1%% depuis la dernière réactualisation.
       Or vous utilisez la réactualisation contrôlée (REAC_FROT='CONTROLE') dans la commande DEFI_CONTACT.
    -> Risque & Conseil :
       Vos résultats risquent d'etre faux, le seuil de Coulomb ne sera peut être pas le bon
       et le frottement pas bien pris en compte.
       Si vous avez volontairement négligé la non-linéarité de frottement (pour des raisons
       de performance), nous vous invitons à vérifier la validité de vos résultats.
"""),

}
