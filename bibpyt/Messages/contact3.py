#@ MODIF contact3 Messages  DATE 16/10/2007   AUTEUR REZETTE C.REZETTE 
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

10 : _("""
La matrice est singulière lors du calcul du repère local tangent au noeud maitre  %(k1)s  sur la maille maitre %(k2)s.
Une erreur de définition de la maille: les vecteurs tangents sont colinéaires.
"""),

11 : _("""
La matrice est singulière lors de la projection du point de contact  sur la maille maitre  %(k1)s.
Une erreur de définition de la maille: les vecteurs tangents sont colinéaires.
"""),

12 : _("""
L'algorithme de Newton a échoué lors du calcul du repère local tangent au noeud maitre %(k1)s sur la maille maitre  %(k2)s.
Erreur de définition de la maille ou projection difficile. Contactez l'assistance dans ce dernier cas. 
"""),

13 : _("""
L'algorithme de Newton a échoué lors de la projection du point de contact  sur la maille maitre  %(k1)s.
Erreur de définition de la maille ou projection difficile. Contactez l'assistance dans ce dernier cas.  
"""),

14 : _("""
Les vecteurs tangents sont nuls au noeud maitre  %(k1)s  sur la maille maitre  %(k2)s.
Une erreur de définition de la maille.
"""),

15 : _("""
Contact méthode continue.
Le vecteur DIRE_APPA est nul !
"""),

21 : _("""
Les vecteurs tangents sont nuls lors de la projection du point de contact sur la maille maitre  %(k1)s. 
Une erreur de définition de la maille.
"""),

22 : _("""
L'élement inconnu sur la maille maitre  %(k1)s.
Cet élément n'est pas programmé pour le contact avec formualtion continue.
Contactez l'assistance. 
"""),

23 : _("""
Le vecteur normal est nul sur le noeud maitre  %(k1)s.
Si vous avez activé le lissage, essayeé de le désactiver. 
"""),

24 : _("""
Il y a plus de trois noeuds exclus sur la maille esclave  %(k1)s  par l'option SANS_GROUP_NO ou SANS_NOEUD.
Supprimer directement la maille esclave de la définition de la surface.
"""),

25 : _("""
L'élément porté par la maille esclave %(k1)s n'est pas du bon type pour un fond de fissure, elle est de type  %(k2)s 
"""),

26 : _("""
Schema d'intégration inconnu sur la maille  %(k1)s. 
"""),

27 : _("""
Code erreur introuvable. Contactez les développeurs.
"""),

28 : _("""
Le statut du contact a changé %(i1)d fois au point de contact numéro %(i2)d sur la maille esclave %(k1)s
Présence de flip-flop. 
"""),

35 : _("""
Contact méthode continue. 
  -> La normale calculée sur une maille est nulle.
  -> Risque & Conseil :
     Vérifier votre maillage.
"""),

42 : _("""
Contact méthodes discrètes. Il y a mélange de mailles quadratiques (TRIA6, TRIA7 ou QUAD9) avec des mailles QUAD8 sur la surface escalve du contact.
On supprime la liaison entre les noeuds sommets et noeud milieu sur le QUAD8.
Il y a risque d'interpénétration du noeud milieu pour le QUAD8 considéré.
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

94 : _("""
Contact méthode discrète. 
 Le jeu entre les noeuds milieux au niveau des lèvres risque
 d'etre imprécis si les mailles en contact sont quadratiques.
"""),

}
