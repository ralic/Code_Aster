#@ MODIF utilitai5 Messages  DATE 02/06/2008   AUTEUR PELLET J.PELLET 
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
 asin/acos svp
"""),

3 : _("""
 selection de ddl : choix < %(k1)s > inconnu
"""),

4 : _("""
 argument d'appel invalide :  typf =  %(k1)s
"""),

5 : _("""
 argument d'appel invalide :  acces =  %(k1)s
"""),

6 : _("""
 argument d'appel invalide :  autor =  %(k1)s
"""),

7 : _("""
 redefinition de l'unite logique  %(k1)s  non autorisee
"""),

8 : _("""
 nombre maximum d'unites logiques ouvertes atteint  %(k1)s
"""),

9 : _("""
 argument d'appel invalide :  unit =  %(k1)s
"""),

10 : _("""
 aucun numero d'unite logiquedisponible
"""),

11 : _("""
 unite logique  %(k1)s  associee au nom  %(k2)s  et au fichier  %(k3)s
"""),

12 : _("""
 vous devez d'abord le fermer pour l'associer au nom  %(k1)s
"""),

13 : _("""
 unite logique  %(k1)s  deja utilisee en acces  %(k2)s  par le fichier  %(k3)s
"""),

14 : _("""
 vous devez d'abord le fermer
"""),

15 : _("""
 unite logique  %(k1)s  deja utilisee en mode binaire par le fichier  %(k2)s
"""),

16 : _("""
 vous devez d'abord fermer le fichier associe
"""),

17 : _("""
 unite logique  %(k1)s  deja utilisee par le fichier  %(k2)s  associee au nom  %(k3)s
"""),

18 : _("""
 unite logique  %(k1)s , probleme lors de l'open  %(k2)s
"""),

19 : _("""
 unite logique  %(k1)s , probleme lors du positionnement
"""),

20 : _("""
 unite logique  %(k1)s , probleme lors de l'inquire
"""),

21 : _("""
 nombre d'unites logiques ouvertes superieur a //k4b
"""),

22 : _("""
 unite logique  %(k1)s , probleme lors du close de la reservation.
"""),

23 : _("""
 la redefinition de l'unite logique  %(k1)s  n'est pas autorisee
"""),

24 : _("""
 type d'acces inconnu " %(k1)s ", unite  %(k2)s
"""),

25 : _("""
 fichier non nomme, unite  %(k1)s
"""),

26 : _("""
 fichier non ouvert, unite  %(k1)s
"""),

27 : _("""
 rewind impossible, unite  %(k1)s
"""),

28 : _("""
 positionnement inconnu " %(k1)s ", unite  %(k2)s
"""),

29 : _("""
 les champs de type " %(k1)s " sont interdits.(a faire ...)
"""),

30 : _("""
 composante  %(k1)s inexistante pour la grandeur  %(k2)s
"""),

31 : _("""
 la maille: %(k1)s n'appartient pas au maillage: %(k2)s
"""),

32 : _("""
 le champ: %(k1)s n'est pas un champ par elements aux noeuds.
"""),

34 : _("""
 la maille: %(k1)s n'est pas affectee dans le ligrel: %(k2)s
"""),

35 : _("""
 la maille:  %(k1)s  possede un type d'element ignorant le cham_elem teste.
"""),

36 : _("""
 num. de sous-point > max
"""),

37 : _("""
 num. de point > max
"""),

38 : _("""
 l'element n'admet pas la composante  %(k1)s
"""),

39 : _("""
 determination de la localisation des points de gauss
"""),

40 : _("""
 LIRE_RESU ne sait pas lire les structures de données de type %(k1) s
"""),

41 : _("""
 xous :  %(k1)s  non prevu.
"""),

42 : _("""
 chaine sch1 trop longue >24
"""),

43 : _("""
 ipos hors de l intervalle (0 24)
"""),

44 : _("""
 longueur totale > 24
"""),

45 : _("""
 on demande un nombre de composantes negatif pour  %(k1)s
"""),

46 : _("""
 on demande des composantes inconnues pour  %(k1)s
"""),

47 : _("""
 mot-clef :  %(k1)s  inconnu.
"""),

48 : _("""
 composante inexistante dans le champ:  %(k1)s
"""),

49 : _("""
 type de champ non traite:  %(k1)s
"""),

52 : _("""
 mauvaise valeur pour fonree
"""),

53 : _("""
 pas de composantes
"""),

54 : _("""
 l"argument "indi" est non valide
"""),

55 : _("""
 l"appel a uttcpu ne peut etre effectue avec la valeur "debut" pour l"argument para
"""),

56 : _("""
 l"appel a uttcpu ne peut etre effectue avec la valeur "fin" pour l"argument para
"""),

57 : _("""
 l"appel a uttcpu ne peut etre effectue avec la valeur  %(k1)s  pour l"argument para
"""),

58 : _("""
 (uttrif) type de fonction non connu.
"""),

59 : _("""
 il existe au moins un noeud qui n appartient pas au groupe de mailles.
"""),

60 : _("""
 un sous-domaine  est non-connexe
"""),

88 : _("""
 L'option " %(k1)s " est à recalculer
"""),

89 : _("""
 Erreur de programmation : contacter l'assistance
"""),

90 : _("""
 On ne trouve pas le VALE_PARA_FONC exact dans la liste de la nappe
"""),

91 : _("""
 %(k1)s n'est pas un champ de résultat.
"""),

92 : _("""
 Interpolation LOG et complexe en ordonnées sont incompatibles !
"""),

93 : _("""
 Le stockage dans la SD résultat du modèle, du champ materiau 
 et des caractéristiques élémentaires  n'est pas possible 
 pour une SD résultat de type %(k1)s. 
"""),

94 : _("""
 Le stockage de la SD charge dans la SD résultat n'est pas
 possible pour une SD résultat de type %(k1)s. 
"""),

95 : _("""
  le noeud %(k1)s ne supporte pas la composante %(k2)s 
"""),

96 : _("""
  le noeud %(k1)s le noeud %(k2)s ne supporte pas la composante %(k3)s 
"""),

97 : _("""
 le type de champ  %(k1)s n''est pas accepte.  %(k2)s 
 veuillez consulter la %(k3)s 
 doc u correspondante %(k4)s 
"""),

}
