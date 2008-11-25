#@ MODIF fonct0 Messages  DATE 25/11/2008   AUTEUR DURAND C.DURAND 
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
Le fichier %(k1)s existe déjà, on écrit à la suite.
"""),

2 : _("""
Il n'y a pas de règles d'interpolation pour LIST_PARA/LIST_RESU,
LIST_PARA/LIST_RESU ne peut donc apparaitre qu'une seule fois
et à la première occurence de COURBE.
"""),

3 : _("""
LIST_PARA et LIST_RESU n'ont pas la meme taille.
"""),

4 : _("""
FONC_X/FONC_Y ne peuvent pas etre des nappes !
"""),

5 : _("""
Au format 'TABLEAU', FONC_X/FONC_Y ne peut apparaitre qu'une seule fois
et à la première occurence de COURBE
"""),

6 : _("""
Il n'y a pas de règles d'interpolation pour ABSCISSE/ORDONNEE,
ABSCISSE/ORDONNEE ne peut donc apparaitre qu'une seule fois
et à la première occurence de COURBE.
"""),

7 : _("""
ABSCISSE et ORDONNEE n'ont pas la meme taille.
"""),

8 : _("""
Format inconnu : %(k1)s
"""),

9 : _("""
Erreur lors de l'interpolation de la fonction '%(k1)s'.
"""),

10 : _("""sur la maille '%(k1)s'
"""),

11 : _("""
L'interpolation de la fonction '%(k1)s' n'est pas autorisée.
Le type d'interpolation de la fonction vaut 'NON'
  
  -> Risque & Conseil :
    Voir le mot-clé INTERPOL des commandes qui créent des fonctions.
"""),

12 : _("""
Une erreur s'est produite dans la recherche de l'intervalle des abscisses contenant la valeur %(r1)s.
  
  -> Risque & Conseil :
    Vérifiez que le type d'interpolation de la fonction ne vaut pas 'NON'
    (mot-clé INTERPOL des commandes qui créent des fonctions).
"""),

13 : _("""
Le type de la fonction '%(k1)s' est inconnu.
Seules les fonctions, nappes, fonctions constantes peuvent etre traitées par FOINTE.

  -> Debug :
      le type est '%(k2)s'
"""),

14 : _("""
Il n'y a pas assez de paramètres pour évaluer la fonction.
Seulement %(i1)d paramètre(s) sont fourni(s) alors que la fonction en réclame %(i2)d.
"""),

15 : _("""
Il y a des doublons dans la liste des paramètres fournis :
   %(ktout)s
"""),

16 : _("""
Les paramètres nécessaires sont :
   %(ktout)s
"""),

17 : _("""
Les paramètres fournis sont :
   %(ktout)s
"""),

18 : _("""
La fonction n'a meme pas un point !
"""),

19 : _("""
On est hors du domaine de définition de la fonction.
On ne peut pas interpoler la fonction pour cette abscisse car le prolongement à gauche est exclus.
   abscisse demandée              : %(r1)f
   borne inférieure des abscisses : %(r2)f
  
  -> Risque & Conseil :
    Voir le mot-clé PROL_GAUCHE des commandes qui créent des fonctions.
"""),

20 : _("""
On est hors du domaine de définition de la fonction.
On ne peut pas interpoler la fonction pour cette abscisse car le prolongement à droite est exclus.
   abscisse demandée              : %(r1)f
   borne supérieure des abscisses : %(r2)f
  
  -> Risque & Conseil :
    Voir le mot-clé PROL_DROITE des commandes qui créent des fonctions.
"""),

21 : _("""
Erreur de programmation : type d'extrapolation inconnu.

  -> Debug :
      le type d'extrapolation est '%(k1)s'
"""),

22 : _("""
La fonction n'est définie qu'en un point. On ne peut pas l'interpoler en
plus d'un point si le prolongement n'est pas constant des deux cotés.
  
  -> Risque & Conseil :
    Voir les mot-clés PROL_GAUCHE/PROL_DROITE des commandes qui créent des fonctions.
"""),

23 : _("""
La fonction n'est définie qu'en un point. On ne peut pas l'interpoler ailleurs
qu'en ce point si le prolongement n'est pas constant des deux cotés.
  
  -> Risque & Conseil :
    Voir les mot-clés PROL_GAUCHE/PROL_DROITE des commandes qui créent des fonctions.
"""),

24 : _("""
On attend une fonction d'un seul paramètre.
La fonction '%(k1)s' est une fonction de %(i1)d paramètres.
"""),

25 : _("""
Le type de la fonction '%(k1)s' est inconnu.
Seules les fonctions, nappes, fonctions constantes et formules sont
traitées par FOINTR.

  -> Debug :
      le type est '%(k2)s'
"""),

26 : _("""
   abscisse demandée : %(r1)f
   intervalle trouvé : [%(r2)f, %(r3)f]
"""),

27 : _("""
Un problème d'interpolation a été rencontré. 
%(k1)s

  -> Risque & Conseil :
      Vérifier les valeurs fournies derrière le mot-clé 'INTERPOL' lors
      de la création de cette(ces) fonction(s).
   
  -> Debug :
      %(k2)s
"""),

28 : _("""
Un problème concernant le nom des abscisses ou ordonnées a été rencontré. 
Vous ne pouvez pas faire la transformée de fourier d'une fonction dont les abscisses sont des fréquences,
   ou si la fonction est a valeurs complexes
Vous ne pouvez pas faire la transformée de fourier inverse d'une fonction dont les abscisses sont des instants,
   ou si la fonction est a valeur réelle.
%(k1)s
   
  -> Risque & Conseil :
      Vérifier la valeur fournie derrière les mots-clés 'NOM_PARA'/'NOM_RESU' lors
      de la création de cette(ces) fonction(s).
   
  -> Debug :
      %(k2)s
"""),

29 : _("""
Un problème concernant le prolongement de la (des) fonction(s) a été rencontré.
%(k1)s
   
  -> Risque & Conseil :
      Vérifier la valeur fournie derrière les mots-clés 'PROL_GAUCHE'/'PROL_DROITE'
      lors de la création de cette(ces) fonction(s).
   
  -> Debug :
      %(k2)s
"""),

30 : _("""
Une erreur s'est produite lors de l'opération.
%(k1)s
   
  -> Debug :
      %(k2)s

Remontée d'erreur (pour aider à l'analyse) :

%(k3)s

"""),

31 : _("""
   Génération par défaut de 3 amortissements :[%(r1)f,%(r2)f,%(r3)f]
"""),

32 : _("""
   Génération par défaut de 150 fréquences :
   %(k1)s
"""),

33 : _("""
   SPEC_OSCI, la norme ne peut etre nulle.
"""),

34 : _("""
   SPEC_OSCI, le type de la fonction doit etre ACCE.
"""),

35 : _("""
   SPEC_OSCI, seule la méthode NIGAM est codée.
"""),

36 : _("""
   SPEC_OSCI, la méthode choisie suppose des amortissements sous-critiques,
   amor<1.
"""),

37 : _("""
 calcul du MAX, la liste de fonctions n'est pas 
 homogène en type (fonctions et nappes)
"""),

38 : _("""
 Calcul du MAX, la liste de fonctions n'est pas homogène
 en label NOM_PARA :%(k1)s
"""),

39 : _("""
 Calcul du MAX, la liste de fonctions n'est pas homogène
 en label NOM_RESU :%(k1)s 
"""),

40 : _("""
 Intensite spectrale, avant de calculer l'intensite spectrale, 
 il est prudent de verifier la norme de la nappe sur laquelle 
 porte le calcul, ceci peut etre une source d erreurs.
"""),

41 : _("""
 Le fichier %(k1)s est introuvable.
"""),

42 : _("""
Erreur lors de la lecture des blocs de valeurs :
   %(k1)s
"""),

43 : _("""
Les fréquences doivent etre strictement positives.
"""),

44 : _("""
Les abscisses de la fonction %(k1)s ne sont pas strictement croissantes.
"""),

45 : _("""
Les abscisses de la fonction %(k1)s ne sont pas croissantes.
"""),

46 : _("""
Les abscisses de la fonction %(k1)s ne sont pas décroissantes.
"""),

47 : _("""
Les abscisses de la fonction %(k1)s ne sont pas strictement décroissantes.
"""),

48 : _("""
La fonction ou formule ne doit avoir qu'une ou deux variables.
"""),

49 : ("""
La nappe ou formule a deux paramètres. Il faut renseigner le mot-clé NOM_PARA_FONC
et soit VALE_PARA_FONC, soit LIST_PARA_FONC.
"""),

50 : _("""
Seules les formules à une variable peuvent être traitées directement par IMPR_FONCTION.

La formule '%(k1)s' dépend de %(i1)d paramètres.

  -> Risque & Conseil :
      - Si votre formule dépend de 2 paramètres, utilisez CALC_FONC_INTERP pour produire
        une nappe puis appeler IMPR_FONCTION.
      - Si votre formule dépend de 3 paramètres ou plus, vous devez d'abord créer une
        nouvelle formule à un seul paramètre (et appelé IMPR_FONCTION) ou à 2 paramètres
        et passer par CALC_FONC_INTERP puis IMPR_FONCTION.
"""),

51 : _("""
Erreur lors de l'interprétation de la formule '%(k1)s'.
"""),

52 : _("""
%(k1)s
"""),

53 : _("""sur le noeud '%(k1)s'
"""),

54 : ("""
Nombre de paramètres fournis : %(i1)d
Noms des paramètres fournis  : %(ktout)s
"""),

}
