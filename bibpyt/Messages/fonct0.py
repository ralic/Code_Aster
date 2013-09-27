# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {

1 : _(u"""
Le fichier %(k1)s existe déjà, on écrit à la suite.
"""),

2 : _(u"""
Il n'y a pas de règles d'interpolation pour LIST_PARA/LIST_RESU,
LIST_PARA/LIST_RESU ne peut donc apparaître qu'une seule fois
et à la première occurrence de COURBE.
"""),

3 : _(u"""
LIST_PARA et LIST_RESU n'ont pas la même taille.
"""),

4 : _(u"""
FONC_X/FONC_Y ne peuvent pas être des nappes !
"""),

5 : _(u"""
Au format 'TABLEAU', FONC_X/FONC_Y ne peut apparaître qu'une seule fois
et à la première occurrence de COURBE
"""),

6 : _(u"""
Il n'y a pas de règles d'interpolation pour ABSCISSE/ORDONNEE,
ABSCISSE/ORDONNEE ne peut donc apparaître qu'une seule fois
et à la première occurrence de COURBE.
"""),

7 : _(u"""
ABSCISSE et ORDONNEE n'ont pas la même taille.
"""),

8 : _(u"""
Format inconnu : %(k1)s
"""),

9 : _(u"""
Erreur lors de l'interpolation de la fonction '%(k1)s'.
"""),

10 : _(u"""sur la maille '%(k1)s'
"""),

11 : _(u"""
L'interpolation de la fonction '%(k1)s' n'est pas autorisée.
Le type d'interpolation de la fonction vaut 'NON'

  -> Risque & Conseil :
    Voir le mot-clé INTERPOL des commandes qui créent des fonctions.
"""),

12 : _(u"""
Une erreur s'est produite dans la recherche de l'intervalle des abscisses contenant la valeur %(r1)f.

  -> Risque & Conseil :
    Vérifiez que le type d'interpolation de la fonction ne vaut pas 'NON'
    (mot-clé INTERPOL des commandes qui créent des fonctions).
"""),

13 : _(u"""
Le type de la fonction '%(k1)s' est inconnu.
Seules les fonctions, nappes, fonctions constantes peuvent être traitées par %(k3)s.

  -> Débogage :
      le type est '%(k2)s'
"""),

14 : _(u"""
Il n'y a pas assez de paramètres pour évaluer la fonction.
Seulement %(i1)d paramètre(s) sont fourni(s) alors que la fonction en réclame %(i2)d.
"""),

15 : _(u"""
Il y a des doublons dans la liste des paramètres fournis :
   %(ktout)s
"""),

16 : _(u"""
Les paramètres nécessaires sont :
   %(ktout)s
"""),

17 : _(u"""
Les paramètres fournis sont :
   %(ktout)s
"""),

18 : _(u"""
La fonction n'a même pas un point !
"""),

19 : _(u"""
On est hors du domaine de définition de la fonction.
On ne peut pas interpoler la fonction pour cette abscisse car le prolongement à gauche est exclus.
   abscisse demandée              : %(r1)f
   borne inférieure des abscisses : %(r2)f

  -> Risque & Conseil :
    Voir le mot-clé PROL_GAUCHE des commandes qui créent des fonctions.
"""),

20 : _(u"""
On est hors du domaine de définition de la fonction.
On ne peut pas interpoler la fonction pour cette abscisse car le prolongement à droite est exclus.
   abscisse demandée              : %(r1)f
   borne supérieure des abscisses : %(r2)f

  -> Risque & Conseil :
    Voir le mot-clé PROL_DROITE des commandes qui créent des fonctions.
"""),

21 : _(u"""
Erreur de programmation : type d'extrapolation inconnu.

  -> Débogage :
      le type d'extrapolation est '%(k1)s'
"""),

22 : _(u"""
La fonction n'est définie qu'en un point. On ne peut pas l'interpoler en
plus d'un point si le prolongement n'est pas constant des deux cotés.

  -> Risque & Conseil :
    Voir les mots-clés PROL_GAUCHE/PROL_DROITE des commandes qui créent des fonctions.
"""),

23 : _(u"""
La fonction n'est définie qu'en un point. On ne peut pas l'interpoler ailleurs
qu'en ce point si le prolongement n'est pas constant des deux cotés.

  -> Risque & Conseil :
    Voir les mots-clés PROL_GAUCHE/PROL_DROITE des commandes qui créent des fonctions.
"""),

24 : _(u"""
On attend une fonction d'un seul paramètre.
La fonction '%(k1)s' est une fonction de %(i1)d paramètres.
"""),

25 : _(u"""
Le type de la fonction '%(k1)s' est inconnu.
Seules les fonctions, nappes, fonctions constantes et formules sont
traitées par %(k3)s.

  -> Débogage :
      le type est '%(k2)s'
"""),

26 : _(u"""
   abscisse demandée : %(r1)f
   intervalle trouvé : [%(r2)f, %(r3)f]
"""),

27 : _(u"""
Un problème d'interpolation a été rencontré.
%(k1)s

  -> Risque & Conseil :
      Vérifier les valeurs fournies derrière le mot-clé 'INTERPOL' lors
      de la création de cette(ces) fonction(s).

  -> Débogage :
      %(k2)s
"""),

28 : _(u"""
Un problème concernant le nom des abscisses ou ordonnées a été rencontré.
Vous ne pouvez pas faire la transformée de Fourier d'une fonction dont les abscisses sont des fréquences,
   ou si la fonction est a valeurs complexes
Vous ne pouvez pas faire la transformée de Fourier inverse d'une fonction dont les abscisses sont des instants,
   ou si la fonction est a valeur réelle.
%(k1)s

  -> Risque & Conseil :
      Vérifier la valeur fournie derrière les mots-clés 'NOM_PARA'/'NOM_RESU' lors
      de la création de cette(ces) fonction(s).

  -> Débogage :
      %(k2)s
"""),

29 : _(u"""
Un problème concernant le prolongement de la (des) fonction(s) a été rencontré.
%(k1)s

  -> Risque & Conseil :
      Vérifier la valeur fournie derrière les mots-clés 'PROL_GAUCHE'/'PROL_DROITE'
      lors de la création de cette(ces) fonction(s).

  -> Débogage :
      %(k2)s
"""),

30 : _(u"""
Une erreur s'est produite lors de l'opération.
%(k1)s

  -> Débogage :
      %(k2)s

Remontée d'erreur (pour aider à l'analyse) :

%(k3)s

"""),

31 : _(u"""
   Génération par défaut de 3 amortissements :[%(r1)f,%(r2)f,%(r3)f]
"""),

32 : _(u"""
   Génération par défaut de 150 fréquences :
   %(k1)s
"""),

33 : _(u"""
   SPEC_OSCI, la norme ne peut être nulle.
"""),

34 : _(u"""
   SPEC_OSCI, le type de la fonction doit être ACCE ou DSP.
"""),

35 : _(u"""
   SPEC_OSCI, il faut choisir METHODE='RICE' pour une DSP.
"""),

36 : _(u"""
   SPEC_OSCI, la méthode choisie suppose des amortissements sous critiques,
   (inférieurs à 1).
"""),

37 : _(u"""
 calcul du MAX, la liste de fonctions n'est pas
 homogène en type (fonctions et nappes)
"""),

38 : _(u"""
 Calcul du MAX, la liste de fonctions n'est pas homogène
 en label NOM_PARA :%(k1)s
"""),

39 : _(u"""
 Calcul du MAX, la liste de fonctions n'est pas homogène
 en label NOM_RESU :%(k1)s
"""),

40 : _(u"""
 Intensité spectrale, avant de calculer l'intensité spectrale,
 il est prudent de vérifier la norme de la nappe sur laquelle
 porte le calcul, ceci peut être une source d'erreurs.
"""),

41 : _(u"""
 Le fichier %(k1)s est introuvable.
"""),

42 : _(u"""
Erreur lors de la lecture des blocs de valeurs :
   %(k1)s
"""),

43 : _(u"""
Les fréquences doivent être strictement positives.
"""),

44 : _(u"""
Les abscisses de la fonction %(k1)s ne sont pas strictement croissantes.
"""),

45 : _(u"""
Les abscisses de la fonction %(k1)s ne sont pas croissantes.
"""),

46 : _(u"""
Les abscisses de la fonction %(k1)s ne sont pas décroissantes.
"""),

47 : _(u"""
Les abscisses de la fonction %(k1)s ne sont pas strictement décroissantes.
"""),

48 : _(u"""
La fonction ou formule ne doit avoir qu'une ou deux variables.
"""),

49 : (u"""
La nappe ou formule a deux paramètres. Il faut renseigner le mot-clé NOM_PARA_FONC
et soit VALE_PARA_FONC, soit LIST_PARA_FONC.
"""),

50 : _(u"""
Seules les formules à une variable peuvent être traitées directement par IMPR_FONCTION.

La formule '%(k1)s' dépend de %(i1)d paramètres.

  -> Risque & Conseil :
      - Si votre formule dépend de 2 paramètres, utilisez CALC_FONC_INTERP pour produire
        une nappe puis appeler IMPR_FONCTION.
      - Si votre formule dépend de 3 paramètres ou plus, vous devez d'abord créer une
        nouvelle formule à un seul paramètre (et appelé IMPR_FONCTION) ou à 2 paramètres
        et passer par CALC_FONC_INTERP puis IMPR_FONCTION.
"""),

52 : _(u"""
Conseils :
  Si le problème reporté ci-dessus ressemble à 'NameError: 'XXX'...',
  vérifiez que le paramètre 'XXX' fait bien partie des paramètres de définition de
  la formule (mot clé FORMULE / NOM_PARA).
"""),

53 : _(u"""sur le noeud '%(k1)s'
"""),

54 : (u"""
Nombre de paramètres fournis : %(i1)d
Noms des paramètres fournis  : %(ktout)s
"""),

55 : _(u"""
  La liste des bornes de l'intervalle n'est pas cohérente.
  Elle doit comporter un nombre pair de valeurs.
"""),

56 : _(u"""
  La borne inférieurs doit être inférieure à la borne supérieure.
  Veuillez revoir la saisie du mot-clé INTERVALLE.
"""),

57 : _(u"""
Le polynôme est de la forme :
    a[0] x^N + a[1] x^(N-1) + a[2] x^(N-2) + ... + a[N]

avec :
   %(k1)s

"""),

58 :_(u"""
Erreur lors de la vérification des noms des paramètres.
Le nom du premier paramètre de la formule en entrée (%(k1)s) est '%(k2)s'.

Or vous avez demandé à créer une nappe avec NOM_PARA='%(k3)s'.
"""),

59 :_(u"""
Erreur lors de la vérification des noms des paramètres.
Le nom du paramètre de la nappe en entrée (%(k1)s) est '%(k2)s'.

Or vous avez demandé à créer une nappe avec NOM_PARA='%(k3)s'.
"""),

60 :_(u"""
Erreur lors de la vérification des noms des paramètres.
Le nom du deuxième paramètre de la formule en entrée (%(k1)s) est '%(k2)s'.

Or vous avez demandé à créer une nappe avec NOM_PARA_FONC='%(k3)s'
"""),

61 :_(u"""
Erreur lors de la vérification des noms des paramètres.
Le nom du paramètre des fonctions de la nappe en entrée (%(k1)s) est '%(k2)s'.

Or vous avez demandé à créer une nappe avec NOM_PARA_FONC='%(k3)s'
"""),

62 : _(u"""
Création de la fonction '%(k1)s'.
"""),

63 : _(u"""
Création d'une fonction de la nappe '%(k1)s'.
"""),

64 : _(u"""
Les abscisses ne sont pas strictement monotones.
"""),

65 : _(u"""
Les abscisses ont été réordonnées.
"""),

66 : _(u"""
L'ordre des abscisses a été inversé.
"""),

67 : _(u"""
Le nombre de valeurs est différent du nombre de paramètres
"""),

68 : _(u"""
Les paramètres de la formule n'ont pas été fournis.
Paramètres manquants : %(k1)s
"""),

69 : _(u"""
Certains paramètres de la formule ont été fournis plusieurs fois.
Paramètres répétés : %(k1)s
"""),

70 : _(u"""
Erreur lors de l'évaluation de la formule.
La remontée d'erreur suivante peut aider à comprendre où se situe l'erreur :
%(k1)s
"""),

}
