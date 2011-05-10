#@ MODIF factor Messages  DATE 09/05/2011   AUTEUR TARDIEU N.TARDIEU 
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

cata_msg={


#-----------------------------------------------------------------------------------------------
10: _("""
(Solveur linéaire LDLT ou MULT_FRONT) Matrice non factorisable !
  On sait en plus que:
   - pivot presque nul à la ligne : %(i1)d,
   - nombre de décimales perdues  : %(i2)d.

  -> Conseil & Risque :
     Verifiez votre mise en donneés (absence ou surabondance de conditions limites, 
     caractéristiques matériaux licites...).
     Si vous avez deja validé cette mise en données, vous pouvez essayer a la place le
     solveur linéaire MUMPS (mot-clé SOLVEUR/METHODE='MUMPS').
"""),

#-----------------------------------------------------------------------------------------------
11: _("""
(Solveur linéaire LDLT ou MULT_FRONT) Matrice non factorisable !
  On sait en plus que:
   - pivot vraiment nul à la ligne : %(i1)d.

  -> Conseil & Risque :
     Vérifiez votre mise en données (absence ou surabondance de conditions limites, 
     caractéristiques matériaux licites...).
     Si vous avez deja validé cette mise en données, vous pouvez essayer a la place le
     solveur linéaire MUMPS (mot-clé SOLVEUR/METHODE='MUMPS').    
"""),

#-----------------------------------------------------------------------------------------------
13: _("""
Solveur FETI :
  Le solveur FETI est impossible dans ce contexte.
Solution :
  Il faut changer de solveur.
"""),


#-----------------------------------------------------------------------------------------------
20: _("""
(Solveur linéaire LDLT ou MULT_FRONT) Matrice non factorisable !
  On sait en plus que:
   - pivot est presque nul à la ligne %(i1)d pour le noeud %(k1)s et
     la composante %(k2)s,
   - nombre de décimales perdues : %(i2)d. 

  -> Conseil & Risque :
     Il s'agit peut être d'un mouvement de corps rigide mal bloqué.
     Vérifiez les conditions aux limites.
     Si vous faites du contact, il ne faut pas que la structure ne "tienne" que par le contact.
"""),

#-----------------------------------------------------------------------------------------------
21: _("""
(Solveur linéaire LDLT ou MULT_FRONT) Matrice non factorisable !
  On sait en plus que:
   - pivot est presque nul à la ligne %(i1)d pour le noeud %(k1)s et
     la composante %(k2)s.

  -> Conseil & Risque :
     Verifiez votre mise en données (absence ou surabondance de conditions limites, 
     caractéristiques matériaux licites...).
     Si vous avez deja validé cette mise en données, vous pouvez essayer a la place le
     solveur linéaire MUMPS (mot-cle SOLVEUR/METHODE='MUMPS').    
"""),

#-----------------------------------------------------------------------------------------------
22: _("""
(Solveur linéaire LDLT ou MULT_FRONT) Matrice non factorisable !
  On sait en plus que:
   - le pivot est presque nul à la ligne %(i1)d pour le noeud %(k1)s et
     la composante %(k2)s,
   - nombre de décimales perdues : %(i2)d.
     
  -> Conseil & Risque :
     Il s'agit peut etre d'un mouvement de corps rigide mal bloqué.
     Vérifiez les conditions aux limites.
     Si vous faites du contact, il ne faut pas que la structure ne "tienne" que par le contact.

     Il se peut aussi que ce phénomène soit tout à fait normal avec X-FEM si la fissure passe
     très près d'un noeud.
     Si le nombre de décimal n'est pas trop grand (maxi 10 décimales)
     vous pouvez relancer le calcul en augmentant le nombre de décimales perdues autorisé :
     mot-clé NPREC dans le bloc SOLVEUR. Sinon, contactez l'équipe de développement.

"""),

#-----------------------------------------------------------------------------------------------
30: _("""
(Solveur linéaire LDLT ou MULT_FRONT) Matrice non factorisable !
  On sait en plus que:
   - pivot presque nul à la ligne : %(i1)d,
   - nombre de décimales perdues  : %(i2)d.

  -> Conseil & Risque :
     Il s'agit sans doute d'une relation linéaire entre ddls surabondante.
     La liste des noeuds concernés par cette relation est imprimée ci-dessus dans le fichier MESSAGE.
     Il faut vérifier de plus près les conditions aux limites cinématiques.
     En particulier, il se peut que la relation linéaire surabondante provienne des conditions de contact.
     Peut-être devriez vous exclure certains noeuds des conditions de contact
     (mots clés SANS_NOEUD et SANS_GROUP_NO).
"""),

#-----------------------------------------------------------------------------------------------
31: _("""
(Solveur linéaire LDLT ou MULT_FRONT) Matrice non factorisable !
  On sait en plus que:
   - pivot vraiment nul à la ligne : %(i1)d.

  -> Conseil & Risque :
     Il s'agit sans doute d'une relation linéaire entre ddls surabondante.
     La liste des noeuds concernés par cette relation est imprimée ci-dessus dans le fichier MESSAGE.
     Verifiez votre mise en donnees (conditions limites, caracteristiques materiaux...),
     En particulier, il se peut que la relation linéaire surabondante provienne des conditions de contact.
     Peut-etre devriez vous exclure certains noeuds des conditions de contact
     (mots clés SANS_NOEUD et SANS_GROUP_NO).
     Si vous avez deja validé cette mise en données, vous pouvez essayer a la place le
     solveur linéaire MUMPS (mot-clé SOLVEUR/METHODE='MUMPS').
"""),


#-----------------------------------------------------------------------------------------------
40: _("""
(Solveur linéaire LDLT ou MULT_FRONT) Matrice non factorisable !
  On sait en plus que:
   - pivot presque nul à la ligne : %(i1)d,
   - nombre de décimales perdues  : %(i2)d.

  -> Conseil & Risque :
     Il s'agit sans doute d'une relation de blocage surabondante.
     Blocage concerné : %(k4)s.
"""),

#-----------------------------------------------------------------------------------------------
41: _("""
(Solveur linéaire LDLT ou MULT_FRONT) Matrice non factorisable !
  On sait en plus que:
  - pivot vraiment nul à la ligne : %(i1)d.

  -> Conseil & Risque :
     Il s'agit sans doute d'une relation de blocage surabondante.
     blocage concerné : %(k4)s.
     Sinon, verifiez votre mise en données (conditions limites, caractéristiques matériaux...).
     Si vous avez deja validé cette mise en données, vous pouvez essayer a la place le
     solveur linéaire MUMPS (mot-clé SOLVEUR/METHODE='MUMPS').  
"""),

#-----------------------------------------------------------------------------------------------
42: _("""
Matrice non factorisable :
  Le solveur MUMPS considère la matrice comme singulière (en structure ou numériquement).

Conseil :
  Il peut s'agir d'une erreur de programmation ou d'un problème de mise en données (blocage
  absent ou surabondant).
"""),
#-----------------------------------------------------------------------------------------------
50: _("""
 Solveur MUMPS :
   -> Vous avez demandé comme renuméroteur RENUM = '%(k1)s', or MUMPS en a
      utilisé un autre.
   -> Risque & Conseil :
      Il se peut que votre version de MUMPS n'ait pas été compilée avec
      le support de ce renuméroteur. Dans le doute, RENUM='AUTO' permet
      de laisser MUMPS faire le meilleur choix.
 """),

#-----------------------------------------------------------------------------------------------
52: _("""
  -> Vous avez demandé une analyse de stabilité et vous utilisez le solveur linéaire '%(k1)s'.
     Ces deux fonctionnalités ne sont pas compatibles.

  -> Conseil :
     Changez de solveur linéaire en utilisant le mot-clé METHODE de SOLVEUR.
"""),

#-----------------------------------------------------------------------------------------------
53: _("""
Solveur MUMPS :
  Mumps manque de mémoire lors de la factorisation de la matrice.
Solution :
  Il faut augmenter la valeur du mot clé  SOLVEUR/PCENT_PIVOT.
Remarque : on a le droit de dépasser la valeur 100.
"""),

#-----------------------------------------------------------------------------------------------
54: _("""
Solveur MUMPS :
  Le solveur Mumps manque de mémoire lors de la factorisation de la matrice.

Solution :
  Il faut augmenter la mémoire accessible à Mumps (et autres programmes hors fortran d'Aster).
  Pour cela, il faut diminuer la mémoire donnée à JEVEUX (ASTK : case "dont Aster (Mo)") ou bien
  augmenter la mémoire totale (ASTK : case "Mémoire totale (Mo))".
"""),

#-----------------------------------------------------------------------------------------------
55: _("""
Solveur MUMPS :
  Problème ou alarme dans le solveur MUMPS.
  Le code retour de mumps (INFOG(1)) est : %(i1)d
Solution :
  Consulter le manuel d'utilisation de Mumps.
  Prévenir l'équipe de développement de Code_Aster.
"""),

#-----------------------------------------------------------------------------------------------
56: _("""
Solveur MUMPS :
  Il ne faut pas utiliser TYPE_RESOL = '%(k1)s'
  Pour une matrice non-symétrique.
Solution :
  Il faut utiliser TYPE_RESOL = 'NONSYM' (ou 'AUTO').
"""),

#-----------------------------------------------------------------------------------------------
57: _("""
Solveur MUMPS :
  La solution du système linéaire est trop imprécise :
  Erreur calculée   : %(r1)g
  Erreur acceptable : %(r2)g   (RESI_RELA)
Solution :
  On peut augmenter la valeur du mot clé SOLVEUR/RESI_RELA.
"""),

#-----------------------------------------------------------------------------------------------
59: _("""
Solveur MUMPS :
  La matrice est déjà factorisée. On ne fait rien.
Solution :
  Il y a sans doute une erreur de programmation.
  Contactez l'assistance.
"""),

#-----------------------------------------------------------------------------------------------
60: _("""
Solveur MUMPS :
  Limite atteinte : le solveur Mumps est utilisé par plus de 5 matrices simultanément.
Solution :
  Contactez l'assistance.
"""),

#-----------------------------------------------------------------------------------------------
61: _("""
Erreur Programmeur lors de la résolution d'un système linéaire :
 La numérotation des inconnues est incohérente entre la matrice et le second membre.
 Matrice       : %(k1)s
 Second membre : %(k2)s

 Si solveur : 'Feti' : numéro du sous-domaine (ou domaine global) : %(i1)d
"""),

#-----------------------------------------------------------------------------------------------
62: _("""
Alarme Solveur MUMPS :
  La procédure de raffinement itératif aurait besoin de plus que les %(i1)d d'itérations
  imposées en dur dans l'appel MUMPS par Code_Aster.
Solution :
  On peut essayer la valeur du mot-clé POSTTRAITEMENTS='FORCE'.
"""),

#-----------------------------------------------------------------------------------------------

64: _("""
Solveur MUMPS :
  Le solveur Mumps manque de mémoire lors de la phase d'analyse de la matrice.

Solution :
  Il faut augmenter la mémoire accessible à Mumps (et autres programmes hors fortran d'Aster).
  Pour cela, il faut diminuer la mémoire donnée à JEVEUX (ASTK : case "dont Aster (Mo)") ou bien
  augmenter la mémoire totale (ASTK : case "Mémoire totale (Mo))".
"""),

#-----------------------------------------------------------------------------------------------

65: _("""
Solveur MUMPS :
  Mumps ne peut pas factoriser la matrice à cause d'un dépassement d'entiers.

Solution :
  Si vous utilisez la version séquentielle, alors il vous faut passer à la version parallèle.
  Si vous utilisez déjà la version parallèle, alors il faut augmenter le nombre de processeurs
  alloués au calcul.
"""),

#-----------------------------------------------------------------------------------------------

66: _("""
Solveur MUMPS :
  Echec de la factorisation OUT-OF-CORE de MUMPS.
  Consulter les  messages délivrés  par MUMPS.
Conseil: Augmenter  le nombre de processeurs utilisés.
"""),
#-----------------------------------------------------------------------------------------------

67: _("""
Erreur d'utilisation (commande RESOUDRE) :
  La matrice et le second membre fournis à la commande RESOUDRE
  ne sont pas de meme dimension (nombre de ddls).
Conseil: Vérifier la cohérence des arguments MATR et CHAM_NO.
"""),
#-----------------------------------------------------------------------------------------------

68: _("""
Erreur d'utilisation (commande RESOUDRE) :
  La matrice et le second membre fournis à la commande RESOUDRE
  ne sont pas du meme type (réel/complex).
Conseil: Vérifier la cohérence des arguments MATR et CHAM_NO.
"""),

#-----------------------------------------------------------------------------------------------

70: _("""
Solveur MUMPS :
  Vous avez activé l'option IMPR='OUI_SOLVE' en surchargeant AMUMPS.F. La résolu
  tion du système linéaire en cours va donc s'effectuer normalement mais en plus
  sa matrice et son second membre vont être écrits dans le fichier d'unité logique
  %(i1)d. Vous pouvez le récupérer (sur le proc 0) via ASTK.
"""),

#-----------------------------------------------------------------------------------------------

71: _("""
Solveur MUMPS :
  Vous avez activé l'option IMPR='OUI_NOSOLVE' en surchargeant AMUMPS.F. La résolu
  tion du système linéaire en cours ne va donc pas s'effectuer mais sa matrice et
  son second membre vont être écrits dans le fichier d'unité logique %(i1)d.
  Après cette écriture, l'execution Aster s'arrête en ERREUR_FATALE pour vous
  permettre de récuperer plus rapidement votre fichier.
  Vous pouvez le récupérer (sur le proc 0) via ASTK.
"""),
#-----------------------------------------------------------------------------------------------

72: _("""
Solveur MUMPS :
  Vous utilisez une version de MUMPS antérieure à la 4.7.3: la %(k1)s.
  Celle-ci n'est plus supportée pour le couplage Code_Aster/MUMPS.
Solution:
  Télécharger et installer une version de MUMPS plus récente.
"""),
#-----------------------------------------------------------------------------------------------

73: _("""
Solveur MUMPS :
  Lors de la factorisation numérique, le pourcentage de pivots, %(r1)d %%, a dépassé le 
  pourcentage prévu par le paramètre SOLVEUR/PCENT_PIVOT= %(r2)d %%.
  Cela peut engendrer un résultat de mauvaise qualité. Vérifiez bien la qualité de celui-ci
  en fin de résolution via la mot-clé RESI_RELA.
Solution:
  Pour améliorer la qualité de la solution vous pouvez activez les options de pré et post-
  traitements (PRETRAITEMENTS='AUTO' et POSTTRAITEMENTS='FORCE' ou 'AUTO'), durcir le critère
  de qualité RESI_RELA ou, si vous avez beaucoup de Lagranges (>10%% de la taille du pb),
  désactivez l'option ELIM_LAGR2 (ELIM_LAGR2='NON').
  Sinon, contactez l'équipe de développement.
"""),

#-----------------------------------------------------------------------------------------------
74: _("""
Solveur MUMPS :
  Vous utilisez une version de MUMPS antérieure à la 4.8.4: la %(k1)s.
  Celle-ci ne permet pas la détection de singularité. On désactive cette fonctionnalité avec
  une valeur SOLVEUR/NPREC négative.
Attention:
  Cette désactivation peut nuire à certains type de calculs (modal, option CRIT_FLAMB...).
"""),

#-----------------------------------------------------------------------------------------------
75: _("""
(solveur linéaire MUMPS) Matrice non factorisable !
  On sait en plus que:
    - pivot presque nul à la ligne : %(i1)d.

  -> Conseil & Risque :
     Vérifiez votre mise en données (absence ou surabondance de conditions limites, 
     caractéristiques matériaux licites...).
"""),

#-----------------------------------------------------------------------------------------------
76: _("""
(solveur linéaire MUMPS) Matrice non factorisable !

  -> Conseil & Risque :
     Vérifiez votre mise en données (absence ou surabondance de conditions limites, 
     caractéristiques matériaux licites...).
"""),

#-----------------------------------------------------------------------------------------------
77: _("""
(solveur linéaire MUMPS) Matrice non factorisable !
  On sait en plus que:
   - pivot est presque nul à la ligne %(i1)d pour le noeud %(k1)s et
     la composante %(k2)s.

  -> Conseil & Risque :
     Il s'agit peut etre d'un mouvement de corps rigide mal bloqué.
     Vérifiez les conditions aux limites.
     Si vous faites du contact, il ne faut pas que la structure ne "tienne" que par le contact.
"""),

#-----------------------------------------------------------------------------------------------
78: _("""
(solveur linéaire MUMPS) Matrice non factorisable !
  On sait en plus que:
   - pivot presque nul à la ligne : %(i1)d.


  -> Conseil & Risque :
     Il s'agit peut etre d'un mouvement de corps rigide mal bloqué.
     Vérifiez les conditions aux limites.
     Si vous faites du contact, il ne faut pas que la structure ne "tienne" que par le contact.

     Il se peut aussi que ce phénomène soit tout à fait normal avec X-FEM si la fissure passe
     très près d'un noeud.
     Si le nombre de décimal n'est pas trop grand (maxi 10 décimales)
     vous pouvez relancer le calcul en augmentant le nombre de décimales perdues autorisé :
     mot-clé NPREC dans  le bloc SOLVEUR. Sinon, contactez l'équipe de développement.
"""),

#-----------------------------------------------------------------------------------------------
79: _("""
(solveur linéaire MUMPS) Matrice non factorisable !
  On sait en plus que:
   - pivot presque nul à la ligne : %(i1)d.

  -> Conseil & Risque :
     Il s'agit sans doute d'une relation de blocage surabondante.
     Blocage concerné : %(k4)s.
"""),
#-----------------------------------------------------------------------------------------------
80: _("""
(solveur linéaire MUMPS) Probleme de paramétrage du solveur !

  Attention, vous avez paramètré le solveur lineaire MUMPS de manière a résoudre un système
  linéaire SPD (reel Symétrique Défini Positif): mot-clé SOLVEUR/TYPE_RESOL='SYMDEF'. Or votre
  matrice est a valeur complexe. Ceci est contradictoire.

    -> Conseil & Risque :
      Utilisez le solveur lineaire MUMPS avec TYPE_RESOL='AUTO'.
"""),
#-----------------------------------------------------------------------------------------------
81: _("""
(solveur linéaire MUMPS) Matrice non factorisable !
  On sait en plus que:
   - pivot presque nul à la ligne : %(i1)d.

  -> Conseil & Risque :
     Il s'agit sans doute d'une relation linéaire entre ddls surabondante.
     La liste des noeuds concernés par cette relation est imprimée ci-dessus dans le fichier MESSAGE.
     Il faut vérifier de plus près les conditions aux limites cinématiques.
     En particulier, il se peut que la relation linéaire surabondante provienne des conditions de contact.
     Peut-être devriez vous exclure certains noeuds des conditions de contact
     (mots clés SANS_NOEUD et SANS_GROUP_NO).
"""),
#-----------------------------------------------------------------------------------------------
82: _("""
(solveur linéaire MUMPS) Matrice non factorisable !

  -> Conseil & Risque :
     Il s'agit sans doute d'une relation linéaire entre ddls surabondante.
     La liste des noeuds concernés par cette relation est imprimée ci-dessus dans le fichier MESSAGE.
     Verifiez votre mise en donnees (conditions limites, caracteristiques materiaux...),
     En particulier, il se peut que la relation linéaire surabondante provienne des conditions de contact.
     Peut-être devriez vous exclure certains noeuds des conditions de contact
     (mots clés SANS_NOEUD et SANS_GROUP_NO).  
"""),
#-----------------------------------------------------------------------------------------------
83: _("""
(solveur linéaire MUMPS) Matrice non factorisable !

  -> Conseil & Risque :
     Verifiez votre mise en données (conditions limites, caractéristiques materiaux...).
"""),
#-----------------------------------------------------------------------------------------------
84: _("""
(solveur linéaire MUMPS) Probleme de paramétrage du solveur !

Attention, vous avez paramètre le solveur lineaire MUMPS de manière a résoudre un système
linéaire SPD (reel Symétrique Défini Positif): mot-clé SOLVEUR/TYPE_RESOL='SYMDEF'. Or votre
matrice comporte des termes négatifs ou nuls sur sa diagonale. Ceci est contradictoire.

    -> Conseil & Risque :
      Si il s'agit d'un test vous voila averti, sinon utilisez le solveur lineaire MUMPS
      avec TYPE_RESOL='AUTO'.
"""),
}
