#@ MODIF calculel5 Messages  DATE 24/10/2011   AUTEUR PELLET J.PELLET 
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

cata_msg = {

1 : _(u"""
Erreur utilisateur dans MODI_MAILLAGE / DEFORME :
  Le fichier de d�placement fourni est associ� au maillage : %(k2)s
  Alors que le maillage � d�former est : %(k1)s

  Il faut que ces 2 maillages soient les m�mes.

Conseils :
  Pour cr�er un champ de d�placement adapt� au maillage %(k1)s, on peut utiliser
  la commande PROJ_CHAMP.
"""),

2 : _(u"""
 pour les options de thermique, il y a encore a travailler !!
"""),

3 : _(u"""
Erreur Utilisateur :
  On cherche � regrouper des �l�ments finis en "paquets" de fa�on � ce que la taille de leurs
  matrices �l�mentaires ne puisse pas d�passer la taille des blocs d�finis dans :
  DEBUT / MEMOIRE / TAILLE_BLOC

  Malheureusement, la taille indiqu�e ne permet m�me pas de mettre un �l�ment dans le paquet.

Conseil :
  Il faut augmenter la valeur du param�tre  DEBUT / MEMOIRE / TAILLE_BLOC
"""),

4 : _(u"""
 !! probleme creation cham_elem nul dans alchml !!
"""),

5 : _(u"""
Erreur utilisateur :
  Vous essayez de faire un calcul non-lin�aire m�canique sur un mod�le dont les �l�ments
  ne sont pas programm�s pour cela.
  On arrete le calcul.

Risques & conseils :
  Vous devriez changer de MODELISATION.
  Par exemple, la mod�lisation 'DST' ne peut pas faire du non-lin�aire alors que la
  mod�lisation 'DKT' le peut.
"""),

6 : _(u"""
Erreur utilisateur :
 Vous utilisez le mot cl� NOM_CMP, mais l'une (au moins) des composantes indiquees
 n'appartient pas � la grandeur : %(k1)s
"""),

7 : _(u"""
Alarme utilisateur :
  Vous utilisez la commande PROJ_CHAMP ou un mot cl� n�cessitant de "projeter"
  des noeuds sur des mailles (par exemple LIAISON_MAIL).
  L'un des noeuds (%(k1)s) du maillage (%(k2)s) a �t� projet� � une distance D non nulle significative.
  (D>10%% de la taille de la maille la plus proche (%(k3)s) du maillage (%(k4)s)).
"""),

8 : _(u"""
 il faut renseigner le mot cl� MAILLE
"""),

9 : _(u"""
Erreur utilisateur :
  Vous ne pouvez pas utiliser la m�thode ECLA_PG avec le mot-cl� RESULTAT.
Conseil :
  Extrayez le champ aux ELGA que contient votre r�sultat puis utilisez la m�thode ECLA_PG avec le mot-cl� CHAM_GD.
"""),


10: _(u"""
Erreur d'utilisation dans la commande CREA_MAILLAGE :
  Le mot cl� MAILLAGE est ici obligatoire.
"""),

11 : _(u"""
 le parametre est a valeurs de type  " %(k1)s "  et la valeur de reference de type  " %(k2)s ".
"""),

12 : _(u"""
 TYPE_TEST inconnu
"""),

13 : _(u"""
 le champ  %(k1)s  est a valeurs de type  " %(k2)s "  et la valeur de reference de type  " %(k3)s ".
"""),

14 : _(u"""
 le champ  %(k1)s  est de type inconnu.
"""),










20 : _(u"""
 le GROUP_NO  %(k1)s  contient  %(k2)s  noeuds
"""),

21 : _(u"""
 le GROUP_MA  %(k1)s  contient  %(k2)s  mailles
"""),














27 : _(u"""
 ! pas de lumpe en 3d p2: hexa20_d --> face8_d !
"""),












30 : _(u"""
Erreur Utilisateur dans PROJ_CHAMP (ou LIAISON_MAILLE) :
 Votre mod�le ne contient pas d'element de meme dimension que le maillage sous-jacent.
 Verifiez la dimension de votre maillage.
"""),



31 : _(u"""
Erreur Utilisateur dans PROJ_CHAMP (ou LIAISON_MAILLE) :
 Votre mod�le contient un type d'�l�ment non support� par la commande.
 Faites une demande d'�volution.
"""),





32: _(u"""
Erreur Utilisateur dans PROJ_CHAMP (ou LIAISON_MAILLE) :
 Pour le modele  %(k1)s  on ne peut pas visualiser ensemble plusieurs champs ELGA (%(k2)s,  ...)
 car les familles de points de Gauss sont differentes
"""),





33 : _(u"""
PROJ_CHAMP (ou LIAISON_MAILLE) :
  La m�thode %(k1)s est incompatible avec le champ %(k2)s.
  V�rifiez le type du champ � projeter !"""),


34 : _(u"""
 ! p2 obligatoire avec terme source non nul !
"""),

35 : _(u"""
PROJ_CHAMP (ou LIAISON_MAILLE) :
  Vous cherchez � projeter un champ par �l�ment (ELGA).
  Pour cela, il vous faut renseigner le mot-cl� MODELE_1."""),

36 : _(u"""
PROJ_CHAMP (ou LIAISON_MAILLE) :
  Le mot-cl� TYPE_CHAM est incompatible avec le mot-cl� CHAM_GD.
  Il n'est utilisable qu'avec le mot-cl� RESULTAT."""),

37 : _(u"""
PROJ_CHAMP (ou LIAISON_MAILLE) :
  Vous cherchez � projeter un champ par �l�ment (ELNO, ELEM ou ELGA).
  Pour cela, il vous faut renseigner le mot-cl� MODELE_2."""),

38 : _(u"""
  il faut definir un champ de vitesse
"""),

39 : _(u"""
 la grandeur pour la variable:  %(k1)s  doit etre:  %(k2)s  mais elle est:  %(k3)s
"""),




41 : _(u"""
 pas de variables internes initiales pour la maille  %(k1)s
"""),

42 : _(u"""
 comportements incompatibles :  %(k1)s  et  %(k2)s  pour la maille  %(k3)s
"""),

43 : _(u"""
PROJ_CHAMP (ou LIAISON_MAILLE) :
  Le noeud %(k1)s de coordonn�es (%(r1)f,%(r2)f,%(r3)f) est projet� � la distance %(r4)f"""),

44 : _(u"""
 ! le champ doit etre un cham_elem !
"""),

45 : _(u"""
 ! longueurs des modes locaux imcompatibles entre eux !
"""),

46 : _(u"""
 ! terme normalisation global nul !
"""),

48 : _(u"""
 PROJ_CHAMP (ou LIAISON_MAIL) :
 Nombre de noeuds projet�s sur des mailles un peu distantes : %(i1)d.
 (la distance � la maille est sup�rieure � 1/10i�me du diam�tre de la maille)

 Le noeud %(k1)s est projet� le plus loin � la distance %(r1)f"""),


49 : _(u"""
 LIAISON_MAIL :
 La relation lin�aire destin�e � �liminer le noeud esclave %(k1)s est une tautologie
 car la maille maitre en vis � vis de ce noeud poss�de ce meme noeud dans sa connectivit�.
 On ne l'�crit donc pas.
"""),

50 : _(u"""
 Pr�sence de coques orthotropes, les mots cl�s ANGL_REP ou VECTEUR
 du mot cl� facteur REPE_COQUE ne sont pas trait�s.
"""),


52 : _(u"""
 Pr�sence de GRILLE dans la mod�lisation, les mots cl�s ANGL_REP ou VECTEUR
 du mot cl� facteur REPE_COQUE ne sont pas trait�s.
"""),

53 : _(u"""
 La super_maille %(k1)s n'existe pas dans le maillage %(k2)s.
"""),

54 : _(u"""
 La maille %(k1)s doit etre une maille de peau de type QUAD ou TRIA
 car on est en 3D et elle est de type %(k2)s.
"""),



56 : _(u"""
 La combinaison 'fonction multiplicatrice' et 'chargement de type fonction' n'est pas autoris�e car
 votre chargement %(k1)s contient une charge exprim�e par une formule.
 Pour r�aliser cette combinaison, vous devez transformer votre charge 'formule' en charge 'fonction'
 (via l'op�rateur DEFI_FONCTION ou CALC_FONC_INTERP).
 On poursuit sans tenir compte de la fonction multiplicatrice.
"""),

57 : _(u"""
 La combinaison de chargements de meme type n'est pas autoris�e car l'un des chargements
 contient une charge exprim�e par une formule.
 Pour r�aliser cette combinaison, vous devez transformer votre charge 'formule' en charge 'fonction'
 (via l'op�rateur DEFI_FONCTION ou CALC_FONC_INTERP)
"""),

58 : _(u"""
 La combinaison de chargements de type 'd�formation initiale' n'a aucun sens physique.'
"""),

59 : _(u"""
 La combinaison de chargements de type 'pesanteur' n'a aucun sens physique.'
"""),

60 : _(u"""
 La combinaison de chargements de type 'rotation' est d�conseill�e.
 Veuillez plutot utiliser un chargement de type 'force interne'.
"""),

63 : _(u"""
 Il faut au moins 2 num�ros d'ordre pour traiter l'option %(k1)s
"""),


65 : _(u"""
 composante non definie dans  la grandeur.  composante:  %(k1)s
"""),

66 : _(u"""

 le nombre de composantes affectees n'est pas egal  au nombre de composantes a affecter
 occurence de affe numero %(i1)d
 nbre de cmp affectees :  %(i2)d
 nbre de cmp a affecter :  %(i3)d
"""),

67 : _(u"""
 erreurs donneesle GROUP_MA  %(k1)s
  n'a pas le meme nombre de mailles  que le GROUP_MA  %(k2)s
"""),

68 : _(u"""
 erreurs donneesle GROUP_MA  %(k1)s
  n'a pas les memes types de maille  que le GROUP_MA  %(k2)s
"""),

69 : _(u"""
 erreurs donnees : la maille  %(k1)s  du maillage  %(k2)s
  n'est pas la translation de la  maille  %(k3)s
  du maillage  %(k4)s
    vecteur translation :  %(r1)f %(r2)f %(r3)f
"""),

70 : _(u"""
 l'instant  de calcul  %(r1)f  n'existe pas dans  %(k1)s
"""),

71 : _(u"""
 plusieurs numeros d'ordre trouves pour l'instant  %(r1)f
"""),

72 : _(u"""
 cette commande est reentrante :   sd resultat en sortie     %(k1)s
    sd resultat "resu_final"  %(k2)s
"""),

73 : _(u"""
 la sd resultat en sortie  %(k1)s
  doit contenir qu'un seul nume_ordre %(k2)s
"""),

74 : _(u"""
 manque le champ  %(k1)s  dans la sd resultat  %(k2)s
  pour le nume_ordre  %(i1)d
"""),

76 : _(u"""
 Il n'est pas encore possible de d�couper le type_element :  %(k1)s  en sous-�l�ments
    elrefa  :  %(k2)s ;
    famille :  %(k3)s.
 Faites une demande d'�volution.
"""),

78 : _(u"""
 Il n'est pas encore possible de d�couper le type_element :  %(k1)s  en sous-�l�ments
    elrefa :  %(k2)s.
 Faites une demande d'�volution.
"""),






85 : _(u"""
 pb liste de mailles carte : %(k1)s  numero entite : %(i1)d
  position ds liste : %(i2)d
  numero de maille  : %(i3)d
"""),

}
