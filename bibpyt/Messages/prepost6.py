#@ MODIF prepost6 Messages  DATE 25/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

2 : _(u"""
 -> le nombre de mailles de votre maillage (%(i1)d) est supérieur
    à la limite de 9 999 999.
 -> Risque & Conseil : veuillez vérifier le script gibi qui vous a permis
    de générer le fichier mgib.
"""),


3 : _(u"""
 le volume differe du volume use mais le nombre d'iteration
  est superieur a  %(i1)d
      volume use:  %(r1)f
  volume calcule:  %(r2)f
"""),

4 : _(u"""
 verifier les parametres d'usure pour le secteur  %(i1)d
"""),

5 : _(u"""
 verifier les parametres d'usure pour le secteur  %(i1)d
"""),

6 : _(u"""
 composante %(k1)s / point  %(i1)d
"""),

7 : _(u"""
   nombre de valeurs        =  %(i1)d
     %(r1)f, %(r2)f, ...
"""),

8 : _(u"""
   nombre de pics extraits   =  %(i1)d
     %(r1)f, %(r2)f, ...
"""),

9 : _(u"""
   nombre de cycles detectes =  %(i1)d
"""),

10 : _(u"""
   %(i1)d  /  %(r1)f   %(r2)f
"""),

11 : _(u"""
   dommage en ce point/cmp  =  %(r1)f
"""),

27 : _(u"""
 parametres de calcul du dommagenombre de numeros d'ordre  =  %(i1)d
 nombre de points de calcul =  %(i2)d
"""),

28 : _(u"""
 calcul     du      dommage en %(k1)s points  de   calcul  du    dommage %(k2)s
 composante(s) grandeur equivalente %(k3)s
 methode  d'extraction  des    pics %(k4)s
 methode  de  comptage  des  cycles %(k5)s
 methode  de  calcul    du  dommage %(k6)s

"""),

29 : _(u"""
 maille:  %(k1)s
"""),

30 : _(u"""
 des mailles de peau ne s'appuient sur aucune maille support
    maille:  %(k1)s
"""),

31 : _(u"""

     ===== GROUP_MA ASTER / PHYSICAL GMSH =====

"""),

32 : _(u"""

  Le GROUP_MA GMSH GM10000 contient %(i1)d éléments :
"""),

33 : _(u"""
       %(i1)d éléments de type %(k1)s
"""),

34 : _(u"""
    La composante %(k1)s que vous avez renseignée ne fait pas partie
    des composantes du champ à imprimer.
"""),

35 : _(u"""
    Le type de champ %(k1)s n'est pas autorisé avec les champs
    élémentaires %(k2)s.
    L'impression du champ sera effectué avec le type SCALAIRE.
"""),

36 : _(u"""
 Veuillez utiliser IMPR_GENE pour l'impression
 de résultats en variables généralisées.
"""),




38 : _(u"""
 Problème dans la lecture du fichier de maillage GMSH.
 Le fichier de maillage ne semble pas être un fichier de type GMSH :
 il manque la balise de début de fichier.
"""),

39 : _(u"""
 <I> Depuis la version 2.2.0 de GMSH il est possible de lire et écrire le format MED.
     Conseil : Utilisez plutot GMSH avec MED comme format d'entrée et de sortie.

"""),

40 : _(u"""
 <I> Le ficher de maillage GMSH est au format version %(i1)d.
"""),

41 : _(u"""
 Problème dans la lecture du fichier de maillage GMSH.
 Il manque la balise de fin de la liste de noeud.
"""),

42 : _(u"""
 Problème dans la lecture du fichier de maillage GMSH.
 Il manque la balise de début de la liste des éléments.
"""),

43 : _(u"""
 -> le nombre de mailles de votre maillage (%(i1)d) est supérieur
    à la limite de 9 999 999.
 -> Risque & Conseil : générez un fichier MED directement depuis GMSH.
"""),

}
