#@ MODIF algorith17 Messages  DATE 08/11/2010   AUTEUR PELLET J.PELLET 
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
# RESPONSABLE DELMAS J.DELMAS

def _(x) : return x

cata_msg={
1: _("""
 Il y a moins de sous-domaines (%(i1)d) que de processeurs participant au calcul (%(i2)d).
 
 Conseils :
   - augmentez le nombre de sous-domaines de la partition du mot-clé PARTITION
   - diminuez le nombre de processeurs du calcul
"""),

2: _("""
         Comportement %(k1)s non implanté pour l'élément d'interface
"""),
3: _("""
        il manque le déplacement de référence DEPL_REFE  
"""), 
4: _("""
        La formulation n'est ni en contrainte nette ni en bishop  
"""), 
5 : _("""
  Le champ posttraité est un cham_elem, le calcul de moyenne ne fonctionne que
 sur les cham_no. Pour les cham_elem utiliser POST_ELEM mot-clé INTEGRALE.
"""), 
6 : _("""
  Le calcul de la racine numero %(i1)d par la méthode de la matrice compagnon a échoué.
"""), 
7 : _("""
  Il n'y a qu'un seul mode_meca en entree de DEFI_BASE_MODALE. La numerotation
  de reference prise est celle associee a celui-ci. Le mot-cle NUME_REF
  n'est pas pris en compte
"""), 
8 : _("""
  Il manque le nume_ddl pour le resultat. Le mot-cle NUME_REF doit etre
  renseigne.
"""), 
9 : _("""L'équation d'évolution de l'endommagement n'admet pas de solution réelle  """),
10 : _("""
  La loi de comportement mécanique %(k1)s n'est pas compatible avec les 
  éléments de joint avec couplage hydro-mécanique.
"""),
11 : _("""
  La fermeture du joint sort des bornes [0,fermeture maximale] sur la maille %(k1)s.
  fermeture du joint CLO = %(r1)f
  fermeture maximale UMC = %(r2)f
  Vérifier la cohérence chargement mécanique, fermeture asymptotique et ouverture 
  initiale.
"""),
12 : _("""
  La température de référence (exprimée en Kelvin) doit toujours être strictement supérieure à zéro.
"""),
13 : _("""
  La pression de gaz de référence doit toujours être différente de zéro.
"""),
}
