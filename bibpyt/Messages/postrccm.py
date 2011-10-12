#@ MODIF postrccm Messages  DATE 12/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

cata_msg={

1: _(u"""
 le parametre %(k2)s n'existe pas dans la table %(k1)s
"""),

2: _(u"""
 probleme pour récupérer dans la table %(k1)s la valeur du parametre %(k2)s
   pour le parametre %(k3)s de valeur %(r1)12.5E et
   pour le parametre %(k4)s de valeur %(r2)12.5E
"""),

3: _(u"""
 l'option "AMORCAGE" est traitée seule
"""),

4: _(u"""
 il manque la donnée de la limite d'élasticité (SY_02 ou SY_MAX) pour le calcul du rochet thermique
"""),

5: _(u"""
 le calcul du critere du rochet thermique pour une variation de température linéaire est impossible
        X = SIGM / SYMAX =  %(r1)12.5E
         SIGM =  %(r2)12.5E
        SYMAX =  %(r3)12.5E
        on doit avoir 0. < X < 1.
"""),

6: _(u"""
 le calcul du critere du rochet thermique pour une variation de température parabolique est impossible
        X = SIGM / SYMAX =  %(r1)12.5E
         SIGM =  %(r2)12.5E
        SYMAX =  %(r3)12.5E
        on doit avoir 0.3 < X < 1.
"""),

7: _(u"""
 il faut définir le comportement %(k1)s dans "DEFI_MATERIAU"
"""),

8: _(u"""
 erreur données, pour le noeud %(k1)s de la maille %(k2)s 
 il manque les caractéristiques élémentaires (le CARA_ELEM)
"""),

9: _(u"""
 erreur données, pour le noeud %(k1)s de la maille %(k2)s 
 il manque l'indice de contraintes %(k3)s 
"""),

10: _(u"""
 materiau non défini, maille %(k1)s 
"""),

12: _(u"""
 "NUME_GROUPE" doit etre strictement positif
"""),

13: _(u"""
 Probleme lors du passage du CH_MATER en CARTE
 Contactez le support
"""),

14: _(u"""
 Probleme lors du passage du TEMPE_REF en CARTE
 Contactez le support
"""),

15: _(u"""
 erreur données, pour la situation numéro %(i1)d sur la maille numéro %(i2)d 
 il manque le %(k1)s
"""),

16: _(u"""
 probleme pour récupérer dans la table %(k1)s la valeur du parametre %(k2)s
   pour le parametre %(k3)s de valeur %(k5)s et
   pour le parametre %(k4)s de valeur %(r1)12.5E
"""),

17: _(u"""
 probleme pour récupérer dans la table  %(k1)s les valeurs du parametre %(k4)s
   pour le parametre %(k2)s de valeur %(k3)s
"""),

18: _(u"""
 erreur données, il manque le %(k1)s 
   pour la maille numéro %(i1)d et le noeud numéro %(i2)d 
"""),

19: _(u"""
 si on est la, y a un bug!
 Contactez le support
"""),

20: _(u"""
 champ de nom symbolique %(k1)s inexistant pour le RESULTAT %(k2)s
 défini sous l'occurence numéro %(i1)d
"""),

21: _(u"""
 il ne faut qu'un seul champ de nom symbolique %(k1)s pour le RESULTAT %(k2)s
 défini sous l'occurence numéro %(i1)d
"""),

22: _(u"""
 probleme pour récupérer le champ de nom symbolique %(k1)s pour le RESULTAT %(k2)s
 défini sous l'occurence numéro %(i1)d
"""),

23: _(u"""
 on n'a pas pu récupérer le résultat thermique correspondant au numero %(i2)d 
 défini par le mot clé "NUME_RESU_THER" sous le mot clé facteur "RESU_THER"
 occurence numéro %(i1)d
"""),

24: _(u"""
 erreur données, pour la situation numéro %(i1)d sur la maille numéro %(i2)d 
   probleme sur le résultat thermique
"""),

25: _(u"""
 erreur données, pour la situation numéro %(i1)d sur la maille numéro %(i2)d et le noeud numéro %(i3)d
   probleme sur le résultat thermique
"""),

26: _(u"""
 il faut définir qu'un seul séisme dans un groupe
   groupe numéro %(i1)d 
   occurence situation %(i2)d et %(i3)d 
"""),

28: _(u"""
 erreur données, pour la situation numero %(i1)d 
 on n'a pas pu récupérer le "RESU_MECA" correspondant au numéro du cas de charge %(i2)d 
"""),

29: _(u"""
 erreur données, pour la situation numero %(i1)d 
 on ne peut pas avoir des charges de type "seisme" et "autre".
"""),

30: _(u"""
 probleme pour recuperer IOC SEISME.
 Contactez le support
"""),

31: _(u"""
 probleme avec TYPEKE.
 Contactez le support
"""),

32: _(u"""
 le nombre de cycles admissibles est négatif, vérifiez la courbe de WOHLER
   contrainte calculée: %(r1)12.5E
   Nadm: %(r2)12.5E
"""),

33: _(u"""
 la distance calculée à partir des ABSC_CURV de la table fournie %(k1)s
 est supérieure à 1 pour cent à la distance récupérée dans le matériau. Vérifiez vos données.
   distance calculée: %(r1)12.5E
   D_AMORC          : %(r2)12.5E
"""),

34: _(u"""
 avec une ou des situations de passage, il faut définir au plus 3 groupes
"""),

36: _(u"""
 bug ! contactez l'assistance.
"""),

37: _(u"""
 -> L'ordre des noeuds de la table %(k1)s n'est pas respecté.
    Les noeuds doivent etre rangés d'une des peaux vers l'autre.
 -> Risque & Conseil:
    Veuillez consulter la documentation U2.09.03.
"""),

38: _(u"""
 -> Les noeuds de la ligne de coupe %(k2)s (table %(k1)s) ne sont pas alignés:
    - distance maximale à la ligne de coupe: %(r1)f
    - longueur de la ligne de coupe        : %(r2)f
 -> Risque & Conseil:
    Les calculs avec POST_RCCM ne sont théoriquement valides que pour des lignes
    de coupe rectilignes. Vérifier les données d'entrée ou utiliser
    MACR_LIGN_COUPE pour extraire le résultat sur un segment de droite.
"""),

39: _(u"""
 -> Il est préférable de fournir des tables comportant les coordonnées des noeuds.
"""),

40: _(u"""
 -> Pour le cas unitaire, il doit y avoir un seul ligament.
    La table %(k1)s contient %(i1)d ligaments.
 -> Risque & Conseil:
    Veuillez revoir le contenu de votre table.
 """),

41: _(u"""
 -> Les tables %(k1)s et %(k2)s ont des noeuds possédant
    des coordonnées différentes:
    - table %(k1)s : %(k3)s = %(r1)f
    - table %(k2)s : %(k3)s = %(r2)f
 -> Risque & Conseil:
    Veuillez revoir le contenu de vos tables
"""),    

 42: _(u"""
 -> Les tables %(k1)s et %(k2)s ne sont pas cohérentes en terme de nombre
    de ligaments:
    - table %(k1)s : %(i1)d ligaments 
    - table %(k2)s : %(i2)d ligaments 
 -> Risque & Conseil:
    Veuillez revoir le contenu de vos tables
"""),

43: _(u"""
 -> Les tables %(k1)s et %(k2)s ne sont pas cohérentes en terme d'instant:
    Une différence a été observée entre les valeurs d'instant d'un meme point
    - table %(k1)s : INST = %(r1)f
    - table %(k2)s : INST = %(r2)f
 -> Risque & Conseil:
    Veuillez revoir le contenu de vos tables
   
"""),

44: _(u"""
 probleme pour récupérer dans la table %(k1)s la valeur du parametre %(k2)s
 pour le parametre %(k3)s de valeur %(r1)12.5E.
"""),

}
