# coding=utf-8
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
# person_in_charge: josselin.delmas at edf.fr

cata_msg={

1: _(u"""
 on a un arc ouvert et le "NOEUD_ORIG" n'est pas une extrémité
"""),

2: _(u"""
 le "NOEUD_ORIG" ne fait pas parti du chemin
"""),

3: _(u"""
 il faut que les angles soient croissants
"""),

4: _(u"""
 il faut que les angles soient dans l'intervalle [-180,180]
"""),

5: _(u"""
 il faut un rayon strictement positif !
"""),

6: _(u"""
 face illégale pour une maille de type %(k1)s
"""),

7: _(u"""
 type de maille non traitée
"""),

8: _(u"""
 type d'intersection non traité: %(k1)s
"""),

9: _(u"""
 détection de deux sommets confondus dans une même face
"""),

10: _(u"""
 la commande "INTE_MAIL_2D" suppose que le maillage est plan (z=constante) ce qui n'est pas le cas ici.
 utilisez la commande "INTE_MAIL_3D".
"""),

11: _(u"""
 aucun segment ne coupe le maillage
"""),

12: _(u"""
 mot clé facteur: %(k1)s, occurrence numéro: %(i1)d
 maille inexistante: %(k2)s 
"""),

13: _(u"""
 mot clé facteur: %(k1)s, occurrence numéro: %(i1)d
 présence de maille(s) surfacique(s), groupe: %(k2)s 
"""),

14: _(u"""
 arrêt sur erreur(s) de données
"""),

15: _(u"""
 mot clé facteur: %(k1)s, occurrence numéro: %(i1)d
 présence de maille surfacique: %(k2)s 
"""),

16: _(u"""
 mot clé facteur: %(k1)s, occurrence numéro: %(i1)d
 groupe de mailles inexistant: %(k2)s
"""),

17: _(u"""
 au noeud %(k1)s on ne peut avoir plus de 2 mailles, nombre de mailles: %(i1)d
"""),

18: _(u"""
 trop de noeuds dans le GROUP_NO: %(k1)s, noeud utilisé: %(k2)s 
"""),

19: _(u"""
  mot clé facteur: %(k1)s, occurrence numéro: %(i1)d
  le mot clé %(k2)s admet pour argument une liste de 2 réels (a1,a2)
  telle que -180. < a1 <= a2 < 180.
"""),

20: _(u"""
 mot clé facteur: %(k1)s, occurrence numéro: %(i1)d
 le centre n'est pas vraiment le centre du cercle
"""),

21: _(u"""
 la partie %(i1)d de la courbe de nom: %(k1)s ne coupe pas le domaine maille
 non production du concept
"""),

22: _(u"""
 face inconnue, maille numéro: %(i1)d  face: %(i2)d 
"""),

23: _(u"""
 problème pour trouver l'intersection pour la face %(i1)d de la maille %(i2)d
"""),

24: _(u"""
 face dégénérée pour la maille numéro: %(i1)d face: %(i2)d 
"""),

25: _(u"""
 segment et face coplanaire, nombre de points: %(i3)d
 problème pour trouver l'intersection pour la maille numéro: %(i1)d face: %(i2)d 
"""),

26: _(u"""
 face dégénérée pour la maille numéro: %(i1)d face: %(i2)d arête: %(i3)d 
"""),

27: _(u"""
 mot clé facteur: %(k1)s, occurrence numéro: %(i1)d
 origine et extrémité confondues à la précision: %(r1)f 
"""),

28: _(u"""
 l'intersection segment %(k1)s avec le maillage %(k2)s est vide
    origine   : %(r1)f %(r2)f %(r3)f
    extrémité : %(r4)f %(r5)f %(r6)f
"""),

29: _(u"""
 il y chevauchement entre les mailles %(k1)s et %(k2)s 
"""),

30: _(u"""
 il y a un saut d'abscisse entre les mailles %(k1)s et %(k2)s 
"""),

31: _(u"""
 le GROUP_NO_ORIG %(k1)s n'existe pas.
"""),

32: _(u"""
 problème pour récupérer la grandeur %(k1)s dans la table "CARA_GEOM"
"""),

33: _(u"""
 occurrence %(i1)d de DEFI_SEGMENT : le segment comporte trop de points 
d intersection avec le maillage. Il faut le diviser en %(i2)d segments
"""),

}
