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
# person_in_charge: josselin.delmas at edf.fr

cata_msg={
1: _(u"""
  !!! mode no : %(i1)d
    linéairement dépendant a déformation statique valeur singulière min :  %(r1)f
    !! nous la forçons a :  %(r2)f
"""),

2: _(u"""
  pour le mode no : %(i1)d participation :  %(r1)f
"""),

4: _(u"""
 mot-clef "AMOR_MECA" interdit :
 il est déjà calcule.
"""),

5: _(u"""
 mot-clef "AMOR_MECA" interdit :
 le résultat :  %(k1)s  existe déjà.
"""),

6: _(u"""
 -> Utilisation d'une fonctionnalité qui va disparaître (APPUI_LACHE)
 -> Risque & Conseil:
    Utilisez  CREA_GROUP_MA / OPTION = 'APPUI', TYPE_APPUI='AU_MOINS_UN'.
"""),


7: _(u"""
 -> Le groupe de mailles %(k1)s est vide. On ne le crée donc pas !
 -> Risque & Conseil:
    Veuillez vous assurer que le type de mailles souhaité soit cohérent
    avec votre maillage.
"""),

8 : _(u"""
  Aucun DDL actif n'a été trouve pour les interfaces données
   => Les modes d'attaches, de contrainte ou de couplage ne peuvent pas être calcules.

  CONSEIL : Vérifiez la cohérence de la définition des interfaces (conditions limites)
            avec la méthode retenue :
             - CRAIGB   : le modèle doit être défini avec des interfaces encastrées,
             - CB_HARMO : le modèle doit être défini avec des interfaces encastrées,
             - MNEAL    : le modèle doit être défini avec des interfaces libres.
"""),


9 : _(u"""
 Le support indiqué pour la restitution %(k1)s n'est
  pas cohérent avec le support utilisé pour la base modale %(k2)s.
 CONSEIL : Renseigner le bon support de restitution dans le fichier de commande.
"""),

10 : _(u"""
 Lors de la copie du groupe de mailles %(k1)s appartenant à la sous-structure %(k2)s,
 le nom qui lui sera affecté dans squelette dépasse 8 caractères. La troncature peut
 générer un conflit plus tard avec les noms des autres groupes de mailles.
"""),

11 : _(u"""
 Vous avez traité plusieurs champs simultanément.
 On ne peut pas utiliser les résultats obtenus pour des calculs de modification structurale.
"""),

12 : _(u"""
fréquences non identique pour les différentes interfaces.
on retient FREQ = %(r1)f
"""),

}
