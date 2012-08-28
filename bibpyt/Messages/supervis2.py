#@ MODIF supervis2 Messages  DATE 27/08/2012   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

1 : _(u"""
 Lecture du fichier %(k1)s..."""),

2 : _(u"""
    Vous utilisez une vieille version de Code_Aster.

    En mettant à jour votre version, vous bénéficierez des dernières améliorations
    apportées au code depuis 15 mois.
    Si vous avez des développements privés, vous risquez d'avoir un travail
    important de portage si vous ne suivez pas les mises à jour.
"""),

3 : _(u"""%(k1)-8s %(k2)-16s ignoré"""),

#4 plus bas avec 10

5 : _(u"""
Erreur inattendue lors de l'exécution de la commande '%(k1)s'.
Merci de signaler cette anomalie.

Erreur :

%(k2)s
%(k3)s
"""),

6 : _(u"""
Erreur lors du chargement du catalogue du matériau '%(k1)s'.
"""),

7 : _(u"""
Erreur dans le catalogue du matériau '%(k1)s'.

Il n'est pas possible d'avoir plusieurs occurrences pour le
mot-clé facteur '%(k2)s'.
"""),

8 : _(u"""
L'opération de retassage de la base GLOBALE (mot clé RETASSAGE="OUI"
dans la commande FIN) est inutile lorsque l'on sauvegarde cette
dernière au format HDF (mot clé FORMAT_HDF="OUI" dans la commande FIN).
"""),

#9 plus bas avec 10

# Lignes d'entete
4 : {  'message' : _(u"""

                -- CODE_ASTER -- VERSION : %(k1)s --
"""),
       'flags' : 'CENTER | ALL_UNIT',
},

9 : {  'message' : _(u"""Version %(k1)s du %(k2)s"""),
       'flags' : 'CENTER | ALL_UNIT',
},

23 : {  'message' : _(u"""Version %(k1)s modifiée le %(k2)s
               révision %(k3)s - branche '%(k4)s'"""),
       'flags' : 'CENTER | ALL_UNIT',
},

10 : {  'message' : _(u"""Copyright EDF R&D %(k1)s - %(k2)s

                Exécution du : %(k3)s
                Nom de la machine : %(k4)s
                Architecture : %(k5)s
                Type de processeur : %(k6)s
                Système d'exploitation : %(k7)s
                Langue des messages : %(k8)s
"""), 'flags' : 'CENTER | ALL_UNIT',
},
# fin Lignes d'entete

11 : {  'message' : _(u"""Parallélisme MPI : actif
                Rang du processeur courant : %(i1)d
                Nombre de processeurs utilisés : %(i2)d"""),
        'flags' : 'CENTER | ALL_UNIT',
},

12 : {  'message' : _(u"""Parallélisme MPI : inactif"""),
        'flags' : 'CENTER | ALL_UNIT',
},

13 : {  'message' : _(u"""Parallélisme OpenMP : actif
                Nombre de processus utilisés : %(i1)d"""),
        'flags' : 'CENTER | ALL_UNIT',
},

14 : {  'message' : _(u"""Version de la librairie HDF5 : %(i1)d.%(i2)d.%(i3)d"""),
        'flags' : 'CENTER | ALL_UNIT',
},

15 : {  'message' : _(u"""Librairie HDF5 : non disponible"""),
        'flags' : 'CENTER | ALL_UNIT',
},

16 : {  'message' : _(u"""Version de la librairie MED : %(i1)d.%(i2)d.%(i3)d"""),
        'flags' : 'CENTER | ALL_UNIT',
},

17 : {  'message' : _(u"""Librairie MED : non disponible"""),
        'flags' : 'CENTER | ALL_UNIT',
},

18 : {  'message' : _(u"""Librairie MUMPS : installée"""),
        'flags' : 'CENTER | ALL_UNIT',
},

19 : {  'message' : _(u"""Librairie MUMPS : non disponible"""),
        'flags' : 'CENTER | ALL_UNIT',
},

20 : {  'message' : _(u"""Version de la librairie SCOTCH : %(i1)d.%(i2)d.%(i3)d"""),
        'flags' : 'CENTER | ALL_UNIT',
},

21 : {  'message' : _(u"""Librairie SCOTCH : non disponible"""),
        'flags' : 'CENTER | ALL_UNIT',
},

22 : {  'message' : _(u"""Mémoire limite pour l'exécution : %(r2).2f Mo      
                          consommée par l'initialisation : %(r3).2f Mo
                         par les objets du jeu de commandes : %(r4).2f Mo
                         reste pour l'allocation dynamique : %(r1).2f Mo"""),
        'flags' : 'CENTER | ALL_UNIT',
},

# 23 plus haut avec 10

24 : {  'message' : _(u"""Taille limite des fichiers d'échange : %(r1).2f Go
"""), 'flags' : 'CENTER | ALL_UNIT',
},


# Affichage des commandes
70 : u"""  # ------------------------------------------------------------------------------------------""",

71 : _(u"""  # Commande No :  %(i1)04d            Concept de type : %(k1)s"""),

72 : _(u"""  # Commande :
  # ----------"""),

73 : _(u"""  # Mémoire (Mo) : %(r1)8.2f / %(r2)8.2f / %(r3)8.2f / %(r4)8.2f (VmPeak / VmSize / Optimum / Minimum)"""),

# attention au décalage
74 : _(u"""  # Mémoire (Mo) : %(r2)8.2f / %(r3)8.2f / %(r4)8.2f (VmSize / Optimum / Minimum)"""),

75 : _(u"""  # Fin commande No : %(i1)04d   user+syst:%(r1)12.2fs (syst:%(r2)12.2fs, elaps:%(r3)12.2fs)"""),

76 : _(u"""  # Fin commande : %(k1)s"""),

# sans formatage pour STAT_NON_LINE (impmem)
77 : _(u"""
  Mémoire (Mo) : %(r1)8.2f / %(r2)8.2f / %(r3)8.2f / %(r4)8.2f (VmPeak / VmSize / Optimum / Minimum)
"""),

# attention au décalage
78 : _(u"""
  Mémoire (Mo) : %(r2)8.2f / %(r3)8.2f / %(r4)8.2f (VmSize / Optimum / Minimum)
"""),


97 : {  'message' : _(u"""
 <FIN> Arrêt normal dans "FIN".
"""), 'flags' : 'ALL_UNIT',
},

}
