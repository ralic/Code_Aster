#@ MODIF catamess Messages  DATE 31/10/2011   AUTEUR COURTOIS M.COURTOIS 
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
# RESPONSABLE COURTOIS M.COURTOIS

# ce catalogue est réservé à Utmess !
cata_msg={

6: _(u"""
 Fin à la suite de message(s) <E>
"""),

41 : _(u"""
Le message d'alarme '%(k1)s' a été émis %(i1)d fois, il ne sera plus affiché.
"""),

55: _(u"""
 Appels récursifs de messages d'erreur ou d'alarme.
"""),

57: _(u"""
   Impossible d'importer '%(k1)s' dans Messages.
   Le fichier %(k1)s.py n'existe pas dans le répertoire 'Messages'
   ou bien la syntaxe du fichier est incorrecte.

   Merci de signaler cette anomalie.

   Erreur :
   %(k2)s
"""),

69: _(u"""
   Destruction du concept '%(k1)s'.
"""),

70: _(u"""
   Validation du concept '%(k1)s'.
"""),

# on ne veut pas émettre d'alarme mais que le message se voit, donc on fait la mise en forme ici !
89 : {  'message' : _(u"""
    Liste des alarmes émises lors de l'exécution du calcul.

    Les alarmes que vous avez choisies d'ignorer sont précédées de (*).
    Nombre d'occurrences pour chacune des alarmes :
"""), 'flags' : 'DECORATED',
},

90 : {  'message' : _(u"""       %(k1)3s %(k2)-20s émise %(i1)4d fois
"""), 'flags' : 'DECORATED',
},

92 : {  'message' : _(u"""           aucune alarme
"""), 'flags' : 'DECORATED',
},

}
