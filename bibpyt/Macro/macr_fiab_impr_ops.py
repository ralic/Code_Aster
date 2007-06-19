#@ MODIF macr_fiab_impr_ops Macro  DATE 24/01/2005   AUTEUR DURAND C.DURAND 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
# RESPONSABLE GNICOLAS G.NICOLAS
#
def macr_fiab_impr_ops(self, INFO,
                       TABLE_CIBLE, NOM_PARA_CIBLE, GRADIENTS, **args):
#
#
#  1. args est le dictionnaire des arguments
#    args.keys() est la liste des mots-clés
#    args.keys()[0] est la premiere valeur de cette liste
#    args.keys()[1:] est la liste des valeurs suivantes dans cette liste
#    args.keys(mot_cle) représente le contenu de la variable mot_cle dans la macro appelante.
#
  """ Macro-commande réalisant l'impression des valeurs pour le logiciel de fiabilite. """
#
# On charge les modules nécessaires
  from Accas import _F
#
#____________________________________________________________________
#
# 1. Préalables
#____________________________________________________________________
#
  erreur = 0
#
# 1.1 ==> La macro compte pour 1 dans l'exécution des commandes
#
  self.set_icmd(1)
#
# 1.2 ==> On importe les définitions des commandes Aster utilisées
#         dans la macro
#
  DEFI_FICHIER = self.get_cmd("DEFI_FICHIER")
  IMPR_TABLE   = self.get_cmd("IMPR_TABLE")
#
# 1.3. ==> Des constantes
#          Atention : le numéro d'unité utilisé ici et celui
#                     utlisé dans le python d'échange lance_aster_5
#                     doivent correspondre.
#
  Unite_Fichier_ASTER_vers_FIABILITE = 91
  Nom_Symbolique_Fichier_ASTER_vers_FIABILITE = "ASTER_vers_FIABILITE"
  FORMAT_R="1PE17.10"
#____________________________________________________________________
#
# 2. Définition d'un fichier d'échange
#____________________________________________________________________
# 
  DEFI_FICHIER ( ACTION= "ASSOCIER",
#                FICHIER = Nom_Symbolique_Fichier_ASTER_vers_FIABILITE,
                 UNITE = Unite_Fichier_ASTER_vers_FIABILITE,
                 TYPE = "ASCII",
                 INFO = INFO )
#____________________________________________________________________
#
# 4. Ecriture de la valeur cible
#____________________________________________________________________
#
  IMPR_TABLE ( TABLE = TABLE_CIBLE,
               NOM_PARA = NOM_PARA_CIBLE,
               UNITE = Unite_Fichier_ASTER_vers_FIABILITE,
               FORMAT_R = FORMAT_R,
               INFO = INFO )
#____________________________________________________________________
#
# 5. Ecritures des gradients
#____________________________________________________________________
#
  if GRADIENTS is not None :
#
    for val in GRADIENTS :
#
      IMPR_TABLE ( TABLE = val["TABLE"],
                   SENSIBILITE = val["PARA_SENSI"],
                   NOM_PARA = (val["NOM_PARA"]),
                   UNITE = Unite_Fichier_ASTER_vers_FIABILITE,
                   FORMAT_R = FORMAT_R,
                   INFO = INFO )
#____________________________________________________________________
#
# 6. Libération du fichier d'échange
#____________________________________________________________________
# 
  DEFI_FICHIER ( ACTION= "LIBERER",
                 UNITE = Unite_Fichier_ASTER_vers_FIABILITE,
                 INFO = INFO )
#
#--------------------------------------------------------------------
# 7. C'est fini !
#--------------------------------------------------------------------
#
  return erreur
