#@ MODIF defi_part_feti_ops Macro  DATE 08/11/2004   AUTEUR ASSIRE A.ASSIRE 
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


# ===========================================================================
#           CORPS DE LA MACRO "DEFI_PART_FETI"
#           -------------------------------------
# USAGE :
#  MAILLAGE        maillage a partitionner
#  MODELE          modele (facultatif)
#  NB_PART         nb de sous-domaines
#  EXCIT           liste des chargements
#  METHODE         PMETIS, KMETIS ou AUTRE
#  LOGICIEL        si AUTRE alors on attend un chemin complet vers executable
#  NOM_GROUP_MA    Un nom de base pour les group_ma contenant les SD
#  INFO            1,2
#  
# ===========================================================================
# script PYTHON : appel a Metis et lancement de DEFI_PART_OPS


def defi_part_feti_ops(self,MAILLAGE,MODELE,NB_PART,EXCIT,METHODE,NOM_GROUP_MA,INFO,**args):


  # MACRO D'APPEL A METIS
  INCLUSE='OUI'  # Est ce que les mailles de bords sont incluses dans les SD
                 # ou bien cree t on des SD pour elles

  import aster, string

  from Accas import _F
  from Noyau.N_utils import AsType

  from Utilitai import partition

#  path_metis = '/home/assire/DEV/FETI/metis/metis-4.0/'
  if METHODE=="AUTRE":
    exe_metis = arg['LOGICIEL']
  else:
    exe_metis = aster.repout() + string.lower(METHODE)

  ier=0

  # On importe les definitions des commandes a utiliser dans la macro
  DEFI_PART_OPS = self.get_cmd('DEFI_PART_OPS')

  # La macro compte pour 1 dans la numerotation des commandes
  self.icmd=1

  # Le concept sortant dans le contexte de la macro
  self.DeclareOut('_SDFETI',self.sd)

  # Debut :
  print """

  #  ---------------------------------------------------------------------------
  #  MACRO-COMMANDE : DEFI_PART_FETI
  #  ----------------
"""

  # Objet Partition
  _part = partition.PARTITION(jdc=self);

  # Executable metis
  _part.OPTIONS['exe_metis'] = exe_metis

  if MODELE:
    _part.Partitionne_Aster(
                       MAILLAGE = MAILLAGE,
                       MODELE   = MODELE,
                       NB_PART  = NB_PART,
                       INFO     = INFO,
                       );

  elif MAILLAGE:
    _part.Partitionne_Aster(
                       MAILLAGE = MAILLAGE,
                       NB_PART  = NB_PART,
                       INFO     = INFO,
                       );

  # Creation des group_ma dans le maillage Aster
  _part.Creation_Group_ma_Aster_par_SD(NOM=NOM_GROUP_MA, NOM2='B', INCLUSE=INCLUSE)

  # Creation de la SDFETI
  _tmp  = []
  for gma in _part.ASTER['GROUP_MA']:
    txt = { 'GROUP_MA': gma }
    _tmp.append( txt )
  motscle2= {'DEFI': _tmp }

  _SDFETI=DEFI_PART_OPS(NOM='SDD',
                        MODELE=MODELE,
                        INFO=1,
#                        EXCIT=EXCIT,
                        **motscle2
                        );

  # Fin :
  print """

  %  FIN MACRO-COMMANDE: DEFI_PART_FETI

"""

  return ier

