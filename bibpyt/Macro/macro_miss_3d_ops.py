#@ MODIF macro_miss_3d_ops Macro  DATE 29/08/2006   AUTEUR MCOURTOI M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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



def macro_miss_3d_ops(self,UNITE_IMPR_ASTER,UNITE_OPTI_MISS,
                           UNITE_MODELE_SOL,UNITE_RESU_IMPE,
                           PROJET,REPERTOIRE,OPTION,VERSION,
                           UNITE_RESU_FORC,PARAMETRE,**args):
  """
     Ecriture de la macro MACRO_MISS_3D
  """
  import types
  from Accas import _F
  from Utilitai.Utmess import UTMESS
  from types import TupleType, ListType

  ier=0
  # On importe les definitions des commandes a utiliser dans la macro
  # Le nom de la variable doit etre obligatoirement le nom de la commande
  DEFI_FICHIER  =self.get_cmd('DEFI_FICHIER')
  EXEC_LOGICIEL =self.get_cmd('EXEC_LOGICIEL')
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  DEFI_FICHIER(ACTION='LIBERER',UNITE=UNITE_IMPR_ASTER)

  import aster 
  loc_fic=aster.repout()
  miss3d=loc_fic+'miss3d'
  #miss3d='/home/acbhhcd/MISS3D/V6.4/miss3d.csh'

  if OPTION['TOUT']!=None:
      MODUL2='COMPLET'
  elif OPTION['MODULE']=='MISS_IMPE':
      MODUL2='CALC_IMPE'
  elif OPTION['MODULE']=='MISS_EVOL':
      MODUL2='MISS_PTAS'
  elif OPTION['MODULE']=='PRE_MISS':
      MODUL2='GTASTER'

  ETUDE = PROJET
  BASE  = REPERTOIRE
  paste = 'fort.'+str(UNITE_IMPR_ASTER)
  popti = 'fort.'+str(UNITE_OPTI_MISS)
  pdsol = 'fort.'+str(UNITE_MODELE_SOL)
  primp = 'fort.'+str(UNITE_RESU_IMPE)
  prfor = 'fort.'+str(UNITE_RESU_FORC)
  
  l_para = ['FREQ_MIN','FREQ_MAX','FREQ_PAS','Z0','RFIC','SURF',
            'FICH_RESU_IMPE','FICH_RESU_FORC','DREF','ALGO',
            'OFFSET_MAX','OFFSET_NB','SPEC_MAX','SPEC_NB','ISSF',
            'FICH_POST_TRAI','CONTR_NB','CONTR_LISTE','LFREQ_NB',
            'LFREQ_LISTE']
  if PARAMETRE != None and PARAMETRE['LFREQ_NB'] != None:
    if len(PARAMETRE['LFREQ_LISTE']) != PARAMETRE['LFREQ_NB']:
      UTMESS('F', 'MACRO_MISS3D', 'Longueur de LFREQ_LISTE incorrecte')
  if PARAMETRE != None and PARAMETRE['CONTR_NB'] != None:
    if len(PARAMETRE['CONTR_LISTE']) != 3*PARAMETRE['CONTR_NB']:
      UTMESS('F', 'MACRO_MISS3D', 'Longueur de CONTR_LISTE incorrecte')
  
  dpara = {}
  for cle in l_para:
    if cle in ('SURF', 'ISSF'):
      dpara[cle] = 'NON'
    else:
      dpara[cle] = '0'
    if PARAMETRE != None and PARAMETRE[cle] != None:
      if type(PARAMETRE[cle]) in (TupleType, ListType):
        dpara[cle] = repr(' '.join([str(s) for s in PARAMETRE[cle]]))
      else:
        dpara[cle] = str(PARAMETRE[cle])
  
  EXEC_LOGICIEL(
                LOGICIEL=miss3d,
                ARGUMENT=(MODUL2,
                          ETUDE,
                          BASE,
                          paste,
                          popti,
                          pdsol,
                          primp,
                          VERSION,
                          dpara['FREQ_MIN'], 
                          dpara['FREQ_MAX'],
                          dpara['FREQ_PAS'],
                          dpara['Z0'], 
                          dpara['SURF'], 
                          dpara['RFIC'],
                          dpara['FICH_RESU_IMPE'],
                          dpara['FICH_RESU_FORC'],
                          dpara['DREF'], 
                          dpara['ALGO'],
                          dpara['OFFSET_MAX'],
                          dpara['OFFSET_NB'],
                          dpara['SPEC_MAX'],
                          dpara['SPEC_NB'],
                          dpara['ISSF'],
                          dpara['FICH_POST_TRAI'],
                          dpara['CONTR_NB'],
                          dpara['CONTR_LISTE'],
                          dpara['LFREQ_NB'],
                          dpara['LFREQ_LISTE'],
                          prfor,
                         ),
                )

  return ier
