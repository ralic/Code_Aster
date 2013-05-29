# coding=utf-8
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

import os

def macro_miss_3d_ops(self,UNITE_IMPR_ASTER,UNITE_OPTI_MISS,
                           UNITE_MODELE_SOL,UNITE_RESU_IMPE,
                           PROJET,REPERTOIRE,OPTION,VERSION,
                           UNITE_RESU_FORC,TABLE_SOL,PARAMETRE,**args):
  """
     Ecriture de la macro MACRO_MISS_3D
  """
  import aster_core
  import aster
  from Accas import _F
  from Utilitai.Utmess        import UTMESS
  from Utilitai.Table         import Table
  from Miss.miss_fichier_sol  import fichier_sol

  ier=0
  # On importe les definitions des commandes a utiliser dans la macro
  # Le nom de la variable doit etre obligatoirement le nom de la commande
  DEFI_FICHIER  =self.get_cmd('DEFI_FICHIER')
  EXEC_LOGICIEL =self.get_cmd('EXEC_LOGICIEL')
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  DEFI_FICHIER(ACTION='LIBERER',UNITE=UNITE_IMPR_ASTER)

  loc_fic=aster_core.get_option('repout')
  tv = aster.__version__.split('.')
  if len(tv) < 3:
      tv.extend(['x']*(3-len(tv)))
  elif len(tv) > 3:
      tv = tv[:3]
  vers = '%2s.%2s.%2s' % tuple(tv)

  # if vers > ' 8. 3.11':
  #    miss3d='/aster/logiciels/MISS3D/NEW/miss3d.csh'
  # else:
  #    miss3d=loc_fic+'miss3d'

  miss3d = os.path.join(loc_fic, 'miss3d')

  # miss3d='/aster/logiciels/MISS3D/miss3d.csh-beta-modif'

  # if VERSION=='V1_2':
  #    if PARAMETRE != None and PARAMETRE['TYPE']=='BINAIRE':
  #       raise AsException("MACRO_MISS_3D/PARAMETRE : type incompatible avec version")

  if OPTION['TOUT']!=None:
      MODUL2='COMPLET'
  elif OPTION['MODULE']=='MISS_IMPE':
      MODUL2='CALC_IMPE'
  elif OPTION['MODULE']=='MISS_EVOL':
      MODUL2='MISS_PTAS'
  elif OPTION['MODULE']=='PRE_MISS':
      MODUL2='GTASTER'

  alarm16 = False
  typ15 = 'A'
  if OPTION['MODULE'] == 'MISS_IMPE' \
     and PARAMETRE != None and PARAMETRE['ISSF'] == 'NON':
         alarm16 = True
         typ15 = 'A+'

  UTMESS(typ15, 'MISS0_15')
  if alarm16:
     UTMESS('A', 'MISS0_16')

  ETUDE = PROJET
  BASE  = REPERTOIRE
  paste = 'fort.'+str(UNITE_IMPR_ASTER)
  popti = 'fort.'+str(UNITE_OPTI_MISS)
  pdsol = 'fort.'+str(UNITE_MODELE_SOL)
  primp = 'fort.'+str(UNITE_RESU_IMPE)
  prfor = 'fort.'+str(UNITE_RESU_FORC)

  if TABLE_SOL != None :

    tabsol = TABLE_SOL.EXTR_TABLE()
    try:
        texte = fichier_sol(tabsol)
    except aster.error, err:
        UTMESS('F', err.id_message, valk=err.valk, vali=err.vali, valr=err.valr)
    fdsol=open(pdsol,'w')
    fdsol.write(texte)
    fdsol.close()

  l_para = ['FREQ_MIN','FREQ_MAX','FREQ_PAS','Z0','RFIC','SURF',
            'FICH_RESU_IMPE','FICH_RESU_FORC','TYPE','DREF','ALGO',
            'OFFSET_MAX','OFFSET_NB','SPEC_MAX','SPEC_NB','ISSF',
            'FICH_POST_TRAI','CONTR_NB','CONTR_LISTE','LFREQ_NB',
            'LFREQ_LISTE','DIRE_ONDE']
  if PARAMETRE != None and PARAMETRE['LFREQ_NB'] != None:
    if len(PARAMETRE['LFREQ_LISTE']) != PARAMETRE['LFREQ_NB']:
      UTMESS('F','MISS0_1')
  if PARAMETRE != None and PARAMETRE['CONTR_NB'] != None:
    if len(PARAMETRE['CONTR_LISTE']) != 3*PARAMETRE['CONTR_NB']:
      UTMESS('F','MISS0_2')

  pndio = '0'
  dpara = {}
  for cle in l_para:
    if cle in ('SURF', 'ISSF', ):
      dpara[cle] = 'NON'
    else:
      dpara[cle] = '0'
    if PARAMETRE != None and PARAMETRE[cle] != None:
      if type(PARAMETRE[cle]) in (tuple, list):
        dpara[cle] = repr(' '.join([str(s) for s in PARAMETRE[cle]]))
      else:
        dpara[cle] = str(PARAMETRE[cle])
      if cle in ('DIRE_ONDE', ):
        pndio = '1'
        dpara['SURF'] = 'NON'

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
                          dpara['TYPE'],
                          prfor,
                          pndio,
                          dpara['DIRE_ONDE'],
                         ),
                INFO=1,)

  return ier
