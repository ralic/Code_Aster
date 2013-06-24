# coding=utf-8
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



def stanley_ops(self,RESULTAT,MODELE,CHAM_MATER,CARA_ELEM,DISPLAY,**args):

  """
     Importation et lancement de Stanley
  """

  import os,string
  import aster
  from Accas import _F
  from Noyau.N_utils import AsType
  from Utilitai.Utmess import  UTMESS
  from Utilitai.UniteAster import UniteAster

  prev_onFatalError = aster.onFatalError()
  aster.onFatalError('EXCEPTION')

  ier=0

  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  # Redefinition eventuelle du DISPLAY
  if DISPLAY:
    UTMESS('I','STANLEY_1',valk=DISPLAY)
    os.environ['DISPLAY'] = DISPLAY

  # Mode validation de la non-regression
  if args['UNITE_VALIDATION']:
     UTMESS('I','STANLEY_2')
     UL = UniteAster()
     FICHIER_VALID=UL.Nom(args['UNITE_VALIDATION'])
  else:
     FICHIER_VALID=None

  # On ne lance Stanley que si la variable DISPLAY est définie
  if os.environ.has_key('DISPLAY'):

    import Stanley
    from Stanley import stanley_engine

    if (RESULTAT and MODELE and CHAM_MATER):
      _MAIL = aster.getvectjev( string.ljust(MODELE.nom,8) + '.MODELE    .LGRF        ' )
      _MAIL = string.strip(_MAIL[0])
      MAILLAGE = self.jdc.g_context[_MAIL]
      if CARA_ELEM:
        stanley_engine.STANLEY(RESULTAT,MAILLAGE,MODELE,CHAM_MATER,CARA_ELEM)
      else:
        stanley_engine.STANLEY(RESULTAT,MAILLAGE,MODELE,CHAM_MATER,None)
    else:
      stanley_engine.PRE_STANLEY(FICHIER_VALID)

  else:
      UTMESS('A','STANLEY_3',valk=['STANLEY'])

  aster.onFatalError(prev_onFatalError)

  return ier
