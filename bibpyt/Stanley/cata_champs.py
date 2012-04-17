#@ MODIF cata_champs Stanley  DATE 17/04/2012   AUTEUR DELMAS J.DELMAS 
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


'''Catalogue des champs de resultats Aster'''

from Cata.cata import *
from Utilitai.Utmess import UTMESS

# ----------------------------------------------------------------------

class CHAMP :

  '''Informations sur les champs'''


  # --------------------------------------------------------------------------
  def Calc_champ(champ, contexte, numeros, phenomene, options=None) :

    para = {
      'reuse'        : contexte.resultat,
      'RESULTAT'     : contexte.resultat,
      'MODELE'       : contexte.modele,
      #'OPTION'       : champ.nom,
      'CHAM_MATER'   : contexte.cham_mater,
      'NUME_ORDRE'   : tuple(numeros),
      phenomene      : champ.nom
      }

    if contexte.cara_elem :
      para['CARA_ELEM'] = contexte.cara_elem

    # Options supplementaires passees a la commande
    if options:
       for cle in options.keys():
          para[cle] = options[cle]

    # Lancement de la commande
    try:
      apply(CALC_CHAMP,(),para)
    except aster.error,err:
      UTMESS('A','STANLEY_4',valk=[str(err)])
    except Exception,err:
      UTMESS('A','STANLEY_5',valk=[str(err)])


  # --------------------------------------------------------------------------
  def Calc_erreur(champ, contexte, numeros, phenomene, options=None) :

    para = {
      'reuse'        : contexte.resultat,
      'RESULTAT'     : contexte.resultat,
      'MODELE'       : contexte.modele,
      #'OPTION'       : champ.nom,
      'CHAM_MATER'   : contexte.cham_mater,
      'NUME_ORDRE'   : tuple(numeros),
      phenomene      : champ.nom
      }

    if contexte.cara_elem :
      para['CARA_ELEM'] = contexte.cara_elem

    # Options supplementaires passees a la commande
    if options:
       for cle in options.keys():
          para[cle] = options[cle]

    # Lancement de la commande
    try:
      apply(CALC_ERREUR,(),para)
    except aster.error,err:
      UTMESS('A','STANLEY_4',valk=[str(err)])
    except Exception,err:
      UTMESS('A','STANLEY_5',valk=[str(err)])


  # --------------------------------------------------------------------------
  def __init__(self, nom_cham, type_cham, heredite, comment, phenomene, fonc) :

    assert type_cham in ['ELNO','ELGA','NOEU','ELEM']

    self.nom       = nom_cham
    self.type      = type_cham
    self.heredite  = heredite
    self.comment   = comment
    self.phenomene = phenomene

    if fonc :
      self.fonc = fonc
    else :

       if phenomene in ('CONTRAINTE', 'DEFORMATION', 'ENERGIE', 'CRITERES', 'VARI_INTERNE', 'HYDRAULIQUE', 'THERMIQUE', 'ACOUSTIQUE', 'FORCE', 'DEPLACEMENT'):
          self.fonc = CHAMP.Calc_champ
       elif phenomene in ('ERREUR'):
          self.fonc = CHAMP.Calc_erreur
       else:
          self.fonc = CHAMP.Calc_champ


  def Evalue(self, contexte, numeros, options=None):
    self.fonc(self, contexte, numeros, self.phenomene, options)



# ----------------------------------------------------------------------

class CATA_CHAMPS :

  """ Base de connaissance sur les champs traitables
  """

  def __init__(self) :

    self.cata = {}

    all = C_NOM_CHAM_INTO()
    for option in all:
       doptinfo = C_NOM_CHAM_INTO(l_nom_cham=option)
       for opt in doptinfo:
          dopt = doptinfo[opt]
          phen = dopt[0]
          linl = dopt[1]
          comm = dopt[2]
          s = opt.split('_')
          if s[-1] in ('NOEU', 'ELNO', 'ELGA', 'ELEM'):
             supp = s[-1]
          else:
             supp = 'NOEU'
          if phen not in ('AUTRES'):
             #print opt, s, supp, phen
             lheredite = list(aster.get_option_dependency(opt))
             lheredite = list(set(lheredite))
             if opt in lheredite:
                lheredite.remove(opt)
             self(opt, supp, lheredite, comm, phen)


  def __getitem__(self, nom_cham) :
    return self.cata[nom_cham]


  def __call__(self, nom_cham, type_cham, heredite, comment, phenomene, fonc = None) :
    self.cata[nom_cham] = CHAMP(nom_cham, type_cham, heredite, comment, phenomene, fonc)


  def Champs_presents(self, type_resu='evol_noli') :
    return self.cata.keys()


  def Ajoute_Champs(self, champ, typech='NOEU'):
    try:
      typech = champ.strip('_')[1]
      if typech in ['ELNO', 'ELGA', 'NOEU', 'ELEM']: typech = [ typech ]
      else: typech = None
    except:
      typech = None

    if typech:
      print 'Ajout de :', champ
      self(champ     , typech,[], "")



