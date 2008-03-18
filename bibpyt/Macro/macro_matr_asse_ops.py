#@ MODIF macro_matr_asse_ops Macro  DATE 18/03/2008   AUTEUR BOYERE E.BOYERE 
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



def macro_matr_asse_ops(self,MODELE,CHAM_MATER,CARA_ELEM,MATR_ASSE,
                        SOLVEUR,NUME_DDL,CHARGE,CHAR_CINE,INST,INFO,**args):
  """
     Ecriture de la macro MACRO_MATR_ASSE
  """
  ier=0
  from Utilitai.Utmess     import  UTMESS

  # On met le mot cle NUME_DDL dans une variable locale pour le proteger
  numeddl=NUME_DDL
  info=INFO
  # On importe les definitions des commandes a utiliser dans la macro
  # Le nom de la variable doit etre obligatoirement le nom de la commande
  CALC_MATR_ELEM=self.get_cmd('CALC_MATR_ELEM')
  NUME_DDL      =self.get_cmd('NUME_DDL')
  ASSE_MATRICE  =self.get_cmd('ASSE_MATRICE')
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  # Les mots cles simples sous SOLVEUR sont par defaut MULT_FRONT/METIS
  methode=SOLVEUR['METHODE']
  renum=SOLVEUR['RENUM']

  if numeddl in self.sdprods:
    # Si le concept numeddl est dans self.sdprods
    # il doit etre  produit par la macro
    # il faudra donc appeler la commande NUME_DDL
    lnume = 1
  else:
    lnume = 0
  lrigel = 0
  lmasel = 0

# decalage eventuel en premiere position dans la liste de l occurence de MATR_ASSE contenant
# l option de rigidite
  try :
    for m in MATR_ASSE:
      option=m['OPTION']
      if option in ('RIGI_MECA','RIGI_MECA_LAGR','RIGI_THER','RIGI_ACOU') :
         decal=m
         MATR_ASSE.remove(decal)
         MATR_ASSE.insert(0,decal)
         break
  except: pass

  iocc=0
  for m in MATR_ASSE:
    iocc=iocc+1
    option=m['OPTION']
    if iocc == 1 and lnume == 1 and option not in ('RIGI_MECA','RIGI_MECA_LAGR',
                                                   'RIGI_THER','RIGI_ACOU')      :
      UTMESS('F','MATRICE0_9')


    motscles={'OPTION':option}
    if option == 'RIGI_MECA_HYST':
       if (not lrigel):
          UTMESS('F','MATRICE0_10')
       motscles['RIGI_MECA']   =rigel
    if option == 'AMOR_MECA':
       if (not lrigel or not lmasel):
          UTMESS('F','MATRICE0_11')
       if CHAM_MATER != None:
          motscles['RIGI_MECA']   =rigel
          motscles['MASS_MECA']   =masel
    if CHARGE     != None:
       if option[0:9] not in ('MASS_THER','RIGI_GEOM','MASS_ID_M'):
                           motscles['CHARGE']      =CHARGE
    if CHAM_MATER != None: motscles['CHAM_MATER']  =CHAM_MATER
    if CARA_ELEM  != None: motscles['CARA_ELEM']   =CARA_ELEM
    if INST       != None: motscles['INST']        =INST

    try : motscles['SIEF_ELGA']   =m['SIEF_ELGA']
    except IndexError : pass

    try : motscles['MODE_FOURIER']   =m['MODE_FOURIER']
    except IndexError : pass

    try : motscles['THETA']   =m['THETA']
    except IndexError : pass

    try : motscles['PROPAGATION']   =m['PROPAGATION']
    except IndexError : pass
    _a=CALC_MATR_ELEM(MODELE=MODELE,**motscles)

    if option == 'RIGI_MECA':
      rigel  = _a
      lrigel = 1
    if option == 'MASS_MECA':
      masel  = _a
      lmasel = 1

    if lnume and option in ('RIGI_MECA','RIGI_THER','RIGI_ACOU','RIGI_MECA_LAGR'):
      self.DeclareOut('num',numeddl)
      # On peut passer des mots cles egaux a None. Ils sont ignores
      num=NUME_DDL(MATR_RIGI=_a,METHODE=methode,RENUM=renum,INFO=info)
    else:
      num=numeddl

    self.DeclareOut('mm',m['MATRICE'])
    motscles={'OPTION':option}
    if CHAR_CINE != None: 
      mm=ASSE_MATRICE(MATR_ELEM=_a,NUME_DDL=num,CHAR_CINE=CHAR_CINE)
    else:
      mm=ASSE_MATRICE(MATR_ELEM=_a,NUME_DDL=num)

  return ier
