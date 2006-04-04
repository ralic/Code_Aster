#@ MODIF cata_champs Stanley  DATE 04/04/2006   AUTEUR CIBHHLV L.VIVAN 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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

try:
   from Utilitai.Utmess import UTMESS
except ImportError:
   def UTMESS(code,sprg,texte):
      fmt='\n <%s> <%s> %s\n\n'
      print fmt % (code,sprg,texte)


# ----------------------------------------------------------------------

class CHAMP :

  '''Informations sur les champs'''


  def Calc_no(champ, contexte, numeros) :

    para = {
      'reuse'      : contexte.resultat,
      'RESULTAT'   : contexte.resultat,
      'OPTION'     : champ.nom,
      'CHAM_MATER' : contexte.cham_mater,
      'NUME_ORDRE' : tuple(numeros)
      }

    if contexte.cara_elem :
      para['CARA_ELEM'] = contexte.cara_elem

    if champ.nom in ['FORC_NODA','REAC_NODA'] :  # une specificite a disparaitre
      para['MODELE'] = contexte.modele

    try:
      apply(CALC_NO,(),para)
    except aster.error,err:
      UTMESS('A','STANLEY',"Une erreur est intervenue. Raison :\n"+str(err))
    except aster.FatalError,err:
      UTMESS('A','STANLEY',"Une erreur est intervenue. Raison :\n"+str(err))
    except Exception,err:
      UTMESS('A','STANLEY',"Cette action n'est pas realisable.\n"+str(err))


  def Calc_elem(champ, contexte, numeros) :

    para = {
      'reuse'      : contexte.resultat,
      'RESULTAT'   : contexte.resultat,
      'MODELE'     : contexte.modele,
      'OPTION'     : champ.nom,
      'CHAM_MATER' : contexte.cham_mater,
      'NUME_ORDRE' : tuple(numeros)
      }

    if contexte.cara_elem :
      para['CARA_ELEM'] = contexte.cara_elem
  
    try:
      apply(CALC_ELEM,(),para)
    except aster.error,err:
      UTMESS('A','STANLEY',"Une erreur est intervenue. Raison :\n"+str(err))
    except aster.FatalError,err:
      UTMESS('A','STANLEY',"Une erreur est intervenue. Raison :\n"+str(err))
    except Exception,err:
      UTMESS('A','STANLEY',"Cette action n'est pas realisable.\n"+str(err))


  def __init__(self, nom_cham, type_cham, heredite, comment, fonc) :
   
    assert type_cham in ['ELNO','ELGA','NOEU']

    self.nom      = nom_cham
    self.type     = type_cham 
    self.heredite = heredite
    self.comment  = comment

    if fonc :
      self.fonc = fonc
    else : 
      if type_cham == 'NOEU' :  self.fonc = CHAMP.Calc_no
      if type_cham == 'ELNO' :  self.fonc = CHAMP.Calc_elem
      if type_cham == 'ELGA' :  self.fonc = CHAMP.Calc_elem
        

  def Evalue(self, contexte, numeros) :
    self.fonc(self, contexte, numeros)
    

        
# ----------------------------------------------------------------------

class CATA_CHAMPS :

  '''Base de connaissance sur les champs traitables'''
  
  
  def __init__(self) :

    self.cata = {}
    self('DEPL'          , 'NOEU',[],                            "Deplacements aux noeuds")
    self('TEMP'          , 'NOEU',[],                            "Temperature aux noeuds")
    self('SIEF_ELGA'     , 'ELGA',[],                            "Contraintes aux points de Gauss")
    self('SIEF_ELGA_DEPL', 'ELGA',[],                            "Contraintes aux points de Gauss en lineaire")
    self('VARI_ELGA'     , 'ELGA',[],                            "Variables internes aux points de Gauss")
    self('SIEF_ELNO_ELGA', 'ELNO',['SIEF_ELGA','SIEF_ELGA_DEPL'],"Contraintes aux noeuds par element")
    self('SIEF_NOEU_ELGA', 'NOEU',['SIEF_ELNO_ELGA'],            "Contraintes aux noeuds")
    self('FLUX_ELNO_TEMP', 'ELNO',['TEMP'],                      "Flux thermique aux noeuds par element")
    self('VARI_ELNO_ELGA', 'ELNO',['VARI_ELGA'],                 "Variables internes aux noeuds par element")
    self('VARI_NOEU_ELGA', 'NOEU',['VARI_ELNO_ELGA'],            "Variables internes aux noeuds")
    self('EQUI_ELGA_SIGM', 'ELGA',['SIEF_ELGA'],                 "Invariants des contraintes aux points de Gauss")
    self('EQUI_ELNO_SIGM', 'ELNO',['SIEF_ELNO_ELGA', 'SIGM_ELNO_COQU'], "Invariants des contraintes aux noeuds par element")
    self('EQUI_NOEU_SIGM', 'NOEU',['EQUI_ELNO_SIGM'],            "Invariants des contraintes aux noeuds")
    self('EPSI_ELGA_DEPL', 'ELGA',['DEPL'],                      "Deformations aux points de Gauss")
    self('EPSI_ELNO_DEPL', 'ELNO',['DEPL'],                      "Deformations aux noeuds par elements")
    self('EPSI_NOEU_DEPL', 'NOEU',['EPSI_ELNO_DEPL'],            "Deformations aux noeuds")
    self('EPSG_ELGA_DEPL', 'ELGA',['DEPL'],                      "Deformations de Green aux points de Gauss")
    self('EPSG_ELNO_DEPL', 'ELNO',['DEPL'],                      "Deformations de Green aux noeuds par elements")
    self('EPSG_NOEU_DEPL', 'NOEU',['EPSG_ELNO_DEPL'],            "Deformations de Green aux noeuds")
    self('EPME_ELNO_DEPL', 'ELNO',['DEPL'],                      "Deformations mecaniques aux noeuds par elements")
    self('EQUI_ELGA_EPSI', 'ELGA',['EPSI_ELGA_DEPL'],            "Invariants des deformations aux points de Gauss")
    self('EQUI_ELNO_EPSI', 'ELNO',['EPSI_ELNO_DEPL'],            "Invariants des deformations aux noeuds par element")
    self('EQUI_NOEU_EPSI', 'NOEU',['EQUI_ELNO_EPSI'],            "Invariants des deformations aux noeuds")
    self('ERRE_ELEM_SIGM', 'ELGA',['SIEF_ELNO_ELGA'],            "Indicateurs d'erreur en residu aux points de Gauss")
    self('ERRE_ELNO_ELEM', 'ELNO',['ERRE_ELEM_SIGM'],            "Indicateurs d'erreur en residu aux noeuds par element")
    self('ERRE_ELNO_ELEM', 'ELNO',[],                            "Indicateurs d'erreur en thermique")
    self('FORC_NODA'     , 'NOEU',['SIEF_ELGA'],                 "Forces nodales")
    self('VALE_CONT'     , 'NOEU',[],                            "Informations sur l'etat de contact")

# dyn
    self('VITE'          , 'NOEU',[],                            "Vitesses aux noeuds")
    self('ACCE'          , 'NOEU',[],                            "Accélérations aux noeuds")
#    self('ECIN_ELEM_DEPL', 'ELEM',[],                            "Accélérations aux noeuds")


# lineaire
    # contraintes
    self('SIGM_ELNO_DEPL', 'ELNO',['DEPL'],                      "Contraintes aux noeuds par element en lineaire")
    self('SIPO_ELNO_DEPL', 'ELNO',['DEPL'],                      "Contraintes aux noeuds par element pour les elements Poutres")
    self('SIGM_ELNO_TUYO', 'ELNO',[],                            "")
    # efforts
    self('EFGE_ELNO_DEPL', 'ELNO',['DEPL'],                      "Efforts generalises aux noeuds par element")
    # déformations
    self('DEGE_ELNO_DEPL', 'ELNO',[],                            "")


# non lineaire
    self('SIGM_ELNO_COQU', 'ELNO',['SIEF_ELGA', 'SIEF_ELGA_DEPL'], "Contraintes aux noeuds par element pour les elements Coques")
    self('VARI_ELNO_COQU', 'ELNO',['VARI_ELGA'],                   "Variables internes aux noeuds par element pour les elements Coques")

    self('EPSP_ELNO',      'ELNO',['SIEF_ELGA', 'DEPL'],         "")
    self('EPSP_ELGA',      'ELGA',['SIEF_ELGA', 'DEPL'],         "")
    self('ETOT_ELGA',      'ELGA',['SIEF_ELGA', 'DEPL'],         "")
    self('ETOT_ELNO_ELGA', 'ELNO',['SIEF_ELGA', 'DEPL'],         "")



  def __getitem__(self, nom_cham) :
    return self.cata[nom_cham]


  def __call__(self, nom_cham, type_cham, heredite, comment, fonc = None) :
    self.cata[nom_cham] = CHAMP(nom_cham, type_cham, heredite, comment, fonc)


  def Champs_presents(self, type_resu='evol_noli') :
#    return self.cata.keys()

    return self.cata.keys()


  def Ajoute_Champs(self, champ, typech='NOEU'):

    try:
      typech = champ.strip('_')[1]
      if typech in ['ELNO', 'ELGA', 'NOEU', 'ELEM']: typech = [ typech ]
      else: typech = None
    except:
      typech = None

    if typech:
#      print 'Ajout de :', champ
      self(champ     , typech,[], "")



