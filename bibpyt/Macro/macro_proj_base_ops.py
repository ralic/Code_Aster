#@ MODIF macro_proj_base_ops Macro  DATE 06/09/2004   AUTEUR MCOURTOI M.COURTOIS 
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

# -*- coding: iso-8859-1 -*-


def macro_proj_base_ops(self,BASE,NB_VECT,MATR_ASSE_GENE,VECT_ASSE_GENE,PROFIL,**args):
  """
     Ecriture de la macro MACRO_PROJ_BASE
  """
  ier=0
  # On importe les definitions des commandes a utiliser dans la macro
  NUME_DDL_GENE  =self.get_cmd('NUME_DDL_GENE')
  PROJ_MATR_BASE =self.get_cmd('PROJ_MATR_BASE')
  PROJ_VECT_BASE =self.get_cmd('PROJ_VECT_BASE')
  # La macro compte pour 1 dans la numerotation des commandes
  #self.icmd=1
  self.set_icmd(1)

  _num=NUME_DDL_GENE(BASE=BASE,NB_VECT=NB_VECT,STOCKAGE=PROFIL)
  if MATR_ASSE_GENE:
    for m in MATR_ASSE_GENE:
      motscles={}
      if   m['MATR_ASSE']     :  motscles['MATR_ASSE']     =m['MATR_ASSE']
      elif m['MATR_ASSE_GENE']:  motscles['MATR_ASSE_GENE']=m['MATR_ASSE_GENE']
      else:
          ier=ier+1
          self.cr.fatal("<F> <MACRO_PROJ_BASE> MATR_ASSE et MATR_ASSE_GENE absents")
          return ier
      self.DeclareOut('mm',m['MATRICE'])
      mm=PROJ_MATR_BASE(BASE=BASE,NUME_DDL_GENE=_num,**motscles)

  if VECT_ASSE_GENE:
    _num=NUME_DDL_GENE(BASE=BASE,NB_VECT=NB_VECT,STOCKAGE=PROFIL)
    for v in VECT_ASSE_GENE:
      motscles={}
      if   v['VECT_ASSE']     :  motscles['VECT_ASSE']     =v['VECT_ASSE']
      elif v['VECT_ASSE_GENE']:  motscles['VECT_ASSE_GENE']=v['VECT_ASSE_GENE']
      else:
          ier=ier+1
          self.cr.fatal("<F> <MACRO_PROJ_BASE>MATR_ASSE et MATR_ASSE_GENE absents")
          return ier
      motscles['TYPE_VECT']=v['TYPE_VECT']
      self.DeclareOut('vv',v['VECTEUR'])
      vv=PROJ_VECT_BASE(BASE=BASE,NUME_DDL_GENE=_num,**motscles)

  return ier
