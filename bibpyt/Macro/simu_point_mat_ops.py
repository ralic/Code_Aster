#@ MODIF simu_point_mat_ops Macro  DATE 10/10/2006   AUTEUR REZETTE C.REZETTE 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
def simu_point_mat_ops(self, COMP_INCR, MATER, INCREMENT, NEWTON,CONVERGENCE,
               SUIVI_DDL,SIGM_IMPOSE,EPSI_IMPOSE, INFO, **args) :

  """Simulation de la reponse d'un point materiel"""

  ier = 0
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  # On importe les definitions des commandes a utiliser dans la macro
  # Le nom de la variable doit etre obligatoirement le nom de la commande
  DEFI_FONCTION   = self.get_cmd('DEFI_FONCTION')
  LIRE_MAILLAGE   = self.get_cmd('LIRE_MAILLAGE')
  AFFE_MATERIAU   = self.get_cmd('AFFE_MATERIAU')
  AFFE_MODELE     = self.get_cmd('AFFE_MODELE')
  AFFE_CHAR_MECA  = self.get_cmd('AFFE_CHAR_MECA')
  STAT_NON_LINE   = self.get_cmd('STAT_NON_LINE')
  STAT_NON_LINE   = self.get_cmd('STAT_NON_LINE')
  POST_RELEVE_T   = self.get_cmd('POST_RELEVE_T')
  CALC_TABLE      = self.get_cmd('CALC_TABLE')
  CALC_ELEM       = self.get_cmd('CALC_ELEM')

  from Accas import _F
  from Utilitai.UniteAster import UniteAster


# -- Tests de cohérence
  __fonczero = DEFI_FONCTION(NOM_PARA = 'INST',
  VALE     = ( 0,0, 10,0 ),PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT')
  
  EPS={}
  SIG={}
  
  CMP_EPS=['EPXX','EPYY','EPZZ','EPXY','EPXZ','EPYZ']
  CMP_SIG=['SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ']
  
  if SIGM_IMPOSE:        
     SIG=SIGM_IMPOSE[0].cree_dict_valeurs(SIGM_IMPOSE[0].mc_liste)
     for i in SIG.keys():
         if SIG[i]==None : SIG[i]=__fonczero
  else:
     for i in range(6):
         SIG[CMP_SIG[i]]=__fonczero

  if EPSI_IMPOSE:        
     EPS=EPSI_IMPOSE[0].cree_dict_valeurs(EPSI_IMPOSE[0].mc_liste)
#      for i in EPS.keys():
#          if EPS[i]==None : EPS[i]=__fonczero
  else:
     for i in range(6):
         EPS[CMP_EPS[i]]=None
         
  for index in range(6):
      iks=CMP_SIG[index]
      ike=CMP_EPS[index]
      if EPS[ike]!=None and SIG[iks] != __fonczero :
         raise ' un seul parmi :' + str(iks) +' '+ str(ike)
         
#   print 'EPS=',EPS
#   print 'SIG=',SIG
# -- Definition du maillage

  texte_ma = """
    COOR_3D                                               
      P0  0.0   0.0   0.0       
      P1  1.0   0.0   0.0      
      P2  0.0   1.0   0.0      
      P3  0.0   0.0   1.0      
    FINSF
    TRIA3
      F1   P0 P3 P2
      F2   P0 P1 P3
      F3   P0 P2 P1
      F4   P1 P2 P3    
    FINSF
    TETRA4
      VOLUME = P0 P1 P2 P3
    FINSF
    FIN
  """
  UL = UniteAster()
  umail = UL.Libre(action='ASSOCIER', nom='simu.mail' )
  
  fi_mail = open('simu.mail','w')
  fi_mail.write(texte_ma)
  fi_mail.close()
  
  __MA  =  LIRE_MAILLAGE(UNITE=umail)
  UL.EtatInit()


#  -- Materiau et modele

  __CHMAT = AFFE_MATERIAU(
    MAILLAGE = __MA, 
    AFFE = _F(
      MAILLE = 'VOLUME', 
      MATER  =  MATER
      )
    )


  __MO = AFFE_MODELE(
    MAILLAGE = __MA, 
    AFFE     = _F(
      MAILLE       = ('VOLUME','F1','F2','F3','F4'), 
      PHENOMENE    = 'MECANIQUE', 
      MODELISATION = '3D'
      )
    )


# -- Mouvement de corps rigide

  __C_RIGIDE = AFFE_CHAR_MECA(
    MODELE = __MO,
    DDL_IMPO = _F(NOEUD = 'P0',DX = 0,DY = 0,DZ = 0),
    LIAISON_DDL = (
      _F(NOEUD=('P2','P1'),DDL=('DX','DY'),COEF_MULT=(1,-1),COEF_IMPO=0),
      _F(NOEUD=('P3','P1'),DDL=('DX','DZ'),COEF_MULT=(1,-1),COEF_IMPO=0),
      _F(NOEUD=('P3','P2'),DDL=('DY','DZ'),COEF_MULT=(1,-1),COEF_IMPO=0),
      )
    )

# -- Chargement en deformation

  __E = [None]*6

  __E[0] = AFFE_CHAR_MECA(
    MODELE = __MO,
    DDL_IMPO = _F(NOEUD='P1', DX=1)
    )
     
  __E[1] = AFFE_CHAR_MECA(
    MODELE = __MO,
    DDL_IMPO = _F(NOEUD='P2', DY=1)
    )
     
  __E[2] = AFFE_CHAR_MECA(
    MODELE = __MO,
    DDL_IMPO = _F(NOEUD='P3', DZ=1)
    )
     
  __E[3] = AFFE_CHAR_MECA(
    MODELE = __MO,
    DDL_IMPO = _F(NOEUD='P1', DY=1)
    )
     
  __E[4] = AFFE_CHAR_MECA(
    MODELE = __MO,
    DDL_IMPO = _F(NOEUD='P1', DZ=1)
    )
     
  __E[5] = AFFE_CHAR_MECA(
    MODELE = __MO,
    DDL_IMPO = _F(NOEUD='P2', DZ=1)
    )
     
     
# -- Chargement en contrainte

  __S = [None]*6
  
  r33 = 3**-0.5
  
  __S[0] = AFFE_CHAR_MECA(
    MODELE = __MO,
    FORCE_FACE = (
      _F(MAILLE='F1', FX=-1),
      _F(MAILLE='F4', FX= r33),
      )
    )
     
  __S[1] = AFFE_CHAR_MECA(
    MODELE = __MO,
    FORCE_FACE = (
      _F(MAILLE='F2', FY=-1),
      _F(MAILLE='F4', FY= r33),
      )
    )
     
  __S[2] = AFFE_CHAR_MECA(
    MODELE = __MO,
    FORCE_FACE = (
      _F(MAILLE='F3', FZ=-1),
      _F(MAILLE='F4', FZ= r33),
      )
    )
     
  __S[3] = AFFE_CHAR_MECA(
    MODELE = __MO,
    FORCE_FACE = (
      _F(MAILLE='F1', FY=-1),
      _F(MAILLE='F2', FX=-1),
      _F(MAILLE='F4', FX= r33, FY=r33),
      )
    )
     
  __S[4] = AFFE_CHAR_MECA(
    MODELE = __MO,
    FORCE_FACE = (
      _F(MAILLE='F1', FZ=-1),
      _F(MAILLE='F3', FX=-1),
      _F(MAILLE='F4', FX= r33, FZ=r33),
      )
    )
     
  __S[5] = AFFE_CHAR_MECA(
    MODELE = __MO,
    FORCE_FACE = (
      _F(MAILLE='F2', FZ=-1),
      _F(MAILLE='F3', FY=-1),
      _F(MAILLE='F4', FY= r33, FZ=r33),
      )
    )
     
     
# -- Construction de la charge

  l_char = [  _F(CHARGE=__C_RIGIDE)  ]
  
  for i in xrange(6) :
    ike=CMP_EPS[i]
    if EPS[ike]:
       l_char.append(  _F(CHARGE=__E[i],FONC_MULT=EPS[ike])  )
       
  for i in xrange(6) :
    iks=CMP_SIG[i]
    l_char.append(  _F(CHARGE=__S[i],FONC_MULT=SIG[iks])  )
      
# -- Deroulement du calcul
  motscles={} 
  motscles['COMP_INCR']   = COMP_INCR.List_F()
  motscles['CONVERGENCE'] = CONVERGENCE.List_F()
  motscles['NEWTON']      = NEWTON.List_F()
  motscles['INCREMENT']   = INCREMENT.List_F()
  
  if   SUIVI_DDL   : 
     motscles['SUIVI_DDL']   = SUIVI_DDL.List_F()
     

  __EVOL = STAT_NON_LINE(
    MODELE = __MO, 
    CHAM_MATER = __CHMAT, 
    EXCIT = l_char,
    ARCHIVAGE = _F(ARCH_ETAT_INIT = 'OUI'),**motscles)


  __EVOL = CALC_ELEM(reuse = __EVOL,
    RESULTAT = __EVOL,
    OPTION = ('SIEF_ELNO_ELGA','EPSI_ELNO_DEPL','VARI_ELNO_ELGA')
    )
    
     
# -- Recuperation des courbes

  __REP_VARI = POST_RELEVE_T(
    ACTION = (
      _F(
        INTITULE  = 'VARI_INT',
        RESULTAT  =  __EVOL,
        NOM_CHAM  = 'VARI_ELNO_ELGA',
        TOUT_CMP  = 'OUI',
        OPERATION = 'EXTRACTION',
        NOEUD     = 'P0'
        ),
      )
    )

    
  __REP_EPSI = POST_RELEVE_T(
    ACTION = (
      _F(
        INTITULE  = 'EPSILON',
        RESULTAT  =  __EVOL,
        NOM_CHAM  = 'EPSI_ELNO_DEPL',
        TOUT_CMP  = 'OUI',
        OPERATION = 'EXTRACTION',
        NOEUD     = 'P0'
        ),
      )
    )

  __REP_SIGM = POST_RELEVE_T(
    ACTION = (
      _F(
        INTITULE  = 'SIGMA',
        RESULTAT  =  __EVOL,
        NOM_CHAM  = 'SIEF_ELNO_ELGA',
        TOUT_CMP  = 'OUI',
        OPERATION = 'EXTRACTION',
        NOEUD     = 'P0'
        ),
      )
    )
  self.DeclareOut('REPONSE',self.sd)
  REPONSE=CALC_TABLE( TABLE=__REP_EPSI,
           ACTION=_F(OPERATION='COMB',TABLE=__REP_SIGM,NOM_PARA=('INST'), ) )

  REPONSE=CALC_TABLE(reuse=REPONSE, TABLE=REPONSE,
           ACTION=_F(OPERATION='COMB',TABLE=__REP_VARI,NOM_PARA=('INST'), ) )


  return ier
  
  
  

