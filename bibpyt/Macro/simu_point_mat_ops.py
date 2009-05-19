#@ MODIF simu_point_mat_ops Macro  DATE 18/05/2009   AUTEUR PROIX J-M.PROIX 
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
def simu_point_mat_ops(self, COMP_INCR, MATER, INCREMENT, NEWTON,CONVERGENCE,RECH_LINEAIRE,SIGM_INIT,EPSI_INIT,VARI_INIT,
          COMP_ELAS,SUIVI_DDL,ARCHIVAGE,SIGM_IMPOSE,EPSI_IMPOSE,MODELISATION, ANGLE, MASSIF,INFO, **args) :

  """Simulation de la reponse d'un point materiel"""

  ier = 0
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)
  
  import Numeric

  # On importe les definitions des commandes a utiliser dans la macro
  # Le nom de la variable doit etre obligatoirement le nom de la commande
  DEFI_FONCTION   = self.get_cmd('DEFI_FONCTION')
  LIRE_MAILLAGE   = self.get_cmd('LIRE_MAILLAGE')
  AFFE_MATERIAU   = self.get_cmd('AFFE_MATERIAU')
  AFFE_MODELE     = self.get_cmd('AFFE_MODELE')
  AFFE_CHAR_MECA  = self.get_cmd('AFFE_CHAR_MECA')
  AFFE_CARA_ELEM  = self.get_cmd('AFFE_CARA_ELEM')
  STAT_NON_LINE   = self.get_cmd('STAT_NON_LINE')
  POST_RELEVE_T   = self.get_cmd('POST_RELEVE_T')
  CALC_TABLE      = self.get_cmd('CALC_TABLE')
  CALC_ELEM       = self.get_cmd('CALC_ELEM')
  CREA_CHAMP       = self.get_cmd('CREA_CHAMP')
  MODI_REPERE       = self.get_cmd('MODI_REPERE')
  MODI_MAILLAGE       = self.get_cmd('MODI_MAILLAGE')

  from Accas import _F
  from Utilitai.UniteAster import UniteAster


# -- Tests de cohérence
  __fonczero = DEFI_FONCTION(NOM_PARA = 'INST',
  VALE     = ( 0,0, 10,0 ),PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT')
  
  EPS={}
  SIG={}
  
  if MODELISATION=="3D":
      nbsig=6
      CMP_EPS=['EPXX','EPYY','EPZZ','EPXY','EPXZ','EPYZ']
      CMP_SIG=['SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ']
  else:
      nbsig=3
      CMP_EPS=['EPXX','EPYY','EPXY']
      CMP_SIG=['SIXX','SIYY','SIXY']
      
  if SIGM_IMPOSE:        
     SIG=SIGM_IMPOSE[0].cree_dict_valeurs(SIGM_IMPOSE[0].mc_liste)
     for i in SIG.keys():
         if SIG[i]==None : SIG[i]=__fonczero
  else:
     for i in range(nbsig):
         SIG[CMP_SIG[i]]=__fonczero

  if EPSI_IMPOSE:        
     EPS=EPSI_IMPOSE[0].cree_dict_valeurs(EPSI_IMPOSE[0].mc_liste)
  else:
     for i in range(nbsig):
         EPS[CMP_EPS[i]]=None
         
  for index in range(nbsig):
      iks=CMP_SIG[index]
      ike=CMP_EPS[index]
      if EPS[ike]!=None and SIG[iks] != __fonczero :
         raise ' un seul parmi :' + str(iks) +' '+ str(ike)
         
#   print 'EPS=',EPS
#   print 'SIG=',SIG
# -- Definition du maillage
  if MODELISATION=="3D":

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
       GROUP_MA
       TOUT  VOLUME
       FINSF
       GROUP_NO
       TOUT  P1 P2 P0 P3
       FINSF
       FIN
     """
  
  else :
  
     texte_ma = """
       COOR_2D                                            
         P0  0.0   0.0
         P1  1.0   0.0
         P2  0.0   1.0
       FINSF
       SEG2
         S1   P2 P0
         S2   P0 P1
         S3   P1 P2
       FINSF
       TRIA3
         VOLUME = P0 P1 P2
       FINSF
       GROUP_MA
       TOUT  VOLUME
       FINSF
       GROUP_NO
       TOUT  P1 P2 P0
       FINSF
       FIN
     """
  
  fi_mail = open('simu.mail','w')
  fi_mail.write(texte_ma)
  fi_mail.close()
  
  UL = UniteAster()
  umail = UL.Libre(action='ASSOCIER', nom='simu.mail' )
  
  __MA  =  LIRE_MAILLAGE(UNITE=umail)
  UL.EtatInit()



  if MODELISATION=="3D":
     __MO = AFFE_MODELE(  MAILLAGE = __MA, 
       AFFE=_F(MAILLE=('VOLUME','F1','F2','F3','F4'),PHENOMENE='MECANIQUE',MODELISATION='3D',))
## ANGLE : rotation de ANGLE autour de Z uniquement, et seulement pour les déformations
##         imposées.
     if ANGLE != 0. :
        __MA=MODI_MAILLAGE(reuse=__MA ,MAILLAGE = __MA ,ROTATION=_F(POIN_1=(0.,0. ),ANGL = ANGLE),)
        c=Numeric.cos(ANGLE*Numeric.pi/180.)
        s=Numeric.sin(ANGLE*Numeric.pi/180.)
        __C_RIGIDE=AFFE_CHAR_MECA(MODELE=__MO, 
              DDL_IMPO=_F(NOEUD='P0',DX=0,DY=0.,DZ=0.),
              LIAISON_DDL = (
        _F(NOEUD=('P1','P1','P2','P2'),DDL=('DX','DY','DX','DY'),COEF_MULT=(s,-c,c,s),COEF_IMPO=0),
        _F(NOEUD=('P1','P3','P3'),DDL=('DZ','DX','DY'),COEF_MULT=(-1.0,c,s),COEF_IMPO=0),
        _F(NOEUD=('P2','P3','P3'),DDL=('DZ','DX','DY'),COEF_MULT=(-1.0,-s,c),COEF_IMPO=0),),)
     else :
# -- Mouvement de corps rigide
        __C_RIGIDE = AFFE_CHAR_MECA(MODELE=__MO,
              DDL_IMPO = _F(NOEUD = 'P0',DX = 0,DY = 0,DZ = 0),
              LIAISON_DDL = (
        _F(NOEUD=('P2','P1'),DDL=('DX','DY'),COEF_MULT=(1,-1),COEF_IMPO=0),
        _F(NOEUD=('P3','P1'),DDL=('DX','DZ'),COEF_MULT=(1,-1),COEF_IMPO=0),
        _F(NOEUD=('P3','P2'),DDL=('DY','DZ'),COEF_MULT=(1,-1),COEF_IMPO=0),))
  else:
  # MODELISATION 2D
     __MO=AFFE_MODELE(MAILLAGE=__MA, 
          AFFE=_F(MAILLE=('VOLUME','S1','S2','S3'),PHENOMENE='MECANIQUE',MODELISATION=MODELISATION))
## ANGLE : rotation de ANGLE autour de Z uniquement, et seulement pour les déformations
##         imposées.
     if ANGLE != 0. :
        __MA=MODI_MAILLAGE(reuse=__MA ,MAILLAGE=__MA,ROTATION=_F(POIN_1=(0.,0.),ANGL=ANGLE),)
        c=Numeric.cos(ANGLE*Numeric.pi/180.)
        s=Numeric.sin(ANGLE*Numeric.pi/180.)
        __C_RIGIDE = AFFE_CHAR_MECA(MODELE = __MO, 
              DDL_IMPO=_F(NOEUD='P0',DX=0,DY=0.),
              LIAISON_DDL=(_F(NOEUD=('P1','P1','P2','P2'),DDL=('DX','DY','DX','DY'),
                   COEF_MULT=(s,-c,c,s),COEF_IMPO=0),),)
     else :
        __C_RIGIDE = AFFE_CHAR_MECA(MODELE = __MO,
            DDL_IMPO = _F(NOEUD = 'P0',DX = 0,DY = 0),
            LIAISON_DDL = (_F(NOEUD=('P2','P1'),DDL=('DX','DY'),COEF_MULT=(1,-1),COEF_IMPO=0),))
            
# --MASSIF : orientation du materiau (monocristal, orthotropie)
  if MASSIF:        
      ANGMAS=MASSIF[0].cree_dict_valeurs(MASSIF[0].mc_liste)
      print "ANGMAS",ANGMAS  
      if ANGMAS["ANGL_REP"]==None :                                     
         __CARA=AFFE_CARA_ELEM(MODELE=__MO,MASSIF=_F(MAILLE='VOLUME',ANGL_EULER=ANGMAS["ANGL_EULER"]),);
      else :                                        
         __CARA=AFFE_CARA_ELEM(MODELE=__MO,MASSIF=_F(MAILLE='VOLUME',ANGL_REP=ANGMAS["ANGL_REP"]),);
         
# -- Chargement en deformation

  __E = [None]*nbsig

  __E[0] = AFFE_CHAR_MECA(MODELE = __MO,
    LIAISON_OBLIQUE = _F(NOEUD='P1', DX=1, ANGL_NAUT=ANGLE))
     
  __E[1] = AFFE_CHAR_MECA(MODELE = __MO,
    LIAISON_OBLIQUE = _F(NOEUD='P2', DY=1, ANGL_NAUT=ANGLE))
    
  if MODELISATION=="3D": 
     
      __E[2] = AFFE_CHAR_MECA(MODELE = __MO,
         LIAISON_OBLIQUE = _F(NOEUD='P3', DZ=1, ANGL_NAUT=ANGLE))
    
      __E[3] = AFFE_CHAR_MECA(MODELE = __MO,
        LIAISON_OBLIQUE = _F(NOEUD='P1', DY=1, ANGL_NAUT=ANGLE))
         
      __E[4] = AFFE_CHAR_MECA(MODELE = __MO,
        LIAISON_OBLIQUE = _F(NOEUD='P1', DZ=1, ANGL_NAUT=ANGLE))
         
      __E[5] = AFFE_CHAR_MECA(MODELE = __MO,
        LIAISON_OBLIQUE = _F(NOEUD='P2', DZ=1, ANGL_NAUT=ANGLE))
        
  else:
      c=Numeric.cos(ANGLE*Numeric.pi/180.)
      s=Numeric.sin(ANGLE*Numeric.pi/180.)
      __E[2] = AFFE_CHAR_MECA(MODELE = __MO,
        LIAISON_OBLIQUE = _F(NOEUD='P1', DY=1, ANGL_NAUT=ANGLE),)

# -- Chargement en contrainte

  __S = [None]*nbsig
  
  if MODELISATION=="3D": 
   
      r33 = 3**-0.5
      __S[0] = AFFE_CHAR_MECA(MODELE = __MO,FORCE_FACE = (
          _F(MAILLE='F1', FX=-1),
          _F(MAILLE='F4', FX= r33),))
         
      __S[1] = AFFE_CHAR_MECA(MODELE = __MO,FORCE_FACE = (
          _F(MAILLE='F2', FY=-1),
          _F(MAILLE='F4', FY= r33),))
         
      __S[2] = AFFE_CHAR_MECA(MODELE = __MO,FORCE_FACE = (
          _F(MAILLE='F3', FZ=-1),
          _F(MAILLE='F4', FZ= r33),))
         
      __S[3] = AFFE_CHAR_MECA(MODELE = __MO,FORCE_FACE = (
          _F(MAILLE='F1', FY=-1),
          _F(MAILLE='F2', FX=-1),
          _F(MAILLE='F4', FX= r33, FY=r33),))
         
      __S[4] = AFFE_CHAR_MECA(MODELE = __MO,FORCE_FACE = (
          _F(MAILLE='F1', FZ=-1),
          _F(MAILLE='F3', FX=-1),
          _F(MAILLE='F4', FX= r33, FZ=r33),))
         
      __S[5] = AFFE_CHAR_MECA( MODELE = __MO, FORCE_FACE = (
          _F(MAILLE='F2', FZ=-1),
          _F(MAILLE='F3', FY=-1),
          _F(MAILLE='F4', FY= r33, FZ=r33), ) )
     
  else:
      r22 = 2**-0.5
      __S[0] = AFFE_CHAR_MECA(MODELE = __MO,FORCE_CONTOUR = (
          _F(MAILLE='S1', FX=-1),
          _F(MAILLE='S3', FX= r22), ))
         
      __S[1] = AFFE_CHAR_MECA(MODELE = __MO,FORCE_CONTOUR = (
          _F(MAILLE='S2', FY=-1),
          _F(MAILLE='S3', FY= r22), ) )
         
      __S[2] = AFFE_CHAR_MECA(MODELE = __MO,FORCE_CONTOUR = (
          _F(MAILLE='S1', FY=-1),
          _F(MAILLE='S2', FX=-1),
          _F(MAILLE='S3', FX= r22, FY=r22), ) )
        
# -- Construction de la charge

  l_char = [  _F(CHARGE=__C_RIGIDE)  ]
  
  for i in xrange(nbsig) :
    ike=CMP_EPS[i]
    if EPS[ike]:
       l_char.append(  _F(CHARGE=__E[i],FONC_MULT=EPS[ike])  )
       
  for i in xrange(nbsig) :
    iks=CMP_SIG[i]
    l_char.append(  _F(CHARGE=__S[i],FONC_MULT=SIG[iks])  )

#  -- Materiau et modele
  __CHMAT=AFFE_MATERIAU(MAILLAGE=__MA,AFFE = _F(MAILLE='VOLUME',MATER=MATER))

# Etat initial
  SIGINI={}
  VARINI={}
  LCSIG=[]
  LVSIG=[]
  init_dico={}
  etatinit=0

# --contraintes initiales
  if SIGM_INIT:        
      etatinit=1                                                     
      SIGINI=SIGM_INIT[0].cree_dict_valeurs(SIGM_INIT[0].mc_liste)   
      for i in SIGINI.keys():                                        
          if SIGINI[i]!=None :                                       
              LCSIG.append(i)                                        
              LVSIG.append(SIGINI[i])                                

      __SIG_INIT=CREA_CHAMP(MAILLAGE=__MA,OPERATION='AFFE',TYPE_CHAM='CART_SIEF_R',                             
                AFFE=_F(TOUT='OUI', NOM_CMP=LCSIG, VALE=LVSIG,))                                
      init_dico['SIGM']=__SIG_INIT                                   
          
# --variables internes initiales
  if VARI_INIT:        
      etatinit=1
      lnomneu=[]
      lnomvar=[]
      VARINI=VARI_INIT[0].cree_dict_valeurs(VARI_INIT[0].mc_liste)
      nbvari=len(VARINI['VALE'])
      for i in range(nbvari):
          lnomneu.append('X'+str(i+1))
          lnomvar.append('V'+str(i+1))

      __NEUT=CREA_CHAMP(OPERATION='AFFE', TYPE_CHAM='CART_NEUT_R', MAILLAGE=__MA,
        AFFE=_F( MAILLE ='VOLUME', NOM_CMP = lnomneu, VALE = VARINI['VALE']))
        
      __VAR_INIT=CREA_CHAMP(MODELE=__MO,OPERATION='ASSE',TYPE_CHAM='ELGA_VARI_R',
                   ASSE=_F(TOUT='OUI',CHAM_GD=__NEUT,NOM_CMP=lnomneu,NOM_CMP_RESU=lnomvar))
      init_dico['VARI']=__VAR_INIT
          
  # --deformations initiales
  if EPSI_INIT:
      etatinit=1
      EPSINI={}
      LCDEPL=[]
      LNDEPL=[]
      LVDEPL=[]
      LIST_AFFE=[]
      mon_dico={}
      mon_dico["NOEUD"]='P0'
      mon_dico["NOM_CMP"]=("DX","DY","DZ")
      mon_dico["VALE"]=(0.,0.,0.)
      LIST_AFFE.append(mon_dico)
      
      EPSINI=EPSI_INIT[0].cree_dict_valeurs(EPSI_INIT[0].mc_liste)
      mon_dico={}
      mon_dico["NOEUD"]='P1'
      mon_dico["NOM_CMP"]='DX'
      mon_dico["VALE"]=EPSINI['EPXX']
      LIST_AFFE.append(mon_dico)
      mon_dico={}
      mon_dico["NOEUD"]='P2'
      mon_dico["NOM_CMP"]='DY'
      mon_dico["VALE"]=EPSINI['EPYY']
      LIST_AFFE.append(mon_dico)
      if MODELISATION=="3D": 
          mon_dico={}
          mon_dico["NOEUD"]='P3'
          mon_dico["NOM_CMP"]='DZ'
          mon_dico["VALE"]=EPSINI['EPZZ']
          LIST_AFFE.append(mon_dico)
          mon_dico={}
          mon_dico["NOEUD"]='P1'
          mon_dico["NOM_CMP"]='DY'
          mon_dico["VALE"]=EPSINI['EPXY']
          LIST_AFFE.append(mon_dico)
          mon_dico={}
          mon_dico["NOEUD"]='P2'
          mon_dico["NOM_CMP"]='DX'
          mon_dico["VALE"]=EPSINI['EPXY']
          LIST_AFFE.append(mon_dico)
          mon_dico={}
          mon_dico["NOEUD"]='P1'
          mon_dico["NOM_CMP"]='DZ'
          mon_dico["VALE"]=EPSINI['EPXZ']
          LIST_AFFE.append(mon_dico)
          mon_dico={}
          mon_dico["NOEUD"]='P3'
          mon_dico["NOM_CMP"]='DX'
          mon_dico["VALE"]=EPSINI['EPXZ']
          LIST_AFFE.append(mon_dico)
          mon_dico={}
          mon_dico["NOEUD"]='P2'
          mon_dico["NOM_CMP"]='DZ'
          mon_dico["VALE"]=EPSINI['EPYZ']
          LIST_AFFE.append(mon_dico)
          mon_dico={}
          mon_dico["NOEUD"]='P3'
          mon_dico["NOM_CMP"]='DY'
          mon_dico["VALE"]=EPSINI['EPYZ']
          LIST_AFFE.append(mon_dico)
      else:
          mon_dico={}
          mon_dico["NOEUD"]='P1',
          mon_dico["NOM_CMP"]='DY'
          mon_dico["VALE"]=EPSINI['EPXY']
          LIST_AFFE.append(mon_dico)
          mon_dico={}
          mon_dico["NOEUD"]='P2'
          mon_dico["NOM_CMP"]='DX'
          mon_dico["VALE"]=EPSINI['EPXY']
          LIST_AFFE.append(mon_dico)
      __DEP_INI=CREA_CHAMP(MAILLAGE=__MA,OPERATION='AFFE',TYPE_CHAM='NOEU_DEPL_R',AFFE=LIST_AFFE)
      init_dico['DEPL']=__DEP_INI
      
# -- Deroulement du calcul
  motscles={} 
  if   COMP_INCR  : 
      motscles['COMP_INCR']   = COMP_INCR.List_F()
  if   COMP_ELAS   : 
      motscles['COMP_ELAS']   = COMP_ELAS.List_F()
  motscles['CONVERGENCE'] = CONVERGENCE.List_F()
  motscles['NEWTON']      = NEWTON.List_F()
  if   RECH_LINEAIRE   : 
      motscles['RECH_LINEAIRE']      = RECH_LINEAIRE.List_F()
  motscles['INCREMENT']   = INCREMENT.List_F()
  
  if   SUIVI_DDL   : 
     motscles['SUIVI_DDL']   = SUIVI_DDL.List_F()
     
  if   ARCHIVAGE   : 
     motscles['ARCHIVAGE']   = ARCHIVAGE.List_F()

  if   etatinit == 1  : 

     if MASSIF:        
      __EVOL1 = STAT_NON_LINE(INFO = INFO,CARA_ELEM=__CARA,MODELE = __MO,CHAM_MATER = __CHMAT,
        ETAT_INIT=init_dico, EXCIT = l_char,**motscles)
     else:
      __EVOL1 = STAT_NON_LINE(INFO = INFO,MODELE = __MO,CHAM_MATER = __CHMAT,
        ETAT_INIT=init_dico, EXCIT = l_char,**motscles)

  else:  

     if MASSIF:        
         __EVOL1 = STAT_NON_LINE(INFO = INFO,MODELE = __MO, CARA_ELEM=__CARA,CHAM_MATER = __CHMAT,
                             EXCIT = l_char,**motscles)
     else:
         __EVOL1 = STAT_NON_LINE(INFO = INFO,MODELE = __MO, CHAM_MATER = __CHMAT,
                             EXCIT = l_char,**motscles)


  __EVOL1 = CALC_ELEM(reuse = __EVOL1,RESULTAT = __EVOL1,
    OPTION = ('SIEF_ELNO_ELGA','EPSI_ELNO_DEPL','VARI_ELNO_ELGA'))
    
  if MODELISATION=="3D":
      angles=(ANGLE,0,0)
      __EVOL=MODI_REPERE(RESULTAT=__EVOL1, MODI_CHAM=(
          _F(NOM_CHAM='DEPL',NOM_CMP=('DX','DY','DZ'),TYPE_CHAM='VECT_3D',),
          _F(NOM_CHAM='SIEF_ELNO_ELGA',NOM_CMP=('SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ'),TYPE_CHAM='TENS_3D',),
          _F(NOM_CHAM='EPSI_ELNO_DEPL',NOM_CMP=('EPXX','EPYY','EPZZ','EPXY','EPXZ','EPYZ'),TYPE_CHAM='TENS_3D',),
                              ),
                 DEFI_REPERE=_F(REPERE='UTILISATEUR',ANGL_NAUT=angles),);
  else :
      angles=ANGLE
      __EVOL=MODI_REPERE(RESULTAT=__EVOL1,MODI_CHAM=(
                 _F(NOM_CHAM='DEPL',NOM_CMP=('DX','DY'),TYPE_CHAM='VECT_2D',),
                 _F(NOM_CHAM='SIEF_ELNO_ELGA',NOM_CMP=('SIXX','SIYY','SIZZ','SIXY'),TYPE_CHAM='TENS_2D',),
                 _F(NOM_CHAM='EPSI_ELNO_DEPL',NOM_CMP=('EPXX','EPYY','EPZZ','EPXY'),TYPE_CHAM='TENS_2D',),
                              ),
                 DEFI_REPERE=_F(REPERE='UTILISATEUR',ANGL_NAUT=angles),);
  
# -- Recuperation des courbes

  __REP_VARI = POST_RELEVE_T(ACTION = (
      _F(INTITULE='VARI_INT',RESULTAT=__EVOL1,NOM_CHAM='VARI_ELNO_ELGA',
        TOUT_CMP='OUI',OPERATION='EXTRACTION',NOEUD='P0'),))

    
  __REP_EPSI = POST_RELEVE_T(ACTION = (
      _F(INTITULE='EPSILON',RESULTAT=__EVOL,NOM_CHAM='EPSI_ELNO_DEPL',
        TOUT_CMP='OUI',OPERATION='EXTRACTION',NOEUD     = 'P0'),))

  __REP_SIGM = POST_RELEVE_T(ACTION = (
      _F(INTITULE  = 'SIGMA',RESULTAT  =  __EVOL,NOM_CHAM  = 'SIEF_ELNO_ELGA',
        TOUT_CMP  = 'OUI',OPERATION = 'EXTRACTION',NOEUD     = 'P0'),))
    
  __REP_INV = POST_RELEVE_T(ACTION = (
      _F(INTITULE  = 'INV',RESULTAT  =  __EVOL,NOM_CHAM  = 'SIEF_ELNO_ELGA',
        INVARIANT  = 'OUI',OPERATION = 'EXTRACTION',NOEUD     = 'P0'),))
        
  __REP_INV=CALC_TABLE( TABLE=__REP_INV,reuse=__REP_INV,
           ACTION=_F(OPERATION='EXTR',NOM_PARA=('INST','TRACE','VMIS'), ) )

  self.DeclareOut('REPONSE',self.sd)
  
  REPONSE=CALC_TABLE( TABLE=__REP_EPSI,TITRE='TABLE ',ACTION=(
                   _F(OPERATION='COMB',TABLE=__REP_SIGM,NOM_PARA=('INST'), ),
                   _F(OPERATION='COMB',TABLE=__REP_INV ,NOM_PARA=('INST'), ),
                   _F(OPERATION='COMB',TABLE=__REP_VARI,NOM_PARA=('INST'), ),))

  return ier

