#@ MODIF test_compor_ops Macro  DATE 26/04/2011   AUTEUR SFAYOLLE S.FAYOLLE 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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

#              MACRO "TEST_COMPOR"
#           ----------------------------

#######################################################################
# utilitaires
#######################################################################
def PROD_ROT(X1,X2):
    # calcul Produit de 2 vecteurs pour les coef de rotations
    # dimension de X1 et X2 liste de 3 scalaire resultat liste de 6 scalaires
    if (len(X1)==len(X2)==3) : 
            Y=[None]*6
            V_ind=[[0,0,0.5],[1,1,0.5],[2,2,0.5],[0,1,1.0],[0,2,1.0],[1,2,1.0]]
            for ind in V_ind:
                    i=V_ind.index(ind)
                    ind1,ind2,coef=ind[0],ind[1],ind[2]
                    Y[i]=coef*(X1[ind1]*X2[ind2]+X1[ind2]*X2[ind1])
            return Y
    else :
            print "CALCUL PROD_ROT IMPOSSIBLE, dimensions innatendues"
            return None
#######################################################################
def RENOMME(self,i,N_pas,label_cal,ch_param,_RES,_RSI):
# On renomme les composantes en fonction de  l'ordre de discrétisation
   from Accas import _F
   DETRUIRE       = self.get_cmd('DETRUIRE')
   CALC_TABLE     = self.get_cmd('CALC_TABLE')
   N = N_pas[i]
   chN=label_cal[i]+str(N)
   for ch in ch_param:
           j=ch_param.index(ch)
           chnew=ch+chN
            ##Extraction par type de variable 
           if _RSI[j] == None:
                   _RSI[j]=CALC_TABLE( TABLE=_RES[i],
                                TITRE = ' ',
                                ACTION=( _F(OPERATION='EXTR',
                                       NOM_PARA=('INST',ch,),),
                                         _F(OPERATION='RENOMME',
                                           NOM_PARA=(ch,chnew,),),
                                                           ),);
           else:
                   TMP_S=CALC_TABLE( TABLE=_RES[i],
                                TITRE = ' ',
                                ACTION=( _F(OPERATION='EXTR',
                                       NOM_PARA=('INST',ch,),),
                                           _F(OPERATION='RENOMME',
                                           NOM_PARA=(ch,chnew,),),
                                                           ),);
                   _RSI[j]=CALC_TABLE( reuse=_RSI[j], TABLE=_RSI[j],
                                TITRE = ' ',
                                   ACTION=( _F(OPERATION='COMB',
                                   TABLE=TMP_S,NOM_PARA='INST',),
                                           ),);
                   DETRUIRE ( CONCEPT = _F (NOM = TMP_S,),INFO=1)
 
   
   return _RSI
#######################################################################

def ERREUR(X,Xref,prec_zero,coef):
    "calcul erreur relative entre deux nombres"
    if (abs(Xref)<prec_zero) : 
       err=0.
    else :
       err= abs((X*coef-Xref)/Xref)
    return err
#######################################################################

def TEST_ECART(self,ch_param2,label_cal,N_pas,Ncal,ch_param,_RSI,prec_ecart,prec_zero):
   from Accas import _F
   DETRUIRE       = self.get_cmd('DETRUIRE')
   FORMULE        = self.get_cmd('FORMULE')
   CALC_TABLE     = self.get_cmd('CALC_TABLE')

   #Exploitations
   CH_V1 = ['INST']
   C_Pa=1.e6
   _ERSI=[None]*len(ch_param2)

   for ch in ch_param2 :
   #CALCUL des ecarts relatifs
        i=ch_param2.index(ch)
        chref1 =ch + label_cal[4] + str(N_pas[4]) 
        chref2 = ch + label_cal[Ncal-1] + str(N_pas[Ncal-1])
        chref = [chref1, chref2]        
        preczero=prec_zero[i]   

        for j in range(Ncal) :
                coef = 1.
                ch_cal = ch + label_cal[j] + str(N_pas[j]) 
                ch_err = 'ER_' + ch_cal
                if j < 4 :
                        if (j==0 and i>0 and i<9) : coef = 1/C_Pa
                        iref = 0
                else :
                        iref =1
                        if (i==0) : CH_V1.append(ch_cal)
#               calcul de l'erreur (ecart relatif)
                valfor='ERREUR(%s,%s,%e,%f)' % (ch_cal,chref[iref],preczero,coef)
                nompar1='%s' % (ch_cal )
                nompar2='%s' % (chref[iref] )                
                ERR_REL = FORMULE(NOM_PARA=(nompar1,nompar2),VALE=valfor)
                if _ERSI[i] == None :
                   _ERSI[i] = CALC_TABLE(TABLE =_RSI[i], 
                                     TITRE = '_RSI'+str(j),
                             ACTION = (_F(OPERATION='OPER',NOM_PARA=ch_err,
                                          FORMULE=ERR_REL),
                                      ),);
                else :
                   _ERSI[i] = CALC_TABLE(TABLE =_ERSI[i], reuse=_ERSI[i],
                                     TITRE = '_RSI'+str(j),
                             ACTION = (_F(OPERATION='OPER',NOM_PARA=ch_err,
                                          FORMULE=ERR_REL),
                                      ),);
                DETRUIRE ( CONCEPT = _F (NOM = ERR_REL,),INFO=1)                
   return _ERSI
        
#######################################################################

def CHAR3D(self,POISSON,YOUNG,_tempsar,INFO):
   CALC_FONCTION       = self.get_cmd('CALC_FONCTION')
   DEFI_FONCTION       = self.get_cmd('DEFI_FONCTION')
   DEFI_LIST_REEL      = self.get_cmd('DEFI_LIST_REEL')
   IMPR_FONCTION       = self.get_cmd('IMPR_FONCTION')
   from Accas import _F
   import numpy as NP
   
   ##################################################################################
 
   # definition du trajet de chargement 3D
 
   #######################################################################
   #fonctions chargement
   calibrage = 3.5;
   Ctau = ((2 * calibrage) * ((1 + POISSON) / (2 * YOUNG)));
   Ctrac = (calibrage * (1 / YOUNG));
   _Ytrace=DEFI_LIST_REEL( VALE = (0., 150., 150., -50, 0., 50., -150., -150., 0.),)
   _Trace=DEFI_FONCTION(NOM_PARA='INST',
                       VALE_PARA= _tempsar,
                       VALE_FONC= _Ytrace,
                       PROL_DROITE='EXCLU',
                       PROL_GAUCHE='EXCLU',);

   _YDevi_1=DEFI_LIST_REEL( VALE = (0., 75., 150., 150, 0., -150., -150., -75., 0.),)
   _Devi_1=DEFI_FONCTION(NOM_PARA='INST',
                       VALE_PARA= _tempsar,
                       VALE_FONC= _YDevi_1,
                        PROL_DROITE='EXCLU',
                        PROL_GAUCHE='EXCLU',);

   _YDevi_2=DEFI_LIST_REEL( VALE = (0., 75., -50., 100, 0., -100., 50., -75., 0.),)
   _Devi_2=DEFI_FONCTION(NOM_PARA='INST',
                       VALE_PARA= _tempsar,
                       VALE_FONC= _YDevi_2,
                        PROL_DROITE='EXCLU',
                        PROL_GAUCHE='EXCLU',);

   _Ytauxy=DEFI_LIST_REEL( VALE = (0., 200., 100., 300, 0., -300., -100., -200., 0.),)
   _TAU_xy=DEFI_FONCTION(NOM_PARA='INST',
                       VALE_PARA= _tempsar,
                       VALE_FONC= _Ytauxy,
                        PROL_DROITE='EXCLU',
                        PROL_GAUCHE='EXCLU',);

   _Ytauxz=DEFI_LIST_REEL( VALE = (0., -100., 100., 200, 0., -200., -100., 100., 0.),)
   _TAU_xz=DEFI_FONCTION(NOM_PARA='INST',
                       VALE_PARA= _tempsar,
                       VALE_FONC= _Ytauxz,
                        PROL_DROITE='EXCLU',
                        PROL_GAUCHE='EXCLU',);

   _Ytauyz=DEFI_LIST_REEL( VALE = (0., 0., 200., -100, 0., 100., -200., 0., 0.),)
   _TAU_yz=DEFI_FONCTION(NOM_PARA='INST',
                       VALE_PARA= _tempsar,
                       VALE_FONC= _Ytauyz,
                        PROL_DROITE='EXCLU',
                        PROL_GAUCHE='EXCLU',);

   _eps_xy=CALC_FONCTION(COMB=_F(FONCTION=_TAU_xy,
                                COEF=Ctau,),);

   _eps_xz=CALC_FONCTION(COMB=_F(FONCTION=_TAU_xz,
                                COEF=Ctau,),);

   _eps_yz=CALC_FONCTION(COMB=_F(FONCTION=_TAU_yz,
                                COEF=Ctau,),);

   _eps_xx=CALC_FONCTION(COMB=(_F(FONCTION=_Trace,
                                 COEF=Ctrac,),
                              _F(FONCTION=_Devi_1,
                                 COEF=Ctrac,),),);

   _eps_yy=CALC_FONCTION(COMB=(_F(FONCTION=_Trace,
                                 COEF=Ctrac,),
                              _F(FONCTION=_Devi_1,
                                 COEF=-(Ctrac),),
                              _F(FONCTION=_Devi_2,
                                 COEF=Ctrac,),),);

   _eps_zz=CALC_FONCTION(COMB=(_F(FONCTION=_Trace,
                                 COEF=Ctrac,),
                              _F(FONCTION=_Devi_2,
                                 COEF=-(Ctrac),),),);
   eps_imp = [_eps_xx, _eps_yy, _eps_zz, _eps_xy, _eps_xz, _eps_yz]
   #rotation tenseur des def
   #angle de precession, nutation, rotation propre
   psi,teta,phi=0.9,0.7,0.4
   cps,cte,cph=NP.cos(psi),NP.cos(teta),NP.cos(phi)
   sps,ste,sph=NP.sin(psi),NP.sin(teta),NP.sin(phi)
   #matrice de passage
   p11,p21,p31 = cph*cps-sph*cte*sps,cph*sps+sph*cte*cps,sph*ste
   p12,p22,p32 = -sph*cps-cph*cte*sps,-sph*sps+cph*cte*cps,cph*ste
   p13,p23,p33 = ste*sps,-ste*cps,cte
   V1= [p11, p21, p31]
   V2= [p12, p22, p32]
   V3= [p13, p23, p33]
   #eps apres rotation
   VI = [[V1,V1],[V2,V2],[V3,V3],[V1,V2],[V1,V3],[V2,V3]]
   _eps_rot = [None]*6
   for vect_i in VI:
           i = VI.index(vect_i)
           V_COEF = PROD_ROT(vect_i[0],vect_i[1])
           _eps_rot[i] =CALC_FONCTION(COMB=(
                           _F(FONCTION=_eps_xx,COEF=V_COEF[0],),
                           _F(FONCTION=_eps_yy,COEF=V_COEF[1],),
                           _F(FONCTION=_eps_zz,COEF=V_COEF[2],),
                           _F(FONCTION=_eps_xy,COEF=V_COEF[3],),
                           _F(FONCTION=_eps_xz,COEF=V_COEF[4],),
                           _F(FONCTION=_eps_yz,COEF=V_COEF[5],),
                                   ),);
   #eps apres symetrie
   eps_sym = [_eps_zz,_eps_xx,_eps_yy,_eps_xz,_eps_yz,_eps_xy]

   V_EPS = [eps_imp,eps_sym,_eps_rot]
   #trace chargement
   if INFO == 2 :
      IMPR_FONCTION(#FORMAT='XMGRACE',PILOTE='INTERACTIF',
                 COURBE=(_F(FONCTION=eps_imp[0],),
                         _F(FONCTION=eps_imp[1],),
                         _F(FONCTION=eps_imp[2],),
                         _F(FONCTION=eps_imp[3],),
                         _F(FONCTION=eps_imp[4],),
                         _F(FONCTION=eps_imp[5],),
                         ),
                 UNITE=29,);
   return eps_imp
        
#######################################################################
def CHAR2D(self,POISSON,YOUNG,_tempsar,INFO):
   CALC_FONCTION       = self.get_cmd('CALC_FONCTION')
   DEFI_FONCTION       = self.get_cmd('DEFI_FONCTION')
   DEFI_LIST_REEL      = self.get_cmd('DEFI_LIST_REEL')
   IMPR_FONCTION       = self.get_cmd('IMPR_FONCTION')
   #######################################################################
   # definition du trajet de chargement 2D
   #######################################################################
   from Accas import _F
   import numpy as NP

   #fonctions chargement
   calibrage = 4.5;
   Ctau = ((2 * calibrage) * ((1 + POISSON) / (2 * YOUNG)));
   Ctrac = (calibrage * (1 / YOUNG));
   YTrace2d=DEFI_LIST_REEL( VALE = (0., 150., 200., 300., 0., -300., -200., -150., 0.),)
   Trace2d=DEFI_FONCTION(NOM_PARA='INST',
                       VALE_PARA= _tempsar,
                       VALE_FONC= YTrace2d,
                       PROL_DROITE='EXCLU',
                       PROL_GAUCHE='EXCLU',);

   Y_Devi2=DEFI_LIST_REEL( VALE = (0., 0., 100., 0., 0., 0., -100., 0., 0.),)
   Devi1_2d=DEFI_FONCTION(NOM_PARA='INST',
                       VALE_PARA= _tempsar,
                       VALE_FONC= Y_Devi2,
                        PROL_DROITE='EXCLU',
                        PROL_GAUCHE='EXCLU',);

   Y_tauxy2=DEFI_LIST_REEL( VALE = (0., 100., 40., 0., 0., 0., -40., -100., 0.0),)
   TAU_xy2=DEFI_FONCTION(NOM_PARA='INST',
                       VALE_PARA= _tempsar,
                       VALE_FONC= Y_tauxy2,
                        PROL_DROITE='EXCLU',
                        PROL_GAUCHE='EXCLU',);

   eps_xy2=CALC_FONCTION(COMB=_F(FONCTION=TAU_xy2,
                                COEF=Ctau,),);

   eps_xx2=CALC_FONCTION(COMB=(_F(FONCTION=Trace2d,
                                 COEF=Ctrac,),
                              _F(FONCTION=Devi1_2d,
                                 COEF=Ctrac,),),);

   eps_yy2=CALC_FONCTION(COMB=(_F(FONCTION=Trace2d,
                                 COEF=Ctrac,),
                              _F(FONCTION=Devi1_2d,
                                 COEF=-(Ctrac),),),);
   #rotation tenseur des def
   angle = 0.9
   c,s = NP.cos(0.9) , NP.sin(0.9)
   c2,s2,cs = c*c, s*s, c*s
   eps_rr2=CALC_FONCTION(COMB=(_F(FONCTION=eps_xx2,
                                   COEF=c2,),
                             _F(FONCTION=eps_yy2,
                                   COEF=s2,),
                             _F(FONCTION=eps_xy2,
                                   COEF=2*cs,),),);

   eps_tt2=CALC_FONCTION(COMB=(_F(FONCTION=eps_xx2,
                                   COEF=s2,),
                             _F(FONCTION=eps_yy2,
                                   COEF=c2,),
                             _F(FONCTION=eps_xy2,
                                   COEF=-2*cs,),),);

   eps_rt2=CALC_FONCTION(COMB=(_F(FONCTION=eps_xx2,
                                   COEF=-cs,),
                             _F(FONCTION=eps_yy2,
                                   COEF=cs,),
                             _F(FONCTION=eps_xy2,
                                   COEF=c2-s2,),),);
   eps_imp = [eps_xx2 , eps_yy2, eps_xy2, eps_rr2,eps_tt2, eps_rt2]

   #Trace2d chargement
   if INFO==2 :
      IMPR_FONCTION(#FORMAT='XMGRACE',PILOTE='INTERACTIF',
                 COURBE=(_F(FONCTION=eps_imp[0],),
                         _F(FONCTION=eps_imp[1],),
                         _F(FONCTION=eps_imp[2],),
                         _F(FONCTION=eps_imp[3],),
                         _F(FONCTION=eps_imp[4],),
                         _F(FONCTION=eps_imp[5],),
                         ),
                 UNITE=29,);
   return eps_imp
        

#######################################################################



def test_compor_ops(self,OPTION,NEWTON,CONVERGENCE,COMP_INCR,COMP_ELAS,LIST_MATER,VARI_TEST,INFO,
                             **args):
 # seule l'option "THER", c'est à dire le test thermomecanique est programmé à ce jour
 # ajouter l'option MECA (tests comp001,002), l'option HYDR, etc..
  from Accas import _F
  import numpy as NP
  from Utilitai.veri_matr_tang import VERI_MATR_TANG
  self.update_const_context({'ERREUR' : ERREUR})
  
  ier=0
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)
  
  # Le concept sortant (de type fonction) est nomme U dans 
  # le contexte de la macro

  self.DeclareOut('U',self.sd)
  
  # On importe les definitions des commandes a utiliser dans la macro
  CALC_FONCTION  = self.get_cmd('CALC_FONCTION')
  CALC_TABLE     = self.get_cmd('CALC_TABLE')
  DEFI_CONSTANTE = self.get_cmd('DEFI_CONSTANTE')
  DEFI_FONCTION  = self.get_cmd('DEFI_FONCTION')
  DEFI_LIST_INST = self.get_cmd('DEFI_LIST_INST')
  DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')
  DEFI_MATERIAU  = self.get_cmd('DEFI_MATERIAU')
  DETRUIRE       = self.get_cmd('DETRUIRE')
  FORMULE        = self.get_cmd('FORMULE')
  IMPR_FONCTION  = self.get_cmd('IMPR_FONCTION')
  SIMU_POINT_MAT = self.get_cmd('SIMU_POINT_MAT')
  TEST_TABLE     = self.get_cmd('TEST_TABLE')
  IMPR_TABLE     = self.get_cmd('IMPR_TABLE')
  CALC_TABLE     = self.get_cmd('CALC_TABLE')

  motscles={}
  if   COMP_INCR  :
      motscles['COMP_INCR']   = COMP_INCR.List_F()
  if   COMP_ELAS   :
      motscles['COMP_ELAS']   = COMP_ELAS.List_F()
  motscles['CONVERGENCE'] = CONVERGENCE.List_F()  
  motscles['NEWTON']      = NEWTON.List_F()
  
  if OPTION=="THER" :
      epsi = 1.E-10
      MATER=args['MATER']
      ALPHA=args['ALPHA']
      YOUNG=args['YOUNG']
      TEMP_INIT=args['TEMP_INIT']
      TEMP_FIN=args['TEMP_FIN']
      NB_VARI=args['NB_VARI']

      if args['INST_FIN'] != None : INST_FIN=args['INST_FIN']
 
      NCAL=len(LIST_MATER)
 
      _LINST0=DEFI_LIST_REEL(DEBUT=0.,
                          INTERVALLE=(_F(JUSQU_A=INST_FIN,
                                         NOMBRE=NCAL,),
                                         ),
                          );

      _LINST=DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=_LINST0,
                                         METHODE='MANUEL',
                                        # PAS_MINI=1.0E-12
                                         ),
                             ECHEC=_F(SUBD_PAS=10),
                           # ADAPTATION=_F(EVENEMENT='SEUIL'),
                            );
      _TIMP=DEFI_FONCTION(NOM_PARA='INST',  NOM_RESU='TEMP',
                         VALE=(  0. , TEMP_INIT, INST_FIN , TEMP_FIN)
                         )

      _zero=DEFI_CONSTANTE(VALE=0.)

 
      _U=SIMU_POINT_MAT(MATER=MATER,INFO=INFO,
                       SUPPORT='ELEMENT',
                         AFFE_VARC=(
                         _F(  NOM_VARC='TEMP',
                              VALE_FONC=_TIMP,
                              VALE_REF=TEMP_INIT),
                              ),
                       INCREMENT=_F(LIST_INST=_LINST,),
                       EPSI_IMPOSE=_F(EPXX=_zero,),
                       **motscles);

      # On ne garde pas les valeurs initiales (NUME_ORDRE = 0 exclu)

      _U = CALC_TABLE(reuse =_U,
                    TABLE=_U,
                    ACTION=(_F(
                          OPERATION = 'FILTRE',
                          NOM_PARA  = 'NUME_ORDRE',
                          CRIT_COMP = 'GT',
                          VALE    = 0,
                         ),),);

      SXM = 0.
      EXM = 0.
      time = 0.

      _RES   = [None]*(NCAL)
      if (NB_VARI > 0):
         Vim    = NP.zeros(NB_VARI)

      for i in range(NCAL):
 
          timem = time
 
          time = timem + INST_FIN/NCAL
 
          Ti = TEMP_INIT + time/INST_FIN * (TEMP_FIN - TEMP_INIT)
 
          Tm = TEMP_INIT + timem/INST_FIN * (TEMP_FIN - TEMP_INIT)
 
          # deformation mecanique imposee correspondant a la deformation thermique du premier calcul
 
          _epsimp =DEFI_CONSTANTE(VALE=-ALPHA(Ti)*(Ti - TEMP_INIT));

          # variation des coef du COMPORtement avec la temperature
          # correction eventuelle des valeurs initiales du temps ti
 
          if i > 0 :
 
             SXM = SXM *(YOUNG(Ti)/YOUNG(Tm))
             # cas particuliers
             if COMP_INCR.List_F()[0]['RELATION'] == 'VMIS_CINE_LINE' :
                 
                 if args['D_SIGM_EPSI'] != None : 
                    D_SIGM_EPSI=args['D_SIGM_EPSI']
                 else :
                    raise 'erreur'
                    
                 Vim[0:5] = Vim[0:5]*D_SIGM_EPSI(Ti)/D_SIGM_EPSI(Tm)
                 
             if COMP_INCR.List_F()[0]['RELATION']== 'VMIS_ECMI_LINE' :
                 if args['C_PRAG'] != None : 
                    C_PRAG=args['C_PRAG']
                 else :
                    raise 'erreur'
                 Vim[2:7] = Vim[2:7]*C_PRAG(Ti)/C_PRAG(Tm)
                 
             if COMP_INCR.List_F()[0]['RELATION']== 'VMIS_ECMI_TRAC' :
                 if args['C_PRAG'] != None : 
                    C_PRAG=args['C_PRAG']
                 else :
                    raise 'erreur'
                 Vim[2:7] = Vim[2:7]*C_PRAG(Ti)/C_PRAG(Tm)
 

          _list0 = DEFI_LIST_REEL(DEBUT=timem,
                      INTERVALLE=(_F(JUSQU_A=time,NOMBRE=1,),),);
 
          _list=DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=_list0,
                                         METHODE='MANUEL',
                                        # PAS_MINI=1.0E-12
                                         ),
                             ECHEC=_F(SUBD_PAS=10),
                           # ADAPTATION=_F(EVENEMENT='SEUIL'),
                            );
          if (NB_VARI > 0):
              _RES[i]=SIMU_POINT_MAT(INFO=INFO,
                                 MATER=LIST_MATER[i],
                                 SUPPORT='ELEMENT',
                                 INCREMENT=_F(LIST_INST = _list, ),
                                 EPSI_IMPOSE=_F(EPXX=_epsimp),
                                 SIGM_INIT=_F(SIXX=SXM),
                                 VARI_INIT=_F(VALE=[Vim[j] for j in range(NB_VARI)]),
                                 EPSI_INIT=_F(EPXX=EXM, EPYY=0.,EPZZ=0.,EPXY=0.,EPXZ=0.,EPYZ=0.),
                                 **motscles);
 
          else :
              _RES[i]=SIMU_POINT_MAT(INFO=INFO,
                                 MATER=LIST_MATER[i],
                                 SUPPORT='ELEMENT',
                                 INCREMENT=_F(LIST_INST = _list, ),
                                 EPSI_IMPOSE=_F(EPXX=_epsimp),
                                 SIGM_INIT=_F(SIXX=SXM),
                                 EPSI_INIT=_F(EPXX=EXM, EPYY=0.,EPZZ=0.,EPXY=0.,EPXZ=0.,EPYZ=0.),
                                 **motscles);
 
          # On ne teste pas les valeurs initiales (NUME_ORDRE = 0 exclu)

          _RES[i] = CALC_TABLE(reuse =_RES[i],
                    TABLE=_RES[i],
                    ACTION=(_F(
                          OPERATION = 'FILTRE',
                          NOM_PARA  = 'NUME_ORDRE',
                          CRIT_COMP = 'GT',
                          VALE    = 0,
                         ),),);
 
          # recuperation des valeurs initiales du futur pas de temps dans la table resultat
 
          EXM = _RES[i]['EPXX',1]
 
          SXM = _RES[i]['SIXX',1]

          if (NB_VARI > 0):
             for j in range(NB_VARI):
                Vim[j] = _RES[i]['V'+str(j+1),1]
 
          DETRUIRE ( CONCEPT =  _F (NOM =_epsimp),INFO=1);
          DETRUIRE ( CONCEPT =  _F (NOM =_list),INFO=1);
 
          

          TEST_TABLE(TABLE=_RES[i],
                     NOM_PARA='VMIS',VALE=_U['VMIS',i+1],
                     FILTRE=_F(NOM_PARA='INST',VALE=time),
                     REFERENCE='AUTRE_ASTER',);

          TEST_TABLE(TABLE=_RES[i],
                     NOM_PARA='TRACE',VALE=_U['TRACE',i+1],
                     FILTRE=_F(NOM_PARA='INST',VALE=time),
                     REFERENCE='AUTRE_ASTER',);
          if (NB_VARI > 0):
             if VARI_TEST <> None  :
                for j in range(len(VARI_TEST)):
                   nomvari=VARI_TEST[j]
                   if abs(_U[nomvari,i+1]) > epsi :
                      TEST_TABLE(TABLE=_RES[i],
                         NOM_PARA=nomvari,VALE=_U[nomvari,i+1],
                         FILTRE=_F(NOM_PARA='INST',VALE=time),
                         REFERENCE='AUTRE_ASTER',);
             else  :
                for j in range(NB_VARI):
                   nomvari='V'+str(j+1)
                   if abs(_U[nomvari,i+1]) > epsi :
                      TEST_TABLE(TABLE=_RES[i],
                         NOM_PARA=nomvari,VALE=_U[nomvari,i+1],
                         FILTRE=_F(NOM_PARA='INST',VALE=time),
                         REFERENCE='AUTRE_ASTER',);
      for i in range(NCAL):
          DETRUIRE ( CONCEPT =  _F (NOM =_RES[i]),INFO=1)
      
  elif OPTION=="MECA" :

      LIST_NPAS=args['LIST_NPAS']
      YOUNG=args['YOUNG']
      POISSON=args['POISSON']

      #Discretisation du calcul
      if args['LIST_NPAS'] != None : 
         LIST_NPAS=args['LIST_NPAS']
      else :
         LIST_NPAS=4*[1] + [1, 5, 25]             
      Ncal = len(LIST_NPAS)

      # les differents calculs effectues et les precisions sur chaque TEST_RESU
      label_cal=['_Pa_','_Th_','_sym_','_rot_'] + 6* ['_N']
      if args['LIST_TOLE'] != None : 
         LIST_TOLE=args['LIST_TOLE']
      else :
         LIST_TOLE=4*[1.E-10] + [1.E-1] + (Ncal-5)*[1.E-2] + [1.E-8]  
                  
      if args['PREC_ZERO'] != None : 
         PREC_ZERO=args['PREC_ZERO']
      else :
         PREC_ZERO=len(VARI_TEST)*[1.E-10]

      prec_tgt=LIST_TOLE[-1]

      #parametres vitesse de sollicitation
      t_0 = 1.0
      if   COMP_INCR  :
         if COMP_INCR.List_F()[0]['RELATION'][0:4]== 'VISC' :
            vitesse = 1.e-5
            t_0 = 5.e-2/(8.0*vitesse)
      # liste d'archivage
      _tempsar=DEFI_LIST_REEL( VALE =[t_0*i for i in range(9)],)
      
      if args['MODELISATION'] != None :
         modelisation=args['MODELISATION']
      else :
         modelisation= "3D"
      if modelisation == "3D" : 

          ##################################################################################
          #  TEST 3D
          ##################################################################################
          
          eps_imp = CHAR3D(self,POISSON,YOUNG,_tempsar,INFO)

          ch_param2 = list(VARI_TEST)
          ch_param=ch_param2+['SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ']

      elif modelisation == "C_PLAN"  : 
      
          ##################################################################################
          #  TEST 2D C_PLAN
          ##################################################################################
          
          eps_imp = CHAR2D(self,POISSON,YOUNG,_tempsar,INFO)

          # les quantites (invariants...) sur lequels portent les calculs d'erreur et les test_resu
          ch_param2 = list(VARI_TEST)
          ch_param=ch_param2+['SIXX','SIYY','SIZZ','SIXY']


      ###############################################################################
      #Discretisation et calcul
      ###############################################################################

      _RES=[None]*Ncal
      _RSI=[None]*len(ch_param)
      TMP_S=[None]

      #pointeur materiau
      P_imat = [0] + [1] + (Ncal-2)*[1]
      #pointeur deformation
      nomdef, symdef, rotdef = [0, 1, 2], [1, 0, 2], [3, 4, 5]
      P_idef = [nomdef, nomdef,symdef, rotdef]
      for i in range(6): P_idef.append(nomdef)

      ldicoeps=[]
      dicoeps={}
      dicoeps['EPXX']=eps_imp[0]
      dicoeps['EPYY']=eps_imp[1]
      dicoeps['EPZZ']=eps_imp[2]
      dicoeps['EPXY']=eps_imp[3]
      if modelisation == "3D" :
         dicoeps['EPXZ']=eps_imp[4]
         dicoeps['EPYZ']=eps_imp[5]
      ldicoeps.append(dicoeps)               
      motscles['EPSI_IMPOSE']=ldicoeps

      if args['SUPPORT'] != None :
         if modelisation == "C_PLAN" : 
            motscles['MODELISATION']='C_PLAN'
            motscles['SUPPORT']='ELEMENT'            
         else :   
            motscles['SUPPORT']=args['SUPPORT']     
         
      #Boucle sur l'ensemble des calculs
      for i in range(Ncal):
              N = LIST_NPAS[i]
              imat = P_imat[i]
              idef = P_idef[i]
              temps=DEFI_LIST_REEL(DEBUT=0.0,
                           INTERVALLE=(_F(JUSQU_A=t_0,NOMBRE=N,),
                                       _F(JUSQU_A=2.0*t_0,NOMBRE=N,),
                                       _F(JUSQU_A=3.0*t_0,NOMBRE=N,),
                                       _F(JUSQU_A=4.0*t_0,NOMBRE=N,),
                                       _F(JUSQU_A=5.0*t_0,NOMBRE=N,),
                                       _F(JUSQU_A=6.0*t_0,NOMBRE=N,),
                                       _F(JUSQU_A=7.0*t_0,NOMBRE=N,),
                                       _F(JUSQU_A=8.0*t_0,NOMBRE=N,),),);

              _DEFLIST =DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST = temps,),
                            ECHEC=_F(SUBD_METHODE='EXTRAPOLE',
                                     SUBD_PAS=10,
                                     SUBD_NIVEAU=10,),)
      #       Resout le pb a deformation imposee
              _RES[i]=SIMU_POINT_MAT(INFO=INFO,
                         MATER      = LIST_MATER[imat],
                         ARCHIVAGE = _F(LIST_INST = _tempsar),
                         INCREMENT=_F(LIST_INST=_DEFLIST,),
                         **motscles
                              );
      
      #       On renomme les composantes en fonction de  l'ordre de discretisation
              _RSI=RENOMME(self,i,LIST_NPAS,label_cal,ch_param,_RES,_RSI)
              
              DETRUIRE ( CONCEPT = _F (NOM = temps,),INFO=1)
 
      # TEST_RESU sur les erreurs relatives
      # les quantites (invariants...) sur lequels portent les calculs d'erreur et les test_resu
      _RSI=TEST_ECART(self,ch_param2,label_cal,LIST_NPAS,Ncal,ch_param,_RSI,LIST_TOLE,PREC_ZERO)
      
      for ch in ch_param2 :
        i=ch_param2.index(ch)
        if INFO==2 : IMPR_TABLE(TABLE=_RSI[i]);
        for j in range(Ncal) :
          ch_cal = ch + label_cal[j] + str(LIST_NPAS[j])
          ch_err = 'ER_' + ch_cal
          TEST_TABLE(TABLE=_RSI[i],NOM_PARA=ch_err,
            TYPE_TEST='MAX',VALE=0.,CRITERE='ABSOLU',PRECISION=LIST_TOLE[j],REFERENCE='ANALYTIQUE',);

      ###############################################################################
      # Test de la matrice tangente sur le calcul le plus fin
      ###############################################################################

      N = LIST_NPAS[Ncal-1]
      _Linst=DEFI_LIST_REEL(DEBUT=0.0,
                           INTERVALLE=(_F(JUSQU_A=t_0,NOMBRE=N,),
                                       _F(JUSQU_A=2.0*t_0,NOMBRE=N,),
                                       _F(JUSQU_A=3.0*t_0,NOMBRE=N,),
                                       _F(JUSQU_A=4.0*t_0,NOMBRE=N,),
                                       _F(JUSQU_A=5.0*t_0,NOMBRE=N,),
                                       _F(JUSQU_A=6.0*t_0,NOMBRE=N,),
                                       _F(JUSQU_A=7.0*t_0,NOMBRE=N,),
                                       _F(JUSQU_A=8.0*t_0,NOMBRE=N,),),);

      if   COMP_INCR  :
          motscles['COMP_INCR'][0]['TYPE_MATR_TANG'] = 'VERIFICATION'
          if args['VERI_MATR_OPTION'] is not None :
              motscles['COMP_INCR'][0]['VALE_PERT_RELA']=args['VERI_MATR_OPTION'].List_F()[0]['VALE_PERT_RELA']
          _DEFLIS2 =DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST = _Linst,),
                            ECHEC=_F(SUBD_METHODE='EXTRAPOLE',
                                     SUBD_PAS=10,
                                     SUBD_NIVEAU=10,),)
      #       Resout le pb a deformation imposee
          U=SIMU_POINT_MAT(INFO=INFO,
                         MATER      = LIST_MATER[imat],
                         ARCHIVAGE = _F(LIST_INST = _tempsar),
                         INCREMENT=_F(LIST_INST=_DEFLIS2,),
                         **motscles
                              );
          motscles={}
          if args['VERI_MATR_OPTION'] is not None :
              motscles['PRECISION']=args['VERI_MATR_OPTION'].List_F()[0]['PRECISION']
              motscles['PREC_ZERO']=args['VERI_MATR_OPTION'].List_F()[0]['PREC_ZERO']

          _DIFFMAT=VERI_MATR_TANG(**motscles)

          TEST_TABLE(TABLE=_DIFFMAT,
                     NOM_PARA='MAT_DIFF',
                     TYPE_TEST='MAX',
                     VALE=0.,
                     CRITERE='ABSOLU',
                     PRECISION=prec_tgt,
                     REFERENCE='ANALYTIQUE',);

          if INFO==2 : IMPR_TABLE(TABLE=_DIFFMAT)
  
  return ier

