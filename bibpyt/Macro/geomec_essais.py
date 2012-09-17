#@ MODIF geomec_essais Macro  DATE 17/09/2012   AUTEUR COURTOIS M.COURTOIS 

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

from geomec_utils import *

# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
#               Essai Triaxial Monotone Draine (TD)
# -----------------------------------------------------------------------
# -----------------------------------------------------------------------

def essai_TD(self,str_n_essai,DicoEssai,MATER,COMP_INCR,CONVERGENCE,INFO):
  """
  Essai Triaxial Monotone Draine (TD)
  """  
  import numpy as NP
  from Accas import _F
  import aster

  DEFI_FONCTION  = self.get_cmd('DEFI_FONCTION')
  SIMU_POINT_MAT = self.get_cmd('SIMU_POINT_MAT')
  DETRUIRE       = self.get_cmd('DETRUIRE')
  CREA_TABLE     = self.get_cmd('CREA_TABLE')
  IMPR_TABLE     = self.get_cmd('IMPR_TABLE')
  DEFI_LIST_INST = self.get_cmd('DEFI_LIST_INST')
  DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')

  PRES_CONF   = DicoEssai['PRES_CONF']
  EPSI_IMPOSE = DicoEssai['EPSI_IMPOSE']
  NB_INST     = DicoEssai['NB_INST']

  # dict permettant la gestion des graphiques
  Courbes  = {}
  NomsFich = {}
  Leg_x    = {}
  Leg_y    = {}

  # dict permettant la gestion des tables en sortie
  Resu_Essai            = {}
  Resu_Essai['INST']    = [[] for k in xrange(len(PRES_CONF))]
  Resu_Essai['EPS_AXI'] = [[] for k in xrange(len(PRES_CONF))]
  Resu_Essai['EPS_LAT'] = [[] for k in xrange(len(PRES_CONF))]
  Resu_Essai['EPS_VOL'] = [[] for k in xrange(len(PRES_CONF))]
  Resu_Essai['SIG_AXI'] = [[] for k in xrange(len(PRES_CONF))]
  Resu_Essai['SIG_LAT'] = [[] for k in xrange(len(PRES_CONF))]
  Resu_Essai['P']       = [[] for k in xrange(len(PRES_CONF))]
  Resu_Essai['Q']       = [[] for k in xrange(len(PRES_CONF))]

  # preparation des graphiques 
  str_fich = "Essai_TD_"+str_n_essai+"_"
  preparer_graphique('2',DicoEssai,str_fich,Courbes,NomsFich,\
                     Leg_x,Leg_y,{},{})

  # Creation de la liste d'instants
  __RLIST = DEFI_LIST_REEL(DEBUT = 0.,
                           INTERVALLE = _F(JUSQU_A = 100.,
                                           NOMBRE  = NB_INST,),
                           INFO = INFO)

  __DLIST = DEFI_LIST_INST(DEFI_LIST = _F(LIST_INST = __RLIST),
                           ECHEC=_F(SUBD_METHODE = 'MANUEL',
                                    SUBD_PAS     = 10,
                                    SUBD_NIVEAU  = 10,),
                           INFO = INFO,)

  # ---
  # Boucle sur les pressions de confinement PRES_CONF
  # --- 
  for i in xrange(len(PRES_CONF)):

    affiche_infos_essai(str_n_essai,"TD",PRES_CONF[i],EPSI_IMPOSE[i])
    
    # ---
    # Definition des chargements                           
    # ---
    __CHAR1 = DEFI_FONCTION(INFO = INFO, NOM_PARA = 'INST',
                            VALE = (0., 0., 100., EPSI_IMPOSE[i],),)
    
    __CHAR2 = DEFI_FONCTION(INFO = INFO, NOM_PARA = 'INST',
                            VALE = (0., PRES_CONF[i], 100., PRES_CONF[i],),)

    # ---
    # Calcul                                                                       
    # ---
    __EVOL = SIMU_POINT_MAT(
             INFO=INFO,
             COMP_INCR=COMP_INCR.List_F(),
             CONVERGENCE=CONVERGENCE.List_F(),
             MATER=MATER,
             INCREMENT=_F(LIST_INST=__DLIST,
                          INST_INIT=0.,
                          INST_FIN =100.,),
             NEWTON=_F(MATRICE='TANGENTE', REAC_ITER=1,),
             ARCHIVAGE=_F(LIST_INST=__RLIST,),
             SIGM_IMPOSE=_F(SIXX=__CHAR2,
                            SIYY=__CHAR2,),
             EPSI_IMPOSE=_F(EPZZ=__CHAR1,),
             SIGM_INIT  =_F(SIXX=PRES_CONF[i],
                            SIYY=PRES_CONF[i],
                            SIZZ=PRES_CONF[i],),
             EPSI_INIT  =_F(EPXX=0,
                            EPYY=0,
                            EPZZ=0,
                            EPXY=0,
                            EPXZ=0,
                            EPYZ=0,),)

    # ---
    # Post-traitements
    # ---
    TabRes  = __EVOL.EXTR_TABLE().values()
    sig_xx  = NP.array(TabRes['SIXX'])
    sig_yy  = NP.array(TabRes['SIYY'])
    sig_zz  = NP.array(TabRes['SIZZ'])
    eps_xx  = NP.array(TabRes['EPXX'])
    eps_yy  = NP.array(TabRes['EPYY'])
    eps_zz  = NP.array(TabRes['EPZZ'])
    inst    = TabRes['INST']
    eps_vol = eps_xx+eps_yy+eps_zz
    p       = (sig_xx+sig_yy+sig_zz)/3.
    q       = abs(sig_zz-sig_xx)

    # remplissage des graphiques
    str_leg = "PRES_CONF = " + str("%E"%(PRES_CONF[i])) + ", EPSI_IMPOSE = "\
                             + str("%E"%(EPSI_IMPOSE[i]))
    remplir_graphique(DicoEssai,Courbes,list(p),list(q),str_leg,"P-Q")
    remplir_graphique(DicoEssai,Courbes,list(eps_zz),list(q),str_leg,"EPS_AXI-Q")
    remplir_graphique(DicoEssai,Courbes,list(eps_zz),list(eps_vol),str_leg,"EPS_AXI-EPS_VOL")

    # stockage pour ecriture dans les tables 
    Resu_Essai['INST'][i]    = inst
    Resu_Essai['EPS_AXI'][i] = list(eps_zz )
    Resu_Essai['EPS_LAT'][i] = list(eps_xx )
    Resu_Essai['EPS_VOL'][i] = list(eps_vol)
    Resu_Essai['SIG_AXI'][i] = list(sig_zz )
    Resu_Essai['SIG_LAT'][i] = list(sig_xx )
    Resu_Essai['P'][i]       = list(p      )
    Resu_Essai['Q'][i]       = list(q      )

    DETRUIRE(CONCEPT=_F(NOM = (__CHAR1,__CHAR2,__EVOL),), INFO=1)
  # ---
  # Fin boucle sur les pressions de confinement PRES_CONF
  # --- 

  # remplissage des tables
  remplir_tables(self,"TD",str_n_essai,DicoEssai,Resu_Essai)

  # impression des graphiques
  impr_graphique(self,DicoEssai,Courbes,NomsFich,Leg_x,Leg_y,{},{})

  DETRUIRE(CONCEPT=_F(NOM = (__RLIST,__DLIST),), INFO=1)





# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
#              Essai Triaxial Monotone Non Draine (TND)
# -----------------------------------------------------------------------
# -----------------------------------------------------------------------

def essai_TND(self,str_n_essai,DicoEssai,MATER,COMP_INCR,CONVERGENCE,INFO):
  """
  Essai Triaxial Monotone Non Draine (TND)
  """  
  import numpy as NP
  from Accas import _F
  import aster

  DEFI_FONCTION  = self.get_cmd('DEFI_FONCTION')
  SIMU_POINT_MAT = self.get_cmd('SIMU_POINT_MAT')
  DETRUIRE       = self.get_cmd('DETRUIRE')
  CREA_TABLE     = self.get_cmd('CREA_TABLE')
  IMPR_TABLE     = self.get_cmd('IMPR_TABLE')
  DEFI_LIST_INST = self.get_cmd('DEFI_LIST_INST')
  DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')

  PRES_CONF   = DicoEssai['PRES_CONF']
  EPSI_IMPOSE = DicoEssai['EPSI_IMPOSE']
  BIOT_COEF   = DicoEssai['BIOT_COEF']
  NB_INST     = DicoEssai['NB_INST']

 # dict permettant la gestion des graphiques
  Courbes  = {}
  NomsFich = {}
  Leg_x    = {}
  Leg_y    = {}

  # dict permettant la gestion des tables en sortie
  Resu_Essai            = {}
  Resu_Essai['INST']    = [[] for k in xrange(len(PRES_CONF))]
  Resu_Essai['EPS_AXI'] = [[] for k in xrange(len(PRES_CONF))]
  Resu_Essai['EPS_LAT'] = [[] for k in xrange(len(PRES_CONF))]
  Resu_Essai['SIG_AXI'] = [[] for k in xrange(len(PRES_CONF))]
  Resu_Essai['SIG_LAT'] = [[] for k in xrange(len(PRES_CONF))]
  Resu_Essai['P']       = [[] for k in xrange(len(PRES_CONF))]
  Resu_Essai['Q']       = [[] for k in xrange(len(PRES_CONF))]
  Resu_Essai['PRE_EAU'] = [[] for k in xrange(len(PRES_CONF))]

  # preparation des graphiques 
  str_fich = "Essai_TND_"+str_n_essai+"_"
  preparer_graphique('2',DicoEssai,str_fich,Courbes,NomsFich,\
                     Leg_x,Leg_y,{},{})

  # Creation de la liste d'instants
  __RLIST = DEFI_LIST_REEL(DEBUT = 0.,
                           INTERVALLE = _F(JUSQU_A = 100.,
                                           NOMBRE  = NB_INST,),
                           INFO = INFO)

  __DLIST = DEFI_LIST_INST(DEFI_LIST = _F(LIST_INST = __RLIST),
                           ECHEC=_F(SUBD_METHODE = 'MANUEL',
                                    SUBD_PAS     = 10,
                                    SUBD_NIVEAU  = 10,),
                           INFO = INFO,)

  # ---
  # Boucle sur les pressions de confinement PRES_CONF
  # --- 
  for i in xrange(len(PRES_CONF)):

    affiche_infos_essai(str_n_essai,"TND",PRES_CONF[i],EPSI_IMPOSE[i])

    # ---
    # Definition des chargements                           
    # ---
    __CHAR1 = DEFI_FONCTION(INFO = INFO, NOM_PARA = 'INST',
                             VALE = (0., 0., 100., EPSI_IMPOSE[i],),)
    
    __CHAR2 = DEFI_FONCTION(INFO = INFO, NOM_PARA = 'INST',
                             VALE = (0., 0., 100., -0.5*EPSI_IMPOSE[i],),)

    # ---
    # Calcul, avec tr(eps) = 0 impose
    # ---
    __EVOL = SIMU_POINT_MAT(
           INFO=INFO,
           COMP_INCR=COMP_INCR.List_F(),
           CONVERGENCE=CONVERGENCE.List_F(),
           MATER=MATER,
           INCREMENT=_F(LIST_INST=__DLIST,
                        INST_INIT=0.,
                        INST_FIN =100.,),
           NEWTON=_F(MATRICE='TANGENTE', REAC_ITER=1,),
           ARCHIVAGE=_F(LIST_INST=__RLIST,),
           EPSI_IMPOSE=_F(EPZZ=__CHAR1,
                          EPXX=__CHAR2,
                          EPYY=__CHAR2,),
           SIGM_INIT  =_F(SIXX=PRES_CONF[i],
                          SIYY=PRES_CONF[i],
                          SIZZ=PRES_CONF[i],),
           EPSI_INIT  =_F(EPXX=0,
                          EPYY=0,
                          EPZZ=0,
                          EPXY=0,
                          EPXZ=0,
                          EPYZ=0,),)

    # ---
    # Post-traitements
    # ---
    TabRes  = __EVOL.EXTR_TABLE().values()
    sig_xx  = NP.array(TabRes['SIXX'])
    sig_yy  = NP.array(TabRes['SIYY'])
    sig_zz  = NP.array(TabRes['SIZZ'])
    eps_xx  = NP.array(TabRes['EPXX'])
    eps_yy  = NP.array(TabRes['EPYY'])
    eps_zz  = NP.array(TabRes['EPZZ'])
    inst    = TabRes['INST']
    eps_vol = eps_xx+eps_yy+eps_zz
    p       = (sig_xx+sig_yy+sig_zz)/3.
    q       = abs(sig_zz-sig_xx)

    pprime  = -1.*q/3.+PRES_CONF[i]
    pre_eau = (p-pprime)/BIOT_COEF

    # remplissage des graphiques
    str_leg = "PRES_CONF = " + str("%E"%(PRES_CONF[i])) + ", EPSI_IMPOSE = "\
                             + str("%E"%(EPSI_IMPOSE[i]))
    remplir_graphique(DicoEssai,Courbes,list(p),list(q),str_leg,"P-Q")
    remplir_graphique(DicoEssai,Courbes,list(eps_zz),list(q),str_leg,"EPS_AXI-Q")
    remplir_graphique(DicoEssai,Courbes,list(eps_zz),list(pre_eau),str_leg,"EPS_AXI-PRE_EAU")

    # stockage pour ecriture dans les tables 
    Resu_Essai['INST'][i]    = inst
    Resu_Essai['EPS_AXI'][i] = list(eps_zz )
    Resu_Essai['EPS_LAT'][i] = list(eps_xx )
    Resu_Essai['SIG_AXI'][i] = list(sig_zz )
    Resu_Essai['SIG_LAT'][i] = list(sig_xx )
    Resu_Essai['P'][i]       = list(p      )
    Resu_Essai['Q'][i]       = list(q      )
    Resu_Essai['PRE_EAU'][i] = list(pre_eau)

    DETRUIRE(CONCEPT=_F(NOM = (__CHAR1,__CHAR2,__EVOL),), INFO=1)
  # ---
  # Fin boucle sur les pressions de confinement PRES_CONF
  # --- 

  # remplissage des tables
  remplir_tables(self,"TND",str_n_essai,DicoEssai,Resu_Essai)

  # impression des graphiques
  impr_graphique(self,DicoEssai,Courbes,NomsFich,Leg_x,Leg_y,{},{})

  DETRUIRE(CONCEPT=_F(NOM = (__RLIST,__DLIST),), INFO=1)





# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
#             Essai de Cisaillement Cyclique Draine (CISA_C)
# -----------------------------------------------------------------------
# -----------------------------------------------------------------------

def essai_CISA_C(self,str_n_essai,DicoEssai,MATER,COMP_INCR,CONVERGENCE,INFO):
  """
  Essai de Cisaillement Cyclique Draine (CISA_C)
  """  
  import numpy as NP
  import math as M
  from Accas import _F
  import aster
  from Utilitai.Utmess import UTMESS

  DEFI_FONCTION  = self.get_cmd('DEFI_FONCTION')
  SIMU_POINT_MAT = self.get_cmd('SIMU_POINT_MAT')
  DETRUIRE       = self.get_cmd('DETRUIRE')
  CREA_TABLE     = self.get_cmd('CREA_TABLE')
  IMPR_TABLE     = self.get_cmd('IMPR_TABLE')
  DEFI_LIST_INST = self.get_cmd('DEFI_LIST_INST')
  DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')

  PRES_CONF   = DicoEssai['PRES_CONF']
  EPSI_IMPOSE = DicoEssai['EPSI_IMPOSE']
  EPSI_ELAS   = DicoEssai['EPSI_ELAS']
  NB_CYCLE    = DicoEssai['NB_CYCLE']
  NB_INST     = DicoEssai['NB_INST']

  # dict permettant la gestion des graphiques
  Courbes_niv1  = {}
  NomsFich_niv1 = {}
  Leg_x_niv1    = {}
  Leg_y_niv1    = {}
  Ech_x_niv1    = {}
  Ech_y_niv1    = {}
  Courbes_niv2  = {}
  NomsFich_niv2 = {}
  Leg_x_niv2    = {}
  Leg_y_niv2    = {}

  # preparation des graphiques (niveau 1)
  str_fich_niv1 = "Essai_CISA_C_"+str_n_essai+"_"
  preparer_graphique('1',DicoEssai,str_fich_niv1,Courbes_niv1,NomsFich_niv1,\
                     Leg_x_niv1,Leg_y_niv1,Ech_x_niv1,Ech_y_niv1)

  # ---
  # Creation de la liste d'instants (NB_INST = nombre d'instants par 1/4 de cycle)
  # ---
  __RLIST = DEFI_LIST_REEL(DEBUT = 0.,
                           INTERVALLE = [_F(JUSQU_A=10.        , NOMBRE=NB_INST,),] + \
                                        [_F(JUSQU_A=10.*(2*k+1), NOMBRE=2*NB_INST,)   \
                                          for k in xrange(1,2*NB_CYCLE+1)],
                           INFO = INFO)

  __DLIST = DEFI_LIST_INST(DEFI_LIST = _F(LIST_INST = __RLIST),
                           ECHEC=_F(SUBD_METHODE = 'MANUEL',
                                    SUBD_PAS     = 10,
                                    SUBD_NIVEAU  = 10,),
                           INFO = INFO,)

  Gs_list    = [[] for k in xrange(len(PRES_CONF))]
  D_list     = [[] for k in xrange(len(PRES_CONF))]
  inst_list  = [[] for k in xrange(len(PRES_CONF))]
  epsxy_list = [[] for k in xrange(len(PRES_CONF))]
  sigxy_list = [[] for k in xrange(len(PRES_CONF))]

  # ---
  # Boucle sur les pressions de confinement PRES_CONF
  # --- 
  for i in xrange(len(PRES_CONF)):

    # preparation des graphiques (niveau 2)
    str_fich_niv2 = "Essai_CISA_C_"+str_n_essai+"_"\
                  + "PRES_CONF_"+int_2_str(i+1,len(PRES_CONF))+"_"
    preparer_graphique('2',DicoEssai,str_fich_niv2,Courbes_niv2,NomsFich_niv2,\
                       Leg_x_niv2,Leg_y_niv2,{},{})

    # ---
    # Calcul du module de cisaillement secant maximal (elasticite) 
    # ---
    Gs_max = Calc_Gs_max(self,EPSI_ELAS,PRES_CONF[i],MATER,COMP_INCR,CONVERGENCE)

    # ---
    # Boucle sur les amplitudes de cisaillement EPSI_IMPOSE
    # --- 
    for j in xrange(len(EPSI_IMPOSE)):

      affiche_infos_essai(str_n_essai,"CISA_C",PRES_CONF[i],EPSI_IMPOSE[j])
    
      # ---
      # Definition des chargements                           
      # ---
      __CHAR1 = DEFI_FONCTION(INFO = INFO, NOM_PARA = 'INST',
                              ABSCISSE = [0.] + [10.*(2*k+1) \
                                                 for k in xrange(2*NB_CYCLE+1)],
                              ORDONNEE = [0.] + [EPSI_IMPOSE[j]*(-1)**(k+1) \
                                                 for k in xrange(2*NB_CYCLE+1)],
                              )
                               
      __CHAR2 = DEFI_FONCTION(INFO = INFO, NOM_PARA = 'INST',
                              VALE = (0.                , PRES_CONF[i], 
                                      10.*(4*NB_CYCLE+1), PRES_CONF[i],),)

      # ---
      # Calcul                                                                       
      # ---
      __EVOL = SIMU_POINT_MAT(
               INFO=INFO,
               COMP_INCR=COMP_INCR.List_F(),
               CONVERGENCE=CONVERGENCE.List_F(),
               MATER=MATER,
               INCREMENT=_F(LIST_INST=__DLIST,
                            INST_INIT=0.,
                            INST_FIN =10.*(4*NB_CYCLE+1),),
               NEWTON=_F(MATRICE='TANGENTE', REAC_ITER=1,),
               ARCHIVAGE=_F(LIST_INST=__RLIST,),
               SIGM_IMPOSE=_F(SIXX=__CHAR2,
                              SIYY=__CHAR2,
                              SIZZ=__CHAR2,),
               EPSI_IMPOSE=_F(EPXY=__CHAR1,),
               SIGM_INIT  =_F(SIXX=PRES_CONF[i],
                              SIYY=PRES_CONF[i],
                              SIZZ=PRES_CONF[i],),
               EPSI_INIT  =_F(EPXX=0,
                              EPYY=0,
                              EPZZ=0,
                              EPXY=0,
                              EPXZ=0,
                              EPYZ=0,),)

      # ---
      # Post-traitements
      # ---
      TabRes  = __EVOL.EXTR_TABLE().values()
      inst    =          TabRes['INST']
      sig_xx  = NP.array(TabRes['SIXX'])
      sig_yy  = NP.array(TabRes['SIYY'])
      sig_zz  = NP.array(TabRes['SIZZ'])
      sig_xy  = NP.array(TabRes['SIXY'])
      eps_xx  = NP.array(TabRes['EPXX'])
      eps_yy  = NP.array(TabRes['EPYY'])
      eps_zz  = NP.array(TabRes['EPZZ'])
      eps_xy  = NP.array(TabRes['EPXY'])
      eps_vol = eps_xx+eps_yy+eps_zz
      p       = (sig_xx+sig_yy+sig_zz)/3.
      q       = abs(sig_zz-sig_xx)

      epsxy_list[i].append(list(eps_xy))
      sigxy_list[i].append(list(sig_xy))
      inst_list[i].append(inst)

      #   ind1 -> indice au debut du dernier cycle
      #   ind2 -> indice a la moitie du dernier cycle
      #   ind3 -> indice a la fin du dernier cycle
      ind1 = inst.index( 10.*(2*(2*NB_CYCLE-2)+1) )
      ind2 = inst.index( 10.*(2*(2*NB_CYCLE-1)+1) ) 
      ind3 = inst.index( 10.*(2*(2*NB_CYCLE)  +1) ) 

      # module de cisaillement secant normalise Gs
      Gs = 0.25*abs(sig_xy[ind3]-sig_xy[ind2])/EPSI_IMPOSE[j]
      Gs = Gs/Gs_max
      if not( Gs <= 1. or abs(Gs-1.) <= 1.e-8 ) :
        UTMESS('F','COMPOR2_36',valk=('CISA_C','EPSI_ELAS'),valr=(EPSI_ELAS))
      Gs_list[i].append(Gs)

      # taux d'amortissement D (aire delta_W : methode des trapezes)
      delta_W = 2.*abs(NP.sum( 0.5*(sig_xy[ind2+1:]+sig_xy[ind2:-1])*\
                                   (eps_xy[ind2+1:]-eps_xy[ind2:-1]) ))
      W = abs(sig_xy[ind3]*EPSI_IMPOSE[j])
      D = delta_W/(4.*M.pi*W)
      D_list[i].append(D)

      # remplissage des graphiques (niveau 2)
      str_leg2 = "PRES_CONF = " + str("%E"%(PRES_CONF[i])) + ", EPSI_IMPOSE = "\
                                + str("%E"%(EPSI_IMPOSE[j]))
      remplir_graphique(DicoEssai,Courbes_niv2,list(p),list(q),str_leg2,"P-Q")
      remplir_graphique(DicoEssai,Courbes_niv2,list(eps_xy),list(sig_xy),str_leg2,"EPSXY-SIGXY")

      DETRUIRE(CONCEPT=_F(NOM = (__CHAR1,__CHAR2,__EVOL),), INFO=1)
    # ---
    # Fin boucle sur les amplitudes de cisaillement EPSI_IMPOSE
    # --- 

    # remplissage des graphiques (niveau 1)
    str_leg1 = "PRES_CONF = " + str("%E"%(PRES_CONF[i]))
    remplir_graphique(DicoEssai,Courbes_niv1,EPSI_IMPOSE,Gs_list[i],str_leg1,"EPSXY-G")
    remplir_graphique(DicoEssai,Courbes_niv1,EPSI_IMPOSE,D_list[i] ,str_leg1,"EPSXY-D")

    # impression des graphiques (niveau 2)
    impr_graphique(self,DicoEssai,Courbes_niv2,NomsFich_niv2,Leg_x_niv2,\
                   Leg_y_niv2,{},{})

  # ---
  # Fin boucle sur les sur les pressions de confinement PRES_CONF
  # --- 

  # remplissage des tables
  Resu_Essai               = {}
  Resu_Essai['INST']       = inst_list
  Resu_Essai['EPS_XY']     = epsxy_list
  Resu_Essai['SIG_XY']     = sigxy_list
  Resu_Essai['G_SUR_GMAX'] = Gs_list
  Resu_Essai['DAMPING']    = D_list
  remplir_tables(self,"CISA_C",str_n_essai,DicoEssai,Resu_Essai)

  # impression des graphiques (niveau 1)
  impr_graphique(self,DicoEssai,Courbes_niv1,NomsFich_niv1,Leg_x_niv1,Leg_y_niv1,\
                 Ech_x_niv1,Ech_y_niv1)

  DETRUIRE(CONCEPT=_F(NOM = (__RLIST,__DLIST),), INFO=1)





# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
#             Essai Triaxial Non Draine Cyclique Draine (TND_C)
# -----------------------------------------------------------------------
# -----------------------------------------------------------------------

def essai_TND_C(self,str_n_essai,DicoEssai,MATER,COMP_INCR,CONVERGENCE,INFO):
  """
  Essai Triaxial Non Draine Cyclique Draine (TND_C)
  """  
  import numpy as NP
  import math as M
  from Accas import _F
  import aster
  from Utilitai.Utmess import UTMESS
  from Comportement import catalc

  DEFI_FONCTION  = self.get_cmd('DEFI_FONCTION')
  CALC_POINT_MAT = self.get_cmd('CALC_POINT_MAT')
  SIMU_POINT_MAT = self.get_cmd('SIMU_POINT_MAT')
  DETRUIRE       = self.get_cmd('DETRUIRE')
  CREA_TABLE     = self.get_cmd('CREA_TABLE')
  IMPR_TABLE     = self.get_cmd('IMPR_TABLE')
  DEFI_LIST_INST = self.get_cmd('DEFI_LIST_INST')
  DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')

  PRES_CONF   = DicoEssai['PRES_CONF']
  SIGM_IMPOSE = DicoEssai['SIGM_IMPOSE']
  BIOT_COEF   = DicoEssai['BIOT_COEF']
  UN_SUR_K    = DicoEssai['UN_SUR_K']
  NB_CYCLE    = DicoEssai['NB_CYCLE']
  NB_INST     = DicoEssai['NB_INST']
  K_EAU       = 1./UN_SUR_K

  # recuperation du nombre de VI associe a la LdC
  nom_lc = COMP_INCR.List_F()[0]['RELATION']
  num_lc, nb_vari = catalc.get_info(nom_lc)
  assert type(nb_vari) is int and nb_vari>0

  # critere de liquefaction (ru = abs(DeltaPeau/Sig'vertical) < ru_max)
  ru_max = 0.8

  # dict permettant la gestion des graphiques
  Courbes_niv1  = {}
  NomsFich_niv1 = {}
  Leg_x_niv1    = {}
  Leg_y_niv1    = {}
  Courbes_niv2  = {}
  NomsFich_niv2 = {}
  Leg_x_niv2    = {}
  Leg_y_niv2    = {}

  # preparation des graphiques (niveau 1)
  str_fich_niv1 = "Essai_TND_C_"+str_n_essai+"_"
  preparer_graphique('1',DicoEssai,str_fich_niv1,Courbes_niv1,NomsFich_niv1,\
                     Leg_x_niv1,Leg_y_niv1,{},{})

  # ---
  # Creation de la liste d'instants (NB_INST = nombre d'instants par 1/4 de cycle )
  # ---
#  long_cyc = 4*10.
#  pas_inst = long_cyc/(4.*NB_INST)
#  inst_ini = 0.
#  L_inst   = [inst_ini + k*pas_inst for k in xrange(4*NB_INST+1)]
#
#  __RLIST = DEFI_LIST_REEL(VALE  = L_inst, INFO = INFO)
#
#  __DLIST = DEFI_LIST_INST(DEFI_LIST = _F(LIST_INST = __RLIST),
#                           ECHEC=_F(SUBD_METHODE = 'MANUEL',
#                                    SUBD_PAS     = 10,
#                                    SUBD_NIVEAU  = 10,),
#                           INFO = INFO,)

  NCyc_list  = [[] for k in xrange(len(PRES_CONF))]
  DSig_list  = [[] for k in xrange(len(PRES_CONF))]
  inst_list  = [[] for k in xrange(len(PRES_CONF))]
  epsxx_list = [[] for k in xrange(len(PRES_CONF))]
  epszz_list = [[] for k in xrange(len(PRES_CONF))]
  epsv_list  = [[] for k in xrange(len(PRES_CONF))]
  sigxx_list = [[] for k in xrange(len(PRES_CONF))]
  sigzz_list = [[] for k in xrange(len(PRES_CONF))]
  p_list     = [[] for k in xrange(len(PRES_CONF))]
  q_list     = [[] for k in xrange(len(PRES_CONF))]
  preau_list = [[] for k in xrange(len(PRES_CONF))]
  error_list = []

  # ---
  # Boucle sur les pressions de confinement PRES_CONF
  # --- 
  for i in xrange(len(PRES_CONF)):

    # preparation des graphiques (niveau 2)
    str_fich_niv2 = "Essai_TND_C_"+str_n_essai+"_"\
                  + "PRES_CONF_"+int_2_str(i+1,len(PRES_CONF))+"_"
    preparer_graphique('2',DicoEssai,str_fich_niv2,Courbes_niv2,NomsFich_niv2,\
                       Leg_x_niv2,Leg_y_niv2,{},{})

    # ---
    # Boucle sur les amplitudes de variation SIGM_IMPOSE
    # --- 
    for j in xrange(len(SIGM_IMPOSE)):

      affiche_infos_essai(str_n_essai,"TND_C",PRES_CONF[i],SIGM_IMPOSE[j])

      Ltmp_inst = []
      Ltmp_epxx = []
      Ltmp_epzz = []
      Ltmp_epv  = []
      Ltmp_sixx = []
      Ltmp_sizz = []
      Ltmp_p    = []
      Ltmp_q    = []
      Ltmp_pre  = []

      # ---
      # Boucle sur les cycles en contrainte
      # --- 
      long_cyc = 4*10.
      inst_ini = -1.*long_cyc
      pas_inst = long_cyc/(4.*NB_INST)

      for i_cycle in xrange(1,NB_CYCLE+1):

        cycle_ok = True
        codret   = ""

        # ---
        # Creation de la liste d'instants (NB_INST = nombre d'instants par 1/4 de cycle )
        # ---
        inst_ini += long_cyc
        L_inst    = [inst_ini + k*pas_inst for k in xrange(4*NB_INST+1)]

        __RLIST = DEFI_LIST_REEL(VALE  = L_inst, INFO = INFO)

        __DLIST = DEFI_LIST_INST(DEFI_LIST = _F(LIST_INST = __RLIST),
                                 ECHEC=_F(SUBD_METHODE = 'MANUEL',
                                          SUBD_PAS     = 10,
                                          SUBD_NIVEAU  = 10,),
                                 INFO = INFO,)

        # ---
        # Definition des chargements                           
        # ---
        __CHAR1 = DEFI_FONCTION(INFO = INFO, NOM_PARA = 'INST',
                                VALE = (inst_ini               , PRES_CONF[i]           ,
                                        inst_ini+long_cyc/4.   , PRES_CONF[i]+SIGM_IMPOSE[j],
                                        inst_ini+3.*long_cyc/4., PRES_CONF[i]-SIGM_IMPOSE[j],
                                        inst_ini+long_cyc      , PRES_CONF[i]           ,))
                               
        __CHAR2 = DEFI_FONCTION(INFO = INFO, NOM_PARA = 'INST',
                                VALE = (inst_ini         , PRES_CONF[i], 
                                        inst_ini+long_cyc, PRES_CONF[i],),)


        # s'il s'agit du premier cycle, Cond_Init toutes a 0.
        if i_cycle == 1: 
          Cond_Init = {}
          Cond_Init['VARI'] = [0.]*nb_vari
          Cond_Init['SIXX'] = PRES_CONF[i]
          Cond_Init['SIYY'] = PRES_CONF[i]
          Cond_Init['SIZZ'] = PRES_CONF[i]
          Cond_Init['EPXX'] = 0.
          Cond_Init['EPYY'] = 0.
          Cond_Init['EPZZ'] = 0.
          Cond_Init['EPXY'] = 0.
          Cond_Init['EPXZ'] = 0.
          Cond_Init['EPYZ'] = 0.

        # ---
        # Calcul                                                                       
        # ---
        try:

          __EVOL = SIMU_POINT_MAT(
                   INFO=INFO,
                   COMP_INCR=COMP_INCR.List_F(),
                   CONVERGENCE=CONVERGENCE.List_F(),
                   MATER=MATER,
                   SUPPORT='POINT',
                   INCREMENT=_F(LIST_INST=__DLIST,
                                INST_INIT=inst_ini,
                                INST_FIN =inst_ini+long_cyc,),
                   NEWTON=_F(MATRICE='TANGENTE', REAC_ITER=1,),
                   ARCHIVAGE=_F(LIST_INST=__RLIST,),
                   VECT_IMPO=(_F(NUME_LIGNE=1,VALE=__CHAR2),
                              _F(NUME_LIGNE=2,VALE=__CHAR2),
                              _F(NUME_LIGNE=3,VALE=__CHAR1),),
                   MATR_C1=(  _F(NUME_LIGNE=1,NUME_COLONNE=1,VALE=1.),
                              _F(NUME_LIGNE=2,NUME_COLONNE=2,VALE=1.),
                              _F(NUME_LIGNE=3,NUME_COLONNE=3,VALE=1.),),
                   MATR_C2=(  _F(NUME_LIGNE=1,NUME_COLONNE=1,VALE=K_EAU),
                              _F(NUME_LIGNE=1,NUME_COLONNE=2,VALE=K_EAU),
                              _F(NUME_LIGNE=1,NUME_COLONNE=3,VALE=K_EAU),
                              _F(NUME_LIGNE=2,NUME_COLONNE=1,VALE=K_EAU),
                              _F(NUME_LIGNE=2,NUME_COLONNE=2,VALE=K_EAU),
                              _F(NUME_LIGNE=2,NUME_COLONNE=3,VALE=K_EAU),
                              _F(NUME_LIGNE=3,NUME_COLONNE=1,VALE=K_EAU),
                              _F(NUME_LIGNE=3,NUME_COLONNE=2,VALE=K_EAU),
                              _F(NUME_LIGNE=3,NUME_COLONNE=3,VALE=K_EAU),),
                   SIGM_INIT= _F(SIXX=Cond_Init['SIXX'],
                                 SIYY=Cond_Init['SIYY'],
                                 SIZZ=Cond_Init['SIZZ'],),
                   EPSI_INIT= _F(EPXX=Cond_Init['EPXX'],
                                 EPYY=Cond_Init['EPYY'],
                                 EPZZ=Cond_Init['EPZZ'],
                                 EPXY=Cond_Init['EPXY'],
                                 EPXZ=Cond_Init['EPXZ'],
                                 EPYZ=Cond_Init['EPYZ'],),
                   VARI_INIT= _F(VALE=Cond_Init['VARI']),)

        except aster.error: 
          cycle_ok = False
          pass

        # ---
        # Si le calcul s'est bien deroule sur ce cycle...
        # ---
        if cycle_ok:

          TabRes = __EVOL.EXTR_TABLE().values()
          # PostTraiter les grandeurs d'interet...
          sig_xx = NP.array(TabRes['SIXX'])
          sig_yy = NP.array(TabRes['SIYY'])
          sig_zz = NP.array(TabRes['SIZZ'])
          eps_xx = NP.array(TabRes['EPXX'])
          eps_yy = NP.array(TabRes['EPYY'])
          eps_zz = NP.array(TabRes['EPZZ'])
          eps_xy = NP.array(TabRes['EPXY'])
          eps_xz = NP.array(TabRes['EPXZ'])
          eps_yz = NP.array(TabRes['EPYZ'])
          inst    =         TabRes['INST']
          eps_vol = eps_xx+eps_yy+eps_zz
          p       = (sig_xx+sig_yy+sig_zz)/3.
          q       = abs(sig_zz-sig_xx)
          coef    = K_EAU/BIOT_COEF
          pre_eau = -1.*coef*eps_vol     
          # .. et les stocker
          Ltmp_inst += inst
          Ltmp_epxx += list(eps_xx )
          Ltmp_epzz += list(eps_zz )
          Ltmp_epv  += list(eps_vol)
          Ltmp_sixx += list(sig_xx )
          Ltmp_sizz += list(sig_zz )
          Ltmp_p    += list(p      )
          Ltmp_q    += list(q      )
          Ltmp_pre  += list(pre_eau)

          # Cond_Init : vi, eps, et sig au dernier inst pr CI cycle suivant
          Cond_Init = {'VARI':[]}
          for i_vari in xrange(1,nb_vari+1):
            VarTmp = TabRes["V"+str(i_vari)]
            Cond_Init['VARI'].append(VarTmp[-1])
          Cond_Init['SIXX'] = sig_xx[-1]
          Cond_Init['SIYY'] = sig_yy[-1]
          Cond_Init['SIZZ'] = sig_zz[-1]
          Cond_Init['EPXX'] = eps_xx[-1]
          Cond_Init['EPYY'] = eps_yy[-1]
          Cond_Init['EPZZ'] = eps_zz[-1]
          Cond_Init['EPXY'] = eps_xy[-1]
          Cond_Init['EPXZ'] = eps_xz[-1]
          Cond_Init['EPYZ'] = eps_yz[-1]

          # Evaluer si le critere de liquefaction est atteint...
          ru = abs(pre_eau/PRES_CONF[i]) >= ru_max
          # ... si oui on sort de la boucle sur les cycles
          if ru.any() :
            codret = "0"
            NCyc_list[i].append(i_cycle)
            DSig_list[i].append(SIGM_IMPOSE[j])
            DETRUIRE(CONCEPT=_F(NOM = (__RLIST,__DLIST,__CHAR1,__CHAR2,__EVOL)), INFO=1)
            break
          # si dernier cycle et que rien ne s'est passe :
          if i_cycle == NB_CYCLE :
            codret = "1"

        # ---
        # Sinon, quitter la boucle sur les cycles en contrainte
        # ---
        else:
          if i_cycle == 1:
            codret = "3"
          else:
            codret = "2"
            NCyc_list[i].append(i_cycle)
            DSig_list[i].append(SIGM_IMPOSE[j])
          DETRUIRE(CONCEPT=_F(NOM = (__RLIST,__DLIST,__CHAR1,__CHAR2,)), INFO=1)
          break

        DETRUIRE(CONCEPT=_F(NOM = (__RLIST,__DLIST,__CHAR1,__CHAR2,__EVOL)), INFO=1)
      # ---
      # Fin boucle sur les cycles en contrainte
      # --- 

      # pour la gestion des alarmes
      dico_tmp = {}
      dico_tmp['chargements'] = (PRES_CONF[i],SIGM_IMPOSE[j])
      dico_tmp['code_retour'] = codret
      error_list.append(dico_tmp)

      # stockage pour ecriture dans les tables 
      inst_list[i].append(  Ltmp_inst )
      epsxx_list[i].append( Ltmp_epxx )
      epszz_list[i].append( Ltmp_epzz )
      epsv_list[i].append(  Ltmp_epv  )
      sigxx_list[i].append( Ltmp_sixx )
      sigzz_list[i].append( Ltmp_sizz )
      p_list[i].append(     Ltmp_p    )
      q_list[i].append(     Ltmp_q    )
      preau_list[i].append( Ltmp_pre  )

      # remplissage des graphiques (niveau 2)
      str_leg2 = "PRES_CONF = " + str("%E"%(PRES_CONF[i])) + ", SIGM_IMPOSE = "\
                                + str("%E"%(SIGM_IMPOSE[j]))
      remplir_graphique(DicoEssai,Courbes_niv2,Ltmp_p,Ltmp_q,str_leg2,"P-Q")
      remplir_graphique(DicoEssai,Courbes_niv2,Ltmp_sizz,Ltmp_pre,str_leg2,"SIG_AXI-PRE_EAU")
    # ---
    # Fin boucle sur les amplitudes de variation SIGM_IMPOSE
    # --- 

    # remplissage des graphiques (niveau 1)
    str_leg1 = "PRES_CONF = " + str("%E"%(PRES_CONF[i]))
    remplir_graphique(DicoEssai,Courbes_niv1,NCyc_list[i],DSig_list[i],\
                      str_leg1,"NCYCL-DSIGM")

    # impression des graphiques (niveau 2)
    impr_graphique(self,DicoEssai,Courbes_niv2,NomsFich_niv2,Leg_x_niv2,\
                   Leg_y_niv2,{},{})
  # ---
  # Fin boucle sur les sur les pressions de confinement PRES_CONF
  # --- 

  # pour la gestion des alarmes
  assert len(error_list) == len(PRES_CONF)*len(SIGM_IMPOSE)
  bilan_alarmes(str_n_essai,"TND_C",DicoEssai,error_list)

  # remplissage des tables
  Resu_Essai            = {}
  Resu_Essai['INST']    = inst_list
  Resu_Essai['EPS_AXI'] = epszz_list
  Resu_Essai['EPS_LAT'] = epsxx_list
  Resu_Essai['EPS_VOL'] = epsv_list
  Resu_Essai['SIG_AXI'] = sigzz_list
  Resu_Essai['SIG_LAT'] = sigxx_list
  Resu_Essai['P']       = p_list
  Resu_Essai['Q']       = q_list
  Resu_Essai['PRE_EAU'] = preau_list
  Resu_Essai['NCYCL']   = NCyc_list
  Resu_Essai['DSIGM']   = DSig_list
  remplir_tables(self,"TND_C",str_n_essai,DicoEssai,Resu_Essai)

  # impression des graphiques (niveau 1)
  impr_graphique(self,DicoEssai,Courbes_niv1,NomsFich_niv1,Leg_x_niv1,Leg_y_niv1,{},{})
