# coding=utf-8
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
# person_in_charge: irmela.zentner at edf.fr

import os
import copy
import traceback
from types import ListType, TupleType
from math import pi,ceil, exp, sqrt, log
import aster_core

EnumTypes = (ListType, TupleType)


def gene_acce_seisme_ops(self,PAS_INST,DSP,SPEC_UNIQUE,SPEC_MEDIANE,SPEC_FRACTILE, MODULATION, DUREE_PHASE_FORTE, NB_POIN,PESANTEUR,NB_TIRAGE, TITRE,INFO,**args):

   import numpy as NP
   import aster
   from Accas                 import _F
   from Utilitai.Utmess       import UTMESS
   from Cata_Utils.t_fonction import t_fonction
   from Utilitai.Table        import Table
   from Macro.defi_inte_spec_ops import tocomplex
   from Cata.cata import nappe_sdaster,fonction_sdaster,fonction_c
   import aster_fonctions
   from Utilitai.optimize   import fmin
   from Utilitai.gauss_process  import  DSP2ACCE1D,itersim_SRO, gene_traj_gauss_evol1D , Rice2, peak, SRO2DSP,DSP2FR,corrcoefmodel,RAND_DSP,RAND_VEC
   from Utilitai.gauss_process  import  calc_dsp_KT,calc_dsp_FR, f_ARIAS, f_ARIAS_TSM,fonctm_gam, fonctm_JetH
   from Utilitai.gauss_process  import  f_opta, f_opt1, f_opt2
   EnumTypes = (list, tuple)
   

   
   commande='GENE_ACCE_SEISME'

   ier = 0
   # La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)

   # Le concept sortant (de type table_fonction) est tab
   self.DeclareOut('tab_out', self.sd)

   # On importe les definitions des commandes a utiliser dans la macro
   CREA_TABLE    = self.get_cmd('CREA_TABLE')
   DEFI_FONCTION  = self.get_cmd('DEFI_FONCTION')

   # parametres des fonctions a creer t_fonctions
   para_dsp = {
         'INTERPOL' : ['LIN','LIN'],
         'NOM_PARA'    : 'FREQ',
         'PROL_DROITE' : 'CONSTANT',
         'PROL_GAUCHE' : 'EXCLU', 'NOM_RESU'   : 'ACCE'}
  
   para_traj      = {  'NOM_PARA' : 'INST', 'NOM_RESU'   : 'ACCE',
                       'PROL_DROITE'   : 'EXCLU', 'PROL_GAUCHE'   : 'EXCLU',
                        'TITRE'   : TITRE,   }

   para_modul      = {  'NOM_PARA' : 'INST', 'NOM_RESU'   : 'ACCE','INTERPOL' : ['LIN','LIN'],
                       'PROL_DROITE'   : 'EXCLU',  'PROL_GAUCHE'   : 'EXCLU', }


#  ------------------------------------------------------------------
#  SEED POUR lA GENERATION DES VA
#  ------------------------------------------------------------------
  
   INIT_ALEA=args['INIT_ALEA']
   if INIT_ALEA!=None :
      NP.random.seed(INIT_ALEA)



#  ------------------------------------------------------------------
#  RECUP DONNEES   
#  ------------------------------------------------------------------   

# donnees generiques
   DUREE=DUREE_PHASE_FORTE
# discretisation temps et freq
   DT=PAS_INST
   OM=pi/DT
   FREQ_COUP=OM/2./pi
   NB_ITER=0  #par defaut pas d'iteration
   FREQ_FILTRE=args['FREQ_FILTRE']
   if FREQ_FILTRE!=None :
      F_CORNER=FREQ_FILTRE
# donnees fonction modulation (obligatoire) 

   NORME=PESANTEUR
   ARIAS=MODULATION['INTE_ARIAS']
   PGA=MODULATION['ACCE_MAX']
   ECART=MODULATION['ECART_TYPE']
   TYPE=MODULATION['TYPE']
   if  ECART!= None:
      ECART=ECART*NORME

   if TYPE=='GAMMA':
      INST_INI=MODULATION['INST_INI']
   else:
      INST_INI=0.0
   print "TYPE MODULATION",  TYPE



# 1) donnees DSP si present

   if DSP !=None :
      amo  =DSP['AMOR_REDUIT']   
      F_RED=DSP['FREQ_FOND']
      wg=F_RED*2.*pi 
      if FREQ_FILTRE == None :
         F_CORNER=0.05*F_RED
      FREQ_PENTE=DSP['FREQ_PENTE']
      if DSP['FREQ_PENTE'] != None :
         wn=FREQ_PENTE*2.*pi 
      SPECTRE =None


# 2) donnees SRO si present

   if SPEC_UNIQUE !=None:
      SPECTRE=SPEC_UNIQUE
      dico_err={'ERRE_ZPA': list(SPECTRE['ERRE_ZPA']), 'ERRE_MAX':  list(SPECTRE['ERRE_MAX']), 'ERRE_RMS': list(SPECTRE['ERRE_RMS'] )}
      err_def=0.2
      for keys in dico_err :
         if len(dico_err[keys]) < 2 :
              dico_err[keys].append(err_def)
      NB_ITER   =SPEC_UNIQUE['NB_ITER']
      FREQ_PENTE=None

   if SPEC_MEDIANE !=None:
      SPECTRE=SPEC_MEDIANE
      NB_ITER   =SPEC_MEDIANE['NB_ITER']
      dico_err={'ERRE_ZPA': list(SPECTRE['ERRE_ZPA']), 'ERRE_MAX':  list(SPECTRE['ERRE_MAX']), 'ERRE_RMS': list(SPECTRE['ERRE_RMS'] )}
      err_def=0.2
      for keys in dico_err :
         if len(dico_err[keys]) < 2 :
              dico_err[keys].append(err_def)
      if NB_TIRAGE==1:
         UTMESS('F', 'SEISME_38' )
      if SPEC_MEDIANE['FREQ_PENTE'] != None :
         FREQ_PENTE=SPEC_MEDIANE['FREQ_PENTE']
         wn=FREQ_PENTE*2.*pi
      else:   FREQ_PENTE = None

   if SPEC_FRACTILE !=None:
      SPECTRE=SPEC_FRACTILE
      if SPEC_FRACTILE['FREQ_PENTE'] != None :
         FREQ_PENTE=SPEC_FRACTILE['FREQ_PENTE']
         wn=FREQ_PENTE*2.*pi
      else:   FREQ_PENTE = None
      spec_sigma=SPEC_FRACTILE['SPEC_1_SIGMA']
      f_spec_sigma = t_fonction(spec_sigma.Absc(), spec_sigma.Ordo(), para=para_dsp)


   if SPECTRE !=None:
      spec_osci = SPECTRE['SPEC_OSCI']
      amo  =SPECTRE['AMOR_REDUIT'] 
      SPEC_PAS   =SPECTRE['FREQ_PAS'] 
      norme_sro=NORME       
      l_freq, sro_ref = spec_osci.Valeurs()

      if SPECTRE==SPEC_FRACTILE :
         f_spec_sigma =f_spec_sigma.evalfonc(l_freq)
         sro_beta=NP.log(f_spec_sigma.vale_y/sro_ref)
         f_beta = t_fonction(l_freq, sro_beta, para=para_dsp)



      ZPA=sro_ref[-1]
      F_MIN=l_freq[0]
      if FREQ_FILTRE == None :
         F_CORNER=0.0

      if FREQ_COUP> l_freq[-1]:
         sro_ref.append(ZPA )
         l_freq.append(FREQ_COUP)


      if F_MIN> 0.0:
         l_freq.insert(0, 0.0)
         sro_ref.insert(0, 0.0)


      f_spec = t_fonction(l_freq, sro_ref, para=para_dsp)

      SRO_args={'TSM':DUREE, 'FCOUP': FREQ_COUP,'NORME':norme_sro,'AMORT':amo, 'PAS': SPEC_PAS,'FCORNER':F_CORNER,'FMIN':F_MIN }



#  ------------------------------------------------------------------
#  ECHANTILLONNAGE
#  ------------------------------------------------------------------   

   if TYPE=='CONSTANT':
      TTS=DUREE # on simule uniquement la phase forte si CONSTANT 
      NB_POIN= int(ceil((TTS/DT+1)/2.) *2. )     # on prend NB_POIN pair uniquement   
      DW=2.*OM/NB_POIN     
      TT=(NB_POIN-1)*DT

   elif NB_POIN!=None :   # on calcule  la duree de simulation si NB_POIN donne
      if  NB_POIN % 2 != 0:
         NB_POIN=NB_POIN+1
      TT=(NB_POIN-1)*DT   
      DW=2.*OM/NB_POIN
      if TT> 3.*DUREE and SPECTRE !=None:
         UTMESS('A', 'SEISME_39', valk=(str(TT)))


   else:             # on prend 3* phase forte comme duree de simulation
      TTS=INST_INI+3.*DUREE     
      NB_POIN= int(ceil((TTS/DT+1)/2.) *2. )     # on prend NB_POIN pair uniquement   
      DW=2.*OM/NB_POIN     
      TT=(NB_POIN-1)*DT

   l_temps=NP.arange(0., NB_POIN*DT,  DT)
   l_w=NP.arange(-OM+DW/2., OM+DW/2., DW)     
   l_w2=NP.arange(DW/2., OM+DW/2., DW)   
   nbfreq=2*len(l_w2) 

# parfois les listes ne sont bien construites  pour cause d'erreur num si valeurs reeles
   l_temps=l_temps[0:NB_POIN]
   l_w=l_w[0:NB_POIN]
   l_w2=l_w2[0:NB_POIN/2]  
       
   if INFO==2:
      print  'FREQUENCE DE COUPURE =',FREQ_COUP ,'Hz     NB_POIN =', NB_POIN ,'Hz     FREQ_PAS =', DW/2./pi, 'Hz'
      print  'PAS DE TEMPS =',DT,  '     INTERVALLE DE TEMPS =',TT
      print  'FREQ_FILTRE  =', F_CORNER,  'Hz'
   assert DUREE+INST_INI <= TT , "Duree de la phase forte > duree du seisme" 
   assert  NB_POIN==nbfreq
   assert len(l_temps)==NB_POIN   
   assert len(l_w)==NB_POIN      

   if SPECTRE !=None and SPEC_PAS == None :
      SPEC_PAS   =DW/2./pi
      SRO_args['PAS']=SPEC_PAS


#     ----------------------------------------------------------------- 
#          MODULATION   GAMMA et JH, constant
#     -----------------------------------------------------------------

   if PGA!= None:      
      spec=calc_dsp_KT(l_w2,wg, amo, F_CORNER)
      m0,m1,m2,vop,delta=Rice2(l_w2,spec) 
      nup=peak(0.5, DUREE, vop,delta)
      sigma=PGA*NORME/nup
      if INFO==2:
         print "FACTEUR DE PIC = ", nup,  " SIGMA = ", sigma


#     -----------------------------------------------------------------
   if TYPE=='GAMMA':
      T1=INST_INI
      T2=T1+DUREE 
      x0=[1.3,0.25 ]
#      assert x0[0]>1.0
      liste_t=NP.arange(0., TT,  0.01)       
      N1= NP.searchsorted(liste_t,T1) 
      N2= NP.searchsorted(liste_t,T2)        
      fqt_ini=fonctm_gam(liste_t, 1.0,x0[0],x0[1]) 
      aria,TSM, t1, t2 =f_ARIAS_TSM (liste_t, fqt_ini, NORME) 
      x_opt=fmin(f_opta,x0,args=(liste_t, N1, N2))     
      a2=x_opt[0]
      a3=x_opt[1]
   
      fqt=fonctm_gam(l_temps, 1.0,a2,a3)

      aria,TSM, t1, t2 =f_ARIAS_TSM (l_temps, fqt, NORME)
      if INFO==2:
         print 'PARAMETRES DE LA FONCTION GAMMA:',a2, a3 
         print 'PARAMETRES INTENSITE ARIAS,  DUREE PHASE FORTE, T1, T2  :', aria , TSM, t1, t2

      if SPECTRE !=None :
#         int12 =NP.trapz(fqt[N1:N2]**2,l_temps[N1:N2])
         int12 =NP.trapz(fqt**2,l_temps)# equivalence energie totale avec signal module par CONSTANT sur DUREE
         fqt=fqt*sqrt(DUREE/int12)     
      elif  ARIAS!= None:
         vale_arias=f_ARIAS (l_temps, fqt, NORME)      
         fqt=fqt*sqrt(ARIAS/vale_arias)
      elif  ECART!= None:
         int12 =NP.trapz((fqt[N1:N2])**2,l_temps[N1:N2])
         fqt=fqt*ECART*sqrt(DUREE/int12)
      elif PGA!= None:
         int12 =NP.trapz(fqt[N1:N2]**2,l_temps[N1:N2])
         fqt=fqt*sigma*sqrt(DUREE/int12)


#     -----------------------------------------------------------------
   if TYPE=='JENNINGS_HOUSNER': 
      x0=[0.5,1.0 ]
      t1_ini=[2.0]
#      identify T1 such that Inta1=0.05*Arias
      liste_t=NP.arange(0., TT,  0.01)

      t_opt=fmin(f_opt1,t1_ini,args=(liste_t, DUREE, 0.5,1.0))     
      T1= t_opt[0]
      x_opt=fmin(f_opt2,x0,args=(liste_t, T1, DUREE))    
      alpha=x_opt[0]
      beta=x_opt[1] 
      T2=T1+DUREE  
      N1= NP.searchsorted(liste_t,T1)  
      N2= NP.searchsorted(liste_t,T2)        

      fqt=fonctm_JetH(l_temps, T1,T2,alpha, beta) 
 
      if INFO==2:  
         print 'PARAMETRES DE LA FONCTION DE JENNINGS&HOUSNER:',alpha, beta
         aria,TSM, t1, t2 =f_ARIAS_TSM (l_temps, fqt, NORME)
         print 'PARAMETRES INTENSITE ARIAS,  DUREE PHASE FORTE, T1, T2  :'  ,aria , TSM, t1, t2
        
 
      if SPECTRE !=None :
         fqt=fqt
      elif  ARIAS!= None:
         vale_arias=f_ARIAS (l_temps, fqt, NORME)
         fqt=fqt*sqrt(ARIAS/vale_arias)
      elif  ECART!= None:
         int12 =NP.trapz((fqt[N1:N2])**2,l_temps[N1:N2])
         fqt=fqt*ECART*sqrt(DUREE/int12)
      elif PGA!= None:
         int12 =NP.trapz((fqt[N1:N2])**2,l_temps[N1:N2])
         fqt=fqt*sigma*sqrt(DUREE/int12)    


#     -----------------------------------------------------------------
   if TYPE == 'CONSTANT':
      if SPECTRE !=None :
         fq=1.0 
      elif  ARIAS!= None:
         vale_arias=TT*pi/(2.*NORME)
         fq=sqrt(ARIAS/vale_arias)
      elif  ECART!= None:
         fq=ECART
      elif PGA!= None:
         fq=sigma 
      fqt=NP.array([fq]*len(l_temps))

#     -----------------------------------------------------------------

   f_mod=t_fonction(l_temps,fqt,para=para_modul )




##     ----------------------------------------------------------------- 
##     CONSTRUCTION DSP  
##     -----------------------------------------------------------------


   if DSP!=None:
      if FREQ_PENTE == None  :       
         #calcul du facteur de normalisation
            dsp=calc_dsp_KT(l_w2,wg, amo,F_CORNER )
            S_cst=1./(NP.trapz(dsp,l_w2)*2.) # constante de normalisation pour que ecart_type=1
         #calcul DSP KT
            vale_dsp_KT=calc_dsp_KT(l_w2,wg, amo, F_CORNER, S_cst)
            fonc_dsp = t_fonction(l_w2, vale_dsp_KT, para=para_dsp, )             

      elif FREQ_PENTE != None  :  
            KT_args={'FCORNER':F_CORNER, 'W0':wg, 'Xi0':amo ,'WPENTE':wn, 'TYPE_DSP': 'KT'}



   if SPECTRE !=None :
      f_dsp = SRO2DSP(f_spec, **SRO_args)  #  CALCUL DE LA DSP SPECTRUM-COMPATIBLE   
      fonc_dsp = f_dsp.evalfonc(l_w2)
      FC=0.05

      if SPEC_MEDIANE != None :

         if NB_ITER>0 :
            fonc_dsp_opt, liste_rv =itersim_SRO(fonc_dsp, f_spec, norme_sro, amo,  DUREE , NB_ITER, f_mod,  INFO, dico_err,  F_MIN, F_CORNER,NB_TIRAGE)

            if FREQ_PENTE != None :
               wg,amort,R0,R2, f_FIT =DSP2FR(fonc_dsp_opt,FC)
               FR_args={'FCORNER':FC, 'W0':wg, 'Xi0':amort ,'WPENTE':wn,'para_R0':R0,'para_R2':R2,'fonc_FIT':f_FIT, 'TYPE_DSP': 'FR'}


         elif FREQ_PENTE != None:
            wg,amort,R0,R2, f_FIT =DSP2FR(fonc_dsp,FC)
            FR_args={'FCORNER':FC, 'W0':wg, 'Xi0':amort ,'WPENTE':wn,'para_R0':R0,'para_R2':R2,'fonc_FIT':f_FIT, 'TYPE_DSP': 'FR'}


      if SPEC_FRACTILE !=None:   
         Periods=1./(l_w2/(2.*pi))
         Periods, MAT_COVC=corrcoefmodel(Periods, f_beta)
         if FREQ_PENTE != None:
            wg,amort,R0,R2, f_FIT =DSP2FR(fonc_dsp,FC)
            FR_args={'FCORNER':FC, 'W0':wg, 'Xi0':amort ,'WPENTE':wn,'para_R0':R0,'para_R2':R2,'fonc_FIT':f_FIT, 'TYPE_DSP': 'FR'}



###     ----------------------------------------------------------------- 
##     ALGORITHMES DE GENERATION DE SIGNAUX ET CREATION RESU
##     -----------------------------------------------------------------
      
##     CREATION TAB_FONC RESU
##     -----------------------------------------------------------------
   # table resultat
   tab = Table(titr='GENE_ACCE_SEISME concept : %s' % self.sd.nom)   
 #--- construction des fonctions sortie
   _f_out=[None]*NB_TIRAGE



###    BOUCLE SUR NB_TIRAGE (nombre accelero a generer)
##     -----------------------------------------------------------------
   for ntir in range(NB_TIRAGE):
 
      if DSP!=None:

         if INFO==2:
            print '----------------------------------------------------------'
            print 'TIRAGE ', ntir+1

         if FREQ_PENTE != None :
            Xt=gene_traj_gauss_evol1D(calc_dsp_KT, l_w2, l_temps,T1,T2,  **KT_args   )
            Xt=NP.array(Xt)*fqt    
            listv=list(Xt)
         else:
            Xt=DSP2ACCE1D(fonc_dsp) 
            Xt=NP.array(Xt)*fqt
            listv=list(Xt)


      if SPECTRE!=None:


         if SPEC_UNIQUE !=None:

            if INFO==2:
               print '----------------------------------------------------------'
               print 'TIRAGE ', ntir+1

            if  NB_ITER >0:     #     cas  SPEC_UNIQUE !=None:
               fonc_dsp_opt, rv =itersim_SRO(fonc_dsp, f_spec, norme_sro, amo,  DUREE , NB_ITER, f_mod,  INFO, dico_err,F_MIN, F_CORNER)
               Xt=DSP2ACCE1D(fonc_dsp_opt ,rv[0])
               Xt=Xt*fqt
               listv=list(Xt)
            else:
               Xt=DSP2ACCE1D(fonc_dsp) 
               Xt=NP.array(Xt)*fqt
               listv=list(Xt)



         if SPEC_MEDIANE != None:
               
            if FREQ_PENTE != None and NB_ITER>0:
               for (ntir,rvtir) in  enumerate(liste_rv):
                  Xt=gene_traj_gauss_evol1D(calc_dsp_FR, l_w2, l_temps,T1,T2,rvtir, **FR_args)
                  listv=list(Xt*fqt)
                  _f_out[ntir]=DEFI_FONCTION( ABSCISSE=tuple(l_temps), ORDONNEE=listv,**para_traj  )
                  tab.append({'NUME_ORDRE' : ntir+1,  'FONCTION' : _f_out[ntir].nom})
               break

            elif NB_ITER>0:
               for (ntir,rvtir) in  enumerate(liste_rv):
                  Xt=DSP2ACCE1D(fonc_dsp_opt ,rvtir)
                  listv=list(Xt*fqt) 
                  _f_out[ntir]=DEFI_FONCTION( ABSCISSE=tuple(l_temps), ORDONNEE=listv,**para_traj  )
                  tab.append({'NUME_ORDRE' : ntir+1,  'FONCTION' : _f_out[ntir].nom})
               break 

            elif FREQ_PENTE != None:
               Xt=gene_traj_gauss_evol1D(calc_dsp_FR, l_w2, l_temps,T1,T2, **FR_args)
               Xt=NP.array(Xt)*fqt
               listv=list(Xt)

            else:
               Xt=DSP2ACCE1D(fonc_dsp) 
               Xt=NP.array(Xt)*fqt
               listv=list(Xt)




         if SPEC_FRACTILE !=None:
#
#            if FREQ_PENTE != None and NB_ITER>0:
#               for (ntir,rvtir) in  enumerate(liste_rv):
#                  alpha2=RAND_VEC(Periods, MAT_COVC, len(l_w2), para=2.0)
#                  FR_args['ALEA_DSP']=alpha2
#                  Xt=gene_traj_gauss_evol1D(calc_dsp_FR, l_w2, l_temps,T1,T2,rvtir, **FR_args)
#                  listv=list(Xt*fqt)
#                  _f_out[ntir]=DEFI_FONCTION( ABSCISSE=tuple(l_temps), ORDONNEE=listv,**para_traj  )
#                  tab.append({'NUME_ORDRE' : ntir+1,  'FONCTION' : _f_out[ntir].nom})
#               break
#
#            elif NB_ITER>0:
#               for (ntir,rvtir) in  enumerate(liste_rv):
#                  fonc_dsp_opt_rv=RAND_DSP(Periods,MAT_COVC,fonc_dsp_opt )
##                  Xt=DSP2ACCE1D(fonc_dsp_opt_rv ,rvtir)
#                  Xt=DSP2ACCE1D(fonc_dsp_opt_rv )
#                  listv=list(Xt*fqt) 
#                  _f_out[ntir]=DEFI_FONCTION( ABSCISSE=tuple(l_temps), ORDONNEE=listv,**para_traj  )
#                  tab.append({'NUME_ORDRE' : ntir+1,  'FONCTION' : _f_out[ntir].nom})
#               break 


            if FREQ_PENTE != None:
               alpha2=RAND_VEC(Periods, MAT_COVC, len(l_w2), para=2.0)
               FR_args['ALEA_DSP']=alpha2
               Xt=gene_traj_gauss_evol1D(calc_dsp_FR, l_w2, l_temps,T1,T2, **FR_args)

            else:
               fonc_dsp_rv=RAND_DSP(Periods,MAT_COVC,fonc_dsp )
               Xt=DSP2ACCE1D(fonc_dsp_rv) 

            Xt=NP.array(Xt)*fqt
            listv=list(Xt)



#--- construction des fonctions sortie
#   for iii in range(NB_TIRAGE):
      _f_out[ntir]=DEFI_FONCTION( ABSCISSE=tuple(l_temps), ORDONNEE=tuple(listv),**para_traj  )
      tab.append({'NUME_ORDRE' : ntir+1,  'FONCTION' : _f_out[ntir].nom})

##     -----------------------------------------------------------------            
##     REMPLISSAGE TAB_FONC RESU
##     -----------------------------------------------------------------
#
#--- construction de la table produite
    # Creation du concept en sortie
   dict_keywords = tab.dict_CREA_TABLE()
   tab_out = CREA_TABLE(TYPE_TABLE='TABLE_FONCTION',
                         **dict_keywords)


   return ier

