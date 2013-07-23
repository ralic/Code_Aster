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
from math import pi,ceil, exp, sqrt, log, cos
import aster_core

EnumTypes = (ListType, TupleType)


def gene_acce_seisme_ops(self,PAS_INST,DSP,SPEC_UNIQUE,SPEC_MEDIANE, MODULATION, DUREE_PHASE_FORTE, NB_POIN,PESANTEUR,NB_TIRAGE, TITRE,INFO,**args):

   import numpy as NP
   import aster
   from Accas                 import _F
   from Cata_Utils.t_fonction import (
                               t_fonction, t_fonction_c, 
   )
   from Utilitai.Table        import Table
   from Macro.defi_inte_spec_ops import tocomplex
   from Cata.cata import nappe_sdaster,fonction_sdaster,fonction_c
   import aster_fonctions
   from Utilitai.optimize   import fmin
   from Utilitai.gauss_process  import  DSP2ACCE1D,itersim_SRO, gene_traj_gauss_evol , Rice2, peak, SRO2DSP
   from Utilitai.gauss_process  import  calc_dsp_KT, f_ARIAS, f_ARIAS_TSM,fonctm_gam, fonctm_JetH,dsp_filtre_CP
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
      WCP=F_CORNER*2.*pi
      FREQ_PENTE=DSP['FREQ_PENTE']
      if DSP['FREQ_PENTE'] != None :
         wn=FREQ_PENTE*2.*pi 
      SPECTRE =None


# 2) donnees SRO si present

   if SPEC_UNIQUE !=None:
      SPECTRE=SPEC_UNIQUE
      FREQ_PENTE=None

   if SPEC_MEDIANE !=None:
      SPECTRE=SPEC_MEDIANE

      FREQ_PENTE=None 
#      FREQ_PENTE=SPEC_MEDIANE['FREQ_PENTE'] 
#      if SPEC_MEDIANE['FREQ_PENTE'] != None :
#         FREQ_PENTE=SPEC_MEDIANE['FREQ_PENTE']
#         wn=FREQ_PENTE*2.*pi

   if SPECTRE !=None:
      spec_osci = SPECTRE['SPEC_OSCI']
      NB_ITER   =SPECTRE['NB_ITER']
      dico_err={'ERRE_ZPA': list(SPECTRE['ERRE_ZPA']), 'ERRE_MAX':  list(SPECTRE['ERRE_MAX']), 'ERRE_RMS': list(SPECTRE['ERRE_RMS'] )}
      err_def=0.2
      for keys in dico_err :
         if len(dico_err[keys]) < 2 :
              dico_err[keys].append(err_def)

      amo  =SPECTRE['AMOR_REDUIT'] 
      FREQ_PAS   =SPECTRE['FREQ_PAS'] 
      if FREQ_FILTRE == None :
         F_CORNER=0.0

      WCP=F_CORNER*2.*pi
      norme_sro=NORME
      para_osci = spec_osci.Parametres()
      l_freq, sro_ref = spec_osci.Valeurs()
      ZPA=sro_ref[-1]

      if FREQ_COUP> l_freq[-1]:
         sro_ref.append(ZPA )
         l_freq.append(FREQ_COUP)
      if l_freq[0]> 0.0:
         l_freq.insert(0, 0.0)
         sro_ref.insert(0, 0.0)
      f_spec = t_fonction(l_freq, sro_ref, para=para_dsp)




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
      print  'CORNER FREQUENCY (FREQ_FILTRE - Hz) =', F_CORNER
   assert DUREE+INST_INI <= TT , "Duree de la phase forte > duree du seisme" 
   assert  NB_POIN==nbfreq
   assert len(l_temps)==NB_POIN   
   assert len(l_w)==NB_POIN      
   nbtraj=1




#     ----------------------------------------------------------------- 
#          MODULATION   GAMMA et JH, constant
#     -----------------------------------------------------------------

   if PGA!= None:      
      spec=calc_dsp_KT(l_w2,wg, amo, WCP)
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
##     CONSTRUCTION DSP SI CAS SEPERABLE  OU SPECTRE  -----
##     -----------------------------------------------------------------

 #     ---------------------Non sep------

   if FREQ_PENTE != None  :      
         if INFO==2:
            print "CAS DSP NON SEPARABLE" 
#     ---------------------Construction des DSP si separabale------
   else :
         if INFO==2:
            print "CAS DSP SEPARABLE"
         if DSP!=None:    
         #calcul du facteur de normalisation
            dsp=calc_dsp_KT(l_w2,wg, amo,F_CORNER )
            S_cst=1./(NP.trapz(dsp,l_w2)*2.) # constante de normalisation pour que ecart_type=1
         #calcul DSP KT
            vale_dsp_KT=calc_dsp_KT(l_w2,wg, amo, F_CORNER, S_cst)
            fonc_dsp = t_fonction(l_w2, vale_dsp_KT, para=para_dsp, )             

         elif SPECTRE !=None :
            f_dsp=SRO2DSP(f_spec, norme_sro, amo, DUREE, FREQ_COUP, FREQ_PAS)  #  CALCUL DE LA DSP SPECTRUM-COMPATIBLE        

            if F_CORNER>0.:
               f_dsp=dsp_filtre_CP(f_dsp,F_CORNER) 
            fonc_dsp = f_dsp.evalfonc(l_w2)


         
##     ----------------------------------------------------------------- 
##     ALGORITHMES DE GENERATION DE SIGNAUX ET CREATION RESU
##     -----------------------------------------------------------------

#     -----------------------------------------------------------------            
##     CREATION TAB_FONC RESU
##     -----------------------------------------------------------------
   # table resultat
   tab = Table(titr='GENE_ACCE_SEISME concept : %s' % self.sd.nom)   
 #--- construction des fonctions sortie
   _f_out=[None]*NB_TIRAGE



# # #    -------------Boucle sur NB_TIRAGE (nombre accelero a generer)------
   for ntir in range(NB_TIRAGE):
 #     -----------------------------------------------------------------              
      if INFO==2:
        print '----------------------------------------------------------'
        print 'TIRAGE ', ntir+1

      if FREQ_PENTE != None :      

         if SPECTRE !=None :
            wg,amo,r0,r2=SRO2FR(f_in, norme, amort, TSM, FC, PAS=0.25/pi)
            Xt=gene_traj_gauss_evol(calc_dsp_FR, l_w2, l_temps,T1,T2,wn,FCORNER=F_CORNER, W0=wg, Xi0=amo , R0=r0,R2=r2, TYPE_DSP= 'FR')
         else:
            Xt=gene_traj_gauss_evol(calc_dsp_KT, l_w2, l_temps,T1,T2,wn,FCORNER=F_CORNER,W0=wg, Xi0=amo , TYPE_DSP='KT'  )

         listv=([Xt[0,iii]*fqti  for iii,fqti in enumerate(fqt) ])  

      else :
         if SPEC_UNIQUE !=None:
#         if NB_ITER   >0:# cas SPECTRE AVEC ITERATIONS, SINON NB_ITER=0
            f_accef , err_zpa =itersim_SRO(fonc_dsp, f_spec, norme_sro, amo,  DUREE , NB_ITER, f_mod,  INFO, dico_err, F_CORNER,) 
#            if err_zpa> 2.*dico_err['ERRE_ZPA'][1]*100.:
#               dico_err['ERRE_ZPA'].pop()
#               dico_err['ERRE_RMS'].pop()
#               dico_err['ERRE_MAX'].pop()
#               f_accef , err_zpa =itersim_SRO(fonc_dsp, f_spec, norme_sro, amo,  DUREE , NB_ITER, f_mod,  INFO, dico_err, F_CORNER,) 
#
            listv=f_accef.vale_y

         elif SPEC_MEDIANE !=None and NB_ITER >0:# cas SPECTRE_mediane AVEC ITERATIONS, SINON NB_ITER=0
            f_accef , err_zpa =itersim_SRO(fonc_dsp, f_spec, norme_sro, amo,  DUREE , NB_ITER, f_mod,  INFO, dico_err, F_CORNER,NB_TIRAGE)  

            for (ntir,accetir) in  enumerate(f_accef):
               listv=accetir.vale_y
               _f_out[ntir]=DEFI_FONCTION( ABSCISSE=tuple(l_temps), ORDONNEE=listv,**para_traj  )
               tab.append({'NUME_ORDRE' : ntir+1,  'FONCTION' : _f_out[ntir].nom})

            break

         else:
            rv=NP.transpose(NP.array(NP.random.normal(0.0,1.,nbfreq)+1j*NP.random.normal(0.0,1.,nbfreq)))
            Xt=DSP2ACCE1D(fonc_dsp,rv) 
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

