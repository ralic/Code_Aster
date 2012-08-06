#@ MODIF gene_acce_seisme_ops Macro  DATE 06/08/2012   AUTEUR ZENTNER I.ZENTNER 
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
import os
import copy
import traceback
from types import ListType, TupleType
from math import pi,ceil, exp, sqrt, log, cos

import aster_core

EnumTypes = (ListType, TupleType)


def gene_acce_seisme_ops(self,PAS_INST,DSP,MODULATION, NB_POIN,PESANTEUR,  TITRE,INFO,**args):

   import numpy as NP
   import aster
   from Accas                 import _F
   from Utilitai.Utmess       import UTMESS
   from Cata_Utils.t_fonction import t_fonction, t_fonction_c
   from Utilitai.Table        import Table
   from Macro.defi_inte_spec_ops import tocomplex
   from Cata.cata import nappe_sdaster,fonction_sdaster,fonction_c
   import aster_fonctions
   from Utilitai.optimize   import fmin
   from Utilitai.gauss_process  import  gene_traj_gauss, gene_traj_gauss_evol , Rice2, peak
   from Utilitai.gauss_process  import  calc_dsp_KT, f_ARIAS, f_ARIAS_TSM,fonctm_gam,f_opta, fonctm_JetH
   EnumTypes = (list, tuple)
   
   
   commande='GENE_ACCE_SEISME'

   ier = 0
   # La macro compte pour 1 dans la numérotation des commandes
   self.set_icmd(1)

   # Le concept sortant (de type table_fonction) est tab
   self.DeclareOut('tab_out', self.sd)

   # On importe les définitions des commandes a utiliser dans la macro
   # Le nom de la variable doit être obligatoirement le nom de la commande
   CREA_TABLE    = self.get_cmd('CREA_TABLE')
   CALC_TABLE    = self.get_cmd('CALC_TABLE')
   RECU_FONCTION = self.get_cmd('RECU_FONCTION')
   DEFI_FONCTION  = self.get_cmd('DEFI_FONCTION')

   para      = { 'INTERPOL'      : ['LIN', 'LIN'],
                     'PROL_DROITE'   : 'EXCLU',
                      'PROL_GAUCHE'   : 'EXCLU',}

#  ------------------------------------------------------------------
#  SEED POUR lA GENERATION DE VA
#  ------------------------------------------------------------------
  
   INIT_ALEA=args['INIT_ALEA']
   if INIT_ALEA!=None :
      NP.random.seed(INIT_ALEA)

#  ------------------------------------------------------------------
#  RECUP DONNEES   
#  ------------------------------------------------------------------   

# donnees DSP
   amo  =DSP['AMOR_REDUIT']   
   F_RED=DSP['FREQ_FOND']
   wg=F_RED*2.*pi 
   if DSP['FREQ_PENTE'] != None :
      FN=DSP['FREQ_PENTE']
      wn=FN*2.*pi   

# donnees fonction modulation   
   NORME=PESANTEUR
   DUREE=MODULATION['DUREE_PHASE_FORTE']
   ARIAS=MODULATION['INTE_ARIAS']
   PGA=MODULATION['ACCE_MAX']
   ECART=MODULATION['ECART_TYPE']
   TYPE=MODULATION['TYPE']
   if  ECART!= None:
      ECART=ECART*NORME
   if TYPE!='CONSTANT':
      TMID=MODULATION['INST_INI']
   else:
      TMID=0.0
   print "TYPE MODULATION",  TYPE

#  ------------------------------------------------------------------
#  ECHANTILLONNAGE
#  ------------------------------------------------------------------   

# discretisation temps et freq
   DT=PAS_INST
   OM=pi/DT
   FREQ_COUP=OM/2./pi
      
   if NB_POIN!=None :
      if  NB_POIN % 2 != 0:
         NB_POIN=NB_POIN+1
      TT=(NB_POIN-1)*DT   
      DW=2.*OM/NB_POIN
   else:
      if TYPE=='CONSTANT':
         TTS=DUREE # on prend  phase forte si CONSTANT      
      else:
         TTS=TMID+4.*DUREE    # on prend 3* phase forte    

      NB_POIN= int(ceil((TTS/DT+1)/2.) *2. )     # on prend NB_POIN pair uniquement   
      DW=2.*OM/NB_POIN     
      TT=(NB_POIN-1)*DT

   if TYPE=='CONSTANT':
      DUREE=TT
   

   l_temps=NP.arange(0., NB_POIN*DT,  DT)
   l_w=NP.arange(-OM+DW/2., OM+DW/2., DW)     
   l_w2=NP.arange(DW/2., OM+DW/2., DW)   
   nbfreq=2*len(l_w2) 

# parfois les listes ne sont bien construites  pour cause d'erreur num si valeurs reeles
   l_temps=l_temps[0:NB_POIN]
   l_w=l_w[0:NB_POIN]
   l_w2=l_w2[0:NB_POIN/2]  
       
   if INFO==2:
      print  'FREQUENCE DE COUPURE =',FREQ_COUP ,'     NB_POIN =', NB_POIN ,'     FREQ_PAS =', DW/2./pi
      print  'PAS DE TEMPS =',DT,  '     INTERVALLE DE TEMPS =',TT

   assert DUREE+TMID <= TT , "Duree de la phase forte > duree du seisme" 
   assert  NB_POIN==nbfreq
   assert len(l_temps)==NB_POIN   
   assert len(l_w)==NB_POIN      
   nbtraj=1


#     ----------------------------------------------------------------- 
#          MODULATION   GAMMA et JH
#     -----------------------------------------------------------------

   if PGA!= None:      
      spec=calc_dsp_KT(l_w2,wg, amo)
      m0,m1,m2,vop,delta=Rice2(l_w2,spec) 
      nup=peak(0.5, DUREE, vop,delta)
      sigma=PGA*NORME/nup
      if INFO==2:
         print "FACTEUR DE PIC = ", nup,  " SIGMA = ", sigma

# identification des parametres a1, a2,a3 a partir de q2
#x=(a1,a2, ARIAS)
   T1=TMID
   T2=TMID+DUREE 
   if TYPE=='GAMMA':
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
#
      aria,TSM, t1, t2 =f_ARIAS_TSM (l_temps, fqt, NORME)
      if INFO==2:
         print 'PARAMETRES GAMMA alpha_2, alpha_3:',a2, a3 
         print 'ARIAS  PHASE FORTE, T1, T2 MODELE :', aria , TSM, t1, t2
         print 'ARIAS  PHASE FORTE, T1 T2  DONNES :',DUREE, TMID, DUREE + TMID
      if  ARIAS!= None:
         vale_arias=f_ARIAS (l_temps, fqt, NORME)      
         fqt=fqt*sqrt(ARIAS/vale_arias)
      if  ECART!= None:
         int12 =NP.trapz(fqt[N1:N2]**2,l_temps[N1:N2])
         fqt=fqt*ECART*sqrt(DUREE/int12)
      if PGA!= None:
         int12 =NP.trapz(fqt[N1:N2]**2,l_temps[N1:N2])
         fqt=fqt*sigma*sqrt(DUREE/int12)

   if TYPE=='JENNINGS_HOUSNER': 
      (alpha, beta)=MODULATION['PARA'] 
      assert alpha>=0.0, "ERROR MODULATION: PARA VALUE NOT POSITIVE"
      assert beta>=0.0, "ERROR MODULATION: PARA VALUE NOT POSITIVE"          
      fqt=fonctm_JetH(l_temps, TMID,DUREE,alpha, beta) 
      if  ARIAS!= None:
         vale_arias=f_ARIAS (l_temps, fqt, NORME)
         fqt=fqt*sqrt(ARIAS/vale_arias)
      if  ECART!= None:
         fqt=fqt*ECART
      if PGA!= None:
         fqt=fqt*sigma      

   if TYPE == 'CONSTANT':
      if  ARIAS!= None:
         vale_arias=TT*pi/(2.*NORME)
         fqt=sqrt(ARIAS/vale_arias)
      if  ECART!= None:
         fqt=ECART
      if PGA!= None:
         fqt=sigma 

##     ----------------------------------------------------------------- 
##     ALGORITHME DE GENERATION DE SIGNAUX GAUSSIENS -----
##     -----------------------------------------------------------------
   aster_core.matfpe(-1)

 
 #     -----------------------------------------------------------------              
   if DSP['FREQ_PENTE'] != None :      
      if INFO==2:
         print "CAS DSP NON SEPARABLE" 

      Xt=gene_traj_gauss_evol(calc_dsp_KT, l_w2, l_temps,T1,T2,wg,wn,amo )
      Xt[0,:]  =NP.array([Xt[0,iii]*fqti  for iii,fqti in enumerate(fqt) ])  

#     -----------------------------------------------------------------    
   else :
      if INFO==2:
         print "CAS DSP SEPARABLE"
   
      Xt=gene_traj_gauss(calc_dsp_KT, l_w2, wg,amo) 
      Xt=NP.array(Xt)*fqt           
#    
#
#
##     -----------------------------------------------------------------            
##     REMPLISSAGE TAB RESU
##     -----------------------------------------------------------------
#
   # table résultat
   tab = Table(titr='GENE_ACCE_SEISME concept : %s' % self.sd.nom)   

 #--- construction des fonctions sortie
 # DEF RESU des trajectoires

   para_traj      = {  'NOM_PARA' : 'INST',
                       'PROL_DROITE'   : 'EXCLU',
                       'PROL_GAUCHE'   : 'EXCLU',
                        'TITRE'   : TITRE,   }

   _f_out=[None]*nbtraj

   for iii in range(nbtraj):
      listv =[]
      for jjj in range(nbfreq):
         listv.append(Xt[iii,jjj])
      _f_out[iii]=DEFI_FONCTION( ABSCISSE=tuple(l_temps), ORDONNEE=listv,**para_traj  )
      tab.append({'NUME_ORDRE' : nbtraj,  'FONCTION' : _f_out[iii].nom})

#--- construction de la table produite
    # Creation du concept en sortie
   dict_keywords = tab.dict_CREA_TABLE()
   tab_out = CREA_TABLE(TYPE_TABLE='TABLE_FONCTION',
                         **dict_keywords)


   return ier

