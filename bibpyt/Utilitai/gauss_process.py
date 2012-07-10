#@ MODIF gauss_process Utilitai  DATE 10/07/2012   AUTEUR ZENTNER I.ZENTNER 
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
# RESPONSABLE ZENTNER I.ZENTNER

# Routines for random signal generation 
"""gauss_process.py

A collection of general-purpose routines using Numeric

gene_traj_gauss        ---      generation of trajectories of a stationary Gaussian process
calc_dsp_KT            ---      KT PSD
"""

from math import pi,ceil, exp, sqrt, log 
import numpy as NP
import aster_fonctions


# ----------------------------------------------------------------- 
#     ALGORITHME DE GENERATION DE SIGNAUX GAUSSIENS classique-----
#-----------------------------------------------------------------


def gene_traj_gauss(calc_dsp_KT, lw2,wgt,amo , FMIN=0.0, nbtraj=1, **args):

#    IN: 
#      calc_dsp_KT   :  function for the definition of the PSD matrix
#      lw2  :    the list of frequencies corresponding to spec (0, OM)
#      nbtraj : leading dim of spec
#    OUT: 
#       Xt trajectoire du processus gaussien stationnaire normalise (m=0, ect=1)

   import aster_core
 

#calcul du facteur de normalisation
   dsp=calc_dsp_KT(lw2,wgt, amo)
   S_cst=1./NP.trapz(dsp,lw2)*0.5 # constante de normalisation


#ajouter:   FMIN, FMAX
   DW=lw2[1]-lw2[0]
   aster_core.matfpe(-1)

   nbfreq2=len(lw2)
   nbfreq=nbfreq2*2
   MAT=NP.matrix([0.0+0j]*nbtraj*nbtraj) 
   MAT.resize(nbtraj,nbtraj) 
   CS=NP.matrix([0.0+0j]*nbtraj*nbfreq)    
   CS.resize(nbfreq,nbtraj)  
   Xt=NP.matrix([0.0]*nbtraj*nbfreq)    
   Xt.resize(nbfreq, nbtraj)     
       
   for (iifr,freq) in enumerate(lw2):
      if freq < FMIN:
         pass      
      else :
         MAT=calc_dsp_KT([freq],wgt, amo, S_cst)
      if nbtraj==1:
            MATc=NP.sqrt(MAT)
      else:
         try:
            MATc=NP.linalg.cholesky(MAT)

         except ValueError, msg :
                  print '------ERROR-----------'

      vecc1=NP.matrix(NP.random.normal(0.0,1.,nbtraj)+1j*NP.random.normal(0.0,1.,nbtraj))
      vecc2=NP.matrix(NP.random.normal(0.0,1.,nbtraj)+1j*NP.random.normal(0.0,1.,nbtraj))
      vale_xp=MATc*NP.transpose(vecc1)
      vale_xn=NP.conjugate(MATc)*NP.transpose(vecc2)
      CS[nbfreq2+iifr]=NP.transpose(vale_xp)
      CS[nbfreq2-iifr-1]=NP.transpose(vale_xn)
           
   CS=NP.transpose(CS)
   SX=NP.fft.ifft(CS,nbfreq,1)*nbfreq
   ha=NP.exp(-1.j*pi*NP.arange(nbfreq)*(1.-1./nbfreq))      

   for kkk in range(nbfreq): 
      Xt[kkk]=sqrt(DW)*(SX[:,kkk]*ha[kkk]).real
   Xt=NP.transpose(Xt)   
   aster_core.matfpe(1)  
   
   return Xt  
   
# ----------------------------------------------------------------- ------- 
#  ALGORITHME DE GENERATION DE SIGNAUX GAUSSIENS DSP evolutive non separable-----
#------------------------------------------------------------------------ 

def gene_traj_gauss_evol(calc_dsp_KT, l_w2,l_temps,t_ini, wg,wp,amo , FMIN=0.0, nbtraj=1, **args):
   from math import cos , sin
#    IN: 
#      calc_dsp_KT   :  function for the definition of the PSD matrix
#      lw2  :    the list of frequencies corresponding to spec (0, OM)
#       wg, wn : fond freq and evolution
#      nbtraj : leading dim of spec
#    OUT: 
#       Xt trajectoire du processus gaussien stationnaire normalise (m=0, ect=1)

   import aster_core
   nbfreq2=len(l_w2)
   nbfreq=2*nbfreq2
#   MAT=NP.matrix([0.0+0j]*nbtraj*nbtraj) 
#   MAT.resize(nbtraj,nbtraj)    
   Xt=NP.matrix([0.0]*nbtraj*nbfreq)    
   Xt.resize(nbtraj, nbfreq)     


#   print " ------- gene_traj_gauss_evol  --------"    
   DW=l_w2[1]-l_w2[0]   
   vecc1=NP.transpose(NP.array(NP.random.normal(0.0,1.,nbfreq2)+1j*NP.random.normal(0.0,1.,nbfreq2)))
   vecc2=NP.transpose(NP.array(NP.random.normal(0.0,1.,nbfreq2)+1j*NP.random.normal(0.0,1.,nbfreq2)))
          
   for (nii, tii) in enumerate(l_temps):   

      if tii<t_ini:
         wgt=wg      
      else:   
         wgt=wg+wp*(tii-t_ini)      
      assert wgt>0.0,  "ATTENTION, FREQ_FOND <0 A L INSTANT "+ str(tii)
   
    #calcul du facteur de normalisation
      dsp=calc_dsp_KT(l_w2,wgt, amo)
      S_cst=1./NP.trapz(dsp,l_w2)*0.5 # constante de normalisation

      if nbtraj==1:
         MAT=calc_dsp_KT(l_w2,wgt, amo, S_cst)
         MATc=(NP.sqrt(MAT))
         vale_xp=(MATc*vecc1)*NP.exp(1.j*l_w2*tii)
         vale_xn=(MATc*vecc2)*NP.exp(-1.j*l_w2*tii)
         vale_Xt= sum(vale_xp)+ sum(vale_xn)

      else:
         vale_Xt=0.0+0.0j
         for (iifr,freq) in enumerate(l_w2):
            MAT=calc_dsp_KT([freq],wgt, amo, S_cst)
            MATc=NP.linalg.cholesky(MAT)
            vsin=1.j*sin(freq*tii)
#         vale_xp=(MATc*vecc1[iifr])*NP.exp(1.j*freq*tii)
#         vale_xn=(NP.conjugate(MATc)*vecc2[iifr])*NP.exp(-1.j*freq*tii)
            vcos=cos(freq*tii)
            vale_xp=(MATc*vecc1[iifr])*(vcos+vsin)
            vale_xn=(NP.conjugate(MATc))*vecc2[iifr]*(vcos-vsin)
            vale_Xt= vale_Xt + vale_xp+  vale_xn

##            vecc1=NP.array(NP.random.uniform(0.0,1.,nbtraj))
##            vecc2=NP.array(NP.random.uniform(0.0,1.,nbtraj))
# #           vale_xn= NP.conjugate(MATc)*NP.exp(-1.j*freq*tii+1.j*vecc2*2.*pi)
##            vale_xp= (MATc)*NP.exp(1.j*freq*tii+1.j*vecc1*2.*pi)
##            print "MATc", MATc
# 
#             vec_rand=NP.random.uniform(0.0,1.)*2.*pi
#             vale_Xt=vale_Xt+ MATc*cos(freq*tii+vec_rand)     
#          print vale_Xt 
      Xt[0,nii]=NP.real(vale_Xt)*sqrt(DW)       

   aster_core.matfpe(1)
          

   return Xt
   
   
        
#----------------------------------------------------------------- 
#     KANAI TAJIMI PSD AT INSTANT T -----
#-----------------------------------------------------------------

def calc_dsp_KT(lfreq, w0,amor, So=1.0):
# KT model 
 
      x11  =NP.array([4.*(amor**2)*(w0**2)*FREQ**2  for FREQ in lfreq ])
      xnum =x11+w0**4
      denom=NP.array([ (w0**2-FREQ**2)**2 for FREQ in lfreq ])
      denom=denom+x11
      valkt=xnum/denom
# CP filter
      wcp=0.5*pi
      amocp=1.0
      x11  =NP.array([4.*(amocp**2)*(wcp**2)*FREQ**2  for FREQ in lfreq ])
      denom=NP.array([ (wcp**2-FREQ**2)**2 for FREQ in lfreq ])
      denom=denom+x11
      valcp=NP.array([FREQ**4  for FREQ in lfreq ])/denom

      dsp=valcp*valkt
      return dsp*So  
   
   
# ----------------------------------------------------------------- 
#     ARIAS, duree phase forte TSM , T1 et T2 -----
#-----------------------------------------------------------------
 

def f_ARIAS (ta, acce)   :
      acce2=NP.array(acce)**2   
      arias = NP.trapz(acce2,ta)   # energie 
      arias =arias*pi/(2.*9.81)   # indic Arias      
      return arias     

def f_ARIAS_TSM (ta, acce)   :
      arias =f_ARIAS (ta, acce)  # indic Arias   
      ener=arias*(2.*9.81)/pi
      acce2=NP.array(acce)**2 
      cumener=NP.array([NP.trapz(acce2[0:ii+1],ta[0:ii+1])  for ii in range(len(ta))])
      fract=cumener/ener 
      n1= NP.searchsorted(fract, 0.05)  
      n2= NP.searchsorted(fract,0.95)
#      n45= NP.searchsorted(fract,0.45)      
      TSM=ta[n2]-ta[n1]
      T1=ta[n1]
      T2=ta[n2]      
      return arias, TSM , T1,  T2     
         
def f_ENER_qt (ta, acce2, n1, n2)   :
      ener= NP.trapz(acce2,ta)   # energie    
      P1=NP.trapz(acce2[0:n1],ta[0:n1])/ener
      P2=NP.trapz(acce2[0:n2],ta[0:n2])/ener             
      return ener, P1,  P2     
         


#----------------------------------------------------------------- 
#     FONCTION DE MODULATION Gamma
#----------------------------------------------------------------
 
 # fonction de modulation gamma:  calcul pour liste de freq (normalisee si a1=1.0)
def fonctm_gam(ltemps, a1,a2,a3):  
      qt=NP.array([a1*tt**(a2-1)*exp(-a3*tt) for tt in ltemps])
      return qt
      
 # fonction de modulation gamma: fonction cout pour identification des parametres     
def f_opta(x0,  ltemps, n1, n2) :
      alpha=x0[0]
      beta=x0[1]
      if alpha<=1.:
         resu=10000
      elif beta<0.01:
         resu=10000
      else:
         qt=fonctm_gam(ltemps, 1.0,alpha,beta)
         ener, PINI, PFIN = f_ENER_qt (ltemps, qt**2, n1, n2)
         resu=sqrt((PINI-0.05)**2+ (PFIN-0.95)**2)
      return resu


# ----------------------------------------------------------------- 
#     FONCTION DE MODULATION Jennnings & Housner
# -----------------------------------------------------------------
 
 # fonction de modulation Jennings & Housner normalisee 
def fonctm_JetH(ltemps, T1,DUREE, a1,a2) :     
      qt=[]
      T2=T1+DUREE
      for tt in ltemps:
         if tt<T1:
            qt.append((tt/T1)**2)
         elif tt<T2 :
            qt.append(1.0)
         else :
            qt.append(exp(-a1*(tt-T2)**a2))
      return NP.array(qt)


    
