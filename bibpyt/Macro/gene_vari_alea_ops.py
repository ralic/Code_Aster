#@ MODIF gene_vari_alea_ops Macro  DATE 16/10/2007   AUTEUR REZETTE C.REZETTE 
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

from math import sqrt,log,exp,pi,atan2,tan

def gene_vari_alea_ops(self,**args):
  self.set_icmd(1)
  return 0

def gene_vari_alea_init(self,d):
  from Utilitai.Utmess import  UTMESS
  a     =self.etape['BORNE_INF']
  moyen =self.etape['VALE_MOY' ]
  TYPE  =self.etape['TYPE']
  if self['INIT_ALEA']!=None :
     jump =self.etape['INIT_ALEA' ]
     self.iniran(jump)
  if   TYPE=='EXP_TRONQUEE' :
     b     =self.etape['BORNE_SUP']
     if (a>=b) :
         UTMESS('F','PROBA0_1',valr=[a,b])
     elif (moyen<=a)or(moyen>=b) :
         UTMESS('F','PROBA0_2',valr=[a,moyen,b])
     k=1./(moyen-a)
     if (exp(-b*k)<1.E-12) :
         UTMESS('F','PROBA0_3')
     # résolution par point fixe
     eps   =1.E-4
     nitmax=100000
     test  =0.
     while abs((test-k)/k)>eps :
         test = k
         k    = 1./(moyen-(a*exp(-a*k) - b*exp(-b*k))/(exp(-a*k) - exp(-b*k)))
     # génération de la variable aléatoire
     alpha = exp(-a*k) - exp(-b*k)
     self.sd.valeur=-( log(exp(-a*k)-alpha*self.getran()[0] ) ) /k
  elif TYPE=='EXPONENTIELLE' :
     if (moyen<=a) :
        UTMESS('F','PROBA0_4',valr=[moyen,a])
     v = moyen-a
     u=self.getran()[0]
     x = -log(1-u)
     self.sd.valeur=a + v*x
  elif TYPE=='GAMMA'         :
     delta =self.etape['COEF_VAR' ]
     if (moyen<=a) :
        UTMESS('F','PROBA0_4',valr=[moyen,a])
     v = moyen-a
     alpha = 1./(delta**2)
     if (alpha<=1.) :
        UTMESS('F','PROBA0_5')
     gamma2 = alpha-1.
     gamm1  = 1./gamma2
     beta   = sqrt(2.*alpha-1.)
     beta2  = 1./(beta**2)
     f0     = 0.5+(1./pi)*atan2(-gamma2/beta,1.)
     c1     = 1.-f0
     c2     = f0-0.5
     vref   = 0.
     vv     = -1.
#
     while (-vv>vref) :
        u=self.getran()[0]
        gamdev = beta*tan(pi*(u*c1+c2))+gamma2
        unif=self.getran()[0]
        if unif<0. :
           UTMESS('F','PROBA0_6')
        vv= -log(unif)
        vref = log(1+beta2*((gamdev-gamma2)**2))+gamma2*log(gamdev*gamm1)-gamdev+gamma2
#
     if vv<=0. :
        UTMESS('F','PROBA0_7')
     self.sd.valeur = a + v*(delta**2)*gamdev
