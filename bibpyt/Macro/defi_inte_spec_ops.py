#@ MODIF defi_inte_spec_ops Macro  DATE 24/05/2011   AUTEUR ZENTNER I.ZENTNER 
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

import math

def tocomplex(arg):
    if arg[0]=='RI' : return complex(arg[1], arg[2])
    if arg[0]=='MP' : return complex(arg[1]*math.cos(arg[2]), arg[1]*math.sin(arg[2]))


def defi_inte_spec_ops(self,DIMENSION,PAR_FONCTION,KANAI_TAJIMI,
                       CONSTANT,TITRE,INFO,**args):
#  ------------------------------------------------------------------
#  Définition d'une matrice interspectrale
#  à partir de fonctions complexes

   import aster
   from types import ListType, TupleType
   EnumTypes = (ListType, TupleType)
   from Accas               import _F
   from Utilitai.Utmess     import  UTMESS
   import numpy
   
   commande='DEFI_INTE_SPEC'

   ier = 0
   # La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)

   # Le concept sortant (de type table_sdaster ou dérivé) est tab
   self.DeclareOut('tabout', self.sd)
   
   # On importe les definitions des commandes a utiliser dans la macro
   # Le nom de la variable doit etre obligatoirement le nom de la commande
   CREA_TABLE    = self.get_cmd('CREA_TABLE')
   CALC_TABLE    = self.get_cmd('CALC_TABLE')
   DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')

#--- Vérifications

   if PAR_FONCTION==None : PAR_FONCTION=[]
   if KANAI_TAJIMI==None : KANAI_TAJIMI=[]
   if CONSTANT    ==None : CONSTANT    =[]

   nfntot = len(PAR_FONCTION)+len(KANAI_TAJIMI)+len(CONSTANT)
   dimh   = (DIMENSION*(DIMENSION+1))/2
   if dimh!=nfntot :
      UTMESS('F','SPECTRAL0_1')

   l_f=[]
   for occ in PAR_FONCTION : l_f.append(('PAR_FONCTION',occ))
   for occ in KANAI_TAJIMI : l_f.append(('KANAI_TAJIMI',occ))
   for occ in CONSTANT     : l_f.append(('CONSTANT'    ,occ))
   for occ in l_f :
      if occ[0]!='PAR_FONCTION' and occ[1]['FREQ_MAX']<occ[1]['FREQ_MIN'] :
          UTMESS('F','SPECTRAL0_2',valk=occ[0])
   l_is=[occ[1]['NUME_ORDRE_I'] for occ in l_f]
   l_js=[occ[1]['NUME_ORDRE_J'] for occ in l_f]
   iis=sum(l_is)
   ijs=sum(l_js)

#--- pour une matrice hermitienne ---
   l_ih=[k*(DIMENSION-k+1) for k in range(1,DIMENSION+1)]
   l_jh=[k*k               for k in range(1,DIMENSION+1)]
   ih=sum(l_ih)
   jh=sum(l_jh)
   if ((iis!=ih) or (ijs!=jh)) :
      UTMESS('F','SPECTRAL0_3')

#--- Construction de la liste de fonctions complexes
   l_fc=[]
   for occ in l_f :

#--- PAR_FONCTION

       if occ[0]=='PAR_FONCTION' :
              l_fc.append(occ[1]['FONCTION'].nom)

#--- KANAI_TAJIMI et CONSTANT

       if occ[0] in ('KANAI_TAJIMI','CONSTANT')     :
              if occ[1]['VALE_R']!=None :
                 valr=occ[1]['VALE_R']
                 vali=0.
              elif occ[1]['VALE_C']!=None :
                 cmpl=tocomplex(occ[1]['VALE_C'])
                 valr=cmpl.real
                 vali=cmpl.imag
              else :
                 valr=1.
                 vali=0.
              x1=numpy.arange(occ[1]['FREQ_MIN'],occ[1]['FREQ_MAX'],occ[1]['PAS'])
              x1=x1.tolist()+[occ[1]['FREQ_MAX'],]
              valc=[]
              for absc in x1 : valc=valc+[absc,valr,vali]

#--- KANAI_TAJIMI

       if occ[0]=='KANAI_TAJIMI'     :
              amor   = occ[1]['AMOR_REDUIT']
              frqmoy = occ[1]['FREQ_MOY']
              x11  =numpy.array([4*(amor**2)*(frqmoy**2)*FREQ**2 \
                                   for FREQ in x1 ])
              xnum =x11+frqmoy**4
              denom=numpy.array([ (frqmoy**2-FREQ**2)**2 \
                                   for FREQ in x1 ])
              denom=denom+x11
              g0=2.*math.pi*numpy.array([valr]*len(denom))
 #              g0=numpy.array([valr]*len(denom))             
              g0=g0*xnum/denom
              valc=[]
              for i in range(len(x1)): valc=valc+[x1[i],g0[i],0.]
       if occ[0] in ('KANAI_TAJIMI','CONSTANT')     :
              _f=DEFI_FONCTION(PROL_GAUCHE=occ[1]['PROL_GAUCHE'],
                               PROL_DROITE=occ[1]['PROL_DROITE'],
                               INTERPOL   =occ[1]['INTERPOL'],
                               VALE_C     =valc,
                               NOM_PARA   ='FREQ',
                               NOM_RESU   ='DSP' )
              l_fc.append(_f.nom)

   mcfact=[]
   mcfact.append(_F(PARA='NOM_CHAM'  ,LISTE_K='DSP' ))
   mcfact.append(_F(PARA='OPTION'    ,LISTE_K='TOUT' ))
   mcfact.append(_F(PARA='DIMENSION' ,LISTE_I=(DIMENSION,) ))
   tabout=CREA_TABLE(LISTE=mcfact, TITRE='',
                     TYPE_TABLE='TABLE_FONCTION')
   mcfact=[]
   mcfact.append(_F(PARA='NUME_ORDRE_I'  ,LISTE_I=l_is ))
   mcfact.append(_F(PARA='NUME_ORDRE_J'  ,LISTE_I=l_js ))
   mcfact.append(_F(PARA='FONCTION_C'    ,LISTE_K=l_fc ,TYPE_K='K24' ))
   __l_fonc=CREA_TABLE(LISTE=mcfact, TITRE='')
   tabout=CALC_TABLE(reuse=tabout,TABLE=tabout,
                     TITRE=self.sd.nom+' : interspectre obtenu par DEFI_INTE_SPEC',
                     ACTION=_F(OPERATION='COMB',TABLE=__l_fonc,))

   return ier
