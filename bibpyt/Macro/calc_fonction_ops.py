#@ MODIF calc_fonction_ops Macro  DATE 31/05/2005   AUTEUR DURAND C.DURAND 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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


def tocomplex(arg):
    if arg[0]=='RI' : return complex(arg[1],arg[2])
    if arg[0]=='MP' : return complex(arg[1]*cos(arg[2]),arg[1]*sin(arg[2]))

def calc_fonction_ops(self,FFT,DERIVE,INTEGRE,LISS_ENVELOP,
                      SPEC_OSCI,ABS,COMB,COMB_C,COMPOSE,EXTRACTION,
                      ENVELOPPE,ASSE,CORR_ACCE,PUISSANCE,INVERSE,
                      NOM_PARA,NOM_RESU,INTERPOL,PROL_DROITE,
                      PROL_GAUCHE,NOM_PARA_FONC,INTERPOL_FONC,PROL_DROITE_FONC,
                      PROL_GAUCHE_FONC,**args):
  """
     Ecriture de la macro CALC_FONCTION
  """
  ier=0
  import types
  import string
  import copy
  from math import pi
  from Utilitai.t_fonction import t_fonction,t_fonction_c,t_nappe
  from Accas import _F
  from Cata.cata import nappe_sdaster,fonction_sdaster,fonction_c
  from Utilitai.Utmess import UTMESS
  from Numeric import alltrue,less,array,reshape,cos,sin,exp,sqrt
  from Numeric import choose,zeros,Float
  import aster_fonctions
  EnumType = (types.ListType,types.TupleType)
  
  ### On importe les definitions des commandes a utiliser dans la macro
  DEFI_FONCTION  = self.get_cmd('DEFI_FONCTION')
  DEFI_NAPPE     = self.get_cmd('DEFI_NAPPE')
  
  ### Comptage commandes + déclaration concept sortant
  self.set_icmd(1)
  self.DeclareOut('C_out',self.sd)

  ### type de traitement
  ###
  if (INTEGRE     != None):
     __ff=INTEGRE['FONCTION'].convert()
     if INTEGRE['METHODE']=='TRAPEZE' : __ex=__ff.trapeze(INTEGRE['COEF'])
     if INTEGRE['METHODE']=='SIMPSON' : __ex=__ff.simpson(INTEGRE['COEF'])
  ###
  if (DERIVE      != None):
     __ff=DERIVE['FONCTION'].convert()
     __ex=__ff.derive()
  ###
  if (INVERSE     != None):
     __ff=INVERSE['FONCTION'].convert()
     __ex=__ff.inverse()
  ###
  if (ABS         != None): 
     __ff=ABS['FONCTION'].convert()
     __ex=__ff.abs()
  ###
  if (COMPOSE     != None): 
     __ff=COMPOSE['FONC_RESU'].convert()
     __fg=COMPOSE['FONC_PARA'].convert()
     __ex=__ff[__fg]
  ###
  if (ASSE        != None):
     __f0=ASSE['FONCTION'][0].convert()
     __f1=ASSE['FONCTION'][1].convert()
     __ex=__f0.cat(__f1,ASSE['SURCHARGE'])
  ###
  if (COMB        != None):
     list_fonc=[]  
     if   isinstance(self.sd,nappe_sdaster):
        for mcfact in COMB :
           list_fonc.append(mcfact['FONCTION'].convert())
        list_fonch=[]  
        for f in list_fonc :
            __ex=f
            for g in list_fonc :
               __ex=__ex.homo_support(g)
            list_fonch.append(__ex)
        list_fonc=list_fonch
     elif isinstance(self.sd,fonction_sdaster):
        for mcfact in COMB :
           __ex=mcfact['FONCTION'].convert()
           list_fonc.append(__ex)

     __ex=list_fonc[0]
     __ex=__ex*COMB[0]['COEF']
     i=1
     for item in list_fonc[1:] :
        item=item*COMB[i]['COEF']
        __ex=__ex+item
        i=i+1
  ###
  if (COMB_C    != None):
     list_fonc=[]  
     if   isinstance(self.sd,nappe_sdaster):
        for mcfact in COMB_C :
           list_fonc.append(mcfact['FONCTION'].convert())
        list_fonch=[]  
        for f in list_fonc :
            __ex=f
            for g in list_fonc :
               __ex=__ex.homo_support(g)
            list_fonch.appen(__ex)
        list_fonc=list_fonch
     elif isinstance(self.sd,fonction_sdaster) or isinstance(self.sd,fonction_c):
        for mcfact in COMB_C :
           __ex=mcfact['FONCTION'].convert(arg='complex')
           list_fonc.append(__ex)

     __ex=list_fonc[0]
     if COMB_C[0]['COEF_R']!=None: __ex=__ex*complex(COMB_C[0]['COEF_R'])
     if COMB_C[0]['COEF_C']!=None: __ex=__ex*tocomplex(COMB_C[0]['COEF_C'])
     i=1
     for item in list_fonc[1:] :
        if COMB_C[i]['COEF_R']!=None: coef=complex(COMB_C[i]['COEF_R'])
        if COMB_C[i]['COEF_C']!=None: coef=tocomplex(COMB_C[i]['COEF_C'])
        item=item*coef
        __ex=__ex+item
        i=i+1
  ###
  if (PUISSANCE   != None): 
     __ff=PUISSANCE['FONCTION'].convert()
     __ex=__ff
     for i in range(PUISSANCE['EXPOSANT']-1) : __ex=__ex*__ff
  ###
  if (EXTRACTION  != None):
     if EXTRACTION['PARTIE']=='REEL'   : __ex=EXTRACTION['FONCTION'].convert(arg='real')
     if EXTRACTION['PARTIE']=='IMAG'   : __ex=EXTRACTION['FONCTION'].convert(arg='imag')
     if EXTRACTION['PARTIE']=='MODULE' : __ex=EXTRACTION['FONCTION'].convert(arg='modul')
     if EXTRACTION['PARTIE']=='PHASE'  : __ex=EXTRACTION['FONCTION'].convert(arg='phase')
  ###
  if (ENVELOPPE   != None):
     list_fonc=[]
     l_env=ENVELOPPE['FONCTION']
     if type(l_env) not in EnumType : l_env=(l_env,)
     if isinstance(self.sd,nappe_sdaster):
        for f in l_env : list_fonc.append(f.convert())
        list_fonch=[]  
        for f in list_fonc :
            __ff=f
            for g in list_fonc :
               __ff=__ff.homo_support(g)
            list_fonch.append(__ff)
        list_fonc=list_fonch
        vale_para=list_fonc[0].vale_para
        para     =list_fonc[0].para
        l_fonc_f =[]
        for i in range(len(vale_para)):
            __ff=list_fonc[0].l_fonc[i]
            if ENVELOPPE['CRITERE']=='SUP' :
              for f in list_fonc[1:] : __ff=__ff.sup(f.l_fonc[i])
            if ENVELOPPE['CRITERE']=='INF' :
              for f in list_fonc[1:] : __ff=__ff.inf(f.l_fonc[i])
            l_fonc_f.append(__ff)
        __ex=t_nappe(vale_para,l_fonc_f,para)
     elif isinstance(self.sd,fonction_sdaster):
        for f in l_env : list_fonc.append(f.convert())
        __ex=list_fonc[0]
        if ENVELOPPE['CRITERE']=='SUP' :
           for f in list_fonc[1:] : __ex=__ex.sup(f)
        if ENVELOPPE['CRITERE']=='INF' :
           for f in list_fonc[1:] : __ex=__ex.inf(f)
  ###
  if (CORR_ACCE   != None):
     __ex=CORR_ACCE['FONCTION'].convert()
     para=copy.copy(__ex.para)
     # suppression de la tendance de l accelero
     __ex=__ex.suppr_tend()
     # calcul de la vitesse
     __ex=__ex.trapeze(0.)
     # calcul de la tendance de la vitesse : y = a1*x +a0
     __ex=__ex.suppr_tend()
     if CORR_ACCE['CORR_DEPL']=='OUI':
        # suppression de la tendance deplacement
        # calcul du deplacement : integration
        __ex=__ex.trapeze(0.)
        # calcul de la tendance du déplacement : y = a1*x +a0
        __ex=__ex.suppr_tend()
        # regeneration de la vitesse : derivation
        __ex=__ex.derive()
     # regeneration de l accelero : derivation
     __ex=__ex.derive()
     __ex.para=para
  ###
  if (FFT         != None):
     if isinstance(self.sd,fonction_c):
        __ff=FFT['FONCTION'].convert()
        __ex=__ff.fft(FFT['METHODE'])
     if isinstance(self.sd,fonction_sdaster):
        __ff=FFT['FONCTION'].convert(arg='complex')
        __ex=__ff.fft(FFT['METHODE'],FFT['SYME'])
  ###
  if (SPEC_OSCI   != None):
     if SPEC_OSCI['AMOR_REDUIT']==None :
        l_amor=[0.02,0.05,0.1]
        UTMESS('I','CALC_FONCTION',' : génération par défaut de 3 amortissements :'+str(l_amor))
     else :
        if type(SPEC_OSCI['AMOR_REDUIT']) not in EnumType :
               l_amor=[SPEC_OSCI['AMOR_REDUIT'],]
        else : l_amor= SPEC_OSCI['AMOR_REDUIT']
     if SPEC_OSCI['FREQ']==None and SPEC_OSCI['LIST_FREQ']==None:
        l_freq=[]
        for i in range(56) : l_freq.append( 0.2+0.050*i)
        for i in range( 8) : l_freq.append( 3.0+0.075*i)
        for i in range(14) : l_freq.append( 3.6+0.100*i)
        for i in range(24) : l_freq.append( 5.0+0.125*i)
        for i in range(28) : l_freq.append( 8.0+0.250*i)
        for i in range( 6) : l_freq.append(15.0+0.500*i)
        for i in range( 4) : l_freq.append(18.0+1.000*i)
        for i in range(10) : l_freq.append(22.0+1.500*i)
        texte=[]
        for i in range(len(l_freq)/5) :
            texte.append(' %f %f %f %f %f' %tuple(l_freq[i*5:i*5+5]))
        UTMESS('I','CALC_FONCTION',' : génération par défaut de 150 fréquences :\n'+'\n'.join(texte))
     elif SPEC_OSCI['LIST_FREQ']!=None:
        l_freq=SPEC_OSCI['LIST_FREQ'].Valeurs()
     elif SPEC_OSCI['FREQ']!=None:
        if type(SPEC_OSCI['FREQ']) not in EnumType:
               l_freq=[SPEC_OSCI['FREQ'],]
        else : l_freq= SPEC_OSCI['FREQ']
     if abs(SPEC_OSCI['NORME'])<1.E-10 :
        UTMESS('S','CALC_FONCTION',' : SPEC_OSCI, la norme ne peut etre nulle')
     if SPEC_OSCI['NATURE_FONC']!='ACCE' :
        UTMESS('S','CALC_FONCTION',' : SPEC_OSCI, le type de la fonction doit etre ACCE')
     if SPEC_OSCI['METHODE']!='NIGAM' :
        UTMESS('S','CALC_FONCTION',' : SPEC_OSCI, seule la méthode NIGAM est codée')
     eps=1.e-6
     for amor in l_amor :
         if amor>(1-eps) :
            UTMESS('S','CALC_FONCTION',' : SPEC_OSCI, la méthode choisie '\
                   'suppose des amortissements sous-critiques, amor<1.')

     __ff=SPEC_OSCI['FONCTION'].convert()
     
     # appel à SPEC_OSCI
     spectr = aster_fonctions.SPEC_OSCI(__ff.vale_x, __ff.vale_y, l_freq, l_amor)

     # construction de la nappe
     vale_para = l_amor
     para      = { 'INTERPOL'      : ['LIN','LOG'],
                   'NOM_PARA_FONC' : 'FREQ',
                   'NOM_PARA'      : 'AMOR',
                   'PROL_DROITE'   : 'EXCLU',
                   'PROL_GAUCHE'   : 'EXCLU',
                   'NOM_RESU'      : SPEC_OSCI['NATURE'] }
     para_fonc = { 'INTERPOL'      : ['LOG','LOG'],
                   'NOM_PARA'      : 'FREQ',
                   'PROL_DROITE'   : 'CONSTANT',
                   'PROL_GAUCHE'   : 'EXCLU',
                   'NOM_RESU'      : SPEC_OSCI['NATURE'] }
     if   SPEC_OSCI['NATURE']=='DEPL' : ideb = 0
     elif SPEC_OSCI['NATURE']=='VITE' : ideb = 1
     else                             : ideb = 2
     l_fonc = []
     for iamor in range(len(l_amor)) :
       l_fonc.append(t_fonction(l_freq,spectr[iamor,ideb,:]/SPEC_OSCI['NORME'],para_fonc))
     __ex=t_nappe(vale_para,l_fonc,para)
  ###
  if (LISS_ENVELOP!= None): return

  ### creation de la fonction produite par appel à DEFI_FONCTION
  ### on récupère les paramètres issus du calcul de __ex
  ### et on les surcharge par ceux imposés par l'utilisateur

  if isinstance(__ex,t_fonction) or isinstance(__ex,t_fonction_c):
     para=__ex.para
     if NOM_PARA   !=None : para['NOM_PARA']   =NOM_PARA
     if NOM_RESU   !=None : para['NOM_RESU']   =NOM_RESU
     if PROL_DROITE!=None : para['PROL_DROITE']=PROL_DROITE
     if PROL_GAUCHE!=None : para['PROL_GAUCHE']=PROL_GAUCHE
     if INTERPOL   !=None : para['INTERPOL']   =INTERPOL
     if   isinstance(__ex,t_fonction_c): para['VALE_C'] = __ex.tabul()
     elif isinstance(__ex,t_fonction)  : para['VALE']   = __ex.tabul()
     C_out=DEFI_FONCTION(**para)
  elif isinstance(__ex,t_nappe):
     def_fonc=[]
     for f in __ex.l_fonc :
       para=f.para
       def_fonc.append(_F(VALE       =f.tabul(),
                          INTERPOL   =f.para['INTERPOL'],
                          PROL_DROITE=f.para['PROL_DROITE'],
                          PROL_GAUCHE=f.para['PROL_GAUCHE'],)
                       )
     para=__ex.para
     if NOM_PARA      !=None : para['NOM_PARA']   =NOM_PARA
     if NOM_RESU      !=None : para['NOM_RESU']   =NOM_RESU
     if PROL_DROITE   !=None : para['PROL_DROITE']=PROL_DROITE
     if PROL_GAUCHE   !=None : para['PROL_GAUCHE']=PROL_GAUCHE
     if NOM_PARA_FONC !=None : para['NOM_PARA_FONC']   =INTERPOL
     if INTERPOL_FONC !=None : para['INTERPOL']   =INTERPOL
     C_out=DEFI_NAPPE(PARA=__ex.vale_para.tolist(),DEFI_FONCTION=def_fonc,**para)
  return ier

