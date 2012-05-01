#@ MODIF calc_fonction_ops Macro  DATE 30/04/2012   AUTEUR COURTOIS M.COURTOIS 
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
# RESPONSABLE COURTOIS M.COURTOIS

import os
import copy
import traceback
import math

def calc_fonction_ops(self,FFT,DERIVE,INTEGRE,LISS_ENVELOP,
                      SPEC_OSCI,ABS,COMB,COMB_C,COMPOSE,EXTRACTION,
                      ENVELOPPE,FRACTILE,ASSE,CORR_ACCE,PUISSANCE,INVERSE,
                      REGR_POLYNOMIALE,DSP,
                      NOM_PARA,NOM_RESU,INTERPOL,PROL_DROITE,
                      PROL_GAUCHE,NOM_PARA_FONC,INTERPOL_FONC,PROL_DROITE_FONC,
                      PROL_GAUCHE_FONC,INFO,**args):
   """
      Ecriture de la macro CALC_FONCTION
   """
   ier=0
   import numpy as NP

   from Cata_Utils.t_fonction import t_fonction, t_fonction_c, t_nappe, homo_support_nappe, \
            FonctionError, ParametreError, InterpolationError, ProlongementError, enveloppe, fractile
   from Utilitai import liss_enveloppe
   from Macro.defi_inte_spec_ops import tocomplex
   from Accas import _F
   from Cata.cata import nappe_sdaster,fonction_sdaster,fonction_c
   from Utilitai.Utmess import  UTMESS
   import aster_fonctions
   EnumTypes = (list, tuple)

   ### On importe les definitions des commandes a utiliser dans la macro
   DEFI_FONCTION  = self.get_cmd('DEFI_FONCTION')
   IMPR_FONCTION  = self.get_cmd('IMPR_FONCTION')
   DEFI_NAPPE     = self.get_cmd('DEFI_NAPPE')

   ### Comptage commandes + déclaration concept sortant
   self.set_icmd(1)
   self.DeclareOut('C_out',self.sd)

   # éléments de contexte
   ctxt = Context()
   ### l'ensemble est dans un try/except pour recuperer les erreurs du module t_fonction
   try:
      ###
      if (INTEGRE     != None):
         __ff=INTEGRE['FONCTION'].convert()
         ctxt.f = __ff.nom
         if INTEGRE['METHODE']=='TRAPEZE' :
            __ex=__ff.trapeze(INTEGRE['COEF'])
         elif INTEGRE['METHODE']=='SIMPSON' :
            __ex=__ff.simpson(INTEGRE['COEF'])
      ###
      if (DERIVE      != None):
         __ff=DERIVE['FONCTION'].convert()
         ctxt.f = __ff.nom
         __ex=__ff.derive()
      ###
      if (INVERSE     != None):
         __ff=INVERSE['FONCTION'].convert()
         ctxt.f = __ff.nom
         __ex=__ff.inverse()
      ###
      if (ABS         != None):
         __ff=ABS['FONCTION'].convert()
         ctxt.f = __ff.nom
         __ex=__ff.abs()
      ###
      if (COMPOSE     != None):
         __ff=COMPOSE['FONC_RESU'].convert()
         __fg=COMPOSE['FONC_PARA'].convert()
         ctxt.f = [__ff.nom, __fg.nom]
         __ex=__ff[__fg]
      ###
      if (ASSE        != None):
         __f0=ASSE['FONCTION'][0].convert()
         __f1=ASSE['FONCTION'][1].convert()
         ctxt.f = [__f0.nom, __f1.nom]
         __ex=__f0.cat(__f1,ASSE['SURCHARGE'])
      ###
      if (COMB != None):
         list_fonc=[]
         if isinstance(self.sd,nappe_sdaster):
            for mcfact in COMB :
               list_fonc.append(mcfact['FONCTION'].convert())
            ctxt.f = [f.nom for f in list_fonc]
            list_fonc = homo_support_nappe(list_fonc)
         elif isinstance(self.sd,fonction_sdaster):
            for mcfact in COMB :
               __ex=mcfact['FONCTION'].convert()
               list_fonc.append(__ex)

         __ex = 0.
         for item, comb in zip(list_fonc, COMB):
            ctxt.f = item.nom
            __ex = item * comb['COEF'] + __ex
         # on prend les paramètres de la 1ère fonction
         __ex.para = copy.copy(list_fonc[0].para)
      ###
      if (COMB_C != None):
         list_fonc=[]
         if isinstance(self.sd,nappe_sdaster):
            for mcfact in COMB_C:
               list_fonc.append(mcfact['FONCTION'].convert())
            ctxt.f = [f.nom for f in list_fonc]
            list_fonc = homo_support_nappe(list_fonc)
         elif isinstance(self.sd,fonction_sdaster) or isinstance(self.sd,fonction_c):
            for mcfact in COMB_C :
               __ex=mcfact['FONCTION'].convert(arg='complex')
               list_fonc.append(__ex)

         __ex = 0.
         for item, comb in zip(list_fonc, COMB_C):
            if comb['COEF_R'] != None:
               coef = complex(comb['COEF_R'])
            elif comb['COEF_C'] != None:
               if type(comb['COEF_C']) in EnumTypes:
                  coef = tocomplex(comb['COEF_C'])
               else:
                  coef = comb['COEF_C']
            ctxt.f = item.nom
            __ex = item * coef + __ex
         # on prend les paramètres de la 1ère fonction
         __ex.para = copy.copy(list_fonc[0].para)

      ### mot clé LIST_PARA uniquement présent si COMB ou COMB_C
      if (COMB != None) or (COMB_C != None) :
         if (args['LIST_PARA'] != None) :
            __ex=__ex.evalfonc(args['LIST_PARA'].Valeurs())
      ###
      if (PUISSANCE   != None):
         __ff=PUISSANCE['FONCTION'].convert()
         ctxt.f = __ff.nom
         __ex=__ff
         for i in range(PUISSANCE['EXPOSANT']-1):
            __ex=__ex*__ff
      ###
      if (EXTRACTION  != None):
         if EXTRACTION['PARTIE']=='REEL':
            __ex=EXTRACTION['FONCTION'].convert(arg='real')
         if EXTRACTION['PARTIE']=='IMAG':
            __ex=EXTRACTION['FONCTION'].convert(arg='imag')
         if EXTRACTION['PARTIE']=='MODULE':
            __ex=EXTRACTION['FONCTION'].convert(arg='modul')
         if EXTRACTION['PARTIE']=='PHASE':
            __ex=EXTRACTION['FONCTION'].convert(arg='phase')
      ###
      if (ENVELOPPE   != None):
         list_fonc=[]
         l_env=ENVELOPPE['FONCTION']
         if type(l_env) not in EnumTypes:
            l_env=(l_env,)
         if isinstance(self.sd,nappe_sdaster):
            for f in l_env:
               list_fonc.append(f.convert())
            ctxt.f = [f.nom for f in list_fonc]
            list_fonc = homo_support_nappe(list_fonc)
            vale_para=list_fonc[0].vale_para
            para     =list_fonc[0].para
            l_fonc_f =[]
            for i in range(len(vale_para)):
               __ff=list_fonc[0].l_fonc[i]
               for nap in list_fonc[1:] :
                  ctxt.f = nap.l_fonc[i].nom
                  __ff=enveloppe([__ff,nap.l_fonc[i]], ENVELOPPE['CRITERE'])
               l_fonc_f.append(__ff)
            __ex=t_nappe(vale_para,l_fonc_f,para)
         elif isinstance(self.sd,fonction_sdaster):
            for f in l_env:
               list_fonc.append(f.convert())
            ctxt.f = [f.nom for f in list_fonc]
            __ex = enveloppe(list_fonc, ENVELOPPE['CRITERE'])
      ###
      if (FRACTILE   != None):
         list_fonc=[]
         l_frac=FRACTILE['FONCTION']
         if type(l_frac) not in EnumTypes:
            l_frac=(l_frac,)
         if isinstance(self.sd,nappe_sdaster):
            for f in l_frac:
               list_fonc.append(f.convert())
            ctxt.f = [f.nom for f in list_fonc]
            list_fonc = homo_support_nappe(list_fonc)
            vale_para=list_fonc[0].vale_para
            para     =list_fonc[0].para
            l_fonc_f =[]
            for i in range(len(vale_para)):
               ctxt.f = [nap.l_fonc[i].nom for nap in list_fonc]
               __ff=fractile([nap.l_fonc[i] for nap in list_fonc], FRACTILE['FRACT'])
               l_fonc_f.append(__ff)
            __ex=t_nappe(vale_para,l_fonc_f,para)
         elif isinstance(self.sd,fonction_sdaster):
            for f in l_frac:
               list_fonc.append(f.convert())
            __ex=list_fonc[0]
            for f in list_fonc[1:]:
               ctxt.f = [__ex.nom, f.nom]
               __ex = fractile(list_fonc, FRACTILE['FRACT'])
      ###
      if (CORR_ACCE   != None):
         __ex=CORR_ACCE['FONCTION'].convert()
         ctxt.f = __ex.nom
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
            ctxt.f = __ff.nom
            __ex=__ff.fft(FFT['METHODE'])
         if isinstance(self.sd,fonction_sdaster):
            __ff=FFT['FONCTION'].convert(arg='complex')
            ctxt.f = __ff.nom
            __ex=__ff.fft(FFT['METHODE'],FFT['SYME'])
      ###
      if (SPEC_OSCI   != None):
         if SPEC_OSCI['AMOR_REDUIT']==None:
            l_amor=[0.02, 0.05, 0.1]
            UTMESS('I','FONCT0_31',valr=l_amor)
         else:
            if type(SPEC_OSCI['AMOR_REDUIT']) not in EnumTypes :
               l_amor=[SPEC_OSCI['AMOR_REDUIT'],]
            else:
               l_amor= SPEC_OSCI['AMOR_REDUIT']
         if SPEC_OSCI['FREQ']==None and SPEC_OSCI['LIST_FREQ']==None:
            l_freq=[]
            for i in range(56):
               l_freq.append( 0.2+0.050*i)
            for i in range( 8):
               l_freq.append( 3.0+0.075*i)
            for i in range(14):
               l_freq.append( 3.6+0.100*i)
            for i in range(24):
               l_freq.append( 5.0+0.125*i)
            for i in range(28):
               l_freq.append( 8.0+0.250*i)
            for i in range( 6):
               l_freq.append(15.0+0.500*i)
            for i in range( 4):
               l_freq.append(18.0+1.000*i)
            for i in range(10):
               l_freq.append(22.0+1.500*i)
            texte=[]
            for i in range(len(l_freq)/5) :
               texte.append(' %f %f %f %f %f' %tuple(l_freq[i*5:i*5+5]))
            UTMESS('I','FONCT0_32',valk=os.linesep.join(texte))
         elif SPEC_OSCI['LIST_FREQ']!=None:
            l_freq=SPEC_OSCI['LIST_FREQ'].Valeurs()
         elif SPEC_OSCI['FREQ']!=None:
            if type(SPEC_OSCI['FREQ']) not in EnumTypes:
               l_freq=[SPEC_OSCI['FREQ'],]
            else:
               l_freq= SPEC_OSCI['FREQ']
         if min(l_freq)<1.E-10 :
            UTMESS('S','FONCT0_43')
         if abs(SPEC_OSCI['NORME'])<1.E-10 :
            UTMESS('S','FONCT0_33')
         if SPEC_OSCI['NATURE_FONC']!='ACCE' :
            UTMESS('S','FONCT0_34')
         if SPEC_OSCI['METHODE']!='NIGAM' :
            UTMESS('S','FONCT0_35')
         eps=1.e-6
         for amor in l_amor :
            if amor>(1-eps) :
              UTMESS('S','FONCT0_36')
         __ff=SPEC_OSCI['FONCTION'].convert()
         ctxt.f = __ff.nom

         # appel à SPEC_OSCI
         spectr = aster_fonctions.SPEC_OSCI(__ff.vale_x, __ff.vale_y, l_freq, l_amor)

         # construction de la nappe
         vale_para = l_amor
         para      = { 'INTERPOL'      : ['LIN', 'LOG'],
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
         if   SPEC_OSCI['NATURE']=='DEPL':
            ideb = 0
         elif SPEC_OSCI['NATURE']=='VITE':
            ideb = 1
         else:
            ideb = 2
         l_fonc = []
         for iamor in range(len(l_amor)) :
            l_fonc.append(t_fonction(l_freq,spectr[iamor,ideb,:]/SPEC_OSCI['NORME'],para_fonc))
         __ex=t_nappe(vale_para,l_fonc,para)
      ###
      if (DSP != None):
        deuxpi = 2. * math.pi
        __ff = DSP['FONCTION'].convert()
        wmin = 1.001
        wcoup = deuxpi * DSP['FREQ_COUP']
        duree = DSP['DUREE']
        ksi = DSP['AMOR_REDUIT']
        pesanteur = DSP['NORME']
        fract = DSP['FRACT']
        if DSP['LIST_FREQ'] != None:
            l_freq = DSP['LIST_FREQ'].Valeurs()
        elif DSP['FREQ'] != None:
            l_freq = DSP['FREQ']
        else:
            l_freq = __ff.vale_x
        sro = __ff.evalfonc(l_freq) * pesanteur
        ctxt.f = sro.nom
        assert 0. < fract < 1., 'invalid value for FRACT'
        assert 0. < ksi < 1., 'invalid value for AMOR_REDUIT'
        def coefn(wn, T, p):
            vo = wn / (2. * math.pi)
            return vo * T / ( -math.log(p) )
        def peak2(p, T, wn, ksi):
            delta = math.sqrt(4. * ksi / math.pi)
            deuxn = 2. * coefn(wn, T, p)
            sexp = - math.pow(delta, 1.2) * math.sqrt(math.pi * math.log(deuxn))
            return 2. * math.log( deuxn * ( 1. - math.exp(sexp)) )

        valw = sro.vale_x * deuxpi
 #       if max(valw) > wmin:
 #           pass
        nbfreq = len(valw)
        valg = NP.zeros(nbfreq)
        sumg = 0.        
        ZPA = __ff.vale_y[-1]
        for n in range(nbfreq):
            wn = valw[n]
            if wn <= wmin:
                valg[n] = 0.0
            else:
                valsro = sro.vale_y[n]
                if wn > wcoup:
                    valsro = ZPA
                npi2 = peak2(fract, duree, wn, ksi)
                v1 = 1./(wn*(math.pi/(2.*ksi)-2.))
                v2 = (valsro**2)/npi2;
                Gw = t_fonction(valw, valg, para=__ff.para)
                v3 = 2. * Gw.trapeze(0.0)(wn)
                valg[n] = max([v1*(v2-v3), 0.])
        valf = valw / deuxpi
        __ex = t_fonction(valf, valg * deuxpi, para=__ff.para)
      ###
      if (LISS_ENVELOP!= None):
         __ff=LISS_ENVELOP['NAPPE'].convert()
         ctxt.f = __ff.nom
         sp_nappe=liss_enveloppe.nappe(listFreq=__ff.l_fonc[0].vale_x, listeTable=[f.vale_y for f in __ff.l_fonc], listAmor=__ff.vale_para, entete="")
         sp_lisse=liss_enveloppe.lissage(nappe=sp_nappe,fmin=LISS_ENVELOP['FREQ_MIN'],fmax=LISS_ENVELOP['FREQ_MAX'],elarg=LISS_ENVELOP['ELARG'],tole_liss=LISS_ENVELOP['TOLE_LISS'])
         para_fonc=__ff.l_fonc[0].para
         l_fonc=[]
         for val in sp_lisse.listTable:
            l_fonc.append(t_fonction(sp_lisse.listFreq,val,para_fonc))
         __ex=t_nappe(vale_para=sp_lisse.listAmor,l_fonc=l_fonc,para=__ff.para)
      ###
      if (REGR_POLYNOMIALE != None):
          __ff = REGR_POLYNOMIALE['FONCTION'].convert()
          ctxt.f = __ff.nom
          deg = REGR_POLYNOMIALE['DEGRE']
          coef = NP.polyfit(__ff.vale_x, __ff.vale_y, deg)
          if coef is None:
              raise FonctionError("La régression polynomiale n'a pas convergé.")
          # interpolation sur une liste d'abscisses
          absc = __ff.vale_x
          if args['LIST_PARA'] is not None:
            absc = args['LIST_PARA'].Valeurs()
          vale = NP.polyval(coef, absc)
          # paramètres
          para = __ff.para.copy()
          para['INTERPOL'] = ['LIN', 'LIN']
          __ex = t_fonction(absc, vale, para)
          coef_as_str = os.linesep.join(['   a[%d] = %f' % (i, ci) \
                                         for i, ci in enumerate(coef)])
          UTMESS('I', 'FONCT0_57', coef_as_str)

   except InterpolationError, msg:
      UTMESS('F', 'FONCT0_27', valk=(ctxt.f, str(msg)))
   except ParametreError, msg:
      UTMESS('F', 'FONCT0_28', valk=(ctxt.f, str(msg)))
   except ProlongementError, msg:
      UTMESS('F', 'FONCT0_29', valk=(ctxt.f, str(msg)))
   except FonctionError, msg:
      UTMESS('F', 'FONCT0_30', valk=(ctxt.f, str(msg), traceback.format_exc()))

   ### creation de la fonction produite par appel à DEFI_FONCTION
   ### on récupère les paramètres issus du calcul de __ex
   ### et on les surcharge par ceux imposés par l'utilisateur

   if isinstance(__ex,t_fonction) or isinstance(__ex,t_fonction_c):
      para=__ex.para
      if NOM_PARA    != None: para['NOM_PARA']    = NOM_PARA
      if NOM_RESU    != None: para['NOM_RESU']    = NOM_RESU
      if PROL_DROITE != None: para['PROL_DROITE'] = PROL_DROITE
      if PROL_GAUCHE != None: para['PROL_GAUCHE'] = PROL_GAUCHE
      if INTERPOL    != None: para['INTERPOL']    = INTERPOL
      if   isinstance(__ex,t_fonction_c): para['VALE_C'] = __ex.tabul()
      elif isinstance(__ex,t_fonction)  : para['VALE']   = __ex.tabul()
      C_out=DEFI_FONCTION(**para)
   elif isinstance(__ex,t_nappe):
      def_fonc=[]
      for f in __ex.l_fonc :
         para=f.para
         def_fonc.append(_F(VALE       =f.tabul(),
                            INTERPOL   =INTERPOL_FONC or f.para['INTERPOL'],
                            PROL_DROITE=PROL_DROITE_FONC or f.para['PROL_DROITE'],
                            PROL_GAUCHE=PROL_GAUCHE_FONC or f.para['PROL_GAUCHE'],))
      para=__ex.para
      if NOM_PARA      != None: para['NOM_PARA']      = NOM_PARA
      if NOM_RESU      != None: para['NOM_RESU']      = NOM_RESU
      if PROL_DROITE   != None: para['PROL_DROITE']   = PROL_DROITE
      if PROL_GAUCHE   != None: para['PROL_GAUCHE']   = PROL_GAUCHE
      if NOM_PARA_FONC != None: para['NOM_PARA_FONC'] = NOM_PARA_FONC
      if INTERPOL      != None: para['INTERPOL']      = INTERPOL
      C_out=DEFI_NAPPE(PARA=__ex.vale_para.tolist(),
                       DEFI_FONCTION=def_fonc,
                       **para)
   if INFO > 1:
      IMPR_FONCTION(FORMAT='TABLEAU',
                    UNITE=6,
                    COURBE=_F(FONCTION=C_out),)
   return ier



class Context(object):
   """Permet de stocker des éléments de contexte pour aider au
   diagnostic lors de l'émission de message.
   usage :
      context = Context()
      context.f = 'nomfon'
      print context.f
   """
   def __init__(self):
      self.__nomf = None

   def get_val(self):
      """Retourne le texte formatté.
      """
      nomf = self.__nomf
      if type(nomf) not in (list, tuple):
         nomf = [nomf,]
      pluriel = ''
      if len(nomf) > 1:
         pluriel = 's'
      try:
         res = """Fonction%(s)s concernée%(s)s : %(nomf)s""" % {
            's'    : pluriel,
            'nomf' : ', '.join(nomf),
         }
      except:
         res = 'erreur de programmation !'
      return res

   def set_val(self, value):
      self.__nomf = value

   def del_val(self):
      del self.__nomf

   f = property(get_val, set_val, del_val, "")
