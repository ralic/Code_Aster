#@ MODIF t_fonction Cata_Utils  DATE 26/05/2010   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
import copy
import types
from math import exp, log

import numpy as NP
import numpy.fft as FFT

from Noyau.N_types import is_float, is_float_or_int, is_complex, is_number, is_enum


# -----------------------------------------------------------------------------
class FonctionError(Exception): pass

class ParametreError(FonctionError):      pass  # probleme de NOM_PARA
class InterpolationError(FonctionError):  pass
class ProlongementError(FonctionError):   pass

# -----------------------------------------------------------------------------
def interp(typ_i,val,x1,x2,y1,y2,tol=1.e-6) :
  """Interpolation linéaire/logarithmique entre un couple de valeurs
  """
  if typ_i==['LIN','LIN']: return y1+(y2-y1)*(val-x1)/(x2-x1)
  if typ_i==['LIN','LOG']: return exp(log(y1)+(val-x1)*(log(y2)-log(y1))/(x2-x1))
  if typ_i==['LOG','LOG']: return exp(log(y1)+(log(val)-log(x1))*(log(y2)-log(y1))/(log(x2)-log(x1)))
  if typ_i==['LOG','LIN']: return y1+(log(val)-log(x1))*(y2-y1)/(log(x2)-log(x1))
  if typ_i[0]=='NON'     :
      if   abs(val-x1) < tol : return y1
      elif abs(val-x2) < tol : return y2
      else:
         raise InterpolationError, "abscisse = %g, intervalle = [%g, %g]" % (val, x1, x2)


def is_ordo(liste):
  if len(liste) > 1:
    val = NP.array(liste, float)
    return min(val[1:len(val)] - val[0:len(val)-1]) >= 0.
  else: return True

# -----------------------------------------------------------------------------
class t_fonction :
  """Classe pour fonctions réelles, équivalent au type aster = fonction_sdaster
  """
  def __init__(self,vale_x,vale_y,para,nom='') :
    """Création d'un objet fonction
    - vale_x et vale_y sont des listes de réels de meme longueur
    - para est un dictionnaire contenant les entrées PROL_DROITE, PROL_GAUCHE et INTERPOL (cf sd ASTER)
    """
    self.nom=nom
    pk=para.keys()
    pk.sort()
    if pk!=['INTERPOL','NOM_PARA','NOM_RESU','PROL_DROITE','PROL_GAUCHE'] :
         raise FonctionError, 'fonction : parametres incorrects'
    if para['INTERPOL'] not in [['NON','NON'],['LIN','LIN'],['LIN','LOG'],['LOG','LOG'],['LOG','LIN'],] :
         raise FonctionError, 'fonction : parametre INTERPOL incorrect'
    if para['PROL_DROITE'] not in ['EXCLU','CONSTANT','LINEAIRE'] :
         raise FonctionError, 'fonction : parametre PROL_DROITE incorrect'
    if para['PROL_GAUCHE'] not in ['EXCLU','CONSTANT','LINEAIRE'] :
         raise FonctionError, 'fonction : parametre PROL_GAUCHE incorrect'
    self.vale_x    = NP.array(vale_x)
    self.vale_y    = NP.array(vale_y)
    self.para      = para
    if len(self.vale_x)!=len(self.vale_y) :
         raise FonctionError, 'fonction : longueur abscisse <> longueur ordonnées'
    if not is_ordo(self.vale_x) :
         raise FonctionError, 'fonction : abscisses non strictement croissantes'

  def __add__(self,other) :
    """addition avec une autre fonction ou un nombre, par surcharge de l'opérateur +
    """
    if   isinstance(other,t_fonction):
      para=copy.copy(self.para)
      vale_x,para['PROL_GAUCHE'],para['PROL_DROITE']=self.homo_support(other)
      fff=self.evalfonc(vale_x)
      ggg=other.evalfonc(vale_x)
      if   isinstance(self,t_fonction_c): return t_fonction_c(vale_x,fff.vale_y+ggg.vale_y,para)
      else                              : return t_fonction(vale_x,fff.vale_y+ggg.vale_y,para)
    elif is_number(other):
      if   isinstance(self,t_fonction_c): return t_fonction_c(self.vale_x,self.vale_y+other,self.para)
      else                              : return t_fonction(self.vale_x,self.vale_y+other,self.para)
    else:  raise FonctionError, 'fonctions : erreur de type dans __add__ : %s %s' % (self, type(other))

  def __mul__(self,other) :
    """multiplication avec une autre fonction ou un nombre, par surcharge de l'opérateur *
    """
    if   isinstance(other,t_fonction):
      para=copy.copy(self.para)
      vale_x,para['PROL_GAUCHE'],para['PROL_DROITE']=self.homo_support(other)
      fff=self.evalfonc(vale_x)
      ggg=other.evalfonc(vale_x)
      if   isinstance(self,t_fonction_c): return t_fonction_c(vale_x,fff.vale_y*ggg.vale_y,para)
      else                              : return t_fonction(vale_x,fff.vale_y*ggg.vale_y,para)
    elif is_float_or_int(other):
      return t_fonction(self.vale_x,self.vale_y*other,self.para)
    elif is_complex(other):
      return t_fonction_c(self.vale_x,self.vale_y*other,self.para)
    else:  raise FonctionError, 'fonctions : erreur de type dans __mul__%s %s' % (self, type(other))

  def __repr__(self) :
    """affichage de la fonction en double colonne
    """
    texte=[]
    for i in range(len(self.vale_x)) :
      texte.append('%f %f' % (self.vale_x[i],self.vale_y[i]))
    return '\n'.join(texte)

  def __getitem__(self,other) :
    """composition de deux fonction F[G]=FoG=F(G(x))
    """
    para=copy.copy(self.para)
    if other.para['NOM_RESU'] != self.para['NOM_PARA'] :
       raise ParametreError,'''composition de fonctions : NOM_RESU1 et NOM_PARA2 incohérents '''
    para['NOM_PARA']==other.para['NOM_PARA']
    return t_fonction(other.vale_x,map(self,other.vale_y),para)

  def __call__(self,val,tol=1.e-6):
    """méthode pour évaluer f(x)
    - tolérance, par défaut 1.e-6 en relatif sur la longueur de l'intervalle
    - adjacent, pour capter les erreurs d'arrondi en cas de prolongement exclu
    """
    i=NP.searchsorted(self.vale_x,val)
    n=len(self.vale_x)
    if n == 1:
      # Utilisation abusive de la tolérance relative mais ce cas est particulier
      if (val-self.vale_x[0]) < tol:
         return self.vale_y[0]
      elif val-self.vale_x[0] > 0:
         if (self.para['PROL_DROITE']=='CONSTANT') or (self.para['PROL_DROITE']=='LINEAIRE'):
            return self.vale_y[0]
         else: raise ProlongementError, 'fonction évaluée hors du domaine de définition'
      elif val-self.vale_x[0] < 0:
         if (self.para['PROL_GAUCHE']=='CONSTANT') or (self.para['PROL_GAUCHE']=='LINEAIRE'):
            return self.vale_y[0]
         else: raise ProlongementError, 'fonction évaluée hors du domaine de définition'
    
    if i==0 :
      if self.para['PROL_GAUCHE']=='EXCLU'    :
         eps_g=(val-self.vale_x[0] )/(self.vale_x[1] -self.vale_x[0])
         if abs(eps_g)<=tol  : return self.vale_y[0]
         else                : raise ProlongementError, 'fonction évaluée hors du domaine de définition'
      else  : 
         if self.para['PROL_GAUCHE']=='CONSTANT' : return self.vale_y[0]
         if self.para['PROL_GAUCHE']=='LINEAIRE' : return interp(self.para['INTERPOL'],val,self.vale_x[0],
                                                                                           self.vale_x[1],
                                                                                           self.vale_y[0],
                                                                                           self.vale_y[1])
    elif i==n :
      if self.para['PROL_DROITE']=='EXCLU'    :
         eps_d=(val-self.vale_x[-1])/(self.vale_x[-1]-self.vale_x[-2])
         if abs(eps_d)<=tol  : return self.vale_y[-1]
         else                : raise ProlongementError, 'fonction évaluée hors du domaine de définition'
      else  : 
         if self.para['PROL_DROITE']=='CONSTANT' : return self.vale_y[-1]
         if self.para['PROL_DROITE']=='LINEAIRE' : return interp(self.para['INTERPOL'],val,self.vale_x[-1],
                                                                                           self.vale_x[-2],
                                                                                           self.vale_y[-1],
                                                                                           self.vale_y[-2])
    else :
      return interp(self.para['INTERPOL'],val,self.vale_x[i-1],
                                              self.vale_x[i],
                                              self.vale_y[i-1],
                                              self.vale_y[i])

  def homo_support(self,other) :
    """Renvoie le support d'abscisses homogénéisé entre self et other
    i.e. si prolongement exclu, on retient plus grand min ou plus petit max, selon
    si prolongement autorisé, on conserve les abscisses d'une fonction, extrapolantes
    sur l'autre.
    Pour les points intermédiaires : union et tri des valeurs des vale_x réunis.
    """
    if other.vale_x[0]>self.vale_x[0]:
       if other.para['PROL_GAUCHE']!='EXCLU' : f_g=self
       else                                  : f_g=other
    else :
       if self.para['PROL_GAUCHE'] !='EXCLU' : f_g=other
       else                                  : f_g=self
    val_min    =f_g.vale_x[0]
    prol_gauche=f_g.para['PROL_GAUCHE']
    if self.vale_x[-1]>other.vale_x[-1]:
       if other.para['PROL_DROITE']!='EXCLU' : f_d=self
       else                                  : f_d=other
    else :
       if self.para['PROL_DROITE'] !='EXCLU' : f_d=other
       else                                  : f_d=self
    val_max    =f_d.vale_x[-1]
    prol_droite=f_d.para['PROL_DROITE']
    vale_x=NP.concatenate((self.vale_x,other.vale_x))
    vale_x=NP.clip(vale_x,val_min,val_max)
    vale_x=NP.sort(list(set(vale_x)))
    return vale_x, prol_gauche, prol_droite

  def cut(self,rinf,rsup,prec,crit='RELATIF') :
    """Renvoie la fonction self dont on a 'coupé' les extrémités en x=rinf et x=rsup
    pour la recherche de rinf et rsup dans la liste d'abscisses :
       prec=precision crit='absolu' ou 'relatif'
    """
    para=copy.copy(self.para)
    para['PROL_GAUCHE']='EXCLU'
    para['PROL_DROITE']='EXCLU'
    if   crit=='ABSOLU' : rinf_tab=NP.greater(abs(self.vale_x-rinf),prec)
    elif crit=='RELATIF': rinf_tab=NP.greater(abs(self.vale_x-rinf),prec*rinf)
    else : raise FonctionError, 'fonction : cut : critère absolu ou relatif'
    if   crit=='ABSOLU' : rsup_tab=NP.greater(abs(self.vale_x-rsup),prec)
    elif crit=='RELATIF': rsup_tab=NP.greater(abs(self.vale_x-rsup),prec*rsup)
    else : raise FonctionError, 'fonction : cut : critère absolu ou relatif'
    if NP.alltrue(rinf_tab) : i=NP.searchsorted(self.vale_x,rinf)
    else                 : i=rinf_tab.tolist().index(0)+1
    if NP.alltrue(rsup_tab) : j=NP.searchsorted(self.vale_x,rsup)
    else                 : j=rsup_tab.tolist().index(0)
    vale_x=NP.array([rinf,]+self.vale_x.tolist()[i:j]+[rsup,])
    vale_y=NP.array([self(rinf),]+self.vale_y.tolist()[i:j]+[self(rsup),])
    return t_fonction(vale_x,vale_y,para)

  def cat(self,other,surcharge) :
    """renvoie une fonction concaténée avec une autre, avec règles de surcharge
    """
    para=copy.copy(self.para)
    if self.para['INTERPOL']!=other.para['INTERPOL'] : raise FonctionError, 'concaténation de fonctions à interpolations différentes'
    if NP.min(self.vale_x)<NP.min(other.vale_x) :
            f1=self
            f2=other
    else                                  :
            f1=other
            f2=self
    para['PROL_GAUCHE']=f1.para['PROL_GAUCHE']
    if   surcharge=='GAUCHE' :
       i=NP.searchsorted(f2.vale_x,f1.vale_x[-1])
       if i!=len(f2.vale_x) : para['PROL_DROITE']=f2.para['PROL_DROITE']
       else                 : para['PROL_DROITE']=f1.para['PROL_DROITE']
       vale_x = NP.concatenate((f1.vale_x, f2.vale_x[i:len(f2.vale_x)]))
       vale_y = NP.concatenate((f1.vale_y, f2.vale_y[i:len(f2.vale_y)]))
    elif surcharge=='DROITE' :
       i=NP.searchsorted(f1.vale_x,f2.vale_x[0])
       if i!=len(f1.vale_x) : para['PROL_DROITE']=f2.para['PROL_DROITE']
       else                 : para['PROL_DROITE']=f1.para['PROL_DROITE']
       vale_x = NP.concatenate((f1.vale_x[:i], f2.vale_x))
       vale_y = NP.concatenate((f1.vale_y[:i], f2.vale_y))
    return t_fonction(vale_x,vale_y,para)

  def tabul(self) :
    """mise en forme de la fonction selon un vecteur unique (x1,y1,x2,y2,...)
    """
    __tab=NP.array([self.vale_x,self.vale_y])
    return NP.ravel(NP.transpose(__tab)).tolist()

  def extreme(self) :
    """renvoie un dictionnaire des valeurs d'ordonnées min et max
    """
    val_min=min(self.vale_y)
    val_max=max(self.vale_y)
    vm={}
    vm['min']=[[self.vale_y[i],self.vale_x[i]] for i in range(len(self.vale_x))\
                                               if self.vale_y[i]==val_min]
    vm['max']=[[self.vale_y[i],self.vale_x[i]] for i in range(len(self.vale_x))\
                                               if self.vale_y[i]==val_max]
    vm['min'].sort()
    vm['max'].sort()
    for item in vm['min'] : item.reverse()
    for item in vm['max'] : item.reverse()
    return vm

  def trapeze(self,coef) :
    """renvoie la primitive de la fonction, calculée avec la constante d'intégration 'coef'
    """
    n = len(self.vale_y)   # [1:n] car pb sur calibre 5 (etch, python2.5 x86_64)
    trapz     = NP.zeros(n, float)
    trapz[0]  = coef
    trapz[1:n] = (self.vale_y[1:n]+self.vale_y[:-1])/2*(self.vale_x[1:n]-self.vale_x[:-1])
    prim_y=NP.cumsum(trapz)
    para=copy.copy(self.para)
    para['PROL_GAUCHE']='EXCLU'
    para['PROL_DROITE']='EXCLU'
    if   para['NOM_RESU'][:4]=='VITE' : para['NOM_RESU']='DEPL'
    elif para['NOM_RESU'][:4]=='ACCE' : para['NOM_RESU']='VITE'
    return t_fonction(self.vale_x,prim_y,para)

  def simpson(self,coef) :
    """renvoie la primitive de la fonction, calculée avec la constante d'intégration 'coef'
    """
    para=copy.copy(self.para)
    para['PROL_GAUCHE']='EXCLU'
    para['PROL_DROITE']='EXCLU'
    if   para['NOM_RESU'][:4]=='VITE' : para['NOM_RESU']='DEPL'
    elif para['NOM_RESU'][:4]=='ACCE' : para['NOM_RESU']='VITE'
    fm      = self.vale_y[0]
    fb      = self.vale_y[1]
    h2      = self.vale_x[1] - self.vale_x[0]
    tabl    = [coef,coef +(fb+fm)*h2/2.]
    prim_y  = copy.copy(tabl)
    iperm   = 0
    ip      = (1,0)
    eps     = 1.E-4
    for i in range(2,len(self.vale_x)) :
       h1  = h2
       h2  = self.vale_x[i] - self.vale_x[i-1]
       bma = h1 + h2
       fa  = fm
       fm  = fb
       fb  = self.vale_y[i]
       deltah = h2 - h1
       if h1==0. or h2==0. or abs( deltah / h1 ) <= eps :
          ct  = (1.,4.,1.)
       else :
          epsi = deltah / (h1*h2)
          ct   = (1.-epsi*h2,2.+epsi*deltah,1.+epsi*h1)
       tabl[iperm] = tabl[iperm] + (bma/6.)*(ct[0]*fa+ct[1]*fm+ct[2]*fb)
       prim_y.append(tabl[iperm])
       iperm       = ip[iperm]
    return t_fonction(self.vale_x,prim_y,para)

  def derive(self) :
    """renvoie la dérivée de la fonction
    """
    n = len(self.vale_y)   # [1:n] car pb sur calibre 5 (etch, python2.5 x86_64)
    pas=self.vale_x[1:n]-self.vale_x[:-1]
    pentes=(self.vale_y[1:n]-self.vale_y[:-1])/(self.vale_x[1:n]-self.vale_x[:-1])
    derive=(pentes[1:n-1]*pas[1:n-1]+pentes[:-1]*pas[:-1])/(pas[1:n-1]+pas[:-1])
    derv_y=[pentes[0]]+derive.tolist()+[pentes[-1]]
    para=copy.copy(self.para)
    para['PROL_GAUCHE']='EXCLU'
    para['PROL_DROITE']='EXCLU'
    if   para['NOM_RESU'][:4]=='DEPL' : para['NOM_RESU']='VITE'
    elif para['NOM_RESU'][:4]=='VITE' : para['NOM_RESU']='ACCE'
    return t_fonction(self.vale_x,derv_y,para)

  def inverse(self) :
    """renvoie l'inverse de la fonction
    on intervertit vale_x et vale_y, on swape interpolation
    """
    para=copy.copy(self.para)
    para['NOM_RESU']='TOUTRESU'
    para['NOM_PARA']=self.para['NOM_PARA']
    para['INTERPOL'].reverse()
    if para['PROL_GAUCHE']=='CONSTANT' : para['PROL_GAUCHE']='EXCLU'
    if para['PROL_DROITE']=='CONSTANT' : para['PROL_DROITE']='EXCLU'
    vale_x=self.vale_y
    vale_y=self.vale_x
    if not is_ordo(vale_x) :
       vale_x=vale_x[::-1]
       vale_y=vale_y[::-1]
    return t_fonction(vale_x,vale_y,para)

  def abs(self) :
    """renvoie la mm fonction avec valeur absolue des ordonnées
    """
    para=copy.copy(self.para)
    if para['PROL_GAUCHE']=='LINEAIRE' : para['PROL_GAUCHE']='EXCLU'
    if para['PROL_DROITE']=='LINEAIRE' : para['PROL_DROITE']='EXCLU'
    return t_fonction(self.vale_x,NP.absolute(self.vale_y),para)

  def evalfonc(self,liste_val) :
    """renvoie la mm fonction interpolée aux points définis par la liste 'liste_val'
    """
    return self.__class__(liste_val,map(self,liste_val),self.para)

  def suppr_tend(self) :
    """pour les corrections d'accélérogrammes
    suppression de la tendance moyenne d'une fonction
    """
    para=copy.copy(self.para)
    xy=NP.sum(self.vale_x*self.vale_y)
    x0=NP.sum(self.vale_x)
    y0=NP.sum(self.vale_y)
    xx=NP.sum(self.vale_x*self.vale_x)
    n=len(self.vale_x)
    a1 = ( n*xy - x0*y0) / (n*xx - x0*x0)
    a0 = (xx*x0 - x0*xy) / (n*xx - x0*x0)
    return t_fonction(self.vale_x,self.vale_y-a1*self.vale_x-a0,self.para)

  def normel2(self) :
    """norme de la fonction
    """
    __ex=self*self
    __ex=__ex.trapeze(0.)
    return NP.sqrt(__ex.vale_y[-1])

  def fft(self,methode) :
    """renvoie la transformée de Fourier rapide FFT
    """
    para=copy.copy(self.para)
    para['NOM_PARA']='FREQ'
    if self.para['NOM_PARA']!='INST' :
       raise ParametreError, 'fonction réelle : FFT : NOM_PARA=INST pour une transformée directe'
    pas = self.vale_x[1]-self.vale_x[0]
    for i in range(1,len(self.vale_x)) :
        ecart = NP.abs(((self.vale_x[i]-self.vale_x[i-1])-pas)/pas)
        if ecart>1.e-2 :
           raise FonctionError, 'fonction réelle : FFT : la fonction doit etre à pas constant'
    n = get_len_puis2(self.vale_x)
    if   methode=='TRONCATURE' :
       vale_y=self.vale_y[:2**n]
    elif methode=='PROL_ZERO'  :
       vale_y=self.vale_y
       if len(self.vale_y) < 2**(n + 1):
          vale_y=NP.array(self.vale_y)
          vale_y = NP.concatenate( (vale_y, NP.zeros(2**(n+1)-len(self.vale_x))) )
    elif   methode=='COMPLET'  : 
       vale_y=self.vale_y
    vect=FFT.fft(vale_y)
    pasfreq =1./(pas*(len(vect)))
    vale_x  =[pasfreq*i for i in range(len(vect))]
    vale_y  =vect
    return t_fonction_c(vale_x,vale_y,para)


# -----------------------------------------------------------------------------
class t_fonction_c(t_fonction) :
  """Classe pour fonctions complexes, équivalent au type aster = fonction_c
  """
  def tabul(self) :
    """mise en forme de la fonction selon un vecteur unique (x1,yr1,yi1,x2,yr2,yr2,...)
    """
    __tab=NP.array([self.vale_x,self.vale_y.real,self.vale_y.imag])
    return NP.ravel(NP.transpose(__tab)).tolist()

  def __repr__(self) :
    """affichage de la fonction en double colonne
    """
    texte=[]
    for i in range(len(self.vale_x)) :
      texte.append('%f %f + %f .j' % (self.vale_x[i],self.vale_y[i].real,self.vale_y[i].imag))
    return '\n'.join(texte)

  def fft(self,methode,syme) :
    """renvoie la transformée de Fourier rapide FFT (sens inverse)

       Dans le cas syme == 'NON', on choisit de renvoyer 
       un vecteur de longueur 2N, ou N est la longueur du vecteur F de depart,
       en faisant l'approximation que pour la vraie FFT, F(N+1) est fonction
       de F(N) et F(N-1).
       On effectue un prolongement a l'ordre 2 par continuite pour simplifier
       l'analyse des pas de temps en post traitement
    """
    para=copy.copy(self.para)
    para['NOM_PARA']='INST'
    if self.para['NOM_PARA']!='FREQ' :
       raise ParametreError, 'fonction complexe : FFT : NOM_PARA=FREQ pour une transformée directe'
    pas = self.vale_x[1]-self.vale_x[0]
    for i in range(1,len(self.vale_x)) :
        ecart = NP.abs(((self.vale_x[i]-self.vale_x[i-1])-pas)/pas)
        if ecart>1.e-3 :
           raise FonctionError, 'fonction complexe : FFT : la fonction doit etre à pas constant'
    n = get_len_puis2(self.vale_x)
    if   syme=='OUI' :
       vale_fonc=self.vale_y
    else :
       if methode=='PROL_ZERO' :
          fonc_temp=self.vale_y
          if len(self.vale_y) < 2**(n + 1):
             fonc_temp = NP.concatenate( (self.vale_y, NP.zeros(2**(n+1)-len(self.vale_x)))  )
       elif methode=='TRONCATURE' :
          fonc_temp=self.vale_y[:2**n]
       elif methode=='COMPLET' :
          fonc_temp=self.vale_y
       part1=fonc_temp.tolist()

       if NP.remainder(len(part1),2) == 0 :
          # Si le nombre de point du spectre est pair,
          # on prolonge en interpolant les 3 dernier points par un polynome de
          # degre 2, en imposant une tangente horizontale au dernier point (celui
          # dont on cherche l'ordonnee :
          # on pose Y=a.w^2+b.w+C , ou Y est la partie reelle de la FFT, et
          # w la frequence. On connait Y(N-1), Y(N), et on impose dY/dw(N+1)=0
          # pour la symetrie. On identifie a, b et c, pour calculer Y(N+1)
          middle=[];
          middle.append((4*part1[-1].real-part1[len(part1)-2].real)/3);
          part2=NP.conjugate(fonc_temp[1:len(part1)])
          part2=part2.tolist();
          part2.reverse();
          vale_fonc=NP.array(part1+middle+part2)
       else :
          # Si le dernier point est effectivement reel, on reconstruit theoriquement
          if abs(part1[-1].imag) < abs(part1[-1].real*1e-6) :
             part1[-1]=part1[-1].real
          else :
          # Sinon, on approxime comme dans le cas ou N est pair
             part1[-1]=(4*part1[len(part1)-2].real-part1[len(part1)-3].real)/3
          part2=NP.conjugate(fonc_temp[1:len(part1)-1])
          part2=part2.tolist();
          part2.reverse();
          vale_fonc=NP.array(part1+part2)

    vect=FFT.ifft(vale_fonc)
    vect=vect.real
    pasfreq =1./(pas*(len(vect)))
    vale_x  =[pasfreq*i for i in range(len(vect))]
    vale_y  =vect
    return t_fonction(vale_x,vale_y,para)

# -----------------------------------------------------------------------------
class t_nappe :
  """Classe pour nappes, équivalent au type aster = nappe_sdaster
  """
  def __init__(self,vale_para,l_fonc,para,nom='') :
    """Création d'un objet nappe
    - vale_para est la liste de valeur des parametres (mot clé PARA dans DEFI_NAPPE)
    - para est un dictionnaire contenant les entrées PROL_DROITE, PROL_GAUCHE et INTERPOL (cf sd ASTER)
    - l_fonc est la liste des fonctions, de cardinal égal à celui de vale_para
    """
    self.nom = nom
    pk=para.keys()
    pk.sort()
    if pk!=['INTERPOL','NOM_PARA','NOM_PARA_FONC','NOM_RESU','PROL_DROITE','PROL_GAUCHE'] :
         raise FonctionError, 'nappe : parametres incorrects'
    if para['INTERPOL'] not in [['NON','NON'],['LIN','LIN'],
                                ['LIN','LOG'],['LOG','LOG'],['LOG','LIN'],] :
         raise FonctionError, 'nappe : parametre INTERPOL incorrect'
    if para['PROL_DROITE'] not in ['EXCLU','CONSTANT','LINEAIRE'] :
         raise FonctionError, 'nappe : parametre PROL_DROITE incorrect'
    if para['PROL_GAUCHE'] not in ['EXCLU','CONSTANT','LINEAIRE'] :
         raise FonctionError, 'nappe : parametre PROL_GAUCHE incorrect'
    self.vale_para    = NP.array(vale_para)
    if not is_enum(l_fonc):
         raise FonctionError, 'nappe : la liste de fonctions fournie n est pas une liste'
    if len(l_fonc)!=len(vale_para) :
         raise FonctionError, 'nappe : nombre de fonctions différent du nombre de valeurs du paramètre'
    for f in l_fonc :
      if not isinstance(f,t_fonction) and not isinstance(f,t_fonction_c) :
         raise FonctionError, 'nappe : les fonctions fournies ne sont pas du bon type'
    self.l_fonc       = l_fonc
    self.para         = para

  def __call__(self,val1,val2):
    """méthode pour évaluer nappe(val1,val2)
    """
    i=NP.searchsorted(self.vale_para,val1)
    n=len(self.vale_para)
    if i==0 :
      if val1==self.vale_para[0]  : return self.l_fonc[0](val2)
      if val1 <self.vale_para[0]  : 
         if self.para['PROL_GAUCHE']=='EXCLU'    : raise ParametreError, 'nappe évaluée hors du domaine de définition'
         if self.para['PROL_GAUCHE']=='CONSTANT' : return self.l_fonc[0](val2)
         if self.para['PROL_GAUCHE']=='LINEAIRE' : return interp(self.para['INTERPOL'],val1,
                                                                 self.vale_para[0],
                                                                 self.vale_para[1],
                                                                 self.l_fonc[0](val2),
                                                                 self.l_fonc[1](val2))
    elif i==n :
      if val1==self.vale_para[-1] : return self.l_fonc[-1](val2)
      if val1 >self.vale_para[-1]  : 
         if self.para['PROL_DROITE']=='EXCLU'    : raise ParametreError, 'nappe évaluée hors du domaine de définition'
         if self.para['PROL_DROITE']=='CONSTANT' : return self.l_fonc[-1](val2)
         if self.para['PROL_DROITE']=='LINEAIRE' : return interp(self.para['INTERPOL'],val1,
                                                                 self.vale_para[-1],
                                                                 self.vale_para[-2],
                                                                 self.l_fonc[-1](val2),
                                                                 self.l_fonc[-2](val2))
    else :
      return interp(self.para['INTERPOL'],val1,self.vale_para[i-1],
                                               self.vale_para[i],
                                               self.l_fonc[i-1](val2),
                                               self.l_fonc[i](val2))

  def evalfonc(self, liste_val) :
    """Renvoie la mm nappe dont les fonctions sont interpolées aux points définis
    par la liste 'liste_val'.
    """
    l_fonc = []
    for f in self.l_fonc:
      f2 = f.evalfonc(liste_val)
      l_fonc.append(f2)
    return t_nappe(self.vale_para, l_fonc, self.para)

  def __add__(self,other) :
    """addition avec une autre nappe ou un nombre, par surcharge de l'opérateur +
    """
    l_fonc=[]
    if   isinstance(other,t_nappe):
      if NP.all(self.vale_para != other.vale_para):
          raise ParametreError, 'nappes à valeurs de paramètres différentes'
      for i in range(len(self.l_fonc)) : l_fonc.append(self.l_fonc[i]+other.l_fonc[i])
    elif is_float_or_int(other):
      for i in range(len(self.l_fonc)) : l_fonc.append(self.l_fonc[i]+other)
    else:  raise FonctionError, 't_nappe : erreur de type dans __add__ : %s %s' % (other, type(other))
    return t_nappe(self.vale_para,l_fonc,self.para)

  def __mul__(self,other) :
    """multiplication avec une autre fonction ou un nombre, par surcharge de l'opérateur *
    """
    l_fonc=[]
    if   isinstance(other,t_nappe):
      if NP.all(self.vale_para != other.vale_para) :
          raise ParametreError, 'nappes à valeurs de paramètres différentes'
      for i in range(len(self.l_fonc)) : l_fonc.append(self.l_fonc[i]*other.l_fonc[i])
    elif is_float_or_int(other):
      for i in range(len(self.l_fonc)) : l_fonc.append(self.l_fonc[i]*other)
    else:  raise FonctionError, 't_nappe : erreur de type dans __mul__ : %s %s' % (other, type(other))
    return t_nappe(self.vale_para,l_fonc,self.para)

  def __repr__(self) :
    """affichage de la nappe en double colonne
    """
    texte=[]
    for i in range(len(self.vale_para)) :
      texte.append('paramètre : %f' % self.vale_para[i])
      texte.append(repr(self.l_fonc[i]))
    return '\n'.join(texte)

  def homo_support(self,other) :
    """Renvoie la nappe self avec un support union de celui de self et de other
    le support est la discrétisation vale_para et les discrétisations des fonctions
    """
    if self==other:
       return self
    vale_para=self.vale_para.tolist()+other.vale_para.tolist()
    vale_para=list(set(vale_para))
    vale_para.sort()
    vale_para=NP.array(vale_para)
    l_fonc=[]
    for val in vale_para :
      if   val in self.vale_para:
         l_fonc.append(self.l_fonc[NP.searchsorted(self.vale_para, val)])
      elif val in other.vale_para:
         other_fonc=other.l_fonc[NP.searchsorted(other.vale_para, val)]
         new_vale_x=other_fonc.vale_x
         new_para  =other_fonc.para
         new_vale_y=[self(val,x) for x in new_vale_x]
         if isinstance(other_fonc, t_fonction):
            l_fonc.append(t_fonction(new_vale_x, new_vale_y, new_para))
         if isinstance(other_fonc, t_fonction_c):
            l_fonc.append(t_fonction_c(new_vale_x, new_vale_y, new_para))
      else:
         raise FonctionError, 'combinaison de nappes : incohérence'
    return t_nappe(vale_para,l_fonc,self.para)

  def extreme(self) :
    """renvoie un dictionnaire des valeurs d'ordonnées min et max
    """
    val_min=min([min(fonc.vale_y) for fonc in self.l_fonc])
    val_max=max([max(fonc.vale_y) for fonc in self.l_fonc])
    vm={'min':[],'max':[]}
    for i in range(len(self.vale_para)) :
        for j in range(len(self.l_fonc[i].vale_y)) :
          y = self.l_fonc[i].vale_y[j]
          if y==val_min : vm['min'].append([y,self.l_fonc[i].vale_x[j],self.vale_para[i]])
          if y==val_max : vm['max'].append([y,self.l_fonc[i].vale_x[j],self.vale_para[i]])
    vm['min'].sort()
    vm['max'].sort()
    for item in vm['min'] : item.reverse()
    for item in vm['max'] : item.reverse()
    return vm


# -----------------------------------------------------------------------------
def homo_support_nappe(l_f):
   """Homogénéisation du support d'une liste de nappes.
   Retourne une liste de nappes.
   """
   if not is_enum(l_f):
      l_f = [l_f,]
   l_fres=[]
   for f in l_f:
      assert isinstance(f, t_nappe), 'Erreur : homo_support_nappe est réservé aux nappes !'
      __ff=f
      for g in l_f:
         __ff=__ff.homo_support(g)
      l_fres.append(__ff)
   return l_fres


def func_union(func,l_f) :
    """Retourne la fonction x : func(y0=l_f[0](x), y1=l_f[1](x), ...)
    sur la liste d'abscisses union de celles de self et de other.
    """
    para = copy.copy(l_f[0].para)
    # Pour les prolongements et l'interpolation, c'est la première fonction qui prime
    vale_x=[]
    for f in l_f :
        vale_x = vale_x + f.vale_x.tolist()
    # on ote les abscisses doublons
    vale_x = list(set(vale_x))
    vale_x.sort()
    vale_x = NP.array(vale_x)
    # interpolation des fonctions sur l'union des abscisses
    vale_y = [map(f,vale_x) for f in l_f]
    # applique la fonction
    vale_y = map(func, *vale_y)
    return t_fonction(vale_x, vale_y, para)


def enveloppe(l_f, crit):
    """renvoie l'enveloppe supérieure ou inférieure de self et other.
    """
    if crit.upper() == 'SUP':
       env = func_union(max, l_f)
    elif crit.upper() == 'INF':
       env = func_union(min, l_f)
    else:
       raise FonctionError, 'enveloppe : le critère doit etre SUP ou INF !'
    return env


def fractile(l_f, fract):
    """renvoie l'enveloppe supérieure ou inférieure de self et other.
    """
    para = copy.copy(l_f[0].para)
    # Pour les prolongements et l'interpolation, c'est la première fonction qui prime
    vale_x=[]
    for f in l_f :
        vale_x = vale_x + f.vale_x.tolist()
    # on ote les abscisses doublons
    vale_x = list(set(vale_x))
    vale_x.sort()
    vale_x = NP.array(vale_x)
    #
    l_vale_y=[]
    for f in l_f :
        vale_y = map(f,vale_x)
        l_vale_y.append(vale_y)
    tab_val=NP.transpose(NP.array(l_vale_y))
    tab_val=tab_val.tolist()
    for l in tab_val : l.sort()
    vale_y=[]
    if fract>=1. :
       for l_val in tab_val :
           vale_y.append(l_val[-1])
    else :
       indice=int((len(tab_val[0])-1)*fract)
       reste =(len(tab_val[0])-1)*fract-indice
       for l_val in tab_val :
           vale_y.append(l_val[indice]*(1-reste)+l_val[indice+1]*reste)
    return t_fonction(vale_x, vale_y, para)


def get_len_puis2(tab_in):
    """Retourne N, la plus grande puissance de 2 telle que 2**N <= len(tab_in)
    """
    return int( log(len(tab_in)) / log(2.) )


