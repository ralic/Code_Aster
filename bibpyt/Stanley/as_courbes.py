#@ MODIF as_courbes Stanley  DATE 05/04/2004   AUTEUR ASSIRE A.ASSIRE 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
import Numeric, copy, xmgrace


def Extr_colonne (table, para, *l_crit) :

# IN crit : couple (para, fonction) : seul les lignes ou fonction(para) = 1 sont gardees
 
  valeurs = []
  nb = 0
  while 1: 
    try:
        x = table[para, nb+1]
        sel = 1
        for crit in l_crit : 
          sel = sel and crit[1]( table[crit[0], nb+1] )
        if sel : valeurs.append(x)
        nb = nb + 1
    except KeyError:
      break  
  
  colonne = Numeric.array(valeurs)
  return colonne




# =========================================================================
#                     Courbes a deux parametres (x,y)
# =========================================================================

class Courbe :

  BAD_VALUES = 'Valeurs incorrectes (x et y -> vecteurs de meme longueur)'


# -------------------------------------------------------------------------
# BUT Initialisation d'un objet de la classe Courbe
# IN  valeurs  Numeric array (N,2) des points (x,y)
#      defaut : 1 ligne nulle


  def __init__ (self, *para) :
  
    """
     aucun parametre -> on cree un seul point (0,0)
     1 parametre     -> on formatte le tableau sur deux colonnes
     2 parametres    -> on formatte chacun des deux tableaux
     On peut ainsi traiter :
       les listes
       les tableaux Numeric
       les matrices Matrix  
    """
    
    npara = len(para)
    if npara == 0 :
      x = Numeric.array([0.])
      y = Numeric.array([0.])
    elif npara == 1 :
      xy = Numeric.array(para[0])
      xy.shape = (-1,2)
      x = xy[:,0]
      y = xy[:,1]
    elif npara == 2 :
      x = Numeric.array(para[0])
      y = Numeric.array(para[1])
      x.shape = (-1,)
      y.shape = (-1,)
    else :
      raise 'Bad parameter number (0,1 or 2)'
        
    dim_x = Numeric.shape(x)
    dim_y = Numeric.shape(y)
    if len(dim_x)<>1 or len(dim_y)<>1 : raise Courbe.BAD_VALUES
    if dim_x <> dim_y : raise Courbe.BAD_VALUES 

    self.x = copy.copy(x)
    self.y = copy.copy(y)


  def set_x(self,vect) :
    
    self.x = Numeric.array(vect)
    self.x.shape = (-1,)
    
  def set_y(self,vect) :
    
    self.y = Numeric.array(vect)
    self.y.shape = (-1,)
    
# -------------------------------------------------------------------------
# BUT Lit les abscisses de la courbe a partir d'une colonne d'une table


  def Lire_x(self, table, para, *l_crit) :

    self.x = apply(Extr_colonne, (table, para) + l_crit)
    

# -------------------------------------------------------------------------
# BUT Lit les ordonnees de la courbe a partir d'une colonne d'une table


  def Lire_y(self, table, para, *l_crit) :

    self.y = apply(Extr_colonne, (table, para) + l_crit)
    

  def Discret(self, xmin, *disc) :

# Introduit dans la premiere colonne une discretisation uniforme
# entre xmin et xmax en nbr points

    l = [xmin]
    p = 0
    while 1:
      try :
        xmax = disc[p]
        nbr  = disc[p+1]
        step = (xmax - xmin) / float(nbr)
        for i in xrange(1,nbr+1) :
          l.append(xmin+i*step)
        p    = p+2
        xmin = xmax
      except IndexError :
        break
    self.x = Numeric.array(l)
    self.y = Numeric.zeros(len(l),Numeric.Float)

  
  def Merge(self) :
  
    """
      Retourne le tuple pret a l'emploi dans VALE = (x(1),y(1),x(2),...)
    """
    
    l = len(self.x)
    tab = Numeric.array([self.x,self.y])
    tab = Numeric.transpose(tab)
    return tuple(Numeric.reshape(tab,(2*l,)))
    
    

  def Trace(self, titre = '') :
  
# BUT Trace la courbe dans xmgrace

    t = xmgrace.Xmgr()
    t.Titre(titre)
    t.Courbe(self)
    t.Attendre()
    

# -------------------------------------------------------------------------
# BUT Exporte la courbe sous forme d'une chaine de caracteres ASCII


  def __repr__(self) :
  
    n = len(self.x)
    ch = ''
      
    for i in xrange(n) :
      x = self.x[i]
      y = self.y[i]
      ch = ch + repr(x) + '  ' + repr(y) + '\n'

    return ch
    
    
    
# -------------------------------------------------------------------------
# BUT Sauvegarde la courbe au format ASCII
  
  
  def Sauve(self, fichier, titre = '') :
  
    file = open(fichier,'w')
    if titre : file.write(titre + '\n')        
    file.write(repr(self))
    file.close()    

                           

  def __ror__ (self, evol_x) :

# BUT Combinaison de deux courbes (x1,y1) et (x2,y2) pour realiser une courbe (y1,y2)

    x = evol_x.y
    y =   self.y
    return Courbe(x,y)



  def Apply(self,fonc) :
  
    """
      Realise : self.y = fonc(self.x)
    """
  
    self.y = Numeric.array(map(fonc,self.x))
  


  
  def Operation(self, fonction, *autres_courbes) :
  
# BUT Applique une fonction sur les ordonnees d'une ou plusieurs courbes

    l_valeurs = [self.y]
    taille = Numeric.shape(self.y)
    
    for courbe in autres_courbes :
      if Numeric.shape(courbe.y) <> taille :
        raise 'Dimensions incompatibles'
      l_valeurs.append(courbe.y)
      
    resu_x = copy.copy(self.x)
    resu_y = apply(fonction, l_valeurs)
    
    return Courbe(resu_x, resu_y)


  def Extract(self, x) :
  
    """
      Retourne self.y qui correspond a self.x = x (si range par ordre croissant)
    """
    
    if x < self.x[0]  or  x > self.x[-1] : 
      raise 'x hors des bornes'
    
    x0 = self.x[0]
    for i in range(1,len(self.x)) :
      x1 = self.x[i]
      if x1 < x0 : 
        raise 'Les abscisses ne sont pas croissantes'
      if x <= x1 :
        x0 = self.x[i-1]
        y0 = self.y[i-1]
        y1 = self.y[i]
        pente = (y1-y0)/(x1-x0)
        y = y0 + pente * (x-x0)
        return y, pente
      x0 = x1
       

  def Proprietes(self, prop) :
  
    if prop == 'MAX' :
      mx = self.y[0]
      for y in self.y :
        if mx < y : mx = y
      return mx
      
    if prop == 'MIN' :
      mi = self.y[0]
      for y in self.y :
        if mi > y : mi = y
      return mi
      
    if prop == 'SOMME' :
      so = 0
      for y in self.y :
        so = so + y
      return so
      
    if prop == 'MOYENNE' :
      so = 0
      for y in self.y :
        so = so + y
      return float(so)/(len(self.y)+1)
      
    raise 'Propriete inconnue'
      
    


