#@ MODIF as_courbes Stanley  DATE 21/01/2003   AUTEUR ASSIRE A.ASSIRE 
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

  BAD_VALUES = 'Valeurs incorrectes'


# -------------------------------------------------------------------------
# BUT Initialisation d'un objet de la classe Courbe
# IN  valeurs  Numeric array (N,2) des points (x,y)
#      defaut : 1 ligne nulle


  def __init__ (self, valeurs = Numeric.zeros([1,2])) :
  
#  Preconditions    
    if type(valeurs) <> Numeric.ArrayType : raise Courbe.BAD_VALUES
    dim = len(Numeric.shape(valeurs))
    if dim <> 2 : raise Courbe.BAD_VALUES
    if Numeric.shape(valeurs)[1] <> 2 : raise Courbe.BAD_VALUES 

    self.x = valeurs[:,0]
    self.y = valeurs[:,1]
    

# -------------------------------------------------------------------------
# BUT Lit les abscisses de la courbe a partir d'une colonne d'une table


  def Lire_x(self, table, para, *l_crit) :

    self.x = apply(Extr_colonne, (table, para) + l_crit)
    

# -------------------------------------------------------------------------
# BUT Lit les ordonnées de la courbe a partir d'une colonne d'une table


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
        step = (xmax - xmin) / nbr
        for i in xrange(1,nbr+1) :
          l.append(xmin+i*step)
        p    = p+2
        xmin = xmax
      except IndexError :
        break
    self.x = Numeric.array(l)
    self.y = Numeric.zeros(len(l))

  

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
    valeurs = Numeric.transpose (Numeric.array( [x,y] ))
    return Courbe(valeurs)
    
  
  def Operation(self, fonction, *autres_courbes) :
  
# BUT Applique une fonction sur les ordonnées d'une ou plusieurs courbes

    l_valeurs = [self.y]
    taille = Numeric.shape(self.y)
    
    for courbe in autres_courbes :
      if Numeric.shape(courbe.y) <> taille :
        raise 'Dimensions incompatibles'
      l_valeurs.append(courbe.y)
      
    resu_x = copy.copy(self.x)
    resu_y = apply(fonction, l_valeurs)
    
    return Courbe(Numeric.transpose(Numeric.array([resu_x, resu_y])))


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
      
    


