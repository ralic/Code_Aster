#@ MODIF gmsh Stanley  DATE 01/07/2003   AUTEUR JMBHH01 J.M.PROIX 
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
import os, signal
from popen2 import Popen3
import env

def Mesh(maillage, **para) :

  """
    maillage : texte du maillage
    para     : liste des parametres du maillage, concatene au maillage
  """
  
  texte = ''
  for p in para.keys() :
    texte = texte + p + ' = ' + repr(para[p]) + ' ;\n'
  texte = texte + maillage
  
  f = open('fort.geo','w')
  f.write(texte)
  f.close()
  os.system('gmsh -3 fort.geo')
  os.rename('fort.msh','fort.19')
  os.remove('fort.geo')
  


# =========================================================================
#                       TERMINAL GRAPHIQUE GMSH
# =========================================================================


def GMSH(mode, fichier) :

    """
      mode     : MAIL (gmsh produit le fichier) ou POST (gmsh lit le fichier)
      fichier  : nom du fichier d'echange
      env      : environnement
    """

    if   env.mode == 'LOCAL' :
      return GMSH_LOCAL(mode, fichier) 
    elif env.mode == 'DISTANT' :
      return GMSH_DISTANT(mode, fichier)
    elif env.mode == 'WINDOWS' :
      return GMSH_WINDOWS(mode, fichier)
    else :
      raise "Mode d'environnement incorrect"
    
    

class GMSH_DISTANT :

  def __init__(self, mode, fichier) :
        
    """
      mode     : MAIL (gmsh produit le fichier) ou POST (gmsh lit le fichier)
      fichier  : nom du fichier d'echange
      env      : environnement
    """
  
#  Mode post-traitement

    if mode == 'POST' :
      ex_gmsh = env.gmsh
      mdis = env.machine_gmsh
      fdis = env.rep_gmsh + '/' + fichier + '.pos'
      fmdis = env.login_gmsh + '@' + mdis + ":" + fdis
      mdis = '-l ' + env.login_gmsh + ' ' + mdis 
      os.system("rcp " + fichier + " " + fmdis)
      os.system("rsh " + mdis + " '" + ex_gmsh + " " + fdis + "'")
      os.system("rsh " + mdis + " rm " + fdis)
     
    if mode == 'MAIL' :
      raise  'NON DVP' 
    

  def Terminal_ouvert(self) : return 0
  
# Retourne 1 si le terminal est ouvert, 0 sinon


  def Fermer(self) : pass

# Ferme le terminal (si necessaire)  
# Fais le menage dans l'objet

    
  def Attendre(self) : pass


      
class GMSH_LOCAL :


  def __init__(self, mode, fichier) :
        
    """
      mode     : MAIL (gmsh produit le fichier) ou POST (gmsh lit le fichier)
      fichier  : nom du fichier d'echange
      env      : environnement
    """
  
#  Mode post-traitement
    if mode == 'POST' :
      ex_gmsh = env.gmsh
      os.rename(fichier, fichier + '.pos')
      shell = env.gmsh + ' ' + fichier + '.pos'
      self.controle = Popen3(shell)  
     
    if mode == 'MAIL' :
      raise  'NON DVP' 
    

  def Terminal_ouvert(self) :
  
# Retourne 1 si le terminal est ouvert, 0 sinon

    etat = self.controle.poll()
    if etat == -1 :
      return(1)
    else :
      return(0)


  def Fermer(self) :

# Ferme le terminal (si necessaire)  
# Fais le menage dans l'objet

    if self.Terminal_ouvert() :
      os.kill(self.controle.pid, signal.SIGTERM)


    
  def Attendre(self) :
  
# Attend que l'on quitte gmsh

  
    self.controle.wait()
    self.Fermer()



class GMSH_WINDOWS :

  def __init__(self, mode, fichier) :
        
    """
      mode     : MAIL (gmsh produit le fichier) ou POST (gmsh lit le fichier)
      fichier  : nom du fichier d'echange
      env      : environnement
    """
  
#  Mode post-traitement

    if mode == 'POST' :
      mdis = env.machine_gmsh
      os.rename(fichier, fichier + '.pos')

# Syntaxe generale de smbclient : smbclient '\\cli70xx\temp' -N -c 'rm fic; put fic'
      if env.user_win == '':
        os.system(env.smbclient + " '\\\\" + mdis + "\\" + env.rep_gmsh + "' -N -c 'rm " + fichier + ".pos ; put " + fichier + ".pos'")
      else:
        os.system(env.smbclient + " '\\\\" + mdis + "\\" + env.rep_gmsh + "' " + env.user_pass + " -U " + env.user_win + " -c 'rm " + fichier + ".pos ; put " + fichier + ".pos'")

    if mode == 'MAIL' :
      raise  'NON DVP' 
    
  def Terminal_ouvert(self) : return 0
  
# Retourne 1 si le terminal est ouvert, 0 sinon

  def Fermer(self) : pass

# Ferme le terminal (si necessaire)  
# Fais le menage dans l'objet
    
  def Attendre(self) : pass
