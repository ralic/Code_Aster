#@ MODIF gmsh Stanley  DATE 17/08/2004   AUTEUR ASSIRE A.ASSIRE 
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

log = 1

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


def GMSH(mode, fichier, param) :

    """
      mode     : MAIL (gmsh produit le fichier) ou POST (gmsh lit le fichier)
      fichier  : nom du fichier d'echange
      param    : parametres d'environnement
    """
    
    if   param['mode'] == 'LOCAL' :
      return GMSH_LOCAL(mode, fichier, param) 
    elif param['mode'] == 'DISTANT' :
      return GMSH_DISTANT(mode, fichier, param)
    elif param['mode'] == 'WINDOWS' :
      return GMSH_WINDOWS(mode, fichier, param)
    else :
      raise "Mode d'environnement incorrect"
    
    

class GMSH_DISTANT :

  def __init__(self, mode, fichier, param) :

    """
      mode     : MAIL (gmsh produit le fichier) ou POST (gmsh lit le fichier)
      fichier  : nom du fichier d'echange
      param    : parametres d'environnement
    """

#  Mode post-traitement

    if mode == 'POST' :

      mdis = param['machine_gmsh']
      ex_gmsh = param['machine_gmsh_exe'] + ' -display ' + param['machine_visu']
      fdis = param['machine_gmsh_tmp'] + '/' + fichier + '.pos'
      fmdis = param['machine_gmsh_login'] + '@' + mdis + ":" + fdis
      mdis = '-l ' + param['machine_gmsh_login'] + ' ' + mdis 

      txt = "rcp " + fichier + " " + fmdis
      if log==1:
        print "Commandes lancées :"
        print txt
      os.system(txt)

      if param['SKIN']=='OUI':
        fw=open('skin.pos','w')
        fw.write( 'Merge "' + param['machine_gmsh_tmp'] + '/' + fichier + '.pos' + '";' +'\n' )
        fw.write( 'Plugin(Skin).iView=-1;' +'\n' )
        fw.write( 'Plugin(Skin).Run;' +'\n' )
        fw.write( 'Delete View[0];' +'\n' )
        fw.close()
        if log==1:
          print   "rcp skin.pos " + param['machine_gmsh_login'] + '@' + param['machine_gmsh'] + ":" + param['machine_gmsh_tmp'] + '/skin.pos'
        os.system("rcp skin.pos " + param['machine_gmsh_login'] + '@' + param['machine_gmsh'] + ":" + param['machine_gmsh_tmp'] + '/skin.pos')

        if param['machine_gmsh_exe'] !='':
          if log==1:
            print   "rsh " + mdis + " '" + ex_gmsh + " " + param['machine_gmsh_tmp'] + '/skin.pos' + "'"
            print   "rsh " + mdis + " 'rm " + fdis + "'"
            print   "rsh " + mdis + " 'rm " + param['machine_gmsh_tmp'] + '/skin.pos' + "'"
          os.system("rsh " + mdis + " '" + ex_gmsh + " " + param['machine_gmsh_tmp'] + '/skin.pos' + "'")
          os.system("rsh " + mdis + " 'rm " + fdis + "'")
          os.system("rsh " + mdis + " 'rm " + param['machine_gmsh_tmp'] + '/skin.pos' + "'")
        else: print "Le parametre 'machine_gmsh_exe' n'est pas renseigné, ouvrir le fichier .pos manuellement."
      else:
        if param['machine_gmsh_exe'] !='':
          if log==1:
            print   "rsh " + mdis + " '" + ex_gmsh + " " + fdis + "'"
            print   "rsh " + mdis + " 'rm " + fdis + "'"
          os.system("rsh " + mdis + " '" + ex_gmsh + " " + fdis + "'")
          os.system("rsh " + mdis + " 'rm " + fdis + "'")
        else: print "Le parametre 'machine_gmsh_exe' n'est pas renseigné, ouvrir le fichier .pos manuellement."

    if mode == 'MAIL' :
      raise  'NON DVP' 

  def Terminal_ouvert(self) : return 0
  
# Retourne 1 si le terminal est ouvert, 0 sinon


  def Fermer(self) : pass

# Ferme le terminal (si necessaire)  
# Fais le menage dans l'objet

    
  def Attendre(self) : pass


      
class GMSH_LOCAL :

  def __init__(self, mode, fichier, param) :
        
    """
      mode     : MAIL (gmsh produit le fichier) ou POST (gmsh lit le fichier)
      fichier  : nom du fichier d'echange
      param    : parametres d'environnement
    """
  
#  Mode post-traitement
    if mode == 'POST' :
      os.rename(fichier, fichier + '.pos')

      if param['SKIN']=='OUI':
        fw=open('skin.pos','w')
        fw.write( 'Merge "' + fichier + '.pos' + '";' +'\n' )
        fw.write( 'Plugin(Skin).iView=-1;' +'\n' )
        fw.write( 'Plugin(Skin).Run;' +'\n' )
        fw.write( 'Delete View[0];' +'\n' )
        fw.close()
        shell = param['gmsh'] + ' skin.pos'
      else:
        shell = param['gmsh'] + ' ' + fichier + '.pos'

      if log==1:
        print "Commandes lancées :"
        print shell

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

  def __init__(self, mode, fichier, param) :
        
    """
      mode     : MAIL (gmsh produit le fichier) ou POST (gmsh lit le fichier)
      fichier  : nom du fichier d'echange
      param    : parametres d'environnement
    """
  
#  Mode post-traitement

    if mode == 'POST' :
      mdis = param['machine_gmsh']
      os.rename(fichier, fichier + '.pos')

      # Syntaxe generale de smbclient : smbclient '\\cli70xx\temp' -N -c 'rm fic; put fic'
      # Copie du fort.33.pos
      if param['machine_gmsh_login'] == '':
        if log==1:
          print "Commandes lancées :"
          print param['smbclient'] + " '\\\\" + mdis + "\\" + param['machine_gmsh_tmp'] + "' -N -c 'rm " + fichier + ".pos ; put " + fichier + ".pos'"
        os.system(param['smbclient'] + " '\\\\" + mdis + "\\" + param['machine_gmsh_tmp'] + "' -N -c 'rm " + fichier + ".pos ; put " + fichier + ".pos'")
      else:
        if log==1:
          print "Commandes lancées :"
          print param['smbclient'] + " '\\\\" + mdis + "\\" + param['machine_gmsh_tmp'] + "' " + "******" + " -U " + param['machine_gmsh_login'] + " -c 'rm " + fichier + ".pos ; put " + fichier + ".pos'"
        os.system(param['smbclient'] + " '\\\\" + mdis + "\\" + param['machine_gmsh_tmp'] + "' " + param['machine_gmsh_pass'] + " -U " + param['machine_gmsh_login'] + " -c 'rm " + fichier + ".pos ; put " + fichier + ".pos'")

      # Creation et copie du skin.pos
      if param['SKIN']=='OUI':
        fw=open('skin.pos','w')
        fw.write( 'Merge "' + fichier + '.pos' + '";' +'\n' )
        fw.write( 'Plugin(Skin).iView=-1;' +'\n' )
        fw.write( 'Plugin(Skin).Run;' +'\n' )
        fw.write( 'Delete View[0];' +'\n' )
        fw.close()
        if param['machine_gmsh_login'] == '':
          txt = param['smbclient'] + " '\\\\" + mdis + "\\" + param['machine_gmsh_tmp'] + "' -N -c 'rm skin.pos ; put skin.pos'"
          if log==1:
            print txt
          os.system(txt)
        else:
          if log==1:
            print param['smbclient'] + " '\\\\" + mdis + "\\" + param['machine_gmsh_tmp'] + "' " + "******" + " -U " + param['machine_gmsh_login'] + " -c 'rm skin.pos ; put skin.pos'"
          os.system(param['smbclient'] + " '\\\\" + mdis + "\\" + param['machine_gmsh_tmp'] + "' " + param['machine_gmsh_pass'] + " -U " + param['machine_gmsh_login'] + " -c 'rm skin.pos ; put skin.pos'")

    if mode == 'MAIL' :
      raise  'NON DVP' 

  def Terminal_ouvert(self) : return 0
  
# Retourne 1 si le terminal est ouvert, 0 sinon

  def Fermer(self) : pass

# Ferme le terminal (si necessaire)  
# Fais le menage dans l'objet
    
  def Attendre(self) : pass
