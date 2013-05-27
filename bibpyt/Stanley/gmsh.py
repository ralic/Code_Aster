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
import os, signal
import aster
from Utilitai.Utmess import UTMESS

try:
  from popen2 import Popen3
except:
  pass


# =========================================================================
#                       TERMINAL GRAPHIQUE GMSH
# =========================================================================

def GMSH(mode, fichier, param, options=[]) :

    """
      mode     : MAIL (gmsh produit le fichier) ou POST (gmsh lit le fichier)
      fichier  : nom du fichier d'echange
      param    : parametres d'environnement
    """

    if not options.has_key('animation_mode'): options['animation_mode'] = None

    if   param['mode'] == 'LOCAL' :
      return GMSH_LOCAL(mode, fichier, param, options)
    elif param['mode'] == 'DISTANT' :
      return GMSH_DISTANT(mode, fichier, param, options)
    elif param['mode'] == 'WINDOWS' :
      return GMSH_WINDOWS(mode, fichier, param, options)
    else :
      raise _("Mode d'environnement incorrect")


# =========================================================================

class GMSH_DISTANT :

  def __init__(self, mode, fichier, param, options) :

    """
      mode     : MAIL (gmsh produit le fichier) ou POST (gmsh lit le fichier)
      fichier  : nom du fichier d'echange
      param    : parametres d'environnement
    """

#  Mode post-traitement

    if mode == 'POST' :

      # Verifications
      for var in ['machine_gmsh', 'tmp', 'protocole']:
         if not param[var].strip():
            UTMESS('A','STANLEY_6',valk=[var])
            return

      # On renomme le fichier fort.33 en fort.33.pos
      os.rename(fichier, fichier + '.pos')
      fichier = fichier + '.pos'

      # Executable Gmsh distant
      if ( (param['machine_gmsh_exe'].strip() != '') and (param['machine_visu'].strip() != '') ):
         ex_gmsh = param['machine_gmsh_exe'] + ' -display ' + param['machine_visu']
      else:
         ex_gmsh = None

      # Protocole de recopie et d'execution distante
      copie     = param['protocole'].split('/')[0]                                    # rcp ou scp
      execution = param['protocole'].split('/')[1]                                    # rsh ou ssh

      # Accès à la machine distante (sans login ou avec login)
      mdis        = param['machine_gmsh']                                             # cli75ca
      rep_distant = mdis + ':' + param['tmp'] + '/'                                   # cli75ca:/tmp/

      # Fichier distant
      fic_distant = param['tmp'] + '/' + fichier                                      # /tmp/fort.33.pos

      if param['machine_gmsh_login'].strip() != '':
         mdis  = '-l ' + param['machine_gmsh_login'] + ' ' + mdis                     # -l assire cli75ca
         rep_distant = param['machine_gmsh_login'] + '@' + rep_distant                # assire@cli75ca:/tmp/


      # Liste des fichiers a copier sur la machine distante
      l_fichier = [ fichier ]


      # Creation du script d'affichage sur la peau et recopie sur le serveur Gmsh
      if param['SKIN'].lower() in ['oui', 'yes']:
         fichier_script = 'skin.pos'
         l_fichier.insert( 0, fichier_script )
         fw=open(fichier_script,'w')
         fw.write( 'Merge "' + fic_distant + '";' +'\n' )
         fw.write( 'Plugin(Skin).iView=-1;' +'\n' )
         fw.write( 'Plugin(Skin).Run;' +'\n' )
         fw.write( 'Delete View[0];' +'\n' )
         fw.close()

      # On efface eventuellement les anciens fichiers .pos
      for fic in l_fichier:
         txt = execution + " " + mdis + " 'rm -f " + param['tmp'] + '/' + fic + "'"
         self.Commande( txt )

      # Recopie du(des) fichier(s) .pos sur le serveur Gmsh
      for fic in l_fichier:
         txt = copie + " " + fic + " " + rep_distant + '/' + fic
         self.Commande( txt )

      # On lance Gmsh sur le serveur distant sur le premier fichier de la liste l_fichier
      if ex_gmsh:
         fic = l_fichier[0]
         txt = execution + " " + mdis + " '" + ex_gmsh + " " + param['tmp'] + '/' + fic + "'"
         self.Commande( txt )
      else:
         UTMESS('A','STANLEY_7')


      UTMESS('I','STANLEY_8')

    if mode == 'MAIL' :
      raise  'NON DVP'


  def Terminal_ouvert(self):
     # Retourne 1 si le terminal est ouvert, 0 sinon
     return 0


  def Fermer(self):
     # Ferme le terminal (si necessaire)
     # Fais le menage dans l'objet
     pass


  def Attendre(self):
     pass


  def Commande(self, cmd):
     """
        Lancement d'une commande
     """
     UTMESS('I','STANLEY_9',valk=[str(cmd)])
     try:    res = os.system(cmd)
     except: pass
     return


# =========================================================================

class GMSH_LOCAL :

  def __init__(self, mode, fichier, param, options) :

    """
      mode     : MAIL (gmsh produit le fichier) ou POST (gmsh lit le fichier)
      fichier  : nom du fichier d'echange
      param    : parametres d'environnement
    """

#  Mode post-traitement
    if mode == 'POST' :
      try: os.remove(fichier + '.pos')
      except: pass
      os.rename(fichier, fichier + '.pos')

      # Script SKIN
      if param['SKIN'].lower() in ['oui', 'yes']:
        fw=open('skin.pos','w')
        fw.write( 'Merge "' + fichier + '.pos' + '";' +'\n' )
        fw.write( 'Plugin(Skin).iView=-1;' +'\n' )
        fw.write( 'Plugin(Skin).Run;' +'\n' )
        fw.write( 'Delete View[0];' +'\n' )
        fw.close()
        shell = param['gmsh'] + ' skin.pos'

      # Script Animation des modes
      elif options['animation_mode'] == 'Animer':
        fw=open('script.pos','w')
        fw.write( 'Merge "' + fichier + '.pos' + '";' +'\n' )

        txt = """
Plugin(HarmonicToTime).iView = -1 ;
Plugin(HarmonicToTime).RealPart = 0 ;
Plugin(HarmonicToTime).ImaginaryPart = 0 ;
Plugin(HarmonicToTime).nSteps = 20 ;
Plugin(HarmonicToTime).Run ;

For i In {1:PostProcessing.NbViews-1}
      Delete View[0];
EndFor
der= PostProcessing.NbViews;
View[der-1].Visible = 1;
View[der-1].ShowScale = 0; // Show value scale?
View[der-1].ShowTime = 0; // Time display mode (0=hidden, 1=value if multiple, 2=value always, 3=step if multiple, 4=step always)
"""
        # Plugin(Annotate).Text = "bla" ;
        # Plugin(Annotate).Font = "Helvetica" ;
        # Plugin(Annotate).FontSize = 14 ;
        # Plugin(Annotate).Align = "Center" ;
        # Plugin(Annotate).Run ;

        fw.write( txt +'\n' )
        fw.close()
        shell = param['gmsh'] + ' script.pos'

      else:
        shell = param['gmsh'] + ' ' + fichier + '.pos'

      UTMESS('I','STANLEY_9',valk=[str(shell)])

      if os.name=='nt':
        res = os.system(shell)
        if res!=0: UTMESS('A','STANLEY_10')
      else:
        self.controle = Popen3(shell)

      UTMESS('I','STANLEY_8')

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


# =========================================================================

class GMSH_WINDOWS :

  def __init__(self, mode, fichier, param, options) :

    """
      mode     : MAIL (gmsh produit le fichier) ou POST (gmsh lit le fichier)
      fichier  : nom du fichier d'echange
      param    : parametres d'environnement
    """

#  Mode post-traitement

    if mode == 'POST' :

      # Verifications
      for var in ['machine_gmsh', 'smbclient', 'tmp']:
        if not param[var].strip():
          UTMESS('A','STANLEY_11',valk=[var])
          return

      # Variables
      machine_win       = param['machine_win']
      partage_win_nom   = param['partage_win_nom']
      partage_win_login = param['partage_win_login']
      partage_win_pass  = param['partage_win_pass']
      smbclient         = param['smbclient']


      lst = partage_win_nom.split('\\')
      _nom_partage = lst[0]
      if len(lst)>1: sous_rep = '\\'.join(lst[1:]) + '\\'
      else: sous_rep = ''

      os.rename(fichier, fichier + '.pos')

      # Syntaxe generale de smbclient : smbclient '\\cli70xx\temp' -N -c 'rm fic; put fic'
      # Copie du fort.33.pos
      if partage_win_login == '':
        txt = smbclient + " '\\\\" + machine_win + "\\" + _nom_partage + "' -N -c 'cd " + sous_rep + " ; rm " + fichier + ".pos ; put " + fichier + ".pos'"
        UTMESS('I','STANLEY_9',valk=[txt])
        os.system(txt)
      else:
        txt = smbclient + " '\\\\" + machine_win + "\\" + _nom_partage + "' " + "****" + " -U " + partage_win_login + " -c 'cd " + sous_rep + " ; rm " + fichier + ".pos ; put " + fichier + ".pos '"
        UTMESS('I','STANLEY_9',valk=[txt])
        txt = smbclient + " '\\\\" + machine_win + "\\" + _nom_partage + "' " + partage_win_pass + " -U " + partage_win_login + " -c 'cd " + sous_rep + " ; rm " + fichier + ".pos ; put " + fichier + ".pos '"
        os.system(txt)

      # Creation et copie du skin.pos
      if param['SKIN'].lower() in ['oui', 'yes']:
        fw=open('skin.pos','w')
        fw.write( 'Merge "' + fichier + '.pos' + '";' +'\n' )
        fw.write( 'Plugin(Skin).iView=-1;' +'\n' )
        fw.write( 'Plugin(Skin).Run;' +'\n' )
        fw.write( 'Delete View[0];' +'\n' )
        fw.close()
        if partage_win_login == '':
          txt = smbclient + " '\\\\" + machine_win + "\\" + _nom_partage + "' -N -c 'cd " + sous_rep + " ; rm skin.pos ; put skin.pos'"
          UTMESS('I','STANLEY_9',valk=[txt])
          os.system(txt)
        else:
          txt = smbclient + " '\\\\" + machine_win + "\\" + _nom_partage + "' " + "****" + " -U " + partage_win_login + " -c 'cd " + sous_rep + " ; rm skin.pos ; put skin.pos'"
          UTMESS('I','STANLEY_9',valk=[txt])
          txt = smbclient + " '\\\\" + machine_win + "\\" + _nom_partage + "' " + partage_win_pass + " -U " + partage_win_login + " -c 'cd " + sous_rep + " ; rm skin.pos ; put skin.pos'"
          os.system(txt)
        UTMESS('I','STANLEY_12')
      else:
        UTMESS('I','STANLEY_13')

    if mode == 'MAIL' :
      raise  'NON DVP'

  def Terminal_ouvert(self) : return 0
     # Retourne 1 si le terminal est ouvert, 0 sinon

  def Fermer(self) : pass
     # Ferme le terminal (si necessaire)
     # Fais le menage dans l'objet

  def Attendre(self) : pass


# =========================================================================

class SCRIPTS :

  def __init__(self, script, fichier='script.pos') :
     """
        Produit le script pour Gmsh
     """

     texte = ''

     if script == 'SKIN':
        texte += 'Merge "' + fichier + '.pos' + '";' +'\n'
        texte += 'Plugin(Skin).iView=-1;' +'\n'
        texte += 'Plugin(Skin).Run;' +'\n'
        texte += 'Delete View[0];' +'\n'
