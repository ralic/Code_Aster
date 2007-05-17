#@ MODIF stanley Stanley  DATE 14/05/2007   AUTEUR TARDIEU N.TARDIEU 
# -*- coding: iso-8859-1 -*-
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
"""
  INTERFACE ASTER -> POST-TRAITEMENT GRAPHIQUE

  Classes :

    CONTEXTE     ensemble des concepts Aster necessaires au post-traitement    
    ETAT_GEOM    ensemble des donnees geometriques pouvant supporter un trace
    ETAT_RESU    Descripteur de la SD resultat (champs, parametres, ...)
    SELECTION    Selection realisee par l'utilisateur via l'interface graphique
    PARAMETRES
    ERREUR
    STANLEY
    INTERFACE
    DRIVER
    DRIVER_GMSH
    DRIVER_GRACE 

"""

info = """
INTERFACE DE POST-TRAITEMENT GRAPHIQUE POUR CODE_ASTER

E. LORENTZ, P. BADEL, A. ASSIRE

STANLEY
"""

import sys, os, os.path, string, copy, tkFileDialog, cPickle, tkMessageBox, time

#import Tix as Tk
import Tkinter as Tk

import as_courbes, xmgrace, gmsh
import cata_champs,aster

# Multi-langues
try:
   import gettext
   _ = gettext.gettext
except:
   def _(mesg):
      return mesg

import Utilitai
from Utilitai import sup_gmsh
from Utilitai.Utmess import UTMESS
from graphiqueTk import *
import ihm_parametres
from Cata.cata import *
from Accas import _F
from types import *
from Utilitai import Sensibilite
from Macro.test_fichier_ops import md5file

# Salome
try:
   import salomeVisu
   __salome__ = True

except:
   txt = _('Le module python "pylotage" semble etre absent, ou mal configure, on desactive le mode Salome de Stanley')
   UTMESS('I','STANLEY',txt)
   __salome__ = False


# Version du fichier des parametres Stanley
__version_parametres__ = 1.0
__fichier_last__ = 'last10.txt'
ignore_prefixe  = 'S9999'


cata = cata_champs.CATA_CHAMPS()

# Gestion des Exceptions
texte_onFatalError = "Une erreur est intervenue. L'operation a ete annulee."

# Texte sensibilite
texte_sensibilite = "Résultat non dérivé"


# ==============================================================================

# Definition d'une table de concept maillage, d'une variable d'increment et de 
# deux unités logiques disponibles pour les operations du superviseur GMSH
global _MA, _NUM, _UL
_MA=[None]*1000
_NUM=0

# Recuperation de deux UL disponibles pour les operations du superviseur GMSH
_UL =[]
_TUL=INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
_ULGMSH=_TUL['UNITE_LIBRE',1]
DEFI_FICHIER(FICHIER='TMP', UNITE=_ULGMSH, INFO=1)
_TUL2=INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
_ULMAIL=_TUL2['UNITE_LIBRE',1]
DEFI_FICHIER(ACTION='LIBERER', UNITE=_ULGMSH, INFO=1)
DETRUIRE(CONCEPT = _F(NOM = (_TUL,_TUL2)), INFO=1, ALARME='NON')
_UL =[_ULGMSH,_ULMAIL]

# ==============================================================================

class ERREUR:
  """
    Gestion des erreurs Aster
  """

  def __init__(self) :
    pass


  def Remonte_Erreur(self, err, l_detr=[], l_return=1, texte=None):

    if not texte: texte = texte_onFatalError + "\nRaison :\n" + str(err)
    if l_detr: self.DETRUIRE(tuple(l_detr))

    UTMESS('A','STANLEY',texte)

    if l_return==0: return
    if l_return==1: return False
    if l_return==2: return False, False


  def DETRUIRE(self, l_detr=[]):
    if len(l_detr)>0: DETRUIRE(CONCEPT = _F(NOM = tuple(l_detr)), INFO=1, ALARME='NON')



# ==============================================================================

class PARAMETRES :

  """
    GESTION DES PARAMETRES DE L'OUTIL
    
    Methodes publiques
      __getitem__ : retourne la valeur d'un parametre (s'il existe)
      
  """

  def __init__(self) :

    # Gestion des erreurs
    self.erreur = ERREUR()

    # Initialisation des parametres
    self.dparam, self.dliste_section, self.aide = self.Initialise_dparam()

    # Parametres utilises par la suite dans Stanley
    self.para = self.Initialise_para(self.dparam)

    # Lecture de la derniere configuration connue ou creation d'une nouvelle configuration
    self.Detecte_Derniere_Config()

    # Si Salome n'est pas present, on l'enleve de la liste des choix possibles
    if not __salome__:
       self.dparam['mode_graphique']['val_possible'].remove('Salome')
       self.para['MODE_GRAPHIQUE']['mode_graphique'] = 'Gmsh/Xmgrace'


    # Ce parametre sert a definir si les parametres doivent etre sauvegardes en sortant de l'IHM
    self.Saved = True

    # Ce parametre sert a definir si les fontes ont ete changées dans la fenetre de parametres
    self.change_fonte = False

# ------------------------------------------------------------------------------

  def Initialise_para(self, dparam):
    '''
       Initialisation de para
    '''

    para = {}

    for section in self.dliste_section.keys():
       para[section] = {}

    for cle in dparam.keys():
       section = dparam[cle]['section']
       try:     para[section][cle] = dparam[cle]['val']
       except:  para[section][cle] = ''

    return para


# ------------------------------------------------------------------------------

  def Affectation_dico_para(self, dico):
    '''
       Affectation du contenu des variables dico dans para
    '''

    # Stocke les anciens parametres (pour voir si il faut sauver ou non le profil)
    self.old_para = self.para

    # Reinitialise para
    self.para = self.Initialise_para(self.dparam)

    # Affecte a para les valeurs de dico
    for cle in self.dparam.keys():
       section = self.dparam[cle]['section']
       try:
                self.para[section][cle] = dico[section][cle]
                if dico[section][cle] != self.old_para[section][cle]: self.Saved = False
       except:
                txt = _("<A> Impossible d'affecter la variable [") + self.dliste_section[section] + " / " + self.dparam[cle]['label'] + "]"
                UTMESS('A','STANLEY',txt)
                if   self.dparam[cle]['type'] == types.FloatType: self.para[section][cle] = 0.
                elif self.dparam[cle]['type'] == types.IntType:   self.para[section][cle] = 0
                else:                                             self.para[section][cle] = ''
                self.Saved = False

    return True


# ------------------------------------------------------------------------------

  def Initialise_dparam(self):
    '''
       Initialisation du dictionnaire des parametres dparam
    '''

    dliste_section={
       'MODE_GRAPHIQUE' : _('Mode graphique'),
       'PARAMETRES'     : _('Serveur de calcul Aster / Stanley'),
       'VISUALISATION'  : _('Options graphiques'),
       'CONFIG'         : _('Poste de travail et Serveurs Gmsh et Salome'),
                   }

     #['arial', 'helvetica', 'Courier', 'Lucida', 'times']

    liste_fontes = []
    for type_fonte in ['arial', 'helvetica', 'Courier', 'Lucida']:
       for taille_fonte in [8, 10, 12, 14, 16]:
          for mod_fonte in ['normal', 'bold']:
             liste_fontes.append( type_fonte + ' ' + str(taille_fonte) + ' ' + mod_fonte )

    dparam={

      # Mode graphique
      'mode_graphique'        : { 'label': _("Mode"),                             'val': 'Gmsh/Xmgrace',     'type': 'liste',          'section': 'MODE_GRAPHIQUE',                                      'val_possible': ["Gmsh/Xmgrace", "Salome"],      'bulle': _("Mode graphique"), },

      # Parametres IHM et Serveur Aster
      'fonte'                 : { 'label': _("Fontes"),                           'val': 'arial 10 normal',  'type': 'liste',          'section': 'PARAMETRES',                                                                                           'bulle': _("Les fontes de l'application"), 'val_possible': liste_fontes, },
      'grace'                 : { 'label': _("Xmgrace"),                          'val': 'xmgrace',          'type': 'fichier',        'section': 'PARAMETRES',      'mode_graphique': ['Gmsh/Xmgrace'],                                                  'bulle': _("Serveur de calcul : Chemin vers l'application Xmgrace"), },
      'smbclient'             : { 'label': _("Smbclient"),                        'val': 'smbclient',        'type': 'fichier',        'section': 'PARAMETRES',      'mode_graphique': ['Gmsh/Xmgrace'],             'mode': ['WINDOWS'],                 'bulle': _("Serveur de calcul : Chemin vers l'application Smbclient (pour Profil Windows)"), },
      'gmsh'                  : { 'label': _("Gmsh"),                             'val': 'gmsh',             'type': 'fichier',        'section': 'PARAMETRES',      'mode_graphique': ['Gmsh/Xmgrace'],                                                  'bulle': _("Serveur de calcul : Chemin vers l'application Gmsh"), },

      # Parametres Graphiques specifiques a Gmsh, Xmgrace, Salome
      'TAILLE_MIN'            : { 'label': _("Gmsh : Taille minimale"),           'val': 0.,                 'type': types.FloatType,  'section': 'VISUALISATION',   'mode_graphique': ['Gmsh/Xmgrace'],                                                  'bulle': '', },
      'SHRINK'                : { 'label': _("Gmsh : Shrink"),                    'val': 1.,                 'type': types.FloatType,  'section': 'VISUALISATION',   'mode_graphique': ['Gmsh/Xmgrace'],                                                  'bulle': _("Parametre pour Gmsh : SHRINK\n\nFacteur de réduction homothétique permettant d'assurer la non interpénétration des mailles."), },
      'SKIN'                  : { 'label': _("Gmsh : Affichage sur la peau"),     'val': 'non',              'type': 'liste',          'section': 'VISUALISATION',   'mode_graphique': ['Gmsh/Xmgrace'], 'val_possible': ["oui", "non"],                  'bulle': _("Parametre pour Gmsh : affichage sur la peau uniquement."), },
      'version_fichier_gmsh'  : { 'label': _("Gmsh : Version du fichier"),        'val': '1.2',              'type': 'liste',          'section': 'VISUALISATION',   'mode_graphique': ['Gmsh/Xmgrace'], 'val_possible': ["1.0", "1.2"],                  'bulle': _("Parametre pour Gmsh : version du fichier resultat."), },

      # Parametres du Poste de travail de l'utilisateur, de la machine des Services Salome ou Gmsh
      'mode'                  : { 'label': _('Mode'),                             'val': 'LOCAL',            'type': 'liste',          'section': 'CONFIG',                                              'val_possible': ["LOCAL", "DISTANT", "WINDOWS"], 'bulle': '', },
      'machine_visu'          : { 'label': _("Machine de visualisation"),         'val': '',                 'type': 'texte',          'section': 'CONFIG',          'mode_graphique': ['Gmsh/Xmgrace'],             'mode': ['DISTANT'],      'bulle': _("Adresse du poste de travail"), },

      'machine_gmsh'          : { 'label': _("Machine de Gmsh"),                  'val': '',                 'type': 'texte',          'section': 'CONFIG',          'mode_graphique': ['Gmsh/Xmgrace'],             'mode': ['DISTANT'],                 'bulle': _("Machine hebergeant le service graphique Gmsh."), },
      'machine_gmsh_login'    : { 'label': _("Login"),                            'val': '',                 'type': 'texte',          'section': 'CONFIG',          'mode_graphique': ['Gmsh/Xmgrace'],             'mode': ['DISTANT'],                 'bulle': _("Login"), },
      'machine_gmsh_exe'      : { 'label': _("Machine Gmsh : chemin vers gmsh"),  'val': 'gmsh',             'type': 'texte',          'section': 'CONFIG',          'mode_graphique': ['Gmsh/Xmgrace'],             'mode': ['DISTANT'],                 'bulle': _("Adresse du poste de travail"), },

      'machine_salome'        : { 'label': _("Machine de Salome"),                'val': '',                 'type': 'texte',          'section': 'CONFIG',          'mode_graphique': ['Salome'],                   'mode': ['DISTANT'],                 'bulle': _("Machine hebergeant le service graphique Salome."), },
      'machine_salome_login'  : { 'label': _("Login"),                            'val': '',                 'type': 'texte',          'section': 'CONFIG',          'mode_graphique': ['Salome'],   'mode': ['DISTANT'],                 'bulle': _("Login"), },
      'machine_salome_port'   : { 'label': _("Port de Salome"),                   'val': '2810',             'type': 'texte',          'section': 'CONFIG',          'mode_graphique': ['Salome'],                   'mode': ['LOCAL', 'DISTANT'],        'bulle': _("Port de Salome sur la machine hebergeant le service Salome."), },

      'machine_win'           : { 'label': _("Machine Windows/Samba"),            'val': '',                 'type': 'texte',          'section': 'CONFIG',          'mode_graphique': ['Gmsh/Xmgrace'],             'mode': ['WINDOWS'],                 'bulle': _("Machine hebergeant le partage Windows/Samba."), },
      'partage_win_nom'       : { 'label': _("Nom de partage Windows/Samba"),     'val': '',                 'type': 'texte',          'section': 'CONFIG',          'mode_graphique': ['Gmsh/Xmgrace'],             'mode': ['WINDOWS'],                 'bulle': _("Nom de partage Windows/Samba"), },
      'partage_win_login'     : { 'label': _("Nom d'utilisateur du partage"),     'val': '',                 'type': 'texte',          'section': 'CONFIG',          'mode_graphique': ['Gmsh/Xmgrace'],             'mode': ['WINDOWS'],                 'bulle': _("Partage Windows/Samba : nom d'utilisateur"), },
      'partage_win_pass'      : { 'label': _("Mot de passe du partage"),          'val': '',                 'type': 'texte',          'section': 'CONFIG',          'mode_graphique': ['Gmsh/Xmgrace'],             'mode': ['WINDOWS'],                 'bulle': _("Partage Windows/Samba : mot de passe"), },

      'protocole'             : { 'label': _("Protocole reseau"),                 'val': 'rcp/rsh',          'type': 'liste',          'section': 'CONFIG',                                              'val_possible': ["rcp/rsh", "scp/ssh"], 'mode': ['DISTANT'], 'bulle': _("Protocole de transfert reseau. \nLes .rhosts ou clés SSH doivent etre à jours."), },

      'tmp'                   : { 'label': _("Repertoire temporaire"),            'val': '/tmp',             'type': 'texte',          'section': 'CONFIG',          'mode_graphique': ['Gmsh/Xmgrace', 'Salome'],   'mode': ['LOCAL', 'DISTANT'],        'bulle': _("Repertoire temporaire"), },

           }


# Champs Bulle particulierement longs

    dparam['mode']['bulle'] = _(
"""Mode d'utilisation suivant le profil du poste de travail.

LOCAL: pour les utilisateurs qui travaillent sur une version locale d'Aster.

DISTANT: le serveur Aster et/ou la machine de Gmsh est une machine distante,
ou bien on utilise un partage Samba pour récuperer les fichiers Gmsh.

WINDOWS: pour les utilisateur qui travaillent sous Windows / Exceed avec un
répertoire partagé.
""")


    dparam['mode']['TAILLE_MIN'] = _(
"""Parametre pour Gmsh : TAILLE_MIN

Ceci permet de fixer la taille minimale d'un cote d'un element. Si cette taille n'est pas atteinte, on
procede à une transformation geometrique (affinite le long du cote trop petit). L'interet est de
pouvoir visualiser des resultats sur des elements tres etires (comme les elements de joint).
Par defaut, tm vaut 0. : on ne modifie pas la geometrie des elements. 
""")


# Aide contextuelle
    aide = { 'Gmsh/Xmgrace': {}, 'Salome': {} }
    aide['Gmsh/Xmgrace'] = {
'LOCAL': """
Mode LOCAL:

Il n'y a qu'une seule machine : le serveur de calcul Aster est sur le poste de travail de l'utilisateur (la machine locale).

Gmsh est executé sur la machine locale.

Il n'y a donc aucune adresse de machine à fournir.
""", 

'DISTANT': """
Mode DISTANT:

- La "Machine de Gmsh" est la machine sur laquelle sera executé Gmsh.
- La "Machine de visualisation" est le poste de travail de l'utilisateur, et en général, c'est la "Machine de Gmsh".

Remarques:

. La "Machine de visualisation" peut eventuellement etre différente de la "Machine de Gmsh". On a dans ce cas un schéma à trois machines différentes.
Gmsh sera executé à distance et son display sera renvoyé vers "Machine de visualisation" (mettre son IP).

. Si le paramètre "Machine de visualisation" est vide, le fichier .pos sera déposé dans le répertoire temporaire sur la "Machine de Gmsh", mais Gmsh ne sera pas lancé.
On pourra donc récupérer manuellement le fichier .pos par réseau et le visualiser (cette configuration permet d'utiliser Stanley via Exceed sous Windows et de récupérer manuellement le fichier sous Windows, par exemple en ftp).
""", 

'WINDOWS': """
Mode WINDOWS:

Dans ce mode, le poste de travail de l'utilisateur est une machine sous Windows et on accède à Stanley par un serveur X11 comme Xming ou Exceed.

On a deux cas possibles :
1. L'utilisateur a les droits pour définir un "répertoire partagé" sous son Windows (bouton droit sur un répertoire + Partage) et dans ce cas il crée lui meme son partage.
2. L'utilisateur utilise une tierce machine avec un serveur Samba sur un serveur Unix/Linux.

- La "Machine Windows/Samba" est la machine qui abrite le partage Windows ou Samba.
- Le "Nom du partage Windows/Samba" est le nom sous lequel le répertoire Windows a été partagé, ou le nom du partage Samba.
- Si nécessaire, on spécifie le nom d'utilisateur du partage et le mot de passe du partage.

Les fichiers .pos sont transférés sur le disque partagé Windows ou le partage Samba.
Dans les deux cas, il faut ouvrir depuis Windows le fichier .pos manuellement avec Gmsh Windows.
""",
}

    aide['Salome'] = {
'LOCAL': """
Mode LOCAL:

Il n'y a qu'une seule machine : le serveur de calcul Aster est sur le poste de travail de l'utilisateur (la machine locale).
Salome est executé sur la machine locale et doit etre lancé indépendament de Stanley.

Il n'y a donc aucune adresse de machine à fournir.
""", 

'DISTANT': """
Mode DISTANT:

- La "Machine de Salome" est la machine sur laquelle sera executé Salome. C'est une machine distante du serveur de calcul Aster.

Salome doit etre lancé indépendament de Stanley.
""", 

'WINDOWS': """
Mode WINDOWS:

Ce mode est indisponible car Salome n'existe pas encore sous Windows.
""", 
}

    return dparam, dliste_section, aide


# ------------------------------------------------------------------------------

  def Detecte_Derniere_Config(self):
    '''
       Lecture de la derniere config connues/utilisée.
       Si echec, alors creation d'une nouvelle configuration.
    '''

    ok_env=True
    res=True

    # essaye de lire le nom de la derniere config utilisée
    fic_env = os.path.join(os.environ['HOME'],'.stanley') + '/' + __fichier_last__

    # Essaye de determiner le dernier fichier d'environnement utilise
    try:
       f = open(fic_env)
       txt = f.read()
       f.close()
       fichier=txt.split('\n')[0]
       if not os.path.isfile(fichier): 
          ok_env=False # Le dernier fichier d'environnement n'existe plus
    except:
       ok_env=False    # Le dernier fichier d'environnement n'existe pas (premiere utilisation)

    # Si on a un fichier d'environnement on le relit
    if ok_env: res = self.Ouvrir_Fichier(fichier)

    # Si on ne trouve pas de configuration precedente, on en cree une nouvelle
    if not ok_env or not res: self.Nouvelle_Configuration()

    # Si on ne trouve pas de configuration precedente, on en cree une nouvelle
    if not res: self.Nouvelle_Configuration()


# ------------------------------------------------------------------------------

  def Ouvrir_Fichier(self, fichier):
    '''
       Lecture du fichier d'environnement "fichier"
    '''

    # Parametres relus du fichier
    old_para = {}
    ok_env=False

    txt = _("Lecture du fichier d'environnement : ") + fichier
    UTMESS('I','STANLEY',txt)
    try:
       f = open(fichier,'r')
       old_para = cPickle.load(f)
       f.close()
       ok_env=True   # Le fichier a ete relu
    except:
       ok_env=False  # Le fichier n'existe plus

    if not ok_env:
       txt = _("Il n'y a pas de fichier d'environnement. On demarre avec une configuration par defaut.")
       UTMESS('A','STANLEY',txt)

    if ok_env:
       # on verifie que le fichier d'environnement relu est conforme
       txt = _("Le fichier d'environnement n'est pas exploitable (par exemple c'est une ancienne version). On demarre avec une configuration par defaut.")
       if old_para.has_key('VERSION'):
          if old_para['VERSION'].has_key('version_parametres'):
             if not str(old_para['VERSION']['version_parametres']) == str(__version_parametres__):
                txt1 = _("Le fichier d'environnement n'a pas la version attendue. On continue mais en cas de probleme, effacer le repertoire ~/.stanley/ et relancer.")
                UTMESS('A','STANLEY',txt1)
          else:
            UTMESS('A','STANLEY',txt)
            ok_env=False    # Le fichier d'environnement est trop vieux
       else:
          UTMESS('A','STANLEY',txt)
          ok_env=False      # Le fichier d'environnement est trop vieux

    # Si la configuration relue est exploitable, on l'utilise
    if ok_env:
       # Affectation des variables lues depuis le pickle a la variable para
       res = self.Affectation_dico_para(old_para)
       self.Saved = True

       # Sauvegarde de la derniere configuration connue (fichier ~/.stanley/__fichier_last__)
       f = os.path.join(os.environ['HOME'],'.stanley') + '/' + __fichier_last__
       fw=open(f,'w')
       fw.write(fichier)
       fw.close()

       return res 
    else: return False


# ------------------------------------------------------------------------------

  def Nouvelle_Configuration(self, mode=None):
    '''
       Creation d'une nouvelle configuration "vierge" (mais pre-remplie suivant le mode et le poste de travail detecte)
    '''

    txt = _("On initialise une configuration par defaut.")
    UTMESS('I','STANLEY',txt)

    # Reinitialisation avec des parametres par defaut
    self.para={}
    for section in self.dliste_section.keys():
       self.para[section] = {}

    # On affecte ici les valeurs par defaut prises dans dparam
    for cle in self.dparam.keys():
       section = self.dparam[cle]['section']
       try:     self.para[section][cle] = self.dparam[cle]['val']
       except:  self.para[section][cle] = ''

    # Detection du mode et surcharge des parametres qu'on peut detecter
    if 'HOSTNAME' in os.environ.keys():
          self.para['CONFIG']['mode']                  = 'LOCAL'
          self.para['CONFIG']['machine_visu']          = os.environ['DISPLAY']
          self.para['CONFIG']['machine_gmsh']          = os.environ['HOSTNAME']
          self.para['CONFIG']['machine_salome']        = os.environ['HOSTNAME']

    elif 'DISPLAY' in os.environ.keys():
       mdisplay = os.environ['DISPLAY'].split(':')[0]
       if mdisplay == 'localhost':
          self.para['CONFIG']['mode']                  = 'LOCAL'
          self.para['CONFIG']['machine_visu']          = os.environ['DISPLAY']
          self.para['CONFIG']['machine_gmsh']          = 'localhost'
          self.para['CONFIG']['machine_salome']        = 'localhost'
       else:
          self.para['CONFIG']['mode']                  = 'DISTANT'
          self.para['CONFIG']['machine_visu']          = os.environ['DISPLAY']
          self.para['CONFIG']['machine_gmsh']          = mdisplay
          self.para['CONFIG']['machine_gmsh_login']    = 'please_change_me'
          self.para['CONFIG']['machine_salome']        = mdisplay
          self.para['CONFIG']['machine_salome_login']  = 'please_change_me'

    # Cas particulier Windows (pour le moment on suppose que tout est en local)
    if os.name=='nt':
          self.para['CONFIG']['mode']                  = 'LOCAL'
          self.para['CONFIG']['machine_visu']          = '127.0.0.1:0'
          self.para['CONFIG']['machine_gmsh']          = '127.0.0.1'
          self.para['CONFIG']['machine_gmsh_login']    = ''
          self.para['CONFIG']['machine_gmsh_exe']      = ''
          self.para['CONFIG']['machine_salome']        = '127.0.0.1'
          self.para['CONFIG']['machine_salome_login']  = ''
          self.para['CONFIG']['tmp']                   = os.environ['TEMP']
          self.para['CONFIG']['machine_salome_port']   = '2810'

    return


# ------------------------------------------------------------------------------

  def __getitem__(self, cle) :

    for nom_classe in self.para.keys() :
      classe = self.para[nom_classe]
      if cle in classe.keys() :
        return classe[cle]
    raise KeyError


  def __setitem__(self,cle,s_val) :

    for i in self.para.keys():
      for j in self.para[i].keys():
        if cle == j:
          self.para[i][j] = str(s_val)


  def Voir(self, interface):

    for i in self.para.keys():
      for j in self.para[i].keys():
        print self.para[i][j]


# ------------------------------------------------------------------------------

  def Ouvrir_Sous(self, interface):
    '''
       Ouvre les parametres a partir d'un fichier à choisir
    '''
    fp=tkFileDialog.askopenfile(mode='r',filetypes=[("Fichiers Stanley", "*.stn"),("Tous", "*")],parent=interface.rootTk,title="Sélectionner le fichier contenant la configuration Stanley",initialdir='~/.stanley')
    if (fp != None):
       fichier = fp.name
       res = self.Ouvrir_Fichier(fichier)
    return


# ------------------------------------------------------------------------------

  def Sauvegarder_Rapide(self, interface):
    '''
       Sauvegarde les options dans la derniere config connues/utilisée
    '''

    # essaye de lire le nom de la derniere config utilisée
    fic_env = os.path.join(os.environ['HOME'],'.stanley') + '/' + __fichier_last__

    try:
       f = open(fic_env)
       txt = f.read()
       f.close()
       fic_last=txt.split('\n')
       fichier = fic_last[0]
    except:
       self.Sauvegarder_Sous(interface)
    else:
       # Sauvegarder dans fichier
       self.Sauvegarder(fichier, interface)

    return


# ------------------------------------------------------------------------------

  def Sauvegarder_Sous(self, interface):
    '''
       Sauvegarde les parametres sous un fichier a choisir
    '''

    try:
      os.mkdir(os.environ['HOME'] + '/.stanley')
    except: pass
    fp=tkFileDialog.asksaveasfile(filetypes=[("Fichiers Stanley", "*.stn"),("Tous", "*")],parent=interface.rootTk,title="Sélectionner le fichier contenant la configuration Stanley",initialdir='~/.stanley')
    if (fp != None):
       if fp.name[-4:]!='.stn': 
          fichier=fp.name+'.stn'
       else:
          fichier=fp.name
       # Sauvegarder dans fichier
       self.Sauvegarder(fichier, interface)

    return


# ------------------------------------------------------------------------------

  def Sauvegarder(self, fichier, interface):
    '''
       Sauvegarde les parametres dans le fichier "fichier"
    '''

    try:

       # Fixe la version
       para = self.para
       para['VERSION'] = {}
       para['VERSION']['version_parametres'] = __version_parametres__

       # Ouverture du fichier
       fp=open(fichier,'w')
       cPickle.dump(para,fp)
       fp.close()

       # Sauvegarde de la derniere configuration connue (fichier ~/.stanley/__fichier_last__)
       f = os.path.join(os.environ['HOME'],'.stanley') + '/' + __fichier_last__
       fw=open(f,'w')
       fw.write(fichier)
       fw.close()
       res = True
    except:
       res = False

    if res:
       self.Saved = True
       txt = _("Nouveaux parametres sauvegardés dans : " + fichier)
       UTMESS('I','STANLEY', txt)
       interface.ligne_etat.Affecter( txt )
    else:
       txt =  _("Impossible de sauvegarder les parametres dans : " + fichier)
       UTMESS('A','STANLEY', txt)
       interface.ligne_etat.Affecter( txt )

    return


# ------------------------------------------------------------------------------

  def Terminer(self, interface):
    '''
       Termine l'execution de Stanley en verifiant la necessité de sauvegarder les parametres
    '''

    if self.Saved == False:
       reponse = tkMessageBox.askokcancel(_("Sauvegarde des parametres"), _("Les paramètres ont été modifiés. Voulez-vous les sauvegarder?") )
       if   reponse == 1: reponse = True
       elif reponse == 0: reponse = False
       if reponse: self.Sauvegarder_Sous(interface)


# ------------------------------------------------------------------------------

  def Afficher_Config(self, interface):

    # IHM d'edition des parametres
    objet_para = ihm_parametres.AFFICHAGE_PARAMETRES(interface.rootTk, self.dliste_section, self.dparam, self.para, self.aide)

    if objet_para.nouveau_para:

       # Verifie s'il faut relancer l'IHM ou pas (cad changement dans les fontes)
       section = 'PARAMETRES'
       cle     = 'fonte'
       if objet_para.nouveau_para[section][cle] != self.para[section][cle]: self.change_fonte = True

       # Affectation des parametres saisis dans l'IHM à la variable self.para
       res = self.Affectation_dico_para(objet_para.nouveau_para)


# ==============================================================================

class CONTEXTE:

  def __init__(self, resultat, maillage, modele, cham_mater, cara_elem, para_sensi) :
  
    self.resultat   = resultat
    self.maillage   = maillage
    self.modele     = modele
    self.cham_mater = cham_mater
    self.cara_elem  = cara_elem 
    self.para_sensi = para_sensi

    self.resultat_sensible = None
    if self.para_sensi:
       nom_resu_sensible = Sensibilite.NomCompose(self.resultat, self.para_sensi)
       self.resultat_sensible = resultat_jeveux(nom_resu_sensible)




# ==============================================================================



class ETAT_GEOM:

  """
    ENSEMBLE DES DONNEES GEOMETRIQUES POUVANT SUPPORTER UN TRACE
    
    Attributs publics
     volu     : liste des group_ma de dimension 3
     surf     : liste des group_ma de dimension 2
     lign     : liste des group_ma de dimension 1
     poin     : liste des group_no a un seul noeud
     mail     : dico qui indique pour chaque groupe precedent le nom
                 du maillage qui le porte
     
    Methodes publiques
     Oriente  : fournit le group_no oriente correspondant a un group_ma ligne
     Fusion   : fusionne un objet de classe ETAT_GEOM dans self
     Nombre   : fournit le nombre d'entites geometriques disponibles
     Type     : fournit le type d'une entite geometrique
     
    attributs prives :
      orie    : dictionnaire des group_no orientes
                 (cle = nom du group_ma, resu = numero)
  """


  
  def __init__(self, maillage) :

    # Gestion des erreurs
    self.erreur = ERREUR()

    self.volu = []
    self.surf = []
    self.lign = []
    self.poin = []
    self.mail = {}
    self.orie = {}


   # Classe les group_ma et les group_no par volumes, surfaces, lignes et points

    info_gma = maillage.LIST_GROUP_MA()
    info_gno = maillage.LIST_GROUP_NO()
    
   # les group_ma de dimension 3
    for gma in info_gma :
      if gma[2] == 3 and gma[0][0]<>'_' : 
        self.volu.append(gma[0])
    self.volu.sort()
      
   # les group_ma de dimension 2
    for gma in info_gma :
      if gma[2] == 2 and gma[0][0]<>'_' : 
        self.surf.append(gma[0])
    self.surf.sort()
      
   # les group_ma de dimension 1
    for gma in info_gma :
      if gma[2] == 1 and gma[0][0]<>'_' : 
        self.lign.append(gma[0])
    self.lign.sort()
      
   # Les points (group_no a un seul noeud)
    for gno in info_gno :
      if gno[1] == 1 and gno[0][0]<>'_' : 
        self.poin.append(gno[0])
    self.poin.sort()


   # renseigne le maillage
    for gp in self.volu + self.surf + self.lign + self.poin :
      self.mail[gp] = maillage



  def Nombre(self) :
  
    """
      Nombre   : fournit le nombre d'entites geometriques disponibles
    """
    
    return len(self.mail.keys())
    
    
  
  def Type(self, geom) :
  
    """
      Fournit le type d'une entite geometrique 
      
      IN
        geom : nom de l'entite
        
      RETURN 
        'VOLUME', 'SURFACE', 'CHEMIN' ou 'POINT' 
    """
    
    if geom in self.volu :
      return 'VOLUME'
    elif geom in self.surf :
      return 'SURFACE'
    elif geom in self.lign :
      return 'CHEMIN'
    elif geom in self.poin :
      return 'POINT'
    else :
      raise geom + " n'appartient pas a la SD ETAT_GEOM"
    
    
  def Oriente(self, ligne) :

    """
     Retourne le nom du group_no oriente correspondant au group_ma ligne
     Le cree le cas echeant
    
     IN  ligne    : nom du group_ma de type ligne qu'on cherche a orienter
    """
    
    if ligne not in self.lign :
      raise "ce n'est pas un chemin (mailles 1D)"
      
    if ligne in self.orie.keys() :
      num = self.orie[ligne]
      return '_OR'+repr(num)

    num = len(self.orie.keys()) + 1
    nom = '_OR'+repr(num)
    self.orie[ligne] = num

    maillage = self.mail[ligne]

    try:
      DEFI_GROUP(reuse = maillage,
                 MAILLAGE = maillage,
                 CREA_GROUP_NO = _F(GROUP_MA=ligne, NOM=nom, OPTION='NOEUD_ORDO')
                )
    except aster.error,err:
      UTMESS('A','STANLEY',texte_onFatalError+"\nRaison :\n"+str(err))
      return None
    except aster.FatalError,err:
      UTMESS('A','STANLEY',texte_onFatalError+"\nRaison :\n"+str(err))
      return None
    except Exception,err:
      UTMESS('A','STANLEY',"Cette action n'est pas realisable.\n"+str(err))
      return None

    return nom



  def Fusion(self, ma) :

    """
      Fusionne un objet de classe ETAT_GEOM dans self
      
      IN ma   nom de l'objet a fusionner dans self
    """

   # Verification qu'il n'y a pas de noms de group en commun
    for gp in ma.mail.keys() :
      if gp in self.mail.keys() :
        raise "Nom de groupe en commun ("+gp+") : fusion impossible"

    self.volu = self.volu + ma.volu
    self.surf = self.surf + ma.surf
    self.lign = self.lign + ma.lign
    self.poin = self.poin + ma.poin

    for cle in ma.mail.keys() :
      self.mail[cle] = ma.mail[cle]

    for cle in ma.orie.keys() :
      self.orie[cle] = ma.orie[cle]

# ==============================================================================

    

class ETAT_RESU:

  """
    DESCRIPTEUR DE LA SD RESULTAT (CHAMPS, PARAMETRES, ...)
    
    
    Attributs publics
    -----------------
    
      contexte
        Historique de creation de la SD resultat (CONTEXTE)

      va
        Dico des variable d acces et parametres
        cle = nom de la variable, resu = liste de ses valeurs

      cmp
        Dico des composantes des champs calcules
        cle = nom du champ, resu = liste des composantes

                        
    Methodes publiques
    ------------------
    
      Refresh
        Remet a jour le descripteur par analyse du concept resultat Aster

      Statut_champ
        Retourne le statut d'un champ pour une liste de numeros d'ordre : 
        'INACCESSIBLE', 'A_CALCULER' ou 'DEJA_CALCULE'
        
      Liste_champs_accessibles
        Pour une liste de numeros d'ordre, fournit la liste des champs accessibles
        (deja calcules et calculables)
        
      Calcul
        Realise le calcul d'un champ pour une liste de numeros d'ordre


    Attributs prives
    ----------------    

      ch
        Dico des champs accessibles a la SD resultat et decrits dans cata_champs
        cle = nom du champ, resu = liste des nume_ordre calcules
    

    Methodes privees
    ----------------

      Deja_calcule 
        Teste si un champ est deja calcule pour une liste de numeros d'ordre 
        
      Etapes_calcul 
        Retourne les differents champs intermediaires a calculer pour 
        determiner un champ sur une liste de numeros d'ordre
    
  """

  def __init__(self, contexte) :

    self.contexte = contexte
    self.Refresh()


  def Refresh(self) :

    """
      Mise a jour des variables d'etat :

        va    variables d'acces et leurs valeurs
        cmp   composantes des champs calcules
        ch    champs references dans la SD et le catalogue
    """


    # Si on n'est pas en sensibilite...
    if not self.contexte.para_sensi:
       self.va  = self.contexte.resultat.LIST_PARA() 
#       self.va  = self.contexte.resultat.LIST_VARI_ACCES() 
       self.cmp = self.contexte.resultat.LIST_NOM_CMP()       
       self.ch  = self.contexte.resultat.LIST_CHAMPS()        

    # Si on est en sensibilite...
    else:
       nom_resu_sensible = Sensibilite.NomCompose(self.contexte.resultat, self.contexte.para_sensi)
       resu_sensible = resultat_jeveux(nom_resu_sensible)

       self.va  = resu_sensible.LIST_PARA() 
#       self.va  = resu_sensible.LIST_VARI_ACCES() 
       self.cmp = resu_sensible.LIST_NOM_CMP()       
       self.ch  = resu_sensible.LIST_CHAMPS()        

    for nom_cham in self.ch.keys() :
      if nom_cham not in cata.Champs_presents() :
        del self.ch[nom_cham]
# AA : pour ajouter automatiquement une option deja calculée, il faut pouvoir determiner son support geometrique... On laisse pour le moment de coté.
#        cata.Ajoute_Champs(nom_cham)


  def Deja_calcule(self, nom_cham, numeros) :

    """
      Teste si un champ est deja calcule pour une liste de numeros d'ordre 
      IN :
        nom_cham : nom du champ
        numeros  : liste des numeros d'ordre
    """
    
    resu = 0
    if nom_cham in self.ch.keys() :
      nume_calc = self.ch[nom_cham]
      for num in numeros :
        if num not in nume_calc : break
      else :
        resu = 1
    return resu


  def Etapes_calcul(self, nom_cham, numeros) :
  
    """
      Retourne les differents champs intermediaires a calculer pour determiner
      le champ nom_cham sur une liste de numeros d'ordre
      IN :
        nom_cham : nom du champ objectif
        numeros  : liste des numeros d'ordre
      RETURN :
        Liste de champs
          si vide -> le champ n'est pas calculable
          sinon   -> champs a calculer successivement (attention, le premier est deja calcule)
          Si la longueur de la liste vaut 1, le champ est deja calcule
    """ 

    if self.Deja_calcule(nom_cham, numeros) :
      return [nom_cham]
    else :
      for ch in cata[nom_cham].heredite :
        etapes = self.Etapes_calcul(ch, numeros)
        if etapes : return (etapes + [nom_cham])
      return []
  
    
  def Statut_champ(self, nom_cham, numeros) :
  
    """
     Retourne le statu d'un champ pour une liste de numeros d'ordre :
     'INACCESSIBLE', 'A_CALCULER' ou 'DEJA_CALCULE'
     
      IN :
        nom_cham : nom du champ
        numeros  : liste des numeros d'ordre
    """
    
    if not nom_cham or not numeros : return 'INACCESSIBLE'
    etapes = self.Etapes_calcul(nom_cham, numeros)
    if len(etapes) == 0 :
      return 'INACCESSIBLE'
    elif len(etapes) == 1 :
      return 'DEJA_CALCULE'
    else :
      return 'A_CALCULER'      


  def Liste_champs_accessibles(self, numeros) :
  
    """ 
      Pour une liste de numeros d'ordre, fournit la liste des champs accessibles
      IN :
        numeros : liste des numeros d'ordre
      RETURN :
        Liste de noms de champs
    """

    liste_ch = []
    for nom_cham in self.ch.keys() :
      if self.Etapes_calcul(nom_cham, numeros) : liste_ch.append(nom_cham)
    return liste_ch
        
    
  def Calcul(self, nom_cham, numeros, options=None) :
    """
      Realise le calcul d'un champ pour une liste de numeros d'ordre
      
      IN :
        nom_cham: nom du champ a calculer
        numeros : liste des numeros d'ordre
    """

    etapes = self.Etapes_calcul(nom_cham, numeros)
    if not etapes :
      raise 'Le champ ' + nom_cham + ' est inaccessible'

    for nom_cham in etapes[1:] :
      try:
        cata[nom_cham].Evalue(self.contexte, numeros, options)
      except aster.error,err:
        UTMESS('A','STANLEY',texte_onFatalError+"\nRaison :\n"+str(err))
      except aster.FatalError,err:
        UTMESS('A','STANLEY',texte_onFatalError+"\nRaison :\n"+str(err))
      except Exception,err:
        UTMESS('A','STANLEY',"Cette action n'est pas realisable.\n"+str(err))

    self.Refresh()



# ==============================================================================

class SELECTION:

  """
    SELECTION REALISEE PAR L'UTILISATEUR DANS L'INTERFACE GRAPHIQUE


    Attributs publics
    -----------------
    
      nom_cham (L)
        Nom du champ selectionne
      
      nom_cmp (L)
        Liste des noms des composantes selectionnees

      numeros (L)
        Liste des numeros d'ordre selectionnes

      nom_va (L)
        Nom de la variable d'acces selectionnee

      vale_va (L)
        Valeurs de la variable d'acces correspondants aux numeros d'ordre selectionnes 

      mode (L)
        Mode de trace : 'Isovaleurs' ou 'Courbes'
        
      geom (L)
        Entites geometriques selectionnees : (type, [entite 1, entite 2, ...])
        type in 'MAILLAGE','GROUP_MA','CHEMIN','POINT'
        Les entites selectionnees sont necessairement du meme type

      statut (L)
        Statut du champ actif (INACCESSIBLE, A_CALCULER ou DEJA_CALCULE)

      compatible (L)
        Compatibilite du champ actif et des entites geometriques actives
          0 -> On ne sait pas tracer
          1 -> On sait tracer
      
      tracable (L)
        Tracabilite du champ
          0 -> impossible a tracer
          1 -> OK pour trace (moyennant eventuellement calculs)


    Methodes publiques
    ------------------
    
      Interface
        Affecte une interface graphique a l'objet selection
      
      Refresh
        Scan l'interface graphique, la SD resultat et etat_geom pour 
        mettre a jour la selection et l'interface graphique
        

    Attributs prives
    -----------------
    
      contexte
        Historique de creation de la SD resultat (CONTEXTE)

      etat_geom
        Etat de la geometrie (ETAT_GEOM)
      
      etat_resu
        Etat du resultat (ETAT_RESU)

      liste_cmp
        Composantes du champ selectionne

      interface
        Interface graphique Tk (INTERFACE)
        
      nbr_geom
        Nombre d'entites geometriques (pour detecter un eventuel ajout)
        
        
    Methodes privees
    -----------------
    
      Examen
        Met a jour les attributs statut et tracable compte-tenu de la selection        
    
      Change_va
        Reaction a un changement de variables d'acces
        
      Change_cmp
        Mise a jour des composantes d'un champ (si nouvelle selection du champ 
        ou calcul du champ)
        
      Change_mode
        Reaction a un changement de mode de trace (isovaleurs ou courbes)
                
  """


  NonDeveloppePG = "Fonctionnalite non developpee : seules les options 'SIEF_ELGA', 'VARI_ELGA', 'SIEF_ELGA_TEMP' et 'FLUX_ELGA_TEMP' peuvent etre visualisees aux points de Gauss."
  NonDeveloppeRS = "Fonctionnalite non developpee : seuls les resultats de type 'EVOL_ELAS', 'EVOL_THER', 'EVOL_NOLI', 'DYNA_TRANS', 'DYNA_HARMO' et 'MODE_MECA' sont geres."

  comb_tracables = {      # Combinaisons (type_champ, type_geom) tracables -> outil de trace
    ('NOEU','MAILLAGE')   : 'GMSH',
    ('NOEU','VOLUME')     : 'GMSH',
    ('NOEU','SURFACE')    : 'GMSH',
    ('NOEU','CHEMIN')     : 'GRACE',
    ('NOEU','POINT')      : 'GRACE',

    ('ELNO','MAILLAGE')   : 'GMSH',
    ('ELNO','VOLUME')     : 'GMSH',
    ('ELNO','SURFACE')    : 'GMSH',
    
    ('ELGA','MAILLAGE')   : 'GMSH',
    ('ELGA','VOLUME')     : 'GMSH',
    ('ELGA','SURFACE')    : 'GMSH',
    ('ELGA','CHEMIN')     : 'GRACE',
    ('ELGA','POINT')      : 'GRACE',

    ('ELEM','MAILLAGE')   : 'GMSH',
    ('ELEM','VOLUME')     : 'GMSH',
    ('ELEM','SURFACE')    : 'GMSH',
    ('ELEM','CHEMIN')     : 'GRACE',
    ('ELEM','POINT')      : 'GRACE',
    }
 
  def __init__(self, contexte, etat_geom, etat_resu) :

    # Gestion des erreurs
    self.erreur = ERREUR()

    self.contexte  = contexte
    self.etat_geom = etat_geom
    self.etat_resu = etat_resu
    self.nbr_geom  = etat_geom.Nombre()
    
    self.nom_va   = 'NUME_ORDRE'
    self.mode     = 'Isovaleurs'      
    self.nom_cham = ''    
    self.nom_cmp  = []    
    self.numeros  = []    
    self.vale_va  = []    
    self.geom     = None  

    self.liste_cmp = []                   
    self.Examen()


  def Interface(self, interface) :
  
    """
      Affecte une interface graphique a l'objet selection
      
      IN
        interface : interface Tk (INTERFACE)
    """
    
    self.interface = interface
    self.Refresh()
    

  def Examen(self) :

    """
      Mise a jour des variables d'etat : statut, compatible, tracable
    """
    
   # Nature du champs (INACCESSIBLE, A_CALCULER ou DEJA_CALCULE) 
    self.statut = self.etat_resu.Statut_champ(self.nom_cham, self.numeros)
            
   # le champ est-il compatible avec les choix geometriques ?  
    self.compatible = 0
    if self.geom and self.statut <> 'INACCESSIBLE' :
      type_champ = cata[self.nom_cham].type
      if (type_champ, self.geom[0]) in SELECTION.comb_tracables.keys() :
        self.compatible = 1
      if self.geom[0] == 'CHEMIN' and len(self.geom[1])>1 :
        self.compatible = 0

    
   # le champ est-il tracable
    if self.compatible == 0 or self.statut == 'INACCESSIBLE' or not self.nom_cmp  :
      self.tracable = 0
    else :
      self.tracable = 1
   

  def Change_va(self, nom_va):
    """
      Reaction a un changement de variables d'acces
      
      IN
        nom_va : nom de la nouvelle variable d'acces
    """
        
    if self.nom_va == nom_va : return
    self.nom_va = nom_va
    self.interface.ordre.Change(['TOUT_ORDRE'] + self.etat_resu.va[nom_va], 'TOUT_ORDRE')   
    self.Refresh()
    


  def Change_mode(self, mode, force = 0):
    """
      Reaction a un changement de mode de trace (isovaleurs ou courbes)

      IN
        mode : nouveau mode de trace
        force: force le changement du mode meme en l'absence de changement
               (pour recrire les group_**)    
    """
    
    if self.mode == mode and not force :
      return
      
    self.mode = mode

    if self.mode in ['Isovaleurs', 'SalomeIsovaleurs']:
      t_geom = ['TOUT_MAILLAGE']
      for nom_group in self.etat_geom.volu :
        t_geom.append(string.ljust(nom_group,8) + " (3D)")
      for nom_group in self.etat_geom.surf :
        t_geom.append(string.ljust(nom_group,8) + " (2D)")
      self.interface.geom.Change(t_geom,'TOUT_MAILLAGE')   

    elif self.mode in ['Courbes', 'SalomeCourbes']:
      t_geom = []
      for nom_group in self.etat_geom.lign :
        t_geom.append(string.ljust(nom_group,8) + " (1D)")
      for nom_group in self.etat_geom.poin :
        t_geom.append(string.ljust(nom_group,8) + " (0D)")
      self.interface.geom.Change(t_geom)   

    else : raise 'ERREUR_DVP'
    self.Refresh()


  def Change_cmp(self) :    
    """
      Mise a jour des composantes d'un champ (si nouvelle selection du champ 
      ou calcul du champ)
    """
          
    if self.nom_cham :
      liste_cmp = self.etat_resu.cmp[self.nom_cham]
    else :
      liste_cmp = []
    if liste_cmp <> self.liste_cmp :
      self.liste_cmp = liste_cmp
      self.interface.cmp.Change(['TOUT_CMP'] + liste_cmp, 'TOUT_CMP')      

    
    
  def Refresh(self) :
    """ 
      Scan l'interface graphique et la SD resultat pour mettre a jour
      la selection et l'interface graphique
    """

   # Mise a jour du nom du champ
    if self.interface.champ.courant :
      self.nom_cham = self.interface.champ.courant[0]
      help = cata[self.nom_cham].comment
      self.interface.ligne_etat.Affecter(help)

    
  # Mise a jour des composantes selectionnables et selectionnees
    self.Change_cmp()
    self.nom_cmp = self.interface.cmp.courant    


  # Mise a jour de la liste des entites geometriques
    nombre = self.etat_geom.Nombre()
    if nombre <> self.nbr_geom :
      self.nbr_geom = nombre
      self.Change_mode(self.mode, force = 1)
    
    
   # Mise a jour des noms d'entites geometriques
    if 'TOUT_MAILLAGE' in self.interface.geom.courant :
      self.geom = ('MAILLAGE',None)
    else :
      type_actu = ''
      liste_actu = []
      for nom in self.interface.geom.courant :
        nom_group  = string.strip(nom[:8]) 
        type_group = self.etat_geom.Type(nom_group)    
        liste_actu.append(nom_group)
        if not type_actu :
          type_actu = type_group
        elif type_group <> type_actu :
          self.geom = None
          break
      else :
        self.geom = (type_actu,liste_actu)
  
   # Mise a jour des numeros d'ordre
    tout_no = self.etat_resu.va['NUME_ORDRE']
    tout_va = self.etat_resu.va[self.nom_va]
    if 'TOUT_ORDRE' in self.interface.ordre.courant :
      self.numeros=tout_no
      self.vale_va=tout_va
    else :
      l_no    = []
      l_va    = []
      for i in self.interface.ordre.indice :
        no = tout_no[i-1]    # decalage de 1 car 'TOUT_ORDRE' est a l'indice 0
        va = tout_va[i-1]    # decalage de 1 car 'TOUT_ORDRE' est a l'indice 0
        l_no.append(no)
        l_va.append(va)
      self.numeros = l_no
      self.vale_va = l_va

   # Mise a jour de la nouvelle selection
    self.Examen()
    
   # Determination de la couleur du feu

    if not self.tracable  :
      couleur = 'red'
    elif self.statut == 'A_CALCULER' :
      couleur = 'orange'
    else :
      couleur = 'green'
    self.interface.feu.Changer_couleur(couleur)

# ==============================================================================


class STANLEY:

  """
    OUTIL DE POST-TRAITEMENT GRAPHIQUE
    
  """
  
  def __init__ (self, resultat, maillage, modele, cham_mater, cara_elem, para_sensi, FICHIER_VALID=None) :

    # Gestion des erreurs
    self.erreur = ERREUR()

    self.contexte   = CONTEXTE(resultat, maillage, modele, cham_mater, cara_elem, para_sensi)
    self.etat_geom  = ETAT_GEOM(maillage)
    self.etat_resu  = ETAT_RESU(self.contexte)
    self.selection  = SELECTION(self.contexte,self.etat_geom,self.etat_resu)
    self.parametres = PARAMETRES()    
    self.FICHIER_VALID = FICHIER_VALID

    self.interface  = INTERFACE(self)
    self.selection.Interface(self.interface)


   # Drivers d'outils de post-traitement
    self.driver = {
      'Isovaleurs'      : DRIVER_GMSH(self),
      'Courbes'         : DRIVER_GRACE(self),          
      'SalomeCourbes'   : DRIVER_SALOME_COURBES(self),
      'SalomeIsovaleurs': DRIVER_SALOME_ISOVALEURS(self)
      }

   # Lancement de l'interface graphique (jusqu'a ce qu'on quitte) 
    self.interface.Go()

   # Menage
    self.interface.Kill()

    for driver in self.driver.keys() :
      try :
        self.driver[driver].Fermer()
      except AttributeError :
        pass



  def Calculer(self) :
    """
      Calculer (si necessaire) le resultat d'une selection
    """

    if self.selection.statut == 'A_CALCULER' :

      # Options supplementaires a passer aux commandes CALC_ELEM/CALC_NO
      options=None

      # Calcul
      self.etat_resu.Calcul(self.selection.nom_cham, self.selection.numeros, options)
      self.selection.Refresh()


  def Tracer(self) :

    """
      Tracer le resultat d'une selection
    """

   # Precondition
    if not self.selection.tracable : 
      self.interface.Bip()
      return

   # Post-traitement (si necessaire)
    self.Calculer()

    # Permet de passer du mode Salome au mode Gmsh/Xmgrace...
    self.selection.mode = self.selection.mode.replace('Salome', '')

   # Visualisation
    if self.parametres.para['MODE_GRAPHIQUE']['mode_graphique'] == 'Salome' and self.selection.mode == 'Courbes':
       self.selection.mode = 'SalomeCourbes'
    elif self.parametres.para['MODE_GRAPHIQUE']['mode_graphique'] == 'Salome' and self.selection.mode == 'Isovaleurs':
       self.selection.mode = 'SalomeIsovaleurs'

    # Options supplementaires a passer au driver graphique
    options = {}

    # Trace sur la deformee
    if self.selection.mode == 'Isovaleurs': options['case_sur_deformee'] = self.interface.case_sur_deformee.Valeur()

    # Animation des modes d'un mode_meca sous Gmsh
    if self.parametres.para['MODE_GRAPHIQUE']['mode_graphique'] == 'Gmsh/Xmgrace':
       if self.selection.contexte.resultat.__class__== mode_meca and self.selection.nom_cham == 'DEPL':
          if not 'TOUT_ORDRE' in self.interface.ordre.courant:
             if len(self.interface.ordre.courant)==1:
                liste = ['Animer', 'Ne pas animer']
                options['animation_mode'] = SAISIE_MODE( liste, _("Animation") )

    # Trace
    self.driver[self.selection.mode].Tracer(self.selection, options )    



  def Information(self) :
  
    self.selection.interface.Information()
    

  def Ajout_point(self) :
  
    """
      Creation interactive d'un point de post-traitement
    """

    # Lecture des caracteristiques du chemin    
    (nom,x0,y0,z0) = self.interface.Requete_point()
    if not nom: return

    try:
      # definition des nouvelles entites geometriques
      driver = DRIVER_SUP_GMSH(self)
      geom = driver.Importer_point(nom,x0,y0,z0)
      if not geom: return
      
      # Incorporation du nouveau point 
      self.etat_geom.Fusion(geom)
    except: pass
    self.selection.Refresh()


  def Ajout_chemin(self) :

    """
      Creation interactive d'un chemin de post-traitement
    """

    # Lecture des caracteristiques du chemin    
    (nom,x0,y0,z0,x1,y1,z1,nbr) = self.interface.Requete_chemin()
    if not nom: return

    try:
      # definition des nouvelles entites geometriques
      driver = DRIVER_SUP_GMSH(self)
      geom = driver.Importer_chemin (nom,x0,y0,z0,x1,y1,z1,nbr)
      if not geom: return

      # Incorporation du nouveau chemin 
      self.etat_geom.Fusion(geom)
    except: pass
    self.selection.Refresh()

  def Editer_Parametres(self) : 
    self.parametres.Afficher_Config(self.interface)

    if self.parametres.change_fonte:
       reponse = tkMessageBox.askokcancel(_("Changement des fontes"), _("Pour actualiser les fontes, il faut relancer l'IHM. Voulez-vous relancer Stanley ? Le fichier des paramètres sera modifiés.") )
       if   reponse == 1: reponse = True
       elif reponse == 0: reponse = False
       if reponse:
          self.selection.interface.rootTk.quit()
          self.parametres.Sauvegarder_Rapide(self.interface)
          self.parametres.Terminer(self.interface)
          self.Fermeture_Propre_Stanley()
          STANLEY(self.contexte.resultat, self.contexte.maillage, self.contexte.modele, self.contexte.cham_mater, self.contexte.cara_elem, self.FICHIER_VALID)
#          self.Select()

  def Voir(self) : 
    self.parametres.Voir(self.interface)

  def Open_config(self): 
    self.parametres.Ouvrir_Sous(self.interface)

  def Save_config(self): 
    self.parametres.Sauvegarder_Sous(self.interface)

  def Save_rapide(self): 
    self.parametres.Sauvegarder_Rapide(self.interface)

  def Quitter(self):
    l_detr=[]
    for i in range(_NUM):
      l_detr.append( '_MA_'+str(i) )
    if len(l_detr)>0: DETRUIRE(CONCEPT=_F(NOM= l_detr ), INFO=1, ALARME='NON')
    self.selection.interface.rootTk.quit()
    self.parametres.Terminer(self.interface)

  def Select(self):
     """
        Ferme Stanley et relance Pre_Stanley
     """
     self.Fermeture_Propre_Stanley()
     PRE_STANLEY(self.FICHIER_VALID)


  def Fermeture_Propre_Stanley(self):
     """
        Fermeture de Stanley : cloture les drivers, efface les concepts temporaires
     """
     for driver in self.driver.keys() :
        try :   
           self.driver[driver].Fermer()
        except AttributeError :
           pass

     for i in range(_NUM):
        DETRUIRE(CONCEPT=_F(NOM='_MA_'+str(i)), INFO=1, ALARME='NON')
     self.interface.Kill()


  def Clavier(self,event) :
    """
      Reaction aux raccourcis clavier (CTRL + touche)
    """
    touche = event.keysym
    if touche == 'q' : self.Quitter()
    if touche == 'c' : self.Calculer()
    if touche == 't' : self.Tracer()
#     if touche == 'v' : self.Voir()
# 
#   def Voir(self):
#     import pprint
#     print 20*'/\__'
#     pprint.pprint (RESU.LIST_PARA())
#     print 20*'/\__'
#     pprint.pprint (RESU.LIST_VARI_ACCES())
#     print 20*'/\__'
#     pprint.pprint (RESU.LIST_NOM_CMP())
#     print 20*'/\__'
#     pprint.pprint (RESU.LIST_CHAMPS())
#     print 20*'/\__'


# ==============================================================================

class INTERFACE :

  """
    INTERFACE GRAPHIQUE TK
    
    Attributs publics
      champ      : LIST_BOX champ selectionne
      cmp        : LIST_BOX composantes selectionnees
      geom       : LIST_BOX entites geometriques selectionnees
      ordre      : LIST_BOX numeros d'ordre selectionnes 
      feu        : FEU_TRICOLORE etat de la selection
      ligne_etat : LIGNE_ETAT ligne d'etat 
      
      
    Methodes publiques
    ------------------ 
    
      Go       
        Lancement du scan des evenements
      
      Kill
        Supprime l'interface
      
      Bip
        Emet un bip
        
      Requete_point
        Boite de dialogue pour la creation d'un point
        
      Requete_chemin
        Boite de dialogue pour la creation d'un chemin
        
      Requete_para
        para       : objet PARAMETRES
        nom_classe : nom de la classe de parametres
        Boite de dialogue pour l'edition de parametres

    Attributs prives
      stan       : reference vers l'objet maitre stanley (pour Refresh, Calculer, Tracer)
      rootTk     : racine des objets Tk
      
    Methodes privees
      Action_evenement : actions consecutives au scan des evenements
      Dessin     : creation des objets graphiques Tk de l'interface  
  """


  def __init__(self, stan) :   
  
    """
      IN stan    : Reference vers stanley pour acces aux methodes Refresh,
                   Tracer et Calculer en lien avec les boutons et un
                   changement dans la selection
    """

    # Gestion des erreurs
    self.erreur = ERREUR()


    etat_geom = stan.etat_geom
    etat_resu = stan.etat_resu
    numeros   = stan.etat_resu.va['NUME_ORDRE']
        
   # Liste des champs
    t_champ = etat_resu.Liste_champs_accessibles(numeros)
    t_champ.sort()  

   # Liste des composantes
    t_cmp = ['TOUT_CMP']
        
   # Liste des entites geometriques     
    t_geom = ['TOUT_MAILLAGE']
    for nom_group in etat_geom.volu :
      t_geom.append(string.ljust(nom_group,8) + " (3D)")
    for nom_group in etat_geom.surf :
      t_geom.append(string.ljust(nom_group,8) + " (2D)")

   # Liste des numeros d'ordre
    t_no = ['TOUT_ORDRE'] + numeros

   # Liste des variables d'acces
    l_va = etat_resu.va.keys()    

    self.stan = stan
    self.rootTk    = Tk.Tk()       
    self.rootTk.wm_title('STANLEY')
    self.Dessin(t_champ, t_cmp, t_geom, t_no, l_va)
    self.Scan_selection()               



  def Go(self) :

    """ 
      Demarre le scan des evenements
    """
    
    self.rootTk.mainloop()


  def Kill(self) :
  
    """
      Supprime l'interface
    """
    try:
      self.rootTk.after_cancel(self.after_id)
      self.rootTk.destroy()
    except:
      pass


  def Scan_selection(self) :

    """
      Action sur les evenements
        Scan les objets selectionnes
        Definit la frequence de scan
    """
  
    different = ( self.champ.Scan() or 
                  self.cmp.Scan()   or
                  self.geom.Scan()  or 
                  self.ordre.Scan()
                )

    if different :
      self.stan.selection.Refresh()   

    self.after_id = self.rootTk.after(30, self.Scan_selection)

    
  def Dessin(self, t_champ, t_cmp, t_geom, t_no, l_va) :

    """
      Creation de tous les objets graphiques de l'interface
    """

    # Fontes
    fonte = self.stan.parametres['fonte']

   # Frames generales
    frame_menu  = Tk.Frame(self.rootTk,relief=Tk.RAISED,bd=1)
    frame_menu.grid(row=0,column=0,sticky = Tk.NW)
#    frame_menu.pack(padx=0,pady=0,anchor = Tk.NW)

    frame_boutons  = Tk.Frame(self.rootTk,relief=Tk.FLAT,bd=1)
    frame_boutons.grid(row=0,column=2, sticky = Tk.NE)
#    frame_boutons.pack(anchor = Tk.NE,pady=0)

    frame_selection  = Tk.Frame(self.rootTk,relief=Tk.RAISED,bd=3)
    frame_selection.grid(row=1,column=0,columnspan=3)
#    frame_selection.pack(padx=2,pady=10)

    frame_bas = Tk.Frame(self.rootTk)
    frame_bas.grid(row=2,column=0,columnspan =3,pady=3)
    frame_espace = Tk.Frame(self.rootTk)
    frame_espace.grid(row=0,column=1,pady=20)
    
    
   # Menu deroulant
#    titres = ['Fichier','Geometrie','Parametres','Actions']
    titres = ['Fichier','Geometrie','Parametres']
    choix  = {}
    choix['Fichier'] = [
                        ('Informations',self.stan.Information),
                        ('Quitter',self.stan.Quitter)]
    choix['Geometrie'] = [('Ajout Point',self.stan.Ajout_point),
                          ('Ajout Chemin',self.stan.Ajout_chemin),]
#     choix['Actions'] = [ ('Calculer',self.stan.Calculer),
#                          ('Tracer',self.stan.Tracer)]

    choix['Parametres'] = [
                            ('Charger',self.stan.Open_config),
                            ('Editer',self.stan.Editer_Parametres),
                            ('Sauvegarder',self.stan.Save_rapide),
                            ('Sauvegarder sous',self.stan.Save_config),
                          ]

    menu = MENU(frame_menu,titres,choix,fonte=fonte)     

   # boite de saisie des champs
    frame_champs = Tk.Frame(frame_selection)
    frame_champs.pack(side=Tk.LEFT,padx=5)
    MENU_RADIO_BOX(frame_champs, "Champs",fonte=fonte)
    self.champ = LIST_BOX(frame_champs,t_champ,Tk.SINGLE,fonte=fonte)

   # boite de saisie des composantes
    frame_cmp  = Tk.Frame(frame_selection)
    frame_cmp.pack(side=Tk.LEFT,padx=5)
    MENU_RADIO_BOX(frame_cmp,"Composantes",fonte=fonte)
    self.cmp = LIST_BOX(frame_cmp,t_cmp,Tk.EXTENDED,defaut = 'TOUT_CMP',fonte=fonte)

   # boite de saisie d'entites geometriques
    frame_geom  = Tk.Frame(frame_selection)
    frame_geom.pack(side=Tk.LEFT,padx=5)
    MENU_RADIO_BOX(frame_geom,"Entites Geometriques",['Isovaleurs','Courbes'],self.stan.selection.Change_mode,fonte=fonte)
    self.geom = LIST_BOX(frame_geom,t_geom,Tk.EXTENDED,defaut = 'TOUT_MAILLAGE',fonte=fonte)

   # boite de saisie des numeros d'ordre
    frame_ordre  = Tk.Frame(frame_selection)
    frame_ordre.pack(side=Tk.LEFT,padx=5)
    MENU_RADIO_BOX(frame_ordre,"Ordres",l_va,self.stan.selection.Change_va,'NUME_ORDRE',fonte=fonte)
    self.ordre = LIST_BOX(frame_ordre,t_no,Tk.EXTENDED,defaut = 'TOUT_ORDRE',fonte=fonte)

   # Feu tricolore
    self.feu = FEU_TRICOLORE(frame_selection)

   # Boutons
    BOUTON(frame_boutons, 'PaleGreen1', 'TRACER'   , self.stan.Tracer,   x=2,y=0, fonte=fonte)
    self.case_sur_deformee = CASE_A_COCHER(frame_boutons, x=2, y=0, txt=_("Sur déformée"), fonte=fonte)
    BOUTON(frame_boutons, 'orange',     'CALCULER' , self.stan.Calculer, x=2,y=0, fonte=fonte)
    BOUTON(frame_boutons, 'skyblue',    'SELECTION', self.stan.Select,   x=2,y=0, fonte=fonte)
    BOUTON(frame_boutons, 'IndianRed1', 'SORTIR'   , self.stan.Quitter,  x=2,y=0, fonte=fonte)

   # Ligne d'etat
    self.ligne_etat = LIGNE_ETAT(frame_bas)
    self.ligne_etat.Affecter('Bienvenue dans STANLEY !')

   # Raccourcis clavier
    self.rootTk.bind_all("<Control-KeyPress>",self.stan.Clavier)    
        

  def Information(self) :
  
    global info
    
    tk = Tk.Tk()
    tk.title = _("A propos de Stanley")

    l = Tk.Label(tk,padx=15,pady=15,text=info)
    l.grid()
    

  def Bip(self) :
  
    """
      Emet un bip
    """
    self.rootTk.bell()


  def Requete_point(self) :

    """
      Boite de dialogue pour definir les caracteristiques
      du point a creer
      
      RETURN
        nom         nom du chemin
        x0,y0,z0    coordonnees du point origine
    """

    fonte = self.stan.parametres['fonte']
    infos = [
      ['Nom du point',1],
      ['Coordonnees',3]]
    defaut = [[''],[0.,0.,0.]]
    
    reponse = SAISIE(infos, _("Creation d'un point"), defaut, fonte=fonte)
    
    nom = reponse[0][0]
    nom = nom[0:8]            # pas plus de 8 caracteres dans un GROUP_MA
    nom = string.strip(nom)   # pas de blancs
    nom = string.upper(nom)   # en majuscules
    x0 = y0 = z0 = 0
    if reponse[1][0] : x0  = string.atof(reponse[1][0])
    if reponse[1][1] : y0  = string.atof(reponse[1][1])
    if reponse[1][2] : z0  = string.atof(reponse[1][2])

    return nom,x0,y0,z0
    

  def Requete_chemin(self) :

    """
      Boite de dialogue pour definir les caracteristiques
      du chemin a creer
      
      RETURN
        nom         nom du chemin
        x0,y0,z0    coordonnees du point origine
        x1,y1,z1    coordonnees du point extremite
        nbr         nombre de points
    """

    fonte = self.stan.parametres['fonte']
    infos = [
      ['Nom du chemin',1],
      ['Origine   (x,y,z)',3],
      ['Extremite (x,y,z)',3],
      ['Nombre de points',1]]
    defaut = [[''],[0.,0.,0.],[1.,0.,0.],[2]]
    reponse = SAISIE(infos, _("Creation d'un chemin") , defaut, fonte=fonte)
    
    nom = reponse[0][0]
    nom = nom[0:8]            # pas plus de 8 caracteres dans un GROUP_MA
    nom = string.strip(nom)   # pas de blancs
    nom = string.upper(nom)   # en majuscules
    x0 = y0 = z0 = x1 = y1 = z1 = 0
    if reponse[1][0] : x0  = string.atof(reponse[1][0])
    if reponse[1][1] : y0  = string.atof(reponse[1][1])
    if reponse[1][2] : z0  = string.atof(reponse[1][2])
    if reponse[2][0] : x1  = string.atof(reponse[2][0])
    if reponse[2][1] : y1  = string.atof(reponse[2][1])
    if reponse[2][2] : z1  = string.atof(reponse[2][2])
    nbr = string.atoi(reponse[3][0])

    return nom,x0,y0,z0,x1,y1,z1,nbr


# ==============================================================================

class DRIVER :

  """
    Driver d'outils de post-traitement

    Il y a deux specialisations : DRIVER_ISOVALEURS et DRIVER_COURBES pour definir
    les options communes des drivers d'isovaleurs et de courbes
    Specialisation a chaque outil par heritage de ces classes

    Methodes publiques
      Fermer : ferme le terminal graphique
      Tracer : Trace la selection (a enrichir dans chaque classe heritee)
      Projeter : Projection d'un champ aux noeuds sur un chemin ou un point (chemin degenere)
      Ecla_Gauss : Projection d'un cham_elem_elga aux points de Gauss
      Test_fichier_resu : En mode Validation, permet d'ecrire dans un fichier les md5 de tous les fichiers resultats produits
  """

  def __init__(self, stan) :

    # Gestion des erreurs
    self.erreur = ERREUR()

    self.terminal = None
    self.stan     = stan


  # ----------------------------------------------------------------------------
  def Fermer(self) :
    self.terminal.Fermer()    


  # ----------------------------------------------------------------------------
  def Tracer(self, selection) : pass


  # ----------------------------------------------------------------------------
  def Projeter(self, selection, contexte, geom) :

    """
      Projection d'un champ au noeud sur l'entite geometrique de nom geom.
      Pour l'instant, l'entite geometrique se reduit a un chemin sur lequel
      on affecte des elements barres ou a un discret.
      
      selection : selection courante de type SELECTION
      contexte  : CONTEXTE pour la projection
      geom      : nom du groupe de mailles sur lequel on projette
      
      retourne le CONTEXTE lie au nouveau resultat produit ainsi que la
      liste des concepts Aster a detruire.
    """

    # Pas de projection si meme maillage 
    __MA = selection.etat_geom.mail[geom]
    if __MA == contexte.maillage :
      return contexte, []

    if selection.geom[0] == 'POINT': type_modelisation = 'DIS_T'
    else:                            type_modelisation = 'BARRE'

    try:
      __MO_P = AFFE_MODELE(MAILLAGE = __MA,
                           AFFE = _F(GROUP_MA = selection.geom[1],
  #                                   TOUT         = 'OUI',
                                     PHENOMENE    = 'MECANIQUE',   # sans doute faire qchose de plus fin
                                     MODELISATION = type_modelisation,       # a ce niveau ...
                                    )
                          )
    except aster.error,err:
      return self.erreur.Remonte_Erreur(err, [], 2)
    except aster.FatalError,err:
      return self.erreur.Remonte_Erreur(err, [], 2)
    except Exception,err:
      texte = "Cette action n'est pas realisable.\n"+str(err)
      return self.erreur.Remonte_Erreur(err, [], 2, texte)

    motscles = { 'METHODE' : 'ELEM' }
    if contexte.para_sensi:
       motscles['SENSIBILITE'] = contexte.para_sensi

    try:
      __RESU_P = PROJ_CHAMP(
                            RESULTAT = contexte.resultat,
                            MODELE_1 = contexte.modele,
                            MODELE_2 = __MO_P,
                            NOM_CHAM = selection.nom_cham,
                            NUME_ORDRE = tuple(selection.numeros),
                            **motscles
                           )

    except aster.error,err:
      return self.erreur.Remonte_Erreur(err, [__MO_P], 2)
    except aster.FatalError,err:
      return self.erreur.Remonte_Erreur(err, [__MO_P], 2)
    except Exception,err:
      texte = "Cette action n'est pas realisable.\n"+str(err)
      return self.erreur.Remonte_Erreur(err, [__MO_P], 2, texte)

    return CONTEXTE(__RESU_P, __MA, __MO_P, None, None, contexte.para_sensi), [__MO_P, __RESU_P]


  # ----------------------------------------------------------------------------
  def Ecla_Gauss(self, selection, contexte, options={}) :

    """
      Projection aux points de Gauss (ECLA_PG)

      IN
        selection  selection courante de type SELECTION
        contexte   CONTEXTE du champ a eclater aux points de Gauss
        options    options supplementaires

      RETURN
        CONTEXTE lie au nouveau resultat produit
        liste des concepts Aster a detruire
    """

    if contexte.para_sensi:
      UTMESS('A','STANLEY', _("La visualisation aux points de Gauss n'est pas permise avec la sensibilité") )
      return False, []

    if selection.nom_cham not in ['SIEF_ELGA','VARI_ELGA','SIEF_ELGA_TEMP','FLUX_ELGA_TEMP']:
      UTMESS('A','STANLEY',SELECTION.NonDeveloppePG)
      return False, []

    if   contexte.resultat.__class__ == evol_elas  : type_resu = 'EVOL_ELAS'
    elif contexte.resultat.__class__ == evol_ther  : type_resu = 'EVOL_THER'
    elif contexte.resultat.__class__ == evol_noli  : type_resu = 'EVOL_NOLI'
    elif contexte.resultat.__class__ == dyna_trans : type_resu = 'DYNA_TRANS'
    elif contexte.resultat.__class__ == dyna_harmo : type_resu = 'DYNA_HARMO'
    elif contexte.resultat.__class__ == mode_meca  : type_resu = 'MODE_MECA'
    else :
      UTMESS('A','STANLEY',SELECTION.NondeveloppeRS)
      return False, []

    para = _F(  
      MODELE     = contexte.modele,
      NOM_CHAM   = selection.nom_cham,
      SHRINK     = float(self.stan.parametres['SHRINK']),
      TAILLE_MIN = float(self.stan.parametres['TAILLE_MIN']),
      )

    if selection.geom[0] in ['VOLUME','SURFACE'] :
      para['GROUP_MA'] = selection.geom[1]

    try:
      __MA_G = CREA_MAILLAGE(MAILLAGE = contexte.maillage,
                             ECLA_PG  = para,
                            )
    except aster.error,err:
      return self.erreur.Remonte_Erreur(err, [], 2)
    except aster.FatalError,err:
      return self.erreur.Remonte_Erreur(err, [], 2)
    except Exception,err:
      texte = "Cette action n'est pas realisable.\n"+str(err)
      return self.erreur.Remonte_Erreur(err, [], 2, texte)


    para = _F(
      MAILLAGE    = __MA_G,
      MODELE_INIT = contexte.modele,
      RESU_INIT   = contexte.resultat,
      NOM_CHAM    = selection.nom_cham,
      NUME_ORDRE  = selection.numeros
      )

    if selection.geom[0] in ['VOLUME','SURFACE'] :
      para['GROUP_MA'] = selection.geom[1]

    try:
      __RESU_G = CREA_RESU(
        OPERATION   = 'ECLA_PG',
        TYPE_RESU   = type_resu,
        ECLA_PG     = para,)
    except aster.error,err:
      return self.erreur.Remonte_Erreur(err, [__MA_G], 2)
    except aster.FatalError,err:
      return self.erreur.Remonte_Erreur(err, [__MA_G], 2)
    except Exception,err:
      texte = "Cette action n'est pas realisable.\n"+str(err)
      return self.erreur.Remonte_Erreur(err, [__MA_G], 2, texte)

    if selection.geom[0] == 'MAILLAGE':
        __MO_G   = copy.copy(contexte.modele)
        ldetr = [__MA_G, __RESU_G]

    else:
        if   selection.geom[0] == 'VOLUME':  pmod = '3D'
        else:                                pmod = 'D_PLAN'

        try:
           __MO_G = AFFE_MODELE(
              MAILLAGE = __MA_G,
              AFFE     = _F( TOUT = 'OUI',
                             PHENOMENE = 'MECANIQUE',
                             MODELISATION = pmod,))
        except aster.error,err:
          return self.erreur.Remonte_Erreur(err, [__MA_G, __RESU_G], 2)
        except aster.FatalError,err:
          return self.erreur.Remonte_Erreur(err, [__MA_G, __RESU_G], 2)
        except Exception,err:
          texte = "Cette action n'est pas realisable.\n"+str(err)
          return self.erreur.Remonte_Erreur(err, [__MA_G, __RESU_G], 2, texte)


        ldetr = [__MA_G, __MO_G, __RESU_G]

    return CONTEXTE(__RESU_G, __MA_G, __MO_G, None, None, None), ldetr


  # ----------------------------------------------------------------------------
  def Test_fichier_resu(self, driver, FICHIER, FICHIER_VALID, selection):
    """
       Permet de generer le md5 pour verifier la conformité du fichier de sortie
    """

    # Type de trace
    if   driver == 'Isovaleurs':
       NB_CHIFFRE = 4
       EPSILON = 10E-10
       regexp_ignore = []
    elif driver == 'Courbes':
       NB_CHIFFRE = 4
       EPSILON = 10E-10
       regexp_ignore=[]
    elif driver == 'SalomeCourbes':
       NB_CHIFFRE = 4
       EPSILON = 10E-10
       regexp_ignore=[]
       return
    elif driver == 'SalomeIsovaleurs':
       NB_CHIFFRE = 4
       EPSILON = 10E-10
       regexp_ignore=[]
       return
    else:
       return

    try:
       # calcule le md5sum du fichier
       ier, mdsum = md5file(FICHIER, NB_CHIFFRE, EPSILON, regexp_ignore, info=1)
       # Affichage de la ligne
       txt = mdsum + ' - ' + FICHIER + ' - ' + ' - '.join( [str(selection.nom_cham), str(selection.nom_cmp), str(selection.numeros), str(selection.geom) ] )
    except Exception,err:
       texte = _("Probleme")+" :\n"+str(err)
       UTMESS('A','STANLEY', texte )

    try:
       f=open(self.stan.FICHIER_VALID, 'a')
       f.write(txt+'\n')
       f.close()
    except:
       UTMESS('A','STANLEY', _("Impossible d'ouvrir en ecriture le fichier : " + self.stan.FICHIER_VALID) )

    return


# ==============================================================================

class DRIVER_ISOVALEURS(DRIVER):

  """
    Driver d'outils de post-traitement
    Specialisation a chaque outil par heritage de cette classe
    Specialisation pour les drivers d'isovaleurs

    Methodes publiques
      Options_Impr_Resu : defini le dictionnaire des options de la commande IMPR_RESU
  """

  def Options_Impr_Resu(self, contexte, selection, options={}):

    """
      Options d'IMPR_RESU communes à tous les drivers d'isovaleurs

      IN
        contexte   CONTEXTE du champ
        selection  selection courante de type SELECTION
        options    options supplementaires

      RETURN
        para       dictionnaire des parametres pour IMPR_RESU
    """

    type_champ = cata[selection.nom_cham].type

    # Options de base
    para = _F(RESULTAT   = contexte.resultat,
              NOM_CHAM   = selection.nom_cham,
              NUME_ORDRE = selection.numeros, 
             )

    if type_champ in ['NOEU','ELNO'] and selection.geom[0] in ['VOLUME','SURFACE'] :
      para['GROUP_MA'] = selection.geom[1]    # non actif a cause de IMPR_RESU

    if 'TOUT_CMP' not in selection.nom_cmp :
      para['NOM_CMP'] = tuple(selection.nom_cmp)

    # Options supplementaires du IMPR_RESU pour la SENSIBILITE
    if contexte.para_sensi:
      para['SENSIBILITE'] = contexte.para_sensi

    # Options supplementaires du IMPR_RESU pour le tracé sur deformée
    if options.has_key( 'case_sur_deformee' ):
      if options['case_sur_deformee'] == 1:
        if selection.nom_cham != 'DEPL':
          if type_champ in ['ELGA', 'ELEM']:
             UTMESS('A','STANLEY',"Attention : on ne peut pas tracer un champs aux points de Gauss sur la deformee...")
          else:
             UTMESS('I','STANLEY',"Le champ est trace avec la deformee")
             if selection.nom_cham != 'DEPL':
                para0 = _F(RESULTAT   = contexte.resultat,
                           NOM_CHAM   = 'DEPL',
                           NUME_ORDRE = selection.numeros,)
                para = [ para, para0 ]

    return para



# ==============================================================================

class DRIVER_GMSH(DRIVER_ISOVALEURS):

  """
    Driver d'outils de post-traitement
    Specialisation pour GMSH

    Methodes publiques
      Tracer : genere le fichier de post-traitement (IMPR_RESU) et lance GMSH
  """

  def Tracer(self, selection, options={}) :

    if self.terminal : self.terminal.Fermer()       # un seul terminal GMSH ouvert en meme temps
    l_detr = []

    # Unite logique du fichier pour GMSH
    ul = 33

    # Nom du fichier
    gmshFileName = 'fort.'+str(ul)

    contexte   = self.stan.contexte
    type_champ = cata[selection.nom_cham].type

    if type_champ == 'ELGA':
      contexte, l_detr = self.Ecla_Gauss(selection, contexte, options)
      if not contexte: return

    # Parametres de la commande IMPR_RESU
    para = self.Options_Impr_Resu(contexte, selection, options)

    # Tracé au format GMSH
    DEFI_FICHIER(UNITE = 33, INFO=1)
    try:
      IMPR_RESU( UNITE   = ul, 
                 FORMAT  = 'GMSH',
                 VERSION = eval(self.stan.parametres['version_fichier_gmsh']),
                 RESU    = para,
               )
    except aster.error,err:
      self.erreur.Remonte_Erreur(err, [], 0)
      DEFI_FICHIER(ACTION='LIBERER', UNITE=ul, INFO=1)
      return
    except aster.FatalError,err:
      self.erreur.Remonte_Erreur(err, [], 0)
      DEFI_FICHIER(ACTION='LIBERER', UNITE=ul, INFO=1)
      return
    except Exception,err:
      texte = "Cette action n'est pas realisable.\n"+str(err)
      self.erreur.Remonte_Erreur(err, [], 0, texte)
      DEFI_FICHIER(ACTION='LIBERER', UNITE=ul, INFO=1)
      return

    DEFI_FICHIER(ACTION='LIBERER', UNITE=ul, INFO=1)


    # Ecriture du fichier de validation
    if self.stan.FICHIER_VALID:
       self.Test_fichier_resu(driver=selection.mode, FICHIER='fort.'+str(ul), FICHIER_VALID=self.stan.FICHIER_VALID, selection=selection)


    if l_detr : 
       DETRUIRE(CONCEPT = _F(NOM = tuple(l_detr)), INFO=1, ALARME='NON')


    self.terminal = gmsh.GMSH('POST', gmshFileName, self.stan.parametres, options)



# ==============================================================================

class DRIVER_SALOME_ISOVALEURS(DRIVER_ISOVALEURS) :

  """
    Driver d'outils de post-traitement
    Specialisation pour GMSH

    Methodes publiques
      Tracer : genere le fichier de post-traitement (IMPR_RESU) et lance GMSH
  """

##    def __init__( self, stan ):    
##          DRIVER.__init__( self, stan )

  def Tracer( self, selection, options={} ) :

    if self.terminal : self.terminal.Fermer()
    l_detr = []

    # Unite logique pour le fichier MED
    ul = 80

    # Nom du fichier
    medFileName = 'fort.'+str(ul)

    contexte   = self.stan.contexte
    type_champ = cata[selection.nom_cham].type
        
    if type_champ == 'ELGA' :
      contexte, l_detr = self.Ecla_Gauss(selection, contexte)
      if not contexte: return

    # On efface le fichier si il existe deja
    if os.path.isfile('fort.'+str(ul)):
      try:    os.remove('fort.'+str(ul))
      except: pass

    DEFI_FICHIER(UNITE = ul, 
                 TYPE  = 'LIBRE',);

    # Parametres de la commande IMPR_RESU
    para = self.Options_Impr_Resu(contexte, selection, options)

    try:
        IMPR_RESU(  FORMAT = 'MED',
                    UNITE  = ul,
                    RESU   = para )                        
    except aster.error,err:
        self.erreur.Remonte_Erreur(err, [], 0)
        DEFI_FICHIER(ACTION='LIBERER',UNITE=ul)
        return
    except aster.FatalError,err:
        self.erreur.Remonte_Erreur(err, [], 0)
        DEFI_FICHIER(ACTION='LIBERER',UNITE=ul)
        return
    except Exception,err:
        texte = "Cette action n'est pas realisable.\n"+str(err)
        self.erreur.Remonte_Erreur(err, [], 0, texte)
        DEFI_FICHIER(ACTION='LIBERER',UNITE=ul)
        return
                          
    DEFI_FICHIER(ACTION='LIBERER',UNITE=ul)
        
    if l_detr :
        DETRUIRE(CONCEPT = _F(NOM = tuple(l_detr)), INFO=1)
        
                           
    self.terminal = salomeVisu.ISOVALEURS( medFileName, self.stan.parametres, selection )



# ==============================================================================

class DRIVER_COURBES(DRIVER) :

  """
    Driver d'outils de post-traitement
    Specialisation a chaque outil par heritage de cette classe
    Specialisation pour les drivers de tracer de courbes
    
    Methodes publiques
      Extract : extrait la table contenant les valeurs a tracer
  """

  def Options_Post_Releve_T(self, contexte, selection, options={}): pass


  def Extract(self, selection) :

    """
      Execute les commandes aster de creation de la table pour GRACE
    """  

    l_courbes = []
    l_detr    = []

    contexte   = self.stan.contexte
    type_champ = cata[selection.nom_cham].type

    if type_champ == 'ELGA' :
      contexte, l_detr = self.Ecla_Gauss(selection, contexte)
      if not contexte: return False

    # Parametres communs a toutes les tables a calculer 
    para = _F(INTITULE   = 'TB_GRACE',
              NOM_CHAM   = selection.nom_cham,
              OPERATION  = 'EXTRACTION'
             )

    if 'TOUT_CMP' in selection.nom_cmp :
      para['TOUT_CMP'] = 'OUI'
      l_nom_cmp = self.stan.etat_resu.cmp[selection.nom_cham]
    else :
      para['NOM_CMP'] = tuple(selection.nom_cmp)
      l_nom_cmp = selection.nom_cmp


    # Options supplementaires du IMPR_RESU pour la SENSIBILITE
    if contexte.para_sensi:
       para['SENSIBILITE'] = contexte.para_sensi

       DETRUIRE(CONCEPT = _F(NOM = 'STNTBLG2'),INFO=1, ALARME='NON')

       try:
          MEMO_NOM_SENSI(NOM=( _F(NOM_SD='STNTBLGR',
                                  PARA_SENSI=contexte.para_sensi,
                                  NOM_COMPOSE='STNTBLG2')));
       except: pass

    DETRUIRE(CONCEPT = _F(NOM = 'STNTBLGR'),INFO=1, ALARME='NON')

    if selection.geom[0] == 'POINT' :

      para['NUME_ORDRE'] = selection.numeros
      for point in selection.geom[1] :
        contexte, detr = self.Projeter(selection, contexte, point) 
        if not contexte: return False

        l_detr += detr
        para['RESULTAT'] = contexte.resultat
        para['GROUP_NO'] = point

        try:
          STNTBLGR = POST_RELEVE_T(ACTION = para)
        except aster.error,err:
          return self.erreur.Remonte_Erreur(err, ['STNTBLGR'], 1)
        except aster.FatalError,err:
          return self.erreur.Remonte_Erreur(err, ['STNTBLGR'], 1)
        except Exception,err:
          texte = "Cette action n'est pas realisable.\n"+str(err)
          return self.erreur.Remonte_Erreur(err, ['STNTBLGR'], 1, texte)

        for comp in l_nom_cmp :

          vale_x = selection.vale_va
          courbe = as_courbes.Courbe(vale_x,vale_x)

          # Sensibilite
          if contexte.para_sensi:
             table_sensible_jeveux = table_jeveux( Sensibilite.NomCompose(STNTBLGR, contexte.para_sensi) )
             courbe.Lire_y(table_sensible_jeveux, comp)
             nom = comp + ' - ' + contexte.para_sensi.nom + ' --- ' + string.ljust(point,8)
          else:
             courbe.Lire_y(STNTBLGR,comp)
             nom = comp + ' --- ' + string.ljust(point,8)

          l_courbes.append( (courbe, nom) )

        DETRUIRE(CONCEPT = _F(NOM = 'STNTBLGR'),INFO=1, ALARME='NON')
        if contexte.para_sensi: DETRUIRE(CONCEPT = _F(NOM = Sensibilite.NomCompose(STNTBLGR, contexte.para_sensi)),INFO=1, ALARME='NON')
        if l_detr: DETRUIRE(CONCEPT = _F(NOM = tuple(l_detr) ),INFO=1, ALARME='NON')


    elif selection.geom[0] == 'CHEMIN' :

      chemin = selection.geom[1][0]

      # Projection si necessaire
      contexte, detr = self.Projeter(selection, contexte, chemin)
      if not contexte: return False
      l_detr += detr    

      para['RESULTAT'] = contexte.resultat
      para['GROUP_NO'] = self.stan.etat_geom.Oriente(chemin)

      for no, va in map(lambda x,y : (x,y), selection.numeros, selection.vale_va) :
        para['NUME_ORDRE'] = no,
        try:
          STNTBLGR = POST_RELEVE_T(ACTION = para)
        except aster.error,err:
          return self.erreur.Remonte_Erreur(err, ['STNTBLGR'], 1)
        except aster.FatalError,err:
          return self.erreur.Remonte_Erreur(err, ['STNTBLGR'], 1)
        except Exception,err:
          texte = "Cette action n'est pas realisable.\n"+str(err)
          return self.erreur.Remonte_Erreur(err, ['STNTBLGR'], 1, texte)


        for comp in l_nom_cmp :
          courbe = as_courbes.Courbe()

          # Sensibilite
          if contexte.para_sensi:
             table_sensible_jeveux = table_jeveux( Sensibilite.NomCompose(STNTBLGR, contexte.para_sensi) )
             courbe.Lire_x(table_sensible_jeveux, 'ABSC_CURV')
             courbe.Lire_y(table_sensible_jeveux, comp)
             nom = comp + ' - ' + contexte.para_sensi.nom + ' --- ' + selection.nom_va + ' = ' + repr(va)
#             nom = comp + ' --- ' + selection.nom_va + ' = ' + repr(va)
          else:
             courbe.Lire_x(STNTBLGR, 'ABSC_CURV')
             courbe.Lire_y(STNTBLGR, comp)
             nom = comp + ' --- ' + selection.nom_va + ' = ' + repr(va)

          l_courbes.append( (courbe, nom) )

        DETRUIRE(CONCEPT = _F(NOM = 'STNTBLGR'),INFO=1, ALARME='NON')
        if contexte.para_sensi: DETRUIRE(CONCEPT = _F(NOM = Sensibilite.NomCompose(STNTBLGR, contexte.para_sensi)),INFO=1, ALARME='NON')

      if l_detr: DETRUIRE(CONCEPT = _F(NOM = tuple(l_detr) ), INFO=1, ALARME='NON')

    else:
      UTMESS('A','STANLEY',_("Cette action n'est pas realisable.") )
      return False

    return l_courbes



# ==============================================================================

class DRIVER_GRACE(DRIVER_COURBES) :

  def Tracer(self, selection, options={}) :

    # Extraction des resultats pour la selection requise
    l_courbes = self.Extract(selection)
    if not l_courbes: return

    # Windows : utilisation de IMPR_COURBE car ne supporte pas les pipes avec xmgrace
    if sys.platform == 'win32':

      if selection.geom[0] == 'POINT' :
        _x = selection.nom_va
        _y = selection.nom_cham
      elif selection.geom[0] == 'CHEMIN' :
        _x = 'ABSC_CURV ' + selection.geom[1][0]
        _y = selection.nom_cham
      else:
        _x = ''
        _y = ''

      _tmp  = []
      for courbe in l_courbes :
        acourbe = []
        for l in string.split(repr(courbe[0])):
          acourbe.append( float(l) )
        lx = acourbe[0:len(acourbe):2]
        ly = acourbe[1:len(acourbe):2]
      
        txt = { 'ABSCISSE': lx, 'ORDONNEE': ly, 'COULEUR': 2 }
        _tmp.append( txt )

      motscle2= {'COURBE': _tmp }

      IMPR_FONCTION(FORMAT='XMGRACE',
                    UNITE=53,
                    PILOTE='INTERACTIF',
#                    TITRE='Titre',
#                    SOUS_TITRE='Sous-titre',
                    LEGENDE_X=_x,
                    LEGENDE_Y=_y,
                    **motscle2
                   )

    # Unix/Linux
    else:
      # Ouverture ou rafraichissement du terminal si necessaire
      xmgrace_exe=self.stan.parametres['grace']
      if not xmgrace_exe.strip(): xmgrace_exe = aster.repout() + '/xmgrace'
  
      if not self.terminal : 
         self.terminal = xmgrace.Xmgr(xmgrace=xmgrace_exe)
      else :
         if not self.terminal.Terminal_ouvert() :
            self.terminal.Fermer()
            self.terminal = xmgrace.Xmgr(xmgrace=xmgrace_exe)

      # Trace proprement dit 
      self.terminal.Nouveau_graphe()

      for courbe in l_courbes :
         self.terminal.Courbe(courbe[0],courbe[1])

      if selection.geom[0] == 'POINT' :
         self.terminal.Axe_x(selection.nom_va)
         self.terminal.Axe_y(selection.nom_cham)
      elif selection.geom[0] == 'CHEMIN' :
         self.terminal.Axe_x('ABSC_CURV ' + selection.geom[1][0])
         self.terminal.Axe_y(selection.nom_cham)

      # Ecriture du fichier de validation
      if self.stan.FICHIER_VALID:
         for fic in os.listdir('.'):
            # Ne prend que les fichiers xmgr.i.j.dat
            if (fic[0:5] == 'xmgr.') and (fic[-4:] == '.dat'):
               self.Test_fichier_resu(driver=selection.mode, FICHIER=fic, FICHIER_VALID=self.stan.FICHIER_VALID, selection=selection)



# ==============================================================================

class DRIVER_SALOME_COURBES(DRIVER_COURBES) :

    def Tracer(self, selection, options={}) :

       if self.terminal : self.terminal.Fermer()
       # Extraction des resultats pour la selection requise
       l_courbes = self.Extract( selection )
       if not l_courbes: return
       self.terminal = salomeVisu.COURBES( l_courbes, self.stan.parametres, selection )





# ==============================================================================

class DRIVER_SUP_GMSH(DRIVER) :

  """
    Interface avec le langage superviseur de GMSH
  """

  
  # ----------------------------------------------------------------------------
  def Importer_point (self,nom,x0,y0,z0) : 

    """ 
      Importe un point
      
      RETURN
        geom : nouvelle entite geometrique (ETAT_GEOM)
    """

    global _MA, _NUM

   # Creation du maillage du point et de la ligne (pour proj_champ)    
    nom_bid = '_'+nom
    nom_bid = nom_bid[:8]
    eps = 1.E-2
    P0  = sup_gmsh.Point(x0,y0,z0)
    P1  = sup_gmsh.Point(x0*(1+eps)+eps,y0*(1+eps)+eps,z0)
    L0  = sup_gmsh.Line(P0,P1)
    L0.Transfinite(1)
    mesh = sup_gmsh.Mesh(gmsh=self.stan.parametres['gmsh'])
    mesh.Physical(nom,P0)
    mesh.Physical(nom_bid,L0)

    try:
      ma = mesh.LIRE_GMSH(UNITE_GMSH=_UL[0],UNITE_MAILLAGE=_UL[1])
    except aster.error,err:
      return self.erreur.Remonte_Erreur(err, [ma], 1)
    except aster.FatalError,err:
      return self.erreur.Remonte_Erreur(err, [ma], 1)
    except Exception,err:
      texte = "Cette action n'est pas realisable.\n"+str(err)
      return self.erreur.Remonte_Erreur(err, [ma], 1, texte)


    INDICE=_NUM

    try:
      _MA[INDICE] = CREA_MAILLAGE(MAILLAGE = ma,)
    except aster.error,err:
      return self.erreur.Remonte_Erreur(err, [ma, _MA[INDICE]], 1)
    except aster.FatalError,err:
      return self.erreur.Remonte_Erreur(err, [ma, _MA[INDICE]], 1)
    except Exception,err:
      texte = "Cette action n'est pas realisable.\n"+str(err)
      return self.erreur.Remonte_Erreur(err, [ma, _MA[INDICE]], 1, texte)

    DETRUIRE(CONCEPT = _F(NOM = ma), INFO=1, ALARME='NON')
    _NUM = _NUM + 1

    return ETAT_GEOM(_MA[INDICE])


  # ----------------------------------------------------------------------------
  def Importer_chemin (self,nom,x0,y0,z0,x1,y1,z1,nbr) : 

    """ 
      Importe un chemin
      
      RETURN
        geom : nouvelle entite geometrique (ETAT_GEOM)
    """

    global _MA, _NUM

   # Creation du maillage de la ligne    
    P0  = sup_gmsh.Point(x0,y0,z0)
    P1  = sup_gmsh.Point(x1,y1,z1)
    L01 = sup_gmsh.Line(P0,P1)
    L01.Transfinite(nbr-1)
    mesh = sup_gmsh.Mesh(gmsh=self.stan.parametres['gmsh'])
    mesh.Physical(nom,L01)    
    try:
      ma = mesh.LIRE_GMSH(UNITE_GMSH=_UL[0],UNITE_MAILLAGE=_UL[1])
    except aster.error,err:
      return self.erreur.Remonte_Erreur(err, [ma], 1)
    except aster.FatalError,err:
      return self.erreur.Remonte_Erreur(err, [ma], 1)
    except Exception,err:
      texte = "Cette action n'est pas realisable.\n"+str(err)
      return self.erreur.Remonte_Erreur(err, [ma], 1, texte)

    INDICE=_NUM

    try:
      _MA[INDICE] = CREA_MAILLAGE(MAILLAGE = ma,)
    except aster.error,err:
      return self.erreur.Remonte_Erreur(err, [ma, _MA[INDICE]], 1)
    except aster.FatalError,err:
      return self.erreur.Remonte_Erreur(err, [ma, _MA[INDICE]], 1)
    except Exception,err:
      texte = "Cette action n'est pas realisable.\n"+str(err)
      return self.erreur.Remonte_Erreur(err, [ma, _MA[INDICE]], 1, texte)

    DETRUIRE(CONCEPT = _F(NOM = ma), INFO=1, ALARME='NON')
    _NUM = _NUM + 1

    return ETAT_GEOM(_MA[INDICE])


# ==============================================================================

class PRE_STANLEY :

  """
    INTERFACE GRAPHIQUE TK DE SELECTION DES CONCEPTS ASTER
    
     Attributs publics
      frame_maillage      : LIST_BOX champ selection des concepts Aster de type "maillage"
      frame_modele        : LIST_BOX champ selection des concepts Aster de type "modele"
      frame_evol          : LIST_BOX champ selection des concepts Aster de type "evol_*"
      frame_cham_mater    : LIST_BOX champ selection des concepts Aster de type "cham_mater"
      frame_cara_elem     : LIST_BOX champ selection des concepts Aster de type "cara_elem"
      frame_para_sensi    : LIST_BOX champ selection des concepts Aster de type "para_sensi"

     Methode publique 
      Exec       : lancement du scan des evenements
      
     Attributs prives
      rootTk     : racine des objets Tk
      
     Methodes privees
      Action_evenement : actions consecutives au scan des evenements
      Dessin     : creation des objets graphiques Tk de l'interface  
  """


  def __init__(self, FICHIER_VALID=None) :   


    self.FICHIER_VALID = FICHIER_VALID
    self.para = PARAMETRES()

    self.rootTk    = Tk.Tk()       
    self.rootTk.wm_title( _("PRE_STANLEY : choix des concepts") )

    # Récupération des concepts Aster présents dans la base
    self.jdc_recupere=CONTEXT.get_current_step().jdc
    t_maillage=[]
    t_modele=[]
    t_evol=[]
    t_cham_mater=[]
    t_cara_elem=[]
    t_para_sensi=[]

    lst = [ 'maillage_sdaster', 'modele_sdaster', 'evol_elas', 'evol_noli', 'evol_ther', 'mode_meca', 'dyna_harmo', 'dyna_trans', 'cham_mater', 'cara_elem_sdaster', 'para_sensi' ]

    for i in self.jdc_recupere.sds_dict.keys( ):

      # On supprime de la liste les concept issus de la sensibilite
      if i[0:len(ignore_prefixe)] == ignore_prefixe :
        if self.jdc_recupere.sds_dict[i].__class__.__name__ in lst:
          txt = _('Concept ignore : ') + i
          UTMESS('I','STANLEY',txt)

      else:

#         print i,self.jdc_recupere.sds_dict[i].__class__.__name__,self.jdc_recupere.sds_dict[i]
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'maillage_sdaster':
          t_maillage.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'modele_sdaster':
          t_modele.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'evol_elas':
          t_evol.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'evol_noli':
          t_evol.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'evol_ther':
          t_evol.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'mode_meca':
          t_evol.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'dyna_harmo':
          t_evol.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'dyna_trans':
          t_evol.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'cham_mater':
          t_cham_mater.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'cara_elem_sdaster':
          t_cara_elem.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'para_sensi':
          t_para_sensi.append( i )

    self.t_maillage=t_maillage
    self.t_modele=t_modele
    self.t_evol=t_evol
    self.t_cham_mater=t_cham_mater
    self.t_cara_elem=t_cara_elem
    self.t_para_sensi=t_para_sensi

    self.t_maillage.sort()
    self.t_modele.sort()
    self.t_evol.sort()
    self.t_cham_mater.sort()
    self.t_cara_elem.sort()
    self.t_para_sensi.sort()

    # Si un des concepts n'a pas été trouvé au moins une fois on arrete
    _lst = []
    if len(t_maillage) ==0:   _lst.append('MAILLAGE')
    if len(t_modele) ==0:     _lst.append('MODELE')
    if len(t_evol) ==0:       _lst.append('EVOL_ELAS ou EVOL_NOLI ou EVOL_THER ou DYNA_TRANS ou DYNA_HARMO ou MODE_MECA')
    if len(t_cham_mater) ==0: _lst.append('CHAM_MATER')
    if len(_lst) > 0:
      txt = """Tous les concepts Aster necessaires à Stanley n'ont
               pas été calculés. Il manque :
"""
      for l in _lst:
        txt = txt +  '               - ' + l + '\n'
      UTMESS('A','STANLEY',txt)
      self.Sortir()


    else:

      # Detecte les concepts associés a chaque resultat
      self.concepts = self.Autodetecte_Concepts(self.t_evol)

      # Detecte les parametres sensibles associés a chaque resultat
      self.dico_para_sensi = self.Autodetecte_Para_Sensi(self.t_evol, self.t_para_sensi)

      # Sinon on continue
      self.Dessin()
      self.Scan_selection()               

      # Preselectionne le dernier resultat de la liste et ses concepts correspondants
      self.Change_selections()

      self.Exec()



  def Autodetecte_Concepts(self, t_evol) :
    """ 
       Detecte les concepts associés à un resultat
    """

    dico = {}

    liste_concept = [ 'MODL', 'MATE', 'CARA' ]
    dico_concept  = { 'MODL': _("modele"), 'MATE': _("Champ matériau"), 'CARA': _("Cara_elem") }

    for evol in t_evol:
       dico[evol] = []

       for concept in liste_concept:
          ltmp_aster = aster.getvectjev( evol.ljust(19) + '.'+ concept )
          l_tmp = []
          if ltmp_aster:
             for i in range(len(ltmp_aster)):
                obj = ltmp_aster[i].strip()
                if obj and obj not in l_tmp: l_tmp.append(obj)

          if len(l_tmp)==1:
             dico[evol].append( l_tmp[0] )

          elif len(l_tmp)>1:
             UTMESS('A','STANLEY',_("Il y a plusieurs concepts ") + dico_concept[concept] + _(" stockés dans la structure de donnée résultat.") )
             dico[evol].append( None )
          else:
             dico[evol].append( None )

    return dico



  def Autodetecte_Para_Sensi(self, t_evol, t_para_sensi) :
    """ 
       Detecte les parametres sensibles d'un resultat
    """

    dico = {}

    for evol in t_evol:
       dico[evol] = []

       for para_sensi in t_para_sensi:
          resu_sensible = resultat_jeveux( Sensibilite.NomCompose(evol, para_sensi) )
          if resu_sensible.nom:
             test_existance = aster.getvectjev( resu_sensible.nom.ljust(19) + '.DESC' )
             if test_existance:
                dico[evol].append( para_sensi )

    return dico



  def Exec(self) :
    """ 
      Demarre le scan des evenements
    """
    
    self.rootTk.mainloop()



  def Change_selections(self) :
    """
      Action sur les evenements
        Selectionne les concepts modele, cham_mater et cara_elem du resultat courant
        Definit la frequence de scan
    """
    try:
       evol = self.evol.courant[0]
       modele = self.concepts[evol][0]
       self.modele.Selectionne( modele )
       cham_mater = self.concepts[evol][1]
       self.cham_mater.Selectionne( cham_mater )
       if self.t_cara_elem != []:
          cara_elem = self.concepts[evol][2]
          self.cara_elem.Selectionne( cara_elem )
       if self.t_para_sensi != []:
          if len(self.dico_para_sensi[evol])>0:
             para = self.dico_para_sensi[evol][0]
             t_para = copy.copy(self.dico_para_sensi[evol])
             t_para.insert(0, texte_sensibilite)
             self.para_sensi.Change( t_para, para )
          else:
             para = self.t_para_sensi[0]
             t_para = copy.copy(self.t_para_sensi)
             t_para.insert(0, texte_sensibilite)
             self.para_sensi.Change( t_para, para )
    except: pass


  def Scan_selection(self) :
    """
      Action sur les evenements
        Scan les objets selectionnes
        Definit la frequence de scan
    """

    # Si le resultat a changé on reselectionne les autres concepts
    if self.evol.Scan():
       try:    self.Change_selections()
       except: pass

    self.after_id = self.rootTk.after(30, self.Scan_selection)


  def Lancer(self) :

     i=int(self.modele.listbox.curselection()[0])
     modele=self.t_modele[i]
     _maillag = aster.getvectjev( string.ljust(modele,8) + '.MODELE    .NOMA        ' )
     maillage = string.rstrip( _maillag[0] )

     i=int(self.evol.listbox.curselection()[0])
     evol=self.t_evol[i]

     i=int(self.cham_mater.listbox.curselection()[0])
     cham_mater=self.t_cham_mater[i]

     if self.t_cara_elem == []:
        cara_elem=None
        c_cara_elem=None
     else:
        i=int(self.cara_elem.listbox.curselection()[0])
        cara_elem=self.t_cara_elem[i]
        c_cara_elem = self.jdc_recupere.sds_dict[cara_elem]

     if self.t_para_sensi == []:
        para_sensi=None
        c_para_sensi=None
     else:
        i=int(self.para_sensi.listbox.curselection()[0])
        if i == 0:
           para_sensi=None
           c_para_sensi=None
        else:
           para_sensi=self.t_para_sensi[i-1]
           c_para_sensi = self.jdc_recupere.sds_dict[para_sensi]

     self.Sortir()


     # Ouvre le fichier de validation
     if self.FICHIER_VALID:
        nom_cas_test = self.jdc_recupere.fico
        if not nom_cas_test: nom_cas_test='(pas de nom de cas-test)'

        date = time.localtime()
        txt = 50*'-'+'\n'+str(date[2])+'/'+str(date[1])+'/'+str(date[0])+' - '+str(date[3])+':'+str(date[4])+'\n'
        txt += nom_cas_test + ' [' + ' / '.join([str(evol), str(modele), str(cham_mater), str(cara_elem), str(para_sensi)]) + ' ]\n'
        try:
           f=open(self.FICHIER_VALID, 'a')
           f.write(txt)
           f.close()
        except:
           UTMESS('A','STANLEY', _("Impossible d'ouvrir en ecriture le fichier : " + self.FICHIER_VALID) )
           self.FICHIER_VALID = None

     # Lancement de Stanley
     STANLEY(self.jdc_recupere.sds_dict[evol], self.jdc_recupere.sds_dict[maillage], self.jdc_recupere.sds_dict[modele], self.jdc_recupere.sds_dict[cham_mater], c_cara_elem, c_para_sensi, self.FICHIER_VALID)


  def Sortir(self):
    """
      Sortir proprement de l'interface
    """
    try:    self.rootTk.after_cancel(self.after_id)
    except: pass
    self.rootTk.destroy()


  def Dessin(self):
    """
      Creation de tous les objets graphiques de l'interface
    """

    fonte = self.para['fonte']
        
   # Frames generales
    frame_selection  = Tk.Frame(self.rootTk,relief=Tk.RAISED,bd=3)
    frame_selection.pack(padx=2,pady=2)
    frame_bas = Tk.Frame(self.rootTk)
    frame_bas.pack(pady=10)
    frame_boutons  = Tk.Frame(frame_bas,relief=Tk.RAISED,bd=1)
    frame_boutons.pack(side=Tk.LEFT,pady=10)

   # boite de saisie des champs
    frame_evol = Tk.Frame(frame_selection)
    frame_evol.pack(side=Tk.LEFT,padx=5)
    MENU_RADIO_BOX(frame_evol, "evol",fonte=fonte)
    self.evol = LIST_BOX(frame_evol,self.t_evol,Tk.SINGLE,self.t_evol[-1],fonte=fonte)
      
   # boite de saisie des champs
    frame_modele = Tk.Frame(frame_selection)
    frame_modele.pack(side=Tk.LEFT,padx=5)
    MENU_RADIO_BOX(frame_modele, "modele",fonte=fonte)
    self.modele = LIST_BOX(frame_modele,self.t_modele,Tk.SINGLE,self.t_modele[-1],fonte=fonte)

   # boite de saisie des champs
    frame_cham_mater = Tk.Frame(frame_selection)
    frame_cham_mater.pack(side=Tk.LEFT,padx=5)
    MENU_RADIO_BOX(frame_cham_mater, "cham_mater",fonte=fonte)
    self.cham_mater = LIST_BOX(frame_cham_mater,self.t_cham_mater,Tk.SINGLE,self.t_cham_mater[-1],fonte=fonte)
      
   # boite de saisie des champs
    if self.t_cara_elem != []:
      frame_cara_elem = Tk.Frame(frame_selection)
      frame_cara_elem.pack(side=Tk.LEFT,padx=5)
      MENU_RADIO_BOX(frame_cara_elem, "cara_elem",fonte=fonte)
      self.cara_elem = LIST_BOX(frame_cara_elem,self.t_cara_elem,Tk.SINGLE,self.t_cara_elem[-1],fonte=fonte)

   # boite de saisie des champs
    if self.t_para_sensi != []:
      frame_para_sensi = Tk.Frame(frame_selection)
      frame_para_sensi.pack(side=Tk.LEFT,padx=5)
      MENU_RADIO_BOX(frame_para_sensi, "para_sensi",fonte=fonte)
      self.para_sensi = LIST_BOX(frame_para_sensi,self.t_para_sensi,Tk.SINGLE,self.t_para_sensi[-1],fonte=fonte)

   # Boutons
    BOUTON(frame_boutons,'PaleGreen1','STANLEY',self.Lancer,fonte=fonte)
    BOUTON(frame_boutons,'IndianRed1','SORTIR',self.Sortir,fonte=fonte)

   
