#@ MODIF stanley Stanley  DATE 06/09/2004   AUTEUR MCOURTOI M.COURTOIS 
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

import sys,os,os.path,string,copy,tkFileDialog,cPickle
import as_courbes, xmgrace, gmsh
import cata_champs,aster

import Utilitai
from Utilitai import sup_gmsh
from graphiqueTk import *
from Cata.cata import *
from Accas import _F

cata = cata_champs.CATA_CHAMPS()


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
DEFI_FICHIER(FICHIER='TMP',UNITE=_ULGMSH)
_TUL2=INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
_ULMAIL=_TUL2['UNITE_LIBRE',1]
DEFI_FICHIER(ACTION='LIBERER',UNITE=_ULGMSH)
DETRUIRE(CONCEPT = _F(NOM = (_TUL,_TUL2)), INFO=1)
_UL =[_ULGMSH,_ULMAIL]

# ==============================================================================


class PARAMETRES :

  """
    GESTION DES PARAMETRES DE L'OUTIL
    
    Methodes publiques
      __getitem__ : retourne la valeur d'un parametre (s'il existe)
      
  """
  
  def __init__(self) :

    ok_env=1
    
    # essaye de lire le nom de la derniere config utilisée
    fic_env = os.path.join(os.environ['HOME'],'.stanley') + '/last.txt'
    try:
      f = open(fic_env)
      txt = f.read()
      f.close()
      fic_last=txt.split('\n')
      try:
        f = open(fic_last[0],'r')
        self.para = cPickle.load(f)
        f.close()
      except IOError, (errno, strerror):
        ok_env=0  # Le fichier n'existe plus
    except IOError, (errno, strerror):
      ok_env=0  # Le fichier d'environnement n'existe pas (premiere utilisation)

    # Si on a pas trouve de configuration precedente, on en cree une nouvelle
    if ok_env==0:
      self.para={}
      self.para['PARAMETRES']= {'fonte': "('Courier',9,'normal')", 
                            'grace': 'xmgrace', 
                            'smbclient': 'smbclient', 
                            'gmsh': 'gmsh',} 

      self.para['VISUALISATION']={'TAILLE_MIN': '0.',
                                  'SHRINK': '1.',
                                  'SKIN': 'NON',
                                  'version_fichier_gmsh': '1.2',}

      if 'HOSTNAME' in  os.environ.keys():
        self.para['CONFIG'] = {'mode': 'LOCAL', 
                               'machine_gmsh_login': '', 
                               'machine_gmsh_pass': '', 
                               'machine_visu': os.environ['DISPLAY'], 
                               'machine_gmsh_exe': '', 
                               'machine_gmsh': os.environ['HOSTNAME'], 
                               'machine_gmsh_tmp': '/tmp'} 
      else:
        mgmsh = string.split(os.environ['DISPLAY'],':')[0]
        self.para['CONFIG'] = {'mode': 'DISTANT',
                               'machine_gmsh_login': 'user',
                               'machine_gmsh_pass': '',
                               'machine_visu': os.environ['DISPLAY'],
                               'machine_gmsh_exe': '/usr/bin/gmsh',
                               'machine_gmsh': mgmsh,
                               'machine_gmsh_tmp': '/tmp'}

  def __getitem__(self, cle) :
  
    for nom_classe in self.para.keys() :
      classe = self.para[nom_classe]
      if cle in classe.keys() :
        return classe[cle]
    raise KeyError


  def __setitem__(self,cle,s_val) :
  
#     if cle == 'SHRINK' :
#       self.para['VISUALISATION']['SHRINK'] = string.atof(s_val)
#     elif cle == 'TAILLE_MIN' :
#       self.para['VISUALISATION']['TAILLE_MIN'] = string.atof(s_val)
#     else :
#       raise 'parametre inconnu'

    for i in self.para.keys():
      for j in self.para[i].keys():
        if cle == j:
          self.para[i][j] = str(s_val)


  def Liste(self, nom_classe) :

    l = self.para[nom_classe].keys()
    l.sort()
    return l


  def Modifier(self, nom_classe, interface) :
  
    if nom_classe not in self.para.keys() :
      raise 'classe de parametres inconnue'
      
    l_para  = self.Liste(nom_classe)
    nb_para = len(l_para)
    defaut  = []
    for p in l_para :
      defaut.append(self[p])
    reponse = interface.Requete_para(l_para,defaut)
        
    for row in xrange(nb_para) :
      p       = l_para[row]
      val_p   = reponse[row]
      self[p] = val_p    


  def Open_config(self, interface):
    fp=tkFileDialog.askopenfile(mode='r',filetypes=[("Fichiers Stanley", "*.stn"),("Tous", "*")],parent=interface.rootTk,title="Sélectionner le fichier contenant la configuration Stanley",initialdir='~/.stanley')
    if (fp != None):
      self.para = cPickle.load(fp)
      fp.close()
      # Sauvegarde de la derniere configuration connue
      f = os.path.join(os.environ['HOME'],'.stanley') + '/last.txt'
      fw=open(f,'w')
      fw.write(fp.name)
      fw.close()
      interface.ligne_etat.Affecter('Nouveaux parametres chargés.')


  def Save_config(self, interface):
    try:
      os.mkdir(os.environ['HOME'] + '/.stanley')
    except: pass
    fp=tkFileDialog.asksaveasfile(filetypes=[("Fichiers Stanley", "*.stn"),("Tous", "*")],parent=interface.rootTk,title="Sélectionner le fichier contenant la configuration Stanley",initialdir='~/.stanley')
    if (fp != None):
      if fp.name[-4:]!='.stn': 
        f=fp.name+'.stn'
      else:
        f=fp.name
      fp=open(f,'w')
      cPickle.dump(self.para,fp)
      fp.close()
      # Sauvegarde de la derniere configuration connue
      f = os.path.join(os.environ['HOME'],'.stanley') + '/last.txt'
      fw=open(f,'w')
      fw.write(fp.name)
      fw.close()
      interface.ligne_etat.Affecter('Nouveaux parametres sauvegardés.')


  def New_config(self, interface):

    lst_mode = ['LOCAL', 'DISTANT', 'WINDOWS']
    reponse = SAISIE_MODE( lst_mode, 'Choisir le mode')

    if (reponse == 'LOCAL'):
      self.para['CONFIG'] = {'mode': 'LOCAL', 
                             'machine_gmsh_login': '-na-', 
                             'machine_gmsh_pass': '-na-', 
                             'machine_visu': os.environ['DISPLAY'], 
                             'machine_gmsh_exe': '-na-', 
                             'machine_gmsh': os.environ['HOSTNAME'], 
                             'machine_gmsh_tmp': '/tmp'} 

    elif (reponse == 'DISTANT'):
      self.para['CONFIG'] = {'mode': 'DISTANT', 
                             'machine_gmsh_login': 'user', 
                             'machine_gmsh_pass': '-na-', 
                             'machine_visu': os.environ['DISPLAY'], 
                             'machine_gmsh_exe': '/usr/bin/gmsh', 
                             'machine_gmsh': 'Ip machine de Gmsh', 
                             'machine_gmsh_tmp': '/tmp'} 

    elif (reponse == 'WINDOWS'):
      self.para['CONFIG'] = {'mode': 'WINDOWS', 
                             'machine_gmsh_login': 'user (si besoin) ou vide', 
                             'machine_gmsh_pass': 'pass (si besoin) ou vide', 
                             'machine_visu': '-na-', 
                             'machine_gmsh_exe': '-na-', 
                             'machine_gmsh': 'Ip machine Win', 
                             'machine_gmsh_tmp': 'Nom de partage rep. Win'} 

    else:
      print "Erreur systeme! Prevenez la maintenance!"

    self.Modifier('CONFIG',interface)

# ==============================================================================



class CONTEXTE :

  def __init__(self, resultat, maillage, modele, cham_mater, cara_elem) :
  
    self.resultat   = resultat
    self.maillage   = maillage
    self.modele     = modele
    self.cham_mater = cham_mater
    self.cara_elem  = cara_elem 
  
# ==============================================================================



class ETAT_GEOM :

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
      if gma[2] == 3 and gma[0][0]<>'_' and gma[0][0:2]<>'GM' : 
        self.volu.append(gma[0])
    self.volu.sort()
      
   # les group_ma de dimension 2
    for gma in info_gma :
      if gma[2] == 2 and gma[0][0]<>'_' and gma[0][0:2]<>'GM' : 
        self.surf.append(gma[0])
    self.surf.sort()
      
   # les group_ma de dimension 1
    for gma in info_gma :
      if gma[2] == 1 and gma[0][0]<>'_' and gma[0][0:2]<>'GM' : 
        self.lign.append(gma[0])
    self.lign.sort()
      
   # Les points (group_no a un seul noeud)
    for gno in info_gno :
      if gno[1] == 1 and gno[0][0]<>'_' and gno[0][0:2]<>'GM' : 
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
    
    DEFI_GROUP(reuse = maillage,
      MAILLAGE = maillage,
      CREA_GROUP_NO = _F(GROUP_MA=ligne, NOM=nom, OPTION='NOEUD_ORDO')
      )
      
    return nom
       
       

  def Fusion(self,ma) :
  
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

    

class ETAT_RESU : 

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
    
    self.va  = self.contexte.resultat.LIST_PARA() 
#    self.va  = self.contexte.resultat.LIST_VARI_ACCES() 
    self.cmp = self.contexte.resultat.LIST_NOM_CMP()       
    self.ch  = self.contexte.resultat.LIST_CHAMPS()        
    for nom_cham in self.ch.keys() :
      if nom_cham not in cata.Champs_presents() :
        del self.ch[nom_cham]
  
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
        
    
  def Calcul(self, nom_cham, numeros) :
  
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
      cata[nom_cham].Evalue(self.contexte, numeros)

    self.Refresh()

# ==============================================================================
     

  
class SELECTION :  

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
  NonDeveloppeRS = "Fonctionnalite non developpee : seuls les resultats de type 'EVOL_ELAS', 'EVOL_THER' et 'EVOL_NOLI' sont geres."

  comb_tracables = {      # Combinaisons (type_champ, type_geom) tracables -> outil de trace
    ('NOEU','MAILLAGE')   : 'GMSH',
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
    }
 
  def __init__(self, contexte, etat_geom, etat_resu) :
  
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

    if self.mode == 'Isovaleurs' :
      t_geom = ['TOUT_MAILLAGE']
      for nom_group in self.etat_geom.volu :
        t_geom.append(string.ljust(nom_group,8) + " (3D)")
      for nom_group in self.etat_geom.surf :
        t_geom.append(string.ljust(nom_group,8) + " (2D)")
      self.interface.geom.Change(t_geom,'TOUT_MAILLAGE')   

    elif self.mode == 'Courbes' :
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


class STANLEY :

  """
    OUTIL DE POST-TRAITEMENT GRAPHIQUE
    
  """
  
  def __init__ (self, resultat, maillage, modele, cham_mater, cara_elem) :
  
    self.contexte   = CONTEXTE(resultat, maillage, modele, cham_mater, cara_elem)
    self.etat_geom  = ETAT_GEOM(maillage)
    self.etat_resu  = ETAT_RESU(self.contexte)
    self.selection  = SELECTION(self.contexte,self.etat_geom,self.etat_resu)
    self.parametres = PARAMETRES()    

    self.interface  = INTERFACE(self)
    self.selection.Interface(self.interface)

   # Drivers d'outils de post-traitement
    self.driver = {
      'Isovaleurs' : DRIVER_GMSH(self),
      'Courbes'    : DRIVER_GRACE(self)
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
      self.etat_resu.Calcul(self.selection.nom_cham, self.selection.numeros)      
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
          
   # Visualisation
    self.driver[self.selection.mode].Tracer(self.selection)    


  def Information(self) :
  
    self.selection.interface.Information()
    

  def Ajout_point(self) :
  
    """
      Creation interactive d'un point de post-traitement
    """

   # Lecture des caracteristiques du chemin    
    (nom,x0,y0,z0) = self.interface.Requete_point()

   # definition des nouvelles entites geometriques
    driver = DRIVER_SUP_GMSH(self)
    geom = driver.Importer_point(nom,x0,y0,z0)
    
   # Incorporation du nouveau point 
    self.etat_geom.Fusion(geom)
    self.selection.Refresh()


  def Ajout_chemin(self) :

    """
      Creation interactive d'un chemin de post-traitement
    """

   # Lecture des caracteristiques du chemin    
    (nom,x0,y0,z0,x1,y1,z1,nbr) = self.interface.Requete_chemin()

   # definition des nouvelles entites geometriques
    driver = DRIVER_SUP_GMSH(self)
    geom = driver.Importer_chemin (nom,x0,y0,z0,x1,y1,z1,nbr)
    
   # Incorporation du nouveau chemin 
    self.etat_geom.Fusion(geom)
    self.selection.Refresh()


  def Modi_visu(self) : 
    self.parametres.Modifier('VISUALISATION',self.interface)

  def Modi_para(self) : 
    self.parametres.Modifier('PARAMETRES',self.interface)

  def Modi_config(self) : 
    self.parametres.Modifier('CONFIG',self.interface)

  def Open_config(self) : 
    self.parametres.Open_config(self.interface)

  def Save_config(self) : 
    self.parametres.Save_config(self.interface)

  def New_config(self) : 
    self.parametres.New_config(self.interface)

  def Rien(self) : 
    pass

  def Quitter(self) :
    for i in range(_NUM):
      DETRUIRE(CONCEPT=_F(NOM='_MA_'+str(i)), INFO=1)
    self.selection.interface.rootTk.quit()

  def Select(self):

    for driver in self.driver.keys() :
      try :
        self.driver[driver].Fermer()
      except AttributeError :
        pass

    for i in range(_NUM):
      DETRUIRE(CONCEPT=_F(NOM='_MA_'+str(i)), INFO=1)
    self.interface.rootTk.destroy()
    PRE_STANLEY()


  def Clavier(self,event) :

    """
      Reaction aux raccourcis clavier (CTRL + touche)
    """
      
    touche = event.keysym
#    if touche == 'q' : self.selection.interface.rootTk.quit()
    if touche == 'q' : self.Quitter()
    if touche == 'c' : self.Calculer()
    if touche == 't' : self.Tracer()

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
    self.rootTk    = Tkinter.Tk()       
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
      self.rootTk.destroy()
    except:
      pass


  def Scan_selection(self) :

    """
      Action sur les evenements
        Scan les objets selectionnes
        Definit la frequence de scan
    """
  
    different = (
      self.champ.Scan() or 
      self.cmp.Scan()    or
      self.geom.Scan()   or 
      self.ordre.Scan()
      )

    if different :
      self.stan.selection.Refresh()   

    self.rootTk.after(30, self.Scan_selection)

    
  def Dessin(self, t_champ, t_cmp, t_geom, t_no, l_va) :

    """
      Creation de tous les objets graphiques de l'interface
    """
    
        

   # Frames generales
    frame_menu  = Tkinter.Frame(self.rootTk,relief=Tkinter.RAISED,bd=1)
    frame_menu.grid(row=0,column=0,sticky = Tkinter.NW)
#    frame_menu.pack(padx=0,pady=0,anchor = Tkinter.NW)
    frame_boutons  = Tkinter.Frame(self.rootTk,relief=Tkinter.FLAT,bd=1)
    frame_boutons.grid(row=0,column=2, sticky = Tkinter.NE)
#    frame_boutons.pack(anchor = Tkinter.NE,pady=0)
    frame_selection  = Tkinter.Frame(self.rootTk,relief=Tkinter.RAISED,bd=3)
    frame_selection.grid(row=1,column=0,columnspan=3)
#    frame_selection.pack(padx=2,pady=10)
    frame_bas = Tkinter.Frame(self.rootTk)
    frame_bas.grid(row=2,column=0,columnspan =3,pady=3)
    frame_espace = Tkinter.Frame(self.rootTk)
    frame_espace.grid(row=0,column=1,pady=20)
    
    
   # Menu deroulant
    titres = ['Fichier','Geometrie','Parametres','Actions']
    choix  = {}
    choix['Fichier'] = [
                        ('Version',self.stan.Information),
                        ('Quitter',self.stan.Quitter)]
    choix['Geometrie'] = [('Ajout Point',self.stan.Ajout_point),
                          ('Ajout Chemin',self.stan.Ajout_chemin),]
    choix['Actions'] = [ ('Calculer',self.stan.Calculer),
                         ('Tracer',self.stan.Tracer)]

    choix['Parametres'] = [ ('Visualisation',self.stan.Modi_visu),
                            ('Serveur de calcul et Stanley',self.stan.Modi_para),
                            ('Poste de travail',self.stan.Modi_config),
                            ('------------------------------',self.stan.Rien),
                            ('Charger une configuration',self.stan.Open_config),
                            ('Sauvegarder une configuration',self.stan.Save_config),
                            ('Nouvelle configuration',self.stan.New_config),
#                             ('Charger une configuration',self.Open_config),
#                             ('Sauvegarder une configuration',self.Save_config),
#                             ('Nouvelle configuration',self.stan.New_config),
                          ]

    menu = MENU(frame_menu,titres,choix)     

    fonte = eval(self.stan.parametres['fonte'])

   # boite de saisie des champs
    frame_champs = Tkinter.Frame(frame_selection)
    frame_champs.pack(side=Tkinter.LEFT,padx=5)
    MENU_RADIO_BOX(frame_champs, "Champs")
    self.champ = LIST_BOX(frame_champs,t_champ,Tkinter.SINGLE,fonte=fonte)

   # boite de saisie des composantes
    frame_cmp  = Tkinter.Frame(frame_selection)
    frame_cmp.pack(side=Tkinter.LEFT,padx=5)
    MENU_RADIO_BOX(frame_cmp,"Composantes")
    self.cmp = LIST_BOX(frame_cmp,t_cmp,Tkinter.EXTENDED,defaut = 'TOUT_CMP',fonte=fonte)

   # boite de saisie d'entites geometriques
    frame_geom  = Tkinter.Frame(frame_selection)
    frame_geom.pack(side=Tkinter.LEFT,padx=5)
    MENU_RADIO_BOX(frame_geom,"Entites Geometriques",['Isovaleurs','Courbes'],self.stan.selection.Change_mode)
    self.geom = LIST_BOX(frame_geom,t_geom,Tkinter.EXTENDED,defaut = 'TOUT_MAILLAGE',fonte=fonte)

   # boite de saisie des numeros d'ordre
    frame_ordre  = Tkinter.Frame(frame_selection)
    frame_ordre.pack(side=Tkinter.LEFT,padx=5)
    MENU_RADIO_BOX(frame_ordre,"Ordres",l_va,self.stan.selection.Change_va,'NUME_ORDRE')
    self.ordre = LIST_BOX(frame_ordre,t_no,Tkinter.EXTENDED,defaut = 'TOUT_ORDRE',fonte=fonte)

   # Feu tricolore
    self.feu = FEU_TRICOLORE(frame_selection)

   # Boutons
    BOUTON(frame_boutons,'PaleGreen1','TRACER'   ,self.stan.Tracer,x=2,y=0)
    BOUTON(frame_boutons,'orange'    ,'CALCULER' ,self.stan.Calculer,x=2,y=0)
    BOUTON(frame_boutons,'skyblue','SELECTION',self.stan.Select,x=2,y=0)
    BOUTON(frame_boutons,'IndianRed1','SORTIR'      ,self.stan.Quitter,x=2,y=0)

   # Ligne d'etat
    self.ligne_etat = LIGNE_ETAT(frame_bas)
    self.ligne_etat.Affecter('Bienvenue dans STANLEY !')
   
   # Raccourcis clavier
    self.rootTk.bind_all("<Control-KeyPress>",self.stan.Clavier)    
        

  def Information(self) :
  
    global info
    
    tk = Tkinter.Tk()
    tk.title = "A propos de Stanley"
    
    l = Tkinter.Label(tk,padx=15,pady=15,text=info)
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
    
    infos = [
      ['Nom du point',1],
      ['Coordonnees',3]]
    defaut = [[''],[0.,0.,0.]]
    
    reponse = SAISIE(infos,"Creation d'un point",defaut)
    
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
    
    infos = [
      ['Nom du chemin',1],
      ['Origine   (x,y,z)',3],
      ['Extremite (x,y,z)',3],
      ['Nombre de points',1]]
    defaut = [[''],[0.,0.,0.],[1.,0.,0.],[2]]
    reponse = SAISIE(infos,"Creation d'un chemin",defaut)
    
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
    
    
  def Requete_para(self, l_para, l_defaut) :

    """
      Boite de dialogue pour definir une classe de parametres
      
      IN :
        l_para   : liste des parametres a editer
        l_defaut : liste des valeurs actuelles des parametres
        
      RETURN :
        liste des valeurs des parametres
    """

    infos   = []
    defaut  = []
    nb_para = len(l_para)

    for row in xrange(nb_para) :
      infos.append( (l_para[row],1) )
      defaut.append( [ l_defaut[row] ])
    titre = "Edition des parametres"
    reponse = SAISIE(infos,titre,defaut)

    sortie = []
    for row in xrange(nb_para) :
      sortie.append(reponse[row][0])

    return sortie

# ==============================================================================



class DRIVER :

  """
    Driver d'outils de post-traitement
    Specialisation a chaque outil par heritage de cette classe

    Methodes publiques
      Fermer : ferme le terminal graphique
      Tracer : Trace la selection (a enrichir dans chaque classe heritee)
      Projeter : Projection d'un champ aux noeuds sur un chemin ou un point (chemin degenere)
      Ecla_Gauss : Projection d'un cham_elem_elga aux points de Gauss
  """

  def __init__(self, stan) :
  
    self.terminal = None
    self.stan     = stan
#    self.contexte = stan.contexte

  def Fermer(self) :
  
    self.terminal.Fermer()    


  def Tracer(self, selection) : pass
  

  def Projeter(self, selection, contexte, geom) :
  
    """
      Projection d'un champ au noeud sur l'entite geometrique de nom geom.
      Pour l'instant, l'entite geometrique se reduit a un chemin sur lequel
      on affecte des elements barres.
      
      selection : selection courante de type SELECTION
      contexte  : CONTEXTE pour la projection
      geom      : nom du groupe de mailles sur lequel on projette
      
      retourne le CONTEXTE lie au nouveau resultat produit ainsi que la
      liste des concepts Aster a detruire.
    """

#    l_detr = []

   # Pas de projection si meme maillage 
    maillage = selection.etat_geom.mail[geom]
    if maillage == contexte.maillage :
      return contexte, []

    MO_P = AFFE_MODELE(
      MAILLAGE = maillage,
      AFFE = _F(
        GROUP_MA = selection.geom[1],
#        TOUT         = 'OUI',
        PHENOMENE    = 'MECANIQUE',   # sans doute faire qchose de plus fin
        MODELISATION = 'BARRE',       # a ce niveau ...
        )
      )

    RESU_P = PROJ_CHAMP(
      METHODE = 'ELEM',
      RESULTAT = contexte.resultat,
      MODELE_1 = contexte.modele,
      MODELE_2 = MO_P,
      NOM_CHAM = selection.nom_cham,
      NUME_ORDRE = tuple(selection.numeros),
      )
      
    return CONTEXTE(RESU_P, maillage, MO_P, None, None), [MO_P, RESU_P]


  def Ecla_Gauss(self, selection, contexte) :
  
    """
      Projection aux points de Gauss (ECLA_PG)

      IN
        selection  selection courante de type SELECTION
        contexte   CONTEXTE du champ a eclater aux points de Gauss

      RETURN
        CONTEXTE lie au nouveau resultat produit
        liste des concepts Aster a detruire
    """

    if selection.nom_cham not in ['SIEF_ELGA','VARI_ELGA','SIEF_ELGA_TEMP','FLUX_ELGA_TEMP'] :
      raise SELECTION.NonDeveloppePG

    if   contexte.resultat.__class__ == evol_elas : 
      type_resu = 'EVOL_ELAS'
    elif contexte.resultat.__class__ == evol_ther :
      type_resu = 'EVOL_THER'
    elif contexte.resultat.__class__ == evol_noli :
      type_resu = 'EVOL_NOLI'
    else :
      raise SELECTION.NondeveloppeRS

    para = _F(  
      MODELE     = contexte.modele,
      SHRINK     = float(self.stan.parametres['SHRINK']),
      TAILLE_MIN = float(self.stan.parametres['TAILLE_MIN']),
      )

    if selection.geom[0] in ['VOLUME','SURFACE'] :
      para['GROUP_MA'] = selection.geom[1]

    __MA_G = CREA_MAILLAGE(
      MAILLAGE = contexte.maillage,
      ECLA_PG  = para,
      )

    para = _F(
      MAILLAGE    = __MA_G,
      MODELE_INIT = contexte.modele,
      RESU_INIT   = contexte.resultat,
      NOM_CHAM    = selection.nom_cham,
      NUME_ORDRE  = selection.numeros
      )

    if selection.geom[0] in ['VOLUME','SURFACE'] :
      para['GROUP_MA'] = selection.geom[1]

    __RESU_G = CREA_RESU(
      OPERATION   = 'ECLA_PG',
      TYPE_RESU   = type_resu,
      ECLA_PG     = para,
      )

    if selection.geom[0] == 'MAILLAGE':
      __MO_G   = copy.copy(contexte.modele)
      ldetr = [__MA_G, __RESU_G]

    else:
      if   selection.geom[0] == 'VOLUME':  pmod = '3D'
      else:                                pmod = 'D_PLAN'

      __MO_G = AFFE_MODELE(
        MAILLAGE = __MA_G,
        AFFE     = _F( TOUT = 'OUI',
                       PHENOMENE = 'MECANIQUE',
                       MODELISATION = pmod,))

      ldetr = [__MA_G, __MO_G, __RESU_G]

#    return CONTEXTE(__RESU_G, __MA_G, __MO_G, None, None), [__MA_G, __MO_G, __RESU_G]
    return CONTEXTE(__RESU_G, __MA_G, __MO_G, None, None), ldetr

# ==============================================================================

  

class DRIVER_SUP_GMSH(DRIVER) :

  """
    Interface avec le langage superviseur de GMSH
  """

  
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
#    mesh = sup_gmsh.Mesh()
    mesh = sup_gmsh.Mesh(gmsh=self.stan.parametres['gmsh'])
    mesh.Physical(nom,P0)
    mesh.Physical(nom_bid,L0)

    ma = mesh.LIRE_GMSH(UNITE_GMSH=_UL[0],UNITE_MAILLAGE=_UL[1])

    INDICE=_NUM
    _MA[INDICE] = CREA_MAILLAGE(MAILLAGE = ma,)
    DETRUIRE(CONCEPT = _F(NOM = ma), INFO=1)
    _NUM = _NUM + 1

    return ETAT_GEOM(_MA[INDICE])


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
#    mesh = sup_gmsh.Mesh()
    mesh = sup_gmsh.Mesh(gmsh=self.stan.parametres['gmsh'])
    mesh.Physical(nom,L01)    
#    ma = mesh.LIRE_GMSH()
    ma = mesh.LIRE_GMSH(UNITE_GMSH=_UL[0],UNITE_MAILLAGE=_UL[1])

    INDICE=_NUM
    _MA[INDICE] = CREA_MAILLAGE(MAILLAGE = ma,)
    DETRUIRE(CONCEPT = _F(NOM = ma), INFO=1)
    _NUM = _NUM + 1

    return ETAT_GEOM(_MA[INDICE])



    
# ==============================================================================



class DRIVER_GMSH(DRIVER) :

  def Tracer(self, selection) :

    if self.terminal : self.terminal.Fermer()       # un seul terminal GMSH ouvert en meme temps
    l_detr = []
  
    DEFI_FICHIER(
        UNITE = 33, 
#        FICHIER   = 'GMSH_POS'
      )

    contexte   = self.stan.contexte
    type_champ = cata[selection.nom_cham].type

    if type_champ == 'ELGA' :
      contexte, l_detr = self.Ecla_Gauss(selection, contexte)

    para = _F(
      RESULTAT   = contexte.resultat,
      NOM_CHAM   = selection.nom_cham,
      NUME_ORDRE = selection.numeros, 
#      FICHIER    = 'GMSH_POS',
#       FORMAT     = 'GMSH',
#       VERSION    = eval(self.stan.parametres['version_fichier_gmsh']),
      )

    if type_champ in ['NOEU','ELNO'] and selection.geom[0] in ['VOLUME','SURFACE'] :
      para['GROUP_MA'] = selection.geom[1]    # non actif a cause de IMPR_RESU

    if 'TOUT_CMP' not in selection.nom_cmp :
      para['NOM_CMP'] = tuple(selection.nom_cmp)


#    IMPR_RESU(RESU = para)

    IMPR_RESU( UNITE   = 33, 
               FORMAT  = 'GMSH',
               VERSION = eval(self.stan.parametres['version_fichier_gmsh']),
               RESU    = para,
             )

    DEFI_FICHIER(ACTION='LIBERER',UNITE=33)

    if l_detr : 
      DETRUIRE(CONCEPT = _F(NOM = tuple(l_detr)), INFO=1)

    self.terminal = gmsh.GMSH('POST','fort.33',self.stan.parametres)
      
# ==============================================================================

  
  
class DRIVER_GRACE(DRIVER) :

  CheminMultiple = "Le trace de courbe ne peut se faire que sur un chemin unique"


  def Tracer(self, selection) :
  
   # Ouverture ou rafraichissement du terminal si necessaire

    if not self.terminal : 
      self.terminal = xmgrace.Xmgr()
    else :
      if not self.terminal.Terminal_ouvert() :
        self.terminal.Fermer()
        self.terminal = xmgrace.Xmgr()

   # Extraction des resultats pour la selection requise
    l_courbes = self.Extract(selection)
    
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

   # Parametres communs a toutes les tables a calculer 
    para = _F(
      INTITULE   = 'TB_GRACE',
      NOM_CHAM   = selection.nom_cham,
      OPERATION  = 'EXTRACTION'
      )
      
    if 'TOUT_CMP' in selection.nom_cmp :
      para['TOUT_CMP'] = 'OUI'
      l_nom_cmp = self.stan.etat_resu.cmp[selection.nom_cham]
    else :
      para['NOM_CMP'] = tuple(selection.nom_cmp)
      l_nom_cmp = selection.nom_cmp
      
      
    
    if selection.geom[0] == 'POINT' :
    
      para['NUME_ORDRE'] = selection.numeros
      for point in selection.geom[1] :
        contexte, detr = self.Projeter(selection, contexte, point) 
        l_detr += detr
        para['RESULTAT'] = contexte.resultat
        para['GROUP_NO'] = point
        __GRACE = POST_RELEVE_T(ACTION = para)
        
        for comp in l_nom_cmp :
          vale_x = selection.vale_va
          courbe = as_courbes.Courbe(vale_x,vale_x)
          courbe.Lire_y(__GRACE,comp)
          nom = comp + ' --- ' + string.ljust(point,8)
          l_courbes.append( (courbe, nom) )
      
        DETRUIRE(CONCEPT = _F(NOM = __GRACE),INFO=1)
        if l_detr: DETRUIRE(CONCEPT = _F(NOM = tuple(l_detr) ),INFO=1)
        
           
    elif selection.geom[0] == 'CHEMIN' :
    
      chemin = selection.geom[1][0]
     
     # projection si necessaire
      contexte, detr = self.Projeter(selection, contexte, chemin)
      l_detr += detr    

      para['RESULTAT'] = contexte.resultat
      para['GROUP_NO'] = self.stan.etat_geom.Oriente(chemin)

      for no, va in map(lambda x,y : (x,y), selection.numeros, selection.vale_va) :
        para['NUME_ORDRE'] = no,
        __GRACE = POST_RELEVE_T(ACTION = para)

        for comp in l_nom_cmp :
          courbe = as_courbes.Courbe()
          courbe.Lire_x(__GRACE, 'ABSC_CURV')
          courbe.Lire_y(__GRACE, comp)

          nom_va = selection.nom_va
          nom = comp + ' --- ' + nom_va + ' = ' + repr(va)
          l_courbes.append( (courbe, nom) )

        DETRUIRE(CONCEPT = _F(NOM = __GRACE),INFO=1)

      if l_detr: DETRUIRE(CONCEPT = _F(NOM = tuple(l_detr) ), INFO=1)

    else : raise 'ERREUR_DVP'    

    return l_courbes
      
# ==============================================================================

# -*- coding: iso-8859-1 -*-

class PRE_STANLEY :

  """
    INTERFACE GRAPHIQUE TK DE SELECTION DES CONCEPTS ASTER
    
     Attributs publics
      frame_maillage      : LIST_BOX champ selection des concepts Aster de type "maillage"
      frame_modele        : LIST_BOX champ selection des concepts Aster de type "modele"
      frame_evol          : LIST_BOX champ selection des concepts Aster de type "evol_*"
      frame_cham_mater    : LIST_BOX champ selection des concepts Aster de type "cham_mater"
      frame_cara_elem     : LIST_BOX champ selection des concepts Aster de type "cara_elem"
      
     Methode publique 
      Exec       : lancement du scan des evenements
      
     Attributs prives
      rootTk     : racine des objets Tk
      
     Methodes privees
      Action_evenement : actions consecutives au scan des evenements
      Dessin     : creation des objets graphiques Tk de l'interface  
  """


  def __init__(self) :   


    self.para = PARAMETRES()

    self.rootTk    = Tkinter.Tk()       
    self.rootTk.wm_title('PRE_STANLEY : choix des concepts')

    # Récupération des concepts Aster présents dans la base
    self.jdc_recupere=CONTEXT.get_current_step().jdc
    t_maillage=[]
    t_modele=[]
    t_evol=[]
    t_cham_mater=[]
    t_cara_elem=[]

    for i in self.jdc_recupere.sds_dict.keys( ):
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
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'cham_mater':
          t_cham_mater.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'cara_elem_sdaster':
          t_cara_elem.append( i )

    self.t_maillage=t_maillage
    self.t_modele=t_modele
    self.t_evol=t_evol
    self.t_cham_mater=t_cham_mater
    self.t_cara_elem=t_cara_elem

    self.Dessin()
    self.Scan_selection()               
    self.Exec()
    

  def Exec(self) :
  
    """ 
      Demarre le scan des evenements
    """
    
    self.rootTk.mainloop()



  def Scan_selection(self) :

    """
      Action sur les evenements
        Scan les objets selectionnes
        Definit la frequence de scan
    """
  
    self.rootTk.after(30, self.Scan_selection)


  def Lancer(self) :
 
     i=int(self.modele.listbox.curselection()[0])
     modele=self.t_modele[i]
     _maillag = aster.getvectjev( string.ljust(modele,8) + '.MODELE    .NOMA        ' )
     maillage = string.rstrip( _maillag[0] )

     i=int(self.evol.listbox.curselection()[0])
     evol=self.t_evol[i]
     i=int(self.cham_mater.listbox.curselection()[0])
     cham_mater=self.t_cham_mater[i]
     if self.t_cara_elem != []:
       i=int(self.cara_elem.listbox.curselection()[0])
       cara_elem=self.t_cara_elem[i]
       self.rootTk.destroy()
       STANLEY(self.jdc_recupere.sds_dict[evol],self.jdc_recupere.sds_dict[maillage],self.jdc_recupere.sds_dict[modele],self.jdc_recupere.sds_dict[cham_mater],self.jdc_recupere.sds_dict[cara_elem])
     else:
       self.rootTk.destroy()
       STANLEY(self.jdc_recupere.sds_dict[evol],self.jdc_recupere.sds_dict[maillage],self.jdc_recupere.sds_dict[modele],self.jdc_recupere.sds_dict[cham_mater],None)


  def Dessin(self) :

    """
      Creation de tous les objets graphiques de l'interface
    """

    fonte = eval(self.para['fonte'])
        
   # Frames generales
    frame_selection  = Tkinter.Frame(self.rootTk,relief=Tkinter.RAISED,bd=3)
    frame_selection.pack(padx=2,pady=2)
    frame_bas = Tkinter.Frame(self.rootTk)
    frame_bas.pack(pady=10)
    frame_boutons  = Tkinter.Frame(frame_bas,relief=Tkinter.RAISED,bd=1)
    frame_boutons.pack(side=Tkinter.LEFT,pady=10)

   # boite de saisie des champs
    frame_modele = Tkinter.Frame(frame_selection)
    frame_modele.pack(side=Tkinter.LEFT,padx=5)
    MENU_RADIO_BOX(frame_modele, "modele")
    self.modele = LIST_BOX(frame_modele,self.t_modele,Tkinter.SINGLE,self.t_modele[-1],fonte=fonte)
      
   # boite de saisie des champs
    frame_evol = Tkinter.Frame(frame_selection)
    frame_evol.pack(side=Tkinter.LEFT,padx=5)
    MENU_RADIO_BOX(frame_evol, "evol")
    self.evol = LIST_BOX(frame_evol,self.t_evol,Tkinter.SINGLE,self.t_evol[-1],fonte=fonte)
      
   # boite de saisie des champs
    frame_cham_mater = Tkinter.Frame(frame_selection)
    frame_cham_mater.pack(side=Tkinter.LEFT,padx=5)
    MENU_RADIO_BOX(frame_cham_mater, "cham_mater")
    self.cham_mater = LIST_BOX(frame_cham_mater,self.t_cham_mater,Tkinter.SINGLE,self.t_cham_mater[-1],fonte=fonte)
      
   # boite de saisie des champs
    if self.t_cara_elem != []:
      frame_cara_elem = Tkinter.Frame(frame_selection)
      frame_cara_elem.pack(side=Tkinter.LEFT,padx=5)
      MENU_RADIO_BOX(frame_cara_elem, "cara_elem")
      self.cara_elem = LIST_BOX(frame_cara_elem,self.t_cara_elem,Tkinter.SINGLE,self.t_cara_elem[-1],fonte=fonte)

   # Boutons
    BOUTON(frame_boutons,'PaleGreen1','STANLEY',self.Lancer)
    BOUTON(frame_boutons,'IndianRed1','SORTIR',self.rootTk.destroy)

   
