#@ MODIF stanley Stanley  DATE 16/06/2003   AUTEUR JMBHH01 J.M.PROIX 
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
'''Interface aster -> post-traitement graphique'''

import os, string, copy

# Preparer a l'importation le module d'environnement
import sys,os,os.path
home = os.environ['HOME']
rep_env = os.path.join(home,'.stanley')
sys.path.insert(1,rep_env)

fic_env = rep_env + '/env.py'
try:
    f = open(fic_env)
except IOError, (errno, strerror):
    print '  # ---------------------------------------------------------------------------'
    print '  # STANLEY - Erreur : il manque le fichier ~/.stanley/env.py !'
    print '  # ---------------------------------------------------------------------------'
    sys.exit(1)

f.close()

import as_courbes, xmgrace, gmsh, env
import cata_champs,aster
from graphiqueTk import *

from Cata.cata import *
from Accas import _F

cata = cata_champs.CATA_CHAMPS()

# ==============================================================================



class CONTEXTE :

  def __init__(self, resultat, maillage, modele, cham_mater, cara_elem) :
  
    self.resultat   = resultat
    self.maillage   = maillage
    self.modele     = modele
    self.cham_mater = cham_mater
    self.cara_elem  = cara_elem 
  
# ==============================================================================



class ETAT_MA :

  """
    ENSEMBLE DES DONNEES GEOMETRIQUES ATTACHEES AU MAILLAGE
    
    Attributs publics
     volu     : liste des group_ma de dimension 3
     surf     : liste des group_ma de dimension 2
     lign     : liste des group_ma de dimension 1
     poin     : liste des group_no a un seul noeud
     
    Methodes publiques
     Oriente  : fournir le group_no oriente correspondant a un group_ma ligne
  """
  
  def __init__(self, contexte) :

    maillage = contexte.maillage  
    self.maillage = maillage
    self.Classification()
       
    self.orie = {}


  def Classification(self) :
  
    """
     Classe les group_ma et les group_no par volumes, surfaces, lignes et points
    """
    
    self.volu = []
    self.surf = []
    self.lign = []
    self.poin = []

    info_gma = self.maillage.LIST_GROUP_MA()
    info_gno = self.maillage.LIST_GROUP_NO()
    
   # les group_ma de dimension 3
    for gma in info_gma :
      if gma[2] == 3 : self.volu.append(gma[0])
    self.volu.sort()
      
   # les group_ma de dimension 2
    for gma in info_gma :
      if gma[2] == 2 : self.surf.append(gma[0])
    self.surf.sort()
      
   # les group_ma de dimension 1
    for gma in info_gma :
      if gma[2] == 1 : self.lign.append(gma[0])
    self.lign.sort()
      
   # Les points (group_no a un seul noeud)
    for gno in info_gno :
      if gno[1] == 1 : self.poin.append(gno[0])
    self.poin.sort()


  def Oriente(self, ligne) :

    """
     Retourne le nom du group_no oriente correspondant au group_ma ligne
     Le cree le cas echeant
    
     IN  ligne    : nom du group_ma de type ligne qu'on cherche a orienter
    """
    
    if ligne in self.orie.keys() :
      num = self.orie[ligne]
      return '_OR'+repr(num)
      
    num = len(self.orie.keys()) + 1
    nom = '_OR'+repr(num)
    self.orie[ligne] = num
    
    DEFI_GROUP(reuse = self.maillage,
      MAILLAGE = self.maillage,
      CREA_GROUP_NO = _F(GROUP_MA=ligne, NOM=nom, OPTION='NOEUD_ORDO')
      )
      
    return nom
       
# ==============================================================================

    

class ETAT_SD : 

  '''Etat de la SD resultat'''
  
  
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
    
    self.va  = self.contexte.resultat.LIST_VARI_ACCES() 
    self.cmp = self.contexte.resultat.LIST_NOM_CMP()       
    self.ch  = self.contexte.resultat.LIST_CHAMPS()
#    print self.ch
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
        
    
  def Calcul(self, numeros, etapes) :
  
    """
      Realise le calcul d'un champ pour une liste de numeros d'ordre et en s'appuyant 
      sur des etapes intermediaires de calcul.
      IN :
        numeros : liste des numeros d'ordre
        etapes  : liste des champs intermediaires a calculer (fournie par la methode Etapes.calcul)
    """
    
    for nom_cham in etapes[1:] :
      cata[nom_cham].Evalue(self.contexte, numeros)
    self.Refresh()

# ==============================================================================
     

  
class SELECTION :  

  """
    Selection realisee par l'utilisateur :
     - champ a tracer
     - composantes
     - entites geometriques
     - numeros d'ordre
  """
  
  NonDeveloppe = "Fonctionnalite non developpee"
    
  comb_tracables = {      # Combinaisons (type_champ, type_geom) tracables -> outil de trace
    ('NOEU','MAILLAGE')   : 'GMSH',
    ('NOEU','CHEMIN')     : 'GRACE',
    ('NOEU','POINT')      : 'GRACE',
    
    ('ELNO','MAILLAGE')   : 'GMSH',
    ('ELNO','GROUP_MA')   : 'GMSH',
    ('ELNO','CHEMIN')     : 'GRACE',
    ('ELNO','POINT')      : 'GRACE',
    
    ('ELGA','MAILLAGE')   : 'GMSH',
    }
    
      
  def __init__(self, contexte, etat_ma, etat_sd) :
  
    self.contexte = contexte
    self.etat_ma  = etat_ma
    self.etat_sd  = etat_sd
    
    self.nom_cham = ''    # Nom du champ selectionne
    self.nom_cmp  = []    # liste des noms des composantes selectionnees
    self.numeros  = []    # liste des numeros d'ordre selectionnes
    self.geom     = None  # Entites geometriques selectionnees : (type, [entite 1, entite 2, ...])
                          #   type in 'MAILLAGE','GROUP_MA','CHEMIN','POINT'
                          #   les entites selectionnees sont necessairement du meme type
    self.champ    = ''    # Statut du champ actif (INACCESSIBLE, A_CALCULER ou DEJA_CALCULE)
    self.etapes   = []    # Etapes de calcul pour evaluer le champ actif
    self.tracable = 0     # Compatibilite du champ actif et des entites geometriques actives
                          #   0 -> On ne sait pas tracer
                          #   1 -> On sait tracer
        
    self.Refresh()
    
    
    
  def Refresh(self) :
  
    """
      Mise a jour des variables d'etat : champ, etapes, tracable
    """
    
   # Nature du champs (INACCESSIBLE, A_CALCULER ou DEJA_CALCULE) 
    if self.nom_cham and self.numeros :
      self.etapes = self.etat_sd.Etapes_calcul(self.nom_cham, self.numeros)
      if len(self.etapes) == 0 :
        self.champ = 'INACCESSIBLE'
      elif len(self.etapes) == 1 :
        self.champ = 'DEJA_CALCULE'
      else :
        self.champ = 'A_CALCULER'      
    else :
      self.etapes = ['']
      self.champ  = 'INACCESSIBLE'
            
   # le champ est-il compatible avec les choix geometriques ?  
    self.tracable = 0
    if self.geom and self.champ <> 'INACCESSIBLE' :
      type_champ = cata[self.nom_cham].type
      if (type_champ, self.geom[0]) in SELECTION.comb_tracables.keys() :
        self.tracable = 1
      if self.geom[0] == 'CHEMIN' and len(self.geom[1])>1 :
        self.tracable = 0
   
# ==============================================================================



class STANLEY :

  """
    OUTIL DE POST-TRAITEMENT GRAPHIQUE
    
  """
  
  def __init__ (self, resultat, maillage, modele, cham_mater, cara_elem) :

#    # Permet de retrouver le maillage à partir du modele
#     __mail  = aster.getvectjev( string.ljust(modele.nom,8) + '.MODELE    .NOMA        ' )
#     __maill = string.rstrip( __mail[0] )
#     jdc_recupere=CONTEXT.get_current_step( )
#     for i in jdc_recupere.sds_dict.keys( ):
#       if i == __maill:
#         maillage = jdc_recupere.sds_dict[i]
    
    self.contexte  = CONTEXTE(resultat, maillage, modele, cham_mater, cara_elem)
    self.etat_ma   = ETAT_MA(self.contexte)
    self.etat_sd   = ETAT_SD(self.contexte)
    self.selection = SELECTION(self.contexte,self.etat_ma,self.etat_sd)
    
    self.nom_va    = 'NUME_ORDRE'
    self.format    = 'Isovaleurs'             # Isovaleurs ou Courbes
    self.terminal  = None               # Terminal xmgrace
    self.liste_cmp = []
    
    
   # Drivers d'outils de post-traitement
    self.driver = {
      'Isovaleurs' : DRIVER_GMSH(self),
      'Courbes'    : DRIVER_GRACE(self)
      }
    
   # Lancement de l'interface graphique (jusqu'a ce qu'on quitte) 
    self.interface = self.Interface()
    self.interface.Exec()

   # Menage
    for driver in self.driver.keys() :
      try :
        self.driver[driver].Fermer()
      except AttributeError :
        pass



  def Interface(self) :
  
    """
      Creation de l'interface graphique
    """
    
   # Liste des champs
    t_champ = self.etat_sd.Liste_champs_accessibles(self.selection.numeros)
    t_champ.sort()  

   # Liste des composantes
    t_cmp = ['TOUT_CMP'] + self.liste_cmp
        
   # Liste des entites geometriques     
    t_geom = ['TOUT_MAILLAGE']
    for nom_group in self.etat_ma.volu :
      t_geom.append(string.ljust(nom_group,8) + " (3D)")
    for nom_group in self.etat_ma.surf :
      t_geom.append(string.ljust(nom_group,8) + " (2D)")

   # Liste des numeros d'ordre
    t_no = ['TOUT_ORDRE'] + self.etat_sd.va['NUME_ORDRE']

   # Liste des variables d'acces
    l_va = self.etat_sd.va.keys()    
    
    return INTERFACE(self,t_champ,t_cmp,t_geom,t_no,l_va)
       
    
        
  def Calculer(self) :
  
    """
      Calculer (si necessaire) le resultat d'une selection
    """

    if self.couleur == 'orange' :
      self.etat_sd.Calcul(self.selection.numeros, self.selection.etapes)      
      self.Refresh()
      
          
  def Tracer(self) :
  
    """
      Tracer le resultat d'une selection
    """

   # Precondition
    if self.couleur == 'red' : return
    
   # Post-traitement (si necessaire)
    self.Calculer()
          
   # Visualisation
    self.driver[self.format].Tracer(self.selection)    

        
          
  def Change_va(self, nom_va):
  
    if self.nom_va == nom_va : return
    self.nom_va = nom_va
    self.interface.ordre.Change(['TOUT_ORDRE'] + self.etat_sd.va[nom_va], 'TOUT_ORDRE')   


  def Change_format(self, format):
  
    if self.format == format : return 
    self.format = format

    if self.format == 'Isovaleurs' :
      t_geom = ['TOUT_MAILLAGE']
      for nom_group in self.etat_ma.volu :
        t_geom.append(string.ljust(nom_group,8) + " (3D)")
      for nom_group in self.etat_ma.surf :
        t_geom.append(string.ljust(nom_group,8) + " (2D)")
      self.interface.geom.Change(t_geom,'TOUT_MAILLAGE')   

    elif self.format == 'Courbes' :
      t_geom = []
      for nom_group in self.etat_ma.lign :
        t_geom.append(string.ljust(nom_group,8) + " (CHEMIN)")
      for nom_group in self.etat_ma.poin :
        t_geom.append(string.ljust(nom_group,8) + " (POINT)")
      self.interface.geom.Change(t_geom)   

    else : raise 'ERREUR_DVP'
    self.Refresh()


  def Change_cmp(self, nom_cham) :    

    if nom_cham :
      liste_cmp = self.etat_sd.cmp[nom_cham]
    else :
      liste_cmp = []
    if liste_cmp <> self.liste_cmp :
      self.liste_cmp = liste_cmp
      self.interface.cmp.Change(['TOUT_CMP'] + liste_cmp, 'TOUT_CMP')      


  def Type_geom(self, nom_geom) :
  
    if string.find(nom_geom,'(3D)'    ) <> -1 : return 'GROUP_MA'
    if string.find(nom_geom,'(2D)'    ) <> -1 : return 'GROUP_MA'
    if string.find(nom_geom,'(1D)'    ) <> -1 : return 'GROUP_MA'
    if string.find(nom_geom,'(CHEMIN)') <> -1 : return 'CHEMIN'
    if string.find(nom_geom,'(POINT)' ) <> -1 : return 'POINT'
    raise 'ERREUR_DVP'
    
    
  def Refresh(self) :
  
    """ 
      Mettre a jour la nouvelle selection :
        mise a jour des champs de l'objet self.selection
        analyse de la compatibilite et mise a jour du feu
    """

   # Mise a jour du nom du champ
    if self.interface.champ.courant :
      self.selection.nom_cham = self.interface.champ.courant[0]
      help = cata[self.selection.nom_cham].comment
      self.interface.ligne_etat.Affecter(help)

    
  # Mise a jour des composantes selectionnables et selectionnees
    self.Change_cmp(self.selection.nom_cham)
    self.selection.nom_cmp = self.interface.cmp.courant    

    
   # Mise a jour des noms d'entites geometriques
    if 'TOUT_MAILLAGE' in self.interface.geom.courant :
      self.selection.geom = ('MAILLAGE',None)
    else :
      type_actu = ''
      liste_actu = []
      for nom in self.interface.geom.courant :
        nom_group  = string.strip(nom[:8]) 
        type_group = self.Type_geom(nom[8:])    
        liste_actu.append(nom_group)
        if not type_actu :
          type_actu = type_group
        elif type_group <> type_actu :
          self.selection.geom = None
          break
      else :
        self.selection.geom = (type_actu,liste_actu)
  
   # Mise a jour des numeros d'ordre
    if 'TOUT_ORDRE' in self.interface.ordre.courant :
      self.selection.numeros=self.etat_sd.va['NUME_ORDRE']
    else :
      if self.nom_va != 'NUME_ORDRE' :
        self.selection.numeros = [self.etat_sd.va['NUME_ORDRE'][self.etat_sd.va[self.nom_va].index(x)] for x in self.interface.ordre.courant]
      else :
        self.selection.numeros = self.interface.ordre.courant
   
   # Mise a jour de la nouvelle selection
    self.selection.Refresh()
    
    
   # Determination de la couleur du feu

    if (self.selection.tracable == 0 or 
        self.selection.champ == 'INACCESSIBLE'    or
        not self.selection.nom_cmp ) :
      self.couleur = 'red'
    elif self.selection.champ == 'A_CALCULER' :
      self.couleur = 'orange'
    else :
      self.couleur = 'green'

    self.interface.feu.Changer_couleur(self.couleur)

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
      
     Methode publique 
      Exec       : lancement du scan des evenements
      
     Attributs prives
      stan       : reference vers l'objet maitre stanley (pour Refresh, Calculer, Tracer)
      rootTk     : racine des objets Tk
      
     Methodes privees
      Action_evenement : actions consecutives au scan des evenements
      Dessin     : creation des objets graphiques Tk de l'interface  
  """


  def __init__(self, stan, t_champ, t_cmp, t_geom, t_no, l_va) :   
  
    """
      IN stan    : Reference vers stanley pour acces aux methodes Refresh,
                   Tracer et Calculer en lien avec les boutons et un
                   changement dans la selection
      IN t_champ : liste des champs selectionnables
      IN t_cmp   : liste des composantes selectionnables
      IN t_geom  : liste des entites geometriques selectionnables
      IN t_no    : liste des numeros d'ordre selectionnables
      IN l_va    : liste des variables d'acces
    """
     
    self.stan = stan                     
    self.rootTk    = Tkinter.Tk()       
    self.rootTk.wm_title('STANLEY')
    self.Dessin(t_champ, t_cmp, t_geom, t_no, l_va)
    self.Scan_selection()               
    

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
  
    different = (
      self.champ.Scan() or 
      self.cmp.Scan()    or
      self.geom.Scan()   or 
      self.ordre.Scan()
      )
        
    if different :
      self.stan.Refresh()   

    self.rootTk.after(30, self.Scan_selection)


    
  def Dessin(self, t_champ, t_cmp, t_geom, t_no, l_va) :

    """
      Creation de tous les objets graphiques de l'interface
    """
        
   # Frames generales
    frame_selection  = Tkinter.Frame(self.rootTk,relief=Tkinter.RAISED,bd=3)
    frame_selection.pack(padx=2,pady=2)
    frame_bas = Tkinter.Frame(self.rootTk)
    frame_bas.pack(pady=10)
    frame_boutons  = Tkinter.Frame(frame_bas,relief=Tkinter.RAISED,bd=1)
    frame_boutons.pack(side=Tkinter.LEFT,pady=10)

   # boite de saisie des champs
    frame_champs = Tkinter.Frame(frame_selection)
    frame_champs.pack(side=Tkinter.LEFT,padx=5)
    MENU_RADIO_BOX(frame_champs, "Champs")
    self.champ = LIST_BOX(frame_champs,t_champ,Tkinter.SINGLE,fonte=env.fonte)
      
   # boite de saisie des composantes
    frame_cmp  = Tkinter.Frame(frame_selection)
    frame_cmp.pack(side=Tkinter.LEFT,padx=5)
    MENU_RADIO_BOX(frame_cmp,"Composantes")
    self.cmp = LIST_BOX(frame_cmp,t_cmp,Tkinter.EXTENDED,defaut = 'TOUT_CMP',fonte=env.fonte)
      
   # boite de saisie d'entites geometriques
    frame_geom  = Tkinter.Frame(frame_selection)
    frame_geom.pack(side=Tkinter.LEFT,padx=5)
    MENU_RADIO_BOX(frame_geom,"Entites Geometriques",['Isovaleurs','Courbes'],self.stan.Change_format)
    self.geom = LIST_BOX(frame_geom,t_geom,Tkinter.EXTENDED,defaut = 'TOUT_MAILLAGE',fonte=env.fonte)
          
   # boite de saisie des numeros d'ordre
    frame_ordre  = Tkinter.Frame(frame_selection)
    frame_ordre.pack(side=Tkinter.LEFT,padx=5)
    MENU_RADIO_BOX(frame_ordre,"Ordres",l_va,self.stan.Change_va,'NUME_ORDRE')
    self.ordre = LIST_BOX(frame_ordre,t_no,Tkinter.EXTENDED,defaut = 'TOUT_ORDRE',fonte=env.fonte)

   # Feu tricolore
    self.feu = FEU_TRICOLORE(frame_selection)
  
   # Boutons
    BOUTON(frame_boutons,'PaleGreen1','TRACER'  ,self.stan.Tracer)
    BOUTON(frame_boutons,'orange'    ,'CALCULER',self.stan.Calculer)
    BOUTON(frame_boutons,'IndianRed1','SORTIR'  ,self.rootTk.destroy)

   # Ligne d'etat
    self.ligne_etat = LIGNE_ETAT(self.rootTk)
     
# ==============================================================================



class DRIVER :

  def __init__(self, stan) :
  
    self.terminal = None
    self.stan     = stan


  def Fermer(self) :
  
    self.terminal.Fermer()    


  def Tracer(self, selection) : pass
  
# ==============================================================================

  
  
class DRIVER_GMSH(DRIVER) :


  def Tracer(self, selection) :
  
    if self.terminal : self.terminal.Fermer()
    self.Exporter(selection)
    self.terminal = gmsh.GMSH('POST','fort.33')
        
    
  def Exporter(self, selection) :

    """
      Execute les commandes aster d'impression du champ au format GMSH
    """  

    import env
        
    type_champ = cata[selection.nom_cham].type
    a_detruire = []
    
    DEFI_FICHIER(
        UNITE = 33, 
        FICHIER   = 'GMSH_POS'
      )
    
    if type_champ == 'ELNO' :     
      para = _F(
        RESULTAT   = selection.contexte.resultat,
        NOM_CHAM   = selection.nom_cham,
        NUME_ORDRE = selection.numeros 
        )
         
      if selection.geom[0] == 'GROUP_MA' :
        para['GROUP_MA'] = selection.geom[1]
        
    elif type_champ == 'NOEU' :     
      para = _F(
        RESULTAT   = selection.contexte.resultat,
        NOM_CHAM   = selection.nom_cham,
        NUME_ORDRE = selection.numeros 
        )
         
    elif type_champ == 'ELGA' :
    
      if selection.nom_cham not in ['SIEF_ELGA','VARI_ELGA','SIEF_ELGA_TEMP','FLUX_ELGA_TEMP'] :
        raise SELECTION.NonDeveloppe
        
      __MA_G = CREA_MAILLAGE(
        MAILLAGE = selection.contexte.maillage,
        ECLA_PG  = _F(
          MODELE = selection.contexte.modele
          )
        )
      
      if   selection.contexte.resultat.__class__ == evol_elas : 
        type_resu = 'EVOL_ELAS'
      elif selection.contexte.resultat.__class__ == evol_ther :
        type_resu = 'EVOL_THER'
      elif selection.contexte.resultat.__class__ == evol_noli :
        type_resu = 'EVOL_NOLI'
      else :
        raise SELECTION.Nondeveloppe


      __RESU_G = CREA_RESU(
        OPERATION   = 'ECLA_PG',
        TYPE_RESU   = type_resu,
        ECLA_PG = _F(
          MAILLAGE    = __MA_G,
          MODELE_INIT = selection.contexte.modele,
          RESU_INIT   = selection.contexte.resultat,
          NOM_CHAM    = selection.nom_cham,
          NUME_ORDRE  = selection.numeros
          )
        )
         
      a_detruire = [__MA_G, __RESU_G]
        
      para = _F(
        RESULTAT   = __RESU_G,
        NOM_CHAM   = selection.nom_cham,
        NUME_ORDRE = selection.numeros 
        )
         
    else :
      raise SELECTION.NonDeveloppe
      
        
    para['FICHIER'] = 'GMSH_POS'
    para['FORMAT']  =  'GMSH'

    if 'version_fichier_gmsh' in  env.__dict__.keys() :
      para['VERSION'] = env.version_fichier_gmsh

    if 'TOUT_CMP' not in selection.nom_cmp :
      para['NOM_CMP'] = tuple(selection.nom_cmp)

    
    IMPR_RESU(
      MODELE = selection.contexte.modele,
      RESU   = para)
    FERMER(UNITE = 33)
    
    if a_detruire : 
      DETRUIRE(
        CONCEPT = _F(
          NOM = tuple(a_detruire)
          )
        )

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
      self.terminal.Axe_x(self.stan.nom_va)
      self.terminal.Axe_y(selection.nom_cham)
    elif selection.geom[0] == 'CHEMIN' :
      self.terminal.Axe_x('ABSC_CURV ' + selection.geom[1][0])
      self.terminal.Axe_y(selection.nom_cham)
      
      
    
  def Extract(self, selection) :
   
    """
      Execute les commandes aster de creation de la table pour GRACE
    """  
    
    l_courbes = []
    
   # Parametres communs a toutes les tables a calculer 
    para = _F(
      INTITULE   = 'TB_GRACE',
      RESULTAT   = selection.contexte.resultat,
      NOM_CHAM   = selection.nom_cham,
      OPERATION  = 'EXTRACTION'
      )
      
    if 'TOUT_CMP' in selection.nom_cmp :
      para['TOUT_CMP'] = 'OUI'
      l_nom_cmp = selection.etat_sd.cmp[selection.nom_cham]
    else :
      para['NOM_CMP'] = tuple(selection.nom_cmp)
      l_nom_cmp = selection.nom_cmp
      
      
    
    if selection.geom[0] == 'POINT' :
    
      para['NUME_ORDRE'] = selection.numeros
      
      for point in selection.geom[1] :
        para['GROUP_NO'] = point
        __GRACE = POST_RELEVE_T(ACTION = para)
        
        for comp in l_nom_cmp :
          courbe = as_courbes.Courbe()
          courbe.Lire_x(__GRACE,self.stan.nom_va)
          courbe.Lire_y(__GRACE,comp)
          nom = comp + ' --- ' + string.ljust(point,8)
          l_courbes.append( (courbe, nom) )
      
        DETRUIRE(CONCEPT = _F(NOM = __GRACE))
        
           
    elif selection.geom[0] == 'CHEMIN' :
    
      para['GROUP_NO'] = selection.etat_ma.Oriente(selection.geom[1][0])

      for no in selection.numeros :
        para['NUME_ORDRE'] = no,
        __GRACE = POST_RELEVE_T(ACTION = para)

        for comp in l_nom_cmp :
          courbe = as_courbes.Courbe()
          courbe.Lire_x(__GRACE, 'ABSC_CURV')
          courbe.Lire_y(__GRACE, comp)

          nom_va = self.stan.nom_va
          pos = self.stan.etat_sd.va['NUME_ORDRE'].index(no)
          val_va = self.stan.etat_sd.va[nom_va][pos]
          nom = comp + ' --- ' + nom_va + ' = ' + repr(val_va)
          l_courbes.append( (courbe, nom) )

        DETRUIRE(CONCEPT = _F(NOM = __GRACE))

    else : raise 'ERREUR_DVP'    

    return l_courbes
      
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
      
     Methode publique 
      Exec       : lancement du scan des evenements
      
     Attributs prives
      rootTk     : racine des objets Tk
      
     Methodes privees
      Action_evenement : actions consecutives au scan des evenements
      Dessin     : creation des objets graphiques Tk de l'interface  
  """


  def __init__(self) :   

    self.rootTk    = Tkinter.Tk()       
    self.rootTk.wm_title('PRE_STANLEY : choix des concepts')

    # Récupération des concepts Aster présents dans la base
    self.jdc_recupere=CONTEXT.get_current_step( )
    t_maillage=[]
    t_modele=[]
    t_evol=[]
    t_cham_mater=[]
    t_cara_elem=[]

    for i in self.jdc_recupere.sds_dict.keys( ):
#         print i,self.jdc_recupere.sds_dict[i].__class__.__name__,self.jdc_recupere.sds_dict[i]
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'maillage':
          t_maillage.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'modele':
          t_modele.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'evol_elas':
          t_evol.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'evol_noli':
          t_evol.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'evol_ther':
          t_evol.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'cham_mater':
          t_cham_mater.append( i )
        if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'cara_elem':
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
    self.modele = LIST_BOX(frame_modele,self.t_modele,Tkinter.SINGLE,self.t_modele[-1],fonte=env.fonte)
      
   # boite de saisie des champs
    frame_evol = Tkinter.Frame(frame_selection)
    frame_evol.pack(side=Tkinter.LEFT,padx=5)
    MENU_RADIO_BOX(frame_evol, "evol")
    self.evol = LIST_BOX(frame_evol,self.t_evol,Tkinter.SINGLE,self.t_evol[-1],fonte=env.fonte)
      
   # boite de saisie des champs
    frame_cham_mater = Tkinter.Frame(frame_selection)
    frame_cham_mater.pack(side=Tkinter.LEFT,padx=5)
    MENU_RADIO_BOX(frame_cham_mater, "cham_mater")
    self.cham_mater = LIST_BOX(frame_cham_mater,self.t_cham_mater,Tkinter.SINGLE,self.t_cham_mater[-1],fonte=env.fonte)
      
   # boite de saisie des champs
    if self.t_cara_elem != []:
      frame_cara_elem = Tkinter.Frame(frame_selection)
      frame_cara_elem.pack(side=Tkinter.LEFT,padx=5)
      MENU_RADIO_BOX(frame_cara_elem, "cara_elem")
      self.cara_elem = LIST_BOX(frame_cara_elem,self.t_cara_elem,Tkinter.SINGLE,self.t_cara_elem[-1],fonte=env.fonte)

   # Boutons
    BOUTON(frame_boutons,'PaleGreen1','LANCER STANLEY',self.Lancer)
    BOUTON(frame_boutons,'IndianRed1','SORTIR',self.rootTk.destroy)

