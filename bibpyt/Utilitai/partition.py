#@ MODIF partition Utilitai  DATE 29/11/2004   AUTEUR ASSIRE A.ASSIRE 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
# RESPONSABLE ASSIRE A.ASSIRE

import aster
import string, os, time, sys, UserList, types
import Numeric as N

from Accas import _F
from Noyau.N_utils import AsType

# ------------------------------------------------------------------ #

def enleve_doublons_liste(liste):
  """ A partir d une liste qui peut contenir des doublons, on renvoie une liste sans
      doublons (et qui reviendra triée)
  """

  """ Au cas ou ca ne serait pas deja un vecteur numpy """  # Exemple
  liste=N.sort(N.array(liste,copy=0))                 # [1, 2, 2, 3, 3, 4, 5]
  liste_diff=liste[1:]-liste[:-1]                     # [1, 0, 1, 0, 1, 1]
  liste_temp=N.nonzero(liste_diff)                    # [0, 2, 4, 5]
  liste_indice=N.zeros(liste_temp.shape[0]+1)
  liste_indice[0]=0
  liste_indice[1:]=liste_temp+1                       # [0, 1, 3, 5, 6]
  liste2=N.take(liste,liste_indice)                   # [1, 2, 3, 4, 5]
  return liste2


# ============================================================================ #


class CONNEC(UserList.UserList) :
  """
    Connectivite : sequence mutable de sequences de noeuds
    Pour l'instant, on s'appuie sur une liste de liste.
  """
  
  def __init__(self,nma) :
  
    UserList.UserList.__init__(self,[None]*nma)


  def Index(self) :
    """
      Retourne la connectivite sous forme de deux vecteurs numpy :
        ind -> tableau des index (y compris le n+1 eme)
        noe -> tableau des listes de noeuds
    """

    # Dimension des deux vecteurs
    nma  = len(self)
    ltot = N.reduce(lambda x,y : x+len(y), self,0)
    ind = N.zeros(nma+1,Int)
    noe = N.zeros(ltot,Int)

    # Construction des vecteurs
    ind[0] = 0
    for i in range(nma) :
      m = self[i]
      ind[i+1] = ind[i] + len(m)
      noe[ind[i]:ind[i+1]] = N.array(m)

    return ind,noe


# ============================================================================ #
# ============================================================================ #

class MAIL_PY :

  """
    SD PYTHON  MAILLAGE
    La numeration est 0..N-1 pour les noeuds et 0..M-1 pour les mailles
  """

  def __init__(self,nno=0,nma=0) :

    self.cn  = N.zeros((nno,3),N.Float)
    self.tm  = N.zeros(nma,N.Int)
    self.co  = CONNEC(nma)
    self.gma = {}
    self.gno = {}
    self.indice_noeuds = N.arange(nno)

    self.correspondance_noeuds = []
    self.correspondance_mailles = []

    try:
      import aster
      nom_mailles = (None,) + string.strip(aster.getvectjev('&CATA.TM.NOMTM'))
    except:
      nom_mailles = (None,
      'POI1',   'SEG2',   'SEG22',  'SEG3',   'SEG33',  'SEG4',   'TRIA3',
      'TRIA33', 'TRIA6',  'TRIA66', 'TRIA7',  'QUAD4',  'QUAD44', 'QUAD8',
      'QUAD88', 'QUAD9',  'QUAD99', 'TETRA4', 'TETRA10','PENTA6', 'PENTA15',
      'PYRAM5', 'PYRAM13','HEXA8',  'HEXA20', 'HEXA27', 'TR3QU4', 'QU4TR3',
      'TR6TR3', 'TR3TR6', 'TR6QU4', 'QU4TR6', 'TR6QU8', 'QU8TR6', 'TR6QU9',
      'QU9TR6', 'QU8TR3', 'TR3QU8', 'QU8QU4', 'QU4QU8', 'QU8QU9', 'QU9QU8',
      'QU9QU4', 'QU4QU9', 'QU9TR3', 'TR3QU9', 'SEG32',  'SEG23' )

    dic_mailles = {}
    for i in range(len(nom_mailles)) : 
      dic_mailles[nom_mailles[i]] = i

    self.nom = nom_mailles
    self.dic = dic_mailles

    try:
      psyco.bind(self.FromAster)
    except: pass


# -------------------------------------------------------------

  def get_connexite(self, nom, nma):
    co=CONNEC(nma)
    dico_connexite = aster.getcolljev(nom)
    for element in dico_connexite.keys() :
      co[int(element)-1] = (N.array(dico_connexite[element])-1)
    return co


  def get_coordonnees_noeuds(self, nom, nombre_noeuds):
    coordonnees_noeuds = aster.getvectjev(nom)
    coordonnees_noeuds = N.array(coordonnees_noeuds)
    coordonnees_noeuds.shape = (nombre_noeuds,3)
    cn = coordonnees_noeuds
    return cn


# -------------------------------------------------------------

  def FromAster(self,nom) :

    # On accepte le concept Aster ou bien la chaine texte de son nom
    if type(nom)!=types.StringType:
      nom_maillage = nom.nom
    else:
      nom_maillage = nom

    nom_maillage=string.ljust(nom_maillage,8)

    # recuperation de la taille
    self.dime_maillage = aster.getvectjev(nom_maillage+'.DIME')
    nombre_noeuds      = self.dime_maillage[0]
    nombre_mailles     = self.dime_maillage[2]    

    # coordonnees des noeuds
    self.cn = self.get_coordonnees_noeuds(nom_maillage+'.COORDO    .VALE', nombre_noeuds)

    # type de maille
    self.tm = N.array(aster.getvectjev(nom_maillage+'.TYPMAIL'))

    # connexite
    self.co = self.get_connexite(nom_maillage+'.CONNEX', nombre_mailles)

    self.indice_noeuds=N.arange(nombre_noeuds)

    # groupe de noeuds
    Lno_groupno_tot = aster.getcolljev(nom_maillage+'.GROUPENO')

    Lno_groupno={}
    for key in Lno_groupno_tot :
      # Tolerance car parfois defi_group crée des groupes nuls à clé entiere
      try:
        Lno_groupno[key.strip()]=N.array(Lno_groupno_tot[key])-1
      except: pass
    self.gno=Lno_groupno

    # groupe de mailles 
    Lma_groupma_tot     = aster.getcolljev(nom_maillage+'.GROUPEMA')
    Lma_groupma={}
    for key in Lma_groupma_tot :
      Lma_groupma[key.strip()]=N.array(Lma_groupma_tot[key])-1
    self.gma=Lma_groupma

    del(Lma_groupma_tot)

# -------------------------------------------------------------

  def ToAster(self) :

    try:
      INFO_EXEC_ASTER = self.jdc.get_cmd('INFO_EXEC_ASTER')
      DETRUIRE        = self.jdc.get_cmd('DETRUIRE')
      LIRE_MAILLAGE   = self.jdc.get_cmd('LIRE_MAILLAGE')
    except:
      try:
        from Cata.cata import INFO_EXEC_ASTER
        from Cata.cata import DETRUIRE
        from Cata.cata import LIRE_MAILLAGE
      except:
        print "\n\nERREUR : il faut lancer ce programme depuis Aster pour pouvoir \ngénérer un maillage Aster.\n\n"
        sys.exit(1)

    # Recuperation d'une unité logique libre
    _UL=INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
    ul1=_UL['UNITE_LIBRE',1]
    DETRUIRE(CONCEPT=(_F(NOM=_UL),), INFO=1)

    # Sauvegarde du maillage dans un fichier .mail
    fichier = 'fort.'+str(ul1)
    f = open(fichier, 'w')
    f.write( self.Voir_Mail() )
    f.close()

    # Récupération des concepts Aster présents dans la base
# Note AA : ne marche pas encore bien :
# Limitation : ne gere qu'un seul maillage genere par ToAster
#
#     nom='_MSH'
#     try:
#       self.jdc_recupere=CONTEXT.get_current_step()
#       t_maillage=[]
#       for i in self.jdc_recupere.sds_dict.keys( ):
#         if self.jdc_recupere.sds_dict[i].__class__.__name__ == 'maillage_sdaster':
#           if (mail[0:4]==nom):
#             t_maillage.append( i )
# 
#       num=len(_lst)+1
#     except:
#       num=0

    # Lecture de ce fichier .mail pour l'injecter dans l'espace Aster
    _SMESH_ = LIRE_MAILLAGE(UNITE = ul1)

    return(_SMESH_)

# -------------------------------------------------------------

  def __str__(self) :
    return self.Voir_Mail()

# -------------------------------------------------------------

  def Voir_Mail(self) :
    """
      Impression au format ASTER
    """

    l = []
    l.append('TITRE')
    l.append('%  CLASSE PY_MAIL -> MAIL')
    l.append('FINSF')
    l.append('%')

    (nno,ndim) = self.cn.shape
  
    l.append('COOR_3D')
    for i in range(nno) :
      ch = 'N'+repr(i)+'  '+`self.cn[i,0]` + '  ' + `self.cn[i,1]` + '  ' + `self.cn[i,2]`
      l.append(ch)

    ind = N.argsort(self.tm)
    ty = 0
    for m in ind :
      if self.tm[m] <> ty :
        l.append('FINSF') ; l.append('%')
        ty = self.tm[m]
        l.append(self.nom[ty])
      ch = 'M'+`m`+'  '
      for n in self.co[m] :
        ch = ch + 'N'+`n` + ' '
      l.append(ch)
    l.append('FINSF') ; l.append('%')

    entete = ['GROUP_MA','GROUP_NO']
    d_gp   = [self.gma,self.gno]
    pref   = ['M','N']  
    for (d_gp,entete,prefixe) in [(self.gma,'GROUP_MA','M'),(self.gno,'GROUP_NO','N')] :
      for gp in d_gp :
        l.append(entete)
        l.append('  ' + gp)
        ch = ' '
        i=0
        for o in d_gp[gp]:
          i+=1  # on ne met que 8 mailles sur une meme ligne
#          if len(ch) > 60:
          if (len(ch) > 60 or i>7):
            l.append(ch)
            ch = ' '
            i=0
          ch = ch + prefixe + `o` + ' '
        l.append(ch)
        l.append('FINSF') ; l.append('%')            

    l.append('FIN\n')
    return string.join(l,'\n')




# ============================================================================ #
# ============================================================================ #

class PARTITION:

  def __init__(self, jdc=None ,nb=0):

    self.jdc = jdc

    self.fichier_out        = ''
    self.liste_mailles      = N.array( [] )
    self.liste_sd           = N.array( [] )
    self.liste_mailles_bord = []
    self.liste_sd_bord      = []

    self.MAILLAGE_Python    = None

    self.RELATIONS      = { 'C_plus' : None,
                            'C_moins': None,
                            'nr': 0 }

    self.ASTER          = { 'MAILLAGE'        : None,
                            'MODELE'          : None,
                            'GROUP_MA'        : None,
                            'GROUP_MA_BORD'   : None,
                            'DICO_SD_MAILLES' : None,
                             }

    self.OPTIONS        = { 'NB_PART'     : '',
                            'ALGO'        : '',
                            'INFO'        : '',
                            'rep_metis'   : aster.repout(),
                            'exe_metis'   : aster.repout() + 'pmetis',
                            'fichier_in'  : 'fort.66',
                            'fichier_out' : 'fort.68',
                             }

    self.Creation_Dico_Correspondance_Type_Maille()



# ---------------------------------------------------------------------------- #

  def __str__(self) :
    """
      Impression du contenu de la partition
    """
    l = []
    l.append( 'Contenu de la partition :' )
    l.append( '-------------------------' )
    try: l.append( '- Maillage       : ' + str(self.ASTER['MAILLAGE'].nom) )
    except: pass
    try: l.append( '- Modele         : ' + str(self.ASTER['MODELE'].nom) )
    except: pass
    l.append( '- Nb part        : ' + str(self.OPTIONS['NB_PART']) )
    l.append( '- Niveau INFO    : ' + str(self.OPTIONS['INFO']) )
    l.append( '- Liste group_ma : ' + str(self.ASTER['GROUP_MA']) )

    return string.join(l,'\n')

# ---------------------------------------------------------------------------- #

  def Partitionne_Aster(self, MAILLAGE, NB_PART, MODELE=None, METHODE=None, LOGICIEL=None, INFO=1):

    self.t00 = time.clock()

    self.OPTIONS['INFO'] = INFO

    if MODELE:
      # Recuperation de la liste des mailles à perndre en compte
      self.ASTER['MODELE'] = MODELE
      self.ASTER['MAILLAGE'] = MAILLAGE
      _LST_MA = self.Modele_to_Liste_Mailles(MODELE)

    elif MAILLAGE:
      self.ASTER['MAILLAGE'] = MAILLAGE
      _LST_MA = None


    # Creation du maillage Python correspondant au maillage Aster
    MAILLAGE_Python = MAIL_PY()
    MAILLAGE_Python.FromAster(MAILLAGE.nom)

    # Partitionne le maillage Python avec la liste de mailles _LST_MA
    self.Partitionne_Maillage(MAILLAGE_Python, NB_PART, MAILLE=_LST_MA, METHODE=METHODE, LOGICIEL=LOGICIEL, INFO=INFO)

    return


# ---------------------------------------------------------------------------- #

  def Partitionne_Maillage(self, MAILLAGE_Python, NB_PART, MAILLE=None, METHODE=None, LOGICIEL=None, INFO=1):

    self.t00 =  time.clock()

    if METHODE:
      self.OPTIONS['exe_metis'] = aster.repout() + string.lower(METHODE)
    elif LOGICIEL:
      self.OPTIONS['exe_metis'] = LOGICIEL

    self.OPTIONS['NB_PART'] = NB_PART
    self.OPTIONS['INFO']    = INFO
    self.MAILLAGE_Python = MAILLAGE_Python

    exe_metis = self.OPTIONS['exe_metis']
    f_metis   = self.OPTIONS['fichier_in']
    fw_metis  = self.OPTIONS['fichier_out']

    _LST_MA = MAILLE

    # On initialise la connectivité et la connectivité inverse des aretes
    self.MAILLAGE_Python.ca = {}
    self.MAILLAGE_Python.cia = {}

    _DIM = self.MAILLAGE_Python.dime_maillage[5]
    _LST_TMA = self.MAILLAGE_Python.tm

    if self.OPTIONS['INFO']>=5:
      print 'cn=', self.MAILLAGE_Python.cn 
      print 'tm=', self.MAILLAGE_Python.tm 
      print 'co=', self.MAILLAGE_Python.co 
      print 'gma=', self.MAILLAGE_Python.gma
      print 'gno=', self.MAILLAGE_Python.gno
      print 'dim=', self.MAILLAGE_Python.dime_maillage
    if self.OPTIONS['INFO']>=5: print '_LST_MA=', _LST_MA

    # Liste des mailles à prendre en compte : dimension _DIM
    _D_DIM_MAILLES = self.Creation_Listes_Mailles_Par_Dim(self.MAILLAGE_Python.tm, _LST_MA=_LST_MA)

    # Connectivité et connectivité inverse sur les bords
    self.Connectivite_Aretes()

    self.liste_mailles = _D_DIM_MAILLES[ _DIM ]

    # Pour prendre en compte des mélanges d'elements de dimension differente
    _LST, _LST_BD = self.Elimination_Mailles_de_bords(MAILLAGE_Python, _D_DIM_MAILLES, _DIM)
    self.liste_mailles = N.concatenate( (self.liste_mailles,N.array(_LST)) )

    if self.OPTIONS['INFO']>=5:
      print '_LST_BD=',_LST_BD
      print '_LST=',_LST


    # Restriction des connectivités aux mailles à prendre en compte
    self.Connectivite_Aretes(OPTION='all', _LST_OK=self.liste_mailles)

    # Creation de l'arbre de connectivité des bords
    self.Creation_Graphe()

    # Reduction de l'arbre de connectivité des bords
    _nb = self.Reduction_Graphe(_DIM)

    # Ecriture du fichier pour Metis/Chaco/Jostle
    _D_CORRES = self.Ecrire_Graphe(f_metis, _nb)

    # Lancement de metis sur le fichier fort.UL (production de fort.UL.part.N)
    txt = exe_metis + ' ' + f_metis + ' ' + str(NB_PART)
    print 'Commande : ',txt
    os.system( txt )

    # Lecture du fichier resultant de Metis
    self.fichier_out = f_metis + '.part.' + str(NB_PART)
    self.liste_sd = self.Lecture_fichier_sdd(self.fichier_out, self.liste_mailles)

    # Traitement des mailles de bords (on les reinjecte dans un SD)
    self.Affectation_Mailles_de_bords(_LST_BD, _DIM)

    t1 = time.clock()
    print "--- FIN PARTITIONNEMENT : ", t1 - self.t00

    return



# ---------------------------------------------------------------------------- #

  def Creation_Dico_Correspondance_Type_Maille(self):

    # TYPE_ELEM : CF. &CATA.TM
    #   1 - >POI1    <>SEG2    <>SEG22   <>SEG3    <>SEG33   <>SEG4    <>TRIA3   <
    #   8 - >TRIA33  <>TRIA6   <>TRIA66  <>TRIA7   <>QUAD4   <>QUAD44  <>QUAD8   <
    #  15 - >QUAD88  <>QUAD9   <>QUAD99  <>TETRA4  <>TETRA10 <>PENTA6  <>PENTA15 <
    #  22 - >PYRAM5  <>PYRAM13 <>HEXA8   <>HEXA20  <>HEXA27  <>TR3QU4  <>QU4TR3  <
    #  29 - >TR6TR3  <>TR3TR6  <>TR6QU4  <>QU4TR6  <>TR6QU8  <>QU8TR6  <>TR6QU9  <
    #  36 - >QU9TR6  <>QU8TR3  <>TR3QU8  <>QU8QU4  <>QU4QU8  <>QU8QU9  <>QU9QU8  <
    #  43 - >QU9QU4  <>QU4QU9  <>QU9TR3  <>TR3QU9  <>SEG32   <>SEG23   <

    # Creation du dictionnaire des correspondance type_maille -> liste des aretes
    maille2aretes={}
    # POI
    maille2aretes[1]  = [ ]
    # SEG
    maille2aretes[2]  = [ (0,1) ]
    maille2aretes[3]  = maille2aretes[4] = maille2aretes[5] = maille2aretes[6] = maille2aretes[2]
    # TRIA
    maille2aretes[7]  = [ (0,1),(1,2),(0,2) ]
    maille2aretes[8]  = maille2aretes[9] = maille2aretes[10] = maille2aretes[11] = maille2aretes[7]
    # QUAD
    maille2aretes[12] = [ (0,1),(1,2),(2,3),(0,3) ]
    maille2aretes[13] = maille2aretes[14] = maille2aretes[15] = maille2aretes[16] = maille2aretes[17] = maille2aretes[12]
    # TETRA
    maille2aretes[18] = [ (0,1,2),(0,1,3),(0,2,3),(1,3,2) ]
    maille2aretes[19] = maille2aretes[18]
    # PENTA
    maille2aretes[20] = [ (0,1,2),(3,4,5),(0,2,5,3),(0,1,4,3),(2,1,4,5) ]
    maille2aretes[21] = maille2aretes[20]
    # PYRAM
    maille2aretes[22] = [ (0,1,4),(1,2,4),(2,3,4),(3,0,4),(0,1,2,3) ]
    maille2aretes[23] = maille2aretes[22]
    # HEXA
    maille2aretes[24] = [ (0,1,2,3), (4,5,6,7), (1,2,6,5), (2,3,7,6), (7,4,0,3), (4,5,1,0) ]
    maille2aretes[25] = maille2aretes[26] =  maille2aretes[24]


    # dictionnaire de correspondance entre type_maille -> nb noeud (maille linéaire)
    maille2nb={}
    # POI
    maille2nb[1]  = 1
    # SEG
    maille2nb[2]  = 2
    maille2nb[3]  = maille2nb[4] = maille2nb[5] = maille2nb[6] = maille2nb[2]
    # TRIA
    maille2nb[7]  = 3
    maille2nb[8]  = maille2nb[9] = maille2nb[10] = maille2nb[11] = maille2nb[7]
    # QUAD
    maille2nb[12] = 4
    maille2nb[13] = maille2nb[14] = maille2nb[15] = maille2nb[16] = maille2nb[17] = maille2nb[12]
    # TETRA
    maille2nb[18] = 4
    maille2nb[19] = maille2nb[18]
    # PENTA
    maille2nb[20] = 5
    maille2nb[21] = maille2nb[20]
    # PYRAM
    maille2nb[22] = 5
    maille2nb[23] = maille2nb[22]
    # HEXA
    maille2nb[24] = 6
    maille2nb[25] = maille2nb[26] =  maille2nb[24]


    # dictionnaire de correspondance entre type_maille -> dimension
    maille2dim = {}
    # POI
    maille2dim[1]  = 0
    # SEG
    maille2dim[2]  = 1
    maille2dim[3]  = maille2dim[4] = maille2dim[5] = maille2dim[6] = maille2dim[2]
    # TRIA
    maille2dim[7]  = 2
    maille2dim[8]  = maille2dim[9] = maille2dim[10] = maille2dim[11] = maille2dim[7]
    # QUAD
    maille2dim[12] = 2
    maille2dim[13] = maille2dim[14] = maille2dim[15] = maille2dim[16] = maille2dim[17] = maille2dim[12]
    # TETRA
    maille2dim[18] = 3
    maille2dim[19] = maille2dim[18]
    # PENTA
    maille2dim[20] = 3
    maille2dim[21] = maille2dim[20]
    # PYRAM
    maille2dim[22] = 3
    maille2dim[23] = maille2dim[22]
    # HEXA
    maille2dim[24] = 3
    maille2dim[25] = maille2dim[26] =  maille2dim[24]

    # On stocke les dictionnaires
    self.maille2aretes = maille2aretes
    self.maille2nb     = maille2nb
    self.maille2dim    = maille2dim

    return


# ---------------------------------------------------------------------------- #

  def Modele_to_Liste_Mailles(self, MODELE):

    nommod = string.ljust(MODELE.nom,8)
    _DIC_MA  = aster.getcolljev(nommod.ljust(8)+'.MODELE    .LIEL')

    # Creation de la liste des mailles
    ll = []
    for type_maille in _DIC_MA.keys():
      ll.extend( _DIC_MA[type_maille][0:-1] )
    _LST_MA = N.array( ll ) - 1

    if self.OPTIONS['INFO']>=5:
      print '\n# ----- MODELE ----- #\n'
      print '_LST_MA=',len(_LST_MA),_LST_MA
      print '_DIC_MA=',len(_DIC_MA),_DIC_MA

    return _LST_MA


# ---------------------------------------------------------------------------- #

  def Creation_Listes_Mailles_Par_Dim(self, _LST_TMA, _LST_MA=None):

    t0 = time.clock()

    # Si _LST_MA est renseigné on extrait la liste des TMA correspondante aux mailles de _LST_MA
    if _LST_MA != None:
      _LST_TMA = N.take(_LST_TMA,_LST_MA)
    else:
      _LST_MA = N.arange(len(_LST_TMA))

    _D_DIM_MAILLES = {}

    # Liste des mailles 3D (type maille de 18 à 26)
    _lst = N.where( _LST_TMA>=18, -3, _LST_TMA )
    _tmp = N.where( _lst==-3, -1, 0 )
#    _D_DIM_MAILLES[3] = N.nonzero( _tmp )
    _D_DIM_MAILLES[3] = N.take(_LST_MA, N.nonzero( _tmp ) )

    # Liste des mailles 2D (type maille de 7 à 17)
    _lst = N.where( _lst>=7, -2, _lst )
    _tmp = N.where( _lst==-2, -1, 0 )
    _D_DIM_MAILLES[2] = N.take(_LST_MA, N.nonzero( _tmp ) )

    # Liste des mailles 1D (type maille de 2 à 6)
    _lst = N.where( _lst>=2, -1, _lst )
    _tmp = N.where( _lst==-1, -1, 0 )
    _D_DIM_MAILLES[1] = N.take(_LST_MA, N.nonzero( _tmp ) )

    # Liste des mailles 0D (type maille 1)
    _lst = N.where( _lst>=1, -4, _lst )
    _tmp = N.where( _lst==-4, -1, 0 )
    _D_DIM_MAILLES[0] = N.take(_LST_MA, N.nonzero( _tmp ) )


    if self.OPTIONS['INFO']>=5:
      for i in _D_DIM_MAILLES.keys():
        print "-----------------"
        print 'Dim:',i, _D_DIM_MAILLES[i]
      print "-----------------"

    print "--- FIN Creation des listes de mailles par Dim : ", time.clock() - t0

    return _D_DIM_MAILLES


# ---------------------------------------------------------------------------- #

  def Connectivite_Aretes(self, OPTION=None, _LST_OK=None):

    t0 = time.clock()

    # Si _LST_OK n'est pas renseigné on prend toutes les mailles
    if not _LST_OK: _LST_OK = N.arange(len(self.MAILLAGE_Python.tm))

    if self.OPTIONS['INFO']>=5: print '_LST_OK (ca)=',_LST_OK

    maille2aretes = self.maille2aretes

    # Creation de la :
    #   - connectivite des aretes (self.MAILLAGE_Python.ca) : m1 -> [ (a1, a2), .. ]
    #   - connectivite inverse des aretes (self.MAILLAGE_Python.cia) : (a1, a2) -> [ m1, m2, ... ]

    self.MAILLAGE_Python.ca  = {}
    self.MAILLAGE_Python.cia = {}

    for n in _LST_OK:
    
        n1 = self.MAILLAGE_Python.tm[n]

        l_aretes = maille2aretes[n1]           # liste des aretes de la maille n
        l_noeuds = self.MAILLAGE_Python.co[n]  # liste des noeuds de la maille n

        for arete in l_aretes:
          ll = []
          for i in arete:
            ll.append( l_noeuds[i] )
          ll.sort()
          ll = tuple(ll)

          # Table de connectivité des aretes
          if OPTION:
            if not self.MAILLAGE_Python.ca.has_key(n): self.MAILLAGE_Python.ca[n]=[]
            self.MAILLAGE_Python.ca[n].append(ll)
#             try:
#               self.MAILLAGE_Python.ca[n].append(ll)
#             except KeyError:
#               self.MAILLAGE_Python.ca[n]=[ll]

          # Table de connectivité inverse des aretes
          if not self.MAILLAGE_Python.cia.has_key(ll): self.MAILLAGE_Python.cia[ll]=[]
          self.MAILLAGE_Python.cia[ll].append(n)
#           try:
#             self.MAILLAGE_Python.cia[ll].append(n)
#           except KeyError:
#             self.MAILLAGE_Python.cia[ll]=[n]


    if self.OPTIONS['INFO']>=5: 
      for k in self.MAILLAGE_Python.cia.keys():
        print 'cia:',k, '     ', self.MAILLAGE_Python.cia[k]
      if OPTION:
        for k in self.MAILLAGE_Python.ca.keys():
          print 'ca: ',k, '     ', self.MAILLAGE_Python.ca[k]


    print "--- FIN Creation de la connectivite simple et inverse des aretes : ", time.clock() - t0

    return


# ---------------------------------------------------------------------------- #

  def Elimination_Mailles_de_bords(self, MAILLAGE_Python, _D_DIM_MAILLES, _DIM):
    """
    Extraction des mailles de bords (mailles incluses dans un bord d une autre maille)
    """

    t0 = time.clock()

    _LST_TMA = self.MAILLAGE_Python.tm

    if self.OPTIONS['INFO']>=5:
      MAILLAGE = self.ASTER['MAILLAGE']
      nommail = string.ljust(MAILLAGE.nom,8)
      _LST_MAI = aster.getvectjev(nommail.ljust(8)+'.NOMMAI')

    # Le dico maille2nb donne le nombre de noeuds definissant un bord (lineaire)
    maille2nb = self.maille2nb


    # construction des listes des mailles de dim N-1 :
    # _LST_OK :   Mailles de dim N-i qui ne sont pas un bord des mailles de dim N
    # _LST_BD :   Mailles de dim N-i qui sont un bord
    #
    if self.OPTIONS['INFO']>=5: print '\n\nElimination des mailles de bord de DIM', _DIM - 1

    _LST4 = _D_DIM_MAILLES[ _DIM - 1 ]
    _LST_IND = N.arange( len(_LST4) ) + 1  # on ajoute 1 pour eviter le premier 0 dans les test nonzero plus bas

    if self.OPTIONS['INFO']>=5: print '  Mailles concernées=',_LST4

    i=0
    for m in _LST4:
      if self.OPTIONS['INFO']>=5: print '\n  Maille de dim N-1:',m, ' Aster:',string.strip(_LST_MAI[m]), ' TMA:',self.MAILLAGE_Python.tm[m], ' CO:',self.MAILLAGE_Python.co[m], '(noeuds de cette maille)'
      nb = maille2nb[ self.MAILLAGE_Python.tm[m] ]
      ll = self.MAILLAGE_Python.co[m][0:nb]
      ll = N.sort(ll)
      ll = ll.tolist()
      ll = tuple(ll)
      if self.OPTIONS['INFO']>=5: print '  Bord (lineaire)', ll, nb

      try:
        if self.OPTIONS['INFO']>=5: print '  CIA=', self.MAILLAGE_Python.cia[ ll ], '(mailles de dim N qui ont cette maille pour bord)'
        _tmp=[]
        for maille in self.MAILLAGE_Python.cia[ ll ]:
          if self.OPTIONS['INFO']>=5: print '  Maille N:', maille, 'Aster:', string.strip(_LST_MAI[maille]), ' TMA:', self.MAILLAGE_Python.tm[maille]
#        self.liste_mailles_bord.append(m)
      except:
        if self.OPTIONS['INFO']>=5: print '  Maille non-bord'
        _LST_IND[i] = 0

      i+=1

    # Recuperation des mailles de bords et non-bords
    _LST_BD = N.nonzero(_LST_IND)
    _LST_BD = N.take(_LST4,_LST_BD)

    _LST_OK = N.where( _LST_IND==0, 1 , 0 )
    _LST_OK = N.nonzero(_LST_OK)
    _LST_OK = N.take(_LST4,_LST_OK)

    if self.OPTIONS['INFO']>=5: print '\nListe Maille de bords de DIM', _DIM - 1,' :',_LST_BD
    if self.OPTIONS['INFO']>=5: print 'Liste Maille de DIM', _DIM - 1,'qui ne sont pas des bords :',_LST_OK

    print "--- FIN Maille de bords de DIM",_DIM - 1, " : ", time.clock() - t0
    t0 = time.clock()


    # On cherche à marier les mailles de dimension N-2, N-3
    # Peut etre lent car on utilise la connectivité ! Mais pour le moment on a rien d'autre.

    _LST_BD0 = []
    _LST_OK0 = []
    _D_BD = {}
    for d in range(_DIM-1):
      _LST4 = _D_DIM_MAILLES[ d ]
      if self.OPTIONS['INFO']>=5: print '\n\nElimination des mailles de bord de DIM', d
      if self.OPTIONS['INFO']>=5: print '  Mailles concernées=',_LST4
      for mai in _LST4:
        if self.OPTIONS['INFO']>=5: print '\n  Maille:', mai, ' Aster:',string.strip(_LST_MAI[mai]), ' TMA:',self.MAILLAGE_Python.tm[mai], ' CO:',self.MAILLAGE_Python.co[mai], '(noeuds de cette maille)'

        nb = maille2nb[ self.MAILLAGE_Python.tm[mai] ]
        ll = self.MAILLAGE_Python.co[mai][0:nb]
        ll = N.sort(ll)
        ll = ll.tolist()
        _tmp = tuple(ll)
#        _tmp = self.MAILLAGE_Python.co[mai]

        if self.OPTIONS['INFO']>=5: print '  Bord (lineaire):', _tmp, nb

        ok=0
        for arete in self.MAILLAGE_Python.cia:
          _nb=0
          for noe in _tmp:
            if noe in arete: _nb+=1
          if _nb == len(_tmp):
            if self.OPTIONS['INFO']>=5: print '  Maille N+i:', self.MAILLAGE_Python.cia[arete], '- Arete:', arete
            _LST_BD0.append( mai )
            ok=1
#             if not _D_BD.has_key( mai ): _D_BD[mai] = []
#             _D_BD[mai].append( self.MAILLAGE_Python.cia[arete] )
            break
        if ok == 0:
          _LST_OK0.append( mai )

#        print 'Mai:',mai, '_D_BD[mai]=',_D_BD[mai]


      if self.OPTIONS['INFO']>=5: print '\nListe Maille de bords de DIM', d,' :',_LST_BD0
      if self.OPTIONS['INFO']>=5: print 'Liste Maille de DIM', d,'qui ne sont pas des bords :',_LST_OK0


      print '--- FIN Maille de bords de DIM', d, ' :',time.clock() - t0
      t0 = time.clock()


    _LST_OK = N.concatenate( (_LST_OK, N.array(_LST_OK0)) )
    _LST_BD = N.concatenate( (_LST_BD, N.array(_LST_BD0)) )

    if self.OPTIONS['INFO']>=5: print '\nTotal:\nListe Maille de bords=',_LST_BD
    if self.OPTIONS['INFO']>=5: print 'Liste Maille non-bords=',_LST_OK,'\n'

#    print "--- FIN Maille de bords 3 : ", time.clock() - t0

    return _LST_OK, _LST_BD


# ---------------------------------------------------------------------------- #

  def Affectation_Mailles_de_bords(self, _LST_BD, _DIM):
    """
    Affectation a un SD des mailles de bords (mailles incluses dans un bord d une autre maille)
    """

    if self.OPTIONS['INFO']>=5:
      print 'liste_mailles_bord=', self.liste_mailles_bord
      print 'liste_sd_bord', self.liste_sd_bord
      print '_LST_BD=',_LST_BD


    MAILLAGE = self.ASTER['MAILLAGE']
    _LST_TMA = self.MAILLAGE_Python.tm
    
    if self.OPTIONS['INFO']>=5:
      nommail = string.ljust(MAILLAGE.nom,8)
      _LST_MAI = aster.getvectjev(nommail.ljust(8)+'.NOMMAI')

    t0 = time.clock()

    # Affectation des mailles de bords à chacun des SD

    # Le dico maille2nb donne le nombre de noeuds definissant un bord (lineaire)
    maille2nb = self.maille2nb

    i = 0
    for m in _LST_BD:
      if self.OPTIONS['INFO']>=5: print '\n  Maille de dim N-1:',m, ' Aster:',string.strip(_LST_MAI[m]), ' TMA:',self.MAILLAGE_Python.tm[m], ' CO:',self.MAILLAGE_Python.co[m], '(noeuds de cette maille)'
      nb = maille2nb[ self.MAILLAGE_Python.tm[m] ]
      ll = self.MAILLAGE_Python.co[m][0:nb]
      ll = N.sort(ll)
      ll = ll.tolist()
      ll = tuple(ll)
      if self.OPTIONS['INFO']>=5: print '  Bord (lineaire)', ll, nb

      # Cas particulier des POI1 en 2D et 3D (ils ne peuvent etre des bords d'elements 2D ou 3D)
      if ( (nb==1) and (_DIM>=2) ):
        _tmp=[]
        for arete in self.MAILLAGE_Python.cia.keys():
          if ll[0] in arete:
            for maille in self.MAILLAGE_Python.cia[ arete ]:
              if self.OPTIONS['INFO']>=5: print '  Maille N+i:', maille, ' Aster:',string.strip(_LST_MAI[maille]), ' Arete:', arete
              _tmp.append( self.liste_sd[maille] )

      # Cas particulier des SEG en 3D (ils ne peuvent etre des bords d'elements 3D)
      elif ( (nb==2) and (_DIM==3) ):
        _tmp=[]
        for arete in self.MAILLAGE_Python.cia.keys():
          _nb=0
          for noe in ll:
            if noe in arete: _nb+=1
          if _nb == len(ll):
            for maille in self.MAILLAGE_Python.cia[arete]:
              if self.OPTIONS['INFO']>=5: print '  Mailles N+i:', maille, ' Aster:',string.strip(_LST_MAI[maille]), ' Arete:', arete
              _tmp.append( self.liste_sd[maille] )

      # Autres mailles de bord
      else:
        if self.OPTIONS['INFO']>=5: print '  CIA=', self.MAILLAGE_Python.cia[ ll ], '(mailles de dim N qui ont cette maille pour bord)'
        _tmp=[]
        for maille in self.MAILLAGE_Python.cia[ ll ]:
          if self.OPTIONS['INFO']>=5: print '  Maille N+i:', maille, 'Aster:', string.strip(_LST_MAI[maille]), ' SD:', self.liste_sd[maille], ' TMA:', self.MAILLAGE_Python.tm[maille]
          _tmp.append( self.liste_sd[maille] )

      # integre la maille au SD le plus faible (pour que des groupes de bords se retrouvent dans le meme SD)
      _tmp.sort()
      self.liste_mailles_bord.append(m)
      self.liste_sd_bord.append( _tmp[0] )
      i += 1
      if self.OPTIONS['INFO']>=5: print '  ---> Maille:',m,'integree au SD:', _tmp[0]

    if self.OPTIONS['INFO']>=5:
      print '\n\nliste_mailles_bord=', self.liste_mailles_bord
      print 'liste_sd_bord=', self.liste_sd_bord


    print "--- FIN Affectation des mailles de bords : ", time.clock() - t0

    return


# ---------------------------------------------------------------------------- #

  def Creation_Graphe(self):

    t0 = time.clock()

    # Creation du graphe complet
    self.GRAPH = {}

    for mai in self.liste_mailles:
      _ll=[]
      for are in self.MAILLAGE_Python.ca[mai]:
        _ll.extend( self.MAILLAGE_Python.cia[are] )
        _mm = enleve_doublons_liste(_ll)  # coute cher!
      _tmp = _mm.tolist()
      _tmp.remove(mai)
      self.GRAPH[mai] = _tmp

      if self.OPTIONS['INFO']>=5: print 'self.GRAPH['+str(mai)+']=', self.GRAPH[mai]

    print "--- FIN Creation du graphe complet : ", time.clock() - t0

    return


# ---------------------------------------------------------------------------- #

  def Reduction_Graphe(self, _DIM):

    t0 = time.clock()

    # Elimination des connectivités à interface nulle
    maille2dim = self.maille2dim
    _lst2 = []
    for mai in self.liste_mailles:
      if self.OPTIONS['INFO']>=5: print '\nmai:', mai, 'co:', self.MAILLAGE_Python.co[mai], 'tm:', self.MAILLAGE_Python.tm[mai]
      _DIM1 = maille2dim[ self.MAILLAGE_Python.tm[mai] ]
      _tmp2 =[]
      for mai2 in self.GRAPH[mai]:
        if self.OPTIONS['INFO']>=5: print 'mai2:', mai2, 'co:', self.MAILLAGE_Python.co[mai2], 'tm:', self.MAILLAGE_Python.tm[mai2]
        # calcule le nombre de noeuds communs aux deux mailles
        _nb = 0
        for noe in self.MAILLAGE_Python.co[mai2]:
          if noe in self.MAILLAGE_Python.co[mai]: _nb += 1
        _DIM2 = maille2dim[ self.MAILLAGE_Python.tm[mai2] ]
        if _nb >= min(_DIM1, _DIM2):  # le min permet de faire du collage 3D-coque par exemple
          _tmp2.append( mai2 )
          _tmp = [mai, mai2]
          _tmp.sort()
          _lst2.append(_tmp)
      self.GRAPH[mai] = _tmp2

    print "--- FIN Elimination des connectivités avec une interface nulle : ", time.clock() - t0
    t0 = time.clock()


    # Calcul du nombre d'aretes
    # A voir : normalement il n'y a rien a faire car nb0 = 2*nb (a verifier...)
    _lst2.sort()
    _v = _lst2[0]
    _nb = 1
    for i in _lst2:
      if i != _v:
        _v = i
        _nb += 1


    if self.OPTIONS['INFO']>=5:
      print '----------------------------------------------'
      for mai in self.liste_mailles:
        print 'self.GRAPH['+str(mai)+']=', self.GRAPH[mai]
      print '----------------------------------------------'

    return _nb


# ------------------------------------------------------------------ #

  def Ecrire_Graphe(self, f_metis, _nb):

    t0 = time.clock()

    # On doit renumeroter les mailles qui arrivent dans self.liste_mailles pour avoir 0... N-1
    _D_CORRES = {}
    for i in N.arange(len(self.liste_mailles)):
      _D_CORRES[ self.liste_mailles[i] ] = i

    # Ecriture du fichier fort.UL pour metis
    fw = open(f_metis,'w')
    fw.write( str(len(self.liste_mailles)) + ' ' +  str(_nb) + '\n')
    for l in self.liste_mailles:
#      try:
        _tmp = []
        for t in self.GRAPH[l]:
          try:
            t = _D_CORRES[t]
            _tmp.append( str(t+1) )    # Necessaire car metis numerote de 1 à N
          except:
            print 'on oublie le bord:', t
        fw.write( string.join(_tmp, ' ') + '\n' )
#      except: 
#        print 'Probleme ecriture graphe! On continue..'
    fw.close()

    print "--- FIN Ecriture du fichier du graphe pour metis : ", time.clock() - t0

    return _D_CORRES


# ---------------------------------------------------------------------------- #

  def Lecture_fichier_sdd(self, fichier, _LST_OK):

    t0 = time.clock()

    # Lecture du fichier produit par metis (partie a optimiser)
    try:
      f = open( fichier, 'r' )
    except:
      print "\n\n          ERREUR: le fichier est introuvable! Le partitionneur \n          ne s'est probablement pas lancé.\n\n"
      sys.exit(1)
    else:
      _tmp = []
      for l in f.readlines():
        _tmp.append( int(string.strip(l)) )
      f.close()
      _l_domaines = N.array(_tmp,copy=0)

      # Pour garder le fichier metis
      os.system( 'mv ' + fichier + ' REPE_OUT/' )

      if self.OPTIONS['INFO']>=5: print '_l_domaines=',_l_domaines
  
      print "--- FIN Lecture du fichier produit par metis : ", time.clock() - t0

    return _l_domaines


# ---------------------------------------------------------------------------- #

  def Creation_Group_ma_Python_par_SD(self, NOM='SD', NOM2='B'):

    t0 = time.clock()

    NB_PART = self.OPTIONS['NB_PART']

    # Creation du dictionnaire des listes des mailles par SD
    #     d_gma : { num sd -> [ liste mailles ] }
    d_gma = {}
    for i in range(NB_PART):
      d_gma[i] = []

    i=0
    for sdd in self.liste_sd:
      d_gma[sdd].append( self.liste_mailles[i] )
      i+=1


    # Creation du dictionnaire des listes des mailles de bord par SD
    #     d_gma_bord : { num sd -> [ liste mailles ] }
    d_gma_bord = {}
    for i in range(NB_PART):
      d_gma_bord[i] = []

    i=0
    for sdd in self.liste_sd_bord:
      d_gma_bord[sdd].append( self.liste_mailles_bord[i] )
      i+=1


    # Generation des listes de noms de groupes
    _l_sd = []
    _l_bord = []
    for i in range(NB_PART):
      if d_gma[i] != []:
        _l_sd.append( NOM + str(i) )
      if d_gma_bord[i] != []:
        _l_bord.append( NOM2 + str(i) )

    # Stockage
    self.ASTER['GROUP_MA']        = _l_sd
    self.ASTER['GROUP_MA_BORD']   = _l_bord


    # Creation des groupes de mailles dans le Maillage Python
    for i in range(NB_PART):
      self.MAILLAGE_Python.gma[NOM+str(i)]  = d_gma[i]
      self.MAILLAGE_Python.gma[NOM2+str(i)] = d_gma_bord[i]

    print "--- FIN creation du dictionnaire des listes des mailles par SD ", time.clock() - t0

    return


# ---------------------------------------------------------------------------- #

  def Creation_Group_ma_Aster_par_SD(self, NOM='SD', NOM2='B', INCLUSE='NON'):

    t0 = time.clock()

    MAILLAGE    = self.ASTER['MAILLAGE']
    NB_PART     = self.OPTIONS['NB_PART']

    nommail     = string.ljust(MAILLAGE.nom,8)
    _LST_MAI    = aster.getvectjev(nommail.ljust(8)+'.NOMMAI')


    # Creation du dictionnaire des listes des mailles par SD
    #     d_gma : { num sd -> [ liste mailles ] }
    d_gma = {}
    for i in range(NB_PART):
      d_gma[i] = []

    m=0
    for sdd in self.liste_sd:
      d_gma[sdd].append( string.strip(_LST_MAI[ self.liste_mailles[m] ]) )   # voir si le strip coute cher !
      m += 1


    # Creation du dictionnaire des listes des mailles de bord par SD
    #     d_gma_bord : { num sd -> [ liste mailles ] }
    d_gma_bord = {}
    for i in range(NB_PART):
      d_gma_bord[i] = []

    # On inclus directement les mailles de bords dans les SD
    if INCLUSE=='OUI':
      m=0
      for sdd in self.liste_sd_bord:
        d_gma[sdd].append( string.strip(_LST_MAI[ self.liste_mailles_bord[m] ]) )   # voir si le strip coute cher !
        m+=1

    else:
      m=0
      for sdd in self.liste_sd_bord:
        d_gma_bord[sdd].append( string.strip(_LST_MAI[ self.liste_mailles_bord[m] ]) )   # voir si le strip coute cher !
        m+=1


    print "--- FIN creation du dictionnaire des listes des mailles par SD ", time.clock() - t0
    t0 = time.clock()


    # Creation et lancement de la commande DEFI_GROUP associée
    try:
      DEFI_GROUP = self.jdc.get_cmd('DEFI_GROUP')
    except:
      try:
        from Cata.cata import DEFI_GROUP
      except:
        print "\n\nERREUR : il faut lancer ce programme depuis Aster pour pouvoir \ngénérer les groupes de mailles Aster.\n\n"
        return

    _tmp  = []
    _l_sd = []
    _l_bord = []
    for i in range(NB_PART):
      if d_gma[i] != []:
        _tmp.append( {'MAILLE': d_gma[i],'NOM': NOM + str(i)} )
        _l_sd.append( NOM + str(i) )
      if d_gma_bord[i] != []:
        _tmp.append( {'MAILLE': d_gma_bord[i],'NOM': NOM2 + str(i)} )
        _l_bord.append( NOM2 + str(i) )

    motscle2= {'CREA_GROUP_MA': _tmp }

    DEFI_GROUP( reuse=MAILLAGE,
                MAILLAGE=MAILLAGE,
                INFO=1,
                **motscle2
               ) ;

    # Stockage
    self.ASTER['DICO_SD_MAILLES'] = d_gma
    self.ASTER['GROUP_MA']        = _l_sd
    self.ASTER['GROUP_MA_BORD']   = _l_bord

    print "--- FIN Creation et lancement de la commande DEFI_GROUP associée : ", time.clock() - t0

    return

# ---------------------------------------------------------------------------- #
