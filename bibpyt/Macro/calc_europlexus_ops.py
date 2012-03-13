#@ MODIF calc_europlexus_ops Macro  DATE 13/03/2012   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

#-----------------------------------------------------------------------   
#----------------------------- Importation des modules  ----------------
#-----------------------------------------------------------------------   

# unite associe au fichier ou le post-traitement CASTEM2000 est fait en commandes epx
# unite_cast2000 = 0 # il ne fait pas le pos-traitement
# __temp
unite_cast2000 = 95

debug = False

import types,string
import os
import numpy
import math 
import copy
import tempfile
from pprint import pprint

# Protection pour Eficas
try:
   import aster
   from Accas import _F
   from Utilitai.partition import MAIL_PY
   from Utilitai.Utmess import UTMESS
except:
  pass
      
#----------------------------- Precision ------------------------------- 
tst = 1.0E-10

def float2str(f):
    """ Imprime un float avec 6 chiffres significatifs dans le fichier EPX """
    return "%.6e" % f


#-----------------------------------------------------------------------   
#----------------------------- Fonctions de calcul vectoriel -----------
#-----------------------------------------------------------------------   

def norme(u) :
  return numpy.sqrt(numpy.dot(u,u))

def vecteurs_egaux(v1,v2):
  diff = v1-v2
  #print 'v1',v1
  #print 'v2',v2
  for v in diff:
    if abs(v) > tst : return False
  return True  

#-----------------------------------------------------------------------   
#----------------------------- Operateur de la Macro-commande ----------
#-----------------------------------------------------------------------   

 
def calc_europlexus_ops(self,MODELE,CARA_ELEM,CHAM_MATER,EXCIT,FONC_PARASOL=None,
                             DIME=None,OBSERVATION=None,ARCHIVAGE=None,COURBE=None,
                             CALCUL=None,DOMAINES=None,INTERFACES=None,INFO=1,**args):
        
  ier=0
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  # On importe les definitions des commandes a utiliser dans la macro
  # Le nom de la variable doit etre obligatoirement le nom de la commande
  
  global _F,INFO_EXEC_ASTER,DETRUIRE,IMPR_RESU,DEFI_FICHIER,LIRE_RESU,CREA_MAILLAGE
  global DEFI_GROUP,LIRE_MAILLAGE,CREA_TABLE,IMPR_TABLE,AFFE_MODELE,EXEC_LOGICIEL
  global LIRE_CHAMP,CREA_CHAMP,CREA_RESU,FORMULE
  
  INFO_EXEC_ASTER = self.get_cmd('INFO_EXEC_ASTER')
  DETRUIRE        = self.get_cmd('DETRUIRE')
  IMPR_RESU       = self.get_cmd('IMPR_RESU')
  DEFI_FICHIER    = self.get_cmd('DEFI_FICHIER')
  LIRE_RESU       = self.get_cmd('LIRE_RESU')
  CREA_MAILLAGE   = self.get_cmd('CREA_MAILLAGE')
  DEFI_GROUP      = self.get_cmd('DEFI_GROUP')
  LIRE_MAILLAGE   = self.get_cmd('LIRE_MAILLAGE')
  CREA_TABLE      = self.get_cmd('CREA_TABLE')
  IMPR_TABLE      = self.get_cmd('IMPR_TABLE')
  AFFE_MODELE     = self.get_cmd('AFFE_MODELE')
  EXEC_LOGICIEL   = self.get_cmd('EXEC_LOGICIEL')
  LIRE_CHAMP      = self.get_cmd('LIRE_CHAMP')
  CREA_CHAMP      = self.get_cmd('CREA_CHAMP')
  CREA_RESU       = self.get_cmd('CREA_RESU')
  FORMULE         = self.get_cmd('FORMULE')

  # Pour la gestion des Exceptions
  prev_onFatalError = aster.onFatalError()
  aster.onFatalError('EXCEPTION')


  # Pour masquer certaines alarmes
  from Utilitai.Utmess import UTMESS, MasquerAlarme, RetablirAlarme
  MasquerAlarme('MED_1')
  MasquerAlarme('MED_54')
  MasquerAlarme('MED_77')
  MasquerAlarme('MED_37')

  MasquerAlarme('MED_98')
  MasquerAlarme('ALGELINE4_43')
  MasquerAlarme('JEVEUX_57')



  # Ligne de commande d'Europlexus
  if args.has_key('LOGICIEL'): EXEC = args['LOGICIEL']
  else: EXEC = '/home/europlex/EPXD/bin/europlexus'
  if debug: print 'args_keys : %s'%args.keys()
  if args.has_key('PAS_NBRE_COURBE') :
     if debug: print 'PAS NBRE COURBE = ok (%s)'%args['PAS_NBRE_COURBE']
  else :
     if debug: print 'PAS NBRE COURBE = nook'
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  # Chemin du repertoire REPE_OUT de l'execution courante d'Aster
  REPE_OUT = os.path.join(os.getcwd(), 'REPE_OUT')

  # Chemin du repertoire temporaire pour l'execution d'EPX (un lien vers REPE_OUT)
  REPE_epx    = tempfile.mkdtemp(suffix='_epx')
  os.rmdir(REPE_epx)
  os.symlink(REPE_OUT, REPE_epx)

  # classs Europlexus permettant de faire le chainage avec le Code_Aster
  EPX = EUROPLEXUS(MODELE,CARA_ELEM,CHAM_MATER,FONC_PARASOL,EXCIT,DIME,
                   OBSERVATION,ARCHIVAGE,COURBE,CALCUL,DOMAINES,INTERFACES,
                   REPE='REPE_OUT',EXEC=EXEC, INFO=INFO, REPE_epx=REPE_epx, args=args)

  # Eriture du fichier de commande Europlexus
  EPX.ecrire_fichier()

  # On ne lance pas le calcul Europlexus...
  if args.has_key('LANCEMENT') and args['LANCEMENT']=='NON': return ier

  # Lancement du calcul Europlexus
  EPX.lancer_calcul()

  # Recuperer le resulat Europlexus a l'aide d'un concept aster de type evol_noli
  fichier_med = 'auto'
  fichier_pun = 'auto'

  self.DeclareOut('resu',self.sd) # Le concept sortant (de type evol_noli) est nomme 'resu'   
  global resu # pour que le nom du concept prenne celui defini par l'utilisateur
  EPX.get_resu(fichier_med=fichier_med)

  # Recuperer les concepts table
  if COURBE is not None:
    global table
    self.DeclareOut('table',args['TABLE_COURBE'])
    EPX.get_table()


  # Pour la gestion des Exceptions
  aster.onFatalError(prev_onFatalError)

  # Pour la gestion des alarmes
  RetablirAlarme('MED_1')
  RetablirAlarme('MED_54')
  RetablirAlarme('MED_77')
  RetablirAlarme('MED_37')

  RetablirAlarme('MED_98')
  RetablirAlarme('ALGELINE4_43')
  RetablirAlarme('JEVEUX_57')

  # Suppression du lien symbolique
  os.remove(REPE_epx)

  return ier

#-----------------------------------------------------------------------   
#----------------------------- class EUROPLEXUS ------------------------
#-----------------------------------------------------------------------   

class EUROPLEXUS:
  def __init__(self,MODELE,CARA_ELEM,CHAM_MATER,FONC_PARASOL,EXCIT,DIME,OBSERVATION,ARCHIVAGE,COURBE,CALCUL,DOMAINES,INTERFACES,REPE,EXEC,INFO,REPE_epx,args):

    if debug: print 'args_key %s'%args.keys()
    # Mettre toutes les entrees en attributs
    self.DIME = DIME
    self.MODELE = MODELE
    self.CARA_ELEM = CARA_ELEM
    self.CHAM_MATER = CHAM_MATER
    self.FONC_PARASOL = FONC_PARASOL
    self.EXCIT = EXCIT
    self.OBSERVATION = OBSERVATION
    self.ARCHIVAGE = ARCHIVAGE
    self.COURBE = COURBE
    self.CALCUL = CALCUL
    self.DOMAINES = DOMAINES
    self.INTERFACES = INTERFACES
    self.INFO = INFO

    self.REPE_epx = REPE_epx
    self.pwd = os.getcwd()

    # Commande d'execution de Europlexus
    self.EXEC   = EXEC
    
    if args.has_key('UNITE_COURBE'): self.UNITE_COURBE = args['UNITE_COURBE']
    else:                            self.UNITE_COURBE = None
    
    if args.has_key('PAS_INST_COURBE'): self.PAS_INST_COURBE = args['PAS_INST_COURBE']
    else:                            self.PAS_INST_COURBE = None
   
    if args.has_key('PAS_NBRE_COURBE'): self.PAS_NBRE_COURBE = args['PAS_NBRE_COURBE']
    else:                            self.PAS_NBRE_COURBE = None

    if args.has_key('TABLE_COURBE'): self.TABLE_COURBE = args['TABLE_COURBE']
    else:                            self.TABLE_COURBE = None
    # Dictionnaire contenant le texte associé au fichier de commande Europlexus
    self.epx = {}

    if debug: print 'pas nbre courbe = %s'%self.PAS_NBRE_COURBE
    
    # Concept pour un nouveau maillage si on utilise RIGI_PARASOL ou 
    # si on regroupe separement les mailles tria3 et quad4
    self.NEW_MA = None

    # Nom des fichiers de Europlexus (commande et sorties)
    self.nom_fichiers = {'COMMANDE' : 'commandes.epx',
                         #'MAILLAGE' : 'maillage_epx',
                         'MAILLAGE' : 'commandes',
                         'ALIT'     : 'champ.alit',
                         'MED'      : 'champ.e2m',
                         'PUN'      : 'courbes.pun',
                        }
    
    

#-----------------------------------------------------------------------       
  def get_unite_libre(self,):

    _UL=INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
    unite=_UL['UNITE_LIBRE',1]
    DETRUIRE(CONCEPT=(_F(NOM=_UL),), INFO=1)
    return(unite)
    
     
#-----------------------------------------------------------------------   
  def recupere_structure(self,concept,mot_cle=None):

    try : structure = concept.etape.valeur
    except : UTMESS('F','PLEXUS_1') 

    if mot_cle : 
      if structure.has_key(mot_cle) : return structure[mot_cle]
      else : return None 
    return structure
#-----------------------------------------------------------------------   
  def get_motcle(self,fact,mot_cle,code_mess='F'):
    try : 
      out = fact[mot_cle]
    except : 
      out = None
    if not out is None : return out    
    if debug: print "get_motcle/%s=%s" % (mot_cle, str(out))
    if not code_mess is None and code_mess: UTMESS(code_mess,'PLEXUS_2',valk=mot_cle)
    return None

#-----------------------------------------------------------------------   
  def get_group_ma(self,fact):
    group_ma = self.get_motcle(fact,'GROUP_MA',code_mess='F')
    if types.TypeType(group_ma) == types.StringType : group_ma = (group_ma,)
    # elif types.TypeType(group_ma) == types.ListType   : group_ma = tupel(group_ma)  
    return group_ma
    
#-----------------------------------------------------------------------   
  def get_group_ma_f(self,fact):
    group_ma = self.get_motcle(fact,'GROUP_MA',None)
    if types.TypeType(group_ma) == types.StringType : group_ma = (group_ma,)
    # elif types.TypeType(group_ma) == types.ListType   : group_ma = tupel(group_ma) 
    if not group_ma : group_ma=[]
    return group_ma
    
#-----------------------------------------------------------------------   
  def setlist(self,fact):
    #print 'types.TypeType(fact)',types.TypeType(fact)
    if fact is None : return fact
    #if not types.TypeType(fact) in [types.ListType,types.TupleType] : fact= [fact]
    if not (isinstance(fact,types.ListType) or isinstance(fact,types.TupleType))  : fact= [fact]
    return fact
    
#-----------------------------------------------------------------------   
  def lire_fichier(self,fichier,):
    fd = open(fichier,'r')
    lignes = fd.readlines()
    fd.close()
    valeurs = []
    commentaires = ['#','%','$','*']
    for ll in lignes:
      add = 1
      for comment in commentaires : 
        if string.find(ll,comment) != -1 : add = 0;break
      if add :   
         data = [val for val in ll.split(None)]
         valeurs.append(data)
    return valeurs
    
#-----------------------------------------------------------------------   
  def fichier2dic(self,fichier):
    valeurs = self.lire_fichier(fichier)
    dic = {}
    mot_cle = None
    for ll in valeurs:
      if len(ll) > 1 :    
        for mot in ll :
          try : 
            val = float(mot)
            if not mot_cle is None : dic[mot_cle].append(val)
          except : 
            mot_cle = mot
            dic[mot_cle] = []
            
    return dic
    
#-----------------------------------------------------------------------   
  def lire_pun(self,fichier):
    if debug: print 'lire_pun : fichier = %s'%fichier
    data  = self.lire_fichier(fichier)
    icourbe = 0
    courbes = {}
    for ligne in data:
      if debug: print 'lire_pun : %s'%ligne
      if ligne[0] in ('VALEURS','VALUES'):
         icourbe += 1;
         nb = int(ligne[1])
         courbes[icourbe] = []
      else:
        ligne_vale = [float(val) for val in ligne]
        courbes[icourbe].append(ligne_vale)
    for courbe in courbes :
        courbes[courbe]=numpy.transpose(numpy.array(courbes[courbe]))
    return courbes
    
#-----------------------------------------------------------------------   
  def export_DEBUT(self):
    
    epx = self.epx
    
    # Cle identifiant 
    MODULE = 'DEBUT'
    
    # Entete de la commande Europlexus courante
    epx[MODULE] = ['*--FICHIER CREE PAR CALC_EUROPLEXUS/Code_Aster']
    
    # Texte declarant le debut
    epx[MODULE].append('TITRE')
    epx[MODULE].append('ECHO')
    options = 'TRID NONL'
# AA    champ_fact = self.ECRITURE['CHAMP']
    champ_fact = self.ARCHIVAGE
    if champ_fact is not None : options += ' MEDE'
    epx[MODULE].append(options)
    epx[MODULE].append('\n')
    
    # __temp
    fichier_cast2000 = 'fort.%i' %unite_cast2000
    if unite_cast2000 and os.path.isfile(fichier_cast2000) :
      sortie_cast2000 = os.path.join(self.REPE_epx, 'post.k2000')

      epx[MODULE].append('OPNF 12') 
      epx[MODULE].append(2*' ' + "'%s'" %sortie_cast2000)
      epx[MODULE].append('\n')

#-----------------------------------------------------------------------   
  def export_MAILLAGE(self,format='CASTEM'):

    epx = self.epx

    # Cle identifiant 
    MODULE = 'MAILLAGE'

    # Entete de la commande Europlexus courante
    epx[MODULE] = ['*--LECTURE MAILLAGE']

    # Recuperer le concept maillage modifie ou initial 
    if self.NEW_MA is None :
      concept_maillage = self.recupere_structure(self.MODELE,'MAILLAGE')
    else :
      concept_maillage = copy.copy(self.NEW_MA) 

    # Ecrire le maillage, qui peut etre a ete enrichi avec des commandes Aster, sous le format souhaite (MED ou CASTEM)
    unite = self.get_unite_libre()

    # Extensions des formats de maillage 
    extension = {'CASTEM' : '.msh', 'MED':'.med'}

    # donner un nom au fichier de maillage parce que le fort.unite peut etre ecrase par d'autre operation d'ecriture
    nom_fichier = self.nom_fichiers['MAILLAGE'] + extension[format]
    fichier_maillage = self.REPE_epx + os.sep + nom_fichier

    DEFI_FICHIER(UNITE=unite, FICHIER=fichier_maillage, ACTION='ASSOCIER')
    IMPR_RESU(UNITE  = unite,
              FORMAT = format,
              RESU   = _F(MAILLAGE=concept_maillage)
             )
    DEFI_FICHIER(UNITE=unite,ACTION='LIBERER');    

    epx[MODULE].append('%s TOUT' % format)

#    epx[MODULE].append('%s '%format)
#    epx[MODULE].append("'%s' TOUT" % fichier_maillage)    
#    epx[MODULE].append("NTMPMA TOUT")    

    epx[MODULE].append('\n')    

#-----------------------------------------------------------------------   
  def export_DIME(self,):
    epx = self.epx

    # Cle identifiant 
    MODULE = 'DIME'

    # Entete de la commande Europlexus courante
    epx[MODULE] = ['*--DIMENSIONNEMENT']
    epx[MODULE].append('\n')

    # Test si des parametres de dime seront introuduites à l'aide d'un fichier externe 
    try    : unite_dime = self.DIME['UNITE_DIME']
    except : unite_dime = None

    # Recuperer le dictionnaire issu du fichier associe à UNITE_DIME (s'il existe)
    if unite_dime is not None : 
      fort = 'fort.%i' %unite_dime
      dic_fichier = self.fichier2dic(fort)
    else:
      dic_fichier = {}

    # mettre a jour les cles definis dans le fichier par celles qui sont 
    # definies directement sous le mot-cle facteur DIME
    cles = ['Q4GS', 'FORCE', 'PT6L', 'ZONE', 'POUT', 
            'ECRO', 'APPU', 'BLOQ', 'PRESS', 'PMAT', 
            'FTAB', 'DKT3', 'DEPL', 'FNOM', 'TABLE', 
            'LIAI', 'MTTI', 'PT1L','NEPE']

    for cle in cles:
      try:
        if self.DIME[cle] is not None : 
          dic_fichier[cle] = self.DIME[cle]
      except : pass  

    # Imprimer les mot-cles et leurs dans epx
    epx[MODULE].append('DIME')
    for cle in dic_fichier:
      vale = dic_fichier[cle]
      st = 5*' ' + cle
      for v in vale:
        st += ' %i' %v
      epx[MODULE].append(st)  
    epx[MODULE].append('TERM')    

    epx[MODULE].append('\n')
         
#-----------------------------------------------------------------------   
  def export_MODELE(self):
    
    epx = self.epx
    
    # Cle identifiant 
    MODULE = 'MODELE'
    
    epx[MODULE] = ['*--DEFINITION DE LA GEOMETRIE']
    
    # Entete de la commande Europlexus courante
    epx[MODULE].append('GEOMETRIE')
    
    # Recuperer la structure sous le mot_cle facteur AFFE de AFFE_MODELE 
    affe_modele = self.recupere_structure(self.MODELE,'AFFE')
    affe_modele = self.setlist(affe_modele)
    
 
    # Correspondance de modelisation aster/europlexus
    dic_modele = {'DKT':'DKT3','DKTG':'Q4GS','POU_D_E':'POUT'}
    
    # Correspondance de modelisation europlexus/aster
    dic_modele_epx = {}
    for cle in dic_modele.keys():
      dic_modele_epx[dic_modele[cle]] = cle

    # initialisation du dictioannaire qui contient les group_ma en fonction de la modelisation
    dic_gma = {}
    for cle in dic_modele.values():
      dic_gma[cle] = []
      
    for affe in affe_modele:
      modelisation = affe['MODELISATION']
      phenomene = affe['PHENOMENE']
      if  phenomene == 'MECANIQUE' and modelisation in dic_modele.keys():
        if affe.has_key('GROUP_MA') :
          group_ma = self.get_group_ma(affe)
          dic_gma[dic_modele[modelisation]].extend(group_ma)
        else : 
          UTMESS('F','PLEXUS_3') 
    
    # Regrouper separement les mailles tria3 et quad4 de la modilisation DKT (s'il y a lieu)
    # parce que leurs modilisations dans europlexus sont differentes:
    #                                                                tria3 : DKT3
    #                                                                quad4 : Q4GS
    
    # Initialisation du dictionnaire contenant les elements du modele de type TRIA3 et QUAD4
    nom_groups= {'TRIA3':[],'QUAD4':[]}
    
    if len(dic_gma['DKT3']) > 0:
      concept_maillage = self.recupere_structure(self.MODELE,'MAILLAGE')
      class_dkt = DKT(MAILLAGE=concept_maillage,)
      nom_groups = class_dkt.aster2epx(groups=dic_gma['DKT3'])
      if debug: print 'nom_groups = %s'%nom_groups
      # mettre a jour les groups de chaque modelisation
      dic_gma['DKT3'] = nom_groups['TRIA3']
      dic_gma['Q4GS'].extend(nom_groups['QUAD4'])
      
    
    # liste comportant les modelisations definis dans le module GEOMETRIE
    # cette liste va servir à determiner les indices des champs stockes dans le fichier med (get_resu)
    self.modelisations = []
    # Ecriture sous format europlexus
    for modelisation in  dic_gma.keys() :
      len_groups = len(dic_gma[modelisation])
      if len_groups > 0 :
        self.modelisations.append(modelisation)
        epx[MODULE].append(5*' ' + modelisation + 2*' ' + dic_gma[modelisation][0])
        if len_groups > 1 :
          for ig in range(1,len_groups) :
            group_ma = dic_gma[modelisation][ig]
            epx[MODULE].append((len(modelisation)+5+2)*' ' + group_ma)

                
    
    # Mettre a jour le modele dans le cas ou le maillage a ete modifie pour la seperation de TRIA3 et QUAD4
    # ce modele va etre utilise dans la lecture du fichier med (get_resu)
    if len(nom_groups['QUAD4']) == 0 :
      self.NEW_MODELE = copy.copy(self.MODELE)
    else :
      affe_model = []
      for modelisation in self.modelisations :
         affe_model.append({'GROUP_MA':dic_gma[modelisation],
                           'MODELISATION': dic_modele_epx[modelisation],
                           'PHENOMENE':'MECANIQUE' })
        
      __MO = AFFE_MODELE(MAILLAGE = concept_maillage,
                          AFFE = affe_model,
                          );
        
      self.NEW_MODELE = copy.copy(__MO);
    
    # Utiliser dans Ecriture des elements et points du fichier med    
    self.dic_gma = dic_gma
    # Il y a une suite dans RIGI_PARSOL
    
    #epx[MODULE].append('TERM')
    #epx[MODULE].append('\n') 
    
    
 #-----------------------------------------------------------------------   
  def export_CARA_ELEM(self):
    
    epx = self.epx
    
    # Cle identifiant 
    MODULE = 'CARA_ELEM'

    # Recuperer la structure du concept sorti de AFFE_CARA_ELEM
    cara_elem_struc = self.recupere_structure(self.CARA_ELEM)

    epx[MODULE] = ['*--CARACTERISTIQUES DES ELEMENTS DE STRUCTURE']

    # Entete de la commande Europlexus courante
    epx[MODULE].append('COMPLEMENT')

    # Dictionnaire conteant les donnees associees aux differents elements de structure
    dic_elem = {}
    for cle in ['DISCRET','COQUE','POUTRE','RIGI_PARASOL']:
        if cara_elem_struc.has_key(cle):
            fact = cara_elem_struc[cle]
            if fact :
                dic_elem[cle] = self.setlist(fact)

    if debug:
       print "AA/dic_elem="
       pprint(dic_elem)

    # traduire les masses concentrees si elles existent
    cle = 'DISCRET'
    self.discretParasol = {}
    self.discretParasolAmor = {}
    if dic_elem.has_key(cle):
        epx[MODULE].append('\n') 
        epx[MODULE].append('*--MASSES AJOUTEES')
        for elem in dic_elem[cle]:
            if debug: print "AA/elem="
            if elem['CARA'] == 'M_T_D_N' :
                group_ma = self.get_group_ma(elem)
                vale = elem['VALE']
                epx[MODULE].append('MASSE  123456 %s' %vale)
                epx[MODULE].append(7*' ' + 'LECT')
                for group in group_ma:
                    epx[MODULE].append(11*' '+group)
                epx[MODULE].append(7*' ' + 'TERM')
            if elem['CARA'] == 'K_TR_D_N' :
                group_ma = self.get_group_ma(elem)
                vale     = elem['VALE']
                for gma in group_ma :
                    self.discretParasol[gma] = vale
            if elem['CARA'] == 'A_TR_D_N' :
                group_ma = self.get_group_ma(elem)
                vale     = elem['VALE']
                for gma in group_ma :
                    self.discretParasolAmor[gma] = vale

    # traduire les elements coques s'ils existent
    cle = 'COQUE'
    self.dicOrthotropie={}
    self.listEpais=[]
    self.listEpais2=[]
    if dic_elem.has_key(cle):
        epx[MODULE].append('\n') 
        epx[MODULE].append('*--ELEMENTS COQUES')
        for elem in dic_elem[cle]:
            group_ma = self.get_group_ma(elem)
            epais = elem['EPAIS']
            if self.ARCHIVAGE['CONT_GENER'] == 'OUI' :
                self.listEpais.append({'VALE' : epais, 'GROUP_MA' : group_ma, 'NOM_CMP' : 'X21'})
                self.listEpais2.append({'VALE' : epais**2/6., 'GROUP_MA' : group_ma, 'NOM_CMP' : 'X22'})
            else :
                self.listEpais.append({'VALE' : 1., 'GROUP_MA' : group_ma, 'NOM_CMP' : 'X21'})
                self.listEpais2.append({'VALE' : 1., 'GROUP_MA' : group_ma, 'NOM_CMP' : 'X22'})
            st = 'EPAIS  %s  ' %epais
            #ajouter les group_ma qui ont ete affecte par ces caracteristiques
            epx[MODULE].append(st+'LECT')
            for group in group_ma:
                epx[MODULE].append((len(st)+4)*' '+group)
            epx[MODULE].append((len(st))*' '+'TERM')
            if elem.has_key('VECTEUR'):
                for group in group_ma :
                    self.dicOrthotropie[group] = elem['VECTEUR']

    # traduire les elements poutres s'ils existent
    cle = 'POUTRE'
    if dic_elem.has_key(cle):
        concept_maillage = self.recupere_structure(self.MODELE,'MAILLAGE')
        # classe permettant de calculer et verifier les vecteurs de poutre dans Europlexus
        class_poutre = POUTRE(MAILLAGE=concept_maillage,CARA_ELEM=self.CARA_ELEM)
        epx[MODULE].append('\n') 
        epx[MODULE].append('*--ELEMENTS POUTRES')
        for elem in dic_elem[cle]:       
            group_ma = self.get_group_ma(elem)
            vecteurs = class_poutre.getvecteurs(group_ma,verif='non')
            vect_y = vecteurs[group_ma[0]]
            type_section = elem['SECTION']
            st = 'GEOP %s ' %type_section
            if type_section == 'RECTANGLE' :
                # Correspondance de caracteristique de poutre aster/europlexus
                dic_corresp = {'HY':'AY','HZ':'AZ'}
                epx[MODULE].append(st)
                # ajouter le vecteur definisant l'orientation de la section
                epx[MODULE].append(len(st)*' ' + 'VX %s' %vect_y[0])
                epx[MODULE].append(len(st)*' ' + 'VY %s' %vect_y[1])
                epx[MODULE].append(len(st)*' ' + 'VZ %s' %vect_y[2])
                # ajouter les caracteristiques de la poutre
                cara = elem['CARA']
                vale = elem['VALE']
                for icar in range(len(cara)):
                  car = cara[icar]
                  val = vale[icar]
                  car_epx = dic_corresp[car]
                  epx[MODULE].append(len(st)*' ' + '%s %s' %(car_epx,val))
                # ajouter les group_ma qui ont ete affecte par ces caracteristiques
                epx[MODULE].append(5*' '+'LECT')
                for group in group_ma:
                  epx[MODULE].append(len(st)*' '+group)
                epx[MODULE].append(5*' '+'TERM')     
            else : 
                UTMESS('A','PLEXUS_4',valk=type_section) 
      
    epx[MODULE].append('\n') 

#-----------------------------------------------------------------------   
  def export_RIGI_PARASOL(self):

    epx = self.epx
    
    # Cle identifiant 
    MODULE = 'RIGI_PARASOL'
    concept_maillage = self.recupere_structure(self.MODELE,'MAILLAGE')
    self.MApyt = MAIL_PY()
    self.MApyt.FromAster(concept_maillage)

    # Recuperer la structure du concept sorti de AFFE_CARA_ELEM
    cara_elem_struc = self.recupere_structure(self.CARA_ELEM)

    # Recuperer la structure RIGI_PARASOL si elles existe
    list_rigi_parasol = None
    if cara_elem_struc.has_key('RIGI_PARASOL'):
        fact = cara_elem_struc['RIGI_PARASOL']
        if fact: list_rigi_parasol = self.setlist(fact)

    # Traduction des fonctions definies dans FONC_PARASOL    
    #dic_fonc_parasol = self.get_FONC_PARASOL(list_rigi_parasol)
    dic_fonc_parasol = self.get_FONC_PARASOL()
#    if not dic_fonc_parasol: return

    # Verifications croisees : AFFE_CARA_ELEM / RIGI_PARASOL versus CALC_EUROPLEXUS / FONC_PARASOL
    if list_rigi_parasol:
        list_cara_rigi_parasol = list_rigi_parasol[0]['CARA']
        if 'K_TR_D_N' in list_cara_rigi_parasol:
            for f in ['NFKT', 'NFKR']:
                if not f in dic_fonc_parasol.keys():
                    UTMESS('A','PLEXUS_15',valk=('K_TR_D_N', f) )
        if 'A_TR_D_N' in list_cara_rigi_parasol:
            for f in ['NFAT', 'NFAR']:
                if not f in dic_fonc_parasol.keys():
                    UTMESS('A','PLEXUS_15',valk=('A_TR_D_N', f) )
    
        for f in ['NFKT', 'NFKR']:
            if f in dic_fonc_parasol.keys() and not 'K_TR_D_N' in list_cara_rigi_parasol:
                UTMESS('F','PLEXUS_16',valk=('K_TR_D_N', f) )
        for f in ['NFAT', 'NFAR']:
            if f in dic_fonc_parasol.keys() and not 'A_TR_D_N' in list_cara_rigi_parasol:
                UTMESS('F','PLEXUS_16',valk=('K_TR_D_N', f) )



    epx[MODULE] = ['*--SUPPORT ELASTIQUE']

    # si l'utilisateur a lui meme fourni des GROUP_MA de discrets : on commence par la
    gmaParasol=self.get_group_ma_f(self.FONC_PARASOL)
    group_union = ''

    if debug:
        print 'AA2=', dic_fonc_parasol.keys(), dic_fonc_parasol
        print 'AA3=', gmaParasol

    for gma in gmaParasol :
        if gma not in self.discretParasol :
            if debug: print 'export_RIGI_PARASOL/gma=', gma
            if debug: print 'export_RIGI_PARASOL/self.discretParasol.keys()=', self.discretParasol.keys()
            raise 'Discret non defini'
        epx[MODULE].append('SUPPORT')
        if 'NFKT' in dic_fonc_parasol.keys() and 'NFKR' in dic_fonc_parasol.keys():
            vale = [ float2str(x) for x in self.discretParasol[gma] ]
            epx[MODULE].append(6*' '+'KX  %s KY  %s KZ  %s NFKT %s' % ( vale[0], vale[1], vale[2], dic_fonc_parasol['NFKT'] ) )
            epx[MODULE].append(6*' '+'KRX %s KRY %s KRZ %s NFKR %s' % ( vale[3], vale[4], vale[5], dic_fonc_parasol['NFKR'] ) )
        if 'NFAT' in dic_fonc_parasol.keys() and 'NFAR' in dic_fonc_parasol.keys():
            vale = [ float2str(x) for x in self.discretParasolAmor[gma] ]
            epx[MODULE].append(6*' '+'AX  %s AY  %s AZ  %s NFAT %s' % ( vale[0], vale[1], vale[2], dic_fonc_parasol['NFAT'] ) )
            epx[MODULE].append(6*' '+'ARX %s ARY %s ARZ %s NFAR %s' % ( vale[3], vale[4], vale[5], dic_fonc_parasol['NFAR'] ) )

        epx[MODULE].append(6*' '+'LECT %s TERM'% gma)
        group_union += '%s '% gma
    if len(gmaParasol) > 0 :
        epx['MODELE'].append(5*' ' + 'APPU  ' + group_union)


    # traduire les rigi_parasol
    if list_rigi_parasol and dic_fonc_parasol:
        ressorts, amorts = self.CARA_ELEM.toEPX()
        if debug:
           print "AA2:ressorts", ressorts
           print "AA2:amorts", amorts

        # Creer un maillage qui, pour chaque noeud de l'interface utilisee dans rigi_parasol, associe un group_ma associe
        # parce que on n'a pas les memes numeros des noeuds dans Europlexus
        crea_poi1 = []
        dic_gma = {}
        igr = -1
        # Liste des group_ma associes au noeuds ou le RIGI_PARSOL a ete applique 
        groups_parasol = []

        for noeud in ressorts.keys() :
            # noeud = ressor[0]
            if not dic_gma.has_key(noeud):
              igr += 1
              group_ma = 'SUP%i' %igr
              dic_gma[noeud] = group_ma
              groups_parasol.append(group_ma)
              crea_poi1.append(_F(NOEUD=noeud,NOM_GROUP_MA=group_ma)) 

        # ATTENTION : a deplacer
        # crea_maillage a faire de facon centraliser pour RIGI_PARASOL et le POST-TRAITEMENT ailleurs
        courbe_fact = self.COURBE
        if courbe_fact is None: courbe_fact = []
        lnoeuds=set()
        lmailles=set()
        dicma=[]
        for courbe in courbe_fact :
            if courbe['GROUP_NO'] != None :
                noeuds = courbe['GROUP_NO']
                if type(noeuds) == tuple :
                    for el in noeuds :
                        lnoeuds.add(el)
                else :
                    lnoeuds.add(noeuds)
            if courbe['GROUP_MA'] != None :
                mailles = courbe['GROUP_MA']
                if type(mailles) == tuple :
                    for el in mailles :
                        lmailles.add(el)
                else :
                    lmailles.add(mailles)
        for maille in lmailles :
            dicma.append({'NOM' : maille, 'MAILLE' : maille})

        for no in lnoeuds :
            if not self.MApyt.gno.has_key(no) :
                crea_poi1.append(_F(NOEUD=no,NOM_GROUP_MA=no)) 

        __NEW = CREA_MAILLAGE(MAILLAGE=concept_maillage,
                              CREA_POI1 = crea_poi1,
                              );
        self.NEW_MA = copy.copy(__NEW)


        # Ecriture sous format europlexus
        for noeud in ressorts :
            epx[MODULE].append('SUPPORT')

            # ecriture des composantes et de leurs valeurs associees
            if 'NFKT' in dic_fonc_parasol.keys() and 'NFKR' in dic_fonc_parasol.keys():
                vale = [ float2str(x) for x in ressorts[noeud] ]
                epx[MODULE].append(6*' '+'KX  %s KY  %s KZ  %s NFKT %s' % ( vale[0], vale[1], vale[2], dic_fonc_parasol['NFKT'] ) )
                epx[MODULE].append(6*' '+'KRX %s KRY %s KRZ %s NFKR %s' % ( vale[3], vale[4], vale[5], dic_fonc_parasol['NFKR'] ) )

            if 'NFAT' in dic_fonc_parasol.keys() and 'NFAR' in dic_fonc_parasol.keys():
                vale = [ float2str(x) for x in amorts[noeud] ]
                epx[MODULE].append(6*' '+'AX  %s AY %s  AZ  %s NFAT %s'  % ( vale[0], vale[1], vale[2], dic_fonc_parasol['NFAT'] ) )
                epx[MODULE].append(6*' '+'ARX %s ARY %s ARZ %s NFAR %s' % ( vale[3], vale[4], vale[5], dic_fonc_parasol['NFAR'] ) )

            # ecriture du group_ma associe
            group_ma = dic_gma[noeud]
            epx[MODULE].append(3*' ' + 'LECT %s TERM' % group_ma) 

        epx[MODULE].append('\n')

        group_union = 'PARASOL'
        dicma.append({'NOM' : group_union, 'UNION' : groups_parasol})
        __NEW = DEFI_GROUP(reuse = __NEW, 
                  MAILLAGE = __NEW,
                  CREA_GROUP_MA = dicma
                  )     

        # Mettre a jour le module modele qd les ressorts existent
        epx['MODELE'].append(5*' ' + 'APPU  ' + group_union)

    # Fermer le module modele dans tous les cas 
    self.multidomaine = 0
    listInterfaces = self.INTERFACES
    self.gmaInterfaces=[]
    if listInterfaces :
        i=0
        self.multidomaine = 1
        for interface in listInterfaces :
            Lgma1 = self.setlist(interface['GROUP_MA_1'])
            Lgma2 = self.setlist(interface['GROUP_MA_2'])
            self.gmaInterfaces.extend(Lgma1)
            self.gmaInterfaces.extend(Lgma2)
            for gma in Lgma1 :
                if i==0 :
                    epx['MODELE'].append(5*' ' + 'CL3L' + 2*' ' + gma)
                else :
                    epx['MODELE'].append(11*' ' + gma)
            for gma in Lgma2 :
                    epx['MODELE'].append(11*' ' + gma)
            i+=1
    epx['MODELE'].append('TERM')
    epx['MODELE'].append('\n')

    if self.gmaInterfaces :
        epx['CHAM_MATER'].append('FANTOME 0. LECT')
        for gma in self.gmaInterfaces :
            epx['CHAM_MATER'].append(15*' '+'%s'% gma)
        epx['CHAM_MATER'].append('           TERM')
    epx['CHAM_MATER'].append('\n')   
    
    
    # DEFINITION REPERES ORTHOTROPIE QUI DOIT ETRE APRES LES SUPPORTS
    for gma in self.gmaGLRC :
        if gma not in self.dicOrthotropie :
            raise Exception("Erreur : %s non affecte par un repere d'orthotropie : ajouter le mot-cle 'VECTEUR' dans AFFE_CARA_ELEM"% gma)
        vale = self.dicOrthotropie[gma]
        epx[MODULE].append('COMP ORTS %s %s %s'%(vale[0],vale[1],vale[2]))
        epx[MODULE].append(3*' '+'LECT')
        epx[MODULE].append(7*' '+'%s'% gma)
        epx[MODULE].append(3*' '+'TERM')
 
    epx[MODULE].append('\n') 
 
#-----------------------------------------------------------------------   
  def get_FONC_PARASOL(self,list_rigi_parasol=None) :
    
    epx = self.epx

    MODULE = 'FONC_PARASOL'
    if list_rigi_parasol is not None :
      if self.FONC_PARASOL is None : UTMESS('F','PLEXUS_12')
    # else : return None
    
    epx[MODULE] = ['*--DEFINITIONS DES FONCTIONS']
    
    # Entete de la commande europlexus
    epx[MODULE].append('FONC')
    
    # Cles de FONC_PARASOL dans l'ordre
    cles = ['NFKT','NFKR','NFAT','NFAR']
#    cles = ['NFKT','NFKR']
    # Dictionnaire faisant la correspondance entre la fonction et son numero europlexus
    dic_fonc = {}
    try : 
        ifonc = self.ifonc
        if debug: print 'self.ifonc = %s'%ifonc
    except : 
        self.ifonc=0
        ifonc = self.ifonc     
        if debug: print 'ifonc = %s'%ifonc
    if epx.has_key('FONC_MATER') :
        epx[MODULE].extend(epx['FONC_MATER'])
    for cle in cles:
        fonction = self.get_motcle(self.FONC_PARASOL,cle,code_mess=None)
        if debug: print "AA5/fonction=", fonction
        if fonction:
            ifonc += 1
            dic_fonc[cle] = ifonc
            #if not fonction: return {}    # Si les mots-cles sont manquants on passe
            (temps,valeurs) = fonction.Valeurs()
            # imprimer les valeurs de la table
            epx[MODULE].append(5*' ' +'NOPA %i TABLE %i' %(ifonc,len(temps)) )
            for ival in range(len(temps)):
                epx[MODULE].append(17*' ' +'%s %s' %(temps[ival],valeurs[ival]))
    self.ifonc = ifonc
    self.dic_fonc = dic_fonc
    epx[MODULE].append('\n')
    return dic_fonc

#-----------------------------------------------------------------------   
  def export_CHAM_MATER(self): 

    epx = self.epx

    # CLe identifiant
    MODULE = 'CHAM_MATER'

    epx[MODULE] = ['*--DEFINITION DES MATERIAUX']

    # Entete de la commande Europlexus courante
    epx[MODULE].append('MATERIAUX')

    # Recuperer la structure sous le mot_cle facteur AFFE de AFFE_MATER 
    affe_mater = self.recupere_structure(self.CHAM_MATER,'AFFE')
    affe_mater = self.setlist(affe_mater)

    # Dictionnaire stoquant les caracteristiques mecaniques et les group_ma associe a chaque materiau
    dic_mater = {}
    typMat = {}
    # GLRC impose de définir l'orientation : on stocke dans gmaGLRC les GMA dont il faudra retrouver l'orientation dans MODI_MAILLAGE/ORIE_NORM_COQUE
    self.gmaGLRC = []
    epx['FONC_MATER'] = []
    ref_fonc=[]
    for affe in affe_mater :
      # Recuperer le concept du materiau defini par DEFI_MATERIAU
      concept_mater = affe['MATER']
      # Recuperer le nom du materiau
      nom_mater = concept_mater.get_name()
      # Recuperer le group_ma concerne
      group_ma = self.get_group_ma(affe)
      if debug: print 'type(group_ma) = %s'%type(group_ma)
      if not dic_mater.has_key(nom_mater):
         dic_mater[nom_mater] = {'GROUP_MA':[]}
         # Recuperer les caracteristiques elastiques du materiau
         elas = self.recupere_structure(concept_mater,'ELAS')
         if elas :
            typMat[nom_mater] = 'ELAS'
            for car in ['E','RHO','NU']:
              dic_mater[nom_mater][car] = elas[car]
            for car in ['AMOR_ALPHA','AMOR_BETA'] :
              if elas.has_key(car) :
                dic_mater[nom_mater][car] = elas[car]
              else :
                dic_mater[nom_mater][car] = None                 
         beton = self.recupere_structure(concept_mater,'BETON')
         if beton  :
            typMat[nom_mater] = 'GLRC'
            self.gmaGLRC.extend(group_ma)
            dic_mater[nom_mater]['LINER']=[]
            dic_mater[nom_mater]['NAPPE']=[]
            materBeton = beton['MATER']
            elasBeton = self.recupere_structure(materBeton,'ELAS')
            dic_mater[nom_mater]['BETON']={}
            for car in ['E','RHO','NU']:
              dic_mater[nom_mater]['BETON'][car] = self.get_motcle(elasBeton,car)
            for car in ['AMOR_ALPHA','AMOR_BETA']:
              dic_mater[nom_mater]['BETON'][car] = self.get_motcle(elasBeton,car,None)
            ecroBeton = self.recupere_structure(materBeton,'BETON_ECRO_LINE')
            for car in ['SYT','SYC']:
              dic_mater[nom_mater]['BETON'][car] = self.get_motcle(ecroBeton,car,None)
            for car in ['EPAIS','GAMMA','QP1','QP2','C1N1','C1N2','C1N3','C2N1','C2N2','C2N3','C1M1','C1M2','C1M3','C2M1','C2M2','C2M3'] :
              dic_mater[nom_mater]['BETON'][car] = self.get_motcle(beton,car)
            for car in ['OMT','EAT','BT1','BT2'] :
              dic_mater[nom_mater]['BETON'][car] = self.get_motcle(beton,car,None)
            for car in ['MP1X','MP1Y','MP2X','MP2Y'] :
              car_temp = self.get_motcle(beton,car,None)
              if debug: print 'type(car_temp) = %s'%type(car_temp)
              dic_mater[nom_mater]['BETON'][car] = None
              if car_temp :
                  if isinstance(car_temp, float) :
                    dic_mater[nom_mater]['BETON'][car] = car_temp
                  else :
                    if debug: print dir(car_temp)
                    try : 
                        ifonc = self.ifonc
                    except : 
                        self.ifonc=0
                        ifonc = self.ifonc     
                    ifonc += 1
                    val = car_temp.Valeurs()
                    x = val[:len(val):2]
                    y = val[1:len(val):2]
                    # imprimer les valeurs de la table
                    epx['FONC_MATER'].append(5*' ' +'%i LSQU 2 TABLE %i' %(ifonc,len(x)) )
                    for ival in range(len(x)):
                       epx['FONC_MATER'].append(17*' ' +'%s %s' %(x[ival],y[ival]))   
                    dic_mater[nom_mater]['BETON'][car] = 'FONC %i'%ifonc
                    self.ifonc=ifonc


         cable = self.recupere_structure(concept_mater,'CABLE_PREC')
         if cable  :
            materCable = cable['MATER']
            elasCable = self.recupere_structure(materCable,'ELAS')
            ecroCable = self.recupere_structure(materCable,'ECRO_LINE')
            dic_mater[nom_mater]['CABLE']={}
            for car in ['E','NU']:
              dic_mater[nom_mater]['CABLE'][car] = elasCable[car]           
            for car in ['SY']:
              dic_mater[nom_mater]['CABLE'][car] = ecroCable[car]
            for car in ['OMX','OMY','RX','RY','PREX','PREY'] :
              dic_mater[nom_mater]['CABLE'][car] = cable[car]
         Lnappes = self.setlist(self.recupere_structure(concept_mater,'NAPPE'))
         if Lnappes  :
            for nappe in Lnappes :
                dic_tmp = {}
                materNappe = nappe['MATER']
                elasNappe = self.recupere_structure(materNappe,'ELAS')
                ecroNappe = self.recupere_structure(materNappe,'ECRO_LINE')
                for car in ['E','NU']:
                  dic_tmp[car] = elasNappe[car]           
                for car in ['SY']:
                  if ecroNappe :
                    dic_tmp[car] = ecroNappe[car]
                  else :
                    dic_tmp[car] = None
                for car in ['OMX','OMY','RX','RY'] :
                  dic_tmp[car] = nappe[car]
                for car in ['FS',] :
                  if nappe.has_key(car) :
                    dic_tmp[car] = nappe[car]
                  else :
                    dic_tmp[car] = None
                dic_mater[nom_mater]['NAPPE'].append(dic_tmp)
         Lliners = self.setlist(self.recupere_structure(concept_mater,'LINER'))
         if Lliners :
            for liner in Lliners :
                dic_tmp = {}
                materLiner = liner['MATER']
                elasLiner = self.recupere_structure(materLiner,'ELAS')
                ecroLiner = self.recupere_structure(materLiner,'ECRO_LINE')
                for car in ['E','NU']:
                  dic_tmp[car] = elasLiner[car]           
                for car in ['SY']:
                  dic_tmp[car] = ecroLiner[car]
                for car in ['OML','RLR'] :
                  dic_tmp[car] = liner[car]
                dic_mater[nom_mater]['LINER'].append(dic_tmp)
                

      if debug: print 'MATER = %s \n type = %s \n dic = %s'%(nom_mater,typMat, dic_mater[nom_mater])
      dic_mater[nom_mater]['GROUP_MA'].extend(group_ma)
      
      # if gmaGLRC :
          # concept_maillage = self.recupere_structure(self.MODELE,'MAILLAGE')
          # if debug: print 'ETAPES MAILLAGES'
          # if debug: print dir(concept_maillage)
          # if debug: print concept_maillage.executed
          # if debug: print concept_maillage.etape
          # if debug: print dir(concept_maillage.etape)
          # if debug: print concept_maillage.etape.get_liste_etapes()
          # if debug: print concept_maillage.etape.valeur
        
      
    # Impression au format Europlexus
    
    for nom_mater in dic_mater.keys():
      epx[MODULE].append('*--MATERIAU %s' %nom_mater)
      # mot cle indicant qu'il s'agit des caracteristiques lineaires du materiau
      if typMat[nom_mater] == 'ELAS' :
          dic_corresp = {'E':'YOUNG','NU':'NU','RHO':'RO'}
          dic_corresp2 ={'AMOR_ALPHA':'KRAY','AMOR_BETA':'MRAY'}
          epx[MODULE].append('LINE')
          for car_aster in dic_corresp.keys():
            vale    = dic_mater[nom_mater][car_aster]
            car_epx = dic_corresp[car_aster]
            if vale is not None :
              epx[MODULE].append('%s %s' %(car_epx,vale))
          for car_aster in dic_corresp2.keys():
            vale    = dic_mater[nom_mater][car_aster]
            car_epx = dic_corresp2[car_aster]
            if vale is not None :
              epx[MODULE].append('%s %s' %(car_epx,vale))
      else : #GLRC
        dic_corres1 = {'RHO':'RO','EPAIS':'H','E':'EB','NU':'NUB'}
        dic_corresNapp = {'E' : 'EA','SY':'FY','OMX':'OMX','OMY':'OMY','RX':'RX','RY':'RY','FS':'FS'}
        dic_corresPrec = {'E' : 'EA','SY':'FY','OMX':'OMX','OMY':'OMY','RX':'RX','RY':'RY'}
        dic_corresLinr = {'E' : 'EA','SY':'FY','OML':'OMLR','NU':'NULR','RLR':'RLR'}
        dic_corres2 = {'OMT' : 'OMT','EAT':'EAT','BT1':'BT1','BT2':'BT2','SYT' : 'FT','GAMMA':'GAMM','QP1':'QP1','QP2':'QP2','C1N1':'C1N1','C1N2':'C1N2','C1N3':'C1N3','C2N1':'C2N1','C2N2':'C2N2','C2N3':'C2N3','C1M1':'C1M1','C1M2':'C1M2','C1M3':'C1M3','C2M1':'C2M1','C2M2':'C2M2','C2M3':'C2M3','SYC':'FC'}
        dic_corres2b = {'MP1X':'MP1X','MP2X':'MP2X','MP1Y':'MP1Y','MP2Y':'MP2Y',}
        dic_corres3 = {'PREX' : 'PREX', 'PREY' : 'PREY'}
        dic_corres4 = {'AMOR_ALPHA':'KRAY','AMOR_BETA':'MRAY'}
        epx[MODULE].append('GLRC DAMA')
        for car_aster in dic_corres1.keys():
            vale    = dic_mater[nom_mater]['BETON'][car_aster]
            car_epx = dic_corres1[car_aster]
            if vale is not None :
                epx[MODULE].append('%s %s' %(car_epx,vale))
        nlit = len(dic_mater[nom_mater]['NAPPE'])+len(dic_mater[nom_mater]['LINER'])
        if dic_mater[nom_mater].has_key('CABLE') : 
            nlit+=1
        if nlit :
            epx[MODULE].append('%s %s' %('NLIT',nlit))
        for nappe in dic_mater[nom_mater]['NAPPE'] :
            epx[MODULE].append('NAPP')
            for car_aster in dic_corresNapp.keys():
                if nappe.has_key(car_aster) :
                    vale    = nappe[car_aster]
                    car_epx = dic_corresNapp[car_aster]
                    if vale is not None:
                        epx[MODULE].append('%s %s' %(car_epx,vale))
        if dic_mater[nom_mater].has_key('CABLE') : 
            epx[MODULE].append('PREC')
            for car_aster in dic_corresPrec.keys():
                vale    = dic_mater[nom_mater]['CABLE'][car_aster]
                car_epx = dic_corresPrec[car_aster]
                if vale is not None :
                    epx[MODULE].append('%s %s' %(car_epx,vale))
        for liner in dic_mater[nom_mater]['LINER'] :
            epx[MODULE].append('LINR')
            for car_aster in dic_corresLinr.keys():
                vale    = liner[car_aster]
                car_epx = dic_corresLinr[car_aster]
                if vale is not None :
                    epx[MODULE].append('%s %s' %(car_epx,vale))
        for car_aster in dic_corres2.keys():
            vale    = dic_mater[nom_mater]['BETON'][car_aster]
            car_epx = dic_corres2[car_aster]
            if vale is not None :
                if isinstance(vale, float) :
                    epx[MODULE].append('%s %s' %(car_epx,vale))
                else :
                    epx[MODULE].append('%s %s' %(car_epx,vale))
        for car_aster in dic_corres2b.keys():
            vale    = dic_mater[nom_mater]['BETON'][car_aster]
            car_epx = dic_corres2b[car_aster]
            if vale is not None :
                if isinstance(vale, float) :
                    epx[MODULE].append('%s %s' %(car_epx,vale))
                else :
                    epx[MODULE].append('%s %s' %(car_epx,vale))
        if dic_mater[nom_mater].has_key('CABLE') : 
            for car_aster in dic_corres3.keys():
                vale    = dic_mater[nom_mater]['CABLE'][car_aster]
                car_epx = dic_corres3[car_aster]
                if vale is not None :
                    epx[MODULE].append('%s %s' %(car_epx,vale))
        for car_aster in dic_corres4.keys():
            vale    = dic_mater[nom_mater]['BETON'][car_aster]
            car_epx = dic_corres4[car_aster]
            if vale is not None :
                epx[MODULE].append('%s %s' %(car_epx,vale))
      
          # ajouter les group_ma qui ont ete affecte par ces caracteristiques
      epx[MODULE].append(3*' '+'LECT')
      for group in dic_mater[nom_mater]['GROUP_MA']:
        epx[MODULE].append(7*' '+group)
      epx[MODULE].append(3*' '+'TERM')

    
    return epx
    
#-----------------------------------------------------------------------   
  def export_EXCIT(self):
  
    epx = self.epx
    
    # CLe identifiant
    MODULE = 'EXCIT'
    
    epx[MODULE] = ['*--DEFINITION DES CHARGEMENTS et DES CONDITIONS AUX LIMITES']
    
    # Entete de la commande Europlexus courante
    #epx[MODULE].append('CHARGEMENT')

    try : 
      test = self.EXCIT[0]
      excit_list = self.EXCIT
    except :  
      excit_list = self.setlist(self.EXCIT)
    
    #print '<INFO> excit_list = ',excit_list
    #print '<INFO> type(excit_list) = ', type(excit_list)
    
    # Traduction de ddl_impo
    dic_ddl_impo = {'DX':1,'DY':2,'DZ':3,'DRZ':4,'DRY':5,'DRZ':6}

    # Initialisation des variables decrivant le chargement et les conditions de blocage
    CHARGEMENT = []
    LIAISON   = []
    
    for excit in excit_list:
      concept_charge = excit['CHARGE']
      pres_rep_list = self.recupere_structure(concept_charge,'PRES_REP')
      ddl_impo_list = self.recupere_structure(concept_charge,'DDL_IMPO')
      pres_rep_list = self.setlist(pres_rep_list)
      ddl_impo_list = self.setlist(ddl_impo_list)
      # PRES_REP
      if pres_rep_list :
        if len(CHARGEMENT) == 0 : 
          CHARGEMENT = ['CHARGEMENT']
          CHARGEMENT.append(2*' ' + '1 FACTO 2')
        fonction = self.get_motcle(excit,'FONC_MULT',code_mess='A')
        if fonction is None:
          UTMESS('F','PLEXUS_7')
        (temps,valeurs) = fonction.Valeurs()
        for pres_rep in  pres_rep_list :
          pression = pres_rep['PRES']
          group_ma = self.get_group_ma(pres_rep)
          CHARGEMENT.append(6*' ' +'PRESS COQU %s' %pression)
          # ajouter les group_ma qui ont ete affecte par ces caracteristiques
          CHARGEMENT.append(6*' '+'LECT')
          for group in group_ma:
            CHARGEMENT.append(10*' '+group)
          CHARGEMENT.append(6*' '+'TERM') 
          # imprimer les valeurs de la table
          CHARGEMENT.append(6*' ' +'TABLE %i' %len(temps))
          for ival in range(len(temps)):
            CHARGEMENT.append(10*' ' +'%s %s' %(temps[ival],valeurs[ival]))
      # DDL_IMPO
      #if debug: print 'ddl_impo_list',ddl_impo_list
      if ddl_impo_list :
        #UTMESS('A','PLEXUS_8')
# AA        if len(LIAISON) == 0 : LIAISON = ['LIAISON']
        if len(LIAISON) == 0 : LIAISON = ['LINK']

        for ddl_impo in ddl_impo_list:
          blocage = ''

          # DDL_IMPO / DX
          for cle in dic_ddl_impo.keys() :
            vale = self.get_motcle(ddl_impo,cle,code_mess=None)
            if vale is not None:
              if vale != 0 : UTMESS('F','PLEXUS_13')
              else : 
                blocage += `dic_ddl_impo[cle]`

          # LIAISON / ENCASTRE
          if ddl_impo.has_key('LIAISON'):
             if ddl_impo['LIAISON']=='ENCASTRE':
                blocage += ' 123456'

          # recupere les entites geometriques
          for entite in ['GROUP_MA','GROUP_NO'] :
            group_ma = self.get_motcle(ddl_impo,entite,code_mess=None)
            if group_ma is not None : 
              group_ma = self.setlist(group_ma);break
          LIAISON.append(' '*2 + 'BLOQ %s' %blocage)
          # ajouter les group_ma qui ont ete affecte par ces caracteristiques
          LIAISON.append(6*' '+'LECT')
          for group in group_ma:
            LIAISON.append(10*' '+group)
          LIAISON.append(6*' '+'TERM') 

      if not ddl_impo_list and not pres_rep_list :
        UTMESS('F','PLEXUS_9')
    
    # Transmettre les commandes a la liste global epx
    for var in [CHARGEMENT,LIAISON] :
      if len(var) > 0 :
        epx[MODULE].append('\n')
        for ligne in var : epx[MODULE].append(ligne)
    
    epx[MODULE].append('\n')    
#-----------------------------------------------------------------------   
  def Correspondance_champ(self):
   
    # Dictionnaire de passage de champs Aster --> Europlexus
    dic_champ = {'DEPL':'DEPLACEMENT','VITE':'VITESSE','ACCE':'ACCELERATION',
                 'SIEF_ELGA':'CONT','EPSI_ELGA':'EPST','VARI_ELGA':'ECRO'}
    
    # Dictionnaire de passage de composantes Aster --> Europlexus
    dic_cmp = {}
    for cham in ['DEPL','VITE','ACCE'] :
      dic_cmp[cham] = {'DX' : 1,'DY':2,'DZ':3,'DRX':4,'DRY':5,'DRZ':6}
    
    dic_cmp['SIEF_ELGA']  = {'SIXX':1,'SIYY':2,'SIZZ':3,'SIXY':4,'SIXZ':5,'SIYZ':6,'NXX':1,'NYY':2,'NXY':3,'MXX':4,'MYY':5,'MXY':6,'QX':7,'QY':8}
    dic_cmp['VARI_ELGA'] = {}
    for ii in range(1,25):
      dic_cmp['VARI_ELGA']['V%i'%ii] = ii
    
    self.dic_champ = dic_champ  
    self.dic_cmp = dic_cmp
        
    
#-----------------------------------------------------------------------   
  def export_ECRITURE(self):
    
    epx = self.epx
    
    self.Correspondance_champ()
             
    # CLe identifiant
    MODULE = 'ECRITURE'

    dcles_freq = {'FREQ':'PAS_NBRE', 'TFREQ':'PAS_INST'}

    def get_freq(fact):
      for cle_freq in dcles_freq.keys(): 
# AA        vale_freq = self.get_motcle(fact,cle_freq,code_mess=None)
        vale_freq = self.get_motcle(fact,dcles_freq[cle_freq],code_mess=None)
        if vale_freq is not None : break
      return cle_freq,vale_freq  
      
    def get_freq2() :
        if self.PAS_NBRE_COURBE :
            return 'FREQ',self.PAS_NBRE_COURBE
        elif self.PAS_INST_COURBE :
            return 'TFREQ',self.PAS_INST_COURBE
        else :
            raise 'Erreur programmation'
        

    def string2list(var) :
      if types.TypeType(var) == types.StringType : var = (var,)
      return var


    epx[MODULE] = ['*--ECRITURES DES RESULTATS']
    epx[MODULE] = ['opti K2GP']
    # Entete de la commande Europlexus courante
    epx[MODULE].append('ECRITURE')


    # Traitement du mot-cle facteur OBSERVATION (EPX = LISTING)
# AA    listing_fact = self.ECRITURE['LISTING']
    listing_fact = self.OBSERVATION
    nom_cham = string2list(listing_fact['NOM_CHAM'])

    cle_freq_listing, vale_freq_listing = get_freq(listing_fact)
# AA   cles_entite = ['POINTS','ELEMEMTS']
    dcles_entite = {'POINT':'GROUP_NO', 'ELEM':'GROUP_MA'}
# AA    dcles_tout   = {'POINTS':'TOUT_GROUP_NO', 'ELEMEMTS':'GROUP_MA'}
    cles_entite = dcles_entite.keys()

    # Ecriture format Europlexus
    
    # Ecriture LISTING
    st = 2*' '   
    for cham_aster in nom_cham:
      cham_epx = self.dic_champ[cham_aster]
      st +=  '%s ' % cham_epx  
    st += ' %s %s' % (cle_freq_listing,vale_freq_listing)
    st += ' NOPO NOEL'
    epx[MODULE].append(st)
    for cle in cles_entite:

       entite_geo = string2list(listing_fact[dcles_entite[cle]])
       # if listing_fact['TOUT_%s' % dcles_entite[cle]] == 'OUI': epx[MODULE].append(2*' ' +'%s TOUS ' % cle)
       # elif entite_geo is None:                                 epx[MODULE].append(2*' ' +'NO%s' % cle)
       # else:
          # epx[MODULE].append(2*' ' +'%s LECT' %cle)
          # for group in entite_geo :
             # epx[MODULE].append(6*' '+group)
          # epx[MODULE].append(2*' '+'TERM') 

#        if entite_geo is None : 
#           epx[MODULE].append(2*' ' +'NO%s' %cle)
#        elif  entite_geo[0] == 'TOUS' :
#          epx[MODULE].append(2*' ' +'%s TOUS ' %cle)
#        else :
#           epx[MODULE].append(2*' ' +'%s LECT' %cle)
#           for group in entite_geo :
#              epx[MODULE].append(6*' '+group)
#           epx[MODULE].append(2*' '+'TERM') 
#           

    # Ecriture FICHIER ALICE UTILISE par le mot-cle facteur COURBE
# AA    courbe_fact = self.ECRITURE['COURBE']
    courbe_fact = self.COURBE
    if courbe_fact is not None :

      # dcourbe=[]
      # for j in courbe_fact:
         # dcourbe.append(j.cree_dict_valeurs(j.mc_liste))
         # for i in dcourbe[-1].keys():
            # if dcourbe[-1][i]==None : del dcourbe[-1][i]

      # if debug: print 'dcourbe=', dcourbe

      cle_freq, vale_freq = get_freq2()
      fichier_alit = os.path.join(self.REPE_epx, self.nom_fichiers['ALIT'])

      epx[MODULE].append(2*' ' + "FICH ALIT 11  %s %s" %(cle_freq,vale_freq))

      # Liste les noeuds a postraiter
      lnoeuds=set()
      lmailles=set()
      for courbe in courbe_fact :
        if courbe['GROUP_NO'] != None :
            noeuds = courbe['GROUP_NO']
            if debug: print type(noeuds)
            # liste de noeud ou un seul noeud ?
            if type(noeuds) == tuple :
                for el in noeuds :
                    lnoeuds.add(el)
            else :
                lnoeuds.add(noeuds)
        elif courbe['GROUP_MA'] != None :
            mailles = courbe['GROUP_MA']
            if debug: print type(mailles)
            # liste de mailles ou une seule maille ?
            if type(mailles ) == tuple :
                for el in mailles :
                    lmailles.add(el)
            else :
                lmailles.add(mailles)           
        else :
            raise 'Erreur ni noeud ni maille'

      # Ecriture la liste des noeuds sous format epx
      if lnoeuds :
          epx[MODULE].append(4*' ' +'POINTS')
          epx[MODULE].append(6*' ' +'LECT ')
          for noeud in lnoeuds :
            epx[MODULE].append(8*' '+noeud)
          epx[MODULE].append(6*' '+'TERM')   
      if lmailles :
          epx[MODULE].append(4*' ' +'ELEM')
          epx[MODULE].append(6*' ' +'LECT ')
          for maille in lmailles :
            epx[MODULE].append(8*' '+maille)
          epx[MODULE].append(6*' '+'TERM')   
     
     
      # Pas besoin d'elements 
      # epx[MODULE].append(4*' ' + 'NOELEMENTS')
    
    # Ecriture fichier MED representant les champs aux noeuds et aux points de gauss
    # Traitement du mot-cle facteur ARCHIVAGE
#    # Traitement du mot-cle facteur CHAMP
#    champ_fact = self.ECRITURE['CHAMP']
    champ_fact = self.ARCHIVAGE
    if champ_fact is not None :
      cle_freq_champ,vale_freq_champ = get_freq(champ_fact)
      epx[MODULE].append(2*' ' + 'FICHIER MED')
      # chemin complet du fichier med
      fichier_med = os.path.join(self.REPE_epx, self.nom_fichiers['MED'])

      epx[MODULE].append(2*' ' + "'%s'" %fichier_med)
      cle_freq,vale_freq = get_freq(champ_fact)
      epx[MODULE].append(2*' ' + ' %s %s' %(cle_freq_champ,vale_freq_champ))
      
      # groupes de mailles du modele
      entite_geo={}
      entite_geo['ELEM'] = []
      for model in ['DKT3','Q4GS'] :
         if model in self.modelisations :
             entite_geo['ELEM'].extend(self.dic_gma[model])
      entite_geo['POINT'] = []
      for model in ['DKT3','Q4GS','POUT'] :
         if model in self.modelisations :
             entite_geo['POINT'].extend(self.dic_gma[model])
      for cle in cles_entite :
       epx[MODULE].append(2*' ' +'%s LECT' %cle)
       for group in entite_geo[cle] :
          epx[MODULE].append(6*' '+group)
       epx[MODULE].append(2*' '+'TERM') 
    
    
    # ecriture CAST2000
    # __temp
    fichier_cast2000 = 'fort.%i' %unite_cast2000
    if unite_cast2000 and os.path.isfile(fichier_cast2000) :
      # titre
      epx[MODULE].append('\n*-- CAST2000')
      fd = open(fichier_cast2000,'r')
      lst = fd.readlines()
      fd.close()
      for st in lst :
        st = string.replace(st,'\n','')
        epx[MODULE].append(st)
          
    # Une ligne de saut
    epx[MODULE].append('\n')
    
#-----------------------------------------------------------------------   

  def export_POST_COURBE(self):

    # Suite de postraitement permettant d'ecrire des fichiers ASCII 
    # des grandeurs demandees
    
    # Tester si le mot_cle facteur COURBE a ete renseigne
    courbe_fact = self.COURBE
    if courbe_fact is None : return

    epx = self.epx
                 
    # Cle identifiant
    MODULE = 'POST_COURBE'

    # Ecriture fort Europlexus
    # Entete de la commande courante
    epx[MODULE] = ['SUITE\n']

    if self.UNITE_COURBE: fichier_courbes = os.path.join( self.REPE_epx, 'fort.%s' % str(self.UNITE_COURBE) )
    else:                 fichier_courbes = os.path.join( self.REPE_epx, self.nom_fichiers['PUN'] )
    fichier_alit = os.path.join( self.REPE_epx, self.nom_fichiers['ALIT'] )

    epx[MODULE].append("RESULTAT ALICE TEMPS 11")
    epx[MODULE].append("OPNF FORMAT 17 '%s'" %fichier_courbes)    
    epx[MODULE].append("SORTIES GRAPHIQUES")
    epx[MODULE].append("AXTEMPS 1. 'TEMPS(s)'")

    # recuperer le mot-cle facteur COURBE
    courbe_fact = self.COURBE
    # recuperer les mot_cle TABLE
    # tables = self.get_motcle(courbe_fact,'TABLE',code_mess='F')
    #tables = self.setlist(tables)

    # Dictionnaire decrivant les legendes des abscisses et ordodonnees des courbes imprimees
    # etl utilisee dans get_tables
    self.legend_courbes = {}
    entites_courbe = ['GROUP_NO','GROUP_MA'] 
    entite_EPX={'GROUP_NO' : 'NOEUD', 'GROUP_MA' : 'ELEM'}
    icourbe = 0
    # for table in tables:
    lnoeuds = []
    for table in courbe_fact :
      for entite_type in entites_courbe :
        try : entite = table[entite_type]
        except : entite = None
        if entite is not None :        
            cham_aster = table['NOM_CHAM']
            cmp_aster = table['NOM_CMP']
            cham_epx = self.dic_champ[cham_aster]
            cmp_epx  = self.dic_cmp[cham_aster][cmp_aster]
            ylabel   = cham_aster + '_' + cmp_aster
            #ylabel   = cmp_aster
            if type(entite) is not tuple : entite = [entite,]
            for el in entite :
                icourbe+=1
                label = ylabel + '_%s'%el
                if entite_type == 'GROUP_MA' :
                    label = label+'_%s'%table['NUM_GAUSS']
                st = "COURBE %i '%s' %s COMP %i "% (icourbe,label,cham_epx,cmp_epx)
                if entite_type == 'GROUP_MA' :
                    st=st+"GAUSS %i "%table['NUM_GAUSS']
                st=st+"%s LECT %s TERM"%(entite_EPX[entite_type],el)
                if debug: print 'st = %s'%st
                epx[MODULE].append(st)
                # epx[MODULE].append("LIST %i AXES 1. '%s'" % (icourbe,ylabel))
                st = "LIST %s AXES 1. '%s'" % (icourbe,label)
                if debug: print 'st = %s'%st
                epx[MODULE].append(st)
                self.legend_courbes[icourbe] = ['TEMPS',label]



    # Une ligne de saut
    epx[MODULE].append('\n')

#-----------------------------------------------------------------------   
  def export_CALCUL(self):
    
    epx = self.epx
    
    # CLe identifiant
    MODULE = 'CALCUL'

    # Entete de la commande Europlexus courante
    epx[MODULE] = ['*--OPTION DE CALCUL']
    epx[MODULE].append('')
    type_discr = self.CALCUL['TYPE_DISCRETISATION']
    
    epx[MODULE].append('OPTION PAS %s' %type_discr)
      
    if  type_discr == 'AUTO':
      cstab = self.CALCUL['CSTAB']
      epx[MODULE].append(7*' ' + 'CSTAB %s' %cstab)
    
    listInterfaces = self.INTERFACES
    listDomaines = self.DOMAINES
    domaineInterfaces = {}
    if listDomaines :
        epx[MODULE].append('\n')
        epx[MODULE].append('*--DEFINITION DES SOUS DOMAINES')
        epx[MODULE].append('')
        epx[MODULE].append('STRUCTURE %s'%len(listDomaines))
        for interface in listInterfaces :
            Lgma1 = self.setlist(interface['GROUP_MA_1'])
            Lgma2 = self.setlist(interface['GROUP_MA_2'])
            idS1 = interface['IDENT_DOMAINE_1']
            idS2 = interface['IDENT_DOMAINE_2']
            if not domaineInterfaces.has_key(idS1) :
                domaineInterfaces[idS1]= []
            if not domaineInterfaces.has_key(idS2) :
                domaineInterfaces[idS2]= []
            domaineInterfaces[idS1].extend(Lgma1)
            domaineInterfaces[idS2].extend(Lgma2)
    else : 
        listDomaines = []
    for domaine in listDomaines :
        Lgma = self.setlist(domaine['GROUP_MA'])
        id  = domaine['IDENTIFIANT']
        epx[MODULE].append(3*' '+'DOMA LECT')
        for gma in Lgma :
            epx[MODULE].append(12*' '+gma)
        for gma in domaineInterfaces[id] :
            epx[MODULE].append(12*' '+gma)
        epx[MODULE].append(8*' '+'TERM')
        epx[MODULE].append(8*' '+'IDENTIFIANT %s\n'%id)
        
    
    if listInterfaces :
        epx[MODULE].append('')
        epx[MODULE].append('INTERFACE %s'%len(listInterfaces))
    else :
        listInterfaces = []
    for interface in listInterfaces :
        Lgma1 = self.setlist(interface['GROUP_MA_1'])
        Lgma2 = self.setlist(interface['GROUP_MA_2'])
        idS1 = interface['IDENT_DOMAINE_1']
        idS2 = interface['IDENT_DOMAINE_2']
        tole = interface['TOLE']
        epx[MODULE].append(3*' '+'MORTAR TOLE %s'%tole)
        epx[MODULE].append(7*' '+'DOMA %s LECT'%idS1)
        for gma in Lgma1 :
            epx[MODULE].append(13*' '+gma)
        epx[MODULE].append(10*' '+'TERM')
        epx[MODULE].append(7*' '+'DOMA %s LECT'%idS2)
        for gma in Lgma2 :
            epx[MODULE].append(13*' '+gma)
        epx[MODULE].append(10*' '+'TERM')
                
    epx[MODULE].append('\n')
    epx[MODULE].append('*--LANCEMENT DE CALCUL')
    epx[MODULE].append('')
    calcul = 'CALCUL'
    cles = ['INST_INIT','PASFIX','INST_FIN']
    dcles = {'INST_INIT':'TINI', 'PASFIX':'PASFIX', 'INST_FIN':'TFIN'}
    for cle in dcles.keys():
      try : calcul += ' %s %s' %(dcles[cle], self.CALCUL[cle])
      except : pass
    # Doit etre mis en entier
    try : calcul += ' %s %d' %('NMAX',self.CALCUL['NMAX'])
    except : pass
    epx[MODULE].append(calcul) 
    epx[MODULE].append('\n')
    #epx[MODULE].append('FIN')  
    
  
#-----------------------------------------------------------------------   
  def ecrire_fichier(self,):
    
# AA    fichier = self.REPE + os.sep + self.nom_fichiers['COMMANDE']
    fichier = os.path.join(self.REPE_epx, self.nom_fichiers['COMMANDE'])

    # ordre des modules de definition du modele EPX
    modules = ['DEBUT','MAILLAGE','DIME','MODELE','CARA_ELEM','FONC_PARASOL','CHAM_MATER','RIGI_PARASOL','EXCIT','ECRITURE','CALCUL','POST_COURBE']

    #les modules MODELE et RIGI_PARASOL doivent etre executes avant MAILLAGE 
    # car le maillage peut etre modifie dans ces modules
    modules_exe = []
    modules_exe.extend(modules)
    modules_exe.remove('MAILLAGE') 
    modules_exe.append('MAILLAGE')
    
    # Excecution des differentes modules
    for module in modules_exe:
      fct = 'export_%s' %module
      if hasattr(self,fct) : 
          eval('self.'+fct+'()')  
#       else :  
#           print 'module %s pas encore developpe' % fct
    
    # Ecriture du fichier texte contenant toutes les commandes Europlexus
    fd = open(fichier,'w')
    for module in modules :
      if self.epx.has_key(module) :
        for ll in self.epx[module]:
          if debug: print 'EPX : %s'%ll
          fd.write('%s\n'%ll)
    # Fin du fichier de commandes epx      
    fd.write('FIN')      
    fd.close()

#-----------------------------------------------------------------------   
  def get_table(self,icourbe=1,fichier='auto') :
   
    global table
    
    if not hasattr(self,'courbes'): 
#      if fichier == 'auto' : fichier = self.pwd + self.REPE + os.sep + self.nom_fichiers['PUN']
      if fichier == 'auto' : fichier = os.path.join(self.REPE_epx, self.nom_fichiers['PUN'] )
      if not os.path.isfile(fichier) : return
      self.courbes = self.lire_pun(fichier=fichier)
    
    if not os.path.isfile(fichier) : return
    if debug: print self.courbes,type(self.courbes)
    nc = 0
    para_ordonnee = []
    dico = []
    for icourbe in self.courbes :
        valeurs = self.courbes[icourbe]
        if debug: print 'icourbe = %s ; valeurs = %s'%(icourbe, valeurs)
        if nc == 0 :
            para_abscisse = self.legend_courbes[icourbe][0]
            vale_abscisse = valeurs[0,:].tolist()
            assert (len(para_abscisse)<17)
            dico.append({'TYPE_K':'K16','LISTE_R' : vale_abscisse , 'PARA' : para_abscisse})
            para_ordonnee = self.legend_courbes[icourbe][1]
            vale_ordonnee = valeurs[1,:].tolist()
            assert (len(para_ordonnee)<17)
            dico.append({'TYPE_K':'K16','LISTE_R' : vale_ordonnee , 'PARA' : para_ordonnee})
            nc=1
        else :
            if ((self.legend_courbes[icourbe][0] == para_abscisse) and (vale_abscisse == valeurs[0,:].tolist())) :
                para_ordonnee = self.legend_courbes[icourbe][1]
                vale_ordonnee = valeurs[1,:].tolist()
                assert (len(para_ordonnee)<17)
                dico.append({'TYPE_K':'K16','LISTE_R' : vale_ordonnee , 'PARA' : para_ordonnee})
            else :
                raise 'Table non compatible'
    
    table = CREA_TABLE( LISTE =dico
                      );
    
    if 0 :
      unite = self.get_unite_libre()
      unite = 90
      DEFI_FICHIER(UNITE=unite,ACTION='ASSOCIER'); 
                                                
      IMPR_TABLE(UNITE = unite,
               FORMAT = 'XMGRACE',
               TABLE = table,
               LEGENDE_X = para_abscisse,
               LEGENDE_Y = para_ordonnee,
               LEGENDE   = 'test'
           );
           
      os.system('xmgrace fort.%i' %unite) 
    
      DEFI_FICHIER(UNITE=unite,ACTION='LIBERER'); 
           
                  
#-----------------------------------------------------------------------
  def get_resu(self,fichier_med='auto'):
    
    # Rendre global le resu pour qu'il soit accepte dans self.DeclareOut 
    global resu
    
    if fichier_med == 'auto' : fichier_med = os.path.join(self.REPE_epx, self.nom_fichiers['MED'])

    if debug: print fichier_med
    if not os.path.isfile(fichier_med) :
       print "Pas de fichier MED !"
       return
        
    # Format med des champs depl, vite et acce
    format_med =[_F(NOM_CHAM_MED='CHAMP___DEPLACEMENT___001',
                    NOM_CMP=('DX','DY','DZ','DRX','DRY','DRZ'),
                    NOM_CMP_MED=('UX','UY','UZ','RX','RY','RZ'),
                    NOM_CHAM    ='DEPL' ),
                 _F(NOM_CHAM_MED='CHAMP___VITESSE___001',
                    NOM_CMP=('DX','DY','DZ','DRX','DRY','DRZ'),
                    NOM_CMP_MED=('VX','VY','VZ','RX','RY','RZ'),
                    NOM_CHAM    ='VITE' ),
                 _F(NOM_CHAM_MED='CHAMP___ACCELERATION___001',
                    NOM_CMP=('DX','DY','DZ','DRX','DRY','DRZ'),
                    NOM_CMP_MED=('GX','GY','GZ','RX','RY','RZ'),
                    NOM_CHAM    ='ACCE' ),
                ]    

    # Dicionnaire permettant de faire la correspondance des champs aux pts de gauss entre le med de europlexus et aster
    dic_cmp_gauss = {}
    
    dic_cmp_gauss['CONTRAINTE'] = {'DKT3': {'NOM_CMP'     : ('SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ'),
                                            'NOM_CMP_MED' : ('SIG1','SIG2','SIG3','SIG4','SIG5','SIG6',),},
                                   'Q4GS': {'NOM_CMP'     : ('NXX','NYY','NXY','MXX','MYY','MXY','QX','QY'),
                                            'NOM_CMP_MED' : ('SIG1','SIG2','SIG3','SIG4','SIG5','SIG6','SIG7','SIG8'),},
                                   'POUT' : {}        
                    
                                   }

    dic_cmp_gauss['DEFORMATION'] = {'DKT3': {'NOM_CMP'     : ('EPXX','EPYY','EPZZ','EPXY','EPXZ','EPYZ'),
                                            'NOM_CMP_MED' : ('EPS1','EPS2','EPS3','EPS4','EPS5','EPS6',),},
                                   'Q4GS': {'NOM_CMP'     : ('EXX','EYY','EXY','KXX','KYY','KXY','GAX','GAY'),
                                            'NOM_CMP_MED' : ('EPS1','EPS2','EPS3','EPS4','EPS5','EPS6','EPS7','EPS8'),},
                                   'POUT' : {}        
                    
                                   }

    dic_cmp_gauss['ECROUISSAGE'] = {'DKT3': {'NOM_CMP'     : ('V1','V2','V3','V4','V5','V6','V7','V8','V9','V10','V11','V12','V13','V14','V15','V16','V17','V18','V19'),
                                            'NOM_CMP_MED'  : ('VAR1','VAR2','VAR3','VAR4','VAR5','VAR6','VAR7','VAR8','VAR9','VAR10','VAR11','VAR12','VAR13','VAR14','VAR15','VAR16','VAR17','VAR18','VAR19'),},
                                   'Q4GS' : {'NOM_CMP'     : ('V1','V2','V3','V4','V5','V6','V7','V8','V9','V10','V11','V12','V13','V14','V15','V16','V17','V18','V19'),
                                            'NOM_CMP_MED'  : ('VAR1','VAR2','VAR3','VAR4','VAR5','VAR6','VAR7','VAR8','VAR9','VAR10','VAR11','VAR12','VAR13','VAR14','VAR15','VAR16','VAR17','VAR18','VAR19'),},
                                   'POUT' : {}        
                        
                                    }
    tupVar = ('X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12','X13','X14','X15','X16','X17','X18','X19')                                
    
    # Dictionnaire permettant de traduire le champ epx en med au nom asscie dans aster
    epx2aster = {'CONTRAINTE':'SIEF_ELGA','DEFORMATION':'EPSI_ELGA','ECROUISSAGE':'VARI_ELGA'}

# AA : desactive pour le moment
#     # Enrichir la liste format_med par les champs aux pts de gauss
#     imode = 0
#     if debug: print 'self.modelisations',self.modelisations
#     for modelisation in self.modelisations :
#       #if modelisation in ['Q4GS','DKT3'] :
#         imode += 1
#         numero =  string.replace('%3.i' %imode,' ','0')
#         for champ in dic_cmp_gauss.keys():
#           if len(dic_cmp_gauss[champ][modelisation]) > 0 :
#             nom_cham_med = 'CHAMP___'+ champ + (13-len(champ))*'_' + numero
#             nom_cham = epx2aster[champ]
#             nom_cmp = dic_cmp_gauss[champ][modelisation]['NOM_CMP']
#             nom_cmp_med = dic_cmp_gauss[champ][modelisation]['NOM_CMP_MED']
#             # ajouer a la list format_med
#             format_med.append(_F(NOM_CHAM_MED=nom_cham_med,NOM_CMP=nom_cmp,
#                                NOM_CMP_MED=nom_cmp_med,NOM_CHAM=nom_cham)
#                            )

    # Lire le fichier med avec les options choisies dans la liste format_med
    unite = self.get_unite_libre()
    DEFI_FICHIER(UNITE=unite,ACTION='LIBERER'); 
    fort = 'fort.%i' %unite
    if os.path.isfile(fort) : os.remove(fort)

    if not os.path.isfile(fichier_med): UTMESS('F','PLEXUS_14')   

    os.symlink(fichier_med,fort)

    # Regeneration des mots-cles EXCIT passés en argument de la macro
    dExcit=[]
    for j in self.EXCIT :
       dExcit.append(j.cree_dict_valeurs(j.mc_liste))
       for i in dExcit[-1].keys():
          if dExcit[-1][i]==None : del dExcit[-1][i]

    resu = LIRE_RESU(TYPE_RESU='EVOL_NOLI',
#                  VERI_ELGA='NON',
                  FORMAT='MED',
                  MODELE=self.NEW_MODELE,
                  FORMAT_MED=format_med,
                  UNITE=unite,
                  CHAM_MATER=self.CHAM_MATER,
                  CARA_ELEM=self.CARA_ELEM,
                  TOUT_ORDRE='OUI',
                  EXCIT=dExcit,
                  INFO=self.INFO,
                  );

    __EPN = CREA_CHAMP(
            INFO      = self.INFO,
            TYPE_CHAM = 'ELGA_NEUT_R',
            OPERATION = 'AFFE',
            MODELE    = self.NEW_MODELE,
            PROL_ZERO = 'OUI',
            AFFE      = self.listEpais,
            )
    __EPN2 = CREA_CHAMP(
            INFO      = self.INFO,
            TYPE_CHAM = 'ELGA_NEUT_R',
            OPERATION = 'AFFE',
            MODELE    = self.NEW_MODELE,
            PROL_ZERO = 'OUI',
            AFFE      = self.listEpais2,
            )
    __UNN = CREA_CHAMP(
            INFO      = self.INFO,
            TYPE_CHAM = 'ELGA_NEUT_R',
            OPERATION = 'AFFE',
            MODELE    = self.NEW_MODELE,
            PROL_ZERO = 'OUI',
            AFFE = _F(VALE=(1.,1.), TOUT='OUI', NOM_CMP=('X21','X22')),
            )
    __UNDEU = CREA_CHAMP(
            INFO      = self.INFO,
            TYPE_CHAM = 'ELGA_NEUT_R',
            OPERATION = 'AFFE',
            MODELE    = self.NEW_MODELE,
            PROL_ZERO = 'OUI',
            AFFE = _F(VALE=(1.,1./2.), TOUT='OUI', NOM_CMP=('X21','X22')),
            )
    __FONC1 = FORMULE(VALE='X1*X21',NOM_PARA=('X1','X21'))
    __FONC2 = FORMULE(VALE='X2*X21',NOM_PARA=('X2','X21'))
    __FONC3 = FORMULE(VALE='X3*X21',NOM_PARA=('X3','X21'))
    __FONC4 = FORMULE(VALE='X4*X22',NOM_PARA=('X4','X22'))
    __FONC5 = FORMULE(VALE='X5*X22',NOM_PARA=('X5','X22'))
    __FONC6 = FORMULE(VALE='X6*X22',NOM_PARA=('X6','X22'))
    __FONC7 = FORMULE(VALE='X7*X21',NOM_PARA=('X7','X21'))
    __FONC8 = FORMULE(VALE='X8*X21',NOM_PARA=('X8','X21'))
    __FONC11 = FORMULE(VALE='X11*X21',NOM_PARA=('X11','X21'))
    __FONC12 = FORMULE(VALE='X12*X21',NOM_PARA=('X12','X21'))
    __FONC13 = FORMULE(VALE='X13*X21',NOM_PARA=('X13','X21'))
    __FONC14 = FORMULE(VALE='X14*X22',NOM_PARA=('X14','X22'))
    __FONC15 = FORMULE(VALE='X15*X22',NOM_PARA=('X15','X22'))
    __FONC16 = FORMULE(VALE='X16*X22',NOM_PARA=('X16','X22'))
    __FONC17 = FORMULE(VALE='X17*X21',NOM_PARA=('X17','X21'))
    __FONC18 = FORMULE(VALE='X18*X21',NOM_PARA=('X18','X21'))

    __FONE1 = FORMULE(VALE='X1*X21',NOM_PARA=('X1','X21'))
    __FONE2 = FORMULE(VALE='X2*X21',NOM_PARA=('X2','X21'))
    __FONE3 = FORMULE(VALE='X3*X22',NOM_PARA=('X3','X22'))
    __FONE4 = FORMULE(VALE='X4*X21',NOM_PARA=('X4','X21'))
    __FONE5 = FORMULE(VALE='X5*X21',NOM_PARA=('X5','X21'))
    __FONE6 = FORMULE(VALE='X6*X22',NOM_PARA=('X6','X22'))
    __FONE7 = FORMULE(VALE='X7*X21',NOM_PARA=('X7','X21'))
    __FONE8 = FORMULE(VALE='X8*X21',NOM_PARA=('X8','X21'))
    __FONE9 = FORMULE(VALE='X9*X21',NOM_PARA=('X9','X21'))
    __FONE10 = FORMULE(VALE='X10*X21',NOM_PARA=('X10','X21'))
    __FONE11 = FORMULE(VALE='X11*X21',NOM_PARA=('X11','X21'))
    __FONE12 = FORMULE(VALE='X12*X21',NOM_PARA=('X12','X21'))
    __FONE13 = FORMULE(VALE='X13*X21',NOM_PARA=('X13','X21'))
    __FONE14 = FORMULE(VALE='X14*X21',NOM_PARA=('X14','X21'))
    __FONE15 = FORMULE(VALE='X15*X21',NOM_PARA=('X15','X21'))
    __FONE16 = FORMULE(VALE='X16*X21',NOM_PARA=('X16','X21'))
    __FONE17 = FORMULE(VALE='X17*X21',NOM_PARA=('X17','X21'))
    __FONE18 = FORMULE(VALE='X18*X21',NOM_PARA=('X18','X21'))
    __FONE19 = FORMULE(VALE='X19*X21',NOM_PARA=('X19','X21'))
    
    __FONCC = CREA_CHAMP(
        INFO      = self.INFO,
        TYPE_CHAM = 'ELGA_NEUT_F',
        OPERATION = 'AFFE',
        MODELE    = self.NEW_MODELE,
        PROL_ZERO = 'OUI',
        AFFE      = _F(
               TOUT    = 'OUI',
               NOM_CMP = ('X1','X2','X3','X4','X5','X6','X7','X8','X11','X12','X13','X14','X15','X16','X17','X18'),
               VALE_F  = (__FONC1,__FONC2,__FONC3,__FONC4,__FONC5,__FONC6,__FONC7,__FONC8,__FONC11,__FONC12,__FONC13,
                          __FONC14,__FONC15,__FONC16,__FONC17,__FONC18)),
                          )
    __FONCC2 = CREA_CHAMP(
        INFO      = self.INFO,
        TYPE_CHAM = 'ELGA_NEUT_F',
        OPERATION = 'AFFE',
        MODELE    = self.NEW_MODELE,
        PROL_ZERO = 'OUI',
        AFFE      = _F(
               TOUT    = 'OUI',
               NOM_CMP = tupVar,
     #          VALE_F  = (__FONE1,__FONE2)
               VALE_F  = (__FONE1,__FONE2,__FONE3,__FONE4,__FONE5,__FONE6,__FONE7,__FONE8,__FONE9,__FONE10,__FONE11,__FONE12,__FONE13,__FONE14,__FONE15,__FONE16,__FONE17,__FONE18,__FONE19)
               ),
               )
    listEffg = []
    i=0
    listType=[]
    __SIG11 = [None]*10
    __SIG21 = [None]*10
    __ECR11 = [None]*10

    # Pour masquer certaines alarmes
    from Utilitai.Utmess import MasquerAlarme, RetablirAlarme
    MasquerAlarme('MED_83')
    MasquerAlarme('MED_98')

    while 1:
        # index=1
        # pas = self.ARCHIVAGE['PAS_NBRE']
        # dicDetr=[]
        # if 'Q4GS' in self.modelisations :
            err = 0
            try :
                __SIG11[i] = LIRE_CHAMP(
                    INFO        = self.INFO,
                    TYPE_CHAM   = 'ELGA_SIEF_R',
                    UNITE       = 99,
                    NUME_PT     = 0,
                    MODELE      = self.NEW_MODELE,
                    MAILLAGE    = self.recupere_structure(self.MODELE,'MAILLAGE'),
                    PROL_ZERO   = 'OUI',
                    NOM_MED     = 'CHAMP___CONTRAINTE___00%d'%(i+1),
                    NOM_CMP     = dic_cmp_gauss['CONTRAINTE']['Q4GS']['NOM_CMP'],
                    NOM_CMP_MED = dic_cmp_gauss['CONTRAINTE']['Q4GS']['NOM_CMP_MED'])
                # dicDetr.append({'NOM' : __SIG11})
                DETRUIRE(CONCEPT=_F(NOM = __SIG11[i]), INFO=1)
                listType.append('Q4GS')
                # index=2 
            except :
                err+=1
            try :
                __SIG21[i] = LIRE_CHAMP(
                    INFO        = self.INFO,
                    TYPE_CHAM   = 'ELGA_SIEF_R',
                    UNITE       = 99,
                    NUME_PT     = 0,
                    MODELE      = self.NEW_MODELE,
                    MAILLAGE    = self.recupere_structure(self.MODELE,'MAILLAGE'),
                    PROL_ZERO   = 'OUI',
                    NOM_MED     = 'CHAMP___CONTRAINTE___00%d'%(i+1),
                    NOM_CMP     = dic_cmp_gauss['CONTRAINTE']['DKT3']['NOM_CMP'],
                    NOM_CMP_MED = dic_cmp_gauss['CONTRAINTE']['DKT3']['NOM_CMP_MED']),
                DETRUIRE(CONCEPT=_F(NOM = __SIG21[i]), INFO=1)
                if len(listType)<i+1 :
                    listType.append('DKT3')
            except : 
                err+=1
            if err<2 :
                i+=1
            else :
                break
        # if 'DKT3' in self.modelisations :
            # try : 
                # __SIG21 = LIRE_CHAMP(
                    # INFO        = self.INFO,
                    # TYPE_CHAM   = 'ELGA_SIEF_R',
                    # UNITE       = 99,
                    # NUME_PT     = i*pas,
                    # MODELE      = self.NEW_MODELE,
                    # MAILLAGE    = self.recupere_structure(self.MODELE,'MAILLAGE'),
                    # PROL_ZERO   = 'OUI',
                    # NOM_MED     = 'CHAMP___CONTRAINTE___00%d'%index,
                    # NOM_CMP     = dic_cmp_gauss['CONTRAINTE']['DKT3']['NOM_CMP'],
                    # NOM_CMP_MED = dic_cmp_gauss['CONTRAINTE']['DKT3']['NOM_CMP_MED']),
                # dicDetr.append({'NOM' : __SIG21})

            # except :
                # err +=1
        # if err > 1 :
            # break
        # DETRUIRE(CONCEPT=dicDetr) 
        # i+=1

    # Pour la gestion des alarmes
    RetablirAlarme('MED_83')
    RetablirAlarme('MED_98')


    nbChamp = i
    
    listVari = [0]*nbChamp
    
    for i in xrange(nbChamp) :
            if listType[i] == 'Q4GS' :
               try :
                   __ECR11[i] = LIRE_CHAMP(
                       INFO        = self.INFO,
                       TYPE_CHAM   = 'ELGA_VARI_R',
                       UNITE       = 99,
                       NUME_PT     = 0,
                       MODELE      = self.NEW_MODELE,
                       MAILLAGE    = self.recupere_structure(self.MODELE,'MAILLAGE'),
                       PROL_ZERO   = 'OUI',
                       NOM_MED     = 'CHAMP___ECROUISSAGE__00%d'%(i+1),
                       NOM_CMP     = dic_cmp_gauss['ECROUISSAGE']['Q4GS']['NOM_CMP'],
                       NOM_CMP_MED = dic_cmp_gauss['ECROUISSAGE']['Q4GS']['NOM_CMP_MED'])
                   # dicDetr.append({'NOM' : __SIG11})
                   DETRUIRE(CONCEPT=_F(NOM = __ECR11[i]), INFO=1)
                   listVari[i]=1
               except :
                   err+=1
            elif listType[i] == 'DKT3' :
               try :
                   __ECR11[i] = LIRE_CHAMP(
                       INFO        = self.INFO,
                       TYPE_CHAM   = 'ELGA_VARI_R',
                       UNITE       = 99,
                       NUME_PT     = 0,
                       MODELE      = self.NEW_MODELE,
                       MAILLAGE    = self.recupere_structure(self.MODELE,'MAILLAGE'),
                       PROL_ZERO   = 'OUI',
                       NOM_MED     = 'CHAMP___ECROUISSAGE__00%d'%(i+1),
                       NOM_CMP     = dic_cmp_gauss['ECROUISSAGE']['DKT3']['NOM_CMP'],
                       NOM_CMP_MED = dic_cmp_gauss['ECROUISSAGE']['DKT3']['NOM_CMP_MED'])
                   # dicDetr.append({'NOM' : __SIG11})
                   DETRUIRE(CONCEPT=_F(NOM = __ECR11[i]), INFO=1)
                   listVari[i]=1
               except :
                  err+=1
    
    
    
    itot=len(resu.LIST_PARA()['INST'])
    __EFFG=[None]*itot
    # __EPSG=[None]*itot
    __ECRG=[None]*itot
    __SIG1 = [None]*nbChamp
    __SIG2 = [None]*nbChamp
    __ECR1 = [None]*nbChamp
    __ECR2 = [None]*nbChamp
    for i in xrange(itot) :
        dicAffe=[]
        dicAffe2=[]
        dicAffe3=[]
        index=1
        pas = self.ARCHIVAGE['PAS_NBRE']
        dicAsse = [{'TOUT' : 'OUI', 'CHAM_GD' : __EPN, 'NOM_CMP' : 'X21', 'NOM_CMP_RESU' : 'X21'}]
        dicAsse.append({'TOUT' : 'OUI', 'CHAM_GD' : __EPN2, 'NOM_CMP' : 'X22', 'NOM_CMP_RESU' : 'X22', 'CUMUL' : 'OUI','COEF_R':1.})
        dicAsse2 = [{'TOUT' : 'OUI', 'CHAM_GD' : __UNN, 'NOM_CMP' : 'X21', 'NOM_CMP_RESU' : 'X21'}]
        dicAsse2.append({'TOUT' : 'OUI', 'CHAM_GD' : __UNN, 'NOM_CMP' : 'X22', 'NOM_CMP_RESU' : 'X22', 'CUMUL' : 'OUI','COEF_R':1.})
        dicAsse3 = [{'TOUT' : 'OUI', 'CHAM_GD' : __UNDEU, 'NOM_CMP' : 'X21', 'NOM_CMP_RESU' : 'X21'}]
        dicAsse3.append({'TOUT' : 'OUI', 'CHAM_GD' : __UNDEU, 'NOM_CMP' : 'X22', 'NOM_CMP_RESU' : 'X22', 'CUMUL' : 'OUI','COEF_R':1.})
        dicDetr=[]
        # if 'Q4GS' in self.modelisations :
        for j in xrange(nbChamp) :
            if listType[j] == 'Q4GS' :
                __SIG1[j] = LIRE_CHAMP(
                    INFO        = self.INFO,
                    TYPE_CHAM   = 'ELGA_SIEF_R',
                    UNITE       = 99,
                    NUME_PT     = resu.LIST_PARA()['NUME_ORDRE'][i],
                    MODELE      = self.NEW_MODELE,
                    MAILLAGE    = self.recupere_structure(self.MODELE,'MAILLAGE'),
                    PROL_ZERO   = 'OUI',
                    NOM_MED     = 'CHAMP___CONTRAINTE___00%d'%(j+1),
                    NOM_CMP     = dic_cmp_gauss['CONTRAINTE']['Q4GS']['NOM_CMP'],
                    NOM_CMP_MED = dic_cmp_gauss['CONTRAINTE']['Q4GS']['NOM_CMP_MED'],
                        )
                dicDetr.append({'NOM' : __SIG1[j]})
                dicAsse.append({'TOUT' : 'OUI', 'CHAM_GD' : __SIG1[j], 'NOM_CMP' : tuple(dic_cmp_gauss['CONTRAINTE']['Q4GS']['NOM_CMP']), 
                            'NOM_CMP_RESU' : ('X1','X2','X3','X4','X5','X6','X7','X8'), 'CUMUL' : 'OUI','COEF_R':1.})
                # __EPS1 = LIRE_CHAMP(
                    # INFO        = self.INFO,
                    # TYPE_CHAM   = 'ELGA_EPSI_R',
                    # UNITE       = 99,
                    # NUME_PT     = resu.LIST_PARA()['NUME_ORDRE'][i],
                    # MODELE      = self.NEW_MODELE,
                    # MAILLAGE    = self.recupere_structure(self.MODELE,'MAILLAGE'),
                    # PROL_ZERO   = 'OUI',
                    # NOM_MED     = 'CHAMP___DEFORM_TOT___001',
                    # NOM_CMP     = dic_cmp_gauss['DEFORMATION']['Q4GS']['NOM_CMP'],
                    # NOM_CMP_MED = dic_cmp_gauss['DEFORMATION']['Q4GS']['NOM_CMP_MED'])
                if listVari[j] :
                    __ECR1[j] = LIRE_CHAMP(
                        INFO     = self.INFO,
                        TYPE_CHAM   = 'ELGA_VARI_R',
                        UNITE    = 99,
                        NUME_PT  = resu.LIST_PARA()['NUME_ORDRE'][i],
                        MODELE   = self.NEW_MODELE,
                        MAILLAGE         = self.recupere_structure(self.MODELE,'MAILLAGE'),
                        PROL_ZERO   = 'OUI',
                        NOM_MED  = 'CHAMP___ECROUISSAGE__00%d'%(j+1),
                        NOM_CMP  = dic_cmp_gauss['ECROUISSAGE']['Q4GS']['NOM_CMP'],
                        NOM_CMP_MED = dic_cmp_gauss['ECROUISSAGE']['Q4GS']['NOM_CMP_MED'])
                    dicAsse3.append({'TOUT' : 'OUI', 'CHAM_GD' : __ECR1[j], 'NOM_CMP' : tuple(dic_cmp_gauss['ECROUISSAGE']['Q4GS']['NOM_CMP']), 
                             'NOM_CMP_RESU' : tupVar, 'CUMUL' : 'OUI','COEF_R':1.})
                    dicDetr.append({'NOM' : __ECR1[j]})
                # dicAsse2.append({'TOUT' : 'OUI', 'CHAM_GD' : __EPS1, 'NOM_CMP' : tuple(dic_cmp_gauss['DEFORMATION']['Q4GS']['NOM_CMP']), 
                            # 'NOM_CMP_RESU' : ('X1','X2','X3','X4','X5','X6','X7','X8'), 'CUMUL' : 'OUI','COEF_R':1.})
            else :
                __SIG2[j] = LIRE_CHAMP(
                    INFO        = self.INFO,
                    TYPE_CHAM   = 'ELGA_SIEF_R',
                    UNITE       = 99,
                    NUME_PT     = resu.LIST_PARA()['NUME_ORDRE'][i],
                    MODELE      = self.NEW_MODELE,
                    MAILLAGE    = self.recupere_structure(self.MODELE,'MAILLAGE'),
                    PROL_ZERO   = 'OUI',
                    NOM_MED     = 'CHAMP___CONTRAINTE___00%d'%(j+1),
                    NOM_CMP     = dic_cmp_gauss['CONTRAINTE']['DKT3']['NOM_CMP'],
                    NOM_CMP_MED = dic_cmp_gauss['CONTRAINTE']['DKT3']['NOM_CMP_MED'],
                    )
                dicAsse.append({'TOUT' : 'OUI', 'CHAM_GD' : __SIG2[j], 'NOM_CMP' : tuple(dic_cmp_gauss['CONTRAINTE']['DKT3']['NOM_CMP']),
                                'NOM_CMP_RESU' : ('X11','X12','X13','X14','X15','X16'), 'CUMUL' : 'OUI','COEF_R':1.})
                dicDetr.append({'NOM' : __SIG2[j]})
                # __EPS2 = LIRE_CHAMP(
                    # INFO        = self.INFO,
                    # TYPE_CHAM   = 'ELGA_EPSI_R',
                    # UNITE       = 99,
                    # NUME_PT     = resu.LIST_PARA()['NUME_ORDRE'][i],
                    # MODELE      = self.NEW_MODELE,
                    # MAILLAGE    = self.recupere_structure(self.MODELE,'MAILLAGE'),
                    # PROL_ZERO   = 'OUI',
                    # NOM_MED     = 'CHAMP___DEFORM_TOT___00%d'%index,
                    # NOM_CMP     = dic_cmp_gauss['DEFORMATION']['DKT3']['NOM_CMP'],
                    # NOM_CMP_MED = dic_cmp_gauss['DEFORMATION']['DKT3']['NOM_CMP_MED']),
                if listVari[j] :

                   __ECR2[j] = LIRE_CHAMP(
                       INFO      = self.INFO,
                       TYPE_CHAM   = 'ELGA_VARI_R',
                       UNITE     = 99,
                       NUME_PT   = resu.LIST_PARA()['NUME_ORDRE'][i],
                       MODELE    = self.NEW_MODELE,
                       MAILLAGE  = self.recupere_structure(self.MODELE,'MAILLAGE'),
                       PROL_ZERO   = 'OUI',
                       NOM_MED   = 'CHAMP___ECROUISSAGE__00%d'%(j+1),
                       NOM_CMP   = dic_cmp_gauss['ECROUISSAGE']['DKT3']['NOM_CMP'],
                       NOM_CMP_MED = dic_cmp_gauss['ECROUISSAGE']['DKT3']['NOM_CMP_MED'])
                   dicAsse3.append({'TOUT' : 'OUI', 'CHAM_GD' : __ECR2[j], 'NOM_CMP' : tuple(dic_cmp_gauss['ECROUISSAGE']['DKT3']['NOM_CMP']), 
                                   'NOM_CMP_RESU' : tupVar, 'CUMUL' : 'OUI','COEF_R':1.})
                   dicDetr.append({'NOM' : __ECR2[j]})
                # dicAsse2.append({'TOUT' : 'OUI', 'CHAM_GD' : __EPS2, 'NOM_CMP' : tuple(dic_cmp_gauss['DEFORMATION']['DKT3']['NOM_CMP']), 
                                # 'NOM_CMP_RESU' : ('X11','X12','X13','X14','X15','X16'), 'CUMUL' : 'OUI','COEF_R':1.})
                # dicDetr.append({'NOM' : __EPS2})
#                dicDetr.append({'NOM' : __ECR2})
                # dicDetr.append({'NOM' : __EPS1})
#                dicDetr.append({'NOM' : __ECR1})
        # if 'DKT3' in self.modelisations:

        __SIGN = CREA_CHAMP(
            INFO      = self.INFO,
            TYPE_CHAM = 'ELGA_NEUT_R',
            OPERATION = 'ASSE',
            PROL_ZERO = 'OUI',
            MODELE    = self.NEW_MODELE,
            ASSE      = dicAsse,
            )
        # __EPSN = CREA_CHAMP(
            # INFO      = self.INFO,
            # TYPE_CHAM = 'ELGA_NEUT_R',
            # OPERATION = 'ASSE',
            # PROL_ZERO = 'OUI',
            # MODELE    = self.NEW_MODELE,
            # ASSE      = dicAsse2)
        __ECRN = CREA_CHAMP(
             INFO      = self.INFO,
             TYPE_CHAM = 'ELGA_NEUT_R',
             OPERATION = 'ASSE',
             PROL_ZERO = 'OUI',
             MODELE    = self.NEW_MODELE,
             ASSE      = dicAsse3)
        dicDetr.append({'NOM' : __SIGN})
        # dicDetr.append({'NOM' : __EPSN})
        dicDetr.append({'NOM' : __ECRN})
        __EFFGN = CREA_CHAMP(
            INFO      = self.INFO,
            TYPE_CHAM = 'ELGA_NEUT_R',
            OPERATION = 'EVAL',
            CHAM_F    = __FONCC,
            CHAM_PARA = (__SIGN),
          )
        # __EPSGN = CREA_CHAMP(
            # INFO      = self.INFO,
            # TYPE_CHAM = 'ELGA_NEUT_R',
            # OPERATION = 'EVAL',
            # CHAM_F    = __FONCC,
            # CHAM_PARA   = (__EPSN))
        __ECRGN = CREA_CHAMP(
            INFO      = self.INFO,
            TYPE_CHAM = 'ELGA_NEUT_R',
            OPERATION = 'EVAL',
            CHAM_F    = __FONCC2,
            CHAM_PARA   = (__ECRN))
        dicDetr.append({'NOM' : __EFFGN})
        # dicDetr.append({'NOM' : __EPSGN})
        dicDetr.append({'NOM' : __ECRGN})
        __EFFG[i] = CREA_CHAMP(
            INFO      = self.INFO,
            TYPE_CHAM  = 'ELGA_SIEF_R',
            OPERATION = 'ASSE',
            PROL_ZERO = 'OUI',
            MODELE    = self.NEW_MODELE,
            ASSE      = _F(
                TOUT = 'OUI',
                CHAM_GD = __EFFGN,
                NOM_CMP = ('X1','X2','X3','X4','X5','X6','X7','X8')+('X11','X12','X13','X14','X15','X16'),
                NOM_CMP_RESU = tuple(dic_cmp_gauss['CONTRAINTE']['Q4GS']['NOM_CMP']+dic_cmp_gauss['CONTRAINTE']['DKT3']['NOM_CMP'])),
                )
        # __EPSG[i] = CREA_CHAMP(
            # INFO      = self.INFO,
            # TYPE_CHAM  = 'ELGA_EPSI_R',
            # OPERATION = 'ASSE',
            # PROL_ZERO = 'OUI',
            # MODELE    = self.NEW_MODELE,
            # ASSE      = _F(
                # TOUT = 'OUI',
                # CHAM_GD = __EPSGN,
                # NOM_CMP = ('X1','X2','X3','X4','X5','X6','X7','X8')+('X11','X12','X13','X14','X15','X16'),
                # NOM_CMP_RESU = tuple(dic_cmp_gauss['DEFORMATION']['Q4GS']['NOM_CMP']+dic_cmp_gauss['DEFORMATION']['DKT3']['NOM_CMP'])))
        __ECRG[i] = CREA_CHAMP(
            INFO      = self.INFO,
            TYPE_CHAM  = 'ELGA_VARI_R',
            OPERATION = 'ASSE',
            PROL_ZERO = 'OUI',
            MODELE    = self.NEW_MODELE,
            ASSE      = _F(
                TOUT = 'OUI',
                CHAM_GD = __ECRGN,
                NOM_CMP = tupVar,
                NOM_CMP_RESU = tuple(dic_cmp_gauss['ECROUISSAGE']['Q4GS']['NOM_CMP'])))
# AA        dicAffe.append({'CHAM_GD' : __EFFG[i], 'MODELE' : self.NEW_MODELE,'CHAM_MATER' : self.CHAM_MATER,'INST': resu.LIST_PARA()['INST'][i]})
        dicAffe.append({'CHAM_GD' : __EFFG[i], 'MODELE' : self.NEW_MODELE,'CHAM_MATER' : self.CHAM_MATER, 'CARA_ELEM' : self.CARA_ELEM, 'INST': resu.LIST_PARA()['INST'][i]})
        # dicAffe2.append({'CHAM_GD' : __EPSG[i], 'MODELE' : self.NEW_MODELE,'CHAM_MATER' : self.CHAM_MATER,'INST': resu.LIST_PARA()['INST'][i]})
# AA        dicAffe3.append({'CHAM_GD' : __ECRG[i], 'MODELE' : self.NEW_MODELE,'CHAM_MATER' : self.CHAM_MATER,'INST': resu.LIST_PARA()['INST'][i]})
        dicAffe3.append({'CHAM_GD' : __ECRG[i], 'MODELE' : self.NEW_MODELE,'CHAM_MATER' : self.CHAM_MATER, 'CARA_ELEM' : self.CARA_ELEM, 'INST': resu.LIST_PARA()['INST'][i]})
        DETRUIRE(CONCEPT=dicDetr, INFO=1)
        resu = CREA_RESU(reuse=resu,
            OPERATION = 'AFFE',
            TYPE_RESU = 'EVOL_NOLI',
            NOM_CHAM  = 'SIEF_ELGA',
            AFFE = dicAffe,
            )
        # resu = CREA_RESU(reuse=resu,
            # OPERATION = 'AFFE',
            # TYPE_RESU = 'EVOL_NOLI',
            # NOM_CHAM  = 'EPSI_ELGA',
            # AFFE = dicAffe2)
        resu = CREA_RESU(reuse=resu,
            OPERATION = 'AFFE',
            TYPE_RESU = 'EVOL_NOLI',
            NOM_CHAM  = 'VARI_ELGA',
            AFFE = dicAffe3,
            )

    DEFI_FICHIER(UNITE=unite,ACTION='LIBERER');              

    os.remove(fort)

#-----------------------------------------------------------------------
  def lancer_calcul(self,fichier_med='auto'):

     fichier_epx = self.nom_fichiers['COMMANDE']
#     EXEC_LOGICIEL(LOGICIEL='cd %s ; unset TMPDIR ; %s -usetmpdir %s ; iret=$? ; cd %s ; echo "Code_Retour Europlexus : $iret" ; exit 0' % (self.pwd + self.REPE, self.EXEC, fichier_epx, self.pwd),
#                   CODE_RETOUR_MAXI=-1,
#                   INFO=2)

     EXEC_LOGICIEL(LOGICIEL='cd %s ; unset TMPDIR ; unset PMI_RANK ; %s %s ; iret=$? ; cd %s ; echo "Code_Retour Europlexus : $iret" ; exit 0' % (self.REPE_epx, self.EXEC, fichier_epx, self.pwd),
                   CODE_RETOUR_MAXI=-1,
                   INFO=2)


#------------------------------------------------------------------------
#----------------------------- FIN class EUROPLEXUS ---------------------
#------------------------------------------------------------------------

 
#------------------------------------------------------------------------
#----------------------------- class POUTRE -----------------------------
#------------------------------------------------------------------------

class POUTRE:
  def __init__(self,MAILLAGE,CARA_ELEM):
    
    # recuperer les infos du maillage sous format python 
    self.MApyt = MAIL_PY()
    self.MApyt.FromAster(MAILLAGE)
    self.CARA_ELEM = CARA_ELEM
    # un dictionnaire stockant tous orientations definis dans AFF_CARA_ELEM
    self.dic_gma = self.orientation_cara_elem()
       
#------------------------------------------------------------------------
  def getvecteurs(self,groupes_poutres,verif='non'):
    vecteurs = {}
    ig = 0
    message = 0
    for gr in groupes_poutres :
      ig += 1 
      if not self.dic_gma.has_key(gr): self.dic_gma[gr] = {'ANGL_VRIL':0.0}
      vecteurs[gr] = self.get_vecty_group_ma(gr)  
      if verif == 'oui':
        if ig > 1 :
          if not vecteurs_egaux(vecteurs[gr],vect_old) : message = 1
        vect_old = vecteurs[gr]
    
    if message : UTMESS('F','PLEXUS_10',valk=groupes_poutres)   
    
    return vecteurs   
  
#------------------------------------------------------------------------
  def orientation_cara_elem(self,):
    
    dic_gma = {}
    etapes = self.CARA_ELEM.etape.valeur
    
    if not etapes.has_key('ORIENTATION') : return dic_gma
         
    orientation = etapes['ORIENTATION']
    try :
      test = orientation [0]
    except :
      orientation  = [orientation]

    for ll in orientation  :
      cara = ll['CARA']
      if cara in ['ANGL_VRIL','ANGL_NAUT','VECT_Y'] : 
        if ll.has_key('GROUP_MA') :
          group_ma = ll['GROUP_MA']
          if types.TypeType(group_ma) == types.StringType : group_ma = (group_ma,) 
          a = ll['VALE']
          for gr in group_ma:
            if not dic_gma.has_key(gr): dic_gma[gr] = {}
            dic_gma[gr][cara] = a
    
    for gr in dic_gma.keys() :
      if not dic_gma[gr].has_key('VECT_Y'):
        if not dic_gma[gr].has_key('ANGL_VRIL') : dic_gma[gr]['ANGL_VRIL'] = 0.0
        if dic_gma[gr].has_key('ANGL_NAUT'):
          if len(dic_gma[gr]['ANGL_NAUT']) == 2 : 
            dic_gma[gr]['ANGL_NAUT'] = list(dic_gma[gr]['ANGL_NAUT']) + [dic_gma[gr]['ANGL_VRIL']]
          
    return(dic_gma)
    
#------------------------------------------------------------------------
  def get_vecty_group_ma(self,group_ma):
    
    #if debug: print '*'*10
    #if debug: print 'Test de group_ma',group_ma
    #if debug: print '*'*10

    message = 0

    if self.dic_gma[group_ma].has_key('VECT_Y') : return self.dic_gma[group_ma]['VECT_Y']

    if not self.dic_gma[group_ma].has_key('ANGL_NAUT'):calcul_angle = 1
    else: calcul_angle = 0

    mailles = self.MApyt.gma[string.rstrip(group_ma)]

    for imaille in range(len(mailles)):
      maille = mailles[imaille]
      if calcul_angle:
        alpha,beta = self.calcul_angles_naut(maille)
        angl = [alpha,beta,self.dic_gma[group_ma]['ANGL_VRIL']]
      else:
        angl = self.dic_gma[group_ma]['ANGL_NAUT']

      vect_y = self.angle2vecty(angl)
      if imaille > 1 :  
        if not vecteurs_egaux(vect_y0,vect_y) : message = 1
      vect_y0 = vect_y   

    self.dic_gma[group_ma]['VECT_Y'] = vect_y 

    if message : 
      #if debug: print 'group_ma',group_ma
      UTMESS('F','PLEXUS_11',valk=group_ma) 

    return vect_y        

#------------------------------------------------------------------------
  def angle2vecty(self,angl):
    #transformer en rd
    for iangl in range(len(angl)):
      angl[iangl] = numpy.pi*angl[iangl]/180. 
 
    cosa = math.cos( angl[0] )
    sina = math.sin( angl[0] )
    cosb = math.cos( angl[1] )
    sinb = math.sin( angl[1] )
    cosg = math.cos( angl[2] )
    sing = math.sin( angl[2] )
    vect = [ sing*sinb*cosa - cosg*sina , cosg*cosa + sing*sinb*sina , sing*cosb ]
    for ii in range(len(vect)):
      if abs(vect[ii]) <= tst : vect[ii] = 0.0
    vect = numpy.array(vect)
    vect = vect/norme(vect)
    return vect
    
#------------------------------------------------------------------------
  def get_coor_nodes_maille(self,maille):
    node1,node2 = self.MApyt.co[maille]
    coor1 = self.MApyt.cn[node1]
    coor2 = self.MApyt.cn[node2]
    return [coor1,coor2]
    
#------------------------------------------------------------------------
  def calcul_angles_naut(self,maille):
    
    # Angles directeurs d'une poutre (nautiques)
    
    a,b = self.get_coor_nodes_maille(maille)

    gx = [ b[0]-a[0] , b[1]-a[1] , b[2]-a[2] ]
   
    if( (abs(gx[1]) < tst)  and (abs(gx[0]) <= tst) ):
      alpha = 0.0
    else:
      alpha = math.atan2(gx[1],gx[0])

    p  = math.sqrt( gx[0]*gx[0] + gx[1]*gx[1] )
    if( (abs(gx[2]) < tst ) and (abs(p) <=tst) ):
      beta = 0.0
    else:
      beta  = -math.atan2(gx[2],p)
    
    # alpha et Beta en degre
    alpha = alpha*180./math.pi
    beta = beta*180./math.pi
    
    return alpha,beta

#------------------------------------------------------------------------
#----------------------------- FIN class POUTRE -------------------------
#------------------------------------------------------------------------


#------------------------------------------------------------------------
#----------------------------- class DKT --------------------------------
#------------------------------------------------------------------------

class DKT:

  def __init__(self,MAILLAGE):
   
    self.MAILLAGE = MAILLAGE
    # recuperer les infos du maillage sous format python 
    self.MApyt = MAIL_PY()
    self.MApyt.FromAster(MAILLAGE)
  
  def aster2epx(self,groups):
    
    # initialisations du dic contenant les mailles de tria3 et quad4
    dic_groups = {}
    for cle in ['TRIA3','QUAD4']:
      dic_groups[cle] = []
    # placer les mailles dans le champ associe
    for gr in groups:
      mailles = self.MApyt.gma[string.rstrip(gr)]
      for maille in mailles:
        code_maille = self.MApyt.tm[maille]
        type_maille = self.MApyt.nom[code_maille]
        maille_initiale = string.rstrip(self.MApyt.correspondance_mailles[maille]) 
        if not maille_initiale in dic_groups[type_maille] :
          dic_groups[type_maille].append(maille_initiale) 
        
    # creer le mot-cle facteur permettant de creer les groupes de mailles associes au TRIA3 et QUAD4
    crea_group_ma = []
    # dictionnair contenant les noms des groups qui vont etre creer au besoin
    nom_groups = {}
    for cle in dic_groups.keys() :
      if len(dic_groups[cle]) > 0 :
        crea_group_ma.append({'MAILLE':dic_groups[cle],'NOM':cle})
        nom_groups[cle] = [cle]
      else:
        nom_groups[cle] = []
    # ce n'est pas necessaire de creer les group_ma si on n'a pas de quad4
    if len(dic_groups['QUAD4']) >0:
      
      #global DEFI_GROUP
      
      DEFI_GROUP(reuse         = self.MAILLAGE,
                 MAILLAGE      = self.MAILLAGE,
                 CREA_GROUP_MA = crea_group_ma,
                );
       
    else :
      # on affecte au TRIA les groupes deja fournis en argument
      nom_groups['TRIA3'] = groups
    
    return nom_groups
      
#------------------------------------------------------------------------
#----------------------------- FIN class DKT ----------------------------
#------------------------------------------------------------------------
