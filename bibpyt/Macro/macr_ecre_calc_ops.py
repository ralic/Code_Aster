#@ MODIF macr_ecre_calc_ops Macro  DATE 21/04/2010   AUTEUR BOTTONI M.BOTTONI 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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


def macr_ecre_calc_ops(
  self,
  TABLE,
  DEBIT,
  FISSURE,
  ECOULEMENT,
  TEMPERATURE,
  MODELE_ECRE,
  CONVERGENCE,
  LOGICIEL,
  VERSION,
  ENTETE,
  #COURBES,
  IMPRESSION,
  INFO,
  **args):

  """
     Procedure de couplage Aster-Ecrevisse
     Generation par Aster du fichier de donnees d'Ecrevisse et lancement d'Ecrevisse
  """

  import os, string, types, shutil
  import aster
  from Accas import _F
  #from Noyau.N_utils import AsType
  from Utilitai.Utmess import UTMESS
  from Utilitai.System import ExecCommand

  ier=0


  # PARAMETRES DEBUG
  debug = True



  # Concept sortant
  self.DeclareOut('__TAB',TABLE)
  self.DeclareOut('__DEB',DEBIT)

  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  # Info
  info2 = (INFO==2)
  if debug :
      info2=True

  # Parametres Developpeur
  tmp_ecrevisse = "tmp_ecrevisse"
  fichier_data  = "data.dat"
  defaut        = '00'

  # IMPORTATION DE COMMANDES ASTER
  EXEC_LOGICIEL  = self.get_cmd("EXEC_LOGICIEL")
  CREA_TABLE     = self.get_cmd("CREA_TABLE")
#  IMPR_TABLE     = self.get_cmd("IMPR_TABLE")
  IMPR_FONCTION  = self.get_cmd("IMPR_FONCTION")

  # RECUPERATION DES MOTS-CLES FACTEURS
  dFISSURE=FISSURE[0].cree_dict_valeurs(FISSURE[0].mc_liste)
  for i in dFISSURE.keys():
    if dFISSURE[i]==None : del dFISSURE[i]

  dECOULEMENT=ECOULEMENT[0].cree_dict_valeurs(ECOULEMENT[0].mc_liste)
  for i in dECOULEMENT.keys():
    if dECOULEMENT[i]==None : del dECOULEMENT[i]

  dTEMPERATURE=TEMPERATURE[0].cree_dict_valeurs(TEMPERATURE[0].mc_liste)
  for i in dTEMPERATURE.keys():
    if dTEMPERATURE[i]==None : del dTEMPERATURE[i]

  dMODELE_ECRE=MODELE_ECRE[0].cree_dict_valeurs(MODELE_ECRE[0].mc_liste)
  for i in dMODELE_ECRE.keys():
    if dMODELE_ECRE[i]==None : del dMODELE_ECRE[i]

  dCONVERGENCE=CONVERGENCE[0].cree_dict_valeurs(CONVERGENCE[0].mc_liste)
  for i in dCONVERGENCE.keys():
    if dCONVERGENCE[i]==None : del dCONVERGENCE[i]

  if debug:
      print 'dFISSURE = ', dFISSURE
      print 'dECOULEMENT = ', dECOULEMENT
      print 'dTEMPERATURE = ', dTEMPERATURE
      print 'dMODELE_ECRE = ', dMODELE_ECRE
      print 'dCONVERGENCE = ', dCONVERGENCE
      print 'ENTETE = ', ENTETE
      print 'IMPRESSION = ', IMPRESSION
      print 'INFO = ', INFO




# ---------------------------------------------------------------------
# CONSTRUCTION DU JEU DE PARAMETRES

  d = {}
  d[ 0 ] = ENTETE,

  # FISSURE
  d[ 1 ]  = defaut,
  d[ 2 ]  = str(len(dFISSURE['LISTE_COTES_AH'])),
  d[ 3 ]  = dFISSURE['LISTE_COTES_AH'],
  d[ 4 ]  = dFISSURE['LISTE_VAL_AH'],
  d[ 5 ]  = str(len(dFISSURE['LISTE_COTES_BL'])),
  d[ 6 ]  = dFISSURE['LISTE_COTES_BL'],
  d[ 7 ]  = dFISSURE['LISTE_VAL_BL'],
  d[ 8 ]  = dFISSURE['LONGUEUR'],
  d[ 9 ]  = dFISSURE['ANGLE'],
  d[ 10 ] = dFISSURE['RUGOSITE'],
  d[ 11 ] = dFISSURE['ZETA'],
  if dFISSURE['SECTION'] == 'ELLIPSE':
    d[ 1 ]  = 1,
  if dFISSURE['SECTION'] == 'RECTANGLE':
    d[ 1 ]  = 2,

  # ECOULEMENT
  d[ 20 ] = dECOULEMENT['PRES_ENTREE'],
  d[ 21 ] = dECOULEMENT['PRES_SORTIE'],
  d[ 22 ] = dECOULEMENT['FLUIDE_ENTREE'],
  d[ 23 ] = defaut,
  d[ 24 ] = defaut,
  d[ 25 ] = defaut,
  if dECOULEMENT['FLUIDE_ENTREE'] in [1, 3, 4, 6]:
    d[ 23 ] = dECOULEMENT['TEMP_ENTREE'],
  if dECOULEMENT['FLUIDE_ENTREE'] in [2, 5]:
    d[ 24 ] = dECOULEMENT['TITR_MASS'],
  if dECOULEMENT['FLUIDE_ENTREE'] in [4, 5]:
    d[ 25 ] = dECOULEMENT['PRES_PART'],

  # TEMPERATURE
  d[ 30 ] = defaut,
  d[ 31 ] = defaut,
  d[ 32 ] = defaut,
  d[ 33 ] = defaut,
  d[ 34 ] = defaut,
  d[ 35 ] = defaut,
  d[ 36 ] = defaut,
  d[ 37 ] = defaut,
  d[ 38 ] = defaut,
  d[ 39 ] = defaut,
  d[ 40 ] = defaut,
  if dTEMPERATURE['GRADIENT'] == 'FOURNI':
    d[ 30 ] = -1
    d[ 31 ] = len(dTEMPERATURE['LISTE_COTES_TEMP'])
    d[ 32 ] = dTEMPERATURE['LISTE_COTES_TEMP']
    d[ 33 ] = dTEMPERATURE['LISTE_VAL_TEMP']
  if dTEMPERATURE['GRADIENT'] == 'IMPOSE':
    d[ 30 ] = 0
    d[ 34 ] = dTEMPERATURE['TEMP1']
    d[ 35 ] = dTEMPERATURE['TEMP2']
  if dTEMPERATURE['GRADIENT'] == 'CALCULE':
    d[ 30 ] = 1
    d[ 36 ] = dTEMPERATURE['EPAISSEUR_PAROI']
    d[ 37 ] = dTEMPERATURE['CONVECTION_AMONT']
    d[ 38 ] = dTEMPERATURE['CONVECTION_AVAL']
    d[ 39 ] = dTEMPERATURE['LAMBDA']
    d[ 40 ] = dTEMPERATURE['TEMP_FLUIDE_AVAL']

  # MODELE_ECRE
  d[ 60 ] = defaut,
  d[ 61 ] = defaut,
  if dMODELE_ECRE['ECOULEMENT'] == 'SATURATION':
    d[ 60 ] = 1,
  if dMODELE_ECRE['ECOULEMENT'] == 'GELE':
    d[ 60 ] = 2,
    d[ 61 ] = dMODELE_ECRE['PRESS_EBULLITION'],

  d[ 70 ] = dMODELE_ECRE['FROTTEMENT'],
  d[ 71 ] = defaut,
  d[ 72 ] = defaut,
  d[ 73 ] = defaut,
  d[ 74 ] = defaut,
  d[ 75 ] = defaut,
  if dMODELE_ECRE['FROTTEMENT'] < 0:
    d[ 71 ] = dMODELE_ECRE['REYNOLDS_LIM'],
    d[ 72 ] = dMODELE_ECRE['FROTTEMENT_LIM'],

  d[ 73 ] = dMODELE_ECRE['TRANSFERT_CHAL'],

  if dMODELE_ECRE['TRANSFERT_CHAL'] == 0: pass # Pour memoire 3 cas >0, =0, <0
  if dMODELE_ECRE['TRANSFERT_CHAL']  > 0: pass # Pour memoire 3 cas >0, =0, <0
  if dMODELE_ECRE['TRANSFERT_CHAL']  < 0:
    d[74] = dMODELE_ECRE['XMINCH'],
    d[75] = dMODELE_ECRE['XMAXCH'],

  d[79] = dMODELE_ECRE['IVENAC'],

  if IMPRESSION == 'OUI':
    d[ 84 ] = 1,
  if IMPRESSION == 'NON':
    d[ 84 ] = 0,

  # CONVERGENCE
  d[ 88 ] = dCONVERGENCE['KGTEST'],
  d[ 89 ] = dCONVERGENCE['ITER_GLOB_MAXI'],
  d[ 90 ] = dCONVERGENCE['CRIT_CONV_DEBI'],

  if debug: print d




# ---------------------------------------------------------------------
# GENERATION DU FICHIER DATA.DAT

  txt = fichier_data_ecrevisse()

  for num_param in d.keys():
    if type(d[num_param]) in [int, float]:
      txt0 = str(d[num_param])
    elif type(d[num_param]) in [tuple, list]:
      txt0 = str(d[num_param]).replace('(', '')
      txt0 = txt0.replace(')', '')
      txt0 = txt0.replace(',', '')
      txt0 = txt0.replace("'", '')
      txt0 = txt0.replace("[", '')
      txt0 = txt0.replace("]", '')
    else:
      try:    txt0 = str(d[num_param])
      except: UTMESS('F','ECREVISSE0_11')

    # On remplace la variable dans le fichier par sa valeur
    txt = txt.replace( '$V['+str(num_param)+']', txt0 )

  if debug : print txt




# ---------------------------------------------------------------------
# CREATION DE L'ENVIRONNEMENT D'ETUDE POUR ECREVISSE


  # Repertoire temporaire d'execution d'Ecrevisse
  tmp_ecrevisse = os.path.join(os.getcwd(),tmp_ecrevisse)

  if not os.path.isdir(tmp_ecrevisse) :
    try :
      os.mkdir(tmp_ecrevisse)
    except os.error,erreur :
      if debug :
        print "Code d'erreur de mkdir : " + str(erreur[0]) + " : " + str(erreur[1])
      UTMESS('F','ECREVISSE0_12',valk=[tmp_ecrevisse])

  # On recopie eventuellement l'ancien fichier debits
  src = os.path.join('.', 'REPE_OUT', 'debits_dernier')
  dst = os.path.join(tmp_ecrevisse, 'debits')
  if os.path.isfile(src):
     try:
        shutil.copyfile(src, dst)
     except Exception, e:
        UTMESS('F','ECREVISSE0_24',valk=[src, dst])
        #print "ERREUR : copyfile %s -> %s" % (src, dst)

  # Executable Ecrevisse
  if LOGICIEL :
    if not os.path.isfile(str(LOGICIEL)):
      UTMESS('F','ECREVISSE0_13')
    else:
      chemin_executable = str(LOGICIEL)
  else :
    chemin_executable = os.path.join(aster.repout(), 'ecrevisse')
    #chemin_executable = os.path.join(aster.repout(),version,'ecrevisse') 
    if debug :
      print 'chemin_executable:',chemin_executable

  # Soit on fait un lien symbolique (incompatible avec certaines plate-formes) soit on recopie l'executable
  if not os.path.isfile(os.path.join(tmp_ecrevisse, 'ecrevisse')):
    try:
      os.symlink( chemin_executable, os.path.join(tmp_ecrevisse, 'ecrevisse') )
    except:
      UTMESS('A','ECREVISSE0_14')
      cmd = 'cp ' + chemin_executable + ' ' + os.path.join(tmp_ecrevisse, 'ecrevisse')
      res = os.system(cmd)
      os.chmod( os.path.join(tmp_ecrevisse, 'ecrevisse') ,0755)
      if (res!=0): UTMESS('F','ECREVISSE0_15')

  # Ecriture du fichier de donnees pour Ecrevisse
  fw = open(os.path.join(tmp_ecrevisse, fichier_data),'w')
  fw.write(txt)
  fw.close()

  # Sauvegarde dans REPE_OUT du data.dat
  os.system('cp ' + tmp_ecrevisse + '/data.dat REPE_OUT/')


# ---------------------------------------------------------------------
# EXECUTION D'ECREVISSE

  # Choix du shell
  cmd = '#!sh'
  for shell in ['/bin/sh', '/bin/bash', '/usr/bin/sh']:
    if os.path.isfile(shell):
      cmd = '#!' + shell
      break

#   #Ligne suivante a ajouter avec la version LINUX compilee avec GFORTRAN
#   cmd = cmd + '\nexport LD_LIBRARY_PATH=/logiciels/aster/Linux/GF4/public/gcc-4.1.1/lib:$LD_LIBRARY_PATH'

  # Creation du contenu du script de lancement ecrevisse.sh
  cmd = cmd + '\ncd ' + tmp_ecrevisse + '\n' + os.path.join(tmp_ecrevisse, 'ecrevisse') + ' ' + str(VERSION) + '\nset iret=$?\ncd ..\nexit $iret'
  fw = open(os.path.join(tmp_ecrevisse, 'ecrevisse.sh'),'w')
  fw.write(cmd)
  fw.close()
  os.chmod(os.path.join(tmp_ecrevisse, 'ecrevisse.sh') ,0755)

  # Lancement d'Ecrevisse
  UTMESS('I','ECREVISSE0_16')
  res = ExecCommand(os.path.join(tmp_ecrevisse, 'ecrevisse.sh'),follow_output=True,verbose=True)
  UTMESS('I','ECREVISSE0_17')


  if info2: os.system('ls -al ' + tmp_ecrevisse)




# ---------------------------------------------------------------------
# RECUPERATION DES RESULTATS DEPUIS ECREVISSE


  # 1)  5 COLONNES (a partir de ECREVISSE 3.1.e)
  #       COTE z (m), FLUX THERMIQUE (W/m2), PRESSION TOTALE ABSOLUE (Pa),
  #       TEMPERATURE FLUIDE (degres C), COEFFICIENT DE CONVECTION (W/m2/K)
  try:
      f_ast = open(os.path.join(tmp_ecrevisse, 'pour_aster'),'r')
      _txt = f_ast.read()
      f_ast.close()
      # transforme le texte en liste
      _lst = _txt.split()
      # transforme la liste de textes en liste de float
      _lst = map( float, _lst )

      # ATTENTION : les cotes _ecr_c sont celles des resultats ecrevisse,
      #              c'est a dire les points de milieu du maillage aster,
      #              eventuellement multiplies par 1/TORTUOSITE!
      _ecr_c  = _lst[1:len(_lst):5]
      _ecr_f  = _lst[2:len(_lst):5]
      _ecr_p  = _lst[3:len(_lst):5]
      _ecr_t  = _lst[4:len(_lst):5]
      _ecr_cc = _lst[5:len(_lst):5]

  except:
      UTMESS('F','ECREVISSE0_18')
      _ecr_c  = [-1]
      _ecr_f  = [-1]
      _ecr_p  = [-1]
      _ecr_t  = [-1]
      _ecr_cc = [-1]

  else:
      # On elimine les eventuels points en plus
      #   (cela arrive si la longueur de la fissure est plus grande de la derniere cotes)
      #   mais on considere quand meme la derniere cote du maillage aster
      lx_ast = dFISSURE['LISTE_COTES_AH']
      nbCotesEcre = len(lx_ast)
      _ecr_c  = _ecr_c[0:nbCotesEcre]
      _ecr_f  = _ecr_f[0:nbCotesEcre]
      _ecr_p  = _ecr_p[0:nbCotesEcre]
      _ecr_t  = _ecr_t[0:nbCotesEcre]
      _ecr_cc = _ecr_cc[0:nbCotesEcre]



  # 2)   DEBIT - 5 COLONNES (a partir de ECREVISSE 3.1.e)
  #          DEBIT TOTAL (kg/s), DEBIT AIR (kg/s), DEBIT VAPEUR (kg/s),
  #          DEBIT LIQUIDE (kg/s), TYPE ECOULEMENT
  try:
      f_deb = open(os.path.join(tmp_ecrevisse, 'debits'),'r')
      _tex = f_deb.read()
      f_deb.close()
      _lis = _tex.split()
      _lis = map( float, _lis )

      _dtot  = _lis[0:len(_lis):5]
      _dair  = _lis[1:len(_lis):5]
      _dvap  = _lis[2:len(_lis):5]
      _dliq  = _lis[3:len(_lis):5]
      _ecou  = _lis[4:len(_lis):5]

  except:
      UTMESS('A','ECREVISSE0_18')
      _dtot = [-1]
      _dair = [-1]
      _dvap = [-1]
      _dliq = [-1]
      _ecou = [-1]



# ---------------------------------------------------------------------
# CREATION DES SD-TABLE DE RESULTATS

  __TAB=CREA_TABLE(LISTE=(_F(LISTE_R=_ecr_c,
                           PARA='COTES'),
                        _F(LISTE_R=_ecr_f,
                           PARA='FLUX'),
                        _F(LISTE_R=_ecr_p,
                           PARA='PRESSION'),
                        _F(LISTE_R=_ecr_t,
                           PARA='TEMP'),
                        _F(LISTE_R=_ecr_cc,
                           PARA='COEF_CONV'),
                         ))

  __DEB=CREA_TABLE(LISTE=(_F(LISTE_R=_dtot,
                           PARA='DEBTOT'),
                        _F(LISTE_R=_dair,
                           PARA='DEBAIR'),
                        _F(LISTE_R=_dvap,
                           PARA='DEBVAP'),
                        _F(LISTE_R=_dliq,
                           PARA='DEBLIQ'),
                        _F(LISTE_I=_ecou,
                           PARA='ECOULEMENT'),
                         ))




# ---------------------------------------------------------------------
# DEBUG
  if debug :
      print 'DEBUT DEBUG MACR_ECRE_CALC'
      print 'Chargements donnes par Ecrevisse'
      print 'cotes aster :', lx_ast
      print '_ecr_c  : min=', min(_ecr_c), ' / max=', max(_ecr_c), ' / ', _ecr_c
      print '_ecr_f  : min=', min(_ecr_f), ' / max=', max(_ecr_f), ' / ', _ecr_f
      print '_ecr_p  : min=', min(_ecr_p), ' / max=', max(_ecr_p), ' / ', _ecr_p
      print '_ecr_t  : min=', min(_ecr_t), ' / max=', max(_ecr_t), ' / ', _ecr_t
      print '_ecr_cc : min=', min(_ecr_cc), ' / max=', max(_ecr_cc), ' / ', _ecr_cc
      print '_dtot=', _dtot
      print '_dair=', _dair
      print '_dvap=', _dvap
      print '_dliq=', _dliq
      print '_ecou=', _ecou
      print __DEB.EXTR_TABLE()
      print __TAB.EXTR_TABLE()
      print 'FIN DEBUG MACR_ECRE_CALC'



# ---------------------------------------------------------------------
# RECUPERATION DES RESULTATS OPTIONNELS DEPUIS ECREVISSE
# A finir
#  lst_fic = os.listdir(tmp_ecrevisse)



# ---------------------------------------------------------------------
# FIN MACR_ECRE_CALC
  return ier




# ---------------------------------------------------------------------
def fichier_data_ecrevisse():

  """
     Modele du fichier data.dat a la syntaxe Ecrevisse 3.0
     Cette chaine de caractere est l'exacte replique d'un fichier de donnees
     Ecrevisse 3.0 dans lequel toutes les donnees numeriques ont ete
     remplacees par $V[x].
  """

  txt = """$V[0]


      DONNEES GEOMETRIQUES RELATIVES A LA FISSURE
      *******************************************

$V[1]        is: type de section  (is=1 ellipse is=2 rectangle)
$V[2]        nb points decrivant ah: grand axe  (m)
$V[3]
$V[4]
$V[5]        nb points decrivant bl: petit axe (m)
$V[6]
$V[7]
$V[8]        zl: longueur totale de la fissure (m)
$V[9]        theta: angle par rapport a la verticale ascendante (en degres)
$V[10]       eps: rugosite absolu (m)
$V[11]       zeta: coefficient de la perte de charge singuliere a l'entree

      DONNEES RELATIVES A L"ECOULEMENT
      ********************************

$V[20]       pe: pression de stagnation a l'entree (Pa)
$V[21]       ps: pression de stagnation a la sortie (Pa)
$V[22]       iflow : condition du fluide a l'entree
               iflow=1  : eau sous-refroidie ou saturee
               iflow=2  : fluide diphasique
               iflow=3  : vapeur saturee ou surchauffee
               iflow=4  : air + vapeur surchauffee
               iflow=5  : air + vapeur saturee
               iflow=6  : air seul
$V[23]       te: temperature a l'entree (deg C) (utilise si iflow=1,3,4,6)
$V[24]       xe: titre m. eau vap/eau tot a l'entree (utilise si iflow=2 ou 5)
$V[25]       pae : pression partielle air en entree (Pa) (utilise si iflow>3)

      DONNEES RELATIVES AU PROFIL DE TEMPERATURE A TRAVERS LA PAROI
      *************************************************************

$V[30]       imograd : modele de calcul du gradient de temperature
               imograd=-1/ distribution imposee de temperature (max 100pts)
               imograd=0 : profil impose de la temperature
               imograd=1 : calcul du profil
$V[31]       cas imograd = -1 : ntmat, nb de points donnes ensuite (max=100)
$V[32]
$V[33]
$V[34]       tm1: grad. temp. paroi le lg de l'ecoul.(utilise si imograd=0) (degC/m)
$V[35]       tm2: temperature de la paroi a l'entree (utilise si imograd=0) (degC)
$V[36]       epp: epaisseur de la paroi (utilise si imograd=1) (m)
$V[37]       alphe: coeff. convection face amont (utilise si imograd=1) (W/degC/m2)
$V[38]       alphs: coeff. convection face aval  (utilise si imograd=1) (W/degC/m2)
$V[39]       lambd: conduct. thermique de la paroi (utilise si imograd=1) (W/degC/m)
$V[40]       ts: temperature du fluide cote aval (utilise si imograd=1) (degC)

      CHOIX DES MODELES
      *****************

Modeles d'ecoulement diphasique
-------------------------------
$V[60]       imod : type de modele d'ecoulement diphasique
               imod=1 : modele d'ecoulement a saturation a l'entree
               imod=2 : modele d'ecoulement 'gele' a l'entree
$V[61]       corrp: press. d'ebullition = corrp*psat(t)

Choix des correlations de frottement et de transfert de chaleur
---------------------------------------------------------------
$V[70]       ifrot : frottement (avec : ifrot=1 ou -1 ; sans :  ifrot=0 )
$V[71]       cas ifrot=-1 : relim = Re limite
$V[72]       cas ifrot=-1 : frtlim = coeff frottement si Re > relim
$V[73]       ichal : transfert de chaleur (-2 <= ichal <= 2  ichal=0 ==> Pas d'echange de chaleur)
$V[74]       cas ichal < 0 : xminch = titre m. gaz avec corr. Chen ou liq pour x<xminch
$V[75]       cas ichal < 0 : xmaxch = titre m. gaz avec corr. melange pour x>xmaxch

Modelisation de la vena contracta
---------------------------------
$V[79]       ivenac : avec = 1, sans = 0

Procedure d'impression
----------------------
$V[84]       iprint : impression profils ecran ( oui:1 non:0 )

      DONNEES RELATIVES A LA CONVERGENCE NUMERIQUE
      ********************************************

$V[88]       kgtest (0 < kgtest < 1) tel que g=kgtest*gmax+(1-kgtest)*gmin
$V[89]       itnmax : nombre maximum iterations methode newton
$V[90]       precdb : critere de convergence en debit
"""

  return txt
