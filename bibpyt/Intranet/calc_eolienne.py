#@ MODIF calc_eolienne Intranet  DATE 18/09/2007   AUTEUR DURAND C.DURAND 

#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
#
# LISTE DES FONCTIONS, MACROS et CATALOGUES
#
# MACROS
#
# calc_char_houle
# macr_calc_eolienne
#
# FONCTIONS
#
# mail_eolienne
# eolien_regression
# lect_resu_stream
# donn_boucle_pas_temps
# extr_char_houle
#
# CATALOGUES
#
# CALC_CHAR_HOULE
# MACR_CALC_EOLIENNE
#
#____________________________________________________________________
#
# On charge les modules necessaires
import os
import Numeric
import re
import math
#
# permet de definir dans le meme fichier les catalogues et les macros
from Cata.cata import *
#
#____________________________________________________________________
#____________________________________________________________________
#
#____________________________________________________________________
#___________________ DEBUT MACRO calc_char_houle ____________________
#
#
def calc_char_houle_ops(self, INFO, STREAM, IMPRESSION, **args):
#
#  1. args est le dictionnaire des arguments
#    args.keys() est la liste des mots-clés
#    args.keys()[0] est la premiere valeur de cette liste
#    args.keys()[1:] est la liste des valeurs suivantes dans cette liste
#    args.keys(mot_cle) représente le contenu de la variable mot_cle dans la macro appelante.

  """ Calcule le chargement du a la houle.  """

# On charge les modules nécessaires
  from Accas import _F
  import aster

#____________________________________________________________________
#
# 1. Préalables
#____________________________________________________________________

# 1.1 ==> La macro compte pour 1 dans l'exécution des commandes

  self.set_icmd(1)

# 1.2 ==> On importe les définitions des commandes Aster utilisées
#         dans la macro

  EXEC_LOGICIEL = self.get_cmd("EXEC_LOGICIEL")
  IMPR_TABLE= self.get_cmd("IMPR_TABLE")
  DEFI_FICHIER= self.get_cmd("DEFI_FICHIER")
  
# 1.3 ==> Le nom des programmes à lancer

#  repertoire_outils = "/home/eolien/ASTER_EOLIEN/"
  repertoire_outils = aster.repout()
  calc_houle = repertoire_outils + "calc_houle"

# 1.4 ==> Initialisations

  erreur = 0
  erreur_partiel = [0]
  Rep_Calc_ASTER = os.getcwd()

  messages_erreur = { 0 : "<CALC_CHAR_HOULE> Tout va bien.",
                      1 : "<CALC_CHAR_HOULE> Impossible de créer le répertoire de travail pour le logiciel de calcul de houle.",
                    100 : "<CALC_CHAR_HOULE> Erreur." }

  while not erreur :

#____________________________________________________________________
#
# 2. Répertoires et fichiers
#____________________________________________________________________
#

# 2.1. ==> Création du répertoire pour l'exécution du logiciel STREAM_FM

    Nom_Rep_local_houle = "tmp_stream"
    Rep_Calc_LOGICIEL_local_houle = os.path.join(".",Nom_Rep_local_houle)
    Rep_Calc_LOGICIEL_global_houle = os.path.join(Rep_Calc_ASTER,Nom_Rep_local_houle)

    try :
      os.mkdir(Rep_Calc_LOGICIEL_global_houle)
    except os.error,erreur_partiel :
      self.cr.warn("Code d'erreur de mkdir : " + str(erreur_partiel[0]) + " : " + erreur_partiel[1])
      self.cr.fatal("Impossible de créer le répertoire de travail pour le logiciel de calcul de houle : "+Rep_Calc_LOGICIEL_global_houle)
      erreur = 1
      break

# 2.2. ==> On cree un fichier annexe pour transmettre des infos à la procédure de lancement
#          de STREAM_FM

    fic_Info_ASTER = os.path.join(Rep_Calc_LOGICIEL_global_houle,"InfoExecASTER")
    f_execAster = open(fic_Info_ASTER, "w")
    f_execAster.write(str(INFO)+"\n")
    f_execAster.close()

#____________________________________________________________________
#
# 3. Décodage des arguments de la macro-commande
#    et création des fichiers pour STREAM_FM
#____________________________________________________________________

# 3.1. ==> Les fichiers pour le logiciel STREAM_FM
#          Ils sont créés dans le répertoire d'exécution du logiciel, avec leurs noms officiels

    fic_data_HT = os.path.join(Rep_Calc_LOGICIEL_global_houle,"AMA_HT.par")
    fic_data_FM = os.path.join(Rep_Calc_LOGICIEL_global_houle,"AMA_FM.par")

# 3.2 ==> Construction du fichier 'data_HT'

# 3.2.1 ==> Ouverture du fichier data_HT dans un répertoire temporaire

    f_menu = open(fic_data_HT, "w")

# 3.2.2 ==> Lecture et écriture des données nécessaires

    prof          = STREAM['PROFONDEUR']
    h_houle       = STREAM['H_HOULE']
    peri_houle    = STREAM['PERI_HOULE']
    courant_euler = STREAM['COUR_EULERIEN']
    ordre_fonc    = STREAM['ORDR_FONC_COURAN']
  
    f_menu.write(str(prof)+" <= Profondeur d'eau (m)\n")
    f_menu.write(str(h_houle)+" <= Hauteur de houle (crete-a-creux) (m)\n")
    f_menu.write(str(peri_houle)+" <= Periode de houle (s)\n")
    f_menu.write(str(courant_euler)+" <= Vitesse du courant eulerien (m/s)\n")
    f_menu.write(str(ordre_fonc)+" <= ordre de la fonction de courant (m/s)\n")

# 3.2.3 ==> Fermeture du fichier data_HT

    f_menu.close()

    if INFO >= 2 :
      print "===========================\n"
      print "Contenu du fichier data_HT :"
      fichier = open (fic_data_HT,"r")
      les_lignes = fichier.readlines()
      fichier.close()
      for ligne in les_lignes :
        print ligne[:-1]
      print "===========================\n"

# 3.3 ==> Construction du fichier "data_FM"

# 3.3.1 ==> Ouverture du fichier

    f_FM = open(fic_data_FM, "w")

# 3.3.2 ==> Lecture et écriture des données nécessaires

    diam         = STREAM['DEXT_HAUT_BASE']
    coef_trainee = STREAM['COEF_TRAINEE']
    coef_inertie = STREAM['COEF_INERTIE']
    nb_nive_z    = STREAM['NB_POINTS_VERT']
    nb_dt_par_t  = STREAM['NB_INTER_PERI']
  
    f_FM.write(str(diam)+" <= Diametre du cylindre (m)\n")
    f_FM.write(str(coef_trainee)+" <= Coefficient de trainee Cd\n")
    f_FM.write(str(coef_inertie)+" <= Coefficient d'inertie Cm\n")
    f_FM.write(str(nb_nive_z)+" <= Nbe de points sur la verticale\n")
    f_FM.write(str(nb_dt_par_t)+" <= Nbe de pas de temps sur une periode de houle\n")

# 3.3.3 ==> Fermeture du fichier data_FM

    f_FM.close()

    if INFO >= 2 :
      print "===========================\n"
      print "Contenu du fichier data_FM :"
      fichier = open (fic_data_FM,"r")
      les_lignes = fichier.readlines()
      fichier.close()
      for ligne in les_lignes :
        print ligne[:-1]
      print "===========================\n"

# 3.4 ==> Nom du fichier qui contiendra le résultat

    if (IMPRESSION['UNITE'] != None) :
      Unit_Fich_Resu = 'fort.'+str(IMPRESSION['UNITE'])

#____________________________________________________________________
#
# 4. Ecriture de la commande d"exécution du logiciel STREAM
#
#   Remarque : dans la donnée de la version du logiciel STREAM, il faut remplacer
#              le _ de la donnée par un ., qui
#              est interdit dans la syntaxe du langage de commandes ASTER
#   Remarque : il faut remplacer le N majuscule de la donnee par
#              un n minuscule, qui est interdit dans la syntaxe du langage
#              de commandes ASTER
#____________________________________________________________________
#

    VERSION="aster"
    LOGICIEL = "STREAM"

    EXEC_LOGICIEL ( ARGUMENT = (Rep_Calc_LOGICIEL_global_houle, # nom du repertoire du calcul de houle
                                LOGICIEL,                       # nom du logiciel de calcul de houle
                                VERSION,                        # version du logiciel de calcul de houle
                               ),
                    LOGICIEL = calc_houle
                  )

#____________________________________________________________________
#
# 5. Lecture du fichier de résultats STREAM_FM
#____________________________________________________________________
#
# 5.1 ==> Ouverture, lecture et fermeture du fichier "AMA_FM.res"

    if (IMPRESSION['UNITE'] != None) :
      fic_resuStream = os.path.join(Rep_Calc_LOGICIEL_global_houle,"AMA_FM.res")
      f_resu = open(fic_resuStream, "r")
      lignes = f_resu.readlines()
      f_resu.close()

      Fic_Tabl_Resu = os.path.join(Rep_Calc_ASTER,Unit_Fich_Resu)
      f_resu = open(Fic_Tabl_Resu, "w")
      f_resu.writelines(lignes)
      f_resu.close()
#
#____________________________________________________________________
#
# 6. C'est fini !
#____________________________________________________________________
#    
    break

# 6.1. ==> Arret en cas d'erreur

  if erreur :
    if not messages_erreur.has_key(erreur) :
      erreur = 100
    self.cr.fatal(messages_erreur[erreur])

  return

#____________________________________________________________________
#____________________ FIN MACRO calc_char_houle _____________________
#
#____________________________________________________________________
#___________________ DEBUT FONCTION mail_eolienne ___________________
#
#
# ===>   Creation des fichiers de commandes GIBI
#____________________________________________________________________
#

def mail_eolienne(H_TOTALE, H_BASE, H_IMMERGEE, H_MOYEU, H_JONCTION, DECAL_PALES,
                  DEXT_NACELLE, EPAIS_NACELLE, DEXT_HAUT_BASE, DEXT_BAS_BASE,
                  EPAIS_HAUT_BASE, EPAIS_BAS_BASE, DEXT_HAUT_FUT, DEXT_BAS_FUT,
                  EPAIS_HAUT_FUT, EPAIS_BAS_FUT, NB_ELEM_BASE, NB_ELEM_FUT,
                  NBEL_EPAIS_BASE, NBEL_EPAIS_FUT, NBEL_DCIR_BASE, NBEL_DCIR_FUT,
                  ANGLE_VENT_AXE_X,
                  MODELISATION, TYPE_ELEM, NIVE_GIBI, fichier_datg) :

#
# 1 ==> Reccuperation du repertoire dans lequel s'execute le calcul ASTER
#
  Rep_Calc_ASTER = os.getcwd()

#
# 2 ==> Decoupage des mailles de la base en fonction du niveau d'eau
#
  lb  = H_BASE / NB_ELEM_BASE
  nbi = int(H_IMMERGEE/lb)
  hbe = H_BASE - H_IMMERGEE

  # CAS 1
  if ( ((H_IMMERGEE/lb)-nbi) < 0.5 ) :
      lbi = H_IMMERGEE/nbi
      if (hbe > 0.0) :
          nbe = NB_ELEM_BASE - nbi
          lbe = hbe/nbe
      else :
          nbe = 0
  # CAS 2
  if ( ((H_IMMERGEE/lb)-nbi) >= 0.5 ) :
      lbi = H_IMMERGEE/(nbi+1)
      nbe = NB_ELEM_BASE - (nbi+1)
      lbe = hbe/nbe
      nbi = nbi + 1

#
# 3 ==> Ecriture des donnees necessaires dans le fichier auxiliaire
#
  if MODELISATION == 'POUTRE' :   

    # 3.1 ==> Ouverture du fichier auxiliaire

    fichier_auxi = os.path.join(Rep_Calc_ASTER,"fichaux")
    fdaux = open (fichier_auxi,'w')

    # 3.2 ==> Ecriture des donnees dans le fichier auxiliaire

    j = 0

    for i in range(nbi) :
      j = j+1
      fdaux.write("base"+str(j)+" = baset . "+str(j)+";\n")
    if hbe > 0.0 :
      for i in range(nbe) :
        j = j+1
        fdaux.write("base"+str(j)+" = baset . "+str(j)+";\n") 

    for i in range(NB_ELEM_FUT) :
      fdaux.write("fut"+str(i+1)+" = futt . "+str(i+1)+";\n")

    fdaux.write("SAUV FORM eolienne;\n")
    fdaux.write("FIN;\n")

    # 3.3 ==> Fermeture du fichier auxiliaire

    fdaux.close()

#
# 4 ==> Ecriture des donnees necessaires dans le fichier de donnees GIBI
#

# 4.1 ==> Ouverture du fichier de données GIBI

  fdgib = open (fichier_datg,'w')


# 4.2 ==> Ecriture des donnees dans le fichier de donnees GIBI

  fdgib.write("*********************************\n")
  fdgib.write("** Maillage d'une eolienne en mer\n")
  fdgib.write("*********************************\n")
  fdgib.write("***********************\n")
  fdgib.write("***********************\n")
  fdgib.write("** donnees geometriques\n")
  fdgib.write("***********************\n")

  fdgib.write("opti nive "+str(NIVE_GIBI)+";\n")

  if MODELISATION == 'POUTRE' :
    fdgib.write("modbase = 'poutre';\n")
    fdgib.write("modfut = 'poutre';\n")
  if MODELISATION == 'COQUE' :
    fdgib.write("modbase = 'coque';\n")
    fdgib.write("modfut = 'coque';\n")
  if MODELISATION == '3D' :
    fdgib.write("modbase = '3D';\n")
    fdgib.write("modfut = '3D';\n")
    if TYPE_ELEM == 'CUB8' : fdgib.write("quad = 'non';\n")
    if TYPE_ELEM == 'CU20' : fdgib.write("quad = 'oui';\n")

  fdgib.write("**** hauteur totale du monopode (hors nacelle)\n") 
  fdgib.write("h_totale = "+str(H_TOTALE)+";\n")   
  fdgib.write("**** hauteur de la base du monopode\n") 
  fdgib.write("hb = "+str(H_BASE)+";\n")   
  fdgib.write("**** hauteur immergee (deduite du calcul par STREAM)\n") 
  fdgib.write("hbi = "+str(H_IMMERGEE)+";\n")   
  fdgib.write("**** nombre de mailles immergees de la base\n") 
  fdgib.write("nbi = "+str(nbi)+";\n")   
  fdgib.write("**** hauteur emergee\n") 
  fdgib.write("hbe = "+str(hbe)+";\n")   
  fdgib.write("**** nombre de mailles emergees de la base\n") 
  fdgib.write("nbe = "+str(nbe)+";\n")   
  fdgib.write("**** decoupage vertical\n") 
  fdgib.write("nb_base = "+str(NB_ELEM_BASE)+";\n")   
  fdgib.write("nb_fut = "+str(NB_ELEM_FUT)+";\n")   
  fdgib.write("**** diametre et epaisseur bas du fut\n") 
  fdgib.write("dex_bfut = "+str(DEXT_BAS_FUT)+";\n")   
  fdgib.write("ep_bfut = "+str(EPAIS_BAS_FUT)+";\n")   
  fdgib.write("**** diametre et epaisseur haut du fut\n") 
  fdgib.write("dex_hfut = "+str(DEXT_HAUT_FUT)+";\n")   
  fdgib.write("ep_hfut = "+str(EPAIS_HAUT_FUT)+";\n")   
  fdgib.write("**** diametre et epaisseur bas de la base\n") 
  fdgib.write("dex_bbas = "+str(DEXT_BAS_BASE)+";\n")   
  fdgib.write("ep_bbas = "+str(EPAIS_BAS_BASE)+";\n")   
  fdgib.write("**** diametre et epaisseur haut de la base\n") 
  fdgib.write("dex_hbas = "+str(DEXT_HAUT_BASE)+";\n")   
  fdgib.write("ep_hbas = "+str(EPAIS_HAUT_BASE)+";\n")   

  if MODELISATION == '3D' or MODELISATION == 'COQUE' :   
    fdgib.write("**** nombre d'elements sur un demi-cercle\n") 
    fdgib.write("nbcirfut = "+str(NBEL_DCIR_FUT)+";\n")   
    fdgib.write("nbcirbas = "+str(NBEL_DCIR_BASE)+";\n")  
    fdgib.write("**** hauteur de la jonction base/fut\n") 
    fdgib.write("hbj = "+str(H_JONCTION)+";\n")   
  if MODELISATION == '3D' :     
    fdgib.write("**** nombre d'elements dans l'epaisseur\n") 
    fdgib.write("nbep_fut = "+str(NBEL_EPAIS_FUT)+";\n")   
    fdgib.write("nbep_bas = "+str(NBEL_EPAIS_BASE)+";\n")   

  fdgib.write("**** angle entre la pression du vent et l'axe X (degres)\n") 
  fdgib.write("alpha = "+str(ANGLE_VENT_AXE_X)+";\n")   
  fdgib.write("**** decalage pales\n") 
  fdgib.write("dec0 = "+str(DECAL_PALES)+";\n")   
  fdgib.write("**** hauteur moyeu\n") 
  fdgib.write("hp0 = "+str(H_MOYEU)+";\n")   

  fdgib.write("opti echo 0;\n")   
#  loc_datg = "/home/eolien/DATG/"
  loc_datg = aster.repdex()
  fdgib.write("* \n")   
  fdgib.write(open(os.path.join(loc_datg, 'calc_eolienne.datg'), 'r').read())

# 4.3 ==> Fermeture du fichier de donnees GIBI

  fdgib.close()

# 5 ==> FIN

  return

#____________________________________________________________________
#____________________ FIN FONCTION mail_eolienne ____________________
#
#____________________________________________________________________
#
#____________________________________________________________________
#_________________ DEBUT FONCTION eolien_regression _________________
#
#
# ===>   Regression selon les moindres carres
#____________________________________________________________________
#
#
def eolien_regression (liste, INFO,) :
#
#
#____________________________________________________________________
#
#                    MOINDRES CARRES
#____________________________________________________________________
#
# Calcul des coefficients teta0, teta1, teta2 et teta3 de
# l'application qui passe au mieux (au sens des moindres carres)
# par les couples (profondeur,force lineique).


# 1 ==> Initialisation


# 2 ==> Nombre de points calcules par STREAM selon l'axe vertical

  dim = liste.shape[0]

# Controle : dim == Nb_valeur  a voir

# 3 ==> Definition des polynomes

  global P0, P1, P2, P3

  def P0():
      return 1

  def P1(alpha1, x):
      return (x-alpha1)

  def P2(alpha1, alpha2, beta2, x):
      return (x-alpha2)*(x-alpha1) - beta2

  def P3(alpha1, alpha2, alpha3, beta2, beta3, x):
      return (x-alpha3)*((x-alpha2)*(x-alpha1) - beta2) - beta3*(x-alpha1)

# 4 ==> Definition de la fonction polynomiale de degre 3

  def FF3(alpha1, alpha2, alpha3, beta2, beta3, teta0, teta1, teta2, teta3, x):
      return teta0*P0() + teta1*P1(alpha1,x) + teta2*P2(alpha1,alpha2,beta2,x) + teta3*P3(alpha1,alpha2,alpha3,beta2,beta3,x)

# 5 ==> Calcul des coefficients alpha1, alpha2, alpha3, beta2 et beta3

  numerateur_alpha1 = 0
  denominateur_alpha1 = 0
  alpha1 = 0
  for i in range(dim) :
     numerateur_alpha1 = numerateur_alpha1 + liste[[i,0]]*P0()**2
     denominateur_alpha1 = denominateur_alpha1 + P0()**2

  alpha1 = numerateur_alpha1/denominateur_alpha1

  numerateur_alpha2 = 0
  denominateur_alpha2 = 0
  alpha2 = 0
  numerateur_beta2 = 0
  denominateur_beta2 = 0
  beta2 = 0
  for i in range(dim) :
     numerateur_alpha2 = numerateur_alpha2 + liste[[i,0]]*P1(alpha1,liste[[i,0]])**2
     denominateur_alpha2 = denominateur_alpha2 + P1(alpha1,liste[[i,0]])**2
     numerateur_beta2 = numerateur_beta2 + P1(alpha1,liste[[i,0]])**2
     denominateur_beta2 = denominateur_beta2 + P0()**2

  alpha2 = numerateur_alpha2/denominateur_alpha2
  beta2 = numerateur_beta2/denominateur_beta2

  numerateur_alpha3 = 0
  denominateur_alpha3 = 0
  alpha3 = 0
  numerateur_beta3 = 0
  denominateur_beta3 = 0
  beta3 = 0
  for i in range(dim) :
     numerateur_alpha3 = numerateur_alpha3 + liste[[i,0]]*P2(alpha1,alpha2,beta2,liste[[i,0]])**2
     denominateur_alpha3 = denominateur_alpha3 + P2(alpha1,alpha2,beta2,liste[[i,0]])**2
     numerateur_beta3 = numerateur_beta3 + P2(alpha1,alpha2,beta2,liste[[i,0]])**2
     denominateur_beta3 = denominateur_beta3 + P1(alpha1,liste[[i,0]])**2

  alpha3 = numerateur_alpha3/denominateur_alpha3
  beta3 = numerateur_beta3/denominateur_beta3

# 6 ==> Calcul des estimateurs cherches : teta0, teta1, teta2, teta3

  numerateur_teta0 = 0
  denominateur_teta0 = 0
  teta0 = 0
  for i in range(dim) :
     numerateur_teta0 = numerateur_teta0 + liste[[i,1]]*P0()
     denominateur_teta0 = denominateur_teta0 + P0()**2

  teta0 = numerateur_teta0/denominateur_teta0

  numerateur_teta1 = 0
  denominateur_teta1 = 0
  teta1 = 0
  for i in range(dim) :
     numerateur_teta1 = numerateur_teta1 + liste[[i,1]]*P1(alpha1,liste[[i,0]])
     denominateur_teta1 = denominateur_teta1 + P1(alpha1,liste[[i,0]])**2

  teta1 = numerateur_teta1/denominateur_teta1

  numerateur_teta2 = 0
  denominateur_teta2 = 0
  teta2 = 0
  for i in range(dim) :
     numerateur_teta2 = numerateur_teta2 + liste[[i,1]]*P2(alpha1,alpha2,beta2,liste[[i,0]])
     denominateur_teta2 = denominateur_teta2 + P2(alpha1,alpha2,beta2,liste[[i,0]])**2

  teta2 = numerateur_teta2/denominateur_teta2

  numerateur_teta3 = 0
  denominateur_teta3 = 0
  teta3 = 0
  for i in range(dim) :
     numerateur_teta3 = numerateur_teta3 + liste[[i,1]]*P3(alpha1,alpha2,alpha3,beta2,beta3,liste[[i,0]])
     denominateur_teta3 = denominateur_teta3 + P3(alpha1,alpha2,alpha3,beta2,beta3,liste[[i,0]])**2

  teta3 = numerateur_teta3/denominateur_teta3

# 7 ==> Impression de certains resultats si INFO == 2

  if (INFO == 2) :
    print "  "
    print "<EOLIEN_REGRESSION>   RESULTATS  "
    print "  "
    print "<EOLIEN_REGRESSION> teta0 = ", teta0
    print "<EOLIEN_REGRESSION> teta1 = ", teta1
    print "<EOLIEN_REGRESSION> teta2 = ", teta2
    print "<EOLIEN_REGRESSION> teta3 = ", teta3
    print "  "

    print "<EOLIEN_REGRESSION>  ===>  VERIFICATION INTERPOLATION\n"
    print " No point - profondeur - valeur STREAM - valeur par moindres carres \n"
    for i in range(dim) :
      print '  %3d   %12.4f     %12.4f     %12.4f' % (i+1,liste[[i,0]],liste[[i,1]],FF3(alpha1, alpha2, alpha3, beta2, beta3, teta0, teta1, teta2, teta3,liste[[i,0]]))

  return (teta0,teta1,teta2,teta3,alpha1,alpha2,alpha3,beta2,beta3)

#____________________________________________________________________
#_________________ FIN FONCTION eolien_regression ___________________
#
#____________________________________________________________________
#
#____________________________________________________________________
#_________________ DEBUT FONCTION lect_resu_stream __________________
#
#
# ===>   Lecture du fichier de resultats STREAM_FM
#____________________________________________________________________
#

def lect_resu_stream(INFO) :

# 1 ==> Ouverture, lecture et fermeture du fichier "AMA_FM.res"

  Rep_Calc_ASTER = os.getcwd()
  Nom_Rep_local_houle = "tmp_stream"
  Rep_Calc_LOGICIEL_local_houle = os.path.join(".",Nom_Rep_local_houle)
  Rep_Calc_LOGICIEL_global_houle = os.path.join(Rep_Calc_ASTER,Nom_Rep_local_houle)

  fic_resuStream = os.path.join(Rep_Calc_LOGICIEL_global_houle,"AMA_FM.res")
  f_resu = open(fic_resuStream, "r")
  lignes = f_resu.readlines()
  f_resu.close()

# 2 ==> Lecture du nombre de points (valeurs de forces lineiques).
#       Lecture du nombre par lequel la periode a ete decoupee
#       ATENTION ==> le nombre de pas de temps est egal a ce nombre plus un.

  compt_temps = 0
  compt_valeur = 0
  dict_temps = {}

  for i in range(len(lignes)) :
    ligne = lignes[i].split()
    print"<LECT_RESU_STREAM> ligne = ",ligne
    if ligne[0] == "/PERIODE" and ligne[2] == "HOULE.................T........:" :
      periode_houle = float(ligne[len(ligne)-2])

      if INFO == 2 :
        print "<I> <MACR_CALC_EOLIENNE> Periode de houle = ",periode_houle

    if ligne[0] == "/NB" and ligne[2] == "POINTS" :
      Nb_valeur = int(ligne[len(ligne)-1])

      if INFO == 2 :
        print "<I> <MACR_CALC_EOLIENNE> Nb_valeur = ",Nb_valeur

    if ligne[0] == "/NB" and ligne[4] == "TEMPS" :
      Nb_pas_temps = int(ligne[len(ligne)-1])

      if INFO == 2 :
        print "<I> <MACR_CALC_EOLIENNE> Nb_pas_temps = ",Nb_pas_temps
        break

  return(periode_houle,Nb_valeur,Nb_pas_temps,lignes)

#____________________________________________________________________
#__________________ FIN FONCTION lect_resu_stream ___________________
#
#____________________________________________________________________
#
#____________________________________________________________________
#_______________ DEBUT FONCTION donn_boucle_pas_temps _______________
#
#
# ===>   Preparation donnees necessaires a la boucle sur les pas de temps
#        et verification du nombre de pas de temps
#____________________________________________________________________
#

def donn_boucle_pas_temps(lignes,Nb_pas_temps) :

# 1 ==> Construction du dictionnaire contenant :
#       pas_de_temps : numeros de ligne du fichier resu_STREAM_FM

  compt_temps = 0
  dict_temps = {}

  for i in range(len(lignes)) :
    ligne = lignes[i].split()
    if len(ligne) != 0 :
      if ligne[0] == "#TITRE" and ligne[1] == "TEMPS" :
        compt_temps = compt_temps + 1
        dict_temps["TEMPS_"+str(compt_temps)] = i+1

# 2 ==> Controle de l'egalite : compt_temps == Nb_pas_temps+1
#       et arret en cas d'erreur.

  if compt_temps != (Nb_pas_temps+1) :
    print"\n <F> <DONN_BOUCLE_PAS_TEMPS> IL Y A UN PROBLEME DANS DE NOMBRE DE PAS DE TEMPS.\n"

  return(dict_temps)
#
#____________________________________________________________________
#________________ FIN FONCTION donn_boucle_pas_temps ________________
#
#____________________________________________________________________
#
#____________________________________________________________________
#__________________ DEBUT FONCTION extr_char_houle __________________
#
#
# ===>   Extraction  des couples (profondeur,force lineique)
#        du fichier resu_STREAM_FM au pas de temps courant et
#        remplissage de la liste de travail.
#____________________________________________________________________
#
#####################
#    VALEUR TEST
#
test = 'NON'
#  Nb_pas_temps = 0
#####################   
  
def extr_char_houle(k,dict_temps,Nb_valeur,lignes) :

  liste = Numeric.zeros((Nb_valeur,2), Numeric.Float)

  nume_ligne_temps = dict_temps["TEMPS_"+str(k)]
  range_depart = int(nume_ligne_temps) + 2
  range_arrive = int(nume_ligne_temps) + Nb_valeur + 3
  for l in range(range_depart, range_arrive) :
    ligne = lignes[l].split()
    if (len(ligne) != 0) :
      Flag_Val=re.match(r'^[-]?([0-9]+\.?[0-9]*|\.[0-9]+)([eE][+-]?[0-9]+)?$',ligne[0])
    if (Flag_Val != None) :
      liste[[(l-range_depart),0]] = float(ligne[0])
      liste[[(l-range_depart),1]] = float(ligne[1])

  return(range_depart,range_arrive,liste)

#____________________________________________________________________
#___________________ FIN FONCTION extr_char_houle ___________________
#
#                    _____________________________
#                           _______________       
#                    _____________________________
#
#____________________________________________________________________
#__________________ DEBUT MACRO macr_calc_eolienne __________________
#
#
# ===>   MACRO COMMANDE MACR_CALC_EOLIENNE
#
#____________________________________________________________________
#
# SCRIPT PYTHON
#
def macr_calc_eolienne_ops(self, INFO, MONOPODE, EXEC_MAILLAGE, AFFE_MATERIAU, CHARGEMENT,
                                 IMPRESSION, MODELISATION, NOM_MAIL_REF, TYPE_ELEM, FATIGUE, **args) :

  """ Calcule la structure de l'eolienne en mer.  """

# On charge les modules nécessaires
  from Accas import _F
  import aster
#  import os

#____________________________________________________________________
#
# 1. Préalables
#____________________________________________________________________
#
# 1.1 ==> La macro compte pour 1 dans l'exécution des commandes

  self.set_icmd(1)
  erreur = 0

# 1.2 ==> On importe les définitions des commandes Aster utilisées
#         dans la macro

  affemateriau = AFFE_MATERIAU
#  impression = IMPRESSION
  PRE_GIBI = self.get_cmd("PRE_GIBI")
  EXEC_LOGICIEL = self.get_cmd("EXEC_LOGICIEL")
  LIRE_MAILLAGE = self.get_cmd("LIRE_MAILLAGE")
  DEFI_GROUP = self.get_cmd("DEFI_GROUP")
  CREA_MAILLAGE = self.get_cmd("CREA_MAILLAGE")
  MODI_MAILLAGE = self.get_cmd("MODI_MAILLAGE")
  AFFE_CARA_ELEM = self.get_cmd("AFFE_CARA_ELEM")
  AFFE_MODELE = self.get_cmd("AFFE_MODELE")
  AFFE_MATERIAU = self.get_cmd("AFFE_MATERIAU")
  FORMULE = self.get_cmd("FORMULE")
  AFFE_CHAR_MECA_F= self.get_cmd("AFFE_CHAR_MECA_F")
  AFFE_CHAR_MECA= self.get_cmd("AFFE_CHAR_MECA")
  DEFI_LIST_REEL= self.get_cmd("DEFI_LIST_REEL")
  STAT_NON_LINE= self.get_cmd("STAT_NON_LINE")
  MECA_STATIQUE= self.get_cmd("MECA_STATIQUE")
  CREA_CHAMP= self.get_cmd("CREA_CHAMP")
  PROJ_CHAMP= self.get_cmd("PROJ_CHAMP")
  CREA_RESU= self.get_cmd("CREA_RESU")
  CALC_ELEM= self.get_cmd("CALC_ELEM")
#
#  IMPR_CO= self.get_cmd("IMPR_CO")
#
  IMPR_RESU= self.get_cmd("IMPR_RESU")
  CALC_NO= self.get_cmd("CALC_NO")
  CALC_FATIGUE= self.get_cmd("CALC_FATIGUE")
  DETRUIRE= self.get_cmd("DETRUIRE")
  DEFI_FICHIER= self.get_cmd("DEFI_FICHIER")

# 1.3 ==> Lecture des donnees

  H_TOTALE        = MONOPODE['H_TOTALE']
  H_BASE          = MONOPODE['H_BASE']
  H_MOYEU         = MONOPODE['H_MOYEU']
  DECAL_PALES     = MONOPODE['DECAL_PALES']
  DEXT_HAUT_BASE  = MONOPODE['DEXT_HAUT_BASE']
  DEXT_BAS_BASE   = MONOPODE['DEXT_BAS_BASE']
  EPAIS_HAUT_BASE = MONOPODE['EPAIS_HAUT_BASE']
  EPAIS_BAS_BASE  = MONOPODE['EPAIS_BAS_BASE']
  DEXT_HAUT_FUT   = MONOPODE['DEXT_HAUT_FUT']
  DEXT_BAS_FUT    = MONOPODE['DEXT_BAS_FUT']
  EPAIS_HAUT_FUT  = MONOPODE['EPAIS_HAUT_FUT']
  EPAIS_BAS_FUT   = MONOPODE['EPAIS_BAS_FUT']
  NB_ELEM_BASE    = MONOPODE['NB_ELEM_BASE']
  NB_ELEM_FUT     = MONOPODE['NB_ELEM_FUT']
  if (MODELISATION == '3D' or MODELISATION == 'COQUE') :
     H_JONCTION = MONOPODE['H_JONCTION']
     DEXT_NACELLE  = MONOPODE['DEXT_NACELLE']
     EPAIS_NACELLE = MONOPODE['EPAIS_NACELLE']
     NBEL_EPAIS_BASE = MONOPODE['NBEL_EPAIS_BASE']
     NBEL_EPAIS_FUT  = MONOPODE['NBEL_EPAIS_FUT']
  else :
     H_JONCTION      = None
     DEXT_NACELLE    = None
     EPAIS_NACELLE   = None
     NBEL_EPAIS_BASE = None
     NBEL_EPAIS_FUT  = None
     
  if (MODELISATION == '3D') :
     NBEL_DCIR_BASE  = MONOPODE['NBEL_DCIR_BASE']
     NBEL_DCIR_FUT   = MONOPODE['NBEL_DCIR_FUT']
  else :
     NBEL_DCIR_BASE = None
     NBEL_DCIR_FUT  = None

  ANGLE_VENT_AXE_X = CHARGEMENT['ANGLE_VENT_AXE_X']

  LOGICIEL   = EXEC_MAILLAGE['LOGICIEL']
  UNITE_DATG = EXEC_MAILLAGE['UNITE_DATG']
  UNITE_MGIB = EXEC_MAILLAGE['UNITE_MGIB']
  NIVE_GIBI  = EXEC_MAILLAGE['NIVE_GIBI']

# 1.4 ==> Creation du repertoire pour l'execution du logiciel GIBI

  erreur_partiel = [0]
  Rep_Calc_ASTER = os.getcwd()

  Nom_Rep_local_mail = "tmp_dgib"
  Rep_Calc_LOGICIEL_local_mail = os.path.join(".",Nom_Rep_local_mail)
  Rep_Calc_LOGICIEL_global_mail = os.path.join(Rep_Calc_ASTER,Nom_Rep_local_mail)

  try :
    os.mkdir(Rep_Calc_LOGICIEL_global_mail)
  except os.error,erreur_partiel :
    self.cr.warn("Code d'erreur de mkdir : " + str(erreur_partiel[0]) + " : " + erreur_partiel[1])
    self.cr.fatal("Impossible de creer le repertoire de travail pour le logiciel de maillage : "+Rep_Calc_LOGICIEL_global_mail)

  aux_datg = 'fort.'+str(UNITE_DATG)
  fichier_datg = os.path.join(Rep_Calc_LOGICIEL_global_mail,aux_datg)

  aux_mgib = 'fort.'+str(UNITE_MGIB)
  fichier_mgib = os.path.join(Rep_Calc_ASTER,aux_mgib)

  repertoire_outils = aster.repout()
  if (LOGICIEL == 'GIBI2000') : logi_mail = repertoire_outils+'gibi2000'
  if (LOGICIEL == 'GIBI98') : logi_mail = repertoire_outils+'gibi98'

# 1.5 ==> Construction du maillage et du modele de reference

  if (MODELISATION == '3D') and (FATIGUE != None) :

    H_IMMERGEE = MONOPODE['H_BASE']
  
    mail_eolienne(H_TOTALE, H_BASE, H_IMMERGEE, H_MOYEU, H_JONCTION, DECAL_PALES,
                  DEXT_NACELLE, EPAIS_NACELLE, DEXT_HAUT_BASE, DEXT_BAS_BASE,
                  EPAIS_HAUT_BASE, EPAIS_BAS_BASE, DEXT_HAUT_FUT, DEXT_BAS_FUT,
                  EPAIS_HAUT_FUT, EPAIS_BAS_FUT, NB_ELEM_BASE, NB_ELEM_FUT,
                  NBEL_EPAIS_BASE, NBEL_EPAIS_FUT, NBEL_DCIR_BASE, NBEL_DCIR_FUT,
                  ANGLE_VENT_AXE_X,
                  MODELISATION, TYPE_ELEM, NIVE_GIBI, fichier_datg)

    EXEC_LOGICIEL(
                  LOGICIEL=logi_mail,
                  ARGUMENT=(fichier_datg,   # fichier de donnees GIBI
                            fichier_mgib,   # fichier resultats format GIBI
                           )
                 )

    PRE_GIBI()
    
    if NOM_MAIL_REF!=None : self.DeclareOut('mail_ref',NOM_MAIL_REF)
    mail_ref = LIRE_MAILLAGE()    

# debut test
#       IMPR_CO(CO=mail_ref,)
# fin test

    MOREF = AFFE_MODELE( MAILLAGE=mail_ref,
                         AFFE=(_F(GROUP_MA     = ('MONOPODE'),
                                  PHENOMENE    = 'MECANIQUE',
                                  MODELISATION = '3D',),),);

# 1.6 ==> Lecture du fichier de resultats STREAM_FM

  (periode_houle,Nb_valeur,Nb_pas_temps,lignes) = lect_resu_stream(INFO)

# 1.7 ==> declaration de tableaux pour les concepts Aster

  if (MODELISATION == 'COQUE') :
    MA2      = [None] * (Nb_pas_temps+1) # pour la modelisation COQUE

  MA       = [None] * (Nb_pas_temps+1)   # pour les modelisations POUTRE et 3D
  __MO     = [None] * (Nb_pas_temps+1)
  CARA     = [None] * (Nb_pas_temps+1)
  __affmat = [None] * (Nb_pas_temps+1)  

# 1.8 ==> Construction du dictionnaire contenant :
#         pas_de_temps : numeros de ligne du fichier resu_STREAM_FM

  dict_temps = donn_boucle_pas_temps(lignes,Nb_pas_temps)

#____________________________________________________________________
#
#####################
#    VALEUR TEST
#
  test = 'NON'
#  Nb_pas_temps = 0
#####################   
#____________________________________________________________________
#
# 2. ==> BOUCLE SUR LES PAS DE TEMPS
#        Dans cette boucle, a chaque pas de temps nous produisons :
#        1. un maillage en fonction de la hauteur d'eau ;
#        2. un chargement correspondant a la hauteur d'eau et a la modelisation ;
#        3. un calcul elastique lineaire avec MECA_STATIQUE ;
#        4. un fichier resultat qui est stocke dans un repertoire .repe ;
#        5. dans le cas ou un calcul en fatigue est demande nous projetons
#           le champ de contrainte calcule aux noeuds sur un maillage de reference.
#
  
  for j in range(Nb_pas_temps+1) :

    k = j + 1

# 2.1 ==> Extraction  des couples (profondeur,force lineique)
#         du fichier resu_STREAM_FM au pas de temps courant et
#         remplissage de la liste de travail.

    (range_depart,range_arrive,liste) = extr_char_houle(k,dict_temps,Nb_valeur,lignes)

# 2.2 ==> Extraction de la hauteur d'eau

    H_IMMERGEE = liste[[(range_arrive-range_depart-2),0]]
    if INFO == 2 :
      print "<I> <MACR_CALC_EOLIENNE> ==> Numero d'ordre = ", j    
      print "<I> <MACR_CALC_EOLIENNE> ==> hauteur_eau = ", H_IMMERGEE    

#####################
#    VALEUR TEST
#
#    hauteur_eau = 30.
#####################   
    print "\n<CALC_EOLIENNE>  liste = \n", liste          
    print"   "     

# 2.3 ==> Calcul des coefficients de la regression de maniere a
#         construire un chargement continu.

    (teta0,teta1,teta2,teta3,alpha1,alpha2,alpha3,beta2,beta3) = eolien_regression (liste, INFO)

    print "  "
    print "<I> <MACR_CALC_EOLIENNE>   RESULTATS  "
    print "  "
    print "<I> <MACR_CALC_EOLIENNE> teta0 = ", teta0
    print "<I> <MACR_CALC_EOLIENNE> teta1 = ", teta1
    print "<I> <MACR_CALC_EOLIENNE> teta2 = ", teta2
    print "<I> <MACR_CALC_EOLIENNE> teta3 = ", teta3
    print "  "

# 2.4 ==> Creation du maillage pour la hauteur d'eau (H_IMMERGEE) courante

    mail_eolienne(H_TOTALE, H_BASE, H_IMMERGEE, H_MOYEU, H_JONCTION, DECAL_PALES,
                  DEXT_NACELLE, EPAIS_NACELLE, DEXT_HAUT_BASE, DEXT_BAS_BASE,
                  EPAIS_HAUT_BASE, EPAIS_BAS_BASE, DEXT_HAUT_FUT, DEXT_BAS_FUT,
                  EPAIS_HAUT_FUT, EPAIS_BAS_FUT, NB_ELEM_BASE, NB_ELEM_FUT,
                  NBEL_EPAIS_BASE, NBEL_EPAIS_FUT, NBEL_DCIR_BASE, NBEL_DCIR_FUT,
                  ANGLE_VENT_AXE_X,
                  MODELISATION, TYPE_ELEM, NIVE_GIBI, fichier_datg)

# 2.5 ==> Lancement de GIBI
    EXEC_LOGICIEL(
                   LOGICIEL = logi_mail,
                   ARGUMENT = (fichier_datg,    # fichier de donnees GIBI
                               fichier_mgib,    # fichier resultats format GIBI
                              )
                 )

# 2.6 ==> Lecture du maillage et definition des groupes

    PRE_GIBI()

    MA[j] = LIRE_MAILLAGE()

    MA[j]=DEFI_GROUP( reuse         = MA[j],
                      MAILLAGE      = MA[j],
                      CREA_GROUP_NO = _F( TOUT_GROUP_MA='OUI',),);

# debut test
#  IMPR_CO(CO=MA[j],)
# fin test

# 2.7 ==> Modelisation POUTRE

    if MODELISATION == 'POUTRE' :

      __MO[j]=AFFE_MODELE(MAILLAGE=MA[j],
                 AFFE=_F(GROUP_MA='EOLIENNE',PHENOMENE='MECANIQUE',MODELISATION='POU_D_T',),);

      motscle={}
      motscle['POUTRE']=[]
      for i in range(NB_ELEM_FUT) :
        nom  = 'FUT'+str(i+1)
        incr = (DEXT_BAS_FUT - DEXT_HAUT_FUT)/(2.0*NB_ELEM_FUT)
        r1 = (DEXT_BAS_FUT/2.0) - (i*incr)
        r2 = r1 - incr     
        motscle['POUTRE'].append(_F(GROUP_MA=nom,SECTION='CERCLE',CARA=('R1','R2','EP1','EP2',),VALE=(r1,r2,EPAIS_BAS_FUT,EPAIS_HAUT_FUT,),VARI_SECT='HOMOTHETIQUE',), )

      for i in range(NB_ELEM_BASE) :
        nom = 'BASE'+str(i+1)
        incr = (DEXT_BAS_BASE - DEXT_HAUT_BASE)/(2.0*NB_ELEM_BASE)
        r1 = (DEXT_BAS_BASE/2.0) - (i*incr)        
        r2 = r1 - incr     
        motscle['POUTRE'].append(_F(GROUP_MA=nom,SECTION='CERCLE',CARA=('R1','R2','EP1','EP2',),VALE=(r1,r2,EPAIS_BAS_BASE,EPAIS_HAUT_BASE,),VARI_SECT='HOMOTHETIQUE',), )

      CARA[j]=AFFE_CARA_ELEM( MODELE = __MO[j], 
                              **motscle );

# 2.8 ==> Modelisation COQUE
       
    if MODELISATION == 'COQUE' :
 
      MA2[j] = CREA_MAILLAGE( MAILLAGE=MA[j],
                              MODI_MAILLE=( _F( OPTION   = 'QUAD8_9',
                                                GROUP_MA = ('PARTEM','PARTIM',),),
                                            _F( OPTION   = 'TRIA6_7',
                                                GROUP_MA = ('JONCTION',),),),
                            );

      __MO[j] = AFFE_MODELE( MAILLAGE=MA2[j],
                             AFFE=( _F( GROUP_MA     = ('PARTEM','PARTIM','JONCTION','CHAUTE'),
                                        PHENOMENE    = 'MECANIQUE',
                                        MODELISATION = 'COQUE_3D',),
                                    _F( GROUP_MA     = 'NACELLE',
                                        PHENOMENE    = 'MECANIQUE',
                                        MODELISATION = 'POU_D_E',),),
                            );
  
      
      MA2[j] = MODI_MAILLAGE( reuse         = MA2[j],
                              MAILLAGE      = MA2[j],
                              ORIE_NORM_COQUE = _F(
                              GROUP_MA = ('PARTEM','PARTIM','JONCTION'),),);
 
      CARA[j] = AFFE_CARA_ELEM( MODELE=__MO[j],
                                POUTRE=( _F(GROUP_MA = 'NACELLE',
                                            SECTION  = 'CERCLE',
                                            CARA     = ('R','EP',),
                                            VALE     = (dex_nacelle/2.,ep_nacelle,), ),
                                     ),
                                COQUE=( _F( GROUP_MA = ('PARTEM','PARTIM','CHAUTE'),
                                            EPAIS    = ep_b_fut,
                                            ANGL_REP = (0.0,90.0,), ),

                                        _F( GROUP_MA = ('JONCTION'),
                                            EPAIS    =  h_jonction, ),
                                      ),
                               );

# 2.9 ==> Modelisation 3D

    if MODELISATION == '3D' :
    
      __MO[j] = AFFE_MODELE( MAILLAGE=MA[j],
                             AFFE=(_F( GROUP_MA     = ('BASE','FUT','JONCTION','CHAUTE','SI1','SE1'),
                                       PHENOMENE    = 'MECANIQUE',
                                       MODELISATION = '3D',),
                                   _F( GROUP_MA     ='NACELLE',
                                       PHENOMENE    ='MECANIQUE',
                                       MODELISATION ='POU_D_E',),),
                           );
   
      MA[j] = MODI_MAILLAGE( reuse        = MA[j],
                             MAILLAGE     = MA[j],
                             ORIE_PEAU_3D = _F(
                             GROUP_MA     = ('SE1','SI1','CHAUTE'),),
                           );

      CARA[j] = AFFE_CARA_ELEM( MODELE =__MO[j],
                                POUTRE =(
                                         _F( GROUP_MA = 'NACELLE',
                                             SECTION  = 'CERCLE',
                                             CARA     = ('R','EP',),
                                             VALE     = (DEXT_NACELLE/2.0,EPAIS_NACELLE,), ),
                                        ),
                              );

# 2.10 ==> Affectation du materiau

    motscles={}
    motscles['AFFE_MATERIAU']=[]
    for mat in affemateriau :
      if mat['TOUT'] == None :
        # Creation de mots-cles pour les AFFE_CHAR_MECA
        motscles['AFFE_MATERIAU'].append(_F( GROUP_MA = mat['GROUP_MA'],
                                             MATER    = mat['MATER'],
                                             TEMP_REF = mat['TEMP_REF'],) )
      else :
        # Creation de mots-cles pour les AFFE_CHAR_MECA
        motscles['AFFE_MATERIAU'].append(_F( TOUT     = 'OUI',
                                             MATER    = mat['MATER'],
                                             TEMP_REF = mat['TEMP_REF'],) )
    if MODELISATION == 'COQUE' :
      __affmat[j] = AFFE_MATERIAU( MAILLAGE = MA2[j],
                                   MODELE   = __MO[j],
                                   AFFE     = motscles['AFFE_MATERIAU'],
                                 )
    else :
      __affmat[j] = AFFE_MATERIAU( MAILLAGE = MA[j],
                                   MODELE   = __MO[j],
                                   AFFE     = motscles['AFFE_MATERIAU'],
                                 )

# 2.11 ==> Chargement et conditions aux limites

    RAUMER = 1027.42     # masse volumique de l'eau de mer (kg/m3)
    l_elem_stream = 0.3  # longueur des elements de STREAM_FM (m)
  
    if test == 'OUI':
      # FC3 poutre           
      if MODELISATION == 'POUTRE' :
        FC3 = FORMULE(NOM_PARA=('X','Y','Z'),VALE=' -(exp((Z+231.21)/20.187))/0.3')
      else :
      # FC3 coque et 3D
        FC3 = FORMULE(NOM_PARA=('X','Y','Z'),VALE='''- (
          (3.E-5-(1.E4*Z))+((-6.E-8*(atan(Y/(X+1.E-8)))*(atan(Y/(X+1.E-8)))*(atan(Y/(X+1.E-8)))*(atan(Y/(X+1.E-8)))+2.E-5*(atan(Y/(X+1.E-8)))*(atan(Y/(X+1.E-8)))*(atan(Y/(X+1.E-8)))
          -0.0021*(atan(Y/(X+1.E-8)))*(atan(Y/(X+1.E-8)))+0.0224*(atan(Y/(X+1.E-8)))+0.9328)*
          (0.5*'''+str(RAUMER)+'''*1.6**2)))''')
    else :
      if MODELISATION == 'POUTRE' :   
        FC3 = FORMULE(NOM_PARA=('X','Y','Z'),VALE=''' -('''
           +str(teta0)+'''+('''
           +str(teta1)+''')*(Z-('''+str(alpha1)+'''))+('''
           +str(teta2)+''')*((Z-('''+str(alpha2)+'''))*(Z-('''+str(alpha1)+'''))-('''+str(beta2)+'''))+('''
           +str(teta3)+''')*( (Z-('''+str(alpha3)+'''))*((Z-('''+str(alpha2)+'''))*(Z-('''+str(alpha1)+'''))-('''
                           +str(beta2)+'''))-('''+str(beta3)+''')*(Z-('''+str(alpha1)+'''))))/'''+str(l_elem_stream))
      else :
        r_bas = (DEXT_HAUT_BASE + DEXT_BAS_FUT)/4.0   # rayon moyen de la base
        deux_pi_r_bas = 2.0*math.pi*r_bas             # rayon moyen de la base multiplie par 2*pi
        int_dp_etoil = -4.83893                       # valeur de l integrale de la fonction de repartition de pression

        FC3 = FORMULE(NOM_PARA=('X','Y','Z'),VALE=''' -(
           (('''+str(H_BASE)+'''-Z)*1.0E+4)+(
           ((('''+str(teta0)+'''+'''
           +str(teta1)+'''*(Z-'''+str(alpha1)+''')+'''
           +str(teta2)+'''*((Z-'''+str(alpha2)+''')*(Z-'''+str(alpha1)+''')-'''+str(beta2)+''')+'''
           +str(teta3)+'''*( (Z-'''+str(alpha3)+''')*((Z-'''+str(alpha2)+''')*(Z-'''+str(alpha1)+''')-'''
                           +str(beta2)+''')-'''+str(beta3)+'''*(Z-'''+str(alpha1)+''')))/'''+str(l_elem_stream)+''')-'''
          +str(deux_pi_r_bas)+'''*(('''+str(H_BASE)+'''-Z)*1.0E+4))*
          ((-6.E-8*(atan(Y/(X+1.E-8)))*(atan(Y/(X+1.E-8)))*(atan(Y/(X+1.E-8)))*(atan(Y/(X+1.E-8)))+2.E-5*(atan(Y/(X+1.E-8)))*(atan(Y/(X+1.E-8)))*(atan(Y/(X+1.E-8)))
          -0.0021*(atan(Y/(X+1.E-8)))*(atan(Y/(X+1.E-8)))+0.0224*(atan(Y/(X+1.E-8)))+0.9328))/('''+str(r_bas)+'''*'''+str(int_dp_etoil)+''')))''')     
          
    if CHARGEMENT['VENT'] == 'OUI' :
      FV0 = CHARGEMENT['FORCE_VENT_FUT']
    else :
      FV0 = FORMULE(NOM_PARA=('X','Y','Z'),VALE='0.0*Z')

    if MODELISATION == 'POUTRE' :
      CH=AFFE_CHAR_MECA_F(MODELE=__MO[j],
                      FORCE_POUTRE=(_F(GROUP_MA='PARTIM',FX=FC3,),
                                    _F(GROUP_MA='PARTEM',FX=FV0,),),)
    if MODELISATION == 'COQUE' :
      CH=AFFE_CHAR_MECA_F(MODELE=__MO[j],
                      FORCE_COQUE=(_F(GROUP_MA='SI1',PRES=FC3,),
                                   _F(GROUP_MA='SE1',PRES=FV0,),),);

    if MODELISATION == '3D' :
      CH=AFFE_CHAR_MECA_F(MODELE=__MO[j],
                      PRES_REP=(_F(GROUP_MA='SI1',PRES=FC3,),
                                _F(GROUP_MA='SE1',PRES=FV0,),),);

    # force due au poids du rotor decale de d1 (N)
    F2 = CHARGEMENT['FORCE_POIDS_ROTOR']

    # force due au poids de la nacelle (N)
    F3 = CHARGEMENT['FORCE_POIDS_NACELLE']

    # force totale en z sur le sommet
    FCZ = F2+F3

    # force du à l'effort du vent sur les pales
    FRU = CHARGEMENT['FORCE_VENT_PALES']
    # moment resistant
    MRU = CHARGEMENT['MOMENT_RESISTANT']

    # force et moment dus au vent sur le rotor suivant le repère global
    ALPHAR = CHARGEMENT['ANGLE_VENT_AXE_X']*(math.pi)/180.0

    F1X = FRU*math.cos(ALPHAR)
    F1Y = FRU*math.sin(ALPHAR)
    M1X = MRU*math.cos(ALPHAR)
    M1Y = MRU*math.sin(ALPHAR)

    MCY = M1Y-(MONOPODE['DECAL_PALES']*F2)+(MONOPODE['H_MOYEU']*FRU)

    if MODELISATION == 'POUTRE' :
      LIMIT=AFFE_CHAR_MECA( MODELE       = __MO[j],
                            DDL_IMPO     = _F( GROUP_NO='B_BASE',
                                               DX=0.0,DY=0.0,DZ=0.0,
                                               DRX=0.0,DRY=0.0,DRZ=0.0,),
                            FORCE_NODALE = _F( GROUP_NO='H_FUT',
                                               FX=F1X,
                                               FY=F1Y,
                                               FZ=FCZ,
                                               MX=M1X,
                                               MY=MCY,
                                               MZ=0.0,),)

    if MODELISATION == 'COQUE' :
      LIMIT=AFFE_CHAR_MECA(MODELE=__MO[j],
                         DDL_IMPO=_F(GROUP_NO='CBASI',
                                     DX=0.0,DY=0.0,DZ=0.0,
                                     DRX=0.0,DRY=0.0,DRZ=0.0,),

                        LIAISON_ELEM=_F(OPTION='COQ_POU',
                                        CARA_ELEM = CARA[j],
                                        AXE_POUTRE=(0.,0.,1.,),
                                        GROUP_MA_1='CHAUTE',
                                        GROUP_NO_2='H_FUT',),

                         FORCE_NODALE=(_F(GROUP_NO='CHP',
                                         FX=F1X,FY=F1Y,FZ=F2,
                                         MX=M1X,MY=M1Y,MZ=0.0,),

                                      _F(GROUP_NO='CHP0',FZ=F3,),
                                        ),)
  
    if MODELISATION == '3D' :
      LIMIT=AFFE_CHAR_MECA(MODELE=__MO[j],
                         DDL_IMPO=_F(GROUP_NO='CBASI',
                                     DX=0.0,DY=0.0,DZ=0.0,),

                        LIAISON_ELEM=_F(OPTION='3D_POU',
                                        CARA_ELEM = CARA[j],
                                        AXE_POUTRE=(0.,0.,1.,),
                                        GROUP_MA_1='CHAUTE',
                                        GROUP_NO_2='H_FUT',),

                         FORCE_NODALE=(_F(GROUP_NO='CHP',
                                         FX=F1X,FY=F1Y,FZ=F2,
                                         MX=M1X,MY=M1Y,MZ=0.0,),

                                      _F(GROUP_NO='CHP0',FZ=F3,),
                                        ),)

    POIDS=AFFE_CHAR_MECA( MODELE=__MO[j],
                          PESANTEUR=(9.81,0.0,0.0,-1.0,),)

# 2.12 ==> Realisation du calcul

    if MODELISATION == 'POUTRE' :

      TEMPS=DEFI_LIST_REEL(DEBUT=0.0,
                         INTERVALLE=_F(JUSQU_A=1.0,
                                       PAS=1.0,),)

      RESU=STAT_NON_LINE(MODELE=__MO[j],
                       CHAM_MATER=__affmat[j],
                       CARA_ELEM=CARA[j],
                       EXCIT=(_F(CHARGE=POIDS,),
                              _F(CHARGE=CH,),
                              _F(CHARGE=LIMIT,),),
                       COMP_INCR=_F(RELATION='ELAS',
                                    GROUP_MA='EOLIENNE',),
                       INCREMENT=_F(LIST_INST=TEMPS,
                                    NUME_INST_FIN=1,),)

      RESU=CALC_ELEM( reuse =RESU,
                      RESULTAT=RESU,
                      OPTION=('SIEF_ELNO_ELGA','SIGM_ELNO_SIEF','SIPO_ELNO_SIEF',),)

    if MODELISATION == 'COQUE' :
      RESU=MECA_STATIQUE( MODELE=__MO[j],
                          CHAM_MATER=__affmat[j],
                          CARA_ELEM=CARA[j],
                          EXCIT=(_F(CHARGE=POIDS,),
                                 _F(CHARGE=CH,),
                                 _F(CHARGE=LIMIT,),),
                          NIVE_COUCHE='MOY',
                        );
      RESU=CALC_ELEM( reuse =RESU,
                      RESULTAT=RESU,
                      OPTION=('SIGM_ELNO_DEPL','EQUI_ELNO_SIGM',),)

      RESU = CALC_NO( reuse =RESU,
                      RESULTAT=RESU,
                      OPTION=('SIGM_NOEU_DEPL','EQUI_NOEU_SIGM',),);        

    if MODELISATION == '3D' :
      RESU=MECA_STATIQUE( MODELE=__MO[j],
                          CHAM_MATER=__affmat[j],
                          CARA_ELEM=CARA[j],
                          EXCIT=(_F(CHARGE=POIDS,),
                                 _F(CHARGE=CH,),
                                 _F(CHARGE=LIMIT,),),
                        );
      RESU=CALC_ELEM( reuse =RESU,
                      RESULTAT=RESU,
                      REPE_COQUE=_F(NIVE_COUCHE='MOY',),
                      OPTION=('SIGM_ELNO_DEPL','EQUI_ELNO_SIGM',));

      RESU = CALC_NO( reuse =RESU,
                      RESULTAT=RESU,
                      OPTION=('SIGM_NOEU_DEPL','EQUI_NOEU_SIGM',),);         

# 2.13 ==> Preparation du modele de reference si modelisation 3D calcul de la fatigue

    delta = periode_houle/Nb_pas_temps
    inst = -periode_houle/2.0 + j*delta
    if (MODELISATION == '3D') and (FATIGUE != None) :
  
      if (k == 1) :
        CHAMREF = AFFE_MATERIAU( MAILLAGE = mail_ref,
                                 MODELE   = MOREF,
                                 AFFE     = motscles['AFFE_MATERIAU'],)  
  
      RESPRO = PROJ_CHAMP( METHODE  = 'ELEM',
                           NOM_CHAM = 'SIGM_NOEU_DEPL',
                           RESULTAT = RESU,
                           MODELE_1 = __MO[j],
                           MODELE_2 = MOREF,
                           VIS_A_VIS=(
                                      _F( GROUP_MA_2='MONOPODE',
                                          GROUP_MA_1='MONOPODE' ),
                                     ),                  
                         )

      SIG_PRO = CREA_CHAMP( TYPE_CHAM  = 'NOEU_SIEF_R',
                            OPERATION  = 'EXTR',
                            RESULTAT   =  RESPRO  ,
                            NOM_CHAM   = 'SIGM_NOEU_DEPL',
                            NUME_ORDRE = 1,
                          )

      if (k==1) :
        RESREF = CREA_RESU( 
                            OPERATION = 'AFFE',
                            TYPE_RESU = 'EVOL_ELAS',
                            NOM_CHAM  = 'SIGM_NOEU_DEPL',
                            AFFE      = _F( CHAM_GD = SIG_PRO,
                                            INST = (inst), ), 
                          )
      else :
        RESREF = CREA_RESU( reuse = RESREF,
                            OPERATION = 'AFFE',
                            TYPE_RESU = 'EVOL_ELAS',
                            NOM_CHAM  = 'SIGM_NOEU_DEPL',
                            AFFE      = _F( CHAM_GD = SIG_PRO,
                                            INST = (inst), ),
                          )

# 2.14 ==> Impression des resultats

    if MODELISATION == 'POUTRE' :
      fich1='poutre_t='+str(inst)+'.resu'
      fich2='poutre_t='+str(inst)+'.cast'
      fich3='poutre_t='+str(inst)+'.ensi'
      fich4='poutre_t='+str(inst)+'.unv'
    if MODELISATION == 'COQUE' :
      fich1='coque_t='+str(inst)+'.resu'
      fich2='coque_t='+str(inst)+'.cast'
      fich3='coque_t='+str(inst)+'.ensi'
      fich4='coque_t='+str(inst)+'.unv'
    if MODELISATION == '3D' :
      fich1='3D_t='+str(inst)+'.resu'
      fich2='3D_t='+str(inst)+'.cast'
      fich3='3D_t='+str(inst)+'.ensi'
      fich4='3D_t='+str(inst)+'.unv'

    fich1b = './REPE_OUT/'+fich1
    fich2b = './REPE_OUT/'+fich2
    fich3b = './REPE_OUT/'+fich3
    fich4b = './REPE_OUT/'+fich4

    if IMPRESSION != None :
      motscles={}
      motscles['IMPRESSION']=[]
      # Creation de mots-cles pour les IMPR_RESU
      for impr in IMPRESSION :
        if impr['FORMAT']=='RESULTAT':
          UNIT_1B=DEFI_FICHIER(FICHIER=fich1b)
          unitr = UNIT_1B
        if impr['FORMAT']=='CASTEM':
          UNIT_2B=DEFI_FICHIER(FICHIER=fich2b)
          unitr = UNIT_2B
        if impr['FORMAT']=='ENSIGHT':
          UNIT_3B=DEFI_FICHIER(FICHIER=fich3b)
          unitr = UNIT_3B
        if impr['FORMAT']=='IDEAS':
          UNIT_4B=DEFI_FICHIER(FICHIER=fich4b)
          unitr = UNIT_4B
        if MODELISATION == '3D':    
             motscles['IMPRESSION'].append(_F(MAILLAGE=MA[j],RESULTAT=RESU,
                                             NOM_CHAM=('DEPL','SIGM_ELNO_DEPL','EQUI_ELNO_SIGM',),) )
        if MODELISATION == 'COQUE':      
             motscles['IMPRESSION'].append(_F(MAILLAGE=MA2[j],RESULTAT=RESU,
                                           NOM_CHAM=('DEPL','SIGM_ELNO_DEPL','EQUI_ELNO_SIGM',),) )
        if MODELISATION == 'POUTRE':     
             motscles['IMPRESSION'].append(_F(MAILLAGE=MA[j],RESULTAT=RESU,
                                           NOM_CHAM=('DEPL','SIGM_ELNO_SIEF','SIPO_ELNO_SIEF',),) )

      IMPR_RESU(FORMAT=impr['FORMAT'],UNITE=unitr,
                RESU=motscles['IMPRESSION'],)

      for impr in IMPRESSION :
        if impr['FORMAT']=='RESULTAT':
          DEFI_FICHIER(ACTION='LIBERER',FICHIER=fich1b)
          DETRUIRE(CONCEPT=_F( NOM = UNIT_1B))
        if impr['FORMAT']=='CASTEM':
          DEFI_FICHIER(ACTION='LIBERER',FICHIER=fich2b)
          DETRUIRE(CONCEPT=_F( NOM = UNIT_2B))
        if impr['FORMAT']=='ENSIGHT':
          DEFI_FICHIER(ACTION='LIBERER',FICHIER=fich3b)
          DETRUIRE(CONCEPT=_F( NOM = UNIT_3B))
        if impr['FORMAT']=='IDEAS':
          DEFI_FICHIER(ACTION='LIBERER',FICHIER=fich4b)
          DETRUIRE(CONCEPT=_F( NOM = UNIT_4B))
      
      if (MODELISATION == '3D') and (FATIGUE != None) :
         DETRUIRE(CONCEPT=_F( NOM = SIG_PRO))
         DETRUIRE(CONCEPT=_F( NOM = RESPRO))
      if MODELISATION == 'POUTRE' :
         DETRUIRE(CONCEPT=_F( NOM = TEMPS))

    DETRUIRE(CONCEPT=_F( NOM = FC3))
    DETRUIRE(CONCEPT=_F( NOM = CH))
    DETRUIRE(CONCEPT=_F( NOM = LIMIT))
    DETRUIRE(CONCEPT=_F( NOM = POIDS))
    DETRUIRE(CONCEPT=_F( NOM = RESU))
#
#____________________________________________________________________
#
# 3.  Calcul de fatigue.
#     On calcule la fatigue si la modelisation est 3D et si le mot clef
#     fatigue est present.
#____________________________________________________________________
#
# 3.1 ==> Calcul de la fatigue
#
  if (MODELISATION == '3D') and  (FATIGUE != None) : 
                   
    self.DeclareOut('CHFATI',self.sd)
    CHFATI = CALC_FATIGUE (
                 TYPE_CALCUL = 'FATIGUE_MULTI',
                 TYPE_CHARGE = 'PERIODIQUE',
                 OPTION = 'DOMA_NOEUD',
                 RESULTAT = RESREF,
                 CHAM_MATER = CHAMREF,
                 GROUP_MA = ('BASE','FUT'),
                 MAILLAGE = mail_ref,
                 CRITERE=FATIGUE['CRITERE'],
                 METHODE='CERCLE_EXACT',
                 INFO = 2, )

#____________________________________________________________________
#
# 3.2 ==> Impression des resultats de fatigue
#
    if IMPRESSION != None :
      motscles={}
      motscles['IMPRESSION']=[]

      motscles['IMPRESSION'].append(_F(MAILLAGE=mail_ref,CHAM_GD=CHFATI,) )

    IMPR_RESU( FORMAT=impr['FORMAT'],
               MODELE   = MOREF,
               RESU=motscles['IMPRESSION'],)

#____________________________________________________________________
#
# 4. C'est fini !
#____________________________________________________________________
#    
  if erreur :
    if not messages_erreur.has_key(erreur) :
      erreur = 100
    self.cr.fatal(messages_erreur[erreur])

  return
#
#____________________________________________________________________
#____________________________________________________________________
#
#                  CATALOGUES DES MACRO-COMMANDES
#____________________________________________________________________
#____________________________________________________________________
#
#              ________________________________________
#
#____________________________________________________________________
#____________________________________________________________________
#
# CATALOGUE DE LA MACRO COMMANDE CALC_CHAR_HOULE
#____________________________________________________________________
#____________________________________________________________________
#

CALC_CHAR_HOULE=MACRO( nom="CALC_CHAR_HOULE",op=calc_char_houle_ops,
                       fr="Calcul le chargement du a la houle.",
                       ang=".",reentrant='n',
                       docu="Ux.xx.xx-a",
 
         STREAM            =FACT(statut='o',max=1,
           PROFONDEUR        =SIMP(statut='o',typ='R'),  
           H_HOULE           =SIMP(statut='o',typ='R'),  
           PERI_HOULE        =SIMP(statut='o',typ='R'), 
           COUR_EULERIEN     =SIMP(statut='o',typ='R'), 
           COEF_TRAINEE      =SIMP(statut='o',typ='R'),  
           COEF_INERTIE      =SIMP(statut='o',typ='R'),  
           ORDR_FONC_COURAN  =SIMP(statut='o',typ='I'),  
           NB_POINTS_VERT    =SIMP(statut='o',typ='I'),  
           NB_INTER_PERI     =SIMP(statut='o',typ='I'),  
           DEXT_HAUT_BASE    =SIMP(statut='o',typ='R'), 
         ),
            
         IMPRESSION      =FACT(statut='f',
         UNITE           =SIMP(statut='o',typ='I'),
         ),
             
         INFO           = SIMP(statut='f',typ='I',defaut=1,into=(1,2)),

);

#
#____________________________________________________________________
#____________________________________________________________________
#
# CATALOGUE DE LA MACRO COMMANDE MACR_CALC_EOLIENNE
#____________________________________________________________________
#____________________________________________________________________
#

def macr_calc_eolienne_prod(self,NOM_MAIL_REF,**args):
  if NOM_MAIL_REF != None : self.type_sdprod(NOM_MAIL_REF,maillage_sdaster)
  return cham_no_sdaster


MACR_CALC_EOLIENNE=MACRO(nom="MACR_CALC_EOLIENNE",op=macr_calc_eolienne_ops,
                   sd_prod=macr_calc_eolienne_prod,
                   fr="Calcul d une eolienne en mer.",
                   ang=".",reentrant='n',
                   docu="U2.09.04-a",
 
         EXEC_MAILLAGE   =FACT(statut='o',
           LOGICIEL        =SIMP(statut='o',typ='TXM',defaut="GIBI2000",into=("GIBI98","GIBI2000") ),
           UNITE_DATG      =SIMP(statut='f',typ='I',defaut=70),  
           UNITE_MGIB      =SIMP(statut='f',typ='I',defaut=19),  
           NIVE_GIBI       =SIMP(statut='f',typ='I',defaut=10,into=(3,4,5,6,7,8,9,10,11)),
         ),

         MODELISATION   =SIMP(statut='o',typ='TXM', into=("POUTRE","COQUE","3D") ),
         NOM_MAIL_REF   =SIMP(statut='f',typ=(CO,maillage_sdaster)),
            
         TYPE_ELEM      =SIMP(statut='f',typ='TXM',defaut="CUB8",into=("CUB8","CU20") ),
            
         b_model_3D  =BLOC(condition = "MODELISATION == '3D'",
   
            MONOPODE =FACT(statut='o',max=1,

               H_TOTALE        =SIMP(statut='o',typ='R' ),  
               H_BASE          =SIMP(statut='o',typ='R' ),  
               H_MOYEU         =SIMP(statut='o',typ='R' ),  
               H_JONCTION      =SIMP(statut='o',typ='R' ),  
               DECAL_PALES     =SIMP(statut='o',typ='R' ),  
               DEXT_NACELLE    =SIMP(statut='o',typ='R' ), 
               EPAIS_NACELLE   =SIMP(statut='o',typ='R' ), 
               DEXT_HAUT_BASE  =SIMP(statut='o',typ='R' ), 
               DEXT_BAS_BASE   =SIMP(statut='f',typ='R' ), 
               EPAIS_HAUT_BASE =SIMP(statut='o',typ='R' ), 
               EPAIS_BAS_BASE  =SIMP(statut='f',typ='R' ), 
               DEXT_HAUT_FUT   =SIMP(statut='o',typ='R' ), 
               DEXT_BAS_FUT    =SIMP(statut='f',typ='R' ), 
               EPAIS_HAUT_FUT  =SIMP(statut='o',typ='R' ), 
               EPAIS_BAS_FUT   =SIMP(statut='f',typ='R' ), 
               NB_ELEM_BASE    =SIMP(statut='f',typ='I',defaut=30),
               NB_ELEM_FUT     =SIMP(statut='f',typ='I',defaut=70),
               NBEL_EPAIS_BASE =SIMP(statut='f',typ='I',defaut=3),
               NBEL_EPAIS_FUT  =SIMP(statut='f',typ='I',defaut=3),
               NBEL_DCIR_BASE  =SIMP(statut='f',typ='I',defaut=15),
               NBEL_DCIR_FUT   =SIMP(statut='f',typ='I',defaut=15),
                          ),    
            ),
                        
         b_model_coque  =BLOC(condition = "MODELISATION == 'COQUE' ",
   
           MONOPODE        =FACT(statut='o',max=1,

              H_TOTALE        =SIMP(statut='o',typ='R' ),  
              H_BASE          =SIMP(statut='o',typ='R' ),  
              H_MOYEU         =SIMP(statut='o',typ='R' ),  
              H_JONCTION      =SIMP(statut='o',typ='R' ),  
              DECAL_PALES     =SIMP(statut='o',typ='R' ),  
              DEXT_NACELLE    =SIMP(statut='o',typ='R' ), 
              EPAIS_NACELLE   =SIMP(statut='o',typ='R' ), 
              DEXT_HAUT_BASE  =SIMP(statut='o',typ='R' ), 
              DEXT_BAS_BASE   =SIMP(statut='f',typ='R' ), 
              EPAIS_HAUT_BASE =SIMP(statut='o',typ='R' ), 
              EPAIS_BAS_BASE  =SIMP(statut='f',typ='R' ), 
              DEXT_HAUT_FUT   =SIMP(statut='o',typ='R' ), 
              DEXT_BAS_FUT    =SIMP(statut='f',typ='R' ), 
              EPAIS_HAUT_FUT  =SIMP(statut='o',typ='R' ), 
              EPAIS_BAS_FUT   =SIMP(statut='f',typ='R' ), 
              NB_ELEM_BASE    =SIMP(statut='f',typ='I',defaut=30),
              NB_ELEM_FUT     =SIMP(statut='f',typ='I',defaut=70),
              NBEL_DCIR_BASE  =SIMP(statut='f',typ='I',defaut=15),
              NBEL_DCIR_FUT   =SIMP(statut='f',typ='I',defaut=15),
                             ),    
           ),

         b_model_poutre  =BLOC(condition = "MODELISATION == 'POUTRE' ",
   
           MONOPODE          =FACT(statut='o',max=1,

              H_TOTALE        =SIMP(statut='o',typ='R' ),  
              H_BASE          =SIMP(statut='o',typ='R' ),  
              H_MOYEU         =SIMP(statut='o',typ='R' ),  
              DECAL_PALES     =SIMP(statut='o',typ='R' ),  
              DEXT_HAUT_BASE  =SIMP(statut='o',typ='R' ), 
              DEXT_BAS_BASE   =SIMP(statut='f',typ='R' ), 
              EPAIS_HAUT_BASE =SIMP(statut='o',typ='R' ), 
              EPAIS_BAS_BASE  =SIMP(statut='f',typ='R' ), 
              DEXT_HAUT_FUT   =SIMP(statut='o',typ='R' ), 
              DEXT_BAS_FUT    =SIMP(statut='f',typ='R' ), 
              EPAIS_HAUT_FUT  =SIMP(statut='o',typ='R' ), 
              EPAIS_BAS_FUT   =SIMP(statut='f',typ='R' ), 
              NB_ELEM_BASE    =SIMP(statut='f',typ='I',defaut=30),
              NB_ELEM_FUT     =SIMP(statut='f',typ='I',defaut=70),
                             ),    
           ),
                  
         AFFE_MATERIAU   =FACT(statut='o',max=3,
             regles=(UN_PARMI('TOUT','GROUP_MA'),),
             TOUT           =SIMP(statut='f',typ='TXM',into=("OUI",) ),
             GROUP_MA       =SIMP(statut='f',typ='TXM',into=("BASE","FUT") ),
             MATER          =SIMP(statut='o',typ=mater_sdaster ),
             TEMP_REF       =SIMP(statut='f',typ='R',defaut= 0.E+0 ),
         ),

         CHARGEMENT        =FACT(statut='o',
           FORCE_POIDS_ROTOR   =SIMP(statut='o',typ='R' ),
           FORCE_POIDS_NACELLE =SIMP(statut='o',typ='R' ),
           FORCE_VENT_PALES =SIMP(statut='o',typ='R' ),
           ANGLE_VENT_AXE_X =SIMP(statut='o',typ='R' ),
           MOMENT_RESISTANT =SIMP(statut='o',typ='R' ),
           FORCE_VENT_FUT   =SIMP(statut='o',typ=(fonction_sdaster,formule) ),
           VENT    =SIMP(statut='o',typ='TXM',into=("OUI","NON") ),
           HOULE   =SIMP(statut='o',typ='TXM',into=("OUI","NON") ),
         ),

         FATIGUE   =FACT(statut='f',
           CRITERE   =SIMP(statut='f',typ='TXM',defaut="DANG_VAN_MODI_AC",into=("MATAKE","DANG_VAN_MODI_AC") ),
         ),

         SOLVEUR         =FACT(statut='d',
           METHODE         =SIMP(statut='f',typ='TXM',defaut="MULT_FRONT",into=("MULT_FRONT","LDLT","GCPC") ),
           b_mult_front    =BLOC(condition = "METHODE == 'MULT_FRONT' ",fr="Paramètres de la méthode multi frontale",
             RENUM           =SIMP(statut='f',typ='TXM',defaut="METIS",into=("MD","MDA","METIS") ),
           ),
           b_ldlt         =BLOC(condition = "METHODE == 'LDLT' ",fr="Paramètres de la méthode LDLT",
             RENUM           =SIMP(statut='f',typ='TXM',defaut="RCMK",into=("RCMK","SANS") ),
           ),
           b_ldlt_mult    =BLOC(condition = "METHODE == 'LDLT' or METHODE == 'MULT_FRONT' ",
                                   fr="Paramètres relatifs à la non inversibilité de la matrice à factorise",
             NPREC           =SIMP(statut='f',typ='I',defaut= 8 ),
             STOP_SINGULIER  =SIMP(statut='f',typ='TXM',defaut="OUI",into=("OUI","NON") ),
           ),
           b_gcpc         =BLOC(condition = "METHODE == 'GCPC' ", fr="Paramètres de la méthode du gradient conjugué",
             PRE_COND        =SIMP(statut='f',typ='TXM',into=("LDLT_INC",),defaut="LDLT_INC" ),
             NIVE_REMPLISSAGE=SIMP(statut='f',typ='I',defaut= 0 ),
             RENUM           =SIMP(statut='f',typ='TXM',defaut="RCMK",into=("SANS","RCMK") ),
             RESI_RELA       =SIMP(statut='f',typ='R',defaut= 1.E-6 ),
             NMAX_ITER       =SIMP(statut='f',typ='I',defaut= 0 ),
           ),
           SYME            =SIMP(statut='f',typ='TXM',defaut="NON",into=("OUI","NON") ),
         ),

         IMPRESSION      =FACT(statut='f',
           FORMAT          =SIMP(statut='f',typ='TXM',defaut="RESULTAT",
                                 into=("RESULTAT","CASTEM","IDEAS","ENSIGHT")),
                                 
           b_format_ideas  =BLOC(condition="FORMAT=='IDEAS'",fr="version Ideas",
             VERSION         =SIMP(statut='f',typ='I',defaut=5,into=(4,5)),
           ),  

           b_format_castem =BLOC(condition="FORMAT=='CASTEM'",fr="version Castem",
             NIVE_GIBI       =SIMP(statut='f',typ='I',defaut=10,into=(3,10)),
           ),
             
             TOUT_ORDRE      =SIMP(statut='f',typ='TXM',into=("OUI",) ),
             NUME_ORDRE      =SIMP(statut='f',typ='I',validators=NoRepeat(),max='**'),
             INST            =SIMP(statut='f',typ='R',validators=NoRepeat(),max='**'),
         ),      

        INFO           = SIMP(statut='f',typ='I',defaut=1,into=(1,2)),

);
