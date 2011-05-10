#@ MODIF macr_adap_mail_ops Macro  DATE 10/05/2011   AUTEUR SELLENET N.SELLENET 
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
# RESPONSABLE GNICOLAS G.NICOLAS
#
"""
Traitement des macros MACR_ADAP_MAIL/MACR_INFO_MAIL
"""
__revision__ = "V1.6"
#
def macr_adap_mail_ops ( self,
                         INFO, VERSION_HOMARD, MAILLAGE_FRONTIERE,
                         **args):
  """
     Traitement des macros MACR_ADAP_MAIL/MACR_INFO_MAIL
  """
#
#  1. args est le dictionnaire des arguments
#     args.keys() est la liste des mots-cles
#     args.keys()[0] est la premiere valeur de cette liste
#     args.keys()[1:] est la liste des valeurs suivantes dans cette liste
#     args.keys(mot_cle) represente le contenu de la variable mot_cle dans la macro appelante.
#
###  print 'glop'
###  print args
###  print args.keys()
###  if len (args.keys())>0 : print args.keys()[0]
###  print args["MAILLAGE"]
#
#  2. Les caracteristiques d'un passage sont conservees dans un dictionnaire. Il y a autant de
#     dictionnaires que de sollicitations pour une serie d'adaptation. L'ensemble de ces dictionnaires
#     est conserve dans la liste Liste_Passages. Cette liste est necessairement globale pour pouvoir
#     la retrouver a chaque nouveau passage.
#     Description du dictionnaire de passages :
#        dico["Maillage_0"]             = o ; string ; nom du concept du maillage initial de la serie d'adaptation
#        dico["Maillage_NP1"]           = o ; string ; nom du concept du dernier maillage adapte
#        dico["Rep_Calc_HOMARD_global"] = o ; string ; Nom global du repertoire de calcul pour HOMARD
#        dico["Rep_Calc_HOMARD_local"]  = o ; string ; Nom local du repertoire de calcul pour HOMARD
#                                                      depuis le repertoire de calcul pour ASTER
#        dico["niter"]                  = o ; entier ; numero d'iteration de depart
#
#  3. Les caracteristiques d'un maillage sont conservees dans un dictionnaire. Il y a autant de
#     dictionnaires que de maillages manipules. L'ensemble de ces dictionnaires est conserve
#     dans la liste liste_maillages.
#     Description du dictionnaire de maillages :
#        dico["Type_Maillage"] = o ; string ; "MAILLAGE_N", "MAILLAGE_NP1", "MAILLAGE_NP1_ANNEXE" ou "MAILLAGE_FRONTIERE"
#        dico["Nom_ASTER"]     = o ; concept ASTER associe
#        dico["Action"]        = o ; string ; "A_ecrire" ou "A_lire"
#        dico["NOM_MED"]       = o ; string ; Nom MED du maillage
#
#  4. Les caracteristiques d'un champ sont conservees dans un dictionnaire. Il y a autant de
#     dictionnaires que de champs manipules. L'ensemble de ces dictionnaires est conserve
#     dans la liste liste_champs.
#     Description du dictionnaire de champs :
#        dico["Type_Champ"]   = o ; string ; "INDICATEUR" ou "CHAMP_MAJ"
#        dico["RESULTAT"]     = f ; concept ASTER du resutat associe
#        dico["NOM_CHAM"]     = f ; string ; Nom ASTER du champ
#        dico["CHAM_GD"]      = f ; concept ASTER du champ de grandeur associee
#        dico["COMPOSANTE"]   = f ; liste ; Liste des noms ASTER des composantes du champ
#        dico["NUME_ORDRE"]   = f ; entier ; Numero d'ordre du champ
#        dico["INST"]         = f ; entier ; Instant du champ
#        dico["PRECISION"]    = f ; entier ; Precision sur l'instant du champ
#        dico["CRITERE"]      = f ; entier ; Critere de precision sur l'instant du champ
#        dico["CHAM_MAJ"]     = f ; string ; Nom ASTER du champ interpole sur le nouveau maillage
#        dico["NOM_CHAM_MED"] = o ; string ; Nom MED du champ
#        dico["SENSIBILITE"]  = f ; string ; Nom du parametre sensible associe
#
#  5. Signification de INFO
#     INFO = 1 : aucun message
#     INFO = 2 : les messages des commandes annexes (DEFI_FICHIER, IMPR_RESU, LIRE_MAILLAGE, LIRE_CHAMP)
#     INFO = 3 : aucun message pour les commandes annexes
#                1er niveau de message pour l'execution de HOMARD
#     INFO = 4 : aucun message pour les commandes annexes
#                2nd niveau de message pour l'execution de HOMARD
#
  from Accas import _F
  from Macro import creation_donnees_homard
  from Utilitai.Utmess     import UTMESS, MasquerAlarme, RetablirAlarme
  import aster
  import string
  import os
  import cPickle
  from glob import glob
  import tarfile
  from types import ListType, TupleType
  EnumTypes = (ListType, TupleType)
#gn  import shutil
#
  global Liste_Passages
  global numero_passage_fonction
#
#====================================================================
# 1. Prealables
#====================================================================
#
# 1.1. ==> La macro compte pour 1 dans la numerotation des commandes
#
  self.set_icmd(1)
#
# 1.2. ==> Initialisations de parametres Aster
#
  repertoire_outils = aster.repout()
  Rep_Calc_ASTER = os.getcwd()
  if ( INFO >= 3 ) : print os.listdir(Rep_Calc_ASTER)
#
# Remarque : le nom pick.homard.tar est obligatoire car ASTK rapatrie dans la base tous les fichiers en pick.*
  fichier_archive = os.path.join(Rep_Calc_ASTER, "pick.homard.tar")
#gn  print "fichier_archive = ",fichier_archive
#
# 1.3. ==> Numero du passage dans cette macro
#
  try :
    numero_passage_fonction = numero_passage_fonction + 1
  except :
    numero_passage_fonction = 1
#gn  print "numero_passage_fonction = ",numero_passage_fonction
#
# 1.4. ==> Au tout premier passage
#
  if numero_passage_fonction == 1 :
#
    Liste_Passages = []
#
# 1.4.2. ==> Avec un fichier de reprise : on recupere les repertoires archives
#
    if os.path.isfile(fichier_archive) :
#
#     Extraction de l'archive
#     Remarque : a partir de python 2.5 on pourra utiliser extractall
      if ( INFO >= 3 ) : print "Extraction de l'archive", fichier_archive
      file = tarfile.open(fichier_archive, "r")
      for tarinfo in file :
        if ( INFO >= 3 ) : print tarinfo.name, "is", tarinfo.size, "bytes in size and is",
        if tarinfo.isreg():
          if ( INFO >= 3 ) : print "a regular file."
          file.extract(tarinfo.name)
        elif tarinfo.isdir():
          if ( INFO >= 3 ) : print "a directory."
          file.extract(tarinfo.name)
        else:
          if ( INFO >= 3 ) : print "something else."
      if ( INFO >= 3 ) : print os.listdir(Rep_Calc_ASTER)
#     Liste de tous les repertoires d'adaptation qui ont ete recuperes
      laux = glob("*_ADAP_*")
#     On prend le fichier pickle du 1er repertoire (ce sont tous les memes),
#     puis on recupere la liste des passages
      fic = os.path.join(Rep_Calc_ASTER, laux[0], "pick.1")
      file = open(fic, "r")
      laux = cPickle.load(file)
      file.close()
#     Pour chaque cas, mise a jour du repertoire global
      for dico in laux :
        Rep_Calc_HOMARD_local  = dico["Rep_Calc_HOMARD_local"]
        dico["Rep_Calc_HOMARD_global"] = os.path.join(Rep_Calc_ASTER, Rep_Calc_HOMARD_local)
        Liste_Passages.append(dico)
#
  if ( INFO >= 3 ) : print "Liste_Passages = ", Liste_Passages
#
# 1.3. ==> On importe les definitions des commandes a utiliser dans la macro
#
  DEFI_FICHIER    = self.get_cmd("DEFI_FICHIER")
  IMPR_RESU       = self.get_cmd("IMPR_RESU")
  EXEC_LOGICIEL   = self.get_cmd("EXEC_LOGICIEL")
  LIRE_MAILLAGE   = self.get_cmd("LIRE_MAILLAGE")
  LIRE_CHAMP      = self.get_cmd("LIRE_CHAMP")
#
# 1.5. ==> Initialisations
#
  codret_partiel = [0]
#
  homard          = repertoire_outils + "homard"
#
  liste_maillages = []
  liste_champs    = []
  liste_zones     = []
  liste_front_analytiques = []
  dico_indi = {}
#
  LISTE_ADAPTATION_LIBRE = ("RAFF_DERA" , "RAFFINEMENT" , "DERAFFINEMENT")
#
  if ( INFO == 2 ) :
    infomail = "OUI"
    infocomm = 2
  else :
    infomail = "NON"
    infocomm = 1
#
#====================================================================
# 2. Decodage des arguments de la macro-commande
#====================================================================
# 2.1. ==> Donnees de pilotage de l'adaptation
#
  if ( self.nom == "MACR_ADAP_MAIL" ) :
#
    if args["ADAPTATION"] == "MODIFICATION" :
      mode_homard = "MODI"
    else :
      mode_homard = "ADAP"
#
# 2.1.1. ==> Les concepts "maillage"
#
#gn    print "\n.. Debut de 2.1.1"
    for mot_cle in ["MAILLAGE_N" , "MAILLAGE_NP1" , "MAILLAGE_NP1_ANNEXE"] :
#gn      print "\nmot_cle = ",mot_cle
      dico = {}
      dico["Type_Maillage"] = mot_cle
      if ( args[mot_cle] != None ) :
#gn        print "==> args[",mot_cle,"] = ",args[mot_cle]
        dico["Nom_ASTER"] = args[mot_cle]
        if ( mot_cle == "MAILLAGE_N" ) :
          dico["Action"] = "A_ecrire"
        else :
          dico["Action"] = "A_lire"
      else :
        dico["Action"] = "Rien"
#gn      print "dico = ",dico
      liste_maillages.append(dico)
#
# 2.1.2. ==> L'eventuel pilotage de l'adaptation
#
#gn    print "\n.. Debut de 2.1.2"
#gn    print "args = ", args
    if args["ADAPTATION"] in LISTE_ADAPTATION_LIBRE :
      dico = {}
      dico["Type_Champ"] = "INDICATEUR"
      if ( args["RESULTAT_N"] != None ) :
        dico["RESULTAT"]   = args["RESULTAT_N"]
        dico["NOM_CHAM"]   = args["NOM_CHAM"]
        if ( args["NUME_ORDRE"] != None ) :
          dico["NUME_ORDRE"] = args["NUME_ORDRE"]
        if ( args["INST"] != None ) :
          dico["INST"] = args["INST"]
          for cle in [ "PRECISION", "CRITERE" ] :
            if ( args[cle] != None ) :
              dico[cle] = args[cle]
        if ( args["SENSIBILITE"] != None ) :
          dico["SENSIBILITE"] = args["SENSIBILITE"]
      else :
        dico["CHAM_GD"] = args["CHAM_GD"]
#gn      print "dico = ", dico
#
      nom_cham_med_fichier = "champ_de_pilotage"
#                             12345678901234567890123456789012
      dico["NOM_CHAM_MED"] = nom_cham_med_fichier
#gn      print "==> dico[\"NOM_CHAM_MED\"] = ", dico["NOM_CHAM_MED"]
#
      if args.has_key("NOM_CMP") :
        if args["NOM_CMP"] is not None :
          if not type(args["NOM_CMP"]) in EnumTypes :
            l_aux = [args["NOM_CMP"]]
          else :
            l_aux = []
            les_composantes = args["NOM_CMP"]
            for composante in les_composantes :
              l_aux.append(composante)
          dico["COMPOSANTE"] = l_aux
#
      liste_champs.append(dico)
      dico_indi = dico
###      print dico
#
# 2.1.3. ==> Les champs a mettre a jour
#gn     print "\n.. Debut de 2.1.3."
#
    if args.has_key("MAJ_CHAM") :
#
      if args["MAJ_CHAM"] is None :
        les_champs = []
      else :
        les_champs = args["MAJ_CHAM"]
#
      dico_interp = {}
      dico_interp["AUTO"] = 0
      dico_interp["ISOP2"] = 3
#
      iaux = 0
      for maj_cham in les_champs :
#gn        print "maj_cham :", maj_cham
#gn        print type(maj_cham)
#
        dico = {}
        dico["Type_Champ"] = "CHAMP_MAJ"
        l_aux = [ "CHAM_MAJ", "TYPE_CHAM" ]
        if ( maj_cham["RESULTAT"] != None ) :
          l_aux.append("RESULTAT")
          l_aux.append("NOM_CHAM")
          if ( maj_cham["NUME_ORDRE"] != None ) :
            dico["NUME_ORDRE"] = maj_cham["NUME_ORDRE"]
          elif ( maj_cham["INST"] != None ) :
            dico["INST"] = maj_cham["INST"]
            for cle in [ "PRECISION", "CRITERE" ] :
              if ( maj_cham[cle] != None ) :
                dico[cle] = maj_cham[cle]
          if ( maj_cham["SENSIBILITE"] != None ) :
            dico["SENSIBILITE"] = maj_cham["SENSIBILITE"]
        else :
          l_aux.append("CHAM_GD")
        for cle in l_aux :
          dico[cle] = maj_cham[cle]
#gn        print "dico = ", dico
#
        if maj_cham["NOM_CMP"] is not None :
          if not type(maj_cham["NOM_CMP"]) in EnumTypes :
            l_aux = [maj_cham["NOM_CMP"]]
          else :
            l_aux = []
            les_composantes = maj_cham["NOM_CMP"]
            for composante in les_composantes :
              l_aux.append(composante)
          dico["COMPOSANTE"] = l_aux
#gn          print "COMPOSANTEmaj_cham", dico["COMPOSANTE"]
#
#gn        print "dico = ", dico
        iaux += 1
        la_chaine = '%08d' % iaux
        nom_cham_med_fichier = "champ_" + la_chaine
#                               123456789012345678901234    56789012
        dico["NOM_CHAM_MED"] = nom_cham_med_fichier
#gn        print "==> dico[\"NOM_CHAM_MED\"] = ", dico["NOM_CHAM_MED"]
#
        dico["TYPE_MAJ"] = dico_interp[maj_cham["TYPE_MAJ"]]
#gn        print "==> dico[\"TYPE_MAJ\"] = ", dico["TYPE_MAJ"]
#
#gn        print "dico :", dico
        liste_champs.append(dico)
#
# 2.1.4. ==> Les zones de raffinement
#
###    print "\n.. Debut de 2.1.4."
#
    if args.has_key("ZONE") :
#
      if args["ZONE"] is not None :
        l_aux = ['TYPE', 'X_MINI', 'X_MAXI', 'Y_MINI', 'Y_MAXI', 'Z_MINI', 'Z_MAXI', 'X_CENTRE', 'Y_CENTRE', 'Z_CENTRE', 'RAYON', 'RAYON_INT', 'RAYON_EXT', 'X_AXE', 'Y_AXE', 'Z_AXE', 'X_BASE', 'Y_BASE', 'Z_BASE', 'HAUTEUR' ]
        les_zones = args["ZONE"]
#
        for zone in les_zones :
###          print zone, "de type", type(zone)
          dico = {}
          for aux in l_aux :
            if ( zone[aux] != None ) :
              dico[aux] = zone[aux]
###        print dico
          liste_zones.append(dico)
#
###    print liste_zones
#
# 2.2. ==> Donnees de pilotage de l'information
#
  else :
#
    mode_homard = "INFO"
#
    dico = {}
    dico["Type_Maillage"] = "MAILLAGE_N"
    dico["Nom_ASTER"]     = args["MAILLAGE"]
    dico["Action"]        = "A_ecrire"
    liste_maillages.append(dico)
#
# 2.3. ==> Suivi d'une frontiere
# 2.3.1. ==> Suivi d'une frontiere maillee
#
#gn  print "\n.. Debut de 2.3.1."
#
  if ( MAILLAGE_FRONTIERE != None ) :
#
    dico = {}
    dico["Type_Maillage"] = "MAILLAGE_FRONTIERE"
    dico["Nom_ASTER"]     = MAILLAGE_FRONTIERE
    dico["Action"]        = "A_ecrire"
    liste_maillages.append(dico)
#
# 2.3.2. ==> Suivi d'une frontiere analytique
#
#gn  print "\n.. Debut de 2.3.2."
#
  if args.has_key("FRONTIERE_ANALYTIQUE") :
#
    if args["FRONTIERE_ANALYTIQUE"] is None :
      les_front_analytiques = []
    else :
      les_front_analytiques = args["FRONTIERE_ANALYTIQUE"]
#
    for frontiere in les_front_analytiques :
      l_aux = [ "NOM", "TYPE", "GROUP_MA", "RAYON", "X_CENTRE", "Y_CENTRE", "Z_CENTRE"]
      if ( frontiere["TYPE"] == "CYLINDRE" ) :
        l_aux.append("X_AXE")
        l_aux.append("Y_AXE")
        l_aux.append("Z_AXE")
      dico = {}
      for aux in l_aux :
        dico[aux] = frontiere[aux]
#gn      print dico
#
      liste_front_analytiques.append(dico)
#
# 2.4. ==> Le numero de version de HOMARD
#    Remarque : dans la donnee de la version de HOMARD, il faut remplacer
#               le _ de la donnee par un ., qui est interdit dans la
#               syntaxe du langage de commandes ASTER
#    Remarque : il faut remplacer le N majuscule de la donnee par
#               un n minuscule, qui est interdit dans la syntaxe du langage
#               de commandes ASTER
#
#gn  print "\n.. Debut de 2.4. avec VERSION_HOMARD = ", VERSION_HOMARD
  VERSION_HOMARD = string.replace(VERSION_HOMARD,"_" , ".")
  VERSION_HOMARD = string.replace(VERSION_HOMARD,"N" , "n")
#
  if ( VERSION_HOMARD[-6:]==".PERSO" ):
    VERSION_HOMARD = VERSION_HOMARD[:-6]
    version_perso = 1
  else :
    version_perso = 0
#gn  print ".... VERSION_HOMARD = ", VERSION_HOMARD
#gn  print ".... version_perso  = ", version_perso
#
# 2.54. ==> Les messages d'information
#
#gn  print "\n.. Debut de 2.5."
#gn  print args["INTERPENETRATION"]
  if ( args["INTERPENETRATION"] == "OUI" ) :
    if ( mode_homard == "INFO" ) :
      UTMESS('I','HOMARD0_6')
    else :
      UTMESS('A','HOMARD0_7')
#
#====================================================================
# 3. Preparation du lancement des commandes
#====================================================================
#
# 3.1. ==> . Elaboration des noms MED des concepts de maillage
#          . Memorisation des noms ASTER du maillage en entree et en sortie (sous forme string)
#
#          On cree une nouvelle liste des dictionnaires decrivant les maillages
#          et a la fin on ecrase l'ancienne liste par cette nouvelle.
#
#gn  print "\n.. Debut de 3.1."
#
  Nom_Concept_Maillage_NP1_ANNEXE = None
  l_aux = []
  for dico in liste_maillages :
#gn    print "\ndico avant = ",dico
    if ( dico["Action"] != "Rien" ) :
      dico["NOM_MED"] = aster.mdnoma(dico["Nom_ASTER"].nom)
      l_aux.append(dico)
      if ( dico["Type_Maillage"] == "MAILLAGE_N" ) :
        Nom_Concept_Maillage_N = dico["Nom_ASTER"].nom
      elif ( dico["Type_Maillage"] == "MAILLAGE_NP1" ) :
        Nom_Concept_Maillage_NP1 = dico["Nom_ASTER"].nom
      elif ( dico["Type_Maillage"] == "MAILLAGE_NP1_ANNEXE" ) :
        Nom_Concept_Maillage_NP1_ANNEXE = dico["Nom_ASTER"].nom
#gn    print "\ndico apres = ",dico
  liste_maillages = l_aux
#
# 3.2. ==> Recherche du numero d'iteration et du repertoire de travail
#
# 3.2.1. ==> Par defaut :
#            . le numero d'iteration est nul
#            . le nom du repertoire de lancement de HOMARD est construit sur le nom
#              du maillage en entree et le numero de passage dans la fonction
#
#gn  print "\.. Debut de 3.2.1."
#
  niter = 0
  Nom_Rep_local = Nom_Concept_Maillage_N + "_" + mode_homard + "_" + str(numero_passage_fonction)
  Rep_Calc_HOMARD_local = os.path.join(".", Nom_Rep_local)
  Rep_Calc_HOMARD_global = os.path.join(Rep_Calc_ASTER, Nom_Rep_local)
#gn  print "Rep_Calc_HOMARD_local  = ", Rep_Calc_HOMARD_local
#gn  print "Rep_Calc_HOMARD_global = ", Rep_Calc_HOMARD_global
#
# 3.2.2. ==> En adaptation : il faut repartir du repertoire de l'iteration precedente
#
#gn  print "\.. Debut de 3.2.2."
#
  if ( mode_homard == "ADAP" ) :
#
# 3.2.2.1. ==> On recherche si dans les passages deja effectues, il en existe un
#              dont le maillage d'arrivee etait l'actuel maillage d'entree. Si c'est
#              le cas, cela veut dire que l'adaptation en cours est la suite d'une
#              precedente. On doit donc utiliser le meme repertoire. Le numero
#              d'iteration est celui de l'adaptation precedente augmente de 1.
#              Si on n'a rien trouve, c'est que l'on demarre un nouveau cas.
#
#gn    print "\.. Debut de 3.2.2.1."
#
    for dico in Liste_Passages :
      if ( dico["Maillage_NP1"] == Nom_Concept_Maillage_N ) :
        niter   = dico["niter"] + 1
        Rep_Calc_HOMARD_local  = dico["Rep_Calc_HOMARD_local"]
        Rep_Calc_HOMARD_global = dico["Rep_Calc_HOMARD_global"]
#
# 3.2.2.2. ==> Memorisation de ce passage
#
#gn    print "\.. Debut de 3.2.2.2."
#
# 3.2.2.2.1. ==> Enregistrement d'un nouveau cas
#                On emet une alarme si il existe deja un cas pour etre certain
#                que l'utilisateur ne s'est pas trompe dans l'enchainement
#
    if ( niter == 0 ) :
      for dico in Liste_Passages :
        UTMESS("A", 'HOMARD0_9', valk=(Nom_Concept_Maillage_N, Nom_Concept_Maillage_NP1, dico["Maillage_NP1"], dico["Maillage_0"]), vali=dico["niter"]+1 )
      dico = {}
      dico["Maillage_0"]             = Nom_Concept_Maillage_N
      dico["Maillage_NP1"]           = Nom_Concept_Maillage_NP1
      dico["Maillage_NP1_ANNEXE"]    = Nom_Concept_Maillage_NP1_ANNEXE
      dico["Rep_Calc_HOMARD_local"]  = Rep_Calc_HOMARD_local
      dico["Rep_Calc_HOMARD_global"] = Rep_Calc_HOMARD_global
      dico["niter"]                  = niter
      Liste_Passages.append(dico)
#
# 3.2.2.2.2. ==> Modification du cas en cours
#
    else :
      l_aux = []
      for dico in Liste_Passages :
        if ( dico["Maillage_NP1"] == Nom_Concept_Maillage_N ) :
          dico["Maillage_NP1"]        = Nom_Concept_Maillage_NP1
          dico["Maillage_NP1_ANNEXE"] = Nom_Concept_Maillage_NP1_ANNEXE
          dico["niter"]               = niter
        l_aux.append(dico)
      Liste_Passages = []
      for dico in l_aux :
        Liste_Passages.append(dico)
#
#gn    print "Apres 3.2.2.2. Liste_Passages = ", Liste_Passages
#
###  print "niter = ", niter, ", Rep_Calc_HOMARD_global = ", Rep_Calc_HOMARD_global
#
# 3.2.3. Le repertoire pour homard
#        Attention : on ne fait cette creation qu'une seule fois par cas
#                    d'adaptation ou d'information
#
#gn  print "\.. Debut de 3.2.3."
#
  if ( niter == 0 ) :
#
    try :
      os.mkdir(Rep_Calc_HOMARD_global)
    except os.error,codret_partiel :
      self.cr.warn("Code d'erreur de mkdir : " + str(codret_partiel[0]) + " : " + codret_partiel[1])
      UTMESS("F", 'HOMARD0_4', valk=Rep_Calc_HOMARD_global)
#
  else :
#
    if not os.path.isdir(Rep_Calc_HOMARD_global) :
      UTMESS("F", 'HOMARD0_8', valk=Rep_Calc_HOMARD_global)
#
#====================================================================
# 4. Ecriture des commandes de creation des donnees MED
#====================================================================
#
#gn  print "\.. Debut de 4."
#
#  On doit ecrire : le maillage,
#                   le champ de pilotage de l'adaptation
#                   les champs a convertir
#  Remarque : on met tout dans le meme fichier
#
#  Chacune de ces ecritures est optionnelle selon le contexte.
#
# 4.1. ==> Noms des fichiers d'ASTER vers HOMARD et eventuellement de HOMARD vers ASTER
#          Remarque : aujourd'hui, les ecritures ou les lectures au format MED se font obligatoirement
#                     dans un fichier de nom fort.n, place dans le repertoire de calcul
##
# 4.1.1. ==> D'ASTER vers HOMARD
#
  unite_fichier_aster_vers_homard = 1787 + 2*numero_passage_fonction
  fichier_aster_vers_homard = os.path.join(Rep_Calc_ASTER, "fort." + str(unite_fichier_aster_vers_homard))
###  print "fichier_aster_vers_homard = ",fichier_aster_vers_homard
#
# 4.1.2. ==> De HOMARD vers ASTER
#
  if ( mode_homard in [ "ADAP", "MODI" ] ) :
    unite_fichier_homard_vers_aster = unite_fichier_aster_vers_homard + 1
    fichier_homard_vers_aster = os.path.join(Rep_Calc_ASTER,"fort." + str(unite_fichier_homard_vers_aster))
###    print "fichier_homard_vers_aster = ",fichier_homard_vers_aster
#
# 4.2. La definition du fichier de ASTER vers HOMARD
#
  DEFI_FICHIER ( ACTION= "ASSOCIER",
                 UNITE = unite_fichier_aster_vers_homard,
                 TYPE = "LIBRE",
                 INFO = infocomm )
#
# 4.3. Le(s) maillage(s)
# Le maillage de calcul et l'eventuel maillage de la frontiere sont ecrits
# dans le meme fichier MED
# En fait, on pourrait s'en passer au dela de la 1ere iteration
# car HOMARD a memorise. Mais des que l'on ecrit un champ,
# les conventions MED imposent la presence du maillage dans le fichier.
# Donc on va toujours ecrire.
#
  for dico in liste_maillages :
    if ( dico["Action"] == "A_ecrire" ) :
      motscsi = {}
      motscsi["MAILLAGE"] = dico["Nom_ASTER"]
      motscfa = {}
      motscfa["RESU"] = _F( INFO_MAILLAGE=infomail,
                          **motscsi )
#
      IMPR_RESU ( INFO = infocomm,
                  FORMAT ='MED', UNITE = unite_fichier_aster_vers_homard,
                  **motscfa )
#
# 4.4. Le(s) champ(s)
#        Attention : il se peut que l'on demande la mise à jour du champ qui a servi comme
#                    pilotage de l'adaptation. Si c'est le cas, il ne faut pas demander son
#                    impression sinon il y a plantage d'IMPR_RESU qui ne sait pas substituer
#                    deux champs. D'ailleurs, c'est plus economique ainsi !
#        Remarque : pour l'adaptation ou les champs a mettre a jour, on peut ne demander
#                   qu'un nombre reduit de composantes.
#        dico["Type_Champ"]   = o ; string ; "INDICATEUR" ou "CHAMP_MAJ"
#        dico["RESULTAT"]     = f ; concept ASTER du resutat associe
#        dico["NOM_CHAM"]     = f ; string ; Nom ASTER du champ
#        dico["CHAM_GD"]      = f ; concept ASTER du champ de grandeur associee
#        dico["COMPOSANTE"]   = f ; liste ; Liste des noms ASTER des composante du champ
#        dico["NUME_ORDRE"]   = f ; entier ; Numero d'ordre du champ
#        dico["INST"]         = f ; entier ; Instant du champ
#        dico["PRECISION"]    = f ; entier ; Precision sur l'instant du champ
#        dico["CRITERE"]      = f ; entier ; Critere de precision sur l'instant du champ
#        dico["CHAM_MAJ"]     = f ; string ; Nom ASTER du champ interpole sur le nouveau maillage
#        dico["NOM_CHAM_MED"] = o ; string ; Nom MED du champ
#        dico["SENSIBILITE"]  = f ; string ; Nom du parametre sensible associe
#
# 4.4.1. Recherche d'un doublon eventuel sur le champ de pilotage de l'adaptation
#
#gn  print "dico_indi = ",dico_indi
  if len(dico_indi) > 0 :
    indic_est_deja_imprime = 0
    if dico_indi.has_key("RESULTAT") :
      l_aux = [ "RESULTAT", "NOM_CHAM" ]
    else :
      l_aux = [ "CHAM_GD" ]
  else :
    indic_est_deja_imprime = 1
    l_aux = [ ]
#gn  print ".. Au debut de la boucle, l_aux = ",l_aux
#gn  print ".. Au debut de la boucle, indic_est_deja_imprime = ",indic_est_deja_imprime
#
  liste_champs_imprime = []
  for dico in liste_champs :
#gn    print "\n.... dico = ",dico
#   Pour un champ a mettre a jour, on a toujours impression
    if ( dico["Type_Champ"] == "CHAMP_MAJ" ) :
      liste_champs_imprime.append(dico)
#     Si le champ de pilotage de l'adaptation n'a toujours pas ete repere comme champ a mettre a jour :
      if not indic_est_deja_imprime :
#       Est-ce le meme champ ?
        on_a_le_champ = 1
        for cle in l_aux :
          if ( dico.has_key(cle) ) :
###            print "...... dico_indi[cle] = ",dico_indi[cle]
###            print "...... dico[cle]      = ",dico[cle]
            if ( dico_indi[cle] != dico[cle] ) :
              on_a_le_champ = 0
              break
          else :
            on_a_le_champ = 0
            break
#       Si oui, est-ce un champ sensible ou non ?
        if on_a_le_champ :
          cle = "SENSIBILITE"
          if dico.has_key(cle) :
            if ( dico[cle] != None ) :
              if dico_indi.has_key(cle) :
                if ( dico_indi[cle] != dico[cle] ) :
                  on_a_le_champ = 0
                  break
              else :
                on_a_le_champ = 0
                break
#       Si oui, est-ce au meme moment ? (remarque : si rien n'est designe, c'est qu'il n'y a qu'un
#       seul instant ... donc c'est le meme ! En revanche, on ne sait pas comparer une donnee
#       en numero d'ordre et une donnee en instant. On croise les doigts.)
        if on_a_le_champ :
          for cle in [ "NUME_ORDRE", "INST" ] :
            if dico.has_key(cle) :
              if ( dico[cle] != None ) :
                if dico_indi.has_key(cle) :
                  if ( dico_indi[cle] != dico[cle] ) :
                    on_a_le_champ = 0
                    break
#       Le champ de pilotage fait partie des champs mis a jour : on le note
#       et on utilise le meme nom de champ MED
        if on_a_le_champ :
          dico_indi["NOM_CHAM_MED"] = dico["NOM_CHAM_MED"]
          indic_est_deja_imprime = 1
#gn  print "\n\nFin de la boucle .. indic_est_deja_imprime = ",indic_est_deja_imprime
#   Si le champ de pilotage de l'adaptation n'a pas ete repere comme champ a mettre a jour,
#   il faut l'inclure dans les champs e imprimer
  if not indic_est_deja_imprime :
    liste_champs_imprime.append(dico_indi)
#
# 4.4.2. Impressions apres le filtrage precedent
#gn  print "\n.... Debut de 4.2.4.2."
#
  for dico in liste_champs_imprime :
    motscsi = {}
    for cle in [ "RESULTAT", "NOM_CHAM", "CHAM_GD", "NUME_ORDRE", "INST", "PRECISION", "CRITERE", "NOM_CHAM_MED" ] :
      if dico.has_key(cle) :
        if ( dico[cle] != None ) :
          motscsi[cle] = dico[cle]
    if dico.has_key("COMPOSANTE") :
      if ( len(dico["COMPOSANTE"]) == 1 ) :
        motscsi["NOM_CMP"] = dico["COMPOSANTE"][0]
      else :
        motscsi["NOM_CMP"] = dico["COMPOSANTE"]
    if dico.has_key("SENSIBILITE") :
      motscsi["SENSIBILITE"] = dico["SENSIBILITE"]
    motscfa = {}
    motscfa["RESU"] = _F( INFO_MAILLAGE=infomail,
                        **motscsi
                      )
#gn    print ".. motscfa = ",motscfa
#
    IMPR_RESU ( INFO = infocomm,
                FORMAT ='MED', UNITE = unite_fichier_aster_vers_homard,
                **motscfa )
#
#====================================================================
# 5. ==> Creation des fichiers de donnees pour HOMARD
#====================================================================
#
#gn  print "\.. Debut de 5."
#
  dico_configuration = {}
#
# 5.1. ==> Les generalites
#
  dico_configuration["INFO"] = INFO
#
  dico_configuration["Rep_Calc_HOMARD_global"] = Rep_Calc_HOMARD_global
  dico_configuration["VERSION_HOMARD"] = VERSION_HOMARD
  dico_configuration["version_perso"] = version_perso
  if args.has_key("UNITE") :
    UNITE = args["UNITE"]
    fichier_conf_suppl = os.path.join(Rep_Calc_ASTER,"fort." + str(UNITE))
    dico_configuration["fichier_conf_suppl"] = fichier_conf_suppl
#
  dico_configuration["niter"] = niter
  dico_configuration["Fichier_ASTER_vers_HOMARD"] = fichier_aster_vers_homard
  if ( mode_homard in [ "ADAP", "MODI" ] ) :
    dico_configuration["Fichier_HOMARD_vers_ASTER"] = fichier_homard_vers_aster
#
# 5.2. ==> Les noms med des maillages
#
  for dico in liste_maillages :
#gn    print "Nom MED de " + dico["Type_Maillage"] + " = " + dico["NOM_MED"]
    dico_configuration[ "NOM_MED_"+dico["Type_Maillage"] ] = dico["NOM_MED"]
#gn  print dico_configuration
#
# 5.3. ==> Les caracteristiques de l'eventuel pilotage de l'adaptation
#
  for dico in liste_champs :
    dico_aux = {}
    if ( dico["Type_Champ"] == "INDICATEUR" ) :
      l_aux = [ "NOM_CHAM_MED" ]
      if dico.has_key("COMPOSANTE") :
        l_aux.append("COMPOSANTE")
      if dico.has_key("NUME_ORDRE") :
        l_aux.append("NUME_ORDRE")
      for cle in l_aux :
        if ( dico[cle] != None ) :
          dico_aux[cle] = dico[cle]
      dico_configuration["Indicateur"] = dico_aux
#gn  if dico_configuration.has_key("Indicateur") :
#gn    print "dico_configuration[Indicateur] = ", dico_configuration["Indicateur"]
#
# 5.4. ==> Les eventuelles zones de raffinement
#
  prem = 1
  for dico in liste_zones :
    if prem :
      l_aux = [dico]
      prem = 0
    else :
      l_aux = dico_configuration["Zones_raffinement"]
      l_aux.append(dico)
    dico_configuration["Zones_raffinement"] = l_aux
###  if dico_configuration.has_key("Zones_raffinement") :
###    print "dico_configuration[Zones_raffinement] = ", dico_configuration["Zones_raffinement"]
#
# 5.5. ==> Les eventuelles mises a jour de champs
#
  prem = 1
  for dico in liste_champs :
    dico_aux = {}
    if ( dico["Type_Champ"] == "CHAMP_MAJ" ) :
      l_aux = [ "NOM_CHAM_MED", "COMPOSANTE", "TYPE_MAJ" ]
      if dico.has_key("NUME_ORDRE") :
        l_aux.append("NUME_ORDRE")
      else :
        for cle in [ "RESULTAT", "NOM_CHAM", "INST", "PRECISION", "CRITERE" ] :
          l_aux.append(cle)
      for cle in l_aux :
        if dico.has_key(cle) :
          if ( dico[cle] != None ) :
            dico_aux[cle] = dico[cle]
#gn      print dico_aux
      if prem :
        l_aux = [dico_aux]
        prem = 0
      else :
        l_aux = dico_configuration["Champs"]
        l_aux.append(dico_aux)
      dico_configuration["Champs"] = l_aux
#gn  if dico_configuration.has_key("Champs") :
#gn   print "dico_configuration[Champs] = ", dico_configuration["Champs"]
#
# 5.6. ==> Les eventuelles frontieres analytiques
#
  prem = 1
  for dico in liste_front_analytiques :
    if prem :
      l_aux = [dico]
      prem = 0
    else :
      l_aux = dico_configuration["Frontiere_analytique"]
      l_aux.append(dico)
    dico_configuration["Frontiere_analytique"] = l_aux
#gn  if dico_configuration.has_key("Frontiere_analytique") :
#gn    print "dico_configuration[Frontiere_analytique] = ", dico_configuration["Frontiere_analytique"]
#
# 5.7. ==> Appel de la fonction de creation
#
  donnees_homard = creation_donnees_homard.creation_donnees_homard ( self.nom, args, dico_configuration )
  if ( INFO >= 4 ) :
    donnees_homard.quel_mode ( )
  fic_homard_niter, fic_homard_niterp1 = donnees_homard.creation_configuration ( )
  donnees_homard.ecrire_fichier_configuration ( )
  if ( mode_homard == "INFO" ) :
    Nom_Fichier_Donnees = donnees_homard.ecrire_fichier_donnees ( )
  else :
    Nom_Fichier_Donnees = "0"
#
# 5.8. ==> Impression eventuelle des fichiers crees
#
#gn  print "Repertoire ",Rep_Calc_HOMARD_global
  if ( INFO >= 4 ) :
    l_aux = ["HOMARD.Donnees" , "HOMARD.Configuration"]
  else :
    l_aux = [ ]
  for nomfic in l_aux :
    fic = os.path.join(Rep_Calc_HOMARD_global, nomfic)
    if os.path.isfile (fic) :
      print "\n\n=============================================================="
      print "Contenu de", nomfic
      fichier = open (fic,"r")
      les_lignes = fichier.readlines()
      fichier.close()
      for ligne in les_lignes :
        print ligne[:-1]
      print "==============================================================\n"
#gn  if ( mode_homard == "ADAP" ) :
#gn    if args.has_key("MAJ_CHAM") :
#gn      if args["MAJ_CHAM"] is not None :
#gn        import time
#gn        time.sleep(3600)
#
#====================================================================
# 6. Ecriture de la commande d'execution de homard
#====================================================================
#gn  print "\.. Debut de 6."
#
  if ( INFO == 1 ) :
    iaux = INFO
  else :
    iaux = 2
  EXEC_LOGICIEL ( ARGUMENT = (Rep_Calc_HOMARD_global, # nom du repertoire
                              VERSION_HOMARD,         # version de homard
                              str(INFO),              # niveau d information
                              Nom_Fichier_Donnees,    # fichier de donnees HOMARD
                              str(version_perso),     # version personnelle de homard ?
                             ),
                  LOGICIEL = homard,
                  INFO     = iaux,
                )
#gn  import time
#gn  time.sleep(3600)
#
#====================================================================
# 7. ==> Ecriture de la commande de lecture des resultats med
#        Remarque :
#        La fonction self.DeclareOut(a,b) fonctionne ainsi :
#        a est une chaine de caracteres
#        b est la variable declaree dans la commande
#        le but est de associer le contenu de b a la variable locale qui sera designee par a
#        Exemple :
#        self.DeclareOut("maillage_a_lire", args["MAILLAGE_NP1"])
#        ==> la variable maillage_a_lire est identifiee a l'argument "MAILLAGE_NP1"
#====================================================================
#gn  print "\.. Debut de 7."
#
  if ( mode_homard in [ "ADAP", "MODI" ] ) :
#
# 7.1. ==> Le maillage
#          On inhibe l'alarme MODELISA5_49 qui apparait car on fait VERIF=NON
#
#gn    print "args = ",args
    for dico in liste_maillages :
#gn      print dico
      if ( dico["Action"] == "A_lire" ) :
#
        MasquerAlarme('MODELISA5_49')
#
        self.DeclareOut("maillage_a_lire", dico["Nom_ASTER"])
        maillage_a_lire = LIRE_MAILLAGE ( UNITE = unite_fichier_homard_vers_aster,
                                       FORMAT = "MED",
                                       NOM_MED = dico["NOM_MED"],
                                       VERI_MAIL = _F(VERIF="NON"), INFO_MED = infocomm, INFO = infocomm )
#
        RetablirAlarme('MODELISA5_49')
#
#gn        print "MAILLAGE = ",maillage_a_lire
#gn        print "NOM_MED = ",dico["NOM_MED"]
        if ( dico["Type_Maillage"] == "MAILLAGE_NP1" ) :
          maillage_np1 = maillage_a_lire
          maillage_np1_nom_med = dico["NOM_MED"]
#
# 7.2. ==> Les champs
#gn    import time
#gn    time.sleep(3600)
#
    for dico in liste_champs :
      if ( dico["Type_Champ"] == "CHAMP_MAJ" ) :
#gn        print dico
        motscsi = {}
        for cle in [ "NUME_ORDRE", "INST", "PRECISION", "CRITERE" ] :
          if dico.has_key(cle) :
            if ( dico[cle] != None ) :
              motscsi[cle] = dico[cle]
        if dico.has_key("NUME_ORDRE") :
          motscsi["NUME_PT"] = dico["NUME_ORDRE"]
#gn        print "MAILLAGE = ",maillage_np1
#gn        print "NOM_MAIL_MED = ",maillage_np1_nom_med
#gn        print "NOM_CHAM_MED = ",dico["NOM_CHAM_MED"]
#gn        print "TYPE_CHAM =", dico["TYPE_CHAM"]
        self.DeclareOut("champ_maj", dico["CHAM_MAJ"])
        champ_maj = LIRE_CHAMP ( UNITE = unite_fichier_homard_vers_aster, FORMAT = "MED",
                                 MAILLAGE = maillage_np1, NOM_MAIL_MED=maillage_np1_nom_med,
                                 NOM_MED = dico["NOM_CHAM_MED"], NOM_CMP_IDEM = "OUI", TYPE_CHAM = dico["TYPE_CHAM"],
                                 INFO = infocomm, **motscsi )
#
#====================================================================
# 8. Menage des fichiers devenus inutiles
#    Il est important de faire le menage des fichiers MED, qui sont
#    les plus gros.
#    On doit imperativement garder le dernier fichier homard produit
#    En mode d'information, on garde egalement les fichiers textes
#====================================================================
#gn  print "\.. Debut de 8."
#
  l_aux = [fichier_aster_vers_homard]
  if ( mode_homard in [ "ADAP", "MODI" ] ) :
    l_aux.append(fichier_homard_vers_aster)
#
  l_aux_bis = os.listdir(Rep_Calc_HOMARD_global)
  for fic in l_aux_bis :
    fic_total = os.path.join(Rep_Calc_HOMARD_global, fic)
    l_aux.append(fic_total)
#
  l_aux_bis = []
  if ( mode_homard == "ADAP" ) :
    fic = os.path.join(Rep_Calc_HOMARD_global, fic_homard_niterp1)
    l_aux_bis.append(fic)
#
  for fic in l_aux :
    if ( INFO >= 3 ) :
      print "Examen du fichier ", fic
    if fic not in l_aux_bis :
      if ( INFO >= 3 ) :
        print "==> Destruction du fichier"
      if os.path.isfile(fic) :
        try :
          os.remove(fic)
        except os.error,codret_partiel :
          self.cr.warn("Code d'erreur de remove : " + str(codret_partiel[0]) + " : " + codret_partiel[1])
          UTMESS("F", 'HOMARD0_5', valk=fic)
#
# Liberation du fichier de ASTER vers HOMARD
#
  DEFI_FICHIER ( ACTION= "LIBERER",
                 UNITE = unite_fichier_aster_vers_homard,
                 INFO = infocomm )
#gn  print "Repertoire ",Rep_Calc_HOMARD_global
#gn  print os.listdir(Rep_Calc_HOMARD_global)
#gn  print "Repertoire ",Rep_Calc_ASTER
#gn  print os.listdir(Rep_Calc_ASTER)
#
#====================================================================
# 9. Archivage des repertoires d'adaptation en vue de poursuite
#====================================================================
#gn  print "\.. Debut de 9."
#
#
  if ( INFO >= 3 ) : print os.listdir(Rep_Calc_ASTER)
  if ( INFO >= 3 ) : print "Archivage dans", fichier_archive
  laux = []
  for dico in Liste_Passages :
#   Memorisation du nom du repertoire local pour ce cas d'adaptation
    Rep_Calc_HOMARD_local  = dico["Rep_Calc_HOMARD_local"]
    laux.append(Rep_Calc_HOMARD_local)
#   Memorisation de la liste des passages
#   Remarque : c'est fait a chaque repertoire pour faciliter le decodage ensuite
    Rep_Calc_HOMARD_global = dico["Rep_Calc_HOMARD_global"]
    fic = os.path.join(Rep_Calc_HOMARD_global, "pick.1")
    file = open(fic, "w")
    cPickle.dump(Liste_Passages, file)
    file.close()
# Si on a au moins un cas d'adaptation, archivage
  if len(laux) > 0 :
    file = tarfile.open(fichier_archive, "w")
    for rep in laux :
      if ( INFO >= 3 ) : print ".. Insertion de", rep
      file.add(rep)
    file.close()
  if ( INFO >= 3 ) : print os.listdir(Rep_Calc_ASTER)
#
#====================================================================
#  C'est fini !
#====================================================================
#
###  if ( mode_homard == "ADAP" and niter == 3 ) :
###  if ( niter == 2 ) :
#gn  import time
#gn  time.sleep(3600)
#
  return



