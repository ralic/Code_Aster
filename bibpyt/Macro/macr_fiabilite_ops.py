#@ MODIF macr_fiabilite_ops Macro  DATE 11/05/2010   AUTEUR COURTOIS M.COURTOIS 
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

# RESPONSABLE GNICOLAS G.NICOLAS

import os
import sys


def macr_fiabilite_ops(self, INFO,
                       LOGICIEL, VERSION,
                       UNITE_ESCL, MESS_ASTER,
                       SEUIL, SEUIL_TYPE,
                       VARIABLE,
                       **args ) :
#
#    args est le dictionnaire des arguments optionnels
#    args.keys() est la liste des mots-clés
#    args.keys()[0] est la premiere valeur de cette liste
#    args.keys()[1:] est la liste des valeurs suivantes dans cette liste
#    args.keys(mot_cle) représente le contenu de la variable mot_cle dans la macro appelante.
###  print args
###  print args.keys()
###  if len (args.keys())>0 : print args.keys()[0]
#
  """ Macro-commande réalisant le pilotage du logiciel de fiabilite. """
#
# On charge les modules nécessaires
  from Accas import _F
  from Macro import fiabilite_mefisto
  import aster
  import numpy
#
#____________________________________________________________________
#
# 1. Préalables
#____________________________________________________________________
#
# 1.1 ==> La macro compte pour 1 dans l'exécution des commandes
#
  self.set_icmd(1)
#
# 1.2 ==> On importe les définitions des commandes Aster utilisées
#         dans la macro
#
  EXEC_LOGICIEL  = self.get_cmd("EXEC_LOGICIEL")
  DEFI_LIST_REEL = self.get_cmd("DEFI_LIST_REEL")
#
# 1.3 ==> Le nom du programme de fiabilite à lancer
#
  repertoire_outils = aster.repout()
  fiabilite      = repertoire_outils + "fiabilite"
#
# 1.4 ==> Initialisations
#
  erreur = 0
  erreur_partiel = [0]
  Rep_Calc_ASTER = os.getcwd()
  Nom_Exec_ASTER = sys.executable
#
  messages_erreur = { 0 : "Tout va bien",
                      1 : "Impossible de créer le répertoire de travail pour le logiciel de fiabilité.",
                      2 : "Probleme d'ouverture du fichier.",
                     10 : "Erreur dans le choix du logiciel de fiabilité.",
                     11 : "Erreur dans la création des données pour le logiciel de fiabilité.",
                    100 : "Erreur." }
#
  while not erreur :
#
#____________________________________________________________________
#
# 2. Répertoires et fichiers
#____________________________________________________________________
#
# 2.1. ==> Création du répertoire pour l'exécution du logiciel de fiabilité
#
    Nom_Rep_local = "tmp_fiabilite"
    Rep_Calc_LOGICIEL_local = os.path.join(".", Nom_Rep_local)
    Rep_Calc_LOGICIEL_global = os.path.join(Rep_Calc_ASTER, Nom_Rep_local)
#
    try :
      os.mkdir(Rep_Calc_LOGICIEL_global)
    except os.error, erreur_partiel :
      self.cr.warn("Code d'erreur de mkdir : " + str(erreur_partiel[0]) + " : " + erreur_partiel[1])
      self.cr.fatal("Impossible de créer le répertoire de travail pour le logiciel de fiabilité : "+Rep_Calc_LOGICIEL_global)
      erreur = erreur + 1
      break
#
# 2.2. ==> On définit un fichier pour les résultats du calcul de fiabilité
#
    FIC_RESU_FIABILITE = os.path.join(Rep_Calc_LOGICIEL_global, "resu_fiabilite")
#
# 2.3. ==> On crée un fichier annexe pour transmettre des données à la procédure
#          de lancement des calculs ASTER par le LOGICIEL.
#          Ce fichier est créé dans le répertoire d'exécution du logiciel de fiabilité.
#          On fait ainsi car les arguments passés ont du mal à transiter via l'exécutable.
#          On stocke :
#          1. Le niveau d'information
#          2. L'unité logique associée au jeu de commandes déterministes
#          3. La gestion des sorties ASTER
#          4. Le nom de l'exécutable ASTER
#          5. Le type de seuil du problème (maximum ou minimum)
#
    fic_Info_ASTER = os.path.join(Rep_Calc_LOGICIEL_global,"InfoExecASTER")
    try :
      f_execAster = open(fic_Info_ASTER, "w")
    except os.error,erreur_partiel :
      self.cr.warn("Fichier : "+fic_Info_ASTER)
      self.cr.warn("Code d'erreur de open : " + str(erreur_partiel[0]) + " : " + erreur_partiel[1])
      erreur = 2
      break
#
    f_execAster.write(str(INFO)+"\n")
    f_execAster.write(str(UNITE_ESCL)+"\n")
    f_execAster.write(str(MESS_ASTER)+"\n")
    f_execAster.write(str(Nom_Exec_ASTER)+"\n")
    f_execAster.write(str(SEUIL_TYPE))
    f_execAster.close()
    fichier = open (fic_Info_ASTER,"r")
#
    if INFO >= 2 :
      print "\nContenu du fichier " + fic_Info_ASTER," :"
      les_lignes = fichier.readlines()
      fichier.close()
      print les_lignes, "\n"
#
#____________________________________________________________________
#
# 3. Les variables par defaut
#____________________________________________________________________
#
# 3.1. ==> Dictionnaire des valeurs physiques et liées à la loi
#
    valeurs_lois = { }
#
    for la_variable in VARIABLE :
#
      v_moy_physique = None
      v_moy_loi = None
      v_min_loi = None
      v_max_loi = None
      sigma_loi = None
#
# 3.1.1. ==> loi uniforme : transfert des min et max
#            on définit une moyennne comme étant la médiane des extremes.
#
      if la_variable["LOI"] == "UNIFORME" :
        v_moy_physique = 0.5 * ( la_variable["VALE_MIN"] + la_variable["VALE_MAX"] )
        v_min_loi = la_variable["VALE_MIN"]
        v_max_loi = la_variable["VALE_MAX"]
#
# 3.1.2. ==> loi normale : transfert des moyennne et écart-type.
#
      elif la_variable["LOI"] == "NORMALE" :
        v_moy_loi = la_variable["VALE_MOY"]
        v_moy_physique = v_moy_loi
        sigma_loi = la_variable["ECART_TYPE"]
#
# 3.1.3. ==> loi lognormale : identité du min, conversion pour le reste
#
      elif la_variable["LOI"] == "LOGNORMALE" :
        v_min_loi = la_variable["VALE_MIN"]
        if la_variable["VALE_MOY_PHY"] is None :
          v_moy_loi = la_variable["VALE_MOY"]
          sigma_loi = la_variable["ECART_TYPE"]
          aux = numpy.exp(0.5*sigma_loi*sigma_loi+v_moy_loi)
          v_moy_physique = v_min_loi + aux
        else :
          v_moy_physique = la_variable["VALE_MOY_PHY"]
          aux = la_variable["ECART_TYPE_PHY"]/(la_variable["VALE_MOY_PHY"]-la_variable["VALE_MIN"])
          aux1 = 1. + aux*aux
          aux2 = numpy.sqrt(aux1)
          v_moy_loi = numpy.log((la_variable["VALE_MOY_PHY"]-la_variable["VALE_MIN"])/aux2)
          aux2 = numpy.log(aux1)
          sigma_loi = numpy.sqrt(aux2)
#
# 3.1.4. ==> loi normale tronquée : transfert des moyenne, mini/maxi et écart-type
#            on définit une moyennne comme étant la médiane des extremes.
#
      else :
        v_moy_loi = la_variable["VALE_MOY"]
        v_min_loi = la_variable["VALE_MIN"]
        v_max_loi = la_variable["VALE_MAX"]
        sigma_loi = la_variable["ECART_TYPE"]
        v_moy_physique = 0.5 * ( la_variable["VALE_MIN"] + la_variable["VALE_MAX"] )
#
      dico = { }
      dico["v_moy_physique"] = v_moy_physique
      dico["v_moy_loi"] = v_moy_loi
      dico["v_min_loi"] = v_min_loi
      dico["v_max_loi"] = v_max_loi
      dico["sigma_loi"] = sigma_loi
      valeurs_lois[la_variable] = dico
#
#____________________________________________________________________
#
# 4. Création des fichiers de donnees pour le logiciel de fiabilite
#____________________________________________________________________
#
    if ( LOGICIEL == "MEFISTO" ) :
#
# 4.1. ==> MEFISTO
#
      erreur = fiabilite_mefisto.fiabilite_mefisto ( self, Rep_Calc_LOGICIEL_global,
                                                     INFO, VERSION,
                                                     SEUIL, SEUIL_TYPE,
                                                     VARIABLE,
                                                     valeurs_lois,
                                                     **args )
#
# 4.2. ==> Erreur si autre logiciel
#
    else :
#
      self.cr.warn("Logiciel de fiabilité : "+LOGICIEL)
      erreur = 10
#
# 4.3. ==> Arret en cas d'erreur
#
    if erreur :
      break
#
#____________________________________________________________________
#
# 5. Ecriture de la commande d"exécution du logiciel de fiabilité
#
#   Remarque : dans la donnée de la version du logiciel de fiabilité, il faut remplacer
#              le _ de la donnée par un ., qui
#              est interdit dans la syntaxe du langage de commandes ASTER
#   Remarque : il faut remplacer le N majuscule de la donnee par
#              un n minuscule, qui est interdit dans la syntaxe du langage
#              de commandes ASTER
#____________________________________________________________________
#
#
    VERSION = VERSION.replace("_", ".").replace("N", "n")
#
    EXEC_LOGICIEL ( ARGUMENT = (Rep_Calc_LOGICIEL_global, # nom du repertoire
                                LOGICIEL,                 # nom du logiciel de fiabilité
                                VERSION,                  # version du logiciel de fiabilité
                                FIC_RESU_FIABILITE,       # fichier des résultats du logiciel de fiabilité
                               ),
                    LOGICIEL = fiabilite
                   )
#
#--------------------------------------------------------------------
# 6. C'est fini !
#--------------------------------------------------------------------
#
    break
#
# 6.1. ==> Arret en cas d'erreur
#
  if erreur :
    if not messages_erreur.has_key(erreur) :
      erreur = 100
    self.cr.fatal(messages_erreur[erreur])
#
# 6.2. ==> Si tout va bien, on crée une liste de réels pour le retour.
#          Si le fichier n'a pas été rempli, on met une valeur nulle unique.
#
  if os.path.isfile(FIC_RESU_FIABILITE) :
    liste_reel = []
    fic = open(FIC_RESU_FIABILITE, "r")
    tout = fic.readlines()
    fic.close
    for ligne in tout:
      liste_reel.append(float(ligne[:-1]))
  else :
    liste_reel = [0.]
#
  self.DeclareOut("nomres", self.sd)
  nomres = DEFI_LIST_REEL( VALE = liste_reel , INFO = 1 )
#
# 6.3. ==> Menage du répertoire créé pour le calcul fiabiliste
#
  liste = os.listdir(Rep_Calc_LOGICIEL_global)
##  print liste
#
  for nomfic in liste :
    fic_total = os.path.join(Rep_Calc_LOGICIEL_global, nomfic)
#
    if os.path.isdir(fic_total) :
      liste_bis = os.listdir(fic_total)
      for nomfic_bis in liste_bis :
        fic_total_bis = os.path.join(fic_total, nomfic_bis)
        if os.path.islink(fic_total_bis) :
          os.unlink (fic_total_bis)
        else :
          os.chmod  (fic_total_bis, 0755)
          os.remove (fic_total_bis)
      os.rmdir (fic_total)
#
    elif os.path.isfile(fic_total) :
      os.chmod  (fic_total, 0755)
      os.remove (fic_total)
#
  os.rmdir (Rep_Calc_LOGICIEL_global)
#
  return
#
##########################  Fin de la fonction##################################
#
##########################   Auto-test##################################
#
if __name__ == "__main__" :
#
  import os
  import sys
  import tempfile
#
  Rep_Calc_LOGICIEL_global = tempfile.mktemp()
  os.mkdir(Rep_Calc_LOGICIEL_global)
#
  classe = None
  INFO = 2
  LOGICIEL = "MEFISTO"
  VERSION = "V3_2"
  UNITE_ESCL = 38
  MESS_ASTER = "DERNIER"
  SEUIL = 1789.
  SEUIL_TYPE = "MAXIMUM"
  VARIABLE = []
  args = {}
#
  lr8 = macr_fiabilite_ops(classe, INFO,
                       LOGICIEL, VERSION,
                       UNITE_ESCL, MESS_ASTER,
                       SEUIL, SEUIL_TYPE,
                       VARIABLE,
                       **args )
###  print "lr8 = ", lr8
  Liste = os.listdir(Rep_Calc_LOGICIEL_global)
#
  for nomfic in Liste :
    fic_total = os.path.join(Rep_Calc_LOGICIEL_global, nomfic)
    os.chmod  (fic_total, 0755)
    os.remove (fic_total)
  os.rmdir (Rep_Calc_LOGICIEL_global)
#
  sys.exit("blabla")
