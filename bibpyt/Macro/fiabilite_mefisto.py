#@ MODIF fiabilite_mefisto Macro  DATE 17/07/2007   AUTEUR REZETTE C.REZETTE 
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
#
def fiabilite_mefisto ( self, Rep_Calc_LOGICIEL_global,
                        INFO, VERSION,
                        SEUIL, SEUIL_TYPE,
                        VARIABLE,
                        valeurs_lois,
                        **args ) :
#
#    valeurs_lois est un dictionnaire indexé sur les variables.
#    Chaque case, valeurs_lois[m], est un dictionnaire contenant :
#    d["v_moy_physique"] = valeur moyenne physique
#    d["v_moy_loi"] = valeur moyenne de la loi
#    d["v_min_loi"] = valeur minimale de la loi
#    d["v_max_loi"] = valeur maximale de la loi
#    d["sigma_loi"] = ecart type de la loi
#
#    args est le dictionnaire des arguments optionnels
#    args.keys() est la liste des mots-clés
#    args.keys()[0] est la premiere valeur de cette liste
#    args.keys()[1:] est la liste des valeurs suivantes dans cette liste
#    args.keys(mot_cle) représente le contenu de la variable mot_cle dans la macro appelante.
#
  """ Ecriture des données spécifiques à MEFISTO. """
#
  from Macro import fiabilite_fichier
  import os
  import string
  import Numeric
  from Utilitai.Utmess import U2MESS as UTMESS
#
#____________________________________________________________________
#
# 1. Préalables
#____________________________________________________________________
#
#
  messages_erreur = { 0 : "Tout va bien",
                      1 : "Fichier inconnu.",
                      2 : "Problème d'ouverture de fichier.",
                     10 : "Problème d'ouverture de fichier.",
                     11 : "Problème de fermeture de fichier.",
                     20 : "Problème d'impression de fichier.",
                     50 : "Donnée inacceptable.",
                    100 : "Erreur." }
#
  trad_oui_non = { "OUI" : 1,
                   "NON" : 0 }
#
  erreur = 0
#
  while not erreur :
#
#____________________________________________________________________
#
# 2. Les fichiers pour le logiciel de fiabilité
#    Ils sont créés dans le répertoire d'exécution du logiciel de fiabilité, avec leurs noms officiels
#____________________________________________________________________
#
#
    fic_dataMenu  = "dataMenu"
    fic_dataStoch = "dataStoch"
    fic_dataNum   = "dataNum"
    fic_dataGrad  = "dataGrad"
#
#____________________________________________________________________
#
# 3. Construction du fichier 'dataMenu'
#____________________________________________________________________
#
# 3.1 ==> Ouverture du fichier
#
    f_menu = fiabilite_fichier.fiabilite_fichier ( self, Rep_Calc_LOGICIEL_global, fic_dataMenu, INFO )
    erreur = f_menu.Ouvre_Fichier ( "w" )
    if erreur :
      break
#
# 3.2 ==> Ecriture des données nécessaires
#
    f_menu.Ecrit_Titre ("MENU DU PROGRAMME MEFISTO")
    f_menu.Ecrit_Titre ("1 <=> OUI et 0 <=> NON (entiers)")
#
    f_menu.Ecrit_Titre ("Recherche du point de conception")
    aux = trad_oui_non[args["RECH_PT_CONCEPT"]]
    f_menu.Ecrit_Valeurs (aux)
#
    f_menu.Ecrit_Titre ("First Order Reliability Analyses")
    aux = trad_oui_non[args["METHODE_FORM"]]
    f_menu.Ecrit_Valeurs (aux)
#
    f_menu.Ecrit_Titre ("Second Order Reliability Analyses")
    aux = trad_oui_non[args["METHODE_SORM"]]
    f_menu.Ecrit_Valeurs (aux)
#
    f_menu.Ecrit_Titre ("Importance Sampling Analyses")
    aux = trad_oui_non[args["TIRAGE_IMPORTANCE"]]
    f_menu.Ecrit_Valeurs (aux)
#
    f_menu.Ecrit_Titre ("Optimality Test (1) : Hessian Test")
    aux = trad_oui_non[args["T_HESSIEN"]]
    f_menu.Ecrit_Valeurs (aux)
#
    f_menu.Ecrit_Titre ("Optimality Test (2) : Sphere Test")
    aux = trad_oui_non[args["T_SPHERE"]]
    f_menu.Ecrit_Valeurs (aux)
#
    f_menu.Ecrit_Titre ("Optimality Test (3) : Strong Max Test")
    aux = trad_oui_non[args["T_MAXIMUM_FORT"]]
    f_menu.Ecrit_Valeurs (aux)
#
    f_menu.Ecrit_Titre ("Experiment Plan")
    aux = trad_oui_non[args["PLAN_EXPERIENCE"]]
    f_menu.Ecrit_Valeurs (aux)
#
    f_menu.Ecrit_Titre ("Polynomial Taylor Approximation (order 2)")
    aux = trad_oui_non[args["POLYNOME_TAYLOR"]]
    f_menu.Ecrit_Valeurs (aux)
#
# 3.3 ==> Fermeture du fichier
#
    erreur = f_menu.Ferme_Fichier ( )
    if erreur :
      break
#
    if INFO >= 2 :
      erreur = f_menu.Imprime_Fichier ( )
      if erreur :
        break
#
#____________________________________________________________________
#
# 4. Construction du fichier "dataStoch"
#____________________________________________________________________
#
# 4.1 ==> Ouverture du fichier
#
    f_stoch = fiabilite_fichier.fiabilite_fichier ( self, Rep_Calc_LOGICIEL_global, fic_dataStoch, INFO )
    erreur = f_stoch.Ouvre_Fichier ( "w" )
    if erreur :
      break
#
# 4.2 ==> Nombre d'occurence de VARIABLE
#
    nb_occu_variable = len(VARIABLE)
#
# 4.3 ==> Ecriture des données nécessaires
#
    f_stoch.Ecrit_Titre ("Code name")
    aux=string.replace(VERSION,"_",".")
    aux=string.replace(aux,"N","n")
    aux=string.replace(aux,"V","v")
    f_stoch.Ecrit_Valeurs ( "aster_" + aux )
#
    aux = [ "Gradients evaluated by the code" ]
    aux.append("1 : Au moins 1 ; 0 : aucun")
    f_stoch.Ecrit_Titre (aux)
    gradient = 0
    for m in VARIABLE :
      if m["GRADIENT"] == "OUI" : gradient = 1
    f_stoch.Ecrit_Valeurs (gradient)
#
    f_stoch.Ecrit_Titre ("Variates number")
    f_stoch.Ecrit_Valeurs ( nb_occu_variable )
#
    aux = [ "Stochastic Variates" ]
    aux.append("1: Uniforme (min, max)")
    aux.append("2: Normal (mean, std dev)")
    aux.append("3: LogNormal (mean, std dev, min)")
    aux.append("4: Normal Truncated (mean, std dev, min, max)")
    f_stoch.Ecrit_Titre (aux)
#
    for m in VARIABLE :
#
      d = valeurs_lois[m]
      if m["LOI"] == "UNIFORME" :
        f_stoch.Ecrit_Valeurs ( [ m["NOM"],  1 , d["v_min_loi"] , d["v_max_loi"] ] )
      elif m["LOI"] == "NORMALE" :
        f_stoch.Ecrit_Valeurs ( [ m["NOM"],  2 , d["v_moy_loi"] , d["sigma_loi"] ] )
      elif m["LOI"] == "LOGNORMALE" :
        f_stoch.Ecrit_Valeurs ( [ m["NOM"],  3 , d["v_moy_loi"] , d["sigma_loi"] , d["v_min_loi"] ] )
      elif m["LOI"] == "NORMALE_TRONQUEE" :
        f_stoch.Ecrit_Valeurs ( [ m["NOM"],  4 , d["v_moy_loi"] , d["sigma_loi"] , d["v_min_loi"] , d["v_max_loi"] ] )
      else :
        erreur = 50
#
    if erreur :
      break
#
    f_stoch.Ecrit_Titre ("Initial Points")
    for m in VARIABLE :
      if m["POINT_INI"] is None :
        aux = valeurs_lois[m]["v_moy_physique"]
      else :
        aux = m["POINT_INI"]
      f_stoch.Ecrit_Valeurs ( aux )
#
    f_stoch.Ecrit_Titre ("Reference Points")
    for m in VARIABLE :
      if m["POINT_REF"] is None :
        aux = valeurs_lois[m]["v_moy_physique"]
      else :
        aux = m["POINT_REF"]
      f_stoch.Ecrit_Valeurs ( aux )
#
    f_stoch.Ecrit_Titre ("Design Points")
    for m in VARIABLE :
      if args["RECH_PT_CONCEPT"] == "OUI" :
        aux = 1792.
      elif m["POINT_CONCEPT"] is None :
        aux = valeurs_lois[m]["v_moy_physique"]
      else :
        aux = m["POINT_CONCEPT"]
      f_stoch.Ecrit_Valeurs ( aux )
#
    f_stoch.Ecrit_Titre ("Correlation matrix fictive")
#    if args.has_key('MATRICE'):
    if args["MATRICE"] != None:
      if len(args["MATRICE"]) != nb_occu_variable**2:
#        +' DU MOT CLE MATRICE DOIT ETRE EGAL A : '
#        +str(nb_occu_variable**2))
        UTMESS('F','FIABILITE0_1',vali=nb_occu_variable**2)
      for m in range(nb_occu_variable) :
        aux = [ ]
        for n in range(nb_occu_variable) :
          aux.append(args["MATRICE"][n + m*nb_occu_variable])
        f_stoch.Ecrit_Valeurs ( aux )
    else:
        aux=Numeric.identity(nb_occu_variable)
        aux=Numeric.concatenate(aux)
        aux=aux.tolist()
        f_stoch.Ecrit_Valeurs ( aux )
#
    f_stoch.Ecrit_Titre ("Parameter threshold value")
    if SEUIL_TYPE == "MAXIMUM" :
      aux = SEUIL
    else :
      aux = -SEUIL
    f_stoch.Ecrit_Valeurs ( aux )
#
# 4.4 ==> Fermeture du fichier
#
    erreur = f_stoch.Ferme_Fichier ( )
    if erreur :
      break
#
    if INFO >= 2 :
      erreur = f_stoch.Imprime_Fichier ( )
      if erreur :
        break
#
#____________________________________________________________________
#
# 5. Construction du fichier 'dataNum'
#____________________________________________________________________
#
# 5.1 ==> Ouverture du fichier
#
    f_num = fiabilite_fichier.fiabilite_fichier ( self, Rep_Calc_LOGICIEL_global, fic_dataNum, INFO )
    erreur = f_num.Ouvre_Fichier ( "w" )
    if erreur :
      break
#
# 5.2 ==> Ecriture des données nécessaires
#
    f_num.Ecrit_Titre ("Parameters : EpsU, EpsG, Tau, Omega, iterMax")
    if args["RECH_PT_CONCEPT"] == "OUI" :
      f_num.Ecrit_Valeurs (args["EPSILON_U"])
      f_num.Ecrit_Valeurs (args["EPSILON_G"])
      f_num.Ecrit_Valeurs (args["TAU"])
      f_num.Ecrit_Valeurs (args["OMEGA"])
      f_num.Ecrit_Valeurs (args["ITER_MAX"])
    else :
      aux = 0.1848
      for k in range(5) :
        f_num.Ecrit_Valeurs (aux)
#
    f_num.Ecrit_Titre ("Parameters : hgrad, hhess")
    f_num.Ecrit_Valeurs (args["HGRAD"])
    f_num.Ecrit_Valeurs (args["HHESS"])
#
    aux = [ "Parameter Optimality Test(sphere)" ]
    aux.append("1: Parametric Method (Point Number in each direction)")
    aux.append("2: Gaussian Method (Total Point Number)")
    aux.append("3: Rejection Method (Total Point Number)")
    f_num.Ecrit_Titre (aux)
#
    if args["T_SPHERE"] == "OUI" :
#
      if args["METHODE_TEST"] == "PARAMETRIQUE" :
        aux1 = 1
      elif args["METHODE_TEST"] == "GAUSSIENNE" :
        aux1 = 2
      elif args["METHODE_TEST"] == "REJECTION" :
        aux1 = 3
      else :
        self.cr.warn("METHODE DE TEST : "+args["METHODE_TEST"])
        erreur = 50
        break
#
      aux2 = args["NB_POINT"]
#
    else :
#
#     remarque : il faut mettre une valeur plausible en aux1, sinon plantage violent ...
      aux1 = 1
      aux2 = 1789
#
    f_num.Ecrit_Valeurs ( [ aux1 , aux2 ] )
#
    aux = [ "Parameters : alpha, beta" ]
    aux.append("alpha: common net")
    aux.append("beta: extreme net")
    f_num.Ecrit_Titre (aux)
    if args["PLAN_EXPERIENCE"] == "OUI" :
      aux1 = args["ALPHA"]
      aux2 = args["BETA"]
    else :
      aux1 = 1789.0
      aux2 = 1789.0
    f_num.Ecrit_Valeurs ( aux1 )
    f_num.Ecrit_Valeurs ( aux2 )
#
    f_num.Ecrit_Titre ("Parameters Strong Max Test : cosLim, dProb")
    if args["T_MAXIMUM_FORT"] == "OUI" :
      aux1 = args["COS_LIM"]
      aux2 = args["DPROB"]
    else :
      aux1 = 0.1789
      aux2 = 0.1789
    f_num.Ecrit_Valeurs ( aux1 )
    f_num.Ecrit_Valeurs ( aux2 )
#
    f_num.Ecrit_Titre ("Parameter Importance Samplings : Simulation Number")
    if args["TIRAGE_IMPORTANCE"] == "OUI" :
      aux = args["NB_SIMULATION"]
    else :
      aux = 1945
    f_num.Ecrit_Valeurs ( aux )
#
# 5.3 ==> Fermeture du fichier
#
    erreur = f_num.Ferme_Fichier ( )
    if erreur :
      break
#
    if INFO >= 2 :
      erreur = f_num.Imprime_Fichier ( )
      if erreur :
        break
#
#____________________________________________________________________
#
# 6. Construction du fichier 'dataGrad'
#____________________________________________________________________
#
# 6.1 ==> Création du fichier
#
    f_grad = fiabilite_fichier.fiabilite_fichier ( self, Rep_Calc_LOGICIEL_global, fic_dataGrad, INFO )
    erreur = f_grad.Ouvre_Fichier ( "w" )
    if erreur :
      break
#
# 6.2 ==> Ecriture des données nécessaires
#
    f_grad.Ecrit_Titre ("Commentaires")
#
    for m in VARIABLE :
      f_grad.Ecrit_Commentaires (m["NOM"])
      if m["GRADIENT"] == "OUI" :
        gradient = 1
        increment = 0.0
      else :
        gradient = 0
        increment = m["INCREMENT"]
      aux = [gradient,increment]
      f_grad.Ecrit_Valeurs (aux)
#
# 6.3 ==> Fermeture du fichier
#
    erreur = f_grad.Ferme_Fichier ( )
    if erreur :
      break
#
    if INFO >= 2 :
      erreur = f_grad.Imprime_Fichier ( )
      if erreur :
        break
#
#____________________________________________________________________
#
# 7. C'est fini !
#____________________________________________________________________
#
    break
#
  if erreur :
    if not messages_erreur.has_key(erreur) :
      erreur = 100
    self.cr.warn(messages_erreur[erreur])
    erreur = 11
#
  return erreur
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
  INFO = 2
  VERSION = "V3_2"
  SEUIL = 1789.
  SEUIL_TYPE = "MAXIMUM"
  VARIABLE = []
  args = {}
  valeurs = {}
#
  erreur = fiabilite_mefisto ( None, Rep_Calc_LOGICIEL_global,
                        INFO, VERSION,
                        SEUIL, SEUIL_TYPE,
                        VARIABLE,
                        valeurs,
                        **args )
###  print "Erreur = ", erreur
  Liste = os.listdir(Rep_Calc_LOGICIEL_global)
#
  for nomfic in Liste :
    fic_total = os.path.join(Rep_Calc_LOGICIEL_global,nomfic)
    os.chmod  (fic_total,0755)
    os.remove (fic_total)
  os.rmdir (Rep_Calc_LOGICIEL_global)
#
  sys.exit("blabla")
