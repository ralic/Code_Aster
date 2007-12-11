#@ MODIF creation_donnees_homard Macro  DATE 11/12/2007   AUTEUR GNICOLAS G.NICOLAS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
"""
Cette classe crée le fichier de configuration permettant de lancer HOMARD depuis Code_Aster.
"""
__revision__ = "V1.3"
__all__ = [ ]
 
import os
import os.path
from types import ListType, TupleType
EnumTypes = (ListType, TupleType)

try:
  from Utilitai.Utmess import   UTMESS
except ImportError:
  def UTMESS(code, idmess, valk=(), vali=(), valr=()):
    """Clone de utmess si on ne reussit pas à le charger
    """
    fmt = '\n <%s> <%s> %s %s %s\n\n'
    print fmt % (code, idmess, valk, vali, valr)

# ------------------------------------------------------------------------------
class creation_donnees_homard:
  """Cette classe crée les données permettant de lancer HOMARD depuis Code_Aster.
      Ce sont :
      . le fichier de configuration
      . le fichier des données dans le cas d'information
   
   Arguments (stockés en tant qu'attribut) :
      . nom_macro : nom de la macro-commande qui appelle :
                      'MACR_ADAP_MAIL' pour une adaptation
                      'MACR_INFO_MAIL' pour une information sur un maillage
      . mots_cles : mots-clés de la macro-commande
      . dico_configuration : dictionnaire des options

   Attributs :
      . Nom_Fichier_Configuration : nom du fichier de configuration (immuable)
      . Nom_Fichier_Donnees : nom du fichier de données (immuable)
      . mode_homard : le mode pour filtrer ici ("ADAP" ou "INFO")
      . ModeHOMA : le mode pour HOMARD (1 ou 2)
      . mode_homard_texte : le mode d'utilisation, en francais ("ADAPTATION" ou "INFORMATION")
      . elements_incompatibles : que faire des éléments incompatibles avec HOMARD
   """
# ------------------------------------------------------------------------------
  def __init__(self, nom_macro, mots_cles, dico_configuration ) :
    """Construction + valeurs par défaut des attributs
    """
#
# 1. Vérification de la macro qui appelle
#
    d_aux = {}
    d_aux["MACR_ADAP_MAIL"] = ( "ADAP", 1, "ADAPTATION" )
    d_aux["MACR_INFO_MAIL"] = ( "INFO", 2, "INFORMATION" )
    if d_aux.has_key(nom_macro) :
      self.mode_homard = d_aux[nom_macro][0]
      self.ModeHOMA = d_aux[nom_macro][1]
      self.mode_homard_texte = d_aux[nom_macro][2]
    else :
      UTMESS("F",'HOMARD0_1')
#
# 2. Données générales de cette initialisation
#
#gn    for mot_cle in mots_cles.keys() :
#gn      print "mots_cles[", mot_cle, "] = ", mots_cles[mot_cle]
#gn    for mot_cle in dico_configuration.keys() :
#gn      print "dico_configuration[", mot_cle, "] = ", dico_configuration[mot_cle]
#
    self.nom_macro = nom_macro
    self.mots_cles = mots_cles
    self.dico_configuration = dico_configuration
#
# 3. Quel type de traitement des elemenst incompatibles
#
    if mots_cles.has_key("ELEMENTS_NON_HOMARD") :
      d_aux = {}
      d_aux["REFUSER"] = "TOUS"
      d_aux["IGNORER"] = "IGNORE_QUAD"
      self.elements_incompatibles = d_aux[mots_cles["ELEMENTS_NON_HOMARD"]]
    else :
      self.elements_incompatibles = None
#
# 4. Attributs immuables
#
    self.Nom_Fichier_Configuration = "HOMARD.Configuration"
    self.Nom_Fichier_Donnees = "HOMARD.Donnees"
#
# ------------------------------------------------------------------------------
  def int_to_str2 (self, entier) :
    """
    Transforme un entier compris entre 0 et 99 en une chaine sur deux caractères
    """
#    print "\nArguments a l'entree de", __name__, ":", entier
#
    try:
      la_chaine = '%02d' % entier
    except TypeError:
      la_chaine = None
#
    return la_chaine
# ------------------------------------------------------------------------------
  def quel_mode (self) :
    """Informe sur le mode de lancement de HOMARD
    """
###    print self.mode_homard_texte
    print "Lancement de creation_donnees_homard en mode", self.mode_homard_texte
    return
# ------------------------------------------------------------------------------
  def creation_configuration (self) :
    """Crée les données nécessaires à la configuration
    """
#
    message_erreur = None
#
    while message_erreur is None :
#
#     1. Les chaines liées aux numéros d'itération
#
      if self.mode_homard == "ADAP" :
        niter = self.dico_configuration["niter"]
        self.str_niter = self.int_to_str2 (niter)
        self.str_niterp1 = self.int_to_str2 (niter+1)
        self.niter_vers_niterp1 = self.str_niter + ".vers." + self.str_niterp1
#
#     2. La liste standard
#
      if self.mode_homard == "INFO" :
        aux = "Liste.info"
      else :
        aux = "Liste." + self.niter_vers_niterp1
      self.ListeStd = aux
#
#     3. Le type de bilan : il faut convertir la donnée textuelle en un entier,
#        produit de nombres premiers.
#        Si rien n'est demandé, on met 0.
#
      aux = 1
      dico_aux = {}
      dico_aux["INTERPENETRATION"] = 3
      dico_aux["NOMBRE"] = 7
      dico_aux["QUALITE"] = 5
      dico_aux["CONNEXITE"] = 11
      dico_aux["TAILLE"] = 13
      l_aux = dico_aux.keys()
      for choix in l_aux :
        if self.mots_cles.has_key(choix) :
          if self.mots_cles[choix] == "OUI" :
            aux = aux * dico_aux[choix]
      if aux == 1 :
        aux = 0
      self.TypeBila = aux
#
#     4. Les entrées/sorties au format MED
#
      self.CCNoMN__ = self.dico_configuration["NOM_MED_MAILLAGE_N"]
      if self.mode_homard == "ADAP" :
        self.CCNoMNP1 = self.dico_configuration["NOM_MED_MAILLAGE_NP1"]
        if self.dico_configuration.has_key("NOM_MED_MAILLAGE_NP1_ANNEXE") :
          self.CCMaiAnn = self.dico_configuration["NOM_MED_MAILLAGE_NP1_ANNEXE"]
        else :
          self.CCMaiAnn = None
#
#     5. Les entrées/sorties au format HOMARD
#
      if self.mode_homard == "ADAP" :
        self.fic_homard_niter   = "M_" + self.str_niter   + ".hom"
        self.fic_homard_niterp1 = "M_" + self.str_niterp1 + ".hom"
      else :
        self.fic_homard_niter = None
        self.fic_homard_niterp1 = None
#
#     6.1. Le pilotage de l'adaptation
#
      if self.mode_homard == "ADAP" :
#
#     6.1. Le type d'adaptation
#
        if self.mots_cles["ADAPTATION"] == "RAFFINEMENT" or self.mots_cles["ADAPTATION"] == "RAFFINEMENT_ZONE" :
          self.TypeRaff = "libre"
          self.TypeDera = "non"
        elif self.mots_cles["ADAPTATION"] == "DERAFFINEMENT" :
          self.TypeRaff = "non"
          self.TypeDera = "libre"
        elif self.mots_cles["ADAPTATION"] == "RAFF_DERA" :
          self.TypeRaff = "libre"
          self.TypeDera = "libre"
        elif self.mots_cles["ADAPTATION"] == "RAFFINEMENT_UNIFORME" :
          self.TypeRaff = "uniforme"
          self.TypeDera = "non"
        elif self.mots_cles["ADAPTATION"] == "DERAFFINEMENT_UNIFORME" :
          self.TypeRaff = "non"
          self.TypeDera = "uniforme"
        elif self.mots_cles["ADAPTATION"] == "RIEN" :
          self.TypeRaff = "non"
          self.TypeDera = "non"
#gn        print "... self.TypeRaff = ",self.TypeRaff
#gn        print "... self.TypeDera = ",self.TypeDera
#
#     6.2. L'éventuel seuil de raffinement
#
        if self.TypeRaff == "libre" and self.mots_cles["ADAPTATION"] != "RAFFINEMENT_ZONE" :
          d_aux = {}
          d_aux["CRIT_RAFF_ABS"] = ("SeuilHau",   1)
          d_aux["CRIT_RAFF_REL"] = ("SeuilHRe", 100)
          d_aux["CRIT_RAFF_PE" ] = ("SeuilHPE", 100)
          l_aux = d_aux.keys()
          for mot_cle in l_aux :
            if self.mots_cles[mot_cle] is not None :
              aux = self.mots_cles[mot_cle]*d_aux[mot_cle][1]
              self.critere_raffinement = (d_aux[mot_cle][0], aux)
        else :
          self.critere_raffinement = None
#gn          print "... self.critere_raffinement = ", self.critere_raffinement
#
#       6.3. L'éventuel seuil de déraffinement
#
        if self.TypeDera == "libre" :
          d_aux = {}
          d_aux["CRIT_DERA_ABS"] = ("SeuilBas",   1)
          d_aux["CRIT_DERA_REL"] = ("SeuilBRe", 100)
          d_aux["CRIT_DERA_PE" ] = ("SeuilBPE", 100)
          l_aux = d_aux.keys()
          for mot_cle in l_aux :
            if self.mots_cles[mot_cle] is not None :
              aux = self.mots_cles[mot_cle]*d_aux[mot_cle][1]
              self.critere_deraffinement = (d_aux[mot_cle][0], aux)
        else :
          self.critere_deraffinement = None
#gn          print "... self.critere_deraffinement = ", self.critere_deraffinement
#
#       6.4. Les niveaux extremes
#
        self.niveau = []
        for mot_cle in [ "NIVE_MIN", "NIVE_MAX" ] :
          if self.mots_cles.has_key(mot_cle) :
            if self.mots_cles[mot_cle] is not None :
              if mot_cle == "NIVE_MIN" :
                aux = "NiveauMi"
              else :
                aux = "NiveauMa"
              self.niveau.append((aux, self.mots_cles[mot_cle]))
        if len(self.niveau) == 2 :
#gn          print self.mots_cles["NIVE_MIN"]
#gn          print self.mots_cles["NIVE_MAX"]
          if self.mots_cles["NIVE_MIN"] >= self.mots_cles["NIVE_MAX"] :
            message_erreur = "Le niveau mini ,"+str(self.mots_cles["NIVE_MIN"])+\
                             ", doit etre < au niveau maxi, "+str(self.mots_cles["NIVE_MAX"])+"."
            break
#
#       6.5. Les éventuelles zones de raffinement
#
        if self.dico_configuration.has_key("Zones") :
          iaux = 0
          for zone in self.dico_configuration["Zones"] :
            iaux = iaux + 1
            s_aux_1 = "Zone numero "+str(iaux)+" : "
            s_aux_2 = ", doit etre < au "
            if zone.has_key("X_MINI") :
              if zone["X_MINI"] > zone["X_MAXI"] :
                message_erreur = s_aux_1+"X mini ,"+str(zone["X_MINI"])+s_aux_2+"X maxi, "+str(zone["X_MAXI"])+"."
              if zone["Y_MINI"] > zone["Y_MAXI"] :
                message_erreur = s_aux_1+"Y mini ,"+str(zone["Y_MINI"])+s_aux_2+"Y maxi, "+str(zone["Y_MAXI"])+"."
            if zone.has_key("Z_MINI") :
              if zone["Z_MINI"] > zone["Z_MAXI"] :
                message_erreur = s_aux_1+"Z mini ,"+str(zone["Z_MINI"])+s_aux_2+"Z maxi, "+str(zone["Z_MAXI"])+"."
#
      break
#
    if message_erreur is not None :
      UTMESS("F",'HOMARD0_2',valk=message_erreur)
#
    return self.fic_homard_niter, self.fic_homard_niterp1
# ------------------------------------------------------------------------------
  def ouvre_fichier (self, nomfic_local) :
    """Ouvre en écriture un fichier après l'avoir éventuellement détruit
    """
    codret_partiel = [0]
###    print nomfic_local
    Rep_Calc_HOMARD_global = self.dico_configuration["Rep_Calc_HOMARD_global"]
    nomfic = os.path.join ( Rep_Calc_HOMARD_global , nomfic_local )
#
    if os.path.isfile (nomfic) :
      try :
        os.remove (nomfic)
      except os.error, codret_partiel :
        print "Probleme au remove, erreur numéro ", codret_partiel[0], ":", codret_partiel[1]
        UTMESS("F",'HOMARD0_3',valk=nomfic)
#
    fichier = open (nomfic,"w")
    self.fichier = fichier
#
    return fichier, nomfic
  def ecrire_ligne_configuration_0 (self, commentaire) :
    """Ecrit une ligne de commentaires du fichier de configuration
   Arguments :
      . commentaire : le commentaire à écrire
    """
#
    ligne = "#\n"
    ligne = ligne + "# " + commentaire + "\n"
    ligne = ligne + "#\n"
    self.fichier.write(ligne)
#
    return
# ------------------------------------------------------------------------------
  def ecrire_ligne_configuration_2 (self, motcle, valeur) :
    """Ecrit une ligne du fichier de configuration dans le cas : motcle + valeur
   Arguments :
      . motcle : le mot-clé HOMARD à écrire
      . valeur : la valeur associée
    """
#
    ligne = motcle + " " + str(valeur) + "\n"
    self.fichier.write(ligne)
#
    return
# ------------------------------------------------------------------------------
  def ecrire_ligne_configuration_3 (self, motcle, valeur1, valeur2) :
    """Ecrit une ligne du fichier de configuration dans le cas : motcle + valeur1 + valeur2
   Arguments :
      . motcle : le mot-clé HOMARD à écrire
      . valeur : la valeur associée
    """
#
    ligne = motcle + " " + str(valeur1) + " " + str(valeur2) + "\n"
    self.fichier.write(ligne)
#
    return
# ------------------------------------------------------------------------------
  def ecrire_fichier_configuration (self) :
    """Ecrit le fichier de configuration
    """
    message_erreur = None
#
    while message_erreur is None :
#
#     1. Ouverture du fichier
#
      fichier, nomfic_global = self.ouvre_fichier(self.Nom_Fichier_Configuration)
#
#     2. Généralités
#
      self.ecrire_ligne_configuration_0("Generalites")
      self.ecrire_ligne_configuration_2("ModeHOMA", self.ModeHOMA)
      self.ecrire_ligne_configuration_2("ListeStd", self.ListeStd)
      self.ecrire_ligne_configuration_2("TypeBila", self.TypeBila)
      self.ecrire_ligne_configuration_2("CCAssoci", "MED")
      self.ecrire_ligne_configuration_2("NumeIter", self.dico_configuration["niter"])
#
#     3. Les fichiers externes
#
      self.ecrire_ligne_configuration_0("Les fichiers au format MED")
      self.ecrire_ligne_configuration_2("CCNoMN__", self.CCNoMN__)
      self.ecrire_ligne_configuration_2("CCMaiN__", self.dico_configuration["Fichier_ASTER_vers_HOMARD"])
      if self.mode_homard == "ADAP" :
        self.ecrire_ligne_configuration_2("CCNoMNP1", self.CCNoMNP1)
        self.ecrire_ligne_configuration_2("CCMaiNP1", self.dico_configuration["Fichier_HOMARD_vers_ASTER"])
#
      self.ecrire_ligne_configuration_0("Le répertoire des fichiers de bilan")
      self.ecrire_ligne_configuration_2("RepeInfo", self.dico_configuration["Rep_Calc_HOMARD_global"])
#
#     4. Les fichiers HOMARD
#
      self.ecrire_ligne_configuration_0("Les fichiers au format HOMARD")
      if self.mode_homard == "ADAP" :
        self.ecrire_ligne_configuration_3("HOMaiN__", "M_"+self.str_niter  , self.fic_homard_niter )
        self.ecrire_ligne_configuration_3("HOMaiNP1", "M_"+self.str_niterp1, self.fic_homard_niterp1)
        aux = "oui"
      else :
        aux = "non"
      self.ecrire_ligne_configuration_2("EcriFiHO", aux)
#
#     5. Le pilotage de l'adaptation
#
      if self.mode_homard == "ADAP" :
        self.ecrire_ligne_configuration_0("Le pilotage de l'adaptation")
#
#     5.1. Type d'adaptation
#
        self.ecrire_ligne_configuration_2("TypeRaff", self.TypeRaff)
        if self.critere_raffinement is not None :
          self.ecrire_ligne_configuration_2(self.critere_raffinement[0], self.critere_raffinement[1])
        self.ecrire_ligne_configuration_2("TypeDera", self.TypeDera)
        if self.critere_deraffinement is not None :
          self.ecrire_ligne_configuration_2(self.critere_deraffinement[0], self.critere_deraffinement[1])
#
#     5.2. L'eventuel indicateur d'erreur
#
        if self.dico_configuration.has_key("Indicateur") :
#
          self.ecrire_ligne_configuration_0("L'indicateur d'erreur")
          self.ecrire_ligne_configuration_2("CCIndica", self.dico_configuration["Fichier_ASTER_vers_HOMARD"])
          self.ecrire_ligne_configuration_2("CCNoChaI", self.dico_configuration["Indicateur"]["NOM_MED"])
          self.ecrire_ligne_configuration_2("CCCoChaI", self.dico_configuration["Indicateur"]["COMPOSANTE"])
          if self.dico_configuration["Indicateur"].has_key("NUME_ORDRE") :
            self.ecrire_ligne_configuration_2("CCNumOrI", self.dico_configuration["Indicateur"]["NUME_ORDRE"])
            self.ecrire_ligne_configuration_2("CCNumPTI", self.dico_configuration["Indicateur"]["NUME_ORDRE"])
          if self.mots_cles.has_key("TYPE_VALEUR_INDICA") :
            self.ecrire_ligne_configuration_2("CCTyVaIn", self.mots_cles["TYPE_VALEUR_INDICA"])
#
#     5.3. Les éventuelles zones de raffinement
#
        if self.dico_configuration.has_key("Zones") :
          iaux = 0
          for zone in self.dico_configuration["Zones"] :
            iaux = iaux + 1
            self.ecrire_ligne_configuration_0("Zone de raffinement numéro "+str(iaux))
            if zone.has_key("X_MINI") :
              self.ecrire_ligne_configuration_3("ZoRaXmin", iaux, zone["X_MINI"])
              self.ecrire_ligne_configuration_3("ZoRaXmax", iaux, zone["X_MAXI"])
              self.ecrire_ligne_configuration_3("ZoRaYmin", iaux, zone["Y_MINI"])
              self.ecrire_ligne_configuration_3("ZoRaYmax", iaux, zone["Y_MAXI"])
            if zone.has_key("Z_MINI") :
              self.ecrire_ligne_configuration_3("ZoRaZmin", iaux, zone["Z_MINI"])
              self.ecrire_ligne_configuration_3("ZoRaZmax", iaux, zone["Z_MAXI"])
            if zone.has_key("X_CENTRE") :
              self.ecrire_ligne_configuration_3("ZoRaXCen", iaux, zone["X_CENTRE"])
              self.ecrire_ligne_configuration_3("ZoRaYCen", iaux, zone["Y_CENTRE"])
              self.ecrire_ligne_configuration_3("ZoRaRayo", iaux, zone["RAYON"])
            if zone.has_key("Z_CENTRE") :
              self.ecrire_ligne_configuration_3("ZoRaZCen", iaux, zone["Z_CENTRE"])
#
#     5.4. Les niveaux extremes
#
        for aux in self.niveau :
          self.ecrire_ligne_configuration_2(aux[0], aux[1])
#
#     5.5. L'usage de l'indicateur
#
        if self.mots_cles.has_key("TYPE_OPER_INDICA") :
          if self.mots_cles["TYPE_OPER_INDICA"] is not None :
            self.ecrire_ligne_configuration_2("CCModeFI", self.mots_cles["TYPE_OPER_INDICA"])
#
#     5.6. Les éventuels groupes de filtrage du raffinement/deraffinement
#
        for cle in ( "GROUP_MA", "GROUP_NO" ) :
          if self.mots_cles.has_key(cle) :
            if self.mots_cles[cle] is not None :
              if not type(self.mots_cles[cle]) in EnumTypes :
                self.ecrire_ligne_configuration_2("CCGroAda", self.mots_cles[cle])
              else :
                for group in self.mots_cles[cle] :
                  self.ecrire_ligne_configuration_2("CCGroAda", group)
#
#     6. Les éventuels champs à mettre à jour
#
      if self.dico_configuration.has_key("Champs") :
        self.ecrire_ligne_configuration_0("Champs à mettre à jour")
        self.ecrire_ligne_configuration_2("CCSolN__", self.dico_configuration["Fichier_ASTER_vers_HOMARD"])
        self.ecrire_ligne_configuration_2("CCSolNP1", self.dico_configuration["Fichier_HOMARD_vers_ASTER"])
        iaux = 0
        for maj_champ in self.dico_configuration["Champs"] :
          iaux = iaux + 1
          self.ecrire_ligne_configuration_0("Mise à jour du champ numéro "+str(iaux))
          self.ecrire_ligne_configuration_3("CCChaNom", iaux, maj_champ["NOM_MED"])
          if maj_champ.has_key("NUME_ORDRE") :
            self.ecrire_ligne_configuration_3("CCChaNuO", iaux, maj_champ["NUME_ORDRE"])
            self.ecrire_ligne_configuration_3("CCChaPdT", iaux, maj_champ["NUME_ORDRE"])
          elif maj_champ.has_key("INST") :
            self.ecrire_ligne_configuration_3("CCChaIns", iaux, maj_champ["INST"])
#
#     7. L'éventuel maillage de frontière
#
      if self.dico_configuration.has_key("NOM_MED_MAILLAGE_FRONTIERE") :
        self.ecrire_ligne_configuration_0("Maillage de frontière")
        self.ecrire_ligne_configuration_2("SuivFron", "oui")
        self.ecrire_ligne_configuration_2("CCFronti", self.dico_configuration["Fichier_ASTER_vers_HOMARD"])
        self.ecrire_ligne_configuration_2("CCNoMFro", self.dico_configuration["NOM_MED_MAILLAGE_FRONTIERE"])
        if self.mots_cles.has_key("GROUP_MA_FRONT") :
          if self.mots_cles["GROUP_MA_FRONT"] is not None :
            if not type(self.mots_cles["GROUP_MA_FRONT"]) in EnumTypes :
              self.ecrire_ligne_configuration_2("CCGroFro", self.mots_cles["GROUP_MA_FRONT"])
            else :
              for group_ma in self.mots_cles["GROUP_MA_FRONT"] :
                self.ecrire_ligne_configuration_2("CCGroFro", group_ma)
#
#     8. L'éventuel maillage annexe
#
      if self.mode_homard == "ADAP" :
        if self.CCMaiAnn is not None :
          self.ecrire_ligne_configuration_0("Maillage d'autre degré")
          self.ecrire_ligne_configuration_2("ModDegre", "oui")
          self.ecrire_ligne_configuration_2("CCNoMAnn", self.CCMaiAnn)
          self.ecrire_ligne_configuration_2("CCMaiAnn", self.dico_configuration["Fichier_HOMARD_vers_ASTER"])
#
#     9. Options particulières
#
      self.ecrire_ligne_configuration_0("Autres options")
      if self.mots_cles.has_key("LANGUE") :
        self.ecrire_ligne_configuration_2("Langue", self.mots_cles["LANGUE"])
      self.ecrire_ligne_configuration_2("MessInfo", self.dico_configuration["INFO"])
      if self.dico_configuration["version_perso"] :
        VERSION_HOMARD = self.dico_configuration["VERSION_HOMARD"]
        self.ecrire_ligne_configuration_2("DicoOSGM", "$HOMARD_USER/"+VERSION_HOMARD+"/CONFIG/typobj.stu")
#
#     10. L'usage des éléments incompatibles avec HOMARD
#
      if self.elements_incompatibles is not None :
        self.ecrire_ligne_configuration_0("Les éléments incompatibles avec HOMARD")
        self.ecrire_ligne_configuration_2("TypeElem", self.elements_incompatibles)
#
#     11. Fermeture du fichier
#
      fichier.close()
      break
#
    if message_erreur is not None :
      message_erreur = "Ecriture de "+nomfic_global+". "+message_erreur
      UTMESS("F",'HOMARD0_2',valk=message_erreur)
#
    return
# ------------------------------------------------------------------------------
  def ecrire_fichier_donnees (self) :
    """Ecrit le fichier des donnees dans le cas d'une demande d'information
    """
    message_erreur = None
#
    while message_erreur is None :
#
#     1. Ouverture du fichier
#
      fichier, nomfic_global = self.ouvre_fichier(self.Nom_Fichier_Donnees)
#
#     2. On ne demande rien pour le moment
#
      fichier.write("0\n")
      fichier.write("0\n")
      fichier.write("0\n")
      fichier.write("q\n")
#
#     n. Fermeture du fichier
#
      fichier.close()
      break
#
    if message_erreur is not None :
      UTMESS("F",'HOMARD0_2',valk=message_erreur)
#
    return nomfic_global
