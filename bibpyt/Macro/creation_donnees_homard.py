#@ MODIF creation_donnees_homard Macro  DATE 31/01/2011   AUTEUR NICOLAS G.NICOLAS 
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
"""
Cette classe crée le fichier de configuration permettant de lancer HOMARD
depuis Code_Aster.
"""
__revision__ = "V1.9"
__all__ = [ ]

import os
import os.path
from types import ListType, TupleType
EnumTypes = (ListType, TupleType)

try:
  from Utilitai.Utmess import   UTMESS
except ImportError:
  def UTMESS(code, idmess, valk=(), vali=(), valr=()):
    """Clone de utmess si on ne reussit pas e le charger
    """
    fmt = '\n <%s> <%s> %s %s %s\n\n'
    print fmt % (code, idmess, valk, vali, valr)

# ------------------------------------------------------------------------------
class creation_donnees_homard:
  """Cette classe crée les données permettant de lancer HOMARD depuis Code_Aster.
      Ce sont :
      . le fichier de configuration
      . le fichier des données dans le cas d'information

   Arguments (stockes en tant qu'attribut) :
      . nom_macro : nom de la macro-commande qui appelle :
                      'MACR_ADAP_MAIL' pour une adaptation
                      'MACR_INFO_MAIL' pour une information sur un maillage
      . mots_cles : mots-cles de la macro-commande
      . dico_configuration : dictionnaire des options

   Attributs :
      . Nom_Fichier_Configuration : nom du fichier de configuration (immuable)
      . Nom_Fichier_Donnees : nom du fichier de données (immuable)
      . mode_homard : le mode pour filtrer ici ("ADAP" ou "INFO")
      . mode_homard_texte : le mode d'utilisation, en francais ("ADAPTATION" ou "INFORMATION")
      . mailles_incompatibles : que faire des mailles incompatibles avec HOMARD
   """
# ------------------------------------------------------------------------------
  def __init__(self, nom_macro, mots_cles, dico_configuration ) :
    """Construction + valeurs par defaut des attributs
    """
#
# 1. Verification de la macro qui appelle
#
    d_aux = {}
    d_aux["MACR_ADAP_MAIL"] = ( "ADAP", "ADAPTATION" )
    d_aux["MACR_INFO_MAIL"] = ( "INFO", "INFORMATION" )
    if d_aux.has_key(nom_macro) :
      self.mode_homard = d_aux[nom_macro][0]
      self.mode_homard_texte = d_aux[nom_macro][1]
    else :
      UTMESS("F", 'HOMARD0_1')
#
# 2. Données generales de cette initialisation
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
# 3. Quel type de traitement des mailles incompatibles
#
    if mots_cles.has_key("ELEMENTS_NON_HOMARD") :
      d_aux = {}
      d_aux["REFUSER"] = "TOUS"
      d_aux["IGNORER"] = "IGNORE_QUAD"
      self.mailles_incompatibles = d_aux[mots_cles["ELEMENTS_NON_HOMARD"]]
    else :
      self.mailles_incompatibles = None
#
# 4. Attributs immuables
#
    self.Nom_Fichier_Configuration = "HOMARD.Configuration"
    self.Nom_Fichier_Donnees = "HOMARD.Donnees"
#
# ------------------------------------------------------------------------------
  def int_to_str2 (self, entier) :
    """
    Transforme un entier positif en une chaine d'au moins deux caracteres
    """
#    print "\nArguments à l'entree de", __name__, ":", entier
#
    if type(entier) == type(0) :
      la_chaine = '%02d' % entier
    else :
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
    """Cree les données necessaires à la configuration
    """
#
    message_erreur = None
#
    while message_erreur is None :
#
#     1. Les chaines liées aux numeros d'iteration
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
#     3. Le mode de pilotage
#
      if self.mode_homard == "INFO" :
        self.ModeHOMA = 2
        self.Action = "info"
      else :
        if self.mots_cles["ADAPTATION"] == "MODIFICATION" :
          self.ModeHOMA = 3
          self.Action = "modi"
          self.ModDegre = "non"
          self.CreJoint = "non"
        else :
          self.ModeHOMA = 1
          self.Action = "homa"
        self.TypeRaff = "non"
        self.TypeDera = "non"
        self.critere_raffinement = None
        self.critere_deraffinement = None
        self.niveau = []
#
#     4. Le type de bilan : il faut convertir la donnée textuelle en un entier,
#        produit de nombres premiers.
#        Si rien n'est demande, on met 1.
#
      aux = 1
      dico_aux = {}
      dico_aux["INTERPENETRATION"] = 3
      dico_aux["NOMBRE"] = 7
      dico_aux["QUALITE"] = 5
      dico_aux["CONNEXITE"] = 11
      dico_aux["TAILLE"] = 13
      dico_aux["PROP_CALCUL"] = 17
      l_aux = dico_aux.keys()
      for choix in l_aux :
        if self.mots_cles.has_key(choix) :
          if self.mots_cles[choix] == "OUI" :
            aux = aux * dico_aux[choix]
      if aux == 1 :
        aux = 0
      self.TypeBila = aux
#
#     5. Les entrées/sorties au format MED
#
      self.CCNoMN__ = self.dico_configuration["NOM_MED_MAILLAGE_N"]
      if self.mode_homard == "ADAP" :
        self.CCNoMNP1 = self.dico_configuration["NOM_MED_MAILLAGE_NP1"]
        if self.dico_configuration.has_key("NOM_MED_MAILLAGE_NP1_ANNEXE") :
          self.CCMaiAnn = self.dico_configuration["NOM_MED_MAILLAGE_NP1_ANNEXE"]
        else :
          self.CCMaiAnn = None
#
#     6. Les entrées/sorties au format HOMARD
#
      if self.mode_homard == "ADAP" :
        self.fic_homard_niter   = "maill." + self.str_niter   + ".hom.med"
        self.fic_homard_niterp1 = "maill." + self.str_niterp1 + ".hom.med"
      else :
        self.fic_homard_niter   = None
        self.fic_homard_niterp1 = None
#
#     7. Le pilotage de l'adaptation
#
      if self.ModeHOMA == 1 :
#
#     7.1. Le type d'adaptation
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
#     7.2. L'éventuel seuil de raffinement
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
#gn          print "... self.critere_raffinement = ", self.critere_raffinement
#
#     7.3. L'éventuel seuil de deraffinement
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
#gn          print "... self.critere_deraffinement = ", self.critere_deraffinement
#
#     7.4. Les profondeurs extremes de raffinement/deraffinement
#
        saux = " "
        for mot_cle in [ "NIVE_MIN", "NIVE_MAX", "DIAM_MIN" ] :
          if self.mots_cles.has_key(mot_cle) :
            if self.mots_cles[mot_cle] is not None :
              if mot_cle == "NIVE_MIN" :
                aux = "NiveauMi"
              elif mot_cle == "NIVE_MAX" :
                aux = "NiveauMa"
              else :
                aux = "DiametMi"
              self.niveau.append((aux, self.mots_cles[mot_cle]))
              saux += aux
#
        if ( "DiametMi" in saux ) :
#gn          print self.mots_cles["DIAM_MIN"]
          if self.mots_cles["DIAM_MIN"] < 0 :
            message_erreur = "Le diametre mini doit etre strictement positif. "+\
                             "La valeur "+str(self.mots_cles["DIAM_MIN"])+" est incorrecte."
            break
#
        if ( ( "NiveauMi" in saux ) and ( "NiveauMa" in saux ) ) :
#gn          print self.mots_cles["NIVE_MIN"]
#gn          print self.mots_cles["NIVE_MAX"]
          if self.mots_cles["NIVE_MIN"] >= self.mots_cles["NIVE_MAX"] :
            message_erreur = "Le niveau mini ,"+str(self.mots_cles["NIVE_MIN"])+\
                             ", doit etre < au niveau maxi, "+str(self.mots_cles["NIVE_MAX"])+"."
            break
#
#     7.5. Les éventuelles zones de raffinement
#
        if self.dico_configuration.has_key("Zones_raffinement") :
          iaux = 0
          for zone in self.dico_configuration["Zones_raffinement"] :
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
#     8. Le pilotage de la modification
#
      if self.ModeHOMA == 3 :
        mot_cle = "DEGRE"
        if self.mots_cles.has_key(mot_cle) :
          if self.mots_cles[mot_cle] is not None :
            self.ModDegre = self.mots_cles[mot_cle]
#
        mot_cle = "JOINT"
        if self.mots_cles.has_key(mot_cle) :
          if self.mots_cles[mot_cle] is not None :
            self.CreJoint = self.mots_cles[mot_cle]
#gn        print self.ModDegre, self.CreJoint
#
#     9. Options annexes
#
      info = self.dico_configuration["INFO"]
      if ( info == 2 ) :
        self.MessInfo = 2
      elif ( info > 2 ) :
        self.MessInfo = 30
      else :
        self.MessInfo = None

#
      break
#
    if message_erreur is not None :
      UTMESS("F", 'HOMARD0_2', valk=message_erreur)
#
    return self.fic_homard_niter, self.fic_homard_niterp1
# ------------------------------------------------------------------------------
  def ouvre_fichier (self, nomfic_local) :
    """Ouvre en ecriture un fichier apres l'avoir eventuellement detruit
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
        UTMESS("F", 'HOMARD0_3', valk=nomfic)
#
    fichier = open (nomfic,"w")
    self.fichier = fichier
#
    return fichier, nomfic
# ------------------------------------------------------------------------------
  def ecrire_ligne_configuration_0 (self, commentaire) :
    """Ecrit une ligne de commentaires du fichier de configuration
   Arguments :
      . commentaire : le commentaire e ecrire
    """
#
    ligne = "#\n"
    ligne = ligne + "# " + commentaire + "\n"
    ligne = ligne + "#\n"
    self.fichier.write(ligne)
#
    return
# ------------------------------------------------------------------------------
  def ecrire_ligne_configuration_1 (self, texte) :
    """Ecrit une ligne brute du fichier de configuration
   Arguments :
      . texte : le texte a ecrire
    """
#
#    print texte
#    ligne = texte + "\n"
    ligne = texte
#    print "==> ",ligne
    self.fichier.write(ligne)
#
    return
# ------------------------------------------------------------------------------
  def ecrire_ligne_configuration_2 (self, motcle, valeur) :
    """Ecrit une ligne du fichier de configuration dans le cas : motcle + valeur
   Arguments :
      . motcle : le mot-cle HOMARD a ecrire
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
      . motcle : le mot-cle HOMARD a ecrire
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
#     2. Generalites
#
      self.ecrire_ligne_configuration_0("Generalites")
      self.ecrire_ligne_configuration_2("ModeHOMA", self.ModeHOMA)
      self.ecrire_ligne_configuration_2("Action", self.Action)
      self.ecrire_ligne_configuration_2("ListeStd", self.ListeStd)
      self.ecrire_ligne_configuration_2("TypeBila", self.TypeBila)
      self.ecrire_ligne_configuration_2("CCAssoci", "MED")
      self.ecrire_ligne_configuration_2("NumeIter", self.dico_configuration["niter"])
#
#     3. Les fichiers externes
#
      self.ecrire_ligne_configuration_0("Les fichiers de Code_Aster, au format MED")
      self.ecrire_ligne_configuration_2("CCNoMN__", self.CCNoMN__)
      self.ecrire_ligne_configuration_2("CCMaiN__", self.dico_configuration["Fichier_ASTER_vers_HOMARD"])
      if self.mode_homard == "ADAP" :
        self.ecrire_ligne_configuration_2("CCNoMNP1", self.CCNoMNP1)
        self.ecrire_ligne_configuration_2("CCMaiNP1", self.dico_configuration["Fichier_HOMARD_vers_ASTER"])
#
      self.ecrire_ligne_configuration_0("Le repertoire des fichiers de bilan")
      self.ecrire_ligne_configuration_2("RepeInfo", self.dico_configuration["Rep_Calc_HOMARD_global"])
#
#     4. Les fichiers HOMARD
#
      self.ecrire_ligne_configuration_0("Les fichiers de HOMARD, au format MED")
      if self.mode_homard == "ADAP" :
        self.ecrire_ligne_configuration_3("HOMaiN__", "M_"+self.str_niter  , self.fic_homard_niter )
        self.ecrire_ligne_configuration_3("HOMaiNP1", "M_"+self.str_niterp1, self.fic_homard_niterp1)
        if ( self.dico_configuration["niter"] == 0 ) :
          aux = "TOUT"
        else :
          aux = "NP1"
        self.ecrire_ligne_configuration_2("EcriFiHO", aux)
#
#     5. Le pilotage de l'adaptation
#
      if self.mode_homard == "ADAP" :
#
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
          self.ecrire_ligne_configuration_2("CCNoChaI", self.dico_configuration["Indicateur"]["NOM_CHAM_MED"])
          if self.dico_configuration["Indicateur"].has_key("COMPOSANTE") :
            for saux in self.dico_configuration["Indicateur"]["COMPOSANTE"] :
              self.ecrire_ligne_configuration_2("CCCoChaI", saux)
          if self.dico_configuration["Indicateur"].has_key("NUME_ORDRE") :
            self.ecrire_ligne_configuration_2("CCNumOrI", self.dico_configuration["Indicateur"]["NUME_ORDRE"])
            self.ecrire_ligne_configuration_2("CCNumPTI", self.dico_configuration["Indicateur"]["NUME_ORDRE"])
          if self.mots_cles.has_key("USAGE_CMP") :
            self.ecrire_ligne_configuration_2("CCUsCmpI", self.mots_cles["USAGE_CMP"])
#
#     5.3. Les eventuelles zones de raffinement
#
        if self.dico_configuration.has_key("Zones_raffinement") :
          dico_zone = {}
          dico_zone["X_MINI"] = "ZoRaXmin"
          dico_zone["X_MAXI"] = "ZoRaXmax"
          dico_zone["Y_MINI"] = "ZoRaYmin"
          dico_zone["Y_MAXI"] = "ZoRaYmax"
          dico_zone["Z_MINI"] = "ZoRaZmin"
          dico_zone["Z_MAXI"] = "ZoRaZmax"
          dico_zone["X_CENTRE"] = "ZoRaXCen"
          dico_zone["Y_CENTRE"] = "ZoRaYCen"
          dico_zone["Z_CENTRE"] = "ZoRaZCen"
          dico_zone["RAYON"] = "ZoRaRayo"
          dico_zone["RAYON_INT"] = "ZoRaRayI"
          dico_zone["RAYON_EXT"] = "ZoRaRayE"
          dico_zone["X_AXE"] = "ZoRaXAxe"
          dico_zone["Y_AXE"] = "ZoRaYAxe"
          dico_zone["Z_AXE"] = "ZoRaZAxe"
          dico_zone["X_BASE"] = "ZoRaXBas"
          dico_zone["Y_BASE"] = "ZoRaYBas"
          dico_zone["Z_BASE"] = "ZoRaZBas"
          dico_zone["HAUTEUR"] = "ZoRaHaut"
          l_aux = dico_zone.keys()
          dico_zone["TYPE"] = "ZoRaType"
          dico_zone["RECTANGLE"] = 1
          dico_zone["BOITE"] = 2
          dico_zone["DISQUE"] = 3
          dico_zone["SPHERE"] = 4
          dico_zone["CYLINDRE"] = 5
          dico_zone["DISQUE_PERCE"] = 6
          dico_zone["TUYAU"] = 7
          iaux = 0
          for zone in self.dico_configuration["Zones_raffinement"] :
            iaux = iaux + 1
            self.ecrire_ligne_configuration_0("Zone de raffinement numero "+str(iaux))
            jaux = dico_zone[zone["TYPE"]]
            self.ecrire_ligne_configuration_3(dico_zone["TYPE"], iaux, jaux)
            for aux in l_aux :
              if zone.has_key(aux) :
                self.ecrire_ligne_configuration_3(dico_zone[aux], iaux, zone[aux])
#
#     5.4. Les profondeurs extremes de raffinement/deraffinement
#
        for aux in self.niveau :
          self.ecrire_ligne_configuration_2(aux[0], aux[1])
#
#     5.5. L'usage de l'indicateur
#
        if self.mots_cles.has_key("USAGE_CHAMP") :
          if self.mots_cles["USAGE_CHAMP"] is not None :
            self.ecrire_ligne_configuration_2("CCModeFI", self.mots_cles["USAGE_CHAMP"])
#
#     5.6. Les eventuels groupes de filtrage du raffinement/deraffinement
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
#     5.7. Les modifications
#
        if self.ModeHOMA == 3 :
          self.ecrire_ligne_configuration_2("ModDegre", self.ModDegre)
          self.ecrire_ligne_configuration_2("CreJoint", self.CreJoint)
#
#     5.8. L'eventuel maillage annexe
#
        if self.CCMaiAnn is not None :
          self.ecrire_ligne_configuration_0("Maillage d'autre degre")
          self.ecrire_ligne_configuration_2("ModDegre", "oui")
          self.ecrire_ligne_configuration_2("CCNoMAnn", self.CCMaiAnn)
          self.ecrire_ligne_configuration_2("CCMaiAnn", self.dico_configuration["Fichier_HOMARD_vers_ASTER"])
#
#     6. Les eventuels champs a mettre a jour
#
      if self.dico_configuration.has_key("Champs") :
        self.ecrire_ligne_configuration_0("Champs a mettre e jour")
        self.ecrire_ligne_configuration_2("CCSolN__", self.dico_configuration["Fichier_ASTER_vers_HOMARD"])
        self.ecrire_ligne_configuration_2("CCSolNP1", self.dico_configuration["Fichier_HOMARD_vers_ASTER"])
        iaux = 0
        for maj_champ in self.dico_configuration["Champs"] :
          iaux = iaux + 1
          self.ecrire_ligne_configuration_0("Mise a jour du champ numero "+str(iaux))
          self.ecrire_ligne_configuration_3("CCChaNom", iaux, maj_champ["NOM_CHAM_MED"])
          self.ecrire_ligne_configuration_3("CCChaTIn", iaux, maj_champ["TYPE_MAJ"])
          if maj_champ.has_key("NUME_ORDRE") :
            self.ecrire_ligne_configuration_3("CCChaNuO", iaux, maj_champ["NUME_ORDRE"])
            self.ecrire_ligne_configuration_3("CCChaPdT", iaux, maj_champ["NUME_ORDRE"])
          elif maj_champ.has_key("INST") :
            self.ecrire_ligne_configuration_3("CCChaIns", iaux, maj_champ["INST"])
#
#     7. L'eventuel suivi de frontiere
#
      SuivFron = 1
#
#     7.1. A partir d'un maillage de la frontiere
#
      if self.dico_configuration.has_key("NOM_MED_MAILLAGE_FRONTIERE") :
        SuivFron = SuivFron * 2
        self.ecrire_ligne_configuration_0("Maillage de frontiere")
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
#     7.2. A partir d'une definition analytique
#
      if self.dico_configuration.has_key("Frontiere_analytique") :
        SuivFron = SuivFron * 3
        dico_frontiere = {}
        dico_frontiere["RAYON"] = "FARayon"
        dico_frontiere["X_CENTRE"] = "FAXCen"
        dico_frontiere["Y_CENTRE"] = "FAYCen"
        dico_frontiere["Z_CENTRE"] = "FAZCen"
        dico_frontiere["X_AXE"] = "FAXAxe"
        dico_frontiere["Y_AXE"] = "FAYAxe"
        dico_frontiere["Z_AXE"] = "FAZAxe"
        l_aux = dico_frontiere.keys()
        dico_frontiere["GROUP_MA"] = "FAGroupe"
        dico_frontiere["TYPE"] = "FAType"
        dico_frontiere["CYLINDRE"] = 1
        dico_frontiere["SPHERE"] = 2
        iaux = 0
        for frontiere in self.dico_configuration["Frontiere_analytique"] :
          if not type(frontiere["GROUP_MA"]) in EnumTypes :
            lt_aux = [ frontiere["GROUP_MA"] ]
          else :
            lt_aux = frontiere["GROUP_MA"]
          for group_ma in lt_aux :
            iaux = iaux + 1
            self.ecrire_ligne_configuration_0("Description de la frontiere analytique numero "+str(iaux))
            jaux = dico_frontiere[frontiere["TYPE"]]
            self.ecrire_ligne_configuration_3(dico_frontiere["TYPE"], iaux, jaux)
            self.ecrire_ligne_configuration_3(dico_frontiere["GROUP_MA"], iaux, group_ma)
            for aux in l_aux :
              if frontiere.has_key(aux) :
                self.ecrire_ligne_configuration_3(dico_frontiere[aux], iaux, frontiere[aux])
#
#     7.3. Activation de la fonction
#
      if ( self.dico_configuration.has_key("NOM_MED_MAILLAGE_FRONTIERE") or self.dico_configuration.has_key("Frontiere_analytique") ) :
        self.ecrire_ligne_configuration_2("SuivFron", SuivFron)
#
#     8. Options particulieres
#
      self.ecrire_ligne_configuration_0("Autres options")
      if self.mots_cles.has_key("LANGUE") :
        self.ecrire_ligne_configuration_2("Langue", self.mots_cles["LANGUE"])
      if self.MessInfo is not None :
        self.ecrire_ligne_configuration_2("MessInfo", self.MessInfo)
      if self.dico_configuration["version_perso"] :
        VERSION_HOMARD = self.dico_configuration["VERSION_HOMARD"]
        self.ecrire_ligne_configuration_2("DicoOSGM", "$HOMARD_USER/"+VERSION_HOMARD+"/CONFIG/typobj.stu")
#
#     9. L'usage des mailles incompatibles avec HOMARD
#
      if self.mailles_incompatibles is not None :
        self.ecrire_ligne_configuration_0("Les mailles incompatibles avec HOMARD")
        self.ecrire_ligne_configuration_2("TypeElem", self.mailles_incompatibles)
#
#     10. L'eventuel complement
#
      if self.dico_configuration.has_key("fichier_conf_suppl") :
        nomfic = self.dico_configuration["fichier_conf_suppl"]
#        print nomfic
        if os.path.isfile(nomfic) :
          fichier_bis = open (nomfic, "r")
          les_lignes = fichier_bis.readlines()
          fichier_bis.close()
#          print les_lignes
          for ligne in les_lignes :
            self.ecrire_ligne_configuration_1(ligne)
#
#     11. Fermeture du fichier
#
      fichier.close()
      break
#
    if message_erreur is not None :
      message_erreur = "Ecriture de "+nomfic_global+". "+message_erreur
      UTMESS("F", 'HOMARD0_2', valk=message_erreur)
#
    return
# ------------------------------------------------------------------------------
  def ecrire_fichier_donnees (self) :
    """Ecrit le fichier des données dans le cas d'une demande d'information
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
      UTMESS("F", 'HOMARD0_2', valk=message_erreur)
#
    return nomfic_global
