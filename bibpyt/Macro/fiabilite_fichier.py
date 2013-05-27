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
# person_in_charge: gerald.nicolas at edf.fr
#
class fiabilite_fichier :
#
  """ Classe des fichiers de données des logiciels fiabilistes
      Cette classe a été mise au point pour le couplage entre
      Code_ASTER et MEFISTO, mais pourrait servir ailleurs.
  """
#
#
#====
# 1. Le constructeur
#====
#
#
  def __init__ ( self, jdc, Rep_Calc_LOGICIEL_global, nomfic, info = 1 ) :
#
#   jdc : le jeu de commandes en cours de traitement
#
    self.jdc = jdc
#
#   Rep_Calc_LOGICIEL_global : le répertoire d'exécution du logiciel de fiabilité
#
    self.Rep_Calc_LOGICIEL_global = Rep_Calc_LOGICIEL_global
#
#   nomfic : nom local du fichier à créer
#
    self.nomfic = nomfic
#
#   messages_erreur : messages d'erreur
#
    self.messages_erreur = { 0 : "Tout va bien",
                             1 : "==> Ce fichier est inconnu.",
                             2 : "==> Ce type d'ouverture est inconnu.",
                            10 : "==> Problème à l'ouverture.",
                            11 : "==> Problème à la fermeture.",
                            20 : "==> Problème à l'impression." }
#
#   info : niveau d'information au sens ASTER
#
    self.info = info
#
#   ligne_sep : ligne de séparation
#
    self.ligne_sep = "========================================================="
    self.ligne_commentaire = "#" + self.ligne_sep + "\n"
#
    if info >= 2 :
      print "Création du fichier : "+self.nomfic
#
#====
# 2. Ouverture du fichier
#====
#
  def Ouvre_Fichier ( self, type_ouvr ) :
#
# 2.0. ==> Préalables
#
    """
    Ouvre le fichier en lecture ou écriture.
    0 : tout s'est bien passé
    1 : on veut ouvrir en lecture un fichier qui n'existe pas
    2 : le mode d'ouverture est inconnu
   10 : impossible d'ouvrir
    """
#
    import os
#
# 2.1. ==> Le nom global du fichier
#
    self.nomfic_global = os.path.join(self.Rep_Calc_LOGICIEL_global,self.nomfic)
#
# 2.2. ==> Controles
#
    erreur = 0
#
    if ( type_ouvr == "w" or type_ouvr == "r" ) :
#
      if ( type_ouvr == "r" ) :
        if not os.path.isfile(self.nomfic_global) :
          erreur = 1          

    else :
#
      self.jdc.cr.warn("Type d'ouverture : "+type_ouvr)
      erreur = 2
#
# 2.3. ==> Ouverture vraie
#
    if not erreur :
#
      erreur_partiel = [0]
      try :
        self.fic = open( self.nomfic_global, type_ouvr )
      except os.error,erreur_partiel :
        self.jdc.cr.warn("Code d'erreur de open : " + str(erreur_partiel[0]) + " : " + erreur_partiel[1])
        erreur = 10
#
# 2.4. ==> C'est fini
#
    if erreur :
      self.jdc.cr.warn("Fichier : "+self.nomfic)
      self.jdc.cr.warn(self.messages_erreur[erreur])
#
    return erreur
#
#====
# 3. Fermeture du fichier
#====
#
  def Ferme_Fichier ( self ) :
#
# 3.0. ==> Préalables
#
    """
    Ferme le fichier.
    0 : tout s'est bien passé
   20 : impossible d'imprimer
    """
#
    import os
#
# 3.1. ==> Controles
#
    erreur = 0
#
    if not os.path.isfile(self.nomfic_global) :
      erreur = 1          
#
# 3.2. ==> Fermeture vraie
#
    if not erreur :
#
      erreur_partiel = [0]
      try :
        self.fic.close( )
      except os.error,erreur_partiel :
        self.jdc.cr.warn("Code d'erreur de close : " + str(erreur_partiel[0]) + " : " + erreur_partiel[1])
        erreur = 11
#
# 3.3. ==> C'est fini
#
    if erreur :
      self.jdc.cr.warn("Fichier : "+self.nomfic)
      self.jdc.cr.warn(self.messages_erreur[erreur])
#
    return erreur
#
#====
# 4. Impression du contenu du fichier
#====
#
  def Imprime_Fichier ( self ) :
#
# 4.0. ==> Préalables
#
    """
    Imprime le fichier.
    0 : tout s'est bien passé
   20 : impossible d'imprimer
    """
#
# 4.1. ==> Lecture
#
    erreur = self.Ouvre_Fichier ( "r" )         
    if not erreur :
      les_lignes = self.fic.readlines()
      erreur = self.Ferme_Fichier ( )         
#
# 4.2. ==> Impression
#
    if not erreur :
#
      print "\n"+self.ligne_sep
      print "Contenu du fichier " + self.nomfic," :"
      for ligne in les_lignes :
        print ligne[:-1]
      print self.ligne_sep+"\n"
#
# 4.4. ==> C'est fini
#
    if erreur :
      erreur = 20
      self.jdc.cr.warn("Fichier : "+self.nomfic)
      self.jdc.cr.warn(self.messages_erreur[erreur])
#
    return erreur
#
#====
# 5. Ecriture de lignes de commentaires
#====
#
  def Ecrit_Commentaires ( self, comm ) :
#
    """
    Liste = commentaires à écrire
    Soit c'est une chaine qu'on écrit sur une ligne ;
    Soit c'est une liste, qu'on écrit à raison de une par ligne.
    Remarque : cela suppose que le fichier est ouvert en écriture
    """
#
    if type(comm) == type([ ]) :
      Liste = comm
    else :
      Liste = [comm]
#
    for ligne in Liste :
      self.fic.write("# "+str(ligne)+"\n")
#
#====
# 6. Ecriture de lignes de titres
#====
#
  def Ecrit_Titre ( self, comm ) :
#
    """
    Liste = commentaires à écrire, encadrés par des séparateurs
    Soit c'est une chaine qu'on écrit sur une ligne ;
    Soit c'est une liste, qu'on écrit à raison de une par ligne.
    Remarque : cela suppose que le fichier est ouvert en écriture
    """
#
    self.fic.write(self.ligne_commentaire)
    self.Ecrit_Commentaires(comm)
    self.fic.write(self.ligne_commentaire)
#
#====
# 7. Ecriture d'une ligne de valeurs
#====
#
  def Ecrit_Valeurs ( self, val ) :
#
    """
    Liste = liste des valeurs à écrire, représenatn une ligne
    Remarque : cela suppose que le fichier est ouvert en écriture
    """
#
    if type(val) == type([ ]) :
      ligne = " "
      for aux in val :
        ligne = ligne + " " + str(aux)
    else :
      ligne = str(val)
#
    self.fic.write(ligne+"\n")
#
#
#=======================================================================================
#=======================================================================================


#
#
if __name__ == "__main__" :
#
  import os
  import sys
  import tempfile
#
# 1. ==> Préalable
#
  Rep_Calc_LOGICIEL_global = tempfile.mktemp()
  os.mkdir(Rep_Calc_LOGICIEL_global)
#
  jdc = None
#
# 2. ==> Création de la classe
#
  nomfic = "dataGrad"
  fic = fiabilite_fichier ( jdc, Rep_Calc_LOGICIEL_global , nomfic )
#
# 3. ==> Ouverture du fichier
#
  erreur = fic.Ouvre_Fichier ( "w" )
#
# 4. ==> Remplissage du fichier
#
  if not erreur :
    aux = ["Titre 1", "Titre 2"]
    fic.Ecrit_Titre (aux)
    aux = ["Ligne 1", "Ligne 2"]
    fic.Ecrit_Commentaires (aux)
    aux = "Ligne en forme de chaine"
    fic.Ecrit_Commentaires (aux)
    aux = 1789.1792
    fic.Ecrit_Commentaires (aux)
    aux = [1, 0.0]
    fic.Ecrit_Valeurs (aux)
    aux = 1958.
    fic.Ecrit_Valeurs (aux)
#
# 5. ==> Fermeture du fichier
#
  if not erreur :
    erreur = fic.Ferme_Fichier ( )
#
# 4. ==> Impression du fichier
#
  if not erreur :
    erreur = fic.Imprime_Fichier ( )
#
# 4. ==> La fin
#
  Liste = os.listdir(Rep_Calc_LOGICIEL_global)
#
  for nomfic in Liste :
    fic_total = os.path.join(Rep_Calc_LOGICIEL_global,nomfic)
    os.chmod  (fic_total,0755)
    os.remove (fic_total)
  os.rmdir (Rep_Calc_LOGICIEL_global)
#
  if erreur :
    mess = "Erreur " + str(erreur)
  else :
    mess = "Fin normale."
  sys.exit(mess)
