#@ MODIF macr_adap_mail_ops Macro  DATE 19/01/2004   AUTEUR DURAND C.DURAND 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
def macr_adap_mail_ops ( self,
                         INFO, VERSION_HOMARD, LANGUE,
                         NOMBRE, QUALITE, CONNEXITE, TAILLE, INTERPENETRATION,
                         NON_SIMPLEXE, MAILLAGE_FRONTIERE,
                         **args):
  """
     Ecriture de la macro MACR_ADAP_MAIL/MACR_INFO_MAIL
     Remarque : on ne mentionne explicitement que les mots-clés communs aux
                deux macros. Les autres sont dans le dictionnaire args
  """
#
#  1. args est le dictionnaire des arguments
#     args.keys() est la liste des mots-clés
#     args.keys()[0] est la premiere valeur de cette liste
#     args.keys()[1:] est la liste des valeurs suivantes dans cette liste
#     args.keys(mot_cle) représente le contenu de la variable mot_cle dans la macro appelante.
#
###  print args
###  print args.keys()
###  if len (args.keys())>0 : print args.keys()[0]
###  print args['MAILLAGE']
#
#  2. Les caractéristiques d'un passage sont conservées dans un dictionnaire. Il y a autant de
#     dictionnaires que de sollicitations pour une série d'adaptation. L'ensemble de ces dictionnaires
#     est conservé dans la liste Liste_Passages. Cette liste est nécessairement globale pour pouvoir
#     la retrouver à chaque nouveau passage.
#     Description du dictionnaire de passages :
#        dico['Maillage_0']             = o ; string ; nom du concept du maillage initial de la série d'adaptation
#        dico['Maillage_NP1']           = o ; string ; nom du concept du dernier maillage adapté
#        dico['Rep_Calc_HOMARD_global'] = o ; string ; Nom global du répertoire de calcul pour HOMARD
#        dico['Rep_Calc_HOMARD_local']  = o ; string ; Nom local du répertoire de calcul pour HOMARD
#                                                      depuis le répertoire de calcul pour ASTER
#        dico['niter']                  = o ; entier ; numéro d'itération
#
#  3. Les caractéristiques d'un maillage sont conservées dans un dictionnaire. Il y a autant de
#     dictionnaires que de maillages manipulés. L'ensemble de ces dictionnaires est conservé
#     dans la liste Liste_Maillages.
#     Description du dictionnaire de maillages :
#        dico['Type_Maillage'] = o ; string ; 'MAILLAGE_N', 'MAILLAGE_NP1' ou 'MAILLAGE_FRONTIERE'
#        dico['Nom_ASTER']     = o ; concept ASTER associé
#        dico['Action']        = o ; string ; 'A_ecrire' ou 'A_lire'
#        dico['NOM_MED']       = o ; string ; Nom MED du maillage
#
#  4. Les caractéristiques d'un champ sont conservées dans un dictionnaire. Il y a autant de
#     dictionnaires que de champs manipulés. L'ensemble de ces dictionnaires est conservé
#     dans la liste Liste_Champs.
#     Description du dictionnaire de champs :
#        dico['Type_Champ']   = o ; string ; 'INDICATEUR' ou 'CHAMP'
#        dico['RESULTAT']     = o ; concept ASTER du résutat associé
#        dico['NOM_CHAM']     = o ; string ; Nom ASTER du champ
#        dico['COMPOSANTE']   = f ; string ; Nom ASTER de la composante
#        dico['NUME_ORDRE']   = f ; entier ; Numéro d'ordre du champ
#        dico['INST']         = f ; entier ; Instant du champ
#        dico['PRECISION']    = f ; entier ; Précision sur l'instant du champ
#        dico['CRITERE']      = f ; entier ; Critère de précision sur l'instant du champ
#        dico['CHAM_MAJ']     = f ; string ; Nom ASTER du champ interpolé sur le nouveau maillage
#        dico['NOM_MED']      = o ; string ; Nom MED du champ
#
  from Accas import _F
  import aster 
  import string
  import types
  import os
#
  global Liste_Passages
#
#--------------------------------------------------------------------
# 1. Préalables
#--------------------------------------------------------------------
#
# 1.1. ==> La macro compte pour 1 dans la numerotation des commandes
#
  self.set_icmd(1)
#
# 1.2. ==> Numéro du passage dans cette macro
#
  try :
    self.jdc.indice_macro_homard = self.jdc.indice_macro_homard + 1
  except :
    self.jdc.indice_macro_homard = 1
    Liste_Passages = []
  Numero_Passage_Fonction = self.jdc.indice_macro_homard
###  print "Numero_Passage_Fonction = ",Numero_Passage_Fonction
#
# 1.3. ==> On importe les definitions des commandes a utiliser dans la macro
#
  DEFI_FICHIER    = self.get_cmd('DEFI_FICHIER')
  IMPR_RESU       = self.get_cmd('IMPR_RESU')
  EXEC_LOGICIEL   = self.get_cmd('EXEC_LOGICIEL')
  IMPR_FICO_HOMA  = self.get_cmd('IMPR_FICO_HOMA')
  LIRE_MAILLAGE   = self.get_cmd('LIRE_MAILLAGE')
  LIRE_CHAMP      = self.get_cmd('LIRE_CHAMP')
#
# 1.4. ==> Le nom du programme HOMARD à lancer
#
  repertoire_outils = aster.repout()
  homard            = repertoire_outils + 'homard'
#
# 1.5. ==> Initialisations
#
  codret = 0
  codret_partiel = [0]
  Rep_Calc_ASTER = os.getcwd()
#
  Liste_Maillages = []
  Liste_Champs    = []
#
  ADAPTATION = None
  MAJ_CHAM = None
#
  unite = 71
#
#--------------------------------------------------------------------
# 2. Décodage des arguments de la macro-commande
#--------------------------------------------------------------------
# 2.1. ==> Données de pilotage de l'adaptation
#
  if ( self.nom == 'MACR_ADAP_MAIL' ) :
#
    modhom = "ADAP"
#
    ADAPTATION = args['ADAPTATION']
    if args.has_key('MAJ_CHAM') :
      MAJ_CHAM = args['MAJ_CHAM']
#
# 2.1.1. ==> Les concepts 'maillage'
#
    for mot_cle in ['MAILLAGE_N','MAILLAGE_NP1'] :
      dico = {}
      dico['Type_Maillage'] = mot_cle
      dico['Nom_ASTER']     = ADAPTATION[mot_cle]
      if ( mot_cle == 'MAILLAGE_N' ) :
        dico['Action'] = 'A_ecrire'
      else :
        dico['Action'] = 'A_lire'
      Liste_Maillages.append(dico)
#
# 2.1.2. ==> L'éventuel indicateur d'erreur
#
    if ADAPTATION['LIBRE'] != None :
      dico = {}
      dico['Type_Champ'] = 'INDICATEUR'
      dico['RESULTAT']   = ADAPTATION['RESULTAT_N']
      dico['NOM_CHAM']   = ADAPTATION['INDICATEUR']
      dico['COMPOSANTE'] = ADAPTATION['NOM_CMP_INDICA']
      if ( ADAPTATION['NUME_ORDRE'] != None ) :
        dico['NUME_ORDRE'] = ADAPTATION['NUME_ORDRE']
      if ( ADAPTATION['INST'] != None ) :
        dico['INST'] = ADAPTATION['INST']
        for cle in [ 'PRECISION', 'CRITERE' ] :
          if ( ADAPTATION[cle] != None ) :
            dico[cle] = ADAPTATION[cle]
      dico['NOM_MED'] = aster.mdnoch ( dico['RESULTAT'].nom , dico['NOM_CHAM'] )
      Liste_Champs.append(dico)
###      print dico
#
# 2.1.3. ==> Les champs à mettre à jour
#
    if ( MAJ_CHAM != None ) :
#
      for maj_cham in MAJ_CHAM :
#
        dico = {}
        dico['Type_Champ'] = 'CHAMP'
        for cle in [ 'CHAM_MAJ', 'TYPE_CHAM', 'RESULTAT', 'NOM_CHAM', 'NUME_ORDRE' ] :
          dico[cle] = maj_cham[cle]
        if ( maj_cham['INST'] != None ) :
          dico['INST'] = maj_cham['INST']
          for cle in [ 'PRECISION', 'CRITERE' ] :
            if ( maj_cham[cle] != None ) :
              dico[cle] = maj_cham[cle]
        dico['NOM_MED'] = aster.mdnoch ( dico['RESULTAT'].nom , dico['NOM_CHAM'] )
#
###        print dico
        Liste_Champs.append(dico)
#
# 2.2. ==> Données de pilotage de l'information
#
  else :
#
    modhom = "INFO"
#
    dico = {}
    dico['Type_Maillage'] = 'MAILLAGE_N'
    dico['Nom_ASTER']     = args['MAILLAGE']
    dico['Action']        = 'A_ecrire'
    Liste_Maillages.append(dico)
#
# 2.3. ==> Suivi de frontière
#
  if ( MAILLAGE_FRONTIERE != None ) :
#
    dico = {}
    dico['Type_Maillage'] = 'MAILLAGE_FRONTIERE'
    dico['Nom_ASTER']     = MAILLAGE_FRONTIERE
    dico['Action']        = 'A_ecrire'
    Liste_Maillages.append(dico)
#
#--------------------------------------------------------------------
# 3. Préparation du lancement des commandes
#--------------------------------------------------------------------
#
# 3.1. ==> . Elaboration des noms MED des concepts de maillage
#          . Memorisation des noms ASTER du maillage en entrée et en sortie (sous forme string)
#          On crée une nouvelle liste des dictionnaires décrivant les maillages
#          et à la fin on écrase l'ancienne liste par cette nouvelle.
#
  L = []
  for dico in Liste_Maillages :
    dico['NOM_MED'] = aster.mdnoma(dico['Nom_ASTER'].nom)
    L.append(dico)
    if ( dico['Type_Maillage'] == 'MAILLAGE_N' ) :
      Nom_Concept_Maillage_N = dico['Nom_ASTER'].nom
    elif ( dico['Type_Maillage'] == 'MAILLAGE_NP1' ) :
      Nom_Concept_Maillage_NP1 = dico['Nom_ASTER'].nom
  Liste_Maillages = L
#
# 3.2. ==> Recherche du numéro d'itération et du répertoire de travail
#
# 3.2.1. ==> Par défaut :
#            . le numéro d'itération est nul
#            . le nom du répertoire de lancement de HOMARD est construit sur le nom
#              du maillage en entrée et le numéro de passage dans la fonction
#
  niter = 0
  Nom_Rep_local = Nom_Concept_Maillage_N + "_" + modhom + "_" + str(Numero_Passage_Fonction)
  Rep_Calc_HOMARD_local = os.path.join('.',Nom_Rep_local)
  Rep_Calc_HOMARD_global = os.path.join(Rep_Calc_ASTER,Nom_Rep_local)
###  print "Rep_Calc_HOMARD_local  = ", Rep_Calc_HOMARD_local
###  print "Rep_Calc_HOMARD_global = ", Rep_Calc_HOMARD_global
#
# 3.2.2. ==> En adaptation :
#
  if ( modhom == "ADAP" ) :
#
# 3.2.2.1. ==> On recherche si dans les passages déjà effectués, il en existe un
#              dont le maillage d'arrivée était l'actuel maillage d'entrée. Si c'est
#              le cas, cela veut dire que l'adaptation en cours est la suite d'une
#              précédente. On doit donc utiliser le meme répertoire. Le numéro
#              d'itération est celui de l'adaptation précédente augmenté de 1.
#
    for dico in Liste_Passages :
      if ( dico['Maillage_NP1'] == Nom_Concept_Maillage_N ) :
        niter   = dico['niter'] + 1
        Rep_Calc_HOMARD_local  = dico['Rep_Calc_HOMARD_local']
        Rep_Calc_HOMARD_global = dico['Rep_Calc_HOMARD_global']
#
# 3.2.2.2. ==> Memorisation de ce passage
#
# 3.2.2.2.1. ==> Enregistrement d'un nouveau cas de figure
#
    if ( niter == 0 ) :
      dico = {}
      dico['Maillage_0']   = Nom_Concept_Maillage_N
      dico['Maillage_NP1'] = Nom_Concept_Maillage_NP1
      dico['Rep_Calc_HOMARD_local']  = Rep_Calc_HOMARD_local
      dico['Rep_Calc_HOMARD_global'] = Rep_Calc_HOMARD_global
      dico['niter']        = niter
      Liste_Passages.append(dico)
#
# 3.2.2.2.2. ==> Modification du cas en cours
#
    else :
      L = []
      for dico in Liste_Passages :
        if ( dico['Maillage_NP1'] == Nom_Concept_Maillage_N ) :
          dico['Maillage_NP1'] = Nom_Concept_Maillage_NP1
          dico['niter']        = niter
        L.append(dico)
      Liste_Passages = L
#
###  print "niter = ", niter, ", Rep_Calc_HOMARD_global = ", Rep_Calc_HOMARD_global
#
#--------------------------------------------------------------------
# 4. Ecriture des commandes
#--------------------------------------------------------------------
#
# 4.1. ==> Création du répertoire pour homard
#          attention : on ne fait cette creation qu'une seule fois par cas
#                      d'adaptation ou d'information
#
  if ( niter == 0 ) :
#
    try :
      os.mkdir(Rep_Calc_HOMARD_global)
    except os.error,codret_partiel :
      self.cr.warn("Code d'erreur de mkdir : " + str(codret_partiel[0]) + " : " + codret_partiel[1])
      self.cr.fatal("<F> <MACR_ADAP_MAIL> Impossible de créer le répertoire de travail pour HOMARD : "+Rep_Calc_HOMARD_global)
      codret = codret + 1
      return codret
#
# 4.2. ==> Ecriture des commandes de creation des donnees MED
#
#  On doit écrire : le maillage,
#                   le champ d'indicateur d'erreur
#                   les champs à convertir
#  Remarque : on met tout dans le meme fichier
#
#  Chacune de ces écritures est optionnelle selon le contexte.
#
  if ( INFO > 1 ) : infomail='OUI'
  else :            infomail='NON'
#
# 4.2.1. ==> Noms des fichiers d'ASTER vers HOMARD et éventuellement de HOMARD vers ASTER
#            Remarque : aujourd'hui, les écritures ou les lectures au format MED se font obligatoirement sur
#                       un fichier de nom fort.n, placé dans le répertoire de calcul
#                       Dans le fichier de configuration, on donne comme nom MAILL.(niter).MED en entrée et
#                       MAILL.(niter+1).MED en sortie (cf. adhc00)
#                       Tant que les E/S MED n'ont pas évolué, on fait un lien pour assurer la cohérence.
#
# 4.2.1.1. ==> D'ASTER vers HOMARD
#
  Unite_Fichier_ASTER_vers_HOMARD = 1787 + 2*Numero_Passage_Fonction
  Fichier_ASTER_vers_HOMARD = os.path.join(Rep_Calc_ASTER,"fort." + str(Unite_Fichier_ASTER_vers_HOMARD))
  Nom_Symbolique_Fichier_ASTER_vers_HOMARD = "ASTER_to_HOMARD"
  Fichier_HOMARD_Entree = os.path.join(Rep_Calc_HOMARD_global,"MAILL."+str(niter)+".MED")
###  print "Fichier_ASTER_vers_HOMARD = ",Fichier_ASTER_vers_HOMARD
###  print "Fichier_HOMARD_Entree = ",Fichier_HOMARD_Entree
  try :
    os.symlink(Fichier_ASTER_vers_HOMARD,Fichier_HOMARD_Entree)
  except os.error,codret_partiel :
    self.cr.warn("Code d'erreur de symlink : " + str(codret_partiel[0]) + " : " + codret_partiel[1])
    self.cr.fatal("<F> <MACR_ADAP_MAIL> Probleme au lien entre " + Fichier_ASTER_vers_HOMARD + " et " + Fichier_HOMARD_Entree)
    codret = codret + 1
    return codret
#
# 4.2.1.2. ==> De HOMARD vers ASTER
#  
  if ( modhom == "ADAP" ) :
    Unite_Fichier_HOMARD_vers_ASTER = Unite_Fichier_ASTER_vers_HOMARD + 1
    Fichier_HOMARD_vers_ASTER = os.path.join(Rep_Calc_ASTER,"fort." + str(Unite_Fichier_HOMARD_vers_ASTER))
    Fichier_HOMARD_Sortie = os.path.join(Rep_Calc_HOMARD_global,"MAILL."+str(niter+1)+".MED")
###    print "Fichier_HOMARD_vers_ASTER = ",Fichier_HOMARD_vers_ASTER
###    print "Fichier_HOMARD_Sortie = ",Fichier_HOMARD_Sortie
    try :
      os.symlink(Fichier_HOMARD_vers_ASTER,Fichier_HOMARD_Sortie)
    except os.error,codret_partiel :
      self.cr.warn("Code d'erreur de symlink : " + str(codret_partiel[0]) + " : " + codret_partiel[1])
      self.cr.fatal("<F> <MACR_ADAP_MAIL> Probleme au lien entre " + Fichier_HOMARD_vers_ASTER + " et " + Fichier_HOMARD_Sortie)
      codret = codret + 1
      return codret
#
# 4.2.2. La définition du fichier de ASTER vers HOMARD
# 
  DEFI_FICHIER ( ACTION= "ASSOCIER",
                 FICHIER = Nom_Symbolique_Fichier_ASTER_vers_HOMARD,
                 UNITE = Unite_Fichier_ASTER_vers_HOMARD,
                 TYPE = "LIBRE",
                 INFO = INFO )
#
# 4.2.3. Le(s) maillage(s)
# Le maillage de calcul et l'éventuel maillage de la frontiere sont écrits
# dans le meme fichier MED
# En fait, on pourrait s'en passer au dela de la 1ère itération
# car HOMARD a mémorisé. Mais dès que l'on écrit un champ,
# les conventions MED imposent la présence du maillage dans le fichier.
# Donc on va toujours écrire.
#
  for dico in Liste_Maillages :
    if ( dico['Action'] == 'A_ecrire' ) :
      motscsi={}
      motscsi['MAILLAGE'] = dico['Nom_ASTER']
      motscfa={}
      motscfa['RESU']=_F( INFO_MAILLAGE=infomail,
                          FICHIER=Nom_Symbolique_Fichier_ASTER_vers_HOMARD,
                          FORMAT ='MED',
                          **motscsi )
#
      IMPR_RESU ( INFO = INFO, **motscfa )
#
# 4.2.4. Le(s) champ(s)
#
  for dico in Liste_Champs :
    motscsi={}
    for cle in [ 'RESULTAT', 'NOM_CHAM', 'NUME_ORDRE', 'INST', 'PRECISION', 'CRITERE' ] :
      if dico.has_key(cle) :
        if ( dico[cle] != None ) :
          motscsi[cle] = dico[cle]
    if dico.has_key('COMPOSANTE') :
      motscsi['NOM_CMP'] = dico['COMPOSANTE']
    motscfa={}
    motscfa['RESU']=_F( INFO_MAILLAGE=infomail,
                        FICHIER=Nom_Symbolique_Fichier_ASTER_vers_HOMARD,
                        FORMAT ='MED',
                        **motscsi
                      )
#
    IMPR_RESU ( INFO = INFO, **motscfa )
#
# 4.3. ==> Ecriture de la commande d'écriture des fichiers de données pour HOMARD
#
  motscfa={}
#  
# 4.3.1. ==> Le traitement
#
  motscsi={}
#
# 4.3.1.1. ==> Le type de traitement
#
  if ( modhom == "ADAP" ) :
    if ( ADAPTATION['UNIFORME'] != None ) :
      motscsi['UNIFORME'] = ADAPTATION['UNIFORME']
    else :
      motscsi['ADAPTATION'] = ADAPTATION['LIBRE']
  else :
    motscsi['INFORMATION'] = 'OUI'
#
# 4.3.1.2. ==> Les noms med des maillages
#
  for dico in Liste_Maillages :
###    print "Nom MED de " + dico['Type_Maillage'] + " = " + dico['NOM_MED']
    motscsi[ 'NOM_MED_'+dico['Type_Maillage'] ] = dico['NOM_MED']
#
# 4.3.1.3. ==> Les caracteristiques de l'éventuel indicateur d'erreur
#
  for dico in Liste_Champs :
    if ( dico['Type_Champ'] == 'INDICATEUR' ) :
      Liste_aux = [ 'NOM_MED', 'COMPOSANTE' ]
      if dico.has_key('NUME_ORDRE') :
        Liste_aux.append('NUME_ORDRE')
      else :
        for cle in [ 'RESULTAT', 'NOM_CHAM', 'INST', 'PRECISION', 'CRITERE' ] :
          Liste_aux.append(cle)
      for cle in Liste_aux :
        if dico.has_key(cle) :
          if ( dico[cle] != None ) :
            motscsi[cle] = dico[cle]
#
# 4.3.1.4. ==> Les critères de raffinement et les niveaux extremes
#
  if ( modhom == "ADAP" ) :
    Liste_aux = [ ]
    if ( ADAPTATION['LIBRE'] == 'RAFF_DERA' or ADAPTATION['LIBRE'] == 'RAFFINEMENT' ) :
      Liste_aux.append('CRIT_RAFF_ABS')
      Liste_aux.append('CRIT_RAFF_REL')
      Liste_aux.append('CRIT_RAFF_PE')
    if ( ADAPTATION['LIBRE'] == 'RAFF_DERA' or ADAPTATION['LIBRE'] == 'DERAFFINEMENT' ) :
      Liste_aux.append('CRIT_DERA_ABS')
      Liste_aux.append('CRIT_DERA_REL')
      Liste_aux.append('CRIT_DERA_PE')
    niveau = 0
    if ( ADAPTATION['LIBRE'] == 'RAFF_DERA' or ADAPTATION['LIBRE'] == 'RAFFINEMENT' or ADAPTATION['UNIFORME'] == 'RAFFINEMENT' ) :
      Liste_aux.append('NIVE_MAX')
      niveau = niveau + 1
    if ( ADAPTATION['LIBRE'] == 'RAFF_DERA' or ADAPTATION['LIBRE'] == 'DERAFFINEMENT' or ADAPTATION['UNIFORME'] == 'DERAFFINEMENT' ) :
      Liste_aux.append('NIVE_MIN')
      niveau = niveau + 2
    for mot_cle in Liste_aux :
      if ( ADAPTATION[mot_cle] != None ) :
        motscsi[mot_cle] = ADAPTATION[mot_cle]
#
    if ( niveau == 2 ) : 
      if ( ADAPTATION['NIVE_MIN'] > ADAPTATION['NIVE_MAX'] ) :
        self.cr.fatal("<F> <MACR_ADAP_MAIL> Le niveau minimum doit etre inferieur au niveau maximum.")
        codret = codret + 1
        return codret
#
# 4.3.1.5. ==> Mise à jour de la solution
#
  if ( MAJ_CHAM != None ) :
    motscsi['MAJ_CHAM'] = 'OUI'
#
# 4.3.1.6. ==> Numéro d'itération
#
  if ( modhom == "ADAP" ) :
    motscsi['NITER'] = niter
#
# 4.3.1.7. ==> Suivi de la frontiere
#
  if args.has_key('GROUP_MA') :
    if ( args['GROUP_MA'] != None ) :
      motscsi['GROUP_MA'] = args['GROUP_MA']
#
# 4.3.1.8. ==> Bilan
#
  motscfa['TRAITEMENT'] = _F(**motscsi)
#
# 4.3.2. ==> L'analyse
#
  motscsi={}
  if ( NOMBRE != None )           : motscsi['NOMBRE' ]           = NOMBRE
  if ( QUALITE != None )          : motscsi['QUALITE' ]          = QUALITE
  if ( CONNEXITE != None )        : motscsi['CONNEXITE' ]        = CONNEXITE
  if ( TAILLE != None )           : motscsi['TAILLE' ]           = TAILLE
  if ( INTERPENETRATION != None ) : motscsi['INTERPENETRATION' ] = INTERPENETRATION
#
  motscfa['ANALYSE'] = _F(**motscsi)
#
# 4.3.3. ==> La commande
#
# 4.3.3.1. ==> Les fichiers annexes
#
  dico = {}
#
  Nom_Fichier_Configuration = 'HOMARD.Configuration'
  Fichier_Configuration = os.path.join(Rep_Calc_HOMARD_local,'HOMARD.Configuration')
#                                 1234567890123456
  dico[Fichier_Configuration] = ('HOMARD_CONFIG','FICHIER_CONF',unite)
#
  if ( modhom != "ADAP" ) :
    unite = unite + 1
    Nom_Fichier_Donnees = 'HOMARD.Donnees'
    Fichier_Donnees = os.path.join(Rep_Calc_HOMARD_local,Nom_Fichier_Donnees)
#                             1234567890123456
    dico[Fichier_Donnees] = ('HOMARD_DONN','FICHIER_DONN',unite)
#
# 4.3.3.2. ==> L'ouverture de ces fichiers
#
  for fic in dico.keys() :
    DEFI_FICHIER ( ACTION= "ASSOCIER", NOM_SYSTEME = fic, FICHIER = dico[fic][0], UNITE = dico[fic][2],
                   TYPE = "ASCII", ACCES = "NEW", INFO = INFO )
    motscfa[dico[fic][1]] = dico[fic][0]
#
# 4.3.3.3. ==> Ecriture
#
###  print motscfa
  IMPR_FICO_HOMA ( INFO=INFO, LANGUE = LANGUE, NON_SIMPLEXE = NON_SIMPLEXE, **motscfa )
#
###  for fic in dico.keys() :
###    print "\nContenu de ", fic
###    fichier = open (fic,'r')
###    les_lignes = fichier.readlines()
###    fichier.close()
###    for ligne in les_lignes :
###      print ligne[:-1]
#
# 4.3.3.4. ==> La fermeture des fichiers locaux
#              Remarque : il faut le faire ici pour que le gestionnaire de DEFI_FICHIER soit à jour
#              Remarque : aujourd'hui on est obligé de passer par le numéro d'unité logique
#
  for fic in dico.keys() :
    DEFI_FICHIER ( ACTION= "LIBERER", UNITE = dico[fic][2], INFO = INFO )
#
# 4.4. ==> Ecriture de la commande d'exécution de homard
#    Remarque : dans la donnée de la version de HOMARD, il faut remplacer
#               le _ de la donnee par un ., qui
#               est interdit dans la syntaxe du langage de commandes ASTER
#    Remarque : il faut remplacer le N majuscule de la donnee par
#               un n minuscule, qui est interdit dans la syntaxe du langage
#               de commandes ASTER
#    Remarque : pour le nommage des fichiers d'échange entre ASTER et HOMARD, on utilise
#               la convention implicite du fort.n des entrees/sorties au format MED
#
  VERSION_HOMARD=string.replace(VERSION_HOMARD,'_','.')
  VERSION_HOMARD=string.replace(VERSION_HOMARD,'N','n')
  if ( VERSION_HOMARD[-6:]=='_PERSO' ):
#    motscsi['ARGUMENT']=_F('TYPEXE'='-PERSO')
    VERSION_HOMARD=VERSION_HOMARD[:-6]
#
  if ( modhom == "ADAP" ) :
    Nom_Fichier_Donnees = '0'
#
  EXEC_LOGICIEL ( ARGUMENT = (_F(NOM_PARA=Rep_Calc_HOMARD_global), # nom du repertoire
                              _F(NOM_PARA=VERSION_HOMARD),  # version de homard
                              _F(NOM_PARA=str(INFO)),       # niveau d information
                              _F(NOM_PARA=Nom_Fichier_Donnees), # fichier de données HOMARD
                             ),
                  LOGICIEL = homard
                )
#
# 4.5. ==> Ecriture de la commande de lecture des resultats med
#          Remarque :
#          La fonction self.DeclareOut(a,b) focntionne ainsi :
#          a est une chaine de caracteres
#          b est la variable déclarée dans la commande
#          le but est de associer le contenu de b à la vaiable locale qui sera désignée par a
#          Exemple :
#          self.DeclareOut('maillage_np1',ADAPTATION['MAILLAGE_NP1'])
#          ==> la variable maillage_np1 est identifiée à l'argument 'MAILLAGE_NP1' du mot-clé ADAPTATION
#
  if ( modhom == "ADAP" ) :
#
# 4.5.1. ==> Le maillage
#
    self.DeclareOut('maillage_np1',ADAPTATION['MAILLAGE_NP1'])
    for dico in Liste_Maillages :
      if ( dico['Action'] == 'A_lire' ) :
        maillage_np1 = LIRE_MAILLAGE ( UNITE = Unite_Fichier_HOMARD_vers_ASTER,
                                       FORMAT = 'MED',
                                       NOM_MED = dico['NOM_MED'],
                                       VERI_MAIL = _F(VERIF='NON'), INFO_MED = INFO, INFO = INFO )
#
# 4.5.2. ==> Les champs
#
    for dico in Liste_Champs :
      if ( dico['Type_Champ'] == 'CHAMP' ) :
###        print dico
        self.DeclareOut('champ_maj',dico['CHAM_MAJ'])
        motscsi={}
        for cle in [ 'NUME_ORDRE', 'INST', 'PRECISION', 'CRITERE' ] :
          if dico.has_key(cle) :
            if ( dico[cle] != None ) :
              motscsi[cle] = dico[cle]
        if dico.has_key('NUME_ORDRE') :
          motscsi['NUME_PT'] = dico['NUME_ORDRE']
        champ_maj = LIRE_CHAMP ( UNITE = Unite_Fichier_HOMARD_vers_ASTER, FORMAT = 'MED',
                                 MAILLAGE = maillage_np1,
                                 NOM_MED = dico['NOM_MED'], NOM_CMP_IDEM = 'OUI', TYPE_CHAM = dico['TYPE_CHAM'],
                                 INFO = INFO, **motscsi )
#
#--------------------------------------------------------------------
# 5. Menage des fichiers MED et HOMARD devenus inutiles
#--------------------------------------------------------------------
#
  fic = os.path.join(Rep_Calc_HOMARD_global,"MAILL."+str(niter)+".HOM")
  Liste_aux = [ Fichier_ASTER_vers_HOMARD, Fichier_HOMARD_Entree, fic ]
  if ( modhom == "ADAP" ) :
    Liste_aux.append(Fichier_HOMARD_vers_ASTER)
    Liste_aux.append(Fichier_HOMARD_Sortie)
#
  for fic in Liste_aux :
    if ( INFO > 1 ) : print "Destruction du fichier ", fic
    if os.path.islink(fic) :
      try :
        os.unlink(fic)
      except os.error,codret_partiel :
        self.cr.warn("Code d'erreur de unlink : " + str(codret_partiel[0]) + " : " + codret_partiel[1])
        self.cr.warn("Impossible d'enlever le lien sur le fichier : "+fic)
        codret = codret + 1
    if os.path.isfile(fic) :
      try :
        os.remove(fic)
      except os.error,codret_partiel :
        self.cr.warn("Code d'erreur de remove : " + str(codret_partiel[0]) + " : " + codret_partiel[1])
        self.cr.warn("Impossible de détruire le fichier : "+fic)
        codret = codret + 1
###  print os.listdir(Rep_Calc_ASTER)
###  print os.listdir(Rep_Calc_HOMARD_global)
#
#--------------------------------------------------------------------
# 6. C'est fini !
#--------------------------------------------------------------------
#
  return codret
