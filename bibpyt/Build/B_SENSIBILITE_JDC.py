#@ MODIF B_SENSIBILITE_JDC Build  DATE 30/03/2004   AUTEUR GNICOLAS G.NICOLAS 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
Cette classe pilote les modifications de jdc dans le cas des calculs de sensibilité.
Ces attributs sont :
jdc : le jdc à analyser. C'est l'objet obtenu après les vérifications d'usage et le
      traitement des macro-commandes éventuelles. Il n'est modifié par aucune des
      méthodes de cette classe.
commandes_sensibles : classe SENSIBILITE_COMMANDES_SENSIBLES contenant toutes les
                      informations pour faire les modifications.
                      ce sont les listes des commandes déclanchant le processus,
                      les commandes à dériver, etc.
Attention : aucun test n'a été fait quand une commande DETRUIRE est incluse dans le JDC.
Cela posera des problèmes si c'est la destruction d'un concept impacté par la dérivation.
"""
import copy

from Noyau.N_utils import prbanner
from B_SENSIBILITE_DERIVATION          import SENSIBILITE_DERIVATION
from B_SENSIBILITE_MEMO_NOM_SENSI      import SENSIBILITE_MEMO_NOM_SENSI
from B_SENSIBILITE_COMMANDES_SENSIBLES import SENSIBILITE_COMMANDES_SENSIBLES

class SENSIBILITE_JDC :
   """
   """
   def __init__(self,jdc):
      self.jdc = jdc
      self.commandes_sensibles = SENSIBILITE_COMMANDES_SENSIBLES()
#       print self.jdc
#       print self.commandes_sensibles
      if CONTEXT.debug :
        DEBUG = 1
      else :
        DEBUG = 0
      self.DEBUG = DEBUG
 
# ---------------------------------------------------------------------------
#           Méthodes liées aux études de sensibilité
# ---------------------------------------------------------------------------

   def is_sensible(self) :
      """
      Retourne deux entiers :
      . erreur : 0 : tout s'est bien passé
                 1 : une commande MEMO_NOM_SENSI est présente dans le jdc
      . est_sensible : 1 : le jdc fait l'objet d'une étude de sensibilité
                       0 : pas de sensibilite dans ce jdc.
      Les paramètres concernés sont mémorisés. On a alors deux listes : les 'vrais' paramètres
      sensibles et les autres (champs theta par exemple)

      La technique est la suivante :
      . A priori, pas de sensibilité.
      . On parcourt toutes les étapes du jdc. Pour chacune d'elles :
       . Si l'étape est une définition de paramètre sensible, on mémorise la sd produite.
       . Si l'étape est une commande "sensible", on cherche dans ses données la présence d'une sd
         représentant un paramètre sensible connu. Si oui, on déclare qu'il y a de la sensibilité
         et on mémorise la sd / paramètre sensible impliquée.
         
      """
#
      if self.DEBUG :
        prbanner("Recherche de la sensibilité du JDC : " + self.jdc.nom)
#
# 1. Récupération des caractérisations des commandes sensibles
# 1.1. mot clé pilotant le calcul de sensibilité. C'est sa présence dans les commandes "sensibles"
#      qui caractérisera un calcul avec sensibilité.
      mot_cle = self.commandes_sensibles.mot_cle
      if self.DEBUG :
        print "Mot clé pilotant le calcul de sensibilité = ", mot_cle

# 1.2. commande mémorisant les noms pour la sensibilité
      commande_memo_nom_sensi = self.commandes_sensibles.memo_nom_sensi
      if self.DEBUG :
        print "Commande mémorisant les noms pour la sensibilité = ", commande_memo_nom_sensi

# 1.3. liste des commandes de définition des paramètres sensibles
      l_commandes_defi_para_sensi = self.commandes_sensibles.get_l_commandes_defi_para_sensi()
      if self.DEBUG :
        print "Liste des commandes de définition des paramètres sensibles : ", l_commandes_defi_para_sensi

# 1.4. liste des commandes qui pilotent la sensibilité (ce sont les commandes principales de calcul
#      thermique ou mécanique)
      l_commandes_sensibles = self.commandes_sensibles.get_l_commandes_sensibles()
      if self.DEBUG :
        print "Liste des commandes qui pilotent la sensibilité : ", l_commandes_sensibles
#
# 2. Analyse des étapes du jdc
#
      l_classe_para_sensi = []
      l_param_sensible = []
      l_param_autres = []
      est_sensible = 0
      erreur = 0
#
      for etape in self.jdc.etapes:
#        print "\n. Etape : ",etape.nom
#
# 2.1. L'étape est une définition de paramètre sensible
#      On mémorise la classe correspondant à la sd produite.
#
        if etape.nom is commande_memo_nom_sensi :
          print "La commande ",commande_memo_nom_sensi," est interdite dans un jeu de commandes."
          erreur = 1
#
# 2.2. L'étape est une définition de paramètre sensible
#      On mémorise la classe correspondant à la sd produite.
#
        elif etape.nom in l_commandes_defi_para_sensi :
          erreur0 = self.commandes_sensibles.add_classe_para_sensi(etape.sd.__class__)
          l_classe_para_sensi = self.commandes_sensibles.get_classe_para_sensi()
#
# 2.3. L'étape est une commande qui réalise un calcul de sensibilité
#      On parcourt tous les mots-clés qui sont renseignés dans le jdc en examen
#      Tant que ce n'est pas le mot-clé de la sensibilité, on ne fait rien.
#
        elif etape.nom in l_commandes_sensibles :
#
          for child in etape.mc_liste :

#           L'un des mots-clés renseignés est celui de la sensibilité : 
            if child.nom == mot_cle :
#             On déclare etre en présence d'un calcul sensible
              est_sensible = 1
              if self.DEBUG :
                print ".. Repérage de sensibilité dans la commande ",etape.nom

#             On récupère la liste des arguments associés à ce mot-clé
              if type(child.valeur) in ( type(()),type([])):
                liste = child.valeur
              else:
                liste = [child.valeur]

#             Un argument peut etre de deux types :
#             . soit c'est un paramètre sensible stricto sensu ; on le sait en regardant
#               s'il fait partie des paramètres sensibles définis précédemment dans le jdc
#             . soit c'est un autre type de paramètres de dérivation (champ theta par exemple)
#             On parcourt donc tous les arguments pour faire le tri et les stocker dans
#             les 2 listes ad-hoc.
              for argu in liste :
                if self.DEBUG :
                  print ".... Paramètre sensible : ",argu.nom
                aux = 0
                for classe_para_sensi in l_classe_para_sensi :
                  if argu.__class__ is classe_para_sensi :
                    aux = 1
#
                if aux :
                  if argu not in l_param_sensible :
                    l_param_sensible.append(argu)
                else :  
                  if argu not in l_param_autres :
                    l_param_autres.append(argu)
#      
      self.l_param_sensible = l_param_sensible
      self.l_param_autres   = l_param_autres
#
      if self.DEBUG :
        if est_sensible :
          print "\nRécapitulatif :"  
          for l_param in [l_param_sensible,l_param_autres] :
            liste = []
            for param in l_param :
              liste.append((param.nom,param))
            if l_param is l_param_sensible :
              txt = "sensibles"
            else :
              txt = "autres"
            print "Liste des paramètres "+txt+" : ",liste  
          texte = "est"
        else :
          texte = "n'est pas"
        prbanner("Le JDC "+texte+" concerné par la sensibilité.")
        print "\nCode de retour : ",erreur 
#
      return erreur, est_sensible
#
   def new_jdc(self) :
      """
      Construit un objet JDC copie de self augmentée des commandes de dérivation
      Retourne :
      . erreur : ==0 : tout s'est bien passé
                 !=0 : problème
      . le nouveau jdc
      """
      if self.DEBUG :
        prbanner("Création d'un JDC dérivé à partir de : " + self.jdc.nom)
#
#      print "dans new_jdc : ",self.commandes_sensibles
#      print "l_param_sensible = ",self.l_param_sensible
#      print "l_param_autres = ",self.l_param_autres
# 1. Préparation
#
      if self.DEBUG :
        print ". 1. Préparation"
#
      erreur = 0
#
# 1.1. Récupération des différentes listes de commandes impliquées dans la dérivation
#     l_commandes_a_deriver = liste des commandes à dériver si un de leurs arguments l'a été
      l_commandes_a_deriver = self.commandes_sensibles.get_l_commandes_a_deriver()
#     l_commandes_a_deriver_ensemble = liste des commandes à dériver dans tous les cas
      l_commandes_a_deriver_ensemble = self.commandes_sensibles.get_l_commandes_a_deriver_ensemble()
#     l_commandes_sensibles = liste des commandes qui pilotent la sensibilité
      l_commandes_sensibles = self.commandes_sensibles.get_l_commandes_sensibles()
#     d_commandes_sensibles_speciales = dictionnaire des commandes à dériver selon les mots-clés
      d_commandes_sensibles_speciales = self.commandes_sensibles.get_d_commandes_sensibles_speciales()
#     l_commandes_poursuite = liste des commandes qui pilotent une reprise de calcul
      l_commandes_poursuite = self.commandes_sensibles.get_l_commandes_poursuite()
#     commande mémorisant les noms pour la sensibilité
      commande_memo_nom_sensi = self.commandes_sensibles.memo_nom_sensi
#       print "Liste des commandes sensibles          : ", l_commandes_sensibles
#       print "Liste des commandes à dériver          : ", l_commandes_a_deriver
#       print "Liste des commandes à dériver ensemble : ", l_commandes_a_deriver_ensemble
#       print "Liste des commandes de poursuite       : ", l_commandes_poursuite
#       print "Commandes mémorisant des noms          : ", commande_memo_nom_sensi
#
      mot_cle = self.commandes_sensibles.mot_cle
#
# 1.2. Création de la classe qui mémorisera les noms des différentes sd impliquées
#      dans la sensibilité. Elle est initialisée avec la liste des sd par rapport auxquelles
#      on dérive.
#
      if self.DEBUG :
        print ".. Création d'une instance de la classe SENSIBILITE_MEMO_NOM_SENSI"
      memo_nom_sensi = SENSIBILITE_MEMO_NOM_SENSI(self.l_param_sensible+self.l_param_autres)
#       print "memo_nom_sensi.get_l_param_sensi() = ",memo_nom_sensi.get_l_param_sensi()

# 1.3. Création de la classe qui contiendra les méthodes des dérivations du jdc
#
      if self.DEBUG :
        print ".. Création d'une instance de la classe SENSIBILITE_DERIVATION"
      derivation = SENSIBILITE_DERIVATION(self.jdc,memo_nom_sensi,commande_memo_nom_sensi,self.DEBUG)

# 1.4. Certaines commandes, DEFI_MATERIAU par exemple, peuvent apparaitre plusieurs fois dans le jdc.
#      Si l'une est concernée par un paramètre sensible, toutes doivent etre dérivées
#      par rapport à ce paramètre.
#      On indique ici pour chaque paramètre quelles commandes sont à dériver d'office.
#
      if len (self.l_param_sensible) != 0 :
        for etape in self.jdc.etapes:
          if etape.isactif() :
            if etape.nom in l_commandes_a_deriver_ensemble :
              on_derive = 0
              l_sd_utilisees = etape.get_sd_utilisees()
              for param in self.l_param_sensible :
                for sd_u in l_sd_utilisees :
                  if sd_u is param :
                    erreur = memo_nom_sensi.add_commande(param,etape.nom)
                    if erreur : break
#
# 1.5. Le calcul est-il une reprise depuis un calcul précédent ?
#      Il suffit d'analyser la première commande active.
#
      if not erreur :
#
        poursuite = 0
        for etape in self.jdc.etapes:
          print etape.nom
          if etape.isactif() :
            if etape.nom in l_commandes_poursuite :
              poursuite = 1
            break
#
        if self.DEBUG :
          print "Poursuite : ",poursuite
#
# 2. Création du nouveau JDC
#     Elle se fait avec les principaux arguments du jdc à modifier, en particulier le catalogue des commandes
#     En cas de poursuite, il faut charger le contexte pour connaitre les objets déja construits.
#
      if not erreur :
#
        if self.DEBUG :
          print ". 2. Création du nouveau JDC"
#
        if poursuite :
          context_ini = self.jdc.g_context
        else :
          context_ini = None
#
        new_jdc = self.jdc.definition(cata=self.jdc.cata,appli=self.jdc.appli,procedure="#\n",context_ini=context_ini)
#
        new_jdc.compile()
        if not new_jdc.cr.estvide(): 
          self.MESSAGE("ERREUR DE COMPILATION DANS ACCAS - INTERRUPTION")
          print ">> JDC.py : DEBUT RAPPORT"
          print new_jdc.cr
          print ">> JDC.py : FIN RAPPORT"
          new_jdc.supprime()
          sys.exit(0)
#
        new_jdc.exec_compile()
        if not new_jdc.cr.estvide(): 
          self.MESSAGE("ERREUR A L'INTERPRETATION DANS ACCAS - INTERRUPTION")
          print ">> JDC.py : DEBUT RAPPORT"
          print new_jdc.cr
          print ">> JDC.py : FIN RAPPORT"
          new_jdc.supprime()
          sys.exit(0)
#
        CONTEXT.set_current_step(new_jdc)
#
        if self.DEBUG :
          print "Le nouveau JDC est créé en tant qu'objet : ",new_jdc
#
# 3. On parcourt toutes les étapes du JDC initial
#    On ne s'occupe que des étapes actives et qui ne sont pas des commentaires
#
      if ( not erreur and self.DEBUG ) :
        print ". 3. On parcourt toutes les étapes du JDC initial"
#
      for etape in self.jdc.etapes :
        if self.DEBUG :
           print "\nAAAAAAAAAAAAAAAAA TRAITEMENT DE ",etape.nom," AAAAAAAAAAAAAAAAA"
        if etape.isactif() :
          if hasattr(etape,'sd') :
        # 3.1. Copie telle quelle de l'étape initiale
#            print "Lancement de la copie de ",etape.nom
            new_etape = etape.full_copy(parent=new_jdc)
#            print "Fin de la copie de ",etape.nom
            if self.DEBUG :
              print ".. L'étape a été recopiée à l'identique."

# 3.2. On repère si c'est une commande sensible ou non :
#        . Si c'est une commande sensible, elle va produire les sd dérivées en son sein. Ces
#          sd doivent avoir été déclarées dans MEMO_NOM_SENSI auparavant ; on
#          enregistrera donc la commande sensible après.
#        . Sinon, il faut l'enregistrer tout de suite pour que la sd produite soit
#          connue avant l'appel à MEMO_NOM_SENSI
            if etape.nom in l_commandes_sensibles :
              enregistrement_tardif = 1
            else :
              enregistrement_tardif = 0
              new_jdc.register(new_etape)
#              print ". enregistrement immediat de  = ",etape.nom

# 3.3. Si l'étape produit une sd, on va la dériver dans chacun des cas suivants :
#        . C'est la définition d'un paramètre sensible stricto sensu
#        . C'est la définition d'un paramètre sensible d'un autre type
#        . La commande est à dériver obligatoirement ou car un de ses arguments
#          est lui-meme issu d'une dérivation par rapport à un paramètre sensible
#        On remarque que ces cas sont exclusifs, d'où un filtrage avec la variable a_faire.
            if etape.sd :
              a_faire = 1
#              print "type(etape.definition.sd_prod) = ",type(etape.definition.sd_prod)

# 3.3.1. Dérivation d'une définition de paramètre sensible
#
              for param in self.l_param_sensible :
                if param is etape.sd :
                  if self.DEBUG :
                    print ".. Lancement de la dérivation"
                  erreur = derivation.derivation_para_sensi(etape,param,new_jdc)
                  if erreur : break
                  a_faire = 0

# 3.3.2. Dérivation d'une définition de paramètre autre
#
              if a_faire :
                for param in self.l_param_autres :
                  if param is etape.sd :
                    if self.DEBUG :
                      print ".. Lancement de la dérivation"
                    erreur = derivation.derivation_para_autre(etape,param,new_jdc)
                    if erreur : break
                    a_faire = 0

# 3.3.3. Dérivation d'un autre type de commande
#          Remarques :
#          . Les seules commandes concernées sont les commandes sensibles ou à dériver
#          . On répète l'opération pour chacun des paramètres de dérivation effectifs
              if a_faire :
                if etape.nom in l_commandes_sensibles + l_commandes_a_deriver + l_commandes_a_deriver_ensemble :
                  for param in self.l_param_sensible + self.l_param_autres :

# 3.3.3.1. Récupération du dictionnaire des couples (nom simple,nom composé) existant pour ce paramètre
                    erreur,d_nom_s_c = memo_nom_sensi.get_d_nom_s_c(param)
                    if erreur : break
#
# 3.3.3.2. Doit-on effectivement dériver par rapport au paramètre param ?
#         . Pour une commande du type 'à dériver ensemble', on ne le fait que si l'une d'entre
#           elles est impliquée par le paramètre sensible.
#         . Pour une commande non nécessairement à dériver, on recherche si parmi les sd utilisées
#           par la commande, il y en a au moins une qui a été dérivée par rapport à param
#           Remarque : plusieurs sd peuvent répondre à cela ; il suffit de décrocher dès
#           qu'une a été trouvée car la dérivation les prendra toutes en compte
#
                    on_derive = 0
                    if etape.nom in l_commandes_a_deriver_ensemble :
                      erreur, l_commandes = memo_nom_sensi.get_l_commandes(param)
                      if erreur : break
                      if etape.nom in l_commandes :
                        on_derive = 1
                    else :
                      l_sd_utilisees = etape.get_sd_utilisees()
                      for sd_u in l_sd_utilisees :
                        for sd_d in d_nom_s_c.keys() :
                          if sd_u is sd_d :
                            on_derive = 1
                      if on_derive :
                        if etape.nom not in l_commandes_sensibles :
                          daux = etape.get_sd_mcs_utilisees()
                          if self.DEBUG :
                            print ".. Dictionnaire des sd utilisées : ", daux
                          if daux.has_key(mot_cle) : on_derive = 0
# 3.3.3.3. Remarque : certaines commandes, comme CREA_CHAMP par exemple, doivent etre traitées
#          avec sagesse. En effet, soit elles sont utilisées pour préparer le calcul et doivent
#          alors etre dérivées ; soit elles sont utilisées en post-traitement et ne doivent pas
#          l'etre. Le tri se fait en fonction de valeurs de mot-clés renseignés en dur.
                    if on_derive :
                      if d_commandes_sensibles_speciales.has_key(etape.nom) :
                        d_aux = d_commandes_sensibles_speciales[etape.nom]
                        on_derive = derivation.derivation_speciale(etape,d_aux)
# 3.3.3.4. On doit dériver par rapport au paramètre param ; alors on y va ...
                    if on_derive :
# 3.3.3.4.1. Si la commande en elle-meme est à dériver, on y va
                      if etape.nom in l_commandes_a_deriver + l_commandes_a_deriver_ensemble :
                        if self.DEBUG :
                          print ".. Lancement de la dérivation par derivation_commande"
                        erreur = derivation.derivation_commande(etape,param,new_jdc,d_nom_s_c)
                        if erreur : break
                      else :   
# 3.3.3.4.2. Sinon, il sufit de créer et de mémoriser un nouveau nom de sd. On commence par vérifier que cela
#            n'a pas déjà été fait. Cela arrive quand un opérateur est appelé plusieurs fois de suite.
#            Quand le nom est nouveau, il faut l'enregistrer dans le JDC via la commande MEM0_NON_SENSI
                        if self.DEBUG :
                          print ".. Recherche d'un nouveau nom composé"
                        erreur, nom_sd_derivee = memo_nom_sensi.get_nom_compose(etape.sd,param)
                        if erreur == 2 : 
                          nom_sd_derivee = derivation.get_nouveau_nom_sd()
                          erreur = memo_nom_sensi.add_nom_compose(etape.sd,param,nom_sd_derivee)
                          if erreur : break
#                          print "... code de retour de memo_nom_sensi.add_nom_compose = ",erreur
                          if erreur == 0 : 
                            txt = derivation.get_texte_memo_nom_sensi_compose(etape.sd.nom,param.nom,nom_sd_derivee,None)
                            exec txt in new_jdc.g_context

# 3.3.4. Enregistrement final si cela n'a pas été fait
            if enregistrement_tardif :
               new_jdc.register(new_etape)
#               print ". enregistrement tardif de  = ",etape.nom
#
# 3.4. Message éventuel
          if erreur :
            print ".. Probleme dans la dérivation de ",etape.nom
            break
          if self.DEBUG :
            print ".. Fin du traitement avec erreur = ", erreur
#
# 4. Dans le cas de poursuite, il faut dire que les bases ont déjà été ouvertes
#
      if not erreur :
#
        if self.DEBUG :
          print ". 4. Cas de poursuite"
#
        if poursuite :
          new_jdc.ini = 1
#
        CONTEXT.unset_current_step()            
#
# 5. Fin
#
      if self.DEBUG :
        print "\n. 5. Nouveau JDC"
        for etape in new_jdc.etapes:
          print "Etape : ",etape.nom
        print "\n. "
        print "\nCode de retour : ",erreur 
        prbanner("Fin de la création du JDC dérivé.")
#
      return erreur, new_jdc
