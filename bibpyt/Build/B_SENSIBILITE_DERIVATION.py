#@ MODIF B_SENSIBILITE_DERIVATION Build  DATE 01/07/2003   AUTEUR GNICOLAS G.NICOLAS 
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

"""
import types

class SENSIBILITE_DERIVATION :
   """
   Classe des méthodes liées aux dérivations des commandes d'un jdc
   jdc = jeu de commandes en cours de traitement. Il est inchangé ici.
   memo_nom_sensi = mémorisation des noms liés à la sensibilité
   commande_MEMO_NOM_SENSI = compmande de mémorisation des noms liés à la sensibilité
   reel = la valeur réelle à sustituer aux réels dans une dérivation
   l_nom_sd_prod = liste des noms des sd produites par le jdc à la création
                   de la classe
   l_nouveaux_noms = liste des nouveaux noms déjà créés
   definition_param_sensi = faux tant que l'on n'a pas examiné une commande
                            définissant un paramètre sensible
   """
#
# ---------- Début du constructeur ----------
#
   def __init__(self,jdc,memo_nom_sensi,commande_memo_nom_sensi,DEBUG=None) :
#
       DEBUG_defaut = 0
       self.jdc = jdc
       self.memo_nom_sensi = memo_nom_sensi
       self.commande_memo_nom_sensi = commande_memo_nom_sensi
       self.DEBUG = DEBUG or DEBUG_defaut
#
       self.reel = None
       self.l_nom_sd_prod = []
       if jdc :
         for sd in self.jdc.sds :
           self.l_nom_sd_prod.append(sd.nom)
       self.l_nouveaux_noms = []
       self.definition_param_sensi = 0
       self.fonction_0 = None
       self.fonction_1 = None
#
# ---------- Fin du constructeur ----------
#
   def get_jdc(self) :
       """
       Récupère le jdc associé
       """
       return self.jdc
#
   def get_l_nouveaux_noms(self) :
       """
       Récupère la liste des nouveaux noms déjà créés
       """
       return self.l_nouveaux_noms
#
   def get_l_nom_sd_prod(self) :
       """
       Récupère la liste des noms des sd produites par le jdc à la création de la classe
       """
       return self.l_nom_sd_prod
#
   def put_fonction_0(self,fonction_0) :
       """
       Enregistre la fonction nulle
       Code de retour :  0, tout va bien
             1, la fonction nulle est déjà enregistrée
       """
       if self.fonction_0 :
         codret = 1
       else :
         codret = 0
         self.fonction_0 = fonction_0
       return codret
#
   def get_fonction_0(self) :
       """
       Récupère la fonction nulle associée
       """
       return self.fonction_0
#
   def put_fonction_1(self,fonction_1) :
       """
       Enregistre la fonction unité
       Code de retour :  0, tout va bien
                         1, la fonction nulle est déjà enregistrée
       """
       if self.fonction_1 :
         codret = 1
       else :
         codret = 0
         self.fonction_1 = fonction_1
       return codret
#
   def get_fonction_1(self) :
       """
       Récupère la fonction unité associée
       """
       return self.fonction_1
#
   def derivation_para_sensi(self,etape,param,new_jdc) :
       """
       Dérive une étape de définition d'un paramètre sensible
       Au tout appel à ce traitement :
        . On dupliquera deux fois la commande :
          . la première fois, c'est pour lui mettre une dérivée nulle ; pour cela, on simule
            une dérivation par rapport à n'importe quoi
          . la seconde fois, on obtient la dérivée par rapport à lui-meme, c'est-à-dire 1.
        . On mémorise les noms de ces nouvelles fonctions comme étant des
          fonctions nulle et unité.
       Aux appels suivants, on ne fait que la dérivation du paramètre par rapport à lui-meme.
       """

       if not self.definition_param_sensi :
         for nrpass in range(2) :
#        Création et enregistrement de la commande dérivée
           reel = float(nrpass)
           etape_derivee, l_mc_derives = self.derivation(etape,reel,None,new_jdc)
           new_jdc.register(etape_derivee)
           if nrpass == 1 :
             # Au 2nd passage :
             # . mémorisation du nom de la structure dérivée : dérivation du paramètre par lui-meme
             codret = self.memo_nom_sensi.add_nom_compose(etape.sd,param,etape_derivee.sd)
             txt = self.get_texte_memo_nom_sensi_compose(etape.sd.nom,param.nom,etape_derivee.sd.nom,l_mc_derives)
             exec txt in new_jdc.g_context
#          Mémorisation des deux fonctions dérivées créées, en tant que fonction nulle et fonction unité.
           if nrpass == 0 :
           # Au 1er passage :
           # . mémorisation de la fonction en tant que fonction nulle
             codret = self.put_fonction_0(etape_derivee.sd)
#             txt = self.get_texte_memo_nom_sensi_zero(etape_derivee.sd.nom)
#             print txt
#             exec txt in new_jdc.g_context
           else :
             # Au 2nd passage :
             # . mémorisation de la fonction en tant que fonction unite
              codret = self.put_fonction_1(etape_derivee.sd)
#              txt = self.get_texte_memo_nom_sensi_un(etape_derivee.sd.nom)
#             print txt
#             exec txt in new_jdc.g_context
         self.definition_param_sensi = 1

       else :
         fonction_1 = self.get_fonction_1()
         codret = self.memo_nom_sensi.add_nom_compose(etape.sd,param,fonction_1)
         txt = self.get_texte_memo_nom_sensi_compose(etape.sd.nom,param.nom,fonction_1.nom,None)
         exec txt in new_jdc.g_context

       codret = 0
       return codret
#
   def derivation_commande(self,etape,param,new_jdc,d_nom_s_c) :
       """
       Dérive une étape de commande autre que la définition de paramètre sensible
       """
#       Création et enregistrement de la commande dérivée
       reel = 0.
       etape_derivee, l_mc_derives = self.derivation(etape,reel,d_nom_s_c,new_jdc)
       new_jdc.register(etape_derivee)
       codret = self.memo_nom_sensi.add_nom_compose(etape.sd,param,etape_derivee.sd)
       if codret == 0 :
         txt = self.get_texte_memo_nom_sensi_compose(etape.sd.nom,param.nom,etape_derivee.sd.nom,l_mc_derives)
         exec txt in new_jdc.g_context
       codret = 0
       return codret
#
   def derivation(self,etape,reel,d_nom_s_c,new_jdc) :
       """
       Crée l'étape dérivée de l'étape passée en argument
       Retourne l'étape dérivée et la liste des mot-clés concernés
       par la dérivation de leur valeur
       """
#       print ".... Appel de derivation avec ",etape,reel,d_nom_s_c
       if self.DEBUG :
         print ".... Lancement de la copie pour dérivation de ",etape.nom
       etape_derivee = etape.copy()
       etape_derivee.reparent(new_jdc)
#       print "...... Fin de la copie pour dérivation de ",etape.nom
       sd_derivee = etape.sd.__class__(etape=etape_derivee)
       nom_sd_derivee = self.get_nouveau_nom_sd()
       etape_derivee.sd = sd_derivee
       if etape_derivee.reuse == None :
         new_jdc.NommerSdprod(sd_derivee,nom_sd_derivee)
       else :
         sd_derivee.nom = nom_sd_derivee
       etape_derivee.sdnom = nom_sd_derivee
       self.reel = reel
       self.d_nom_s_c = d_nom_s_c
       l_mc_derives = self.derive_etape(etape_derivee,new_jdc)
       return etape_derivee, l_mc_derives
#
#  ========== ========== ========== Début ========== ========== ==========
#  Les méthodes qui suivent servent à modifier les arguments de la commande
#  en cours de dérivation selon qu'ils sont réels, dérivés ou autre
#
   def derive_etape(self,etape,new_jdc) :
       """
       Réalise la dérivation des arguments de l'étape passée en argument
         - remplace tous les réels par reel (prudence !!!)
         - quand un argument possède une dérivée, on la met à la place
         - remplace toutes les autres fonctions par fonction_nulle
       Retourne la liste des mots-clés concernés par la dérivation de leur valeur 
       """
       liste = []
       mc_derive = None
       for child in etape.mc_liste :
         if child.nature == 'MCSIMP' :
           mc_derive = self.derive_mcsimp(None,child,new_jdc)
           if mc_derive :
             liste.append(mc_derive)
         elif child.nature in ('MCFACT','MCBLOC') :
           liste = liste + self.derive_mccompo(child,new_jdc)
         elif child.nature == 'MCList' :
           liste = liste + self.derive_mcliste(child,new_jdc)

       l_mc_derives = []
       for mc in liste :
         if mc not in l_mc_derives :
           l_mc_derives.append(mc)
       return l_mc_derives       
#
   def derive_mccompo(self,mc,new_jdc) :
       """
       Dérive en place le mccompo passé en argument 
       Retourne la liste des mots_clés concernés par la dérivation de leur valeur
       """
       liste = []
       for child in mc.mc_liste :
         if child.nature == 'MCSIMP' :
           mc_derive = self.derive_mcsimp(mc,child,new_jdc)
           if mc_derive :
             liste.append(mc_derive)
         elif child.nature == 'MCBLOC' :
           liste = liste + self.derive_mccompo(child,new_jdc)
#
       return liste
#
   def derive_mcliste(self,mc,new_jdc) :
       """
       Dérive en place la MCList passée en argument
       Retourne la liste des mots_clés concernés par la dérivation de leur valeur
       """
       liste = []
       for child in mc.data :
         liste = liste + self.derive_mccompo(child,new_jdc)
       return liste 
   
   def derive_mcsimp(self,mcfact,mcsimp,new_jdc) :
       """
       Dérive le mcsimp passé en argument
       Retourne None ou le tuple (mot-clé facteur, mot-clé simple, valeur) concerné
       par la dérivation
       """ 
       assert type(mcsimp.valeur) not in (types.ListType,types.TupleType), "Cas non traité : MCSIMP avec valeur == liste"
#       print 'Ancien :',mcsimp.valeur,' de type ',type(mcsimp.valeur)
       mc_derive = None
       if type(mcsimp.valeur) == types.FloatType :
         mcsimp.valeur = self.reel
       elif mcsimp.valeur in self.d_nom_s_c.keys() :
         mc_derive = (mcfact,mcsimp,mcsimp.valeur)
         mcsimp.valeur = self.d_nom_s_c[mcsimp.valeur]
       elif type(mcsimp.valeur) == types.InstanceType :
         if isinstance(mcsimp.valeur,new_jdc.g_context['fonction']) :
           mcsimp.valeur = self.fonction_0
#       print 'Nouveau :',mcsimp.valeur,' et mc_derive = ',mc_derive
       return mc_derive       
#
#  ========== ========== ========== Fin ========== ========== ==========
#  ========== ========== ========== Début ========== ========== ==========
#  Les méthodes qui suivent servent à mémoriser les correspondances entre
#  les noms simples et les noms composés
#
#
   def get_texte_memo_nom_sensi_compose(self,nom_simple,param_sensi,nom_compose,l_mc_derives) :
       """
       Récupère le texte de la commande ASTER pour l'enregistrement du nom
       composé associé à un nom simple et à un paramètre sensible
       On ajoute la liste des mots-clés correspondant à la dérivation.
       """
       if self.DEBUG :
         print ".... Commande de mémorisation des noms :"
       texte = self.commande_memo_nom_sensi+"(NOM=_F(NOM_SD='%s',PARA_SENSI=%s,NOM_COMPOSE='%s'"%(nom_simple,param_sensi,nom_compose)
       if l_mc_derives :
         texte_mc = ",MOT_CLE=("
         texte_va = ",VALEUR=("
         texte_mf = ",MOT_FACT=("
         for mot_cle in l_mc_derives :
           if mot_cle[0] :
             aux = mot_cle[0].nom
           else :
             aux = " "
           texte_mf = texte_mf + "'%s',"%(aux)
           texte_mc = texte_mc + "'%s',"%(mot_cle[1].nom)
           texte_va = texte_va + "'%s',"%(mot_cle[2].nom)
         texte_mf = texte_mf + ")\n"
         texte_mc = texte_mc + ")\n"
         texte_va = texte_va + ")\n"
         texte = texte + texte_mf + texte_mc + texte_va
       texte = texte + "));\n"
       if self.DEBUG :
         print ".... ", texte
       return texte
#
   def get_texte_memo_nom_sensi_zero(self,nom_fonction) :
       """
       Récupère le texte de la commande ASTER pour l'enregistrement du nom
       de la fonction nulle
       """
       texte = self.commande_memo_nom_sensi+"(NOM_ZERO = %s);\n" %(nom_fonction)
       return texte
#
   def get_texte_memo_nom_sensi_un(self,nom_fonction) :
       """
       Récupère le texte de la commande ASTER pour l'enregistrement du nom
       de la fonction unite
       """
       texte = self.commande_memo_nom_sensi+"(NOM_UN = %s);\n" %(nom_fonction)
       return texte
#
#  ========== ========== ========== Fin ========== ========== ==========
#
   def get_nouveau_nom_sd(self) :
       """
       Retourne un nom de sd jamais utilisé
       Ajoute ce nom à la liste des noms de sd utilisées par le jdc en cours
       """
#       print "self.l_nom_sd_prod   = ",self.l_nom_sd_prod
#       print "self.l_nouveaux_noms = ",self.l_nouveaux_noms
       i = 9999999
       nouveau_nom_sd = 'S' + str(i)
       while nouveau_nom_sd in self.l_nom_sd_prod + self.l_nouveaux_noms :
         i = i - 1
         nouveau_nom_sd = 'S' + str(i)
       self.l_nouveaux_noms.append(nouveau_nom_sd)
       return nouveau_nom_sd         
#
#
if __name__ == "__main__" :
#
#
  derivation = SENSIBILITE_DERIVATION(None,None,"MEMO_NOM_SENSI",1)
  print "\n",derivation
  print "jdc        : ", derivation.get_jdc()
  print "l_nouveaux_noms      : ", derivation.get_l_nouveaux_noms()
  print "l_nom_sd_prod : ", derivation.get_l_nom_sd_prod()
  print "fonction_0 : ", derivation.get_fonction_0()
  fonction_0 = "f_0"
  print derivation.put_fonction_0(fonction_0)
  print "fonction_0 : ", derivation.get_fonction_0()
  fonction_0 = "f_0_bis"
  print derivation.put_fonction_0(fonction_0)
  print "fonction_0 : ", derivation.get_fonction_0()
  d_param = {}
  d_param ['PS1'] = None
  d_param ['PS2'] = ['LAMBDA']
  d_param ['PS3'] = ['E_L','E_T']
#  for param in d_param.keys() :
#    print "Commande ASTER pour 'CH1' + ",param," = 'gabuzome : \n",derivation.get_texte_memo_nom_sensi_compose('CH1',param,'gabuzome',d_param[param])
  print "Commande ASTER pour ZERO : ", derivation.get_texte_memo_nom_sensi_zero('fonc_0')
  print "Commande ASTER pour UN   : ", derivation.get_texte_memo_nom_sensi_un('fonc_1')
