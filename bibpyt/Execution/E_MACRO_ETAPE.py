#@ MODIF E_MACRO_ETAPE Execution  DATE 30/01/2006   AUTEUR DURAND C.DURAND 
# -*- coding: iso-8859-1 -*-
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
#                                                                       
#                                                                       
# ======================================================================


import E_ETAPE
from os import times
import aster
import string
from Noyau.N__F import _F

class MACRO_ETAPE(E_ETAPE.ETAPE):
   """
   Cette classe implémente les méthodes relatives à la phase d'execution
   des macro-commandes.

   Les principales méthodes sont:
      - Exec, pour l'execution en mode par lot
      - Execute, pour l'execution en mode commande par commande (par_lot="NON")
   """

   def AfficheCommande(self):
      """
      Affiche l'echo de la macro commande
      """
      if self.definition.proc is not None or self.definition.op < 0 :
         # affichage du texte de la macro-commande. 
         self.AfficheTexteCommande()

   def affiche_cmd(self):
      if self.jdc.par_lot=="NON":
          self.AfficheCommande()

   def Execute(self):
      """ 
      Cette methode realise la phase d'execution en mode commande
      par commande pour une etape : 
             - construction, 
             - verification, 
             - execution
      en une seule passe. Utilise en mode par_lot='NON'

      L'attribut d'instance executed indique que l'etape a deja ete executee
      Cette methode peut etre appelee plusieurs fois mais ne doit etre
      executee qu'une seule fois.
      Les seuls cas ou on appelle plusieurs fois Execute sont pour les
      commandes INCLUDE et INCLUDE_MATERIAU (appel dans op_init)
      """
      if not self.jdc or self.jdc.par_lot != "NON" :
         return

      if not hasattr(self,"executed") or self.executed == 0:
         self.executed=1

         cr=self.report()
         self.parent.cr.add(cr)
         if not cr.estvide():
           raise EOFError

         try:
             # Apres l appel a Build  les executions de toutes les 
             # sous commandes ont ete realisees
             ier=self.Build()
         except self.codex.error:
             self.detruit_sdprod()
             raise

         if ier > 0 :
            # On termine le traitement
            cr.fatal("Erreurs dans la construction de la macro %s" % self.nom)
            raise EOFError

         self.setmode(1)
         E_ETAPE.ETAPE.Exec(self)
         self.setmode(2)
         E_ETAPE.ETAPE.Exec(self)

         if self.icmd!=None :
            self.cpu_user=times()[0]-self.cpu_user_0
            self.cpu_syst=times()[1]-self.cpu_syst_0
            self.AfficheFinCommande(self.cpu_user,self.cpu_syst)       
         else :
            self.AfficheFinCommande(None,None)       
      elif self.nom == 'INCLUDE':
            self.AfficheFinCommande(None,None)       

      if hasattr(self,'postexec'):
         self.postexec(self)

      # Destruction des concepts temporaires internes à la macro
      # préfixés par '.' dans la base jeveux
      # Pour ne pas avoir l'affichage en cas de IMPR_MACRO='NON'
      # on déclare l'étape DETRUIRE fille de la macro en cours
      # par set_current_step
      self.set_current_step()
      if self.nom!='DETRUIRE' :
         l_obj=[]
         for etape in self.etapes :
             if etape.sd!=None :
                if etape.sd.nom[:1]=='.' :l_obj.append(etape.sd.nom)
         if self.sd!=None : 
                if self.sd.nom in l_obj  :l_obj.remove(self.sd.nom)
         if l_obj!=[] :
             l_detr=dict([(i,0) for i in l_obj]).keys()
             DETRUIRE=self.get_cmd('DETRUIRE')
             DETRUIRE(CONCEPT=_F(NOM=l_detr),ALARME='NON',INFO=1)
      self.reset_current_step()

   def Execute_alone(self):
      """
      Cette methode est une methode speciale reservee au traitement de
      certaines macro-commandes (INCLUDE et INCLUDE_MATERIAU) en mode par_lot='NON'

      Elle realise l execution d une etape :
             - construction,
             - verification,
             - execution
      en une seule passe. Utilise en mode par_lot='NON'.
      Cette methode est semblable a Execute mais appelle Build_alone
      au lieu de Build (permet d'executer la macro avant de construire les sous commandes)

      L'attribut d'instance executed indique que l'etape a deja ete executee.
      Les methodes Execute et Execute_alone peuvent etre appelees plusieurs fois mais 
      l'execution effective ne doit avoir lieu qu'une seule fois.
      Les seuls cas ou on appelle plusieurs fois Execute_alone et Execute sont pour les
      commandes INCLUDE et INCLUDE_MATERIAU (appel dans op_init)
      """
      if hasattr(self,"executed") and self.executed == 1:return
      self.executed=1

      cr=self.report()
      self.parent.cr.add(cr)
      if not cr.estvide():
        raise EOFError

      ier=self.Build_alone()

      if ier > 0 :
        # On termine le traitement
        cr.fatal("Erreurs dans la construction de la macro %s" % self.nom)
        raise EOFError

      self.setmode(1)
      E_ETAPE.ETAPE.Exec(self)
      self.setmode(2)
      E_ETAPE.ETAPE.Exec(self)


   def BuildExec(self):
      """ 
      Cette methode enchaine en une seule passe les phases de construction et d'execution
      """
      self.set_current_step()
      self.building=None
      # Chaque macro_etape doit avoir un attribut cr du type CR
      # (compte-rendu) pour stocker les erreurs eventuelles
      # et doit l'ajouter au cr de l'etape parent pour construire un
      # compte-rendu hierarchique
      self.cr=self.CR(debut='Etape : '+self.nom + '    ligne : '+`self.appel[0]` + '    fichier : '+`self.appel[1]`,
                       fin = 'Fin Etape : '+self.nom)

      self.parent.cr.add(self.cr)

      # Si la liste des etapes est remplie avant l'appel à Build
      # on a affaire à une macro de type INCLUDE
      # Il faut executer les etapes explicitement apres Build
      if self.etapes:has_etapes=1
      else: has_etapes=0

      try:
         # Apres l appel a _Build  les executions de toutes les 
         # sous commandes ont ete realisees sauf dans le cas des INCLUDE
         ier = self._Build()

         if ier > 0 :
           # On termine le traitement
           self.cr.fatal("Erreurs dans la construction de la macro %s" % self.nom)
           raise EOFError

         # La macro de type INCLUDE doit etre executee avant ses sous etapes
         # principalement pour INCLUDE_MATERIAU qui doit definir un prefixe avant
         self.setmode(1)
         E_ETAPE.ETAPE.Exec(self)
         self.setmode(2)
         E_ETAPE.ETAPE.Exec(self)

         if has_etapes:
            for e in self.etapes:
              if e.isactif():
                 e.BuildExec()

         if hasattr(self,'postexec'):
            self.postexec(self)
         if self.icmd!=None :
            self.cpu_user=times()[0]-self.cpu_user_0
            self.cpu_syst=times()[1]-self.cpu_syst_0
            self.AfficheFinCommande(self.cpu_user,self.cpu_syst)       
         else :
            self.AfficheFinCommande(None,None)       
      except:
         self.reset_current_step()
         raise

      # Destruction des concepts temporaires internes à la macro
      # préfixés par '.' dans la base jeveux
      # Pour ne pas avoir l'affichage en cas de IMPR_MACRO='NON'
      # on déclare l'étape DETRUIRE fille de la macro en cours
      # par set_current_step
      if self.nom!='DETRUIRE' :
         l_obj=[]
         for etape in self.etapes :
             if etape.sd!=None :
                if etape.sd.nom[:1]=='.' :l_obj.append(etape.sd.nom)
         if self.sd!=None : 
                if self.sd.nom in l_obj  :l_obj.remove(self.sd.nom)
         if l_obj!=[] :
             l_detr=dict([(i,0) for i in l_obj]).keys()
             DETRUIRE=self.get_cmd('DETRUIRE')
             DETRUIRE(CONCEPT=_F(NOM=l_detr),ALARME='NON',INFO=1)
      self.reset_current_step()

   def get_liste_etapes(self,liste):
      if self.nom=='INCLUDE' : 
         for e in self.etapes:
             e.get_liste_etapes(liste)
      else :
         liste.append(self.etape)

