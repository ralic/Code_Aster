#@ MODIF E_MACRO_ETAPE Execution  DATE 27/03/2002   AUTEUR DURAND C.DURAND 
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
class MACRO_ETAPE(E_ETAPE.ETAPE):
   def Exec(self):
      """ 
         Cette methode lance l'execution de la macro.
         L'execution de la macro comprend : l'execution de toutes
         les sous etapes puis l'execution de la macro proprement dite
      """
      for e in self.etapes:
        if e.isactif():
           e.Exec()
      E_ETAPE.ETAPE.Exec(self)
      if self.definition.proc != None:
         # seulement affichage du texte de la commande. Il s'agit d'une macro Python
         self.AfficheTexteCommande()


   def Execute(self):
      """ 
          Cette methode realise l execution d une etape : construction, verification, execution
          en une seule passe. Utilise en mode par_lot='NON'
      """
      cr=self.report()
      self.parent.cr.add(cr)
      if not cr.estvide():
        raise EOFError
      if self.definition.op in self.NOCMDOPER :return
      # Apres l appel a Build normalement toutes les executions des commandes
      # internes ont ete realisees
      ier=self.Build()
      if ier > 0 and ier != 9999:
        # On termine le traitement
        cr.fatal("Erreurs dans la construction de la macro %s" % self.nom)
        raise EOFError
      self.setmode(1)
      E_ETAPE.ETAPE.Exec(self)
      self.setmode(2)
      E_ETAPE.ETAPE.Exec(self)

