#@ MODIF E_ETAPE Execution  DATE 27/03/2002   AUTEUR DURAND C.DURAND 
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
"""
"""
# Modules Python
import types,sys

# Modules Eficas
from Noyau.N_utils import prbanner
from Noyau.N_Exception import AsException
import genpy

class ETAPE:
   """
   """
   NOEXECOPER= (
             0, # DEBUT ou POURSUITE
            -1  # INCLUDE
           ,-2  # RETOUR
           ,-3  # PROCEDURE
           ,-14 # INCLUDE_MATERIAU
           )

   def Exec(self):
      """
      """
      if self.definition.op != None and self.definition.op not in self.NOEXECOPER :
        # on execute l operateur numero self.definition.op
        # on lui passe les arguments :
        # self : la commande courante
        # lot : le mode d'execution
        #        0 = par lot (verification globale avant execution)
        #        1 = commande par commande (verification + execution commande par commande)
        # ipass : passe de verification
        #        1 = verifications supplémentaires
        #        2 = execution
        # icmd  : numéro d'ordre de la commande
        # Retour : iertot = nombre d erreurs
        if CONTEXT.debug : 
           prbanner(" appel de l operateur %s numero %s " % (self.definition.nom,self.definition.op))

        assert(type(self.modexec)==types.IntType),"type(self.modexec)="+`type(self.modexec)`
        assert(type(self.definition.op)==types.IntType),"type(self.definition.op)="+`type(self.definition.op)`
        # affichage du texte de la commande
        self.AfficheTexteCommande()       

        # appel de la methode oper dans le module codex
        ier=self.codex.oper(self,0,self.modexec,self.icmd)

        if CONTEXT.debug : 
           prbanner(" fin d execution de l operateur %s numero %s " % (self.definition.nom,self.definition.op))
        return ier

   def AfficheTexteCommande( self , sortie=sys.stdout ) :
      """ 
          Methode EXECUTION.AfficheTexteCommande
          Auteur : Antoine Yessayan
          Intention : afficher sur la sortie standard (par defaut) le cartouche de
                      la commande avant son execution.
      """
      if self.modexec != 1 :
         decalage="  "  # blancs au debut de chaque ligne affichee
         sortie.write( '\n' )
         sortie.write( decalage )
         sortie.write("#  ---------------------------------------------------------------------------")
         sortie.write( '\n' )

         # Affichage numero de la commande (4 digits)
         sortie.write( decalage+"#  COMMANDE NO : " )
         chaine=`self.icmd`
         l=len(chaine)
         while l < 4  :
            chaine='0'+chaine
            l=l+1
         sortie.write( chaine )
         # Affichage nom du concept resultat
         sortie.write( "          ")
         sortie.write( "CONCEPT DE TYPE : " )
         if self.sd != None:
            type_concept=self.sd.__class__.__name__
            sortie.write( type_concept )

         sortie.write( '\n' )
         sortie.write( decalage )
         sortie.write( "#  -------------               -----------------")

         # recuperation du texte de la commande courante dans la chaine
         # commande_formatee
         v=genpy.genpy(defaut='avec')
         self.accept(v)
         sortie.write( '\n' )
         commande_formatee=v.formate_etape()
         sortie.write(commande_formatee)
         sortie.write( '\n' )

         sortie.write( '\n' ) # saut de ligne final (pour faire beau)

      return

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
      self.Build()
      self.setmode(1)
      self.Exec()
      self.setmode(2)
      try:
        self.Exec()
      except ValueError,e:
        if str(e) == "exit ASTER":
          # Fin normale
          raise EOFError
        elif str(e) == "abort ASTER":
          raise AsException("abort ASTER")
        else:
          raise

