#@ MODIF E_ETAPE Execution  DATE 26/09/2003   AUTEUR DURAND C.DURAND 
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
import string,types,sys,os

# Modules Eficas
from Noyau.N_utils import prbanner
from Noyau.N_Exception import AsException
import genpy

class ETAPE:
   """
   Cette classe implémente les méthodes relatives à la phase d'execution.

   Les méthodes principales sont:
      - Exec, realise la phase d'execution, en mode par lot
      - Execute, realise la phase d'execution, en mode commande par commande
   """

   def Exec(self):
      """
      Realise une passe d'execution sur l'operateur "fortran" associé
      si le numero d'operateur (self.definition.op) est défini.

      On execute l'operateur numero self.definition.op
      en lui passant les arguments :
        - self : la commande courante
        - lot : le mode d'execution qui peut prendre comme valeur :
               -  0 = execution par lot (verification globale avant execution)
               -  1 = execution commande par commande (verification + execution commande par commande)
        - ipass : passe d'execution
               -  1 = verifications supplémentaires
               -  2 = execution effective
        - icmd  : numéro d'ordre de la commande
      Retour : iertot = nombre d erreurs

      """

      if CONTEXT.debug : 
           prbanner(" appel de l operateur %s numero %s " % (self.definition.nom,self.definition.op))

      # On n'execute pas les etapes qui n'ont pas de numero d'operateur associé
      if self.definition.op is None :return 0

      assert(type(self.modexec)==types.IntType),"type(self.modexec)="+`type(self.modexec)`
      assert(type(self.definition.op)==types.IntType),"type(self.definition.op)="+`type(self.definition.op)`

      ier=0
      if self.definition.op > 0 and self.modexec == 2:
           # affichage du texte de la commande
           self.AfficheTexteCommande()       

      # Il ne faut pas executer les commandes non numerotees
      if self.icmd is not None:
          # appel de la methode oper dans le module codex
          ier=self.codex.oper(self,0,self.modexec,self.icmd)

      if CONTEXT.debug : 
           prbanner(" fin d execution de l operateur %s numero %s " % (self.definition.nom,
                                                                       self.definition.op))
      return ier

   def AfficheTexteCommande( self , sortie=sys.stdout ) :
      """ 
      Methode : ETAPE.AfficheTexteCommande
      Intention : afficher sur la sortie standard (par defaut) le cartouche de
                      la commande avant son execution.
      """
      # impression du fichier .code : compte rendu des commandes et
      # mots clés activés par l'ETAPE
      if self.jdc.fico!=None :
        ficode=open(os.getcwd()+'/ficode','a')
        v=genpy.genpy(defaut='avec',simp='into')
        self.accept(v)
        chaine=' '+string.ljust(self.jdc.fico,10)+string.ljust(self.nom,20)
        for mc in v.args.keys():
            if type(v.args[mc])==types.StringType:
              chainec=chaine+string.ljust(' --',20)+string.ljust(mc,20)+string.ljust(v.args[mc],20)
              ficode.write(chainec)
              ficode.write('\n')
            elif type(v.args[mc])==types.ListType:
              for mcs in v.args[mc]:
                 for mcf in mcs.keys():
                  chainec=chaine+string.ljust(mc,20)+string.ljust(mcf,20)+mcs[mcf]
                  ficode.write(chainec)
                  ficode.write('\n')
            elif type(v.args[mc])==types.DictType:
                mcs=v.args[mc]
                for mcf in mcs.keys():
                  chainec=chaine+string.ljust(mc,20)+string.ljust(mcf,20)+mcs[mcf]
                  ficode.write(chainec)
                  ficode.write('\n')
        ficode.close()

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
      Cette methode realise l execution complete d une etape, en mode commande par commande : 
             - construction, 
             - verification, 
             - execution
      en une seule passe. Utilise en mode par_lot='NON'

      L'attribut d'instance executed indique que l'etape a deja ete executee
      Cette methode peut etre appelee plusieurs fois mais l'execution proprement
      dite ne doit etre realisee qu'une seule fois.
      Les seuls cas ou on appelle plusieurs fois Execute sont pour les
      commandes INCLUDE et INCLUDE_MATERIAU (appel dans op_init)
      """
      if not self.jdc or self.jdc.par_lot != "NON" :
         return

      if hasattr(self,"executed") and self.executed == 1:return
      self.executed=1

      cr=self.report()
      self.parent.cr.add(cr)
      if not cr.estvide():
        raise EOFError

      self.Build()

      self.setmode(1)
      self.Exec()
      self.setmode(2)
      try:
          self.Exec()
      except self.codex.error:
          self.detruit_sdprod()
          raise

   def detruit_sdprod(self):
      """ Cette méthode supprime le concept produit par la commande
          du registre tenu par le JDC
      """
      try:
          del self.jdc.sds_dict[self.sd.nom]
      except:
          pass

   def BuildExec(self):
      """ 
      Cette methode realise l execution complete d une etape, en mode commande par commande : 
             - construction, 
             - execution
      en une seule passe. Utilise en mode par_lot='NON'

      L'attribut d'instance executed indique que l'etape a deja ete executee
      Cette methode peut etre appelee plusieurs fois mais l'execution proprement
      dite ne doit etre realisee qu'une seule fois.
      Les seuls cas ou on appelle plusieurs fois Execute sont pour les
      commandes INCLUDE et INCLUDE_MATERIAU (appel dans op_init)
      """

      if hasattr(self,"executed") and self.executed == 1:return
      self.executed=1

      # Construction des sous-commandes
      self.Build()

      self.setmode(1)
      self.Exec()
      self.setmode(2)
      self.Exec()

