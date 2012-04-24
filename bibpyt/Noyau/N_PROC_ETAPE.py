#@ MODIF N_PROC_ETAPE Noyau  DATE 23/04/2012   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE COURTOIS M.COURTOIS
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    Ce module contient la classe PROC_ETAPE qui sert à vérifier et à exécuter
    une procédure
"""

# Modules Python
import types,sys,string
import traceback

# Modules EFICAS
import N_MCCOMPO, N_ETAPE
from N_Exception import AsException
import N_utils

class PROC_ETAPE(N_ETAPE.ETAPE):
   """
      Cette classe hérite de ETAPE. La seule différence porte sur le fait
      qu'une procédure n'a pas de concept produit

   """
   nature = "PROCEDURE"
   def __init__(self, oper=None, reuse=None, args={}):
      """
      Attributs :
       - definition : objet portant les attributs de définition d'une étape de type opérateur. Il
                      est initialisé par l'argument oper.
       - valeur : arguments d'entrée de type mot-clé=valeur. Initialisé avec l'argument args.
       - reuse : forcément None pour une PROC
      """
      N_ETAPE.ETAPE.__init__(self, oper, reuse=None, args=args)
      self.reuse = None

   def Build_sd(self):
      """
          Cette methode applique la fonction op_init au contexte du parent
          et lance l'exécution en cas de traitement commande par commande
          Elle doit retourner le concept produit qui pour une PROC est toujours None
          En cas d'erreur, elle leve une exception : AsException ou EOFError
      """
      if not self.isactif():return
      try:
         if self.parent:
            if type(self.definition.op_init) == types.FunctionType: 
               apply(self.definition.op_init,(self,self.parent.g_context))
         else:
            pass
      except AsException,e:
        raise AsException("Etape ",self.nom,'ligne : ',self.appel[0],
                              'fichier : ',self.appel[1],e)
      except EOFError:
        raise
      except :
        l=traceback.format_exception(sys.exc_info()[0],sys.exc_info()[1],sys.exc_info()[2])
        raise AsException("Etape ",self.nom,'ligne : ',self.appel[0],
                          'fichier : ',self.appel[1]+'\n',
                          string.join(l))

      self.Execute()
      return None

   def supprime(self):
      """
         Méthode qui supprime toutes les références arrières afin que l'objet puisse
         etre correctement détruit par le garbage collector
      """
      N_MCCOMPO.MCCOMPO.supprime(self)
      self.jdc=None
      self.appel=None

   def accept(self,visitor):
      """
         Cette methode permet de parcourir l'arborescence des objets
         en utilisant le pattern VISITEUR
      """
      visitor.visitPROC_ETAPE(self)

   def update_context(self,d):
      """
         Met à jour le contexte de l'appelant passé en argument (d)
         Une PROC_ETAPE n ajoute pas directement de concept dans le contexte
         Seule une fonction enregistree dans op_init pourrait le faire
      """
      if type(self.definition.op_init) == types.FunctionType:
        apply(self.definition.op_init,(self,d))



