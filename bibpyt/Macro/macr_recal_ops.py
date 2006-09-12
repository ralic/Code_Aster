#@ MODIF macr_recal_ops Macro  DATE 12/09/2006   AUTEUR ASSIRE A.ASSIRE 
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
# ======================================================================

import string, sys, copy, types, Numeric, os
from glob import glob

def macr_recal_ops(self,UNITE_ESCL, RESU_EXP, POIDS, LIST_PARA, LIST_DERIV, RESU_CALC, 
                        ITER_MAXI, RESI_GLOB_RELA,UNITE_RESU,PARA_DIFF_FINI,
                        GRAPHIQUE, INFO, **args ):
   """Macro commande réalisant le recalage de modèles Aster""",
   # Initialisation du compteur d'erreurs
   ier=0

   import aster
   import Macro
   from Cata import cata
   from Cata.cata import DEFI_LIST_REEL

   from Macro import reca_message
   from Macro import reca_algo
   from Macro import reca_interp
   from Macro import reca_utilitaires
   from Macro import reca_calcul_aster
#   from Macro import reca_controles
   from Macro.reca_controles import gestion
#   from Macro.recal import calcul_F
   from Utilitai.Utmess import UTMESS

   # Gestion des Exceptions
   prev_onFatalError = aster.onFatalError()
   aster.onFatalError('EXCEPTION')

   if GRAPHIQUE:
     dGRAPHIQUE=GRAPHIQUE[0].cree_dict_valeurs(GRAPHIQUE[0].mc_liste)
     if dGRAPHIQUE.has_key('FORMAT') and dGRAPHIQUE['FORMAT'] == 'GNUPLOT':
       # On essaie d'importer Gnuplot -> PAS DE GRAPHIQUE
       try:
         import Gnuplot
       except ImportError:
         GRAPHIQUE == None
         UTMESS('A','MACR_RECAL',"Le logiciel Gnuplot ou le module python Gnuplot.py n'est pas disponible. On desactive l'affichage des courbes.")

   import pprint; pprint.pprint(os.environ)

   sys.path.append(os.path.join(os.environ['ASTER_ROOT'], 'ASTK', 'ASTK_SERV', 'lib'))
   print sys.path
   from as_profil import ASTER_PROFIL

   # La macro compte pour 1 dans l'execution des commandes
   self.set_icmd(1)

   self.DeclareOut('nomres',self.sd)


   #_____________________________________________
   #
   # VERIFICATION PREALABLE SUR MEM_ASTER
   #_____________________________________________

   # Lecture du fichier .export
   list_export = glob('*.export')
   if len(list_export) == 0:
      UTMESS('F','MACR_RECAL',"Probleme : il n'y a pas de fichier .export dans le repertoire de travail!")
   elif len(list_export) >1:
      UTMESS('F','MACR_RECAL',"Probleme : il y a plus d'un fichier .export dans le repertoire de travail!")

   prof = ASTER_PROFIL(list_export[0])
   mem_aster = prof['mem_aster'][0]
   memjeveux = prof.args.get('memjeveux')
   
   if mem_aster in ('', '100'):
      UTMESS('A','MACR_RECAL',"Attention : il faut spécifier une valeur pour 'mem_aster' (menu Option de ASTK)" \
                              "pour limiter la mémoire allouée au calcul maitre.")
      mem_aster = '0'
   if not memjeveux:
      UTMESS('F','MACR_RECAL',"Probleme : aucune valeur pour le parametre 'memjeveux'. Verifier le .export")

   try:
      if mem_aster == '0':
         memjeveux_esclave = float(memjeveux)
      else:
         memjeveux_esclave = float(memjeveux) / float(mem_aster) * 100. - float(memjeveux)
   except:
      UTMESS('F','MACR_RECAL',"Probleme : verifier les valeurs des parametres 'mem_aster' et 'memjeveux'")

   UTMESS('I','MACR_RECAL',"Information : les calculs esclaves utiliseront : %.1f Mega Mots." % memjeveux_esclave)
   
   #_____________________________________________
   #
   # INITIALISATIONS
   #_____________________________________________

   # Liste des parametres utilisant la sensibilité
   if not LIST_DERIV: LIST_DERIV = {}
   LIST_SENSI = LIST_DERIV.keys()

   # Stocke l'ordre initial des parametres pour restituer dans le bon ordre les valeurs en sortie de la macro
   LIST_NOM_PARA = [ para[0] for para in LIST_PARA ]

   # On classe les parametres
   LIST_SENSI.sort()
   LIST_PARA.sort()

   # Defini si on utilise le mot-clé SENSIBILITE pour IMPR_TABLE ou non
   if len(LIST_SENSI) >0: table_sensibilite = True
   else:                  table_sensibilite = False

   #_______________________________________________
   #
   # GESTION DE L'OPTION FACULTATIVE POUR LES POIDS
   #_______________________________________________
   if( POIDS == None):
      POIDS=Numeric.ones(len(RESU_EXP))

   #_____________________________________________
   #
   # GESTION DES ERREURS DE SYNTAXE
   #_____________________________________________
   texte_erreur = gestion(UNITE_ESCL,LIST_PARA,RESU_CALC,RESU_EXP,POIDS,GRAPHIQUE,UNITE_RESU)
   if (texte_erreur != ""):
      UTMESS('F', "MACR_RECAL", texte_erreur)

   #_____________________________________________
   #
   # DECLARATION DE TOUTES LES COMMANDES ASTER
   #_____________________________________________
   for k,v in cata.__dict__.items() :
     if type(v)==types.InstanceType:
        if v.__class__.__name__ in ('OPER','MACRO'):
           self.current_context[k]= v
   self.current_context['_F']=cata.__dict__['_F']


   #_____________________________________________
   #
   # INITIALISATIONS
   #_____________________________________________

   iter = 0
   restant,temps_iter=0.,0.
   restant,temps_iter,err=reca_utilitaires.temps_CPU(self,restant,temps_iter)
   para,val,borne_inf,borne_sup = reca_utilitaires.transforme_list_Num(LIST_PARA,RESU_EXP)
   val_init = copy.copy(val)

   # OBJET "PARAMETRES GLOBAUX"
   PARAMETRES = reca_calcul_aster.PARAMETRES(UNITE_RESU=UNITE_RESU, INFO=INFO,
                                             fich_output='./REPE_OUT/output.txt',
                                             mode_include=False,
                                             follow_output=True,
                                             table_sensibilite=table_sensibilite,
                                             memjeveux_esclave=memjeveux_esclave)

   # OBJET "CALCUL"
   CALCUL_ASTER = reca_calcul_aster.CALCUL_ASTER(PARAMETRES, UL=UNITE_ESCL, para=para, reponses=RESU_CALC, LIST_SENSI=LIST_SENSI, LIST_DERIV=LIST_DERIV)

   # Calcul d'initialisation de F, ici L_deriv_sensible ne contient que les termes calculés par la sensibilité, les autres termes sont nuls
   L_init, L_deriv_sensible = CALCUL_ASTER.calcul_F(val)

   # Instance de la classe gérant l'affichage des resultats du calcul de l'optimisation
   Mess = reca_message.Message(para,RESU_EXP,copy.copy(val_init),UNITE_RESU) 

   # Instances des classes pour le calcul de l'erreur et le dimensionnemnt/adim
   Simul = reca_interp.Sim_exp(RESU_EXP,POIDS)
   Dim = reca_algo.Dimension(copy.copy(val_init),para)
   L_J_init,erreur = Simul.multi_interpole(L_init, RESU_CALC)
   J_init = Simul.norme_J(copy.copy(L_J_init),copy.copy(L_J_init),UNITE_RESU)
   J = J_init

   A = Simul.sensibilite(CALCUL_ASTER, L_init, L_deriv_sensible, val, PARA_DIFF_FINI)
   A = Dim.adim_sensi(A)

   l = reca_algo.lambda_init(Numeric.matrixmultiply(Numeric.transpose(A),A))
   gradient_init =reca_algo.calcul_gradient(A,erreur)  #utile pour le test de convergence, on prend les valeurs dimensionnées
   residu = reca_algo.test_convergence(gradient_init,erreur,A,Numeric.zeros(len(gradient_init),Numeric.Float))
   Mess.affiche_result_iter(iter,J,val,residu,Numeric.array([]),UNITE_RESU)
   # On teste un manque de temps CPU
   restant,temps_iter,err=reca_utilitaires.temps_CPU(self,restant,temps_iter)
   if (err==1):
      ier=ier+1
      return ier


   #_____________________________________________
   #
   # BOUCLE PRINCIPALE DE L'ALGORITHME
   #_____________________________________________
   epsilon = 10.*RESI_GLOB_RELA
   while((residu > RESI_GLOB_RELA) & (iter<ITER_MAXI)):  
      iter = iter +1
      new_val, s, l, Act = reca_algo.Levenberg_bornes(self,val,Dim,val_init,borne_inf,borne_sup,A,erreur,l,UNITE_RESU) 

      # Calcul de F, ici L_deriv_sensible ne contient que les termes calculés par la sensibilité, les autres termes sont nuls
      L_F, L_deriv_sensible = CALCUL_ASTER.calcul_F(new_val)

      new_L_J,new_erreur = Simul.multi_interpole(L_F, RESU_CALC)
      new_J = Simul.norme_J(L_J_init,new_L_J,UNITE_RESU)
      l = reca_algo.actualise_lambda(l,Dim.adim(val),Dim.adim(new_val),A,erreur,new_J,J)

      val = copy.copy(new_val)
      erreur = copy.copy(new_erreur)
      J = new_J

      A = Simul.sensibilite(CALCUL_ASTER, L_F, L_deriv_sensible, val, PARA_DIFF_FINI)
      A = Dim.adim_sensi(A)

      residu = reca_algo.test_convergence(gradient_init,erreur,A,s)
      Mess.affiche_result_iter(iter,J,val,residu,Act,UNITE_RESU)

      if (GRAPHIQUE):
            GRAPHE_UL_OUT=GRAPHIQUE['UNITE']
            interactif=(GRAPHIQUE['INTERACTIF']=='OUI')
            reca_utilitaires.graphique(GRAPHIQUE['FORMAT'],L_F,RESU_EXP,RESU_CALC,iter,GRAPHE_UL_OUT,interactif)

      # On teste un manque de temps CPU
      restant,temps_iter,err=reca_utilitaires.temps_CPU(self,restant,temps_iter)
      if (err==1):
         ier=ier+1
         return ier
   
   #_____________________________________________
   #
   # FIN DES ITERATIONS
   # CONVERGENCE OU ECHEC
   #_____________________________________________
   Mess.affiche_etat_final_convergence(iter,ITER_MAXI,RESI_GLOB_RELA,residu,Act,UNITE_RESU)
   reca_algo.calcul_etat_final(para,A,iter,ITER_MAXI,RESI_GLOB_RELA,residu,Mess,UNITE_RESU)

   # Affichage du graphisme
   if (GRAPHIQUE):
        GRAPHE_UL_OUT=GRAPHIQUE['UNITE']
        interactif=(GRAPHIQUE['INTERACTIF']=='OUI')
        reca_utilitaires.graphique(GRAPHIQUE['FORMAT'],L_F,RESU_EXP,RESU_CALC,iter,GRAPHE_UL_OUT,interactif)

   #_____________________________________________
   #
   # CREATIONS DE LA LISTE DE REELS CONTENANT 
   # LES VALEURS DES PARAMETRES A CONVERGENCE
   #_____________________________________________

   LIST_NOM_PARA_ALPHA = [ para[0] for para in LIST_PARA ]
   LIST_NOM_PARA_ALPHA.sort()
   lival=[]
   for i in LIST_NOM_PARA:
      lival.append( val[ LIST_NOM_PARA_ALPHA.index(i) ] )
   nomres=DEFI_LIST_REEL(VALE=lival)

   # Gestion des Exceptions
   aster.onFatalError(prev_onFatalError)

   return 
