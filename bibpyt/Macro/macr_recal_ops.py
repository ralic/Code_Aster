#@ MODIF macr_recal_ops Macro  DATE 21/09/2009   AUTEUR COURTOIS M.COURTOIS 
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
# RESPONSABLE ASSIRE A.ASSIRE

import os, sys, copy, math
from glob import glob
import Numeric

debug = False

INFO = 1
NOMPRO = 'MACR_RECAL'

fichier_export = None
mode_python = False
type_fonctionnelle = 'float'


# --------------------------------------------------------------------------------------------------
def Ecriture_Fonctionnelle(output_file, type_fonctionnelle, fonctionnelle):

   try:    os.remove(output_file)
   except: pass

   f=open(output_file, 'w')
   if type_fonctionnelle == 'vector':
      fonctionnelle = str(fonctionnelle.tolist())
      fonctionnelle = fonctionnelle.replace('[','')
      fonctionnelle = fonctionnelle.replace(']','')
   f.write(str(fonctionnelle))
   f.close()


# --------------------------------------------------------------------------------------------------
def Ecriture_Derivees(output_file, derivees):

   try:    os.remove(output_file)
   except: pass

   # On sort si il n'y a pas de derivees a imprimer
   if not derivees: return

   txt = ''
   a = derivees
   for l in range(len(a[:,0])):
      ligne = []
      for c in range(len(a[0,:])):
         ligne.append( str(a[l,c]) )
      txt += ','.join(ligne) + '\n'

   f=open(output_file, 'w')
   f.write(txt)
   f.close()


# --------------------------------------------------------------------------------------------------
def Sortie(LIST_NOM_PARA, LIST_PARA, val, CALCUL_ASTER, Mess):
   """ Sortie de la macro, on renvoie les parametres obtenus """

   import Cata, aster, Macro
   from Cata.cata import DEFI_LIST_REEL
   from Accas import _F
   from Macro import reca_message
   from Macro import reca_algo
   from Macro import reca_interp
   from Macro import reca_utilitaires
   from Macro import reca_calcul_aster
   from Macro.reca_controles import gestion
   from Utilitai.Utmess import UTMESS, MessageLog

   if CALCUL_ASTER.METHODE != 'EXTERNE':
      UTMESS('I','RECAL0_1', valk=str(CALCUL_ASTER.evaluation_fonction), cc=Mess.get_filename())

   LIST_NOM_PARA_ALPHA = [ para[0] for para in LIST_PARA ]
   LIST_NOM_PARA_ALPHA.sort()
   lival=[]
   for i in LIST_NOM_PARA:
      lival.append( val[ LIST_NOM_PARA_ALPHA.index(i) ] )
   nomres = DEFI_LIST_REEL(VALE=lival)

   return nomres


# --------------------------------------------------------------------------------------------------
def macr_recal_externe( RESU_EXP, LIST_PARA, RESU_CALC, UNITE_ESCL=3, POIDS=None, LIST_DERIV=None, 
               ITER_MAXI=10, ITER_FONC_MAXI=100, RESI_GLOB_RELA=1.e-6, UNITE_RESU=91, PARA_DIFF_FINI=0.001,
               GRAPHIQUE=None, SUIVI_ESCLAVE='NON', METHODE='EXTERNE', INFO=1, **args ):
   """
   Entree du mode EXTERNE (ici macr_recal_ops.py est traite comme un fichier Python et non pas par le superviseur Aster)
   """

   METHODE='EXTERNE'

   # Mot-cle GRAPHIQUE
   if GRAPHIQUE:
      GRAPHIQUE0 = {'PILOTE': '', 'AFFICHAGE': 'TOUTE_ITERATION', 'UNITE': 90, 'FORMAT': 'XMGRACE'}
      for k in GRAPHIQUE0.keys():
         if not GRAPHIQUE.has_key(k): GRAPHIQUE[k] = GRAPHIQUE0[k]

      if optparse_prefix_graph: args['prefix_graph'] = opts.prefix_graph
      else:                     args['prefix_graph'] = os.getcwd() + os.sep + 'graph'

   # Les parametres passes sur la ligne de commande surchargent les parametres de la commande MACR_RECAL
   if optparse_INFO: INFO=opts.INFO
   if optparse_follow_output:
      if opts.follow_output == True: SUIVI_ESCLAVE='OUI'
      else:                          SUIVI_ESCLAVE='NON'

   if optparse_objective:
      if type_fonctionnelle=='vector': args['FONCTIONNELLE']='VECTORIELLE'
      else:                            args['FONCTIONNELLE']='SCALAIRE'

   if optparse_gradient:
      if   opts.gradient=='normal': args['GRADIENT']='NORMAL'
      elif opts.gradient=='adim':   args['GRADIENT']='ADIMENSIONNE'
      else:                         args['GRADIENT']='NON_CALCULE'


   fonctionnelle, gradient = macr_recal(UNITE_ESCL, RESU_EXP, POIDS, LIST_PARA, LIST_DERIV, RESU_CALC, 
                                        ITER_MAXI, ITER_FONC_MAXI, RESI_GLOB_RELA, UNITE_RESU, PARA_DIFF_FINI,
                                        GRAPHIQUE, SUIVI_ESCLAVE, METHODE, INFO, **args )

   return fonctionnelle, gradient

# --------------------------------------------------------------------------------------------------
def force_list(obj, typref=list):
   """Retourne 'obj' sous forme d'une liste de 'typref'.
   """
   if type(obj) not in (list, tuple):
      assert type(obj) == typref, '%s != %s' % (type(obj), typref)
      obj = [obj,]
   elif len(obj) > 0:
      elt = obj[0]
      if type(elt) != typref:
         obj = [obj,]
   return obj

# --------------------------------------------------------------------------------------------------
def macr_recal_ops(self,UNITE_ESCL, RESU_EXP, POIDS, LIST_PARA, LIST_DERIV, RESU_CALC, 
                        ITER_MAXI, ITER_FONC_MAXI, RESI_GLOB_RELA,UNITE_RESU,PARA_DIFF_FINI,
                        GRAPHIQUE, SUIVI_ESCLAVE, METHODE, INFO, **args ):
   """Macro commande realisant le recalage de modeles Aster"""
   # Initialisation du compteur d'erreurs
   ier=0

   import aster
   import Macro
   from Cata import cata
   from Cata.cata import DEFI_LIST_REEL, CREA_TABLE, TEST_TABLE
   from Cata.cata import OPER, MACRO

   from Macro import reca_message
   from Macro import reca_algo
   from Macro import reca_interp
   from Macro import reca_utilitaires
   from Macro import reca_calcul_aster
   from Macro.reca_controles import gestion

   # Gestion des Exceptions
   prev_onFatalError = aster.onFatalError()
   aster.onFatalError('EXCEPTION')

   # La macro compte pour 1 dans l'execution des commandes
   self.set_icmd(1)

   # Concept en sortir
   self.DeclareOut('nomres',self.sd)

   # Declaration de toutes les commandes Aster
   for k,v in cata.__dict__.items() :
      if isinstance(v, (OPER, MACRO)):
         self.current_context[k]= v
   self.current_context['_F']=cata.__dict__['_F']

   macr_recal(UNITE_ESCL, force_list(RESU_EXP, Numeric.ArrayType), POIDS, force_list(LIST_PARA), LIST_DERIV, force_list(RESU_CALC), 
             ITER_MAXI, ITER_FONC_MAXI, RESI_GLOB_RELA,UNITE_RESU,PARA_DIFF_FINI,
             GRAPHIQUE, SUIVI_ESCLAVE, METHODE, INFO, **args)

   aster.onFatalError(prev_onFatalError)
   return


# --------------------------------------------------------------------------------------------------
def macr_recal(UNITE_ESCL, RESU_EXP, POIDS, LIST_PARA, LIST_DERIV, RESU_CALC, 
               ITER_MAXI, ITER_FONC_MAXI, RESI_GLOB_RELA,UNITE_RESU,PARA_DIFF_FINI,
               GRAPHIQUE, SUIVI_ESCLAVE, METHODE, INFO, **args ):

   from Utilitai.Utmess import UTMESS
   # Import d'as_profil
   if os.environ.has_key('ASTER_ROOT'):
      sys.path.append(os.path.join(os.environ['ASTER_ROOT'], 'ASTK', 'ASTK_SERV', 'lib'))
   else:
      try:    sys.path.append(os.path.join(aster.repout, '..', 'ASTK', 'ASTK_SERV', 'lib'))
      except: pass
   try:
      from as_profil import ASTER_PROFIL
   except:
      UTMESS('F','RECAL0_2')

   import Macro, Utilitai
   from Macro import reca_message
   from Macro import reca_algo
   from Macro import reca_interp
   from Macro import reca_utilitaires
   from Macro import reca_calcul_aster
   from Macro.reca_controles import gestion
   if( METHODE != 'EXTERNE'):
      from Utilitai.optimize import fmin, line_search, line_search_BFGS, approx_fprime, approx_fhess_p, fminBFGS, fminNCG

   if( METHODE == 'EXTERNE'):
      pass
   else:
      if GRAPHIQUE:
        dGRAPHIQUE=GRAPHIQUE[0].cree_dict_valeurs(GRAPHIQUE[0].mc_liste)
        if dGRAPHIQUE.has_key('FORMAT') and dGRAPHIQUE['FORMAT'] == 'GNUPLOT':
          # On essaie d'importer Gnuplot -> PAS DE GRAPHIQUE
          try:
            import Gnuplot
          except ImportError:
            GRAPHIQUE == None
            if INFO>=1: UTMESS('A','RECAL0_3')


   #_____________________________________________
   #
   # VERIFICATION PREALABLE SUR MEM_ASTER
   #_____________________________________________

   # Lecture du fichier .export
   list_export = glob('*.export')
   if len(list_export) == 0:
      UTMESS('F','RECAL0_4')
   elif len(list_export) >1:
      UTMESS('F','RECAL0_5')

   prof = ASTER_PROFIL(list_export[0])

   mem_aster = prof['mem_aster'][0]
   memjeveux = prof.args.get('memjeveux')

   if mem_aster in ('', '100'):
      if INFO>=1: UTMESS('A','RECAL0_6')
      mem_aster = '0'
   if not memjeveux:
      UTMESS('F','RECAL0_7')

   try:
      if mem_aster == '0':
         memjeveux_esclave = float(memjeveux)
      else:
         memjeveux_esclave = float(memjeveux) / float(mem_aster) * 100. - float(memjeveux)
   except:
      UTMESS('F','RECAL0_8')

   if INFO>=1: UTMESS('I','RECAL0_9', valr=memjeveux_esclave)


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

   # Defini si on ajoute l'echo des calculs esclaves dans le mess du calcul maitre
   follow_output = False
   if SUIVI_ESCLAVE and SUIVI_ESCLAVE=='OUI': follow_output = True
#   if( METHODE == 'EXTERNE') and mode_python: follow_output = opts.follow_output

   # Pour les algorithmes d'optimize, on a des limitations
   if METHODE in ['FMIN', 'FMINBFGS', 'FMINNCG']:
      # On ne peut tracer qu'a la derniere iteration
      if GRAPHIQUE:
         if GRAPHIQUE['AFFICHAGE']=='TOUTE_ITERATION': UTMESS('I', 'RECAL0_10', valk=METHODE)
      # Les bornes ne sont pas gerees
      UTMESS('I','RECAL0_11', valk=METHODE)

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
   texte_erreur, texte_alarme = gestion(UNITE_ESCL,LIST_PARA,RESU_CALC,RESU_EXP,POIDS,GRAPHIQUE,UNITE_RESU,METHODE)
   if (texte_erreur != ""):
      UTMESS('F', "RECAL0_12", valk=texte_erreur)
   if (texte_alarme != ""):
      UTMESS('A', "RECAL0_12", valk=texte_alarme)


   #_____________________________________________
   #
   # INITIALISATIONS
   #_____________________________________________

   if( METHODE != 'EXTERNE'):
     iter = 0
     restant,temps_iter=0.,0.
     restant,temps_iter,err=reca_utilitaires.temps_CPU(restant,temps_iter)

   para,val,borne_inf,borne_sup = reca_utilitaires.transforme_list_Num(LIST_PARA,RESU_EXP)

   # Pour l'algorithme externe, les valeurs sont celles lues dans le fichier input.txt
   if( METHODE == 'EXTERNE') and mode_python: val = val_externe

   val_init = copy.copy(val)

   # OBJET "PARAMETRES GLOBAUX"
   PARAMETRES = reca_calcul_aster.PARAMETRES(
                                             METHODE=METHODE,
                                             UNITE_RESU=UNITE_RESU,
                                             INFO=INFO,
                                             fich_output='./REPE_OUT/output_esclave.txt',
                                             mode_include=False,
                                             follow_output=follow_output,
                                             table_sensibilite=table_sensibilite,
                                             memjeveux_esclave=memjeveux_esclave,
                                             PARA_DIFF_FINI=PARA_DIFF_FINI,
                                             ITER_MAXI=ITER_MAXI,
                                             ITER_FONC_MAXI=ITER_FONC_MAXI,
                                             )

   if( METHODE == 'EXTERNE'):
      PARAMETRES.fich_output = './tmp_macr_recal/output_esclave.txt'
      type_fonctionnelle = 'float'
      if args.has_key('FONCTIONNELLE') and args['FONCTIONNELLE'] == 'VECTORIELLE':
         PARAMETRES.vector_output = True
         type_fonctionnelle = 'vector'

   # On utilise le critere en erreur plutot que normalise
   elif METHODE in ['FMIN', 'FMINBFGS', 'FMINNCG']: PARAMETRES.error_output = True

   # OBJET "CALCUL"
   CALCUL_ASTER = reca_calcul_aster.CALCUL_ASTER(PARAMETRES, UL=UNITE_ESCL, para=para, reponses=RESU_CALC, LIST_SENSI=LIST_SENSI, LIST_DERIV=LIST_DERIV)

   # Instances des classes pour le calcul de l'erreur et le dimensionnemnt/adim
   Simul = reca_interp.Sim_exp(RESU_EXP,POIDS)
   Dim = reca_algo.Dimension(copy.copy(val_init),para)

   CALCUL_ASTER.Simul     = Simul
   CALCUL_ASTER.Dim       = Dim
   CALCUL_ASTER.reca_algo = reca_algo

   if (GRAPHIQUE):
      CALCUL_ASTER.UNITE_GRAPHIQUE = GRAPHIQUE['UNITE']


   # Instance de la classe gérant l'affichage des resultats du calcul de l'optimisation
   Mess = reca_message.Message(para,RESU_EXP,copy.copy(val_init),UNITE_RESU) 

   if( METHODE != 'EXTERNE'):
      Mess.initialise()
      if INFO>=1: UTMESS('I','RECAL0_13', valk=METHODE, cc=Mess.get_filename())



   #-------------------------------------------------------------------------------
   # Methode EXTERNE (en fait juste une evaluation de la fonction puis on sort)
   #
   if( METHODE == 'EXTERNE'):

      # On sauvegarde le fichier esclave si celui-ci est fort.UL (sinon il sera ecrase)
      fic_esclave = './fort.'+str(UNITE_ESCL)
      txt_old_esclave = None
      if os.path.isfile(fic_esclave):
         f = open(fic_esclave,'r')
         txt_old_esclave = f.read()
         f.close()
      
#       try:    os.remove('./fort.'+str(UNITE_ESCL))
#       except: pass

      # Fichier bilan
      txt = '\nPARAMETRES : ' + str([ para[0] for para in LIST_PARA ]) + ' ' + str(val)
      Mess.ecrire(txt)
  
      # Execution de l'esclave
      if args.has_key('GRADIENT') and args['GRADIENT']!='NON_CALCULE':
  
         # Calcul de F et G
         fonctionnelle, residu, A_nodim, A = CALCUL_ASTER.calcul_FG(val)

         # Ecriture du fichier grad.txt contenant les derivees
         if args['GRADIENT'] == 'ADIMENSIONNE': gradient = A
         else:                                  gradient = A_nodim
  
         # Ecriture du fichier contenant le gradient
         if not mode_python: Ecriture_Derivees(output_file='./fort.1901', derivees=gradient)

      else:
         # Calcul de F
         fonctionnelle = CALCUL_ASTER.calcul_F(val)
         gradient = None

      # Ecriture du fichier contenant la fonctionnelle
      if not mode_python: Ecriture_Fonctionnelle(output_file='./fort.1900', type_fonctionnelle=type_fonctionnelle, fonctionnelle=fonctionnelle)

      # Fichier bilan
      if type(fonctionnelle) == float: txt = '---> fonctionnelle : '       + str(fonctionnelle)
      else:                            txt = '---> norme fonctionnelle : ' + str( math.sqrt( (Numeric.sum( [x**2 for x in fonctionnelle] )) ) )
      Mess.ecrire(txt)

      # Affichage de la valeur de la fonctionnelle
      if mode_python and opts.INFO==-1: print txt

      # Affichage de la norme du gradient (AA: a remplacer par une formule de norme L2 !!)
      if gradient:
         norme = 0
         for l in range(len(gradient[:,0])):
            for c in range(len(gradient[0,:])):
               norme += ( gradient[l,c] * gradient[l,c] )
         norme = math.sqrt(norme)
         txt = '---> norme du gradient : ' + str(norme)
         Mess.ecrire(txt)
         if mode_python and opts.INFO==-1: print txt

  
      try:    os.remove('./fort.'+str(UNITE_ESCL))
      except: pass

      # On remet l'ancien fichier esclave si c'etait fort.UL
      if txt_old_esclave:
         f = open(fic_esclave,'w')
         f.write(txt_old_esclave)
         f.close()

  
      L_F = CALCUL_ASTER.L
      iter = None

      # On va ensuite jusqu'au bout (pour l'impression des graphes)



   #-------------------------------------------------------------------------------
   # Algorithme FMIN (pas d'adimensionnement car n'utilise pas de gradient)
   #
   elif( METHODE == 'FMIN'):
      val, fval, warnflag = fmin(CALCUL_ASTER.calcul_F, val, maxiter=ITER_MAXI, maxfun=ITER_FONC_MAXI, fulloutput=1)

      iter_fonc = CALCUL_ASTER.evaluation_fonction

      Mess.ecrire("\nDerniere iteration : ")
      Mess.affiche_etat_final_convergence(iter,ITER_MAXI,iter_fonc,ITER_FONC_MAXI, RESI_GLOB_RELA,residu=0,Act=[])
      Mess.affiche_fonctionnelle(fval)
      Mess.affiche_valeurs(val)
      if warnflag==1: Mess.ecrire("Attention : le nombre maximum d'evaluations de la fonction (ITER_FONC_MAXI) a ete atteint")
      if warnflag==2: Mess.ecrire("Attention : le nombre maximum d'iteration de l'algorithme (ITER_MAXI) a ete atteint")

      nomres = Sortie(LIST_NOM_PARA, LIST_PARA, val, CALCUL_ASTER, Mess)
      return

   else:
       #-------------------------------------------------------------------------------
       # Pour tous les autres methodes, on adimensionne
    
       # Calcul d'initialisation de F, ici L_deriv_sensible ne contient que les termes calculés par la sensibilité, les autres termes sont nuls
       L_init, L_deriv_sensible = CALCUL_ASTER.calcul_Aster(val, INFO)
    
       L_J_init, erreur = Simul.multi_interpole(L_init, RESU_CALC)
       J_init = Simul.norme_J(copy.copy(L_J_init),copy.copy(L_J_init),UNITE_RESU)
       J = J_init

       A = Simul.sensibilite(CALCUL_ASTER, L_init, L_deriv_sensible, val, PARA_DIFF_FINI)
       A = Dim.adim_sensi(A)

       l = reca_algo.lambda_init(Numeric.matrixmultiply(Numeric.transpose(A),A))

       gradient_init =reca_algo.calcul_gradient(A,erreur)  #utile pour le test de convergence, on prend les valeurs dimensionnées
       residu = reca_algo.test_convergence(gradient_init,erreur,A,Numeric.zeros(len(gradient_init),Numeric.Float))
    
       Mess.affiche_result_iter(iter,J,val,residu,Numeric.array([]))
       # On teste un manque de temps CPU
       restant,temps_iter,err=reca_utilitaires.temps_CPU(restant,temps_iter)
       if (err==1):
          ier=ier+1
          return ier

       CALCUL_ASTER.L_init         = L_init
       CALCUL_ASTER.L_J_init       = L_J_init
       CALCUL_ASTER.J_init         = J_init
       CALCUL_ASTER.A_init         = A
       CALCUL_ASTER.gradient_init  = gradient_init
       CALCUL_ASTER.residu_init    = residu


       #-------------------------------------------------------------------------------
       # Methode FMINBFGS et FMINNCG

       if METHODE in ['FMINBFGS', 'FMINNCG']:
          # Derivees
          fprime=CALCUL_ASTER.calcul_G
          warnflag=0

          if args.has_key('GRADIENT') and args['GRADIENT'] == 'NON_CALCULE': fprime=None

          if fprime: UTMESS('I','RECAL0_14')
          else:      UTMESS('I','RECAL0_15')

          # Lancement de l'optimisation
          if METHODE == 'FMINBFGS':
             val, fval, func_calls, grad_calls, warnflag = fminBFGS(CALCUL_ASTER.calcul_F, val, fprime=fprime, maxiter=ITER_MAXI, avegtol=RESI_GLOB_RELA, fulloutput=1)

          elif METHODE == 'FMINNCG':
             val, fval, func_calls, grad_calls, hcalls, warnflag = fminNCG(CALCUL_ASTER.calcul_F, val, fprime=fprime, fhess_p=None, fhess=None, maxiter=ITER_MAXI, avextol=RESI_GLOB_RELA, fulloutput=1)

          # Affichage des messages de sortie
          iter_fonc = CALCUL_ASTER.evaluation_fonction
          Mess.ecrire("\nDerniere iteration : ")
          Mess.affiche_etat_final_convergence(iter,ITER_MAXI,iter_fonc,ITER_FONC_MAXI, RESI_GLOB_RELA,residu=0,Act=[])
          Mess.affiche_fonctionnelle(fval)
          Mess.affiche_valeurs(val)
#           if warnflag==1: Mess.ecrire("\nAttention : le nombre maximum d'evaluations de la fonction (ITER_FONC_MAXI) a ete atteint")
#           if warnflag==2: Mess.ecrire("\nAttention : le nombre maximum d'iteration de la methode (ITER_MAXI) a ete atteint")

          # Permet d'avoir un diagnostic NOOK pour le job
          if warnflag: iter=ITER_MAXI

          L_F = CALCUL_ASTER.L
          residu = fval




       #-------------------------------------------------------------------------------
       # Methode Levenberg-Marquardt
       else:
    
             #_____________________________________________
             #
             # BOUCLE PRINCIPALE DE L'ALGORITHME
             #_____________________________________________
             epsilon = 10.*RESI_GLOB_RELA
             while((residu > RESI_GLOB_RELA) & (iter<ITER_MAXI)):  
                iter = iter +1
                new_val, s, l, Act = reca_algo.Levenberg_bornes(val,Dim,val_init,borne_inf,borne_sup,A,erreur,l,UNITE_RESU) 

                # Calcul de F, ici L_deriv_sensible ne contient que les termes calculés par la sensibilité, les autres termes sont nuls
                L_F, L_deriv_sensible = CALCUL_ASTER.calcul_Aster(new_val, INFO)

                new_L_J,new_erreur = Simul.multi_interpole(L_F, RESU_CALC)
                new_J = Simul.norme_J(L_J_init,new_L_J,UNITE_RESU)
                l = reca_algo.actualise_lambda(l,Dim.adim(val),Dim.adim(new_val),A,erreur,new_J,J)

                val = copy.copy(new_val)
                erreur = copy.copy(new_erreur)
                J = new_J

                # Calcul de la matrice des sensibilites
                A = Simul.sensibilite(CALCUL_ASTER, L_F, L_deriv_sensible, val, PARA_DIFF_FINI)
                A = Dim.adim_sensi(A)

                # Calcul du residu
                residu = reca_algo.test_convergence(gradient_init,erreur,A,s)

                # Affichage iteration
                Mess.affiche_result_iter(iter,J,val,residu,Act)
                if INFO>=1: UTMESS('I','RECAL0_16',vali=iter, valk=J, valr=residu)

                if (GRAPHIQUE):
                   if GRAPHIQUE['AFFICHAGE']=='TOUTE_ITERATION':
                      GRAPHE_UL_OUT=GRAPHIQUE['UNITE']
                      pilote=GRAPHIQUE['PILOTE']
                      reca_utilitaires.graphique(GRAPHIQUE['FORMAT'],L_F,RESU_EXP,RESU_CALC,iter,GRAPHE_UL_OUT,pilote)

                # On teste un manque de temps CPU
                restant,temps_iter,err=reca_utilitaires.temps_CPU(restant,temps_iter)
                if (err==1):
                   ier=ier+1
                   return ier


             #_____________________________________________
             #
             # FIN DES ITERATIONS
             # CONVERGENCE OU ECHEC
             #_____________________________________________
             iter_fonc = CALCUL_ASTER.evaluation_fonction
             Mess.affiche_etat_final_convergence(iter,ITER_MAXI,iter_fonc,ITER_FONC_MAXI, RESI_GLOB_RELA,residu,Act)
             reca_algo.calcul_etat_final(para,A,iter,ITER_MAXI,RESI_GLOB_RELA,residu,Mess)


       #-------------------------------------------------------------------------------


   #_____________________________________________
   #
   # FIN DES ITERATIONS POUR TOUS LES ALGOS
   #_____________________________________________
   
   if (GRAPHIQUE):
      trace = False
      fichier = None
      # Pour les algorithmes d'optimize.py, on ne peut tracer qu'a la derniere iteration
      if (GRAPHIQUE['AFFICHAGE']=='ITERATION_FINALE') or (METHODE in ['FMIN', 'FMINBFGS', 'FMINNCG']):
         trace = True
      if (METHODE=='EXTERNE' and GRAPHIQUE['AFFICHAGE']=='TOUTE_ITERATION'): 
         trace = True
         if not args.has_key('prefix_graph'): fichier='graph'
         else:                                fichier = args['prefix_graph']
      if trace:
         if INFO>=1: UTMESS('I','RECAL0_17')
         GRAPHE_UL_OUT=GRAPHIQUE['UNITE']
         pilote=GRAPHIQUE['PILOTE']
         reca_utilitaires.graphique(GRAPHIQUE['FORMAT'],L_F,RESU_EXP,RESU_CALC,iter,GRAPHE_UL_OUT,pilote,fichier)

   if( METHODE == 'EXTERNE'):
#      if mode_python: return fonctionnelle, gradient
      return fonctionnelle, gradient

   # Si pas de convergence alors diagnostic NOOK_TEST_RESU
   if residu > RESI_GLOB_RELA:
      from Cata.cata import CREA_TABLE, TEST_TABLE
      _tmp = []
      _tmp.append( { 'PARA': 'ITER_MAXI', 'LISTE_R': 0.0, } )
      motscle= {'LISTE': _tmp }

      TBL=CREA_TABLE(**motscle);
   
      TEST_TABLE(TABLE=TBL,
                 TYPE_TEST='SOMM',
                 NOM_PARA='ITER_MAXI',
                 VALE=1.,);

   #_____________________________________________
   #
   # CREATIONS DE LA LISTE DE REELS CONTENANT 
   # LES VALEURS DES PARAMETRES A CONVERGENCE
   #_____________________________________________

   
   nomres = Sortie(LIST_NOM_PARA, LIST_PARA, val, CALCUL_ASTER, Mess)
   return 













#-------------------------------------------------------------------------------
if __name__ == '__main__':

    mode_python = True

    from optparse import OptionParser, OptionGroup
    from Utilitai.Utmess import UTMESS
    
    p = OptionParser(usage='usage: %s fichier_export [options]' % sys.argv[0])
    p.add_option('-i', '--input',        action='store',   dest='input',         type='string',   default='input.txt',   help='fichier contenant les parametres')
    p.add_option('-o', '--output',       action='store',   dest='output',        type='string',   default='output.txt',  help='fichier contenant la fonctionnelle')
    p.add_option('-g', '--output_grad',  action='store',   dest='output_grad',   type='string',   default='grad.txt',    help='fichier contenant le gradient')
    p.add_option('-p', '--prefix_graph', action='store',   dest='prefix_graph',  type='string',   default='graph',       help='prefixe des fichiers contenant les courbes')
    p.add_option('-v', '--info',         action='store',   dest='INFO',          type='int',                             help='niveau de message (-1, 0, 1, 2)')
    p.add_option('-f', '--follow',       action='store',   dest='follow_output', type='string',                          help="affiche ou non l'output du fichier Aster (True/False)")
    p.add_option('-F', '--objective',    action='store',   dest='objective',     type='string',                          help="type de la fonctionnelle (float/vector)")
    p.add_option('-G', '--gradient',     action='store',   dest='gradient' ,     type='string',   default='no',          help="calcul du gradient par Aster (no/normal/adim)")
    p.add_option('-d', '--display',      action='store',   dest='display' ,      type='string',                          help="renvoi du DISPLAY (pour que la creation des courbes soit moins genante)")

#    p.add_option('-n', '--name',         action='store',   dest='name',          type='string',   default='optim',       help="prefixe du fichier de bilan")

    opts, args = p.parse_args()

    # renvoi du DISPLAY (pour que la creation des courbes soit moins genante)
    if opts.display: os.environ['DISPLAY'] = opts.display


    # Options par defaut
    optparse_input = optparse_output = optparse_output_grad = optparse_prefix_graph = optparse_INFO = optparse_follow_output = optparse_objective = optparse_gradient = optparse_name = None

    if opts.INFO==None: opts.INFO=0

    if opts.input:                                 optparse_input         = True
    if opts.output:                                optparse_output        = True
    if opts.output_grad:                           optparse_output_grad   = True
    if opts.prefix_graph:                          optparse_prefix_graph  = True
    if opts.INFO in [-1, 0, 1, 2]:                 optparse_INFO          = True
    if opts.follow_output in ['True', 'False']:    optparse_follow_output = True
    if opts.objective in ['float', 'vector']:      optparse_objective     = True
    if opts.gradient in ['no', 'normal', 'adim']:  optparse_gradient      = True
#    if opts.name:                                  optparse_name          = True

    if opts.follow_output=='True':  opts.follow_output=True
    if opts.follow_output=='False': opts.follow_output=False


    # Fichier .export
    if args:
       fichier_export =  args[0]
       if not os.path.isfile(fichier_export): fichier_export = None

    INFO = opts.INFO
    input_file  = opts.input
    output_file = opts.output
    output_grad = opts.output_grad
    type_fonctionnelle = opts.objective

    # Import d'as_profil
    if os.environ.has_key('ASTER_ROOT'):
      sys.path.append(os.path.join(os.environ['ASTER_ROOT'], 'ASTK', 'ASTK_SERV', 'lib'))
    try:
      from as_profil import ASTER_PROFIL
    except:
      UTMESS('F','RECAL0_99')

    # Efface les fichiers resultats
    try:    os.remove(output)
    except: pass
    try:    os.remove(output_grad)
    except: pass


    # Si le fichier export n'est pas en argument on prend l'export qui est dans le rep courant
    if not fichier_export:
      # Lecture du fichier .export
      list_export = glob('*.export')
      if len(list_export) != 1:
         UTMESS('F','RECAL0_98')
      else:
         fichier_export = list_export[0]
    prof = ASTER_PROFIL(fichier_export)

    # Execution du fichier .comm
    nom_comm = None
    # fichier/répertoire
    for lab in ('data', 'resu'):
      l_fr = getattr(prof, lab)
      l_tmp = l_fr[:]

      for dico in l_tmp:
        # fichiers
        if not dico['isrep']:
          # Ancien .comm a executer
          if dico['type'] == 'comm' and dico['ul'] == '1':
            nom_comm = dico['path']

    # parametres
    for lab in ('param',):
      l_fr = getattr(prof, lab)
#      print l_fr
#      print l_fr['version']
      try:    os.environ['ASTER_VERSION'] = l_fr['version'][0]
      except: pass


    if not nom_comm:
       UTMESS('F','RECAL0_97')
    if not os.path.isfile(nom_comm):
       UTMESS('F','RECAL0_96', valk=nom_comm)



    # -------------------------------------------------------------------
    # Lecture des valeurs d'entree
    if INFO==2: UTMESS('I','RECAL0_95',valk=input_file)
    try:
       f = open(input_file, 'r')
       txt = f.read()
       f.close()
       txt = txt.replace(',', ' ')
       val_externe = [ float(x) for x in txt.strip().split() ]
    except:
       UTMESS('F','RECAL0_94',valk=input_file)
    if INFO>=2: UTMESS('I','RECAL0_93', valk=str(val_externe))
    if optparse_INFO and opts.INFO == -1: print '\n'+ str(val_externe)


    # -------------------------------------------------------------------
    # Efface les fichiers d'entree et de sortie
    try:    os.remove(input_file)
    except: pass
    try:    os.remove(output_file)
    except: pass
    try:    os.remove(output_grad)
    except: pass




    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    # Ci-dessous on extrait le fichier de commande jusqu'a la commande MACR_RECAL exclue (fichiernew)
    # Puis la commande MACR_RECAL (commandenew)
    # Ensuite on annule l'effet des commandes Aster et on evalue en Python les deux chaines de textes

    # Lecture du fichier .comm
    f=open(nom_comm,'r')
    fichier=f.read()
    f.close

    # Extraction des deux parties dans le fichier de commande
    fichiernew=None
    commandenew=None
    nb_par=-99
    txt1='MACR_RECAL'
    txt2='('
    txt3=')'
    for ligne in fichier.split('\n'):
       if ligne.find( txt1 )!=-1 and ligne.find( txt2 )!=-1 and ligne.strip()[0]!='#':
          nb_par=0
          index_deb1 = fichier.index(ligne)
          fichiernew=fichier[:index_deb1]
#          if debug: print 80*'*' + 2*'\n'+fichiernew+80*'*' + 2*'\n'
       if fichiernew and ligne.find( txt2 )!=-1: nb_par+=1
       if fichiernew and ligne.find( txt3 )!=-1: nb_par-=1
       if fichiernew and nb_par==0:
          index_fin1 = fichier.index(ligne)+len(ligne)
          commandenew=fichier[index_deb1:index_fin1]

          # Remplace le nom de concept a gauche du signe egal
          index_deb2 = commandenew.index(txt1)
          commandenew='fonctionnelle, gradient='+commandenew[index_deb2:]+ '\n'

          if debug: print 80*'*' + 2*'\n'+commandenew+80*'*' + 2*'\n'
          break
    if not fichiernew or not commandenew:
       UTMESS('F','RECAL0_92', valk=nom_comm)


    # -------------------------------------------------------------------
    # Import du module Utilitai
    sys.path.append(os.path.join(os.getcwd(), 'Python'))
    sys.path.append(os.path.join(os.environ['ASTER_ROOT'], os.environ['ASTER_VERSION'], 'bibpyt'))
    try:
       import Utilitai
       from Utilitai.System import ExecCommand
    except:
       UTMESS('F','RECAL0_91')


    # -------------------------------------------------------------------
    # On annule les commandes Aster du fichier maitre .comm
    def DEBUT(*args, **kwargs): pass
    def FIN(*args, **kwargs): pass
    def MACR_RECAL(*args, **kwargs): pass
    def _F(*args, **kwargs): return kwargs
    def DEFI_LIST_REEL(*args, **kwargs): pass
    def DEFI_FONCTION(*args, **kwargs): pass
    def TEST_FONCTION(*args, **kwargs): pass
    def DEFI_CONSTANTE(*args, **kwargs): pass


    # -------------------------------------------------------------------
    # Evaluation du fichier de commande Aster jusqu'a MACR_RECAL
    lance_aster = False
    try:
       exec(fichiernew)
    except:
       txt = "Le mode EXTERNE tourne en mode degrade. Lire la documentation."
       UTMESS('A','RECAL0_90')
       lance_aster = True
    else:
       exec(commandenew.replace(txt1, 'macr_recal_externe'))
#        try:
#           exec(commandenew.replace(txt1, 'macr_recal_externe'))
#        except Exception, err:
#           print err
#           txt = "Erreur lors de l'execution de la commande MACR_RECAL" 
#           UTMESS('F','RECAL0_12',valk=txt)

       Ecriture_Fonctionnelle(output_file, type_fonctionnelle, fonctionnelle)
       Ecriture_Derivees(output_grad, gradient)



    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    # --------------------------------------------------------------------------------------------------------
    # Si l'evaluation du fichier de commande Aster jusqu'a MACR_RECAL a echoue, on execute Aster "normalement"
    if lance_aster:

       _PARAM_ = '_PARAM_'
       new_fichier_comm = os.getcwd() + os.sep + 'tmp_comm'
       new_fichier_export  = os.getcwd() + os.sep + fichier_export.split('/')[-1] + '_new'

       # Lecture du fichier .comm
       f=open(nom_comm,'r')
       fichier=f.read()
       f.close
    
       # -------------------------------------------------------------------
       # Modification du fichier .comm (changement des valeurs, ecriture du resultat dans un fichier)
       if INFO==2: UTMESS('I','RECAL0_89',valk=nom_comm)
       f = open(nom_comm, 'r')
       ok1 = ok3 = ok4 = False
       txt = ''
       for ligne in f:
          if ligne.find('MACR_RECAL')!=-1 and ligne.strip()[0]!='#':            # On determine le nom du concept sortant de MACR_RECAL
             ok3 = True
             _RESU_ = ligne.split('=')[0].strip()
             txt += ligne
          elif ligne.strip()[:len(_PARAM_)] == _PARAM_:                        # On change les parametres : la variables _PARAM_
             ok1 = True
             txt += _PARAM_ + " = " + str(val_externe) + '\n'
          elif ligne.find('METHODE')!=-1 and ligne.strip()[0]!='#':         # On verifie bien que la methode externe est choisi
             if ligne.find("EXTERNE")!=-1:
                ok4 = True
                txt += ligne
          else: txt += ligne
       f.close()

       if not ok1:  UTMESS('F','RECAL0_88',valk=_PARAM_)
       if not ok3:  UTMESS('F','RECAL0_87')
       if not ok4:  UTMESS('F','RECAL0_86')

       txt = txt.replace('_RESU_', _RESU_)

       # Ecriture du nouveau fichier comm temporaire
       if INFO==2: UTMESS('I','RECAL0_85',valk=new_fichier_comm)
       f = open(new_fichier_comm, 'w')
       f.write(txt)
       f.close()

       # On remplace dans l'export par le nouveau .comm
       prof = ASTER_PROFIL(fichier_export)
       for lab in ('data', 'resu'):
          l_fr = getattr(prof, lab)
          l_tmp = l_fr[:]
          for dico in l_tmp:
             # fichiers
             if not dico['isrep']:
                # On remplace par le nouveau .comm
                if dico['type'] == 'comm' and dico['ul'] == '1':
                   dico['path'] = new_fichier_comm

#              if lab == 'resu':
#                 dico['path'] = os.path.join(tmp_macr_recal, os.path.basename(dico['path']))

       # On ajoute au profil le fichier output.txt (unite logique 1900)
       try:    os.remove('./fort.1900')
       except: pass
       if not output_file.find(os.sep)!=-1: output_file = os.getcwd() + os.sep + output_file
       prof.Set('R', {'type':'libr', 'isrep':False, 'path': output_file, 'ul':1900, 'compr': False} )

       # On ajoute au profil le fichier grad.txt (unite logique 1901)
       if optparse_gradient and opts.gradient!='no':
          try:    os.remove('./fort.1901')
          except: pass
          output_grad = opts.gradient
          if not output_grad.find(os.sep)!=-1: output_grad = os.getcwd() + os.sep + output_grad
          prof.Set('R', {'type':'libr', 'isrep':False, 'path': output_grad, 'ul':1901, 'compr': False} )


       # Ecriture du nouveau fichier export
       try:
          if INFO==2: UTMESS('I','RECAL0_85',valk=new_fichier_export)
          prof.WriteExportTo(new_fichier_export)
       except:
          UTMESS('F','RECAL0_84',valk=new_fichier_export)
       if debug: prof.WriteExportTo('/tmp/exp')


       # chemin vers as_run
       if os.environ.has_key('ASTER_ROOT'):
          as_run = os.path.join(os.environ['ASTER_ROOT'], 'ASTK', 'ASTK_SERV', 'bin', 'as_run')
       else:
          as_run = 'as_run'
          if INFO>=1: UTMESS('A', 'RECAL0_83')


       # Import du module Utilitai
       sys.path.append(os.path.join(os.environ['ASTER_ROOT'], os.environ['ASTER_VERSION'], 'bibpyt'))
       try:
          import Utilitai
          from Utilitai.System import ExecCommand
       except:
          UTMESS('F','RECAL0_91')


       # Lancement d'Aster avec le deuxieme export
       cmd = '%s %s' % (as_run, new_fichier_export)
       if INFO>=2: UTMESS('I','EXECLOGICIEL0_8',valk=cmd)
       iret, txt_output = ExecCommand(cmd, follow_output=opts.follow_output,verbose=opts.follow_output)
       if INFO>=2: UTMESS('I','EXECLOGICIEL0_12',valk=cmd)

       try:    os.remove(new_fichier_comm)
       except: pass
       try:    os.remove(new_fichier_export)
       except: pass


