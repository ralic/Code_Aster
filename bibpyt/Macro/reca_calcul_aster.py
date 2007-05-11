#@ MODIF reca_calcul_aster Macro  DATE 07/05/2007   AUTEUR ASSIRE A.ASSIRE 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE ASSIRE A.ASSIRE
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

# mode_include = False
# __follow_output = False
# table_sensibilite = False
debug = False

__commandes_aster__ = False


import copy, Numeric, types, os, sys, pprint, math
from glob import glob

try:
  import aster
except: pass

try:
   from Utilitai.Utmess import UTMESS
except ImportError:
   def UTMESS(code,sprg,texte):
      fmt='\n <%s> <%s> %s\n\n'
      print fmt % (code,sprg,texte)
      if code=='F': sys.exit()

from Utilitai.System import ExecCommand

# Nom de la routine
nompro = 'MACR_RECAL'


# ------------------------------------------------------------------------------

class PARAMETRES:

  def __init__(self, METHODE, UNITE_RESU, INFO=1, fich_output='./REPE_OUT/output_esclave.txt', mode_include=False, follow_output=False, table_sensibilite=False, memjeveux_esclave=None, PARA_DIFF_FINI=1.E-3, ITER_MAXI=10, ITER_FONC_MAXI=100):

    self.METHODE        = METHODE
    self.UNITE_RESU     = UNITE_RESU
    self.INFO           = INFO
    self.fich_output    = fich_output
    self.PARA_DIFF_FINI = PARA_DIFF_FINI
    self.ITER_FONC_MAXI = ITER_FONC_MAXI
    self.ITER_MAXI      = ITER_MAXI,

    try:
      import Cata, aster
      from Cata.cata import INCLUDE, DETRUIRE, FIN, DEFI_FICHIER, IMPR_TABLE, LIRE_TABLE, INFO_EXEC_ASTER, EXTR_TABLE, CREA_TABLE
      from Accas import _F
    except:
      mode_include = False

    if not mode_include:
      try:
        from Macro.lire_table_ops import lecture_table
        mode_aster = True
      except:
        try:
          sys.path.append( './Python/Macro' )
          from Macro.lire_table_ops import lecture_table
          mode_aster = False
        except:
          UTMESS('F','MACR_RECAL',"Probleme : impossible d'importer le module de lecture des tables!")
    self.mode_include      = mode_include
    self.follow_output     = follow_output
    self.mode_aster        = mode_aster
    self.memjeveux_esclave = memjeveux_esclave    
    self.table_sensibilite = table_sensibilite

    self.vector_output     = False
    self.error_output      = False


# ------------------------------------------------------------------------------

class CALCUL_ASTER:

  def __init__(self, PARAMETRES, UL, para, reponses, LIST_SENSI=[], LIST_DERIV=[]):

    self.UL                    = UL
    self.para                  = para
    self.reponses              = reponses
    self.LIST_SENSI            = LIST_SENSI
    self.LIST_DERIV            = LIST_DERIV

    self.METHODE               = PARAMETRES.METHODE
    self.UNITE_RESU            = PARAMETRES.UNITE_RESU
    self.INFO                  = PARAMETRES.INFO
    self.fich_output           = PARAMETRES.fich_output
    self.mode_include          = PARAMETRES.mode_include
    self.follow_output         = PARAMETRES.follow_output
    self.table_sensibilite     = PARAMETRES.table_sensibilite
    self.mode_aster            = PARAMETRES.mode_aster
    self.memjeveux_esclave     = PARAMETRES.memjeveux_esclave    
    self.PARA_DIFF_FINI        = PARAMETRES.PARA_DIFF_FINI
    self.ITER_FONC_MAXI        = PARAMETRES.ITER_FONC_MAXI
    self.vector_output         = PARAMETRES.vector_output
    self.error_output          = PARAMETRES.vector_output

    self.UNITE_GRAPHIQUE       = None
    self.new_export            = 'tmp_export'
    self.nom_fichier_mess_fils = None
    self.nom_fichier_resu_fils = None

    self.fichier_esclave       = None

    self.evaluation_fonction   = 0

    self.L_J_init              = None
    self.val                   = None
    self.L                     = None
    self.L_deriv_sensible      = None



  # ------------------------------------------------------------------------------

  def Lancement_Commande(self, cmd):

          if self.INFO>=1: UTMESS('I','MACR_RECAL',"Lancement de la commande : " + cmd)

          fich_output       = self.fich_output
          follow_output     = self.follow_output

          # Lancement d'Aster avec le deuxieme export
          iret, txt_output = ExecCommand(cmd, follow_output=self.follow_output,verbose=False)

          if fich_output:
             # Recuperation du .mess 'fils'
             f=open(fich_output, 'w')
             f.write( txt_output )
             f.close()

          if self.INFO>=1: UTMESS('I','MACR_RECAL',"Fin du lancement de la commande : " + cmd)

          diag = self.Recuperation_Diagnostic(txt_output)

          return


  # ------------------------------------------------------------------------------

  def Recuperation_Diagnostic(self, output):

    txt = '--- DIAGNOSTIC JOB :'
    diag = None
    for ligne in output.splitlines():
      if ligne.find(txt) > -1:
        diag = ligne.split(txt)[-1].strip()
        break

    if self.INFO>=1: UTMESS('I','MACR_RECAL',"Diagnostic du calcul esclave : " + diag)

    if diag in ['OK', 'NOOK_TEST_RESU', '<A>_ALARM', '<F>_COPY_ERROR']: return True
    else:
      UTMESS('F','MACR_RECAL',"Le fichier esclave ne s'est pas terminé correctement.")


  # ------------------------------------------------------------------------------

  def Remplace_fichier_esclave(self, val_in): pass


  # ------------------------------------------------------------------------------

  def calcul_Aster(self, val, INFO=0):

        self.val   = val
        UL         = self.UL
        para       = self.para
        reponses   = self.reponses
        UNITE_RESU = self.UNITE_RESU
        LIST_SENSI = self.LIST_SENSI
        LIST_DERIV = self.LIST_DERIV

        mode_include      = self.mode_include
        follow_output     = self.follow_output
        table_sensibilite = self.table_sensibilite

        if self.evaluation_fonction > self.ITER_FONC_MAXI:
           UTMESS('F', nompro, "Le nombre d'evaluation de la fonctionnelle depasse le critere ITER_FONC_MAXI.")
        self.evaluation_fonction += 1


        if not mode_include:

          # Creation du repertoire temporaire pour l'execution de l'esclave
          tmp_macr_recal = self.Creation_Temporaire_Esclave()

          # Creation du fichier .export de l'esclave
          self.Creation_Fichier_Export_Esclave(tmp_macr_recal)  

        # Fichier esclave a modifier (si methode EXTERNE alors on prend directement le fichier esclave, sinon c'est le fort.UL dans le repertoire d'execution
        try:
           if self.METHODE=='EXTERNE':
              fic = open(self.fichier_esclave,'r')
           else:
              fic = open('fort.'+str(UL),'r')

           # On stocke le contenu de fort.UL dans la variable fichier qui est une string 
           fichier=fic.read()
           # On stocke le contenu initial de fort.UL dans la variable fichiersauv 
           fichiersauv=copy.copy(fichier)
           fic.close()
        except:
           UTMESS('F', nompro, "Impossible de relire le fichier esclave : \n " + str(self.fichier_esclave))

        # chemin vers as_run
        if os.environ.has_key('ASTER_ROOT'):
           as_run = os.path.join(os.environ['ASTER_ROOT'], 'ASTK', 'ASTK_SERV', 'bin', 'as_run')
        elif os.path.isfile(aster.repout() + os.sep + 'as_run'):
           as_run = aster.repout() + os.sep + 'as_run'
        else:
           as_run = 'as_run'
           if INFO>=1: UTMESS('A', nompro, "Variable d'environnement ASTER_ROOT absente, " \
                               "on essaiera avec 'as_run' dans le $PATH.")

        if __commandes_aster__:
          try:
            from Cata.cata import INCLUDE, DETRUIRE, FIN, DEFI_FICHIER, IMPR_TABLE, LIRE_TABLE, INFO_EXEC_ASTER, EXTR_TABLE, CREA_TABLE
          except: 
            message = "Erreur"
            UTMESS('F', nompro, message)

        # Utilisation du module Python de LIRE_TABLE
        if self.mode_aster:
          from Macro.lire_table_ops import lecture_table
        else:
          try:
            sys.path.append( './Python/Macro' )
            from lire_table_ops import lecture_table
          except:
            UTMESS('F','MACR_RECAL',"Probleme : impossible d'importer le module de lecture des tables!")

        UTMESS('I','MACR_RECAL',"Calcul de F avec les parametres:\n%s" % 'bla')
        txt = []
        for i in para:
          txt.append( "\t\t\t%s : %s" % (i, val[para.index(i)]) )
        if INFO>=1: UTMESS('I','MACR_RECAL',"Calcul de F avec les parametres:\n%s" % '\n'.join(txt))
  
  
  # MARCHE PAS !!
#         # Quelques verifications/modifications sur le fichier esclave, pour blindage
#         fichiernew=[]
#         for ligne in fichier.split('\n'):
#            # DEBUT et FIN (on retire les espaces entre le mot-clé et la premiere parenthese)
#            for (txt1, txt2) in [('DEBUT','('), ('FIN','(')]:
#               if ligne.find( txt1 )!=-1 and ligne.find( txt2 )!=-1:
#                  index_deb1 = ligne.index(txt1)
#                  ligne1 = ligne[ index_deb1+len(txt1):]
#                  if ligne.find( txt1 )!=-1:
# 
# 
#            for (txt1, txt2) in [('DEBUT','('), ('FIN','(')]:
#               if ligne.find( txt1 )!=-1 and ligne.find( txt2 )!=-1:
#                  index_deb1 = ligne.index(txt1)
#                  index_fin1 = index_deb1 + len(txt1)
#                  index_deb2 = ligne.index(txt2)
#                  index_fin2 = index_deb1 + len(txt2)
#                  ligne = ligne[:index_fin1]+ligne[index_deb2:]
#               # on retire les parametes en commentaires
#               for txt in para:
#                  if ligne.find(txt)!=-1:
#                    if ligne.replace(' ', '')[0] == '#': ligne = ''
#            fichiernew.append(ligne)
#         fichier = '\n'.join(fichiernew)



        # On supprime tous les commentaires du fichier esclave
        fichiernew=[]
        for ligne in fichier.split('\n'):
           if ligne.strip() != '':
              if ligne.replace(' ', '')[0] == '#': ligne = ''
              fichiernew.append(ligne)
        fichier = '\n'.join(fichiernew)


        #Fichier_Resu est une liste ou l'on va stocker le fichier modifié
        #idée générale :on délimite des 'blocs' dans fichier
        #on modifie ou non ces blocs suivant les besoins 
        #on ajoute ces blocs dans la liste Fichier_Resu
        Fichier_Resu=[]                      

        # Dans le cas du mode INCLUDE on enleve le mot-clé DEBUT
        if mode_include:
          try: 
             #cherche l'indice de DEBUT()
             index_deb=fichier.index('DEBUT(')
             while( fichier[index_deb]!='\n'):
                index_deb=index_deb+1
             #on restreint fichier en enlevant 'DEBUT();'
             fichier = fichier[index_deb+1:]   
          except:
             #on va dans l'except si on a modifié le fichier au moins une fois
             pass 
  
#         print 60*'o'
#         print fichier
#         print 60*'o'

        # On enleve le mot-clé FIN()
        try:
           #cherche l'indice de FIN()
           index_fin = fichier.index('FIN(')
           #on restreint fichier en enlevant 'FIN();'
           fichier = fichier[:index_fin]
        except : pass

#         print 60*'o'
#         print fichier
#         print 60*'o'

        #--------------------------------------------------------------------------------
        #on cherche à délimiter le bloc des parametres dans le fichier
        #Tout d'abord on cherche les indices d'apparition des paras dans le fichier 
        #en effet l'utilisateur n'est pas obligé de rentrer les paras dans optimise
        #avec le meme ordre que son fichier de commande
        index_para = Numeric.zeros(len(para))
        for i in range(len(para)):
           index_para[i] = fichier.index(para[i])

        #On range les indices par ordre croissant afin de déterminer
        #les indice_max et indice_min
        index_para = Numeric.sort(index_para)
        index_first_para = index_para[0]
        index_last_para = index_para[len(index_para)-1]
  
  
        #on va délimiter les blocs intermédiaires entre chaque para "utiles" à l'optimsation
        bloc_inter ='\n'
        for i in range(len(para)-1):
           j = index_para[i]
           k = index_para[i+1]
           while(fichier[j]!= '\n'):
              j=j+1
           bloc_inter=bloc_inter + fichier[j:k] + '\n'
  
        #on veut se placer sur le premier retour chariot que l'on trouve sur la ligne du dernier para
        i = index_last_para 
        while(fichier[i] != '\n'):
           i = i + 1
        index_last_para  = i
        #on délimite les blocs suivants:
        pre_bloc = fichier[:index_first_para]        #fichier avant premier parametre
        post_bloc = fichier[ index_last_para+ 1:]    #fichier après dernier parametre
  
        #on ajoute dans L tous ce qui est avant le premier paramètre 
        Fichier_Resu.append(pre_bloc)
        Fichier_Resu.append('\n')
  
        # Liste des parametres utilisant la SENSIBILITE
        liste_sensibilite = []
        if len(LIST_SENSI)>0:
          for i in LIST_SENSI:
            liste_sensibilite.append( i )
  
        #On ajoute la nouvelle valeur des parametres
        dim_para=len(para)
        for j in range(dim_para):
           if not para[j] in liste_sensibilite:
             Fichier_Resu.append(para[j]+'='+str(val[j]) + ';' + '\n')
           else:
             Fichier_Resu.append(para[j]+'=DEFI_PARA_SENSI(VALE='+str(val[j]) + ',);' + '\n')


        #On ajoute à Fichier_Resu tous ce qui est entre les parametres
        Fichier_Resu.append(bloc_inter)
        
        Fichier_Resu.append(post_bloc)
  
        #--------------------------------------------------------------------------------
        #on va ajouter la fonction d'extraction du numarray de la table par la méthode Array 
        #et on stocke les réponses calculées dans la liste Lrep
        #qui va etre retournée par la fonction calcul_Aster
        if mode_include:
          self.g_context['Lrep'] = []
          Fichier_Resu.append('Lrep=[]'+'\n')
          for i in range(len(reponses)):
             Fichier_Resu.append('t'+str(reponses[i][0])+'='+str(reponses[i][0])+'.EXTR_TABLE()'+'\n')
             Fichier_Resu.append('F = '+'t'+str(reponses[i][0])+'.Array('+"'"+str(reponses[i][1])+"'"+','+"'"+str(reponses[i][2])+"'"+')'+'\n')
             Fichier_Resu.append('Lrep.append(F)'+'\n')

        #ouverture du fichier fort.3 et mise a jour de celui ci
        x=open('fort.'+str(UL),'w')
        if mode_include:
          x.writelines('from Accas import _F \nfrom Cata.cata import * \n')
        x.writelines(Fichier_Resu)
        x.close()

        del(pre_bloc)
        del(post_bloc)
        del(fichier)

        # ----------------------------------------------------------------------------------
        # Execution d'une deuxieme instance d'Aster

        if not mode_include:

          # Ajout des commandes d'impression des tables Resultats et Derivees à la fin du fichier esclave
          Fichier_Resu = []
          num_ul = '99'

          # Tables correspondant aux Resultats
          for i in range(len(reponses)):
             _ul = str(int(100+i))

             try:    os.remove( tmp_macr_recal+os.sep+"REPE_TABLE"+os.sep+"fort."+_ul )
             except: pass

             Fichier_Resu.append("\n# Recuperation de la table : " + str(reponses[i][0]) + "\n")
             Fichier_Resu.append("DEFI_FICHIER(UNITE="+num_ul+", FICHIER='"+tmp_macr_recal+os.sep+"REPE_TABLE"+os.sep+"fort."+_ul+"',);\n")
             Fichier_Resu.append("IMPR_TABLE(TABLE="+str(reponses[i][0])+", FORMAT='ASTER', UNITE="+num_ul+", INFO=1, FORMAT_R='E30.20',);\n")
             Fichier_Resu.append("DEFI_FICHIER(ACTION='LIBERER', UNITE="+num_ul+",);\n")

          # Tables correspondant aux Derivees
          if len(LIST_SENSI)>0:
              i = 0
              for _para in LIST_SENSI:
                  _lst_tbl  = LIST_DERIV[_para][0]
                  for _lst_tbl in LIST_DERIV[_para]:
                     i += 1
                     _tbl = _lst_tbl[0]
  
                     _ul = str(int(100+len(reponses)+i))
                     try:    os.remove( tmp_macr_recal+os.sep+"REPE_TABLE"+os.sep+"fort."+_ul )
                     except: pass
  
                     Fichier_Resu.append("\n# Recuperation de la table derivee : " + _tbl + " (parametre " + _para + ")\n")
                     Fichier_Resu.append("DEFI_FICHIER(UNITE="+num_ul+", FICHIER='"+tmp_macr_recal+os.sep+"REPE_TABLE"+os.sep+"fort."+_ul+"',);\n")
                     if table_sensibilite:
                       Fichier_Resu.append("IMPR_TABLE(TABLE="+_tbl+", SENSIBILITE="+_para+", FORMAT='ASTER', UNITE="+num_ul+", INFO=1, FORMAT_R='E30.20',);\n")
                     else:
                       Fichier_Resu.append("IMPR_TABLE(TABLE="+_tbl+", FORMAT='ASTER', UNITE="+num_ul+", INFO=1, FORMAT_R='E30.20',);\n")
                     Fichier_Resu.append("DEFI_FICHIER(ACTION='LIBERER', UNITE="+num_ul+",);\n")

          # Ecriture du "nouveau" fichier .comm
          x=open('fort.'+str(UL),'a')
          x.write( '\n'.join(Fichier_Resu) )
          x.write('\nFIN();\n')
          x.close()

#          os.system("cat %s" % self.new_export)

          # Lancement du calcul Aster esclave
          cmd = '%s %s' % (as_run, self.new_export)
          self.Lancement_Commande(cmd)

          # Recuperation du .mess et du .resu 'fils'
          if self.METHODE != 'EXTERNE':
             if self.nom_fichier_mess_fils:
                 cmd = 'cp ' + tmp_macr_recal + os.sep + self.nom_fichier_mess_fils + ' ./REPE_OUT/'
                 os.system( cmd )
             if self.nom_fichier_resu_fils:
                 cmd = 'cp ' + tmp_macr_recal + os.sep + self.nom_fichier_resu_fils + ' ./REPE_OUT/'
                 os.system( cmd )
  
          if __commandes_aster__:
              # Unite logique libre
              _tbul_libre=INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
              _ul_libre=_tbul_libre['UNITE_LIBRE',1]


          # ------------------------------------------------------
          # Recuperation des tableaux resultats
          Lrep=[]
          _TB = [None]*len(reponses)
          for i in range(len(reponses)):

            if __commandes_aster__:

                # Version par des commandes Aster
                # -------


                DEFI_FICHIER(UNITE=_ul_libre, FICHIER=tmp_macr_recal+os.sep+"REPE_TABLE"+os.sep+"fort."+str(int(100+i)), );
                try:
                  _TB[i]=LIRE_TABLE(UNITE=_ul_libre,
                                    FORMAT='ASTER',
                                    NUME_TABLE=1,
                                    SEPARATEUR=' ',);
                  DEFI_FICHIER(ACTION='LIBERER', UNITE=_ul_libre,);
                  tREPONSE=_TB[i].EXTR_TABLE()
  
                  F = tREPONSE.Array( str(reponses[i][1]), str(reponses[i][2]) )
                  Lrep.append(F)
                except:
                  message = "Impossible de recuperer les resultats de calcul esclave!"
                  UTMESS('F', nompro, message)

            else:

                # Version par utilisation directe du python de lire_table
                # -------

                # Chemin vers le fichier contenant la table
                _fic_table = tmp_macr_recal+os.sep+"REPE_TABLE"+os.sep+"fort."+str(int(100+i))

                try:
                   file=open(_fic_table,'r')
                   texte=file.read()
                   file.close()
                except Exception, err:
                   ier=1
                   message = """Impossible de recuperer les resultats de calcul esclave (lecture des tables)! 
Le calcul esclave n'a pas du se terminer correctement (ajouter un repertoire dans ASTK en Resultat avec 
le type repe et voir l'output du fichier esclave dans ce repertoire.
Message:
"""
                   message += str(err)
                   UTMESS('F', nompro, message)

                try:
                   ier, message, table_lue = lecture_table(texte, 1, ' ')
                   list_para = table_lue.para
                   tab_lue   = table_lue.values()
                except Exception, err:
                   ier=1
                   message = "Impossible de recuperer les resultats de calcul esclave (lecture des tables)!\nMessage:\n" + str(err)

                if ier!=0 :
                   UTMESS('F', nompro, message)

                try:
                    nb_val = len(tab_lue[ list_para[0] ])
                    F = Numeric.zeros((nb_val,2), Numeric.Float)
                    for k in range(nb_val):
#                       F[k][0] = tab_lue[ list_para[0] ][1][k]
#                       F[k][1] = tab_lue[ list_para[1] ][1][k]
                      F[k][0] = tab_lue[ str(reponses[i][1]) ][k]
                      F[k][1] = tab_lue[ str(reponses[i][2]) ][k]
                    Lrep.append(F)
                except Exception, err:
                    message = "Impossible de recuperer les resultats de calcul esclave (recuperation des tables)!\nMessage:\n" + str(err)
                    UTMESS('F', nompro, message)


          # ------------------------------------------------------
          # Recuperation des tableaux des derivees (SENSIBILITE)
          L_deriv={}
          if len(LIST_SENSI)>0:
              _lon = 0
              for _para in LIST_SENSI:
                  _lon += len(LIST_DERIV[_para])
              _TBD = [None]*_lon

              i = 0
              for _para in LIST_SENSI:

                  L_deriv[_para] = []
                  _lst_tbl  = LIST_DERIV[_para][0]

                  for _lst_tbl in LIST_DERIV[_para]:
                      j = LIST_DERIV[_para].index(_lst_tbl)
                      _tbl = _lst_tbl[0]

                      if __commandes_aster__:

                          # Version par des commandes Aster
                          # -------

                          DEFI_FICHIER(UNITE=_ul_libre, FICHIER=tmp_macr_recal+os.sep+"REPE_TABLE"+os.sep+"fort."+str(int(100+len(reponses)+1+i)),);
                          _TBD[i]=LIRE_TABLE(UNITE=_ul_libre,
                                           FORMAT='ASTER',
                                           NUME_TABLE=1,
                                           SEPARATEUR=' ',);
                          DEFI_FICHIER(ACTION='LIBERER', UNITE=_ul_libre,);
                          tREPONSE=_TBD[i].EXTR_TABLE()
                          DF = tREPONSE.Array( str(LIST_DERIV[_para][j][1]), str(LIST_DERIV[_para][j][2]) )
                          L_deriv[_para].append(DF)
                          i+=1

                      else:

                          # Version par utilisation directe du python de lire_table
                          # -------
      
                          # Chemin vers le fichier contenant la table
                          _fic_table = tmp_macr_recal+os.sep+"REPE_TABLE"+os.sep+"fort."+str(int(100+len(reponses)+1+i))

                          try:
                             file=open(_fic_table,'r')
                             texte=file.read()
                             file.close()
                          except Exception, err:
                             message = """Impossible de recuperer les resultats de calcul esclave (lecture des tables)! 
Le calcul esclave n'a pas du se terminer correctement (ajouter un repertoire en Resultat avec 
le type repe et voir l'output du fichier esclave dans ce repertoire.
Message:
"""
                             message += str(err)
                             UTMESS('F', nompro, message)

                          try:
                             ier, message, table_lue = lecture_table(texte, 1, ' ')
                             list_para = table_lue.para
                             tab_lue   = table_lue.values()
                          except Exception, err:
                             ier=1
                             message = "Impossible de recuperer les resultats de calcul esclave (lecture des tables)!\nMessage:\n" + str(err)


                          if ier!=0 :
                             UTMESS('F', nompro, message)

                          try:
                              nb_val = len(tab_lue[ list_para[0] ])
                              DF = Numeric.zeros((nb_val,2), Numeric.Float)
                              for k in range(nb_val):
#                                 DF[k][0] = tab_lue[ list_para[0] ][1][k]
#                                 DF[k][1] = tab_lue[ list_para[1] ][1][k]
                                DF[k][0] = tab_lue[ str(LIST_DERIV[_para][j][1]) ][k]
                                DF[k][1] = tab_lue[ str(LIST_DERIV[_para][j][2]) ][k]
                              L_deriv[_para].append(DF)
                              i+=1
                          except Exception, err:
                             message = "Impossible de recuperer les resultats de calcul esclave (recuperation des tables)!\nMessage:\n" + str(err)
                             UTMESS('F', nompro, message)


          # Nettoyage du export
          try:    os.remove(self.new_export)
          except: pass
  
          # Nettoyage du repertoire temporaire
          if self.METHODE == 'EXTERNE': listdir =  ['REPE_TABLE', 'base', 'REPE_IN']
          else:                         listdir =  ['.', 'REPE_TABLE', 'base', 'REPE_OUT', 'REPE_IN']
          for dir in listdir:
            try:
              for fic in os.listdir(tmp_macr_recal+os.sep+dir):
                try:    os.remove(tmp_macr_recal+os.sep+dir+os.sep+fic)
                except: pass
            except: pass



        # ----------------------------------------------------------------------------------
        # Ou bien on inclue le fichier Esclave

        elif mode_include:

          if debug: os.system('cp fort.'+str(UL)+' REPE_OUT/')
  
          INCLUDE(UNITE = UL)
  
          Lrep = self.g_context['Lrep']
          L_deriv = None
  
          # Destruction des concepts Aster
          reca_utilitaires.detr_concepts(self)
  
  
        # ----------------------------------------------------------------------------------
        # Ou alors probleme ?
        else: sys.exit(1)
  
  
        del(Fichier_Resu)
  
        # on remet le fichier dans son etat initial
        x=open('fort.'+str(UL),'w')
        x.writelines(fichiersauv)
        x.close()

        return Lrep, L_deriv



  # ------------------------------------------------------------------------------

  def calcul_FG(self, val):

     self.L, self.L_deriv_sensible = self.calcul_Aster(val, INFO=self.INFO)
     self.L_J, self.erreur = self.Simul.multi_interpole(self.L, self.reponses)
     if not self.L_J_init: self.L_J_init = copy.copy(self.L_J)
     self.J = self.Simul.norme_J(self.L_J_init, self.L_J, self.UNITE_RESU)

     # Calcul des derivees
     self.A_nodim = self.Simul.sensibilite(self, self.L, self.L_deriv_sensible, val, self.PARA_DIFF_FINI)

     self.A = self.Dim.adim_sensi( copy.copy(self.A_nodim) )
#     self.residu = self.reca_algo.test_convergence(self.gradient_init, self.erreur, self.A, Numeric.zeros(len(self.gradient_init),Numeric.Float) )
     self.residu = 0.

     if self.vector_output:
        return self.erreur, self.residu, self.A_nodim, self.A
     else:
        # norme de l'erreur
        self.norme = Numeric.dot(self.erreur, self.erreur)**0.5

        self.norme_A_nodim = Numeric.zeros( (1,len(self.para)), Numeric.Float )
        self.norme_A       = Numeric.zeros( (1,len(self.para)), Numeric.Float )
        for c in range(len(self.A[0,:])):
           norme_A_nodim = 0
           norme_A        = 0
           for l in range(len(self.A[:,0])):
              norme_A_nodim += self.A_nodim[l,c] * self.A_nodim[l,c]
              norme_A       += self.A[l,c] * self.A[l,c]
           self.norme_A_nodim[0,c] = math.sqrt( norme_A_nodim ) 
           self.norme_A[0,c] = math.sqrt( norme_A )

        return self.norme, self.residu, self.norme_A_nodim, self.norme_A


  # ------------------------------------------------------------------------------

  def calcul_F(self, val):

     self.L, self.L_deriv_sensible = self.calcul_Aster(val, INFO=self.INFO)
     L_J, erreur = self.Simul.multi_interpole(self.L, self.reponses)
     if not self.L_J_init: self.L_J_init = copy.copy(L_J)
     J = self.Simul.norme_J(self.L_J_init, L_J, self.UNITE_RESU)

     # norme de l'erreur
     norme = Numeric.sum( [x**2 for x in erreur] )

     if debug:
        print 'erreur=', erreur
        print "norme de l'erreur=", norme
        print "norme de J (fonctionnelle)=", str(J)

     if self.INFO>=1: 
        txt = "Informations de convergence :"
        txt += '\n=======================================================\n'
        if self.evaluation_fonction >1: txt += "\n   Nombre d'evaluation de la fonction =  "  + str(self.evaluation_fonction)
        txt += "\n=> Fonctionnelle     = "+str(J)

     if self.vector_output:
        if self.INFO>=1:
           txt += "\n=> Norme de l'erreur = " + str(norme)
           txt += '\n=======================================================\n'
           UTMESS('I','MACR_RECAL',txt)
        return erreur
     else:
        if self.INFO>=1:
           txt += "\n=> Erreur           = " + str(norme)
           txt += '\n=======================================================\n'
           UTMESS('I','MACR_RECAL',txt)
        return norme



  # ------------------------------------------------------------------------------

  def calcul_G(self, val):

     # Si le calcul Aster est deja effectue pour val on ne le refait pas
     if (self.val == val) and self.L and self.L_deriv_sensible: pass
     else:
        self.L, self.L_deriv_sensible = self.calcul_Aster(val, INFO=self.INFO)
     A = self.Simul.sensibilite(self, self.L, self.L_deriv_sensible, val, self.PARA_DIFF_FINI)
     A = self.Dim.adim_sensi(A)
     L_J, erreur = self.Simul.multi_interpole(self.L, self.reponses)
     grad = Numeric.dot(Numeric.transpose(A),erreur) 
     if debug: print 'grad=', grad
     return grad


  # ------------------------------------------------------------------------------

  def Creation_Temporaire_Esclave(self):
     """
        Creation du repertoire temporaire d'execution du calcul esclace
     """

     # Creation du repertoire temporaire
     tmp_macr_recal = os.getcwd() + os.sep + 'tmp_macr_recal'
     try:    os.mkdir(tmp_macr_recal)
     except: pass
     if not os.path.exists(tmp_macr_recal): UTMESS('F','MACR_RECAL',"Probleme : Impossible de creer le repertoire temporaire : " + tmp_macr_recal)
     try:    os.mkdir(tmp_macr_recal + os.sep + 'REPE_TABLE')
     except: pass
     if not os.path.exists(tmp_macr_recal + os.sep + 'REPE_TABLE'): UTMESS('F','MACR_RECAL',"Probleme : Impossible de creer le repertoire temporaire : " + tmp_macr_recal + os.sep + 'REPE_TABLE')

     return tmp_macr_recal


  # ------------------------------------------------------------------------------

  def Creation_Fichier_Export_Esclave(self, tmp_macr_recal):
     """
        Creation du fichier .export pour le calcul esclave
     """

     from as_profil import ASTER_PROFIL

     # Recuperation du fichier .export
     list_export = glob('*.export')

     if len(list_export) == 0: UTMESS('F','MACR_RECAL',"Probleme : il n'y a pas de fichier .export dans le repertoire de travail!")
     elif len(list_export) >1: UTMESS('F','MACR_RECAL',"Probleme : il y a plus d'un fichier .export dans le repertoire de travail!")

     # On modifie le profil
     prof = ASTER_PROFIL(list_export[0])

     # xterm
     if prof.param.has_key('xterm'):
        del prof.param['xterm']
     # memjeveux
     prof.args['memjeveux'] = self.memjeveux_esclave

     # fichier/répertoire
     for lab in ('data', 'resu'):
       l_fr = getattr(prof, lab)
       l_tmp = l_fr[:]

       for dico in l_tmp:

         # répertoires
         if dico['isrep']:

           # base non prise en compte
           if dico['type'] in ('base', 'bhdf'):
             l_fr.remove(dico)

           if lab == 'resu':
             dico['path'] = os.path.join(tmp_macr_recal, os.path.basename(dico['path']))

         # fichiers
         else:

           # Nom du fichier .mess (pour recuperation dans REPE_OUT)
           if dico['ul'] == '6':
             self.nom_fichier_mess_fils = os.path.basename(dico['path'])
#             self.nom_fichier_mess_fils = os.path.join(os.getcwd(), 'fort.%d' % self.UL)

           # Nom du fichier .resu (pour recuperation dans REPE_OUT)
           if dico['ul'] == '8':
             self.nom_fichier_resu_fils = os.path.basename(dico['path'])

           # Ancien .comm non pris en compte
           # Fichier d'unite logique UNITE_RESU (rapport de MACR_RECAL) non pris en compte
           if dico['type'] == 'comm' or (dico['ul'] == str(self.UNITE_RESU) and lab == 'resu'):
             l_fr.remove(dico)

           # Fichier d'unite logique UL devient le nouveau .comm
           elif dico['ul'] == str(self.UL):
             self.fichier_esclave = dico['path']
             dico['type'] = 'comm'
             dico['ul']   = '1'
             dico['path'] = os.path.join(os.getcwd(), 'fort.%d' % self.UL)

           # Tous les autres fichiers en Resultat
           elif lab == 'resu':
              if self.UNITE_GRAPHIQUE and dico['ul'] == str(self.UNITE_GRAPHIQUE): l_fr.remove(dico)
              else:
                 dico['path'] = os.path.join(tmp_macr_recal, os.path.basename(dico['path']))

           # Tous les autres fichiers en Donnees
           elif lab == 'data':
              if dico['type'] not in ('exec', 'ele'):
                 if dico['ul']   != '0':   # Traite le cas des sources python sourchargees
                    dico['path'] = os.path.join(os.getcwd(), 'fort.%s' % dico['ul'])

           # sinon on garde la ligne telle quelle
       setattr(prof, lab, l_fr)

     # Ecriture du nouveau fichier export
     prof.WriteExportTo(self.new_export)

     if debug: os.system('cp ' + self.new_export + ' /tmp')

  # --FIN CLASSE  ----------------------------------------------------------------------------



