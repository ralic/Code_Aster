#@ MODIF exemple_env Stanley  DATE 04/07/2003   AUTEUR JMBHH01 J.M.PROIX 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
#####################################################################################################
# Exemple de fichier de configuration personnelle pour utiliser Stanley en interactif
#
# Il faut recopier ce fichier dans un répertoire .stanley sur le HOME directory de votre
# machine d'execution, puis dé-commenter le bloc correspondant à votre utilisation :
#
# L'exécution est soit locale (sur une machine autonome), soit distant (3 possibilités)
#
# dans tous les cas, GRACE est lancé sur la machine d'exécution, avec un affichage sur votre 
# terminal, et GMSH est lancé sur la machine locale, ou une autre machine permettant d'afficher
# les graphiques en 3D
#####################################################################################################


## PARAMETRES COMMUNS
fonte = ('Fixed',17,'bold')                             # Fontes utilisée par Stanley (type,taille,style)
# fonte                 = ('Courier',9,'normal')
#fonte                 = ('Courier',17,'normal')
grace                 = 'xmgrace -noask'                # executable grace sur la machine ou tourne Aster
#version_fichier_gmsh  = 1.2                            # version de fichier de postraitement GMSH
# version =1 par défaut : les QUAD sont découpés en TRIA, les HEXA et PENTA en TETRA pour le post-traitement
# version 1.2 (a visualiser avec gmsh plus récente que 1.35) : pas de découpage des QUAD, HEXA et PENTA linéaires


#
# IL FAUT DECOMMENTARISER UN SEUL DES 4 BLOCS SI DESSOUS ET L'ADAPTER EN FONCTION DE VOTRE CONFIGURATION
#

### Configuration 1 : Visualisation Linux + Calcul local Linux :
#mode             = 'LOCAL'                              # LOCAL si on tourne sur une seule machine
#gmsh             = '/local/gmsh/bin/gmsh'               # executable gmsh sur la machine locale



### Configuration 2 : Visualisation Linux + Calcul distant :
#mode             = 'DISTANT'                            # DISTANT si on visualise sur une machine et on tourne sur une autre
#login_gmsh         = 'userid'
#machine_gmsh     = 'clz00zz.der.edf.fr'                 # machine Unix/Linux ou tourne gmsh (dans ce cas c'est la machine de l'utilisateur)
#gmsh             = 'setenv DISPLAY clz00zz.der.edf.fr:0.0 ; /local/Aster/outils/gmsh'       # executable gmsh sur la machine_gmsh
#rep_gmsh         = '/tmp'                               # repertoire temporaire pour gmsh (sur la machine_gmsh)



### Configuration 3 : Visualisation Windows/Exceed 3D (ou Terminal X) + Calcul distant :
#mode             = 'DISTANT'                                       # DISTANT si on est sous Windows / Exceed 3D
#machine_gmsh     = 'clz00zz.der.edf.fr'                            # machine Unix/Linux ou tourne gmsh (dans ce cas, cette machine n'est pas celle de l'utilisateur)
#gmsh             = 'export DISPLAY=clz00zz.der.edf.fr:0.0 ; /home/gmsh-1.35.1/gmsh'        # shell de lancement de gmsh (dans ce cas la machine de visualisation qui est celle du DISPLAY n'est pas la machine ou tourne gmsh)
#rep_gmsh         = '/tmp'                                          # repertoire temporaire pour gmsh (sur la machine_gmsh)



### Configuration 4 : Visualisation Windows/Exceed standard + Calcul distant :
#mode             = 'WINDOWS'                            # WINDOWS si on est sous Windows / Exceed
#machine_gmsh     = 'clz00zz.der.edf.fr'                 # machine windows ou tourne gmsh (graphique)
#rep_gmsh         = 'TEMP'                               # nom de partage du répertoire partagé Windows (le nom sous lequel il apparait dans "Voisinage Réseau" et non pas le chemin du repertoire)
#smbclient        = 'smbclient'                          # chemin de l'executable smbclient sur la machine ou tourne Aster (connection à un disque Windows depuis Unix)
#user_win         = ''                                   # log sur la machine Windows (mettre user_win = '' si le repertoire Windows est partagé en écriture pour tout le monde)
#user_pass        = ''                                   # pass sur la machine Windows (laisser vide si user_win = '' ou si vous voulez le rentrer manuellement dans la fenetre d'execution d'aster)
                                                        # Note : on peut proteger la lecture de ce fichier par d'autres utilisateurs avec l'instruction Unix : 'chmod 700 ~/.stanley'

