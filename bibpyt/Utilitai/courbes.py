#@ MODIF courbes Utilitai  DATE 16/06/2004   AUTEUR DURAND C.DURAND 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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

#==================================================
# fonction "COURBES"
# usage : permet de tracer des courbes en interactif
# avec XMGRACE ou dans un fichier postscript
#==================================================

import Stanley
from Stanley import xmgrace
from Stanley import as_courbes

def COURBES(listcourb,titre=' ',soustitre=' ',legx=' ',legy=' ',bornex=None,borney=None,fichier=None):

# ARGUMENTS 

# listcourb : tuple de courbes, chaque courbe etant definie soit par 
#             (TABLE1, NOM_PARA_X, TABLE2, NOM_PARA_Y, LEGENDE)
# soit par :
#             (FONCTION,LEGENDE)
# titre et sous_titre : facultatifs, titre et sous-tritre du graphique
# legx, legy          : facultatifs, legendes des axes
# bornex, borney      : facultatifs, bornes sur les axes
# fichier             : facultatif : sortie au format postscript si present
#
# exemples  d'appel :
#--------------------
# courb1=(SYYPRT,'ABSC_CURV',SYYPRT,'SIYY','PRT')
# courb2=(SYYMLC10,'ABSC_CURV',SYYMLC10,'SIYY','MLC10')
# courb3=(SYYML100,'ABSC_CURV',SYYML100,'SIYY','MLC100')
# listcourb=(courb1,courb2,courb3)
# COURBES(listcourb,titre='Plaque trouee',legx='Abcisses curvilignes',legy='Contraintes (MPa)',bornex=(0,100),borney=(500,1000))
# fonc1=(F1,'F_PRT')
# fonc2=(F2,'F_MLC10')
# fonc3=(F3,'F_MLC100')
# listfonc=(fonc1,fonc2,fonc3)
# COURBES(listfonc,titre='Fonctions')
# postscript
# COURBES(listfonc,titre='Plaque trouee',fichier='./fort.24')
#--------------------------------------------------------------

# initialisation du trace de  courbes

  if (fichier!=None):
     graphe=xmgrace.Xmgr(10,' -hardcopy -nosafe')
     print "Nombre de courbes  ",len(listcourb)," sur le fichier :",fichier

  else:
     graphe=xmgrace.Xmgr(10,' -noask')
     print "Nombre de courbes  ",len(listcourb)

  graphe.Nouveau_graphe()

# dimensionnement des axes 
  if bornex != None : 
     xmin=list(bornex)[0]
     xmax=list(bornex)[1]
     ctest1 = as_courbes.Courbe()
     ctest1.x=[xmin,xmax]
     ctest1.y=[0.0,0.0]
     graphe.Courbe(ctest1)

  if borney != None : 
     ymin=list(borney)[0]
     ymax=list(borney)[1]
     ctest2 = as_courbes.Courbe()
     ctest2.x=[0.0,0.0]
     ctest2.y=[ymin,ymax]
     graphe.Courbe(ctest2)

  if titre != None :
     if soustitre != None :
        graphe.Titre(titre,soustitre)
     else :
        graphe.Titre(titre,' ')
     
  if legx != None :
     graphe.Axe_x(legx)
     
  if legy != None :
     graphe.Axe_y(legy)

  k = 0
       
  for courbi in listcourb:
     sigi = as_courbes.Courbe()
     
     try :
        # cas d une table
        sigi.Lire_x(courbi[0],courbi[1])
        sigi.Lire_y(courbi[2],courbi[3])
        legende=courbi[4]
     except :
        # cas d une fonction
        val=courbi[0].LIST_VALEURS()
        nb=len(val)/2
        valx=[None]*nb
        valy=[None]*nb
        for i in range(nb):
           valx[i]=val[2*i]
           valy[i]=val[2*i+1]           
        sigi.x=valx
        sigi.y=valy
        legende=courbi[1]
        
     graphe.Courbe(sigi,legende)
     graphe.Send('WITH G'+repr(graphe.gr_act))
     graphe.Send('S' + str(k) + ' SYMBOL ' + str(k+2))
     graphe.Send('S' + str(k) + ' SYMBOL SIZE 0.5')
     graphe.Send('S' + str(k) + ' SYMBOL COLOR '+str(k+2))
     graphe.Send('S' + str(k) + ' LINE COLOR '+str(k+2))
     k = k + 1
     graphe.Send('REDRAW')
  
  if (fichier!=None):
     graphe.Sortie_EPS(fichier)
     graphe.Fermer()
  else:
     graphe.Attendre()

  k=0

#===========================================
