#@ MODIF reca_evol Macro  DATE 11/01/2010   AUTEUR NISTOR I.NISTOR 

#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
#

'''
Le programme d'optimisation d'une fonctionelle base sur l'algorithm genetique,
developpement issu du contrat PdM-AMA
'''

import Numeric
import math
import random



def evolutivo(fonc,val,nb_iter,err_min,nb_parents,nb_fils,sigma,borne_inf,borne_sup,graine):

#initialisation du vecteur des parametres
    par_ini=[]
#les valeurs initiales des parametres sont recuperees 
    for ind in val:
        if ind:
            par_ini.append(ind)

#valeur du critere d arret
    val_crit=nb_iter

#initialisation et remplisage du vecteur des parents
    Parents_ini=[]
    for ind in range(nb_parents):
        Parents_ini.append(par_ini)
    #P sera le vecteur des parents retourne par la fonction figli
    P=[]
    erreurs=[]
    erreurs_ini=[]
    #le premier vecteur d erreurs sera calcule par la fonction errore
    #a partir des valeurs initiales des parents
    
    # On affiche egalement la fenetre MAC pour un appariement manual
    fonc.graph_mac=True
    err_ini=fonc.calcul_F(par_ini)
    #on rempli l'erreur pour chaque parent initial
    for ind in range(nb_parents):
        erreurs_ini.append(err_ini)
    P.append(Parents_ini)
    erreurs.append(erreurs_ini[:])
    in_ciclo=True
    iter=1
    #ici on demarre la boucle de minimisation de la fonction erreur
    while in_ciclo:
        if graine != None: random.seed(graine)
        F=fils(P[-1],nb_parents,nb_fils,sigma,borne_inf, borne_sup)

        #on fait la selection des meilleurs fils - p
        (p,err)=selection(fonc,F,P[-1],erreurs[-1],nb_parents)

        #P est le nouveau jeu de parents
        #attention on stocke ici tous l historique des parents et c'est le meme pour les erreurs dans erreurs
        P.append(p)

        erreurs.append(err)
        #on lance un calcul avec le meilleur jeu de parametres juste pour l'appariement des MAC
        fonc.graph_mac=True
        err_mac=fonc.calcul_F(P[-1][0])
        if erreurs[-1][0]<=err_min:
            in_ciclo=False
        iter+=1
        if iter>val_crit:
            in_ciclo=False

    return P[-1][0]



def selection(fonc,fils,parents,err_parents,nb_parents):
  
    """
    Selection des meilleurs fils a chaque iteration
    """           
       
    famille=[]
    err=[]
    for ind in fils:        
        fonc.graph_mac=False
        err.append(fonc.calcul_F(ind))      
    for ind in err_parents:
        err.append(ind)
    for ind in fils:
        famille.append(ind)
    for ind in parents:
        famille.append(ind)

    ordre=Numeric.argsort(err).tolist()
    fam_ordonne=[]
    err_ordonne=[]
    for ind in ordre:
        fam_ordonne.append(famille[ind])
        err_ordonne.append(err[ind])
              
    return fam_ordonne[0:int(nb_parents)], err_ordonne[0:int(nb_parents)]

 


def fils(parents,nb_parents,nb_fils,sigma,borne_inf, borne_sup):
    """
    Creation des fils
    """

    F=[]
    for ind in range(int(math.floor(nb_fils/nb_parents))):
        for ind2 in range(nb_parents):
            F.append(genere_fils(parents[ind2],sigma,borne_inf, borne_sup))
#le dernier parent est le plus prolific car il va completer le nombres de fils
#mais il est aussi le meilleur parent car correspond a l'erreur minimale            
    for ind2 in range(nb_fils%nb_parents):
        F.append(genere_fils(parents[ind2],sigma,borne_inf, borne_sup))
        
    return F

#les fils sont generes ici            
def genere_fils(parent,sigma,borne_inf, borne_sup):  

    """
    Creation d'un seul fils avec prise en compte des bornes
    """
    errate=True
    while errate:
        errate=False
#F est le vecteur de fils a remplir ici avec la fonction random 
#a partir des valeurs du parent courant
        F=[]
        for ind in parent:
            F.append(ind+ind/100.*random.gauss(0,sigma))
#la variable parametre initialise ici est un index pour defiler les valeurs de F
        parametre=0
        for ind in parent:
            test1=F[parametre]>=borne_inf[parametre]
            test2=F[parametre]<=borne_sup[parametre]
            if test1 & test2:
                pass
            else:
                errate=True
#                print "parametre hors bornes"
            parametre+=1            
#        print 'fils genere:',F            
    return F

