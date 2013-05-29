# coding=utf-8
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
# ======================================================================
from Cata.cata import *

import cPickle
import string

import numpy as NP
from numpy import linalg as LA


class TANGENT :
    """Vérification sur les matrices tangentes

    Attributs publics :
        mat       : matrice tangente
        ddl       : nom des degres de liberte
        nddl      : nombre de ddl
        norme     : norme de la matrice tangente
        prec_zero : en-dessous de prec_zero, on ne compare pas les matrices
    """
    def __init__(self,ddl='',prec_zero=1.E-12) :
        """
        ddl       : chaine de caracteres designant les ddl (ex: 'UUP')
        prec_zero : en-dessous de prec_zero, on ne compare pas les matrices
        """
        self.ddl = ddl
        self.prec_zero = prec_zero

    def Load(self,nom_fichier):
        """lit la matrice depuis un fichier"""
        fichier = file(nom_fichier,'r')
        self.__dict__ = cPickle.load(fichier)

    def Save(self,nom_fichier) :
        """sauvegarde la matrice dans un fichier"""
        fichier = file(nom_fichier,'w')
        cPickle.dump(self.__dict__,fichier)

    def Aster(self,suffixe = 'MATA') :
        """lit la matrice depuis l'espace Aster.
        nom : suffixe de l'objet jeveux
        """
        import aster
        nom_obj_jeveux = string.ljust('PYTHON.TANGENT.' + suffixe, 24)
        obj_jeveux = aster.getvectjev(nom_obj_jeveux)
        if not obj_jeveux :
            raise RuntimeError('TANGENT : OBJET JEVEUX DE SUFFIXE ' + \
                               suffixe + ' INEXISTANT')
        self.Matrice(obj_jeveux)

    def Eigen(self) :
        """Retourne les valeurs propres de la matrice"""
        self.vp = NP.sort(LA.eigvals(self.mat))

    def Matrice(self,matrice) :
        """range la matrice.
        matrice   : la matrice tangente (rangement carre)
        """
        if type(matrice) == type((1,)) :
            matrice = NP.array(list(matrice))
        elif type(matrice) == type([]) :
            matrice = NP.array(matrice)
        matrice = matrice.astype(float)
        nddl = int(len(matrice)**0.5+0.5)
        matrice.shape = (nddl,nddl)
        self.mat = matrice
        self.nddl = nddl
        if not self.ddl :
            self.ddl = 'D'*nddl
        elif len(self.ddl) <> nddl :
            raise RuntimeError('Nommage des DDL incoherents avec la taille de la matrice')
        self.norme = NP.trace(NP.dot(NP.transpose(self.mat),self.mat))

    def Difference(self,matp,affi_ok=0,prec_diff = 1.E-4) :
        """Comparaison relative de la matrice tangente avec une autre matrice
        matp      : matrice avec laquelle self.mat est comparee
        affi_ok   : si oui, on affiche egalement les valeurs qui collent bien
        prec_diff : ecart au-dessus duquel on considere que ce n'est pas OK
        """
        if type(matp) is tuple :
            matp = NP.array(list(matp))
        elif type(matp) is list :
            matp = NP.array(matp)
        elif type(matp) == type(self) :
            matp = matp.mat
        elif type(matp) is NP.ndarray:
            pass
        else:
            raise RuntimeError('1er argument doit etre une matrice (tuple,liste,TANGENT '
                               'ou tableau numpy)')
        matp = NP.ravel(matp)
        matp = matp.astype(float)
        if len(matp) <> self.nddl*self.nddl :
            raise RuntimeError('Matrices de tailles differentes')
        matp.shape = (self.nddl,self.nddl)
        refe = NP.abs(self.mat) + NP.abs(matp)
        diff = NP.where(refe > self.prec_zero,
                        NP.abs(self.mat-matp)/(refe+self.prec_zero), 0)
        nook = (diff.ravel()  > prec_diff).nonzero()[0]
        ok   = (diff.ravel() <= prec_diff).nonzero()[0]
        if affi_ok :
            affi = [ok,nook]
        else :
            affi = [nook]
        liste_i=[]
        liste_j=[]
        liste_matt=[]
        liste_matp=[]
        liste_diff=[]
        for ind in affi :
            for pos in ind :
                i = pos / self.nddl
                j = pos % self.nddl
                liste_i.append(i+1)
                liste_j.append(j+1)
                liste_matt.append(self.mat[i,j])
                liste_matp.append(matp[i,j])
                if NP.abs(self.mat[i,j]) + NP.abs(matp[i,j]) >= self.prec_zero :
                    liste_diff.append(NP.abs(self.mat[i,j]-matp[i,j]) / \
                        (NP.abs(self.mat[i,j]) + NP.abs(matp[i,j]) + self.prec_zero))
                else :
                    liste_diff.append(0.0)
        if self.norme > self.prec_zero :
            ecart = (self.mat - matp)/2.
            nor_ecart = NP.trace(NP.dot(NP.transpose(ecart),ecart))
            nor_diff= nor_ecart / self.norme
        else :
            nor_diff= 0.
        max_diff=0.
        if len(liste_diff) > 0:
            max_diff = NP.max(liste_diff)
        return liste_i, liste_j, liste_matt, liste_matp, liste_diff, nor_diff, max_diff

    def Symetrie(self, prec_diff = 1.E-4) :
        """Vérification que la matrice tangente est symétrique
        On retourne la norme relative de l'ecart a la symetrie : || (A-At)/2|| / ||A||
        On affiche les termes qui s'ecartent de la symetrie
        prec_diff : ecart au-dessus duquel on considere que ce n'est pas OK
        """
        tran = NP.transpose(self.mat)
        return self.Difference(tran, affi_ok=0, prec_diff=prec_diff)


def veri_matr_tang_ops(self, SYMETRIE, DIFFERENCE, PRECISION, PREC_ZERO, **args):
    """
    Ecriture de la macro verif_matrice_tangente_ops
    """
    from Accas import _F

    # On importe les definitions des commandes a utiliser dans la macro
    CREA_TABLE  = self.get_cmd('CREA_TABLE')

    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)
    # Le concept sortant (de type fonction) est nomme ROTGD dans
    # le contexte de la macro

    self.DeclareOut('TAB_MAT', self.sd)
    prec_zero = PREC_ZERO
    tgt = TANGENT(prec_zero=prec_zero)
    tgt.Aster(suffixe='MATA')
    matp = TANGENT(prec_zero=prec_zero)
    matp.Aster(suffixe='MATC')
    prec_diff = PRECISION
    if SYMETRIE == 'OUI':
        symetgt = tgt.Symetrie(prec_diff)[-2]
        symeper = matp.Symetrie(prec_diff)[-2]
        print 'Symetrie de la matrice tangente', symetgt
        print 'Symetrie de la matrice pr pertubation', symeper
    if DIFFERENCE == 'OUI':
        liste_i, liste_j, liste_matt, liste_matp, liste_diff, nor_diff, max_diff = \
            tgt.Difference(matp, prec_diff)
        print 'difference entre matrice tangente et matrice par pertubation : norme=', \
            nor_diff, ' max=', max_diff
        TAB_MAT=CREA_TABLE(LISTE=(_F(PARA     ='I',LISTE_I = liste_i),
                                  _F(PARA     ='J',LISTE_I = liste_j),
                                  _F(PARA     ='MAT_TGTE',LISTE_R = liste_matt),
                                  _F(PARA     ='MAT_PERT',LISTE_R = liste_matp),
                                  _F(PARA     ='MAT_DIFF',LISTE_R = liste_diff),))
    return 0


VERI_MATR_TANG=MACRO(nom="VERI_MATR_TANG", op=veri_matr_tang_ops, sd_prod=table_sdaster,
                     docu="",reentrant='n',
                     fr="verification de la matrice tangente : symetrie et difference "
                        "par rapport a la matrice calculee par perturbation",
         regles=(AU_MOINS_UN('SYMETRIE','DIFFERENCE')),
         SYMETRIE   = SIMP(statut='f', typ='TXM', defaut="NON", into=("OUI", "NON") ),
         DIFFERENCE = SIMP(statut='f', typ='TXM', defaut="OUI", into=("OUI", "NON") ),
         PRECISION  = SIMP(statut='f', typ='R', defaut=1.E-4 ),
         PREC_ZERO  = SIMP(statut='f', typ='R', defaut=1.E-12 ),
)
