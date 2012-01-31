#@ MODIF post_endo_fiss_utils Macro  DATE 30/01/2012   AUTEUR BOTTONI M.BOTTONI 

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
# RESPONSABLE BOTTONI M.BOTTONI
#
# ---------------------------------------------------------------------------
#   UTILITAIRES MACRO POST_ENDO_FISS


import numpy as NP
import os
import math

# RETURN A UNIQUE VECTOR
# Fonction qui elimine les doublons dans un vecteur Vect
#   Vect doit etre un vecteur unidimensionnel
def unique(Vect):
    """
>>> import numpy as NP
>>> unique(NP.array([])).size
0

>>> unique(NP.array([1,2,1,1,2,1,3]))
array([1, 2, 3])

>>> unique(NP.array([3,1,2,1,2,1,3])) # insertion order must be preserved
array([3, 1, 2])
    """
    i = 0
    while i < len(Vect) :
        num = Vect[i]
        idxDou = NP.where(Vect==num)[0]
        if len(idxDou)>1 :
            idxDou = idxDou[1:len(idxDou)]
            Vect = NP.delete(Vect, idxDou)
        i = i+1
    return Vect

def unitVector(Vect) :
    """
>>> import numpy as NP
>>> unitVector(NP.array([])).size
0

>>> unitVector(NP.array([44]))
array([ 1.])

>>> unitVector(NP.array([1, 0, 0]))
array([ 1.,  0.,  0.])

>>> unitVector(NP.array([0., 0., 0.]))
array([ 0.,  0.,  0.])

>>> unitVector(NP.array([5, 0, 0]))
array([ 1.,  0.,  0.])
    """
    norm = NP.linalg.norm(Vect)
    if norm == 0:
        return Vect
    return Vect / norm

# CREATE A 1D-MESH :
#  Cree un maillage SEG2 en 2D
#    Coorx  : liste des coordonnees x des noeuds
#    Coory  : liste des coordonnees y des noeuds
#    Connex : connectivites entre les noeuds
#             (liste de tuples d'entiers)
##
def crea_mail_lin(XcreteTot,YcreteTot,ZcreteTot,ConnTot,lstNomFiss,dime):
    resu  = 'TITRE\n'
    titre = 'Maillage lineaire'+'\n'
    #resu  = resu + titre
    resu  = resu+'FINSF\n'

    # creation des noeuds
    resu  = resu+'COOR_'+str(dime)+'D\n'
    CoorX = XcreteTot[0]
    CoorY = YcreteTot[0]
    CoorZ = ZcreteTot[0]
    for i in range(1,len(XcreteTot)) :
        CoorX = CoorX + XcreteTot[i]
        CoorY = CoorY + YcreteTot[i]
        CoorZ = CoorZ + ZcreteTot[i]
    nbNoeu = len(CoorX)
    for i in range(nbNoeu):
        nbno = i+1
        x = CoorX[i]
        y = CoorY[i]
        z = CoorZ[i]
        if dime == 3 :
            noeud = '  N'+str(nbno)+'   '+str(x)+'    '+str(y)+'   '+str(z)+'\n'
        else :
            noeud = '  N'+str(nbno)+'   '+str(x)+'    '+str(y) +'\n'
        resu  = resu + noeud
    resu = resu+'FINSF\n'

    # creation des mailles
    resu = resu+'SEG2\n'
    nbmailTot = 0
    nbNoTot   = 0
    for j in range(len(ConnTot)) :
        Connex = ConnTot[j]
        nbmail = len(Connex)
        for i in range(nbmail) :
            nbma   = i+1+nbmailTot
            ma     = Connex[i]
            maille = '  M'+str(nbma)+' N'+str(ma[0]+nbNoTot)+' N'+str(ma[1]+nbNoTot)+'\n'
            resu   = resu+maille
        nbmailTot = nbmailTot + len(Connex)
        nbNoTot   = nbNoTot   + len(XcreteTot[j])
    resu = resu+'FINSF\n'

  # creation des groupes de mailles "fissure"
    nbmailTot = 0
    for j in range(len(ConnTot)):
        resu = resu+'GROUP_MA\n'
        #resu = resu+'FISSURE'+str(j+1)+'\n'
        resu = resu+lstNomFiss[j]+'\n'
        Connex = ConnTot[j]
        nbmail = len(Connex)
        for i in range(nbmail):
            nbma = i+1+nbmailTot
            resu = resu +'  M'+str(nbma)+'\n'
        resu = resu+'FINSF\n'
        nbmailTot = nbmailTot + len(Connex)

    resu = resu+'FIN\n'
    return resu



# CREATE AN MESH OBJECT FROM A STRING REPRESENTING
#    A MESH IN THE ASTER FORMAT
def crea_sd_mail(self,mailString):
    from Utilitai.UniteAster   import UniteAster
    LIRE_MAILLAGE = self.get_cmd('LIRE_MAILLAGE')

    nomFichierSortie = os.path.join(os.getcwd(),'maillage.mail')
    fproc            = open(nomFichierSortie, 'w')
    fproc.write(mailString)
    fproc.close()
    UL               = UniteAster()
    uniteMail        = UL.Libre(action = 'ASSOCIER', nom = nomFichierSortie)
    _MAI = LIRE_MAILLAGE(UNITE = uniteMail)
    UL.EtatInit(uniteMail)

    return _MAI


# SMOOTHES A DISCRETE FUNCTION ON A LINE
#   Coorx, Coory (NP arrays) : x-, y-coordinates
#   Fun0 (NP array)          : function values
#   FunReg (NP array)        : smoothed function values
def conv_smoothing1D(lreg, Coorx, Coory, Fun0):
    """
>>> import numpy as NP

>>> smoothed = conv_smoothing1D(0.33, NP.array([0, 1, 2]), NP.array([0, 1, 2]), NP.array([0, 1, 0]))
>>> smoothed[0] > 0 and smoothed[0] < 0.5
True

>>> smoothed[1] < 1 and smoothed[1] > 0.5
True

>>> smoothed[2] > 0 and smoothed[2] < 0.5
True

>>> smoothed[0] < smoothed[1] and smoothed[1] > smoothed[2]
True
    """
    Absc    = curvilinearAbsissa(Coorx, Coory, len(Coorx)/2)
    Gauss   = NP.exp(-Absc**2/(2.*lreg**2))
    area    = NP.convolve(Gauss, NP.ones(len(Gauss)),'valid')
    area    = float(area)
    FunReg  = NP.convolve(Fun0, Gauss, 'same')
    FunReg  = FunReg/area
    return FunReg

# SMOOTHES A DISCRETE FUNCTION ON A CIRCLE
#   Coorx, Coory (NP arrays) : x-, y-coordinates
#   Fun0 (NP array)          : function values
#   FunReg (NP array)        : smoothed function values
def conv_smoothing_arc(lreg,Coorx, Coory,Fun0) :
    DX = Coorx[1:len(Coorx)] - Coorx[0:len(Coorx)-1]
    DY = Coorx[1:len(Coory)] - Coory[0:len(Coory)-1]
    DS = NP.sqrt(DX**2 + DY**2)
    AbsC = NP.cumsum(DS)
    AbsC = NP.concatenate((NP.array([0.]),AbsC))
    FunReg  = NP.array([])
    Gauss   = NP.exp(-AbsC**2/(2.*lreg**2))
    area    = float(NP.convolve(Gauss, NP.ones(len(Gauss)),'valid'))
    for i in range(len(Fun0)) :
        Fun_I  = NP.concatenate((Fun0[i:], Fun0[0:i]), axis = 0)
        Fun_I  = NP.concatenate((Fun_I[len(Fun_I)/2+1:], Fun_I[0:len(Fun_I)/2+1]), axis = 0)
        zreg   = NP.convolve(Fun_I, Gauss, 'valid')
        FunReg = NP.append(FunReg,float(zreg))
    FunReg = FunReg / area
    return FunReg

def conv_smoothing_arc_old(lreg,CoorxOrth, CooryOrth,EndoOrth) :
    EndoReg = NP.zeros((len(EndoOrth),), float)
    X1 = NP.concatenate((CoorxOrth[1:len(CoorxOrth)], NP.array([CoorxOrth[0]])))
    Y1 = NP.concatenate((CooryOrth[1:len(CoorxOrth)], NP.array([CooryOrth[0]])))
    DX = X1-CoorxOrth
    DY = Y1-CooryOrth
    DS = NP.sqrt(DX**2+DY**2)
    for l in range(len(EndoOrth)):
      DSa   = DS[(l-1):len(DS)]
      DSb   = DS[0:(l-1)]
      DS1   = NP.concatenate((DSa,DSb))
      Dist  = NP.zeros((len(EndoOrth),), float)
      for k in range(len(EndoOrth)/2):
        Dist[k+1]  = Dist[k]  + DS1[k]
        Dist[-k-1] = Dist[-k] + DS1[-k-1]
      Gauss = NP.exp( -(2*Dist/(lreg/5))**2 )

      Gauss2 = NP.concatenate((Gauss[1:len(Gauss)], NP.array([Gauss[0]])))
      Den    = DS1 * ((Gauss + Gauss2)/2)

      EndoOrthShft = NP.concatenate((EndoOrth[l:len(EndoOrth)],EndoOrth[0:(l)]))
      Endogauss   = EndoOrthShft * Gauss
      Endogauss2  = NP.concatenate((Endogauss[1:len(Endogauss)], NP.array([Endogauss[0]])))
      Num         = DS1 * ((Endogauss + Endogauss2)/2)

      EndoReg[l]  = NP.sum(Num)/NP.sum(Den)
    return EndoReg

# COMPUTATION OF AVERAGE ORTHOGONAL VECTORS TO THE DISCRETE CRACK
# Coorx, Coory (NP arrays) : x-, y-coordinates
#  Input :  lst_abs (NP array) : x-coordinates of the crack
#           lst_ord (NP array) : y-coordinates of the crack
#           hnor    (NP array) : vector normal to tha crack plan
#  Ouput :  lst_normPoi (NP array) : list of vectors orthogonal to the crack path
#           lst_tanPoi (NP array)  : list of vectors tangent to the crack path
def versDirMoy(Coorx,Coory,Coorz,hnor) :
    htest1 = NP.array( hnor-NP.array([0.,0.,1.]) , int)
    htest6 = NP.array( hnor-NP.array([0.,1.,0.]) , int)

    if htest1.any() == False :
        lst_abs = Coorx
        lst_ord = Coory
    elif htest6.any() == False :
        lst_abs = Coorx
        lst_ord = Coorz
    else :
        lst_abs = Coory
        lst_ord = Coorz

    idxAbsOrd = NP.where(hnor==0.)[0]
    idxNor    = NP.where(hnor==1.)[0]

    abs_ini = NP.array(lst_abs[0:len(lst_abs)-1])
    abs_fin = NP.array(lst_abs[1:len(lst_abs)])
    ord_ini = NP.array(lst_ord[0:len(lst_ord)-1])
    ord_fin = NP.array(lst_ord[1:len(lst_ord)])
    Dabs    = abs_fin - abs_ini
    Dord    = ord_fin - ord_ini

    lst_normPoi = []
    lst_tanPoi  = []

    nbPoints = len(abs_ini)+1
    vecTan0 = NP.zeros(3,float)
    vecTan0[idxAbsOrd[0]] = Dabs[0]
    vecTan0[idxAbsOrd[1]] = Dord[0]
    vecTan0[idxNor]       = 0.
    vecTan0               = unitVector(vecTan0)
    for idxPoi in range(nbPoints) :
        idxSeg = min(idxPoi,nbPoints-2)
        vecTan1 = NP.zeros(3,float)
        vecTan1[idxAbsOrd[0]] = Dabs[idxSeg]
        vecTan1[idxAbsOrd[1]] = Dord[idxSeg]
        vecTan1[idxNor]       = 0.
        vecTan1 = unitVector(vecTan1)
        vecTan  = unitVector((vecTan0 + vecTan1)*0.5)
        lst_tanPoi.append(vecTan)
        lst_normPoi.append( unitVector(NP.cross(hnor, vecTan)) )
        vecTan0 = vecTan1.copy()

    return lst_tanPoi, lst_normPoi

class ThresholdTooHighError(ValueError):
    def __init__(self, vector, threshold):
        self.vector = vector
        self.threshold = threshold

# FIND THE VALUE NEAREST TO A GIVEN POSITION IN AN ARRAY
def findExtr(Vector,value,idxCenter) :
    """
    Vector : NP array
    idxCenter : index for initial given position in the array
    value : value to be searched for at extremities
    idxG, idxD : indexes of left and rigth extremities

>>> findExtr([0, 0, 1, 0], 1, 2)
(2, 2)

>>> findExtr([0, 0, 0, 0], 1, 2)
Traceback (most recent call last):
...
NoMaximaError

>>> findExtr([0, 0.1, 0.2, 0.3, 0.2, 0.1, 0], 0.2, 1)
(2, 4)

>>> findExtr([0, 0.1, 0.2, 0.3, 0.2, 0.1, 0], -0.2, 1)
(0, 6)

>>> findExtr([0, 0.1, 0.2, 0.3, 0.2, 0.1, 0], 2, 1)
Traceback (most recent call last):
...
ThresholdTooHighError
    """
    idxMax, vecMax = nearestMax(Vector, idxCenter)
    
    if value > vecMax:
        raise ThresholdTooHighError(Vector, value)
    
    idxG = idxD = idxMax
    while idxG > 0:
        if Vector[idxG - 1] >= value:
            idxG -= 1
        else:
            break
    while idxD < (len(Vector) - 1):
        if Vector[idxD + 1] >= value:
            idxD += 1
        else:
            break
            
    return idxG, idxD

class NoMaximaError(ValueError):
    def __init__(self, vector):
        self.vector = vector

# FIND THE RELATIVE MAXIMUM NEAREST TO A GIVEN POSITION IN AN ARRAY
#    Vector : NP array
#    idxIni : index for initial given position in the array
#    idxMax, vecMax : index and value of nearest max
def nearestMax(Vector, idxIni, threshold = 0) :
    """
>>> import numpy as NP

>>> nearestMax(NP.array([0, 0, 0, 0]), 1)
Traceback (most recent call last):
...
NoMaximaError

>>> nearestMax(NP.array([0, 0, 0, 1, 0]), 1)
(3, 1)

>>> idxMax, vecMax = nearestMax(NP.array([0, 0.1, 0.2, 0.3, 0.2, 0.1, 0]), 2)
>>> print (idxMax, '%.1f' % vecMax)
(3, '0.3')
    """
    maxima, maxima_indexes = relativeMaxima(Vector, threshold, return_indexes=True)
    if len(maxima) == 0:
        raise NoMaximaError(Vector)
    nearest_maxima_index = findSimilarValue(maxima_indexes, idxIni)
    return nearest_maxima_index, Vector[nearest_maxima_index]

# FIND THE ARRAY INDEX CORRESPONDING TO A VALUE NEAREST TO THE SEARCHED ONE
#    vector : NP array
#    return nearest index
def findSimilarValue(vector,search_value, return_index = False):
    """
>>> import numpy as NP
>>> findSimilarValue(NP.array([7, 10, 13]), 8)
7

>>> findSimilarValue(NP.array([7, 10, 13]), 8, return_index=True)
(7, 0)

>>> findSimilarValue(NP.array([7, 10, 13]), 9)
10

>>> findSimilarValue(NP.array([7, 10, 13]), 999)
13

>>> findSimilarValue(NP.array([7, 10, 13]), -77)
7

>>> findSimilarValue(NP.array([3]), 2)
3
    """
    idx = (NP.abs(NP.array(vector) - search_value)).argmin()
    if return_index:
        return vector[idx], idx
    else:
        return vector[idx]

# FIND A LIST OF INDEXES, EACH CORRESPONDING TO A RELATIVE MAXIMA IN AN ARRAY
#    vector : NP array
#    return list of relative maxima indexes
def relativeMaxima(vector, threshold = 0, return_indexes = False):
    """
>>> import numpy as NP
>>> relativeMaxima([10.3,2,0.9,4,5,6,7,34,2,5,25,3,-26,-20,-29])
[34, 25, -20]

>>> relativeMaxima([10.3,2,0.9,4,5,6,7,34,2,5,25,3,-26,-20,-29], return_indexes=True)
([34, 25, -20], [7, 10, 13])

>>> relativeMaxima([10.3,2,0.9,4,5,6,7,34,2,5,25,3,-26,-20,-29], threshold=10)
[34, 25]

>>> relativeMaxima([10.3,2,0.9,4,5,6,7,34,2,5,25,3,-26,-20,-29], threshold=21)
[34]

>>> relativeMaxima([0, 10, 10, 10, 0])
[]

>>> relativeMaxima([-10, -1, -10, 2, -10])
[-1, 2]

>>> relativeMaxima([10, 1, 1, 1, 10])
[]

>>> relativeMaxima([0, 0.1, 0.2, 0.3, 0.2, 0.1, 0], return_indexes=True)[1]
[3]
    """
    gradients=NP.diff(vector)

    maxima_indexes=[]
    maxima=[]
    for index in xrange(len(gradients) - 1):
        current_gradient = gradients[index]
        next_gradient = gradients[index + 1]
        if (current_gradient > threshold and next_gradient + threshold < 0):
            maxima_indexes.append(index + 1)
            maxima.append(vector[index + 1])
    if return_indexes:
        return maxima, maxima_indexes
    else:
        return maxima

# GIVE THE CURVILINEAR ABSCISSA FROM COORDINATES AND POSITION OF ZERO
# Input points are on a straigth line
#   Coorx, Coory : x, y coordinates of the line (NP array)
#   idxZero : index of the point having abscissa 0
#   Absc : curvilinear abscissa (NP array)
def curvilinearAbsissa(Coorx, Coory, idxZero) :
    """
>>> import numpy as NP

>>> curvilinearAbsissa([], [], 0)
Traceback (most recent call last):
...
IndexError: list index out of range

>>> curvilinearAbsissa(NP.array([0, 0.5, 1]), NP.array([0, NP.sqrt(3)/2, NP.sqrt(3)]), 1) # 60 degrees
array([-1.,  0.,  1.])

>>> curvilinearAbsissa(NP.array([0, 0.5, 1]), NP.array([0, NP.sqrt(3)/2, NP.sqrt(3)]), 0) # 60 degrees
array([ 0.,  1.,  2.])

>>> curvilinearAbsissa(NP.array([0, 0.5, 1]), NP.array([0, NP.sqrt(3)/2, NP.sqrt(3)]), 2) # 60 degrees
array([-2., -1.,  0.])
    """
    Coorx2 = Coorx-Coorx[0]
    Coory2 = Coory-Coory[0]
    Absc0  = NP.sqrt( (Coorx-Coorx[0])**2 + (Coory-Coory[0])**2 )
    xz = Coorx[idxZero]
    yz = Coory[idxZero]
    distZ = NP.sqrt( (Coorx[0]-xz)**2 + (Coory[0]-yz)**2 )
    Absc  =  Absc0 - distZ
    return Absc


# FIND THE CRACK OPENING AND ERROR WITH THE STRONG METHOD
#   Coorx, Coory : x, y coordinates on the orthogonal line (NP array)
#   lreg : regularization length
#   Epsi : Strains on the line points (NP array)
#   CO  : crack opening array
#   errPerc : errors expressed as pourcentage (NP array)
def crackOpeningStrong(lreg, Coorx, Coory, Epsi) :
    import numpy as NP
    Absc    = curvilinearAbsissa(Coorx, Coory, len(Coorx)/2)
    Gauss   = NP.exp(-Absc**2/(2.*lreg**2))
    area    = NP.trapz(Gauss,x=Absc)
    EpsiReg = conv_smoothing1D(lreg, Coorx, Coory, Epsi)
    idxMax, epsMax = nearestMax(EpsiReg, len(Coorx)/2)
    gauss0   = 1.
    Itheo    = lreg * NP.sqrt(2.*NP.pi)
    CO       = epsMax * Itheo / gauss0
    xcentre  = Coorx[idxMax]
    ycentre  = Coory[idxMax]
    Absc     = curvilinearAbsissa(Coorx, Coory, idxMax)
    EpsSDreg =  CO/area * NP.exp(-Absc**2/(2.*lreg**2))
    errPerc  = 100.* ( NP.trapz(abs(EpsiReg-EpsSDreg), x=Absc)) / ( NP.trapz(abs(EpsiReg), x=Absc) )
    return CO, errPerc
    
    
# RETURN EULER ANGLES FROM ROTATION MATRIX
#    angles in degrees -- axes sequence : zyx
#    
def euler_angles(matrix):
    # epsilon for testing whether a number is close to zero
    _EPS  = NP.finfo(float).eps * 4.0
    M  = NP.array(matrix, dtype=NP.float64, copy=False)[:3, :3]
    cy = math.sqrt(M[0, 0]*M[0, 0] + M[1, 0]*M[1, 0])
    if cy > _EPS:
        alpha = math.atan2( M[1, 0],  M[0, 0])
        beta  = math.atan2(-M[2, 0],  cy)
        gamma = math.atan2( M[2, 1],  M[2, 2])
    else :
        alpha = 0.0
        beta  = math.atan2(-M[2, 0],  cy)
        gamma = math.atan2(-M[1, 2],  M[1, 1])
    alpha = math.degrees(alpha)
    beta  = math.degrees(beta)
    gamma = math.degrees(gamma)
    return alpha, beta, gamma

if __name__ == "__main__":
    import doctest
    doctest.testmod(verbose=True)
