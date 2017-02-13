# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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

import math,copy
import numpy as N
from scipy import interpolate 
from Utilitai.Utmess import UTMESS
  
class filtre:

    """
        La classe filtre est la classe de base des filtres applicables au spectre
        Elle possède une fonction privée filtre qui prend un spectre en entrée et qui
        retourne un spectre filtré en sortie et qui est appelée par la fonction __call__
    """

    def __init__(self):
        pass

    def __call__(self, sp):
        return self._filtre(sp)

    def _filtre(self, sp):
        spr = sp
        return spr  # la fonction filtre de la classe de base retourne le spectre sans le modifier

class filtreLogLog(filtre):

    """
        Convertit un spectre en LogLog (log base 10)
    """

    def __init__(self, **listOpt):
        try:
            self.logAbc = listOpt['logAbc']
        except KeyError:
            self.logAbc = True
        try:
            self.logOrd = listOpt['logOrd']
        except KeyError:
            self.logOrd = True

    def _filtre(self, sp):
        spr = spectre()
        if self.logAbc:
            # spr.listFreq = [math.log10(i) for i in sp.listFreq]
            # conversion definie dans le excel du Septen
            spr.listFreq = [math.log10(i)+4.0 for i in sp.listFreq] 
        else:
            spr.listFreq = [i for i in sp.listFreq]
        if self.logOrd:
            # spr.dataVal = [math.log10(i) for i in sp.dataVal]
            # conversion definie dans le excel du Septen
            spr.dataVal = [math.log10(i)+4.0 for i in sp.dataVal]
        else:
            spr.dataVal = [i for i in sp.dataVal]

        return spr
       
class filtreLinLin(filtre):

    """
        Convertit un spectre en LinLin (10^n) à partir d'un spectre en linLog,LogLin ou logLog
    """

    def __init__(self, **listOpt):
        try:
            self.logAbc = listOpt['logAbc']
        except KeyError:
            self.logAbc = True
        try:
            self.logOrd = listOpt['logOrd']
        except KeyError:
            self.logOrd = True

    def _filtre(self, sp):
        spr = spectre()
        if self.logAbc:
            # spr.listFreq = [10 ** i for i in sp.listFreq]
            spr.listFreq = [10 ** (i-4.0) for i in sp.listFreq]
        else:
            spr.listFreq = [i for i in sp.listFreq]
        if self.logOrd:
            # spr.dataVal = [10 ** i for i in sp.dataVal]
            spr.dataVal = [10 ** (i-4.0) for i in sp.dataVal]
        else:
            spr.dataVal = [i for i in sp.dataVal]

        return spr
        
class filtreLowerPeaks(filtre):
    """
        enleve les pics inferieur dans le signal
    """
    def __init__(self):
        pass
        
    def _filtre(self, sp):
        l_freq = sp.listFreq
        l_val  = sp.dataVal
        for j in range(0,len(l_freq)-2) :
            # pente entre le point j et j+1
            tpa1 = (l_val[j+1] - l_val[j]) / (l_freq[j+1] - l_freq[j])
            # pente entre le point j et j+1
            tpa2 = (l_val[j+2] - l_val[j+1]) / (l_freq[j+2] - l_freq[j+1])
            # si on a un creux, on augmente la valeur au point j
            if (tpa2 * tpa1) <= 0 and tpa1 < 0 :
                tpa3 = (l_val[j+2] - l_val[j]) / (l_freq[j+2] - l_freq[j])
                tpb3 = l_val[j] - (tpa3 * l_freq[j])
                l_val[j+1] = tpa3 * l_freq[j+1] + tpb3
        return spectre(l_freq, l_val)
                   
class filtreBandWidth(filtre):
    """
        enleve les frequences 
    """
    
    def __init__(self, **listOpt):
        try:
            self.lowerBound = listOpt['lower']
        except KeyError:
            self.lowerBound = 0.2
        try:
            self.upperBound = listOpt['upper']
        except KeyError:
            self.upperBound = 35.5

    def _filtre(self, sp):
        spr = sp
        toDel = []
        for i in range(0, len(spr.listFreq)):
            if spr.listFreq[i] > self.upperBound:
                toDel = toDel + [i]

        # Nettoyage des fréquences à supprimer (on commence par les plus hautes)
        for i in toDel[::-1]:
            del spr.listFreq[i]
            del spr.dataVal[i]

        toDel = []
        for i in range(0, len(spr.listFreq)):
            if spr.listFreq[i] < self.lowerBound:
                toDel = toDel + [i]
            else:
                break

        # Nettoyage des fréquences à suppimer (on finit par les plus basses)
        for i in toDel[::-1]:
            del spr.listFreq[i]
            del spr.dataVal[i]

        return spr
   
class filtreExpand(filtre):

    """ effectue l'expansion du spectre """

    def __init__(self, **listOpt):
        try:
            self.expandCoef = listOpt['coef']
        except KeyError:
            self.expandCoef = 0.1

    def _filtre(self, sp):
        spLower = spectre()
        spUpper = spectre()
        # Etape 1 : Construction du spectre inférieur sans considération des échelons de fréquence
        for i in range(0, len(sp.listFreq)):
            spLower.listFreq = spLower.listFreq + \
                [sp.listFreq[i] - abs(sp.listFreq[i] * self.expandCoef)]
            spLower.dataVal = spLower.dataVal + [sp.dataVal[i]]
            spUpper.listFreq = spUpper.listFreq + \
                [sp.listFreq[i] + abs(sp.listFreq[i] * self.expandCoef)]
            spUpper.dataVal = spUpper.dataVal + [sp.dataVal[i]]

        # Etape 2 : Construction du spectre "élargi" sur la base de fréquence du spectre initial
        indmin = 0
        while spLower.listFreq[indmin] < sp.listFreq[0]:
            indmin += 1
            
        fmin  = spLower.listFreq[indmin-1]
            
        indmax = 0
        while spUpper.listFreq[indmax] < sp.listFreq[len(sp.listFreq) - 1]:
            indmax += 1
        fmax  = spUpper.listFreq[indmax]
                
        # Recopie des valeurs à conserver
        spLower.dataVal  = spLower.dataVal[indmin-1:]+[sp.dataVal[-1]]
        spLower.listFreq = spLower.listFreq[indmin-1:]+[fmax]
        
        # Recopie des valeurs à conserver
        spUpper.dataVal = [sp.dataVal[0]]+spUpper.dataVal[0:indmax+1]
        spUpper.listFreq = [fmin]+spUpper.listFreq[0:indmax+1]

        # Mise a jour du spectre initial pour les frequences extremites
        spMid = spectre()
        spMid.listFreq = [fmin]+sp.listFreq+[fmax]
        spMid.dataVal = [sp.dataVal[0]]+sp.dataVal+[sp.dataVal[-1]]
        
        # Enveloppe des spectres
        spr = enveloppe_spectres([spLower, spMid, spUpper])
        
        # Filtre sur les frequences initiales
        l_val=[]
        for f,freq in enumerate(spr.listFreq):
            if freq in sp.listFreq:
                l_val.append(spr.dataVal[f])
        sp.dataVal  = l_val
        return sp

        
class spectre:

    """
        décrit un spectre composé d'un ensemble de résultat associé à un ensemble de fréquence
    """

    def __init__(self, listFreq=[], dataVal=[]):
        self.listFreq = [v for v in listFreq]
        self.dataVal = [v for v in dataVal]
        self.l_area = []
        self.area   = 0
        
    def filtre(self, fi):
        """ Applique le filtre passé en paramètre au spectre et retourne un nouveau spectre"""
        self.l_area = []
        self.area   = 0
        return fi(self)
 
    def getArea(self):
        l_area = []
        area_total = 0
        for j in range(1,len(self.listFreq)) :
            tpa1 = (0.5 * (self.dataVal[j] + self.dataVal[j-1]) * (self.listFreq[j] - self.listFreq[j-1]))
            l_area.append(tpa1)
            area_total += abs(tpa1)
        self.l_area = l_area + [0.]
        self.area   = area_total-(self.listFreq[-1]-self.listFreq[0])*self.dataVal[0]
        return l_area, area_total
        
    def getExtremum(self):
        self.fmax = max( self.listFreq )
        self.samax = max( self.dataVal )
        
    def getdArea(self,j):
        # on suppose que la frequence peut etre supprimee
        elim = True
        
        # calcul extremum
        self.getExtremum() 
        
        # calcul les coeff directeurs de deux droites successive
        tpc1 = (self.dataVal[j]   - self.dataVal[j-1]) / (self.listFreq[j]   - self.listFreq[j-1])
        tpc2 = (self.dataVal[j+1] - self.dataVal[j])   / (self.listFreq[j+1] - self.listFreq[j])
        
        # CAS 1 : On supprime le point j 
        if tpc2 >= tpc1 :   
            # aire de la courbe approcime avec la suppresion du point j
            tpa2 = 0.5 * (self.dataVal[j-1] + self.dataVal[j+1]) * (self.listFreq[j+1] - self.listFreq[j-1]) 
            taba1 = abs(tpa2)
            # difference avec la courbe reelle et normalisation par l'aire totale
            # ( on donne ainsi de l'importance au amortissement + fort car aire totale plus faible )
            dArea= abs(taba1 - self.l_area[j] - self.l_area[j-1]) / self.area
            
        # CAS 2 : On conserve le point j et on modifie les valeurs de j-1 et j+1
        else:
            tpda3 = (self.dataVal[j+1] - self.dataVal[j-1]) / (self.listFreq[j+1] - self.listFreq[j-1])
            # liste temporaire avec les nouvelles valeurs
            l_tmp = N.zeros(5)
            if j != 1 : 
                l_tmp[0]=self.dataVal[j-2]
            l_tmp[1]=tpda3 * (self.listFreq[j-1] - self.listFreq[j]) + self.dataVal[j]
            l_tmp[2]=self.dataVal[j]
            l_tmp[3]=tpda3 * (self.listFreq[j+1] - self.listFreq[j]) + self.dataVal[j]
            if self.dataVal[j+1] != self.dataVal[-1] :  
                l_tmp[4]=self.dataVal[j+2] 
            # test si on depasse la valeur max en spectre
            if l_tmp[3] > self.samax or l_tmp[1] > self.samax:   
                elim = False
            # calcul la difference d'air entre la courbe approxime et la courbe reelle
            for o in range(0, 4) :
                if j == 1 and o == 0 :
                    l_tmp[o] = abs(0.0 - abs(0.5 * (l_tmp[o] + l_tmp[o+1]) * (self.listFreq[j-1+o] - 0.0))) ## =0 VLC ???
                elif (j-1+o) > (len(self.listFreq) - 1):
                    l_tmp[o] = abs(self.l_area[j-2+o] - abs(0.5 * (l_tmp[o] + l_tmp[o+1]) * (self.listFreq[j-2+o] - self.listFreq[j-2+o]))) # =0 VLC terme = 0
                else :
                    l_tmp[o] = abs(self.l_area[j-2+o] - abs(0.5 * (l_tmp[o] + l_tmp[o+1]) * (self.listFreq[j-1+o] - self.listFreq[j-2+o])))
            if j == 1 :
                dArea = (l_tmp[1] + l_tmp[2] + l_tmp[3]) / self.area
            elif self.listFreq[j+1] >= self.fmax :
                dArea = (l_tmp[0] + l_tmp[1] + l_tmp[2]) / self.area
            else :
                dArea = (l_tmp[0] + l_tmp[1] + l_tmp[2] + l_tmp[3]) / self.area
                    
        return elim, dArea            
    
    def removeFreq(self,j,elim):
        """Suppression de la frequence f et modification des valeurs"""
        tpc1 = (self.dataVal[j]   - self.dataVal[j-1]) / (self.listFreq[j]   - self.listFreq[j-1])
        tpc2 = (self.dataVal[j+1] - self.dataVal[j])   / (self.listFreq[j+1] - self.listFreq[j])
        tpda4 = tpc1 * tpc2
        # calcul le a de y = ax + b
        tpda3 = (self.dataVal[j+1] -self.dataVal[j-1]) / (self.listFreq[j+1] - self.listFreq[j-1])
        # valeur des y pour la courbe approximé
        tpa4_0 = tpda3 * (self.listFreq[j-1] - self.listFreq[j]) + self.dataVal[j]
        tpa4_1 = tpda3 * (self.listFreq[j+1] - self.listFreq[j]) + self.dataVal[j]
         
        # on verifie si on supprime un point ou pas
        if elim == True :
            if tpc1 > tpc2 : # CAS 1
                self.listFreq[j] = self.listFreq[j+1]
                self.dataVal[j-1] = tpa4_0
                self.dataVal[j] = tpa4_1
                self.l_area[j-1] = self.l_area[j] + self.l_area[j-1]
                self.l_area[j] = self.l_area[j+1]
            else : # CAS 2
                self.listFreq[j] = self.listFreq[j+1]
                self.dataVal[j]  = self.dataVal[j+1]
                self.l_area[j-1] = self.l_area[j] + self.l_area[j-1]
                self.l_area[j] = self.l_area[j+1] 
            # decale le reste des valeurs
            jmax = len(self.listFreq)-2
            if (j + 1) < jmax+1:
                for p in range((j + 1),jmax+1) :
                    self.dataVal[p]      = self.dataVal[p+1]
                    self.listFreq[p]     = self.listFreq[p+1]
                    self.l_area[p]       = self.l_area[p+1]
            del self.dataVal[-1]
            del self.listFreq[-1]
            del self.l_area[-1]  
        # dans le cas ou on ne supprime pas de valeur
        else :        
        # test si on depasse la valeur max en y et met les valeur au meme niveau
        # que la valeur directement superieur, aucune elimination de poinds ne sera alors faite.
            if tpa4_1 > self.samax :
                self.dataVal[j+1] = self.samax
                # dans le cas d'un palier met tout les valeurs au meme niveau (sinon pb de convergenge)
                tpe = True
                if ((tpa4_0 / self.samax) > 0.99999) and (tpc1 * tpc2 == 0.) :
                    self.dataVal[j-1] = self.samax
                    tpe = False
                if abs(tpc2 / tpc1) > 0.9999999 :
                    tpg = (self.samax - self.dataVal[j]) / (self.listFreq[j+1] - self.listFreq[j])
                    self.dataVal[j-1] = tpg * (self.listFreq[j-1] - self.listFreq[j]) + self.dataVal[j]
                    tpe = False
                if tpe == True :
                    self.dataVal[j-1] = tpa4_0

            if tpa4_0 > self.samax :
                self.dataVal[j-1] = self.samax
                tpe = True 
                if ((tpa4_1 / self.samax) > 0.99999) and (tpc1 * tpc2 == 0.) :
                    self.dataVal[j+1] = self.samax
                    tpe = False
                if (abs(tpc1 / tpc2) > 0.9999999) :
                    tpg = (self.dataVal[j] - self.samax) / (self.listFreq[j] - self.listFreq[j-1])
                    self.dataVal[j+1] = tpg * (self.listFreq[j+1] - self.listFreq[j]) + self.dataVal[j]
                    tpe = False
                if tpe == True :
                    self.dataVal[j+1] = tpa4_1                   
        
        
class nappe:

    """
        décrit un objet nappe qui associe à un ensemble de fréquence à une enesmble de résultats
    """

    def __init__(self, listFreq=[], listeTable=[], listAmor=[], entete=""):
        self.listFreq = [v for v in listFreq]  # meme frequence pour tous
        self.listSpec = []
        for spec in listeTable:
            self.listSpec.append(spectre(listFreq, spec))
            
        self.listAmor = [l for l in listAmor]
        self.entete = entete
        
    def getSpectre(self, index):
        """Retourne le spectre d'indice 'index' dans la nappe"""
        return self.listTable[index]
    
    def getNbSpectres(self):
        """ Retourne le nombre d'éléments dans la nappe """
        return len(self.listAmor)
    
    def getNbFreq(self):
        """ Retourne le nombre d'éléments dans la nappe """
        return len(self.listFreq)
      
    def updateSpectre(self,index,sp):
        """Met à jour le spectre d'indice 'index' dans la nappe (liste des freq pas modifiees )!"""
        self.listTable[index] =sp.dataVal
      
    def updateFreq(self,listFreq):
        """Met à jour la liste des frequences"""
        self.listFreq =listFreq  

    def filtre(self, filter):
        """Applique un filtre à l'ensemble des spectres de la nappe"""
        for j in range(0, self.getNbSpectres()):
            sp = self.listSpec[j]
            sp = sp.filtre(filter) 
            self.listSpec[j]=sp
        self.listFreq = sp.listFreq
               
    def getArea(self):
        """Calcul de l'aire sous chaque spectre"""
        for j in range(0, self.getNbSpectres()):
            sp = self.listSpec[j]
            l_area, area_total = sp.getArea()
      
    def getdArea(self):
        """Calcul de difference entre la courbe approxime et la courbe reelle pour chaque frequence
              somme pour tous les amortissements"""
        
        l_freq_sdarea = N.zeros(len(self.listFreq))
        l_freq_elim   = N.ones(len(self.listFreq))
        for f in range(1,len(self.listFreq)-1):
            for j in range(0, self.getNbSpectres()):    
                sp = self.listSpec[j]
                elim, dArea = sp.getdArea(f)
                l_freq_sdarea[f]+= dArea
                l_freq_elim[f]   = l_freq_elim[f]*elim
            
        return l_freq_elim, l_freq_sdarea
        
    def verifZpa(self,j):
        """Verification que le ZPA n'est pas modifie par la suppression de la frequence f"""
        for s in range(0, self.getNbSpectres()):    
            sp = self.listSpec[s]
            tpc1 = (sp.dataVal[j]   - sp.dataVal[j-1]) / (sp.listFreq[j] - sp.listFreq[j-1])
            tpc2 = (sp.dataVal[j+1] - sp.dataVal[j]) / (sp.listFreq[j+1] - sp.listFreq[j])
            if (tpc1 > tpc2):
                return False
            if (tpc1 * tpc2) == 0. :    
                return False
        return True        
                
    def removeFreq(self,j,elim, check=2, zpa=None):
        """Suppression de la frequence f et modification des valeurs"""
        for s in range(0, self.getNbSpectres()):    
            sp = self.listSpec[s] 
            sp.removeFreq(j,elim) 
          
        if check ==2:    
            self.check_nappe(check, zpa)
        
        # Mise a jour de la liste des frequences
        self.listFreq = sp.listFreq
        
    def check_nappe(self,check=1,zpa=None):
        # tous les amor on la meme ZPA:
        l_zpa = []
        for s in range(0, self.getNbSpectres()):    
            sp = self.listSpec[s]
            l_zpa.append(sp.dataVal[-1])
        if min(l_zpa)!=max(l_zpa):  
            #'Correction ZPA necessaire'
            if check!=0:
                for s in range(0, self.getNbSpectres()): 
                    if zpa!=None:
                        self.listSpec[s].dataVal[-1]=zpa
                    else:
                        self.listSpec[s].dataVal[-1]=max(l_zpa)
            
        # on verifie les croisements 
        l_amor = N.array(self.listAmor)
        ind_amor = l_amor.argsort()
        ind_amor = ind_amor.tolist()
        nb_amor = len(l_amor)
       
        dico_res = {}
        for f,freq in enumerate(self.listFreq):
            l_val = []
            # on parcourt les spectres dans l'ordre décroissant d'amortissement
            for s,s_id in enumerate(ind_amor[:-1]):    
                sp = self.listSpec[s_id].dataVal[f]
                l_val =[]
                for s_inf in range(s+1,nb_amor):
                    l_val.append(self.listSpec[ind_amor[s_inf]].dataVal[f])
                if sp < max(l_val):
                    # on corrige en prenant la valeur max de la nappe ou la valeur zpa 
                    if check!=0:    
                        self.listSpec[s_id].dataVal[f] = max(l_val)
        return l_zpa

def verif_freq(freq, l_freq, precision, critere):
    if len(l_freq)==0:
        return True
    else:   
        ind = 0
        while l_freq[ind]<=freq:
            if critere=='RELATIF':
                if abs(l_freq[ind]-freq)/abs(freq)<precision:
                    return False
            if critere=='ABSOLU':
                if abs(l_freq[ind]-freq)<precision:
                    return False
            ind +=1
            if ind == len(l_freq):
                return True
        return True 
         
def lissage_spectres(nappe=nappe, fmin=0.2, fmax=35.5, nb_pts=50,l_freq=[], precision=1e-3, critereVF='ABSOLU',check = 2, zpa = None):

    # verification et correction 
    # check = 0 verif mais pas de correction
    # check = 1 verif et correction à la fin
    # check = 2 verif et correction au cours de l'algo 
    if len(l_freq)>nb_pts: 
        UTMESS('A', 'FONCT0_72', valk=(str(nb_pts), str(len(l_freq))))
        nb_pts = len(l_freq) 
    l_freq.sort()
    for freq in l_freq:
        if verif_freq(freq, nappe.listFreq, precision, critereVF):
            UTMESS('A', 'FONCT0_77', valk=(str(freq)))
    l_freq_log = [math.log10(i)+4.0 for i in l_freq]
    
    # garder une copie de la nappe
    nappe_up = copy.deepcopy(nappe)
    # nappe_up = copy.copy(nappe)
    
    # Limitation de la bande de fréquence
    if fmin!=None or fmax!=None:
        if fmin==None: fmin = min(nappe_up.listFreq)
        if fmax==None: fmax = max(nappe_up.listFreq)
        filter = filtreBandWidth(lower=fmin, upper=fmax)
        nappe_up.filtre(filter)
        
    # Suppression des pics inferieurs
    # filter = filtreLowerPeaks()
    # nappe_up.filtre(filter)
    
    # Mettre les valeurs en echelle log-log
    filter = filtreLogLog()
    nappe_up.filtre(filter)
    if zpa!=None:
        zpa_log = math.log10(zpa)+4.0
    else:
        zpa_log = None
    # Initialisation
    iter = 0
    iter_max = 5*len(nappe_up.listFreq) 
    critere = True
    nappe_up.getArea()
    if len(nappe_up.listFreq)<=nb_pts:
        critere = False
   
    # Boucle jusqu'a atteinte du critere
    while critere and iter<iter_max:
        # Incrementation de l'iteration
        iter+=1
        # Tableau : frequence / valeur somme augmentation aire pour tous les amortissements
        l_freq_elim, l_freq_sdarea = nappe_up.getdArea()
        # Determination de la frequence a supprimer
        j_ord = N.argsort(l_freq_sdarea)
        jmax  = max(j_ord)
        stop = 0
        for j in j_ord:
            # on verifie - que le Zpa n'est pas modifie
            #            - que la frequence ne fait pas partie des frequences exclues
            if (j!=0 and j!=jmax and not stop and verif_freq(nappe_up.listFreq[j], l_freq_log, precision, critereVF)): 
                if j+1!=jmax or nappe_up.verifZpa(j):
                    j_supp = j
                    stop = 1
                    # break
        # Suppression de la frequence j_supp
        nappe_up.removeFreq(j_supp,l_freq_elim[j_supp],check,zpa_log)
        
        # Controle nombre de frequence
        if nappe_up.getNbFreq()<=nb_pts:
            critere = False
            
    # Non-convergence de l'algorithme 
    if iter==iter_max:    
        UTMESS('A', 'FONCT0_73',valk=(str(iter_max),str(nappe_up.getNbFreq()),str(nb_pts)))
        
    # Mettre les valeurs en echelle log-log
    filter = filtreLinLin()
    nappe_up.filtre(filter)
    
    # verification et correction
    if check!=2:
        nappe_up.check_nappe(check)
    
    return nappe_up    
    
 
def enveloppe_nappe(l_nappe):
    # verification que la liste d'amortissement est identique
    l_amor_unique = []
    for nappe in l_nappe:
        l_amor_unique+=nappe.listAmor
    #Suppression doublons
    l_amor_unique = list(set(l_amor_unique))
    l_amor_unique.sort()
    l_amor_unique = N.array(l_amor_unique)
    
    #Filtres
    filterLogLog = filtreLogLog()
    filterLinLin = filtreLinLin()
    
    l_spec_amor=[]
    for amor in l_amor_unique:
        l_spec = []
        l_freq = []
        for nappe in l_nappe:
            try:
                ind = (nappe.listAmor).index(amor)
                spec = copy.copy(nappe.listSpec[ind])
                spec.filtre(filterLogLog)
                l_spec.append(spec)
                l_freq+=spec.listFreq
            except:
                pass
        #Suppression doublons
        l_freq = list(set(l_freq))
        l_freq.sort()
        l_freq = N.array(l_freq) 
        s_max = N.zeros(len(l_freq))
        for spec in l_spec:
            fd = interpolate.interp1d(spec.listFreq, spec.dataVal)
            ynew = fd(l_freq)
            s_max = [ max(s_max[t], ynew[t]) for t in range(len(l_freq))]
        spec= spectre(listFreq=l_freq,dataVal=s_max)
        spec.filtre(filterLinLin)
        l_spec_amor.append(spec)
        
    sp_nappe = copy.copy(nappe)
    sp_nappe.listAmor = l_amor_unique
    sp_nappe.listFreq = l_freq   
    sp_nappe.listSpec = l_spec_amor   
       
    return sp_nappe
      
def enveloppe_spectres(listSpec):
    l_freq = []
    l_spec = []
    #Filtres
    filterLogLog = filtreLogLog()
    filterLinLin = filtreLinLin()
    for spec in listSpec:
        l_freq+=spec.listFreq
        specLL = copy.copy(spec)
        specLL.filtre(filterLogLog)
        l_spec.append(specLL)
    #Suppression doublons
    l_freq = list(set(l_freq))
    l_freq.sort()
    l_freq = N.array(l_freq) 
    s_max = N.zeros(len(l_freq))
    for spec in l_spec:
        fd = interpolate.interp1d(spec.listFreq, spec.dataVal)
        ynew = fd(l_freq)
        s_max = [ max(s_max[t], ynew[t]) for t in range(len(l_freq))]
    spec= spectre(listFreq=l_freq,dataVal=s_max)
    spec.filtre(filterLinLin)
    return spec
        
def elargis_spectres(l_spectre,l_coef):
    if (len(l_coef)!=len(l_spectre)):
        UTMESS('F', 'FONCT0_76')
        
    filterLogLog = filtreLogLog()
    filterLinLin = filtreLinLin()
    l_spec_elagr=[]
    
    for c,coef in enumerate(l_coef):
        if coef!=0.:
            specLL = copy.copy(l_spectre[c])
            specLL.filtre(filterLogLog)
            filtreElarg = filtreExpand(coef=coef)
            specLL.filtre(filtreElarg)
            specLL.filtre(filterLinLin)   
            l_spec_elagr.append(specLL)
        else:
            l_spec_elagr.append(l_spectre[c])
    
    return l_spec_elagr
    
def liss_enveloppe(l_nappes ,option = 'CONCEPTION', nb_pts = 50, coef_elarg = None, fmin=0.2, fmax=35.5,l_freq=[], precision=1e-3, critere='RELATIF', zpa = None ):
    
    if option == 'CONCEPTION':
        if len(l_nappes)>1:
            env_nappe = enveloppe_nappe(l_nappes)
        else:
            env_nappe = l_nappes[0]
        if len(nb_pts)>1:
            UTMESS('A', 'FONCT0_75')
        liss_nappe = lissage_spectres(nappe=env_nappe, fmin=fmin, fmax=fmax, nb_pts=nb_pts[0],l_freq=l_freq,check=2, zpa= zpa)   
            
        return liss_nappe
        
    elif option == 'VERIFICATION':
        if len(nb_pts)>1:
            nb_pts_1 = nb_pts[0]
            nb_pts_2 = nb_pts[1]
            if len(nb_pts)>2:
                UTMESS('A', 'FONCT0_75')
        else:    
            nb_pts_1 = nb_pts_2 = nb_pts
            
        # Lissage pour chaque nappe    
        l_liss_nappe = []
        for nappe in l_nappes:
            liss_nappe = lissage_spectres(nappe=nappe, fmin=fmin, fmax=fmax, nb_pts=nb_pts_1,l_freq=l_freq,check=2, zpa= zpa) 
            l_liss_nappe.append(liss_nappe)
        # Elargissement
        if coef_elarg!=None:
            l_liss_nappe = elargis_spectres(l_liss_nappe,coef_elarg)
        # Enveloppe
        env_nappe = enveloppe_nappe(l_liss_nappe)
        # Lissage 
        liss_nappe = lissage_spectres(nappe=env_nappe, fmin=fmin, fmax=fmax, nb_pts=nb_pts_2,l_freq=l_freq,check=2, zpa = zpa)        
        return liss_nappe
    else:
        print "L'option %s n'est pas traitée"%option
        
      
