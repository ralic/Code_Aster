# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
"""
    Maquette demande SEPTEN fonction de lissage enveloppe
    Les données se présentent sous la forme d'un fichier texte comportant
    un ensemble de groupe de lignes organisé comme suit :
        - ligne 1 : Informations générales
        - ligne 2 : une liste de valeur d'amortissement
        - lignes 3...n : une liste de valeur commencant par une frequence suivit 
                         des amplitudes du spectre pour chacun des amortissements
                         liste en ligne 2
    Points importants :
        - Le nombre de lignes définissant le spectre peut varier
        - Le nombre de valeur d'amortissement peut varier ?

    ==> On propose d'opérer ligne par ligne
    ==> L'absence d'informations sur la variabilité du nombre d'éléments oblige à traiter le cas général

        

    Etapes du développement :
        24/05/2005 : Test de lecture du fichier, choix d'une stratégie de gestion
        25/05/2005 : Objet itérable pour la  lecture du fichier
        29/05/2005 : Créations de filtres pour les spectres
"""

import math

def nearestKeys(k1, dct) :
    """
        retourne les clés (doublet) les plus proches de 'key' dans le dictionnaire dct
        par valeur inférieure et supérieures
    """
    kr = min(dct.keys())
    for k2 in dct.keys() :
        if (k2<k1) and (k2>kr) : kr = k2 
    kinf = kr

    kr = max(dct.keys())
    for k2 in dct.keys() :
        if (k2>k1) and (k2<kr) : kr = k2
    ksup = kr

    return (kinf, ksup)

def interpole(x2, x0, y0, x1, y1) :
    """
        renvoie la valeur pour x2 interpolée (linéairement) entre x0 et x1  
    """
    try : 
        a = (y1-y0) / (x1-x0)
    except ZeroDivisionError :
        return y0

    return a * (x2-x0) + y0


def listToDict(lst) :
    """
        Cette fonction recoit une liste et la transforme en un dictionnaire 
    """
    dctRes = {}
    for val in lst :
        dctRes[val] = True
    return dctRes

def inclus(l1, l2) :
    """
        Teste si une liste (lst1) est incluse dans une autre (lst2)
        Renvoie le premier élément de l1 qui n'est pas inclus ou None si l1 inclus dans l2)
    """
    for v in l1 :
        try :
            l2.index(v)
        except ValueError :
            return v
    return None

def exclus(i1, i2) :
    """
        Teste si deux listes ne partagent pas d'élément commun
        Renvoie le premier élément de l1 qui n'est pas exclus ou None si l1 exclus de l2)
    """
    for v in i1 :
        try :
            i2.index(v)
            return v
        except ValueError :
            continue
    return None
        
class NappeCreationError(Exception) :
    def __init__(self) :
        self.mess = "Un problème est survenu lors dla création d'une nappe"
        self.otherExcept = Exception()
        
    def getMess(self) :
        """ Retourne le message associé à l'erreur """
        # Analyse les différents cas d'erreurs
        if self.otherExcept == IOError :
            self.mess += "\nProblème à l'ouverture du fichier\n"

        return self.mess
    
class SpectreError(Exception) :
    def __init__(self) :
        self.mess = "Un problème est survenu lors de la construction du spectre"
        self.otherExcept = Exception()
        
    def getMess(self) :
        """ Retourne le message associé à l'erreur """
        # Analyse les différents cas d'erreurs
        if self.otherExcept == IOError :
            self.mess += "\nProblème à l'ouverture du fichier\n"

        return self.mess
    
class filtre :
    """
        La classe filtre est la classe de base des filtres applicables au spectre
        Elle possède une fonction privée filtre qui prend un spectre en entrée et qui
        retourne un spectre filtré en sortie et qui est appelée par la fonction __call__
    """
    def __init__(self): pass
    def __call__(self, sp) :
        return self._filtre(sp)

    def _filtre(self, sp) :
        spr = sp
        return spr # la fonction filtre de la classe de base retourne le spectre sans le modifier

class filtreExpand(filtre) :
    """ effectue l'expansion du spectre selon spécif du SEPTEN """
    def __init__(self, **listOpt) :
        try :
            self.expandCoef = listOpt['coef']
        except KeyError :
            self.expandCoef = 0.1
       
    def _filtre(self, sp) :
        spLower = spectre()
        spUpper = spectre()
        # Etape 1 : Construction du spectre inférieur sans considération des échelons de fréquence
        for i in range(0, len(sp.listFreq)) :
            spLower.listFreq = spLower.listFreq + [sp.listFreq[i] - abs(sp.listFreq[i]*self.expandCoef)]
            spLower.dataVal = spLower.dataVal + [sp.dataVal[i]]
            spUpper.listFreq = spUpper.listFreq + [sp.listFreq[i] + abs(sp.listFreq[i]*self.expandCoef)]
            spUpper.dataVal = spUpper.dataVal + [sp.dataVal[i]]


        # Etape 2 : Construction du spectre "élargi" sur la base de fréquence du spectre initial
        #           On tronque en deca de la fréquence minimale du spectre de référence
        index = 0
        while spLower.listFreq[index] < sp.listFreq[0] : index+=1
            
        # Recopie des valeurs à conserver
        spLower.dataVal = spLower.dataVal[index:]

        index = 0
        while spUpper.listFreq[index] < sp.listFreq[len(sp.listFreq)-1] : index+=1
            
        # Recopie des valeurs à conserver
        spUpper.dataVal = spUpper.dataVal[0:index]
        # calcul du nombre d'éléments à rajouter
        nb = len(sp.dataVal) - index
        #Décalage le la liste de nb elements
        for i in range(0, nb) : spUpper.dataVal.insert(0,-1.0e6)

        #On remplace la base de fréquence 'décalée' de lower et upper par la base de fréquence 'standard'
        spLower.listFreq = sp.listFreq
        spUpper.listFreq = sp.listFreq

        return self._selectVal(spLower, sp, spUpper)

    def _selectVal(self,spLower, sp, spUpper) :
        spr = sp
        for i in range(0, len(sp.listFreq)) :
            try :
                v1 = spLower.dataVal[i]
            except IndexError :
                v1 = -200.0
            try :
                v2 = sp.dataVal[i]
            except IndexError :
                v2 = -200.0
            try :
                v3 = spUpper.dataVal[i]
            except IndexError :
                v3 = -200.0

            spr.dataVal[i] = max([v1,v2,v3])

        return spr
    
class filtreLog(filtre) :
    """
        Convertit un spectre en LogLog (log base 10)
            + Possibilité d'obtenir un linLog (abcsisses linéaires, ordonnées en log)
            + Possibilité d'obtenir un logLin (abcsisses log, ordonnées en linéaires) 
    """
    def __init__(self, **listOpt) :
        try :
            self.logAbc = listOpt['logAbc']
        except KeyError :
            self.logAbc = True
        try :
            self.logOrd = listOpt['logOrd']
        except KeyError :
            self.logOrd = True

    def _filtre(self, sp) :
        spr = spectre()
        if self.logAbc :
            spr.listFreq = [math.log10(i) for i in sp.listFreq]
        else :
            spr.listFreq = [i for i in sp.listFreq]
        if self.logOrd :
            spr.dataVal  = [math.log10(i) for i in sp.dataVal]
        else :
            spr.dataVal  = [i for i in sp.dataVal]
            
        return spr

class filtreLin(filtre) :
    """
        Convertit un spectre en LinLin (10^n) à partir d'un spectre en linLog,LogLin ou logLog
    """
    def __init__(self, **listOpt) :
        try :
            self.logAbc = listOpt['logAbc']
        except KeyError :
            self.logAbc = True
        try :
            self.logOrd = listOpt['logOrd']
        except KeyError :
            self.logOrd = True

    def _filtre(self, sp) :
        spr = spectre()
        if self.logAbc :
            spr.listFreq = [10**i for i in sp.listFreq]
        else :
            spr.listFreq = [i for i in sp.listFreq]
        if self.logOrd :
            spr.dataVal  = [10**i for i in sp.dataVal]
        else :
            spr.dataVal  = [i for i in sp.dataVal]
            
        return spr
        
class filtreBandWidth(filtre) :
    def __init__(self, **listOpt) :
        try :
            self.lowerBound = listOpt['lower']
        except KeyError :
            self.lowerBound = 0.2
        try :
            self.upperBound = listOpt['upper']
        except KeyError :
            self.upperBound = 35.5

    def _filtre(self, sp) :
        spr = sp
        toDel = []
        for i in range(0, len(spr.listFreq)) :
            if spr.listFreq[i] > self.upperBound :
                toDel = toDel + [i]

        # Nettoyage des fréquences à suppimer (on commence par les plus hautes)
        for i in toDel[::-1] :
            del spr.listFreq[i]
            del spr.dataVal[i]

        toDel = []
        for i in range(0, len(spr.listFreq)) :
            if spr.listFreq[i] < self.lowerBound :
                toDel = toDel + [i]
            else :
                break

        # Nettoyage des fréquences à suppimer (on finit par les plus basses)
        for i in toDel[::-1] :
            del spr.listFreq[i]
            del spr.dataVal[i]
             
        return spr

class filtreCrible(filtre):
    """
        Criblage du spectre selon specif SEPTEN §C-5 (ce que j'en comprend)
    """
    def __init__(self, **listOpt):
        try :
            self.tolerance = listOpt['tolerance']
        except KeyError :
            self.tolerance = 0.25

        self.listEtats = []

    def _filtre(self, sp) :
            self._initListeEtats(sp) # Création de la table des étsts des valeurs du spectre
            coef = 1

            # Parcours de la liste des fréquences
            i1, i2, i3 = 0, 2, 1
            bTest = True
            while True :
                try :
                    bTest = self._amplitude(sp, i1, i2, i3, coef)
                    if not(bTest) and ((i2-i1) > 2) :
                        # Le point a été éliminé, on réexamine le point précédent sauf si c'est le premier examiné
                        i3 -= 1
                        if self._amplitude(sp, i1, i2, i3, coef) :
                            # Le point a été "récupéré", il devient la nouvelle origine
                            i1 = i3
                            i2 = i2 # écrit quand meme pour la compréhension
                            i3 += 1
                        else :
                            # Le point reste désactivé, on avance au point suivant, le point d'origine est conservé
                            i1 = i1
                            i2 += 1
                            i3 += 2
                    elif not(bTest) and not((i2-i1) > 2) :
                        i1 = i1
                        i2 += 1
                        i3 += 1
                    else : # Le point est conservé, il devient la nouvelle origine
                        i1 = i3
                        i2 += 1
                        i3 += 1
                except IndexError : 
                    break

            return self._crible(sp)

    def _initListeEtats(self, sp) :
        """
            Crée une liste associant à chaque fréquence du spectre passé en paramètre, un état booléen
            qui spécifie si ce couple fréquence-valeur est supprimé ou pas
            NB : au départ toutes les valeur sont "True" car aucune valeur n'a été supprimée
        """
        self.listEtats = [True for x in sp.listFreq]

    def _crible(self, sp) :
        """
            Supprime les points de fréquence qui sont marqué False dans listEtats
        """
        sp2 = spectre([], []) # On force car il y a un problème de persistance su spectre précédent
        for x,y,z in zip(self.listEtats, sp.listFreq, sp.dataVal) :
            if x :
                sp2.listFreq.append(y)
                sp2.dataVal.append(z)
                
        return sp2

    def _amplitude(self, sp, id1, id2, id3, coef=1) :
        """
            teste le point d'indice id3 par rapport aux points à sa gauche(p1 d'indice id1) et 
            à sa droite (p2 d'indice id2).
            Le point est éliminé si sa valeur est en dessous de la droite reliant les points
            d'indice id1 et id2 sauf si sa distance à cette droite est supérieure à :
                tolerance*ordonnée
            Le critère est purement sur l'amplitude du point indépendemment de l'intervalle
            sur lequel il s'applique
        """
        x0 = sp.listFreq[id1]
        y0 = sp.dataVal[id1]
        x1 = sp.listFreq[id2]
        y1 = sp.dataVal[id2]
        x2 = sp.listFreq[id3]
        y2 = sp.dataVal[id3]

        yp2 = interpole(x2, x0, y0, x1, y1)
        
        # Le point est il susceptible d'etre supprimé (est il en dessous de la droite p1-p2 ?)
        # Faut-il le supprimer pour autant (distance y2 à yp2 > tolerance% de y2)
        bSup = not((y2 < yp2) and (abs(yp2-y2)/y2 < self.tolerance))

        # Changement de l'état du point
        self.listEtats[id3] = bSup

        return bSup

class filtreChevauchement(filtre):
    """
        Compare un spectre à un spectre de référence fréquence par fréquence.
        Si une fréquence n'existe pas, on cherche la valeur équivalent par interpolation
        Pour éviter tout recouvrement, il est éventuellement nécessaire de rajouter
        des informations à certaines fréquences
    """
    def __init__(self, **listOpt) :
        try :
            self.spRef = listOpt['ref']
        except KeyError :
            self.spRef = spectre()

        try :
            signe = listOpt['ordre']
            self.ordre = signe/abs(signe) #coefficient +1 ou -1
        except KeyError :
            self.ordre = +1
        except ZeroDivisionError :
            self.ordre = +1

    def _filtre(self, sp) :
        spDict = sp.buildMap()
        spRefDict = self.spRef.buildMap()
        spTestDict = {}

        # On commence par construire un dictionnaire des valeurs à tester comportant toutes les clés contenues
        for k in spDict.keys() : spTestDict[k] = True
        for k in spRefDict.keys() : spTestDict[k] = True

        # On teste ensuite toutes les valeurs du dictionnaire
        for k in spTestDict.keys() :
            # Test d'existence dans le dictionnaire du spectre de référence
            try :
                vr = spRefDict[k]
            except KeyError :
                ki = nearestKeys(k, spRefDict)
                vr = interpole(k, ki[0], spRefDict[ki[0]], ki[1], spRefDict[ki[1]])
            # Test d'existence dans le dictionnaire du spectre à tester
            try :
                vt = spDict[k]
            except KeyError :
                ki = nearestKeys(k, spDict)
                vt = interpole(k, ki[0], spDict[ki[0]], ki[1], spDict[ki[1]])

            # Comparaison des deux valeurs. La clé est ajoutée si elle n'existe pas
            if vt*self.ordre < vr*self.ordre : spDict[k] = vr

        return spectre.sortSpectre(spDict)

class spectre :
    """
        décrit un spectre composé d'un ensemble de résultat associé à un ensemble de fréquence
    """
    def __init__(self, listFreq = [], dataVal = []) :
        self.listFreq = [v for v in listFreq]
        self.dataVal  = [v for v in dataVal]

    def filtre(self, fi) :
        """
        Applique le filtre passé en paramètre au spectre et retourne un nouveau spectre
        """
        return fi(self)
    
    def __staticSortSpectre(dict) :
        """
            Convertit un spectre présenté sous forme d'un dictionnaire en un spectre normal
            Fonction créé parceque les clés du dictionnaire ne sont pas ordonnées
        """
        lstFrq = dict.keys()
        lstFrq.sort()
        lstVal = []
        for fr in lstFrq :
            try :
                lstVal.append(dict[fr])
            except KeyError : # Ne devrait jamais arriver
                lstVal.append(-1E15)

        return spectre(lstFrq, lstVal)
    
    sortSpectre = staticmethod(__staticSortSpectre) # définition en tant que méthode statique

    def getCoupleVal(self,indice) :
        return (self.listFreq[indice], self.dataVal[indice])
    
    def moyenne(self) :
        """
            Calcule la moyenne pondéré : somme(An* dfn) /F
        """
        somme = 0.0
        X0 = self.listFreq[0]
        X1 = self.listFreq[len(self.listFreq)-1]
        for i in range(0,len(self.listFreq)-1) :
            x0 = self.listFreq[i]
            y0 = self.dataVal[i]
            x1 = self.listFreq[i+1]
            y1 = self.dataVal[i+1]
            
            somme = somme + (y0+y1) * abs(x1-x0) / 2

        return somme/abs(X1-X0)

    def seuil(self, limit=75) :
        """
            retourne un couple d'index délimitant l'ensemble des valeurs du spectre
            définissant "limit" pourcent du total cumulé des valeurs
            [borne à gauche inclue, borne à droite exclue[
            ATTENTION on fait l'hypothèse que le spectre a une forme en cloche.
        """
        resu = [0 for v in self.dataVal] # initialisation du tableau resultat

        maxi = max(self.dataVal) # Valeur maxu du spectre
        iMaxi = self.dataVal.index(maxi) # Index de la valeur max du spectre

        # ETAPE 1 : SOMMATION
        somme = 0.0
        for v, i in zip(self.dataVal[0:iMaxi], range(0, iMaxi)) :
            somme = somme + v
            resu[i] = somme

        somme = 0.0
        for v, i in zip(self.dataVal[:iMaxi:-1], range(len(self.dataVal)-1, iMaxi, -1)) :
            somme = somme + v
            resu[i] = somme
            
        resu[iMaxi] = resu[iMaxi-1] + self.dataVal[iMaxi] + resu[iMaxi+1]

        #ETAPE 2 : POURCENTAGE (PAS NECESSAIRE MAIS PLUS LISIBLE)
        for v, i in zip(self.dataVal[0:iMaxi], range(0, iMaxi)) :
            resu[i] = (resu[i] + maxi/2) / resu[iMaxi] * 100

        for v, i in zip(self.dataVal[iMaxi+1:], range(iMaxi+1, len(self.dataVal))) :
            resu[i] = (resu[i] + maxi/2) / resu[iMaxi] * 100

        resu[iMaxi] =  resu[iMaxi-1] + resu[iMaxi+1]

        # ETAPE 3 : RECHERCHE DES BORNES
        limit = (100.0 - limit) / 2.0
        b1 = b2 = True
        for v1, v2 in zip(resu[:], resu[::-1]): # Parcours simultané dans les deux sens
            if b1 and v1 >= limit : # Borne à gauche trouvée
                i1 = resu.index(v1)
                b1 = False
            if b2 and v2 >= limit : # Borne à droite trouvée
                i2 = resu.index(v2) + 1 # Borne à droit exclue de l'intervalle
                b2 = False
                
        return (i1, i2)
        
    def cut(self, nuplet) :
        """
            Découpe un spectre en sous-spectres qui sont retournés en sortie de la fonction
            sous la forme d'un tableau de spectres
        """
        # transformation du nuplet en tableau (permet de lui ajouter un élément)
        tabNuplet = [v for v in nuplet]
        tabNuplet.append(len(self.listFreq))

        # Traitement
        tableRes = list()
        bGauche = 0
        for borne in tabNuplet :
            bDroite = borne
            sp = spectre()
            for i in range(bGauche, bDroite) :
                sp.listFreq.append(self.listFreq[i])
                sp.dataVal.append(self.dataVal[i])

            tableRes.append(sp)
            bGauche = bDroite

        return tableRes

    def __staticMerge(tabSpectre) :
        """
            A l'inverse de la fonction cut, construit un seul spectre à partir d'un ensemble de spectres
        """
        # On vérifie d'abord que les spectres ne partagent pas la meme bande de fréquence (fut ce partiellement)
        for i in range(0, len(tabSpectre)-1) :
            if exclus(tabSpectre[i].listFreq, tabSpectre[i+1].listFreq) : raise SpectreError
        if exclus(tabSpectre[0].listFreq, tabSpectre[len(tabSpectre)-1].listFreq) : raise SpectreError

        spRes = spectre()
        #cumul des spectres
        for sp in tabSpectre :
            for f, v in zip(sp.listFreq, sp.dataVal) :
                spRes.listFreq.append(f)
                spRes.dataVal.append(v)

        return spRes

    merge = staticmethod(__staticMerge) # définition en tant que méthode statique
    
    def buildMap(self) :
        """
            Construit un dictionnaire à partir d'un spectre
        """
        dict = {}
        for i, j in zip(self.listFreq, self.dataVal) :
            dict[i] = j

        return dict 

class nappe :
    """
        décrit un objet nappe qui associe à un ensemble de fréquence à une enesmble de résultats
    """
    def __init__(self, listFreq = [], listeTable = [], listAmor = [], entete = ""):
        self.listFreq = [v for v in listFreq] # recopie physique !
        self.listTable = [list() for t in listeTable] 
        for t, st in zip(listeTable, self.listTable) :
            for v in t  : st.append(v)
                
        self.listAmor = [l for l in listAmor]
        self.entete = entete

    def __staticBuildFromListSpectre(lsp) :
        """
            Construction d'une nappe à partir d'une liste de spectres
        """
        # On commence par vérifier que toutes les nappes on la meme base de fréquences
        # A inclus dans B inclus dans C inclus dans .... et DERNIER inclus dans PREMIER ==> tous égaux 
        for i in range(0,len(lsp.listSp)-1) :
            if inclus(lsp.listSp[i].listFreq, lsp.listSp[i+1].listFreq) : raise NappeCreationError
        if inclus(lsp.listSp[i+1].listFreq, lsp.listSp[0].listFreq) : raise NappeCreationError

        # Construction de la nappe à proprement parler
        listeFreq = [fr for fr in lsp.listSp[0].listFreq]
        listeTable = [list() for sp in lsp.listSp]
        for sp, lv in zip(lsp.listSp, listeTable) :
            for v in sp.dataVal :
                lv.append(v)
        return nappe(listeFreq, listeTable, [], 'toto')
        
    buildFromListSpectre = staticmethod(__staticBuildFromListSpectre) # définition en tant que méthode statique

    def getNbSpectres(self) :
        """ Retourne le nombre d'éléments dans la nappe """
        return len(self.listAmor)

    def getNbFreq(self) :
        """ Retourne le nombre d'éléments dans chaque spectre """
        return len(self.listFreq)

    def getSpectre(self, index) :
        """
            Retourne le spectre d'indice 'index' dans la nappe
        """
        return spectre(self.listFreq, self.listTable[index])

    def filtreDoublons(self):
        """
            Supprime bandes de fréquences constantes 
        """
        prevCpl = None
        bCount = False
        i=0
        # Recherche des doublons
        lstBor = list() # Liste de listes de bornes
        lst = list()
        for cpl in self.__getListFreq() :
            if not(prevCpl) :
                prevCpl = cpl
                continue
            bTest = True
            for v1, v2 in zip(cpl[1], prevCpl[1]) :
                bTest &= (v1==v2)
            if bTest and not bCount : # Début d'une suite de valeurs égales
                bCount = True
                lst.append(i)
            elif not bTest and bCount : # Fin d'une suite de valeurs égales
                bCount = False
                lst.append(i)
                lstBor.append(lst)
                lst = list() # Nouvelle liste

            prevCpl = cpl
            i += 1

        # Suppression des doublons si plus de deux valeurs
        for cpl in lstBor :
            if (cpl[1]-cpl[0]) < 2 : continue
            for i in range(cpl[1]-1, cpl[0], -1) :
                del self.listFreq[i]
                for j in range(0, len(self.listTable)) :
                    del self.listTable[j][i]


            

    def __getListFreq(self) :
        """
            Fonction privé qui parcours la matrice ligne par ligne
            Retourne à chaque itération un couple frequence, liste de valeurs 
        """
        fr = 0.0

        for i in range(0, self.getNbFreq()) :
            fr = self.listFreq[i]
            listVal = []
            for j in range(0, len(self.listTable)):
                listVal.append(self.listTable[j][i])
            yield (fr, listVal)

        raise StopIteration    

class listSpectre :
    """
        classe container d'une liste de spectre ne partageant pas la meme base de fréquence
        cas des spectres à l'issue de la première passe de l'opération de filtrage d'enveloppe
    """
    def __init__(self, *listSp) :
        self.listSp = []
        for sp in listSp :
            self.listSp = sp

    def append(self, spectre) :
        """ Ajoute un spectre à la liste """
        self.listSp.append(spectre)
        
    def __staticBuildFromNappe(uneNappe) :
        """
            Construit une liste de spectres (indépendants) à partir d'une nappe
        """
        res = listSpectre()
        for i in range(0, len(uneNappe.listAmor)) :
            res.append(uneNappe.getSpectre(i))

        return res
    
    buildFromNappe = staticmethod(__staticBuildFromNappe) #Définition en tant que méthode statique
    
    def testChevauchement(self) :
        """
            Supprime les effets de chevauchement entre les spectres
        """
        for i in range(0, len(self.listSp)-1) :
            filter = filtreChevauchement(ref=self.listSp[i+1])
            self.listSp[i] = self.listSp[i].filtre(filter)

    def createBase(self, lspRef = None) :
        """
            Crée une base de fréquence commune pour l'ensemble des spectres
            En s'assurant que le l'on reste enveloppe des spectre de la liste lspRef
        """
        lspRes = listSpectre([spectre() for sp in self.listSp]) # Liste résultante

        # Recherche des fréquences attribuées à 5 spectres, 4 spectres, ... classées dans un dictionnaire
        dctOc = self.__sortByOccurence()

        iOcc = max(dctOc.keys())
        lst = dctOc[iOcc] # On comence par mettre les frequences communes à tous les spectres
        lst.sort() 
        iOcc -= 1
        test = 0
        while True : 
            lspRes.__addFreqFromList(self, lst)
            # On vérifie si on reste enveloppe du spectre initial
            spTest = spectre()
            lstComp = list()
            for sp0, sp1 in zip(lspRes.listSp, self.listSp) :
                filter = filtreChevauchement(ref=sp1)
                spTest = sp0.filtre(filter)
                # Crée une liste des fréquences ajoutées (s'il y en a...)
                for fr in spTest.listFreq :
                    try :
                        idx = sp0.listFreq.index(fr)
                    except ValueError : # Valeur non trouvée dans le tableau
                        lstComp.append(fr)

            if len(lstComp) > 0 : # Il est nécessaire de compléter les spectres
                # on prend de préférence les fréquences définies sur le plus de spectre possible
                while True :
                    lstFreq = dctOc[iOcc]
                    prevLst = lst # On sauvegarde la liste précédente pour comparaison
                    lst = self.__buildList(lstComp, lstFreq)
                    if not(inclus(lst, prevLst)) :
                        iOcc -= 1
                    else :
                        break
                continue
            else :
                break # On peut sortir les spectres sont complets

        self.listSp = lspRes.listSp # Remplacement de la liste des spectres

        # On s'assure que le spectre reste enveloppe du spectre de référence rajoute des fréquences si nécessaire
        # 1. filtre chevauchement
        if lspRef : # Si une liste de spectre de référence a été définie, on vérifie le caractère enveloppe du résultat
            listComp = list()

            for sp1, sp2 in zip(self.listSp, lspRef.listSp) :
                filter = filtreChevauchement(ref=sp2)
                spTest = sp1.filtre(filter)
                test = inclus(spTest.listFreq, sp1.listFreq)
                if test : listComp.append(test)
            # 3. Complément éventuel de l'ensemble des spectres
            if listComp : lspRes.__addFreqFromList(self, listComp)

        self.listSp = lspRes.listSp # Remplacement de la liste des spectres

    def filtre(self, filter):
        """
            Applique un filtre à l'ensemble des spectres de la liste
        """
        self.listSp = [sp.filtre(filter) for sp in self.listSp]


    def __sortByOccurence(self) :
        """
            Fonction qui trie les fréquences par leur occurence d'apparition dans la liste de spectre
        """
        dct = {}
        for sp in self.listSp : # Boucle sur tous les spectres
           for fr in sp.listFreq : # Boucle sur toutes les fréquences de chaque spectre
                try :
                    dct[fr] += 1
                except KeyError :
                    dct[fr] = 1

        # "Inversion" du dictionnaire
        dctOc = {} # Dictionnaire des occurences
        for k in dct.keys() :
            try :
                dctOc[dct[k]].append(k)
            except KeyError :
                dctOc[dct[k]]=[k]


        return dctOc
    
    def __addFreqFromList(self, lstSp, lstFreq) :
        """
            Rajoute les fréquences contenues dans lstFreq aux spectres d'un listeSpectre
            à partir des spectres fournis par le listeSpectre (lstSp) passé en paramètre
            en procédant éventuellement à une interpolation linéaire
        """
        # Suppression des doublons de la liste des fréquences
        lstFreq = listToDict(lstFreq).keys() # lst est la liste des points qu'il faudrait ajouter pour rester enveloppe
        lstFreq.sort()
        
        for i in range(0, len(self.listSp)) :
            # Conversion des spectres en dictionnaire pour pouvoir les traiter
            spDctSelf = self.listSp[i].buildMap()
            spDctRef = lstSp.listSp[i].buildMap() 
            for fr in lstFreq :
                # On cherche la valeur dans le spectre de référence
                try :
                    vr = spDctRef[fr]
                except KeyError :
                    ki = nearestKeys(fr, spDctRef)
                    vr = interpole(fr, ki[0], spDctRef[ki[0]], ki[1], spDctRef[ki[1]])

                # On rajoute la valeur dans le spectre résultat
                spDctSelf[fr] = vr

            # Conversion du dictionnaire en spectre réel
            self.listSp[i] = spectre.sortSpectre(spDctSelf)
            
    def __buildList(self, lstComp, lstFreq) :
        """
            Construit une liste de fréquences à ajouter à partir d'une liste de fréquences
            à ajouter (listComp) et d'une liste de référence, (listFreq)
            retourne une liste
        """
        lst = list()
        for fr in lstComp :
            try :
                idx = lstFreq.index(fr)
                lst.append(fr)
            except ValueError : # Fréquence non présente, recherche de la plus proche 
                couple = nearestKeys(fr, listToDict(lstFreq))
                if abs(couple[0]-fr) > abs(couple[1]-fr) :
                    lst.append(couple[1])
                else :
                    lst.append(couple[0])

        lst = listToDict(lst).keys() # Suppression des doublons
        lst.sort()
        return lst
        
      
def lissage(nappe=nappe,fmin=0.2,fmax=35.5,elarg=0.1,tole_liss=0.25) :
    resultat = listSpectre() # Le résultat sera contenu dans une liste de spectre
    lspBrut = listSpectre.buildFromNappe(nappe)
    # Passage en LogLog
    lspBrut.filtre(filtreLog())
    for j in range(0,nappe.getNbSpectres()) :
        # Spectre brut
        sp = nappe.getSpectre(j)
        # Limitation de la bande de fréquence
        filter = filtreBandWidth(lower=fmin, upper=fmax)
        sp = sp.filtre(filter)
        # Expansion du spectre
        filter = filtreExpand(coef=elarg)
        sp = sp.filtre(filter)
        # Passage en LogLin
        filter = filtreLog(logOrd=False)
        sp = sp.filtre(filter)
        # éclatement du spectre en 3 sous-parties
        tabSpectre = sp.cut(sp.seuil())
        # traitement individuel des sous parties
        filter = filtreCrible(tolerance=2.*tole_liss)
        tabSpectre[0] = tabSpectre[0].filtre(filter)
        tabSpectre[2] = tabSpectre[2].filtre(filter)
        filter.tolerance = tole_liss
        tabSpectre[1] = tabSpectre[1].filtre(filter)
        # Fusion des sous-spectres
        sp = spectre.merge(tabSpectre)

        # Seconde passe de filtrage
        sp = sp.filtre(filter)

        # On passe en log-log pour les tests de chevauchement
        filter = filtreLog(logAbc=False)
        sp = sp.filtre(filter)
        # Ecriture dans la liste de spectres résultat
        resultat.append(sp) # Ajoute la spectre lissé à la liste des spectres
            
    resultat.testChevauchement() # Test de chevauchement entre les spectre de la liste
    resultat.createBase(lspBrut) # construction d'une base commune de fréquence

    # Passage en lin
    resultat.filtre(filtreLin())
        
    # Construction de la nappe résultat
    nappeRes = nappe.buildFromListSpectre(resultat)
    nappeRes.listAmor=nappe.listAmor
    nappeRes.filtreDoublons() # Suppression des valeurs identiques accolées
    
    return nappeRes
