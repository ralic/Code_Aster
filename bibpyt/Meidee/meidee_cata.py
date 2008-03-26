#@ MODIF meidee_cata Meidee  DATE 26/03/2008   AUTEUR BODEL C.BODEL 
# -*- coding: iso-8859-1 -*-
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


##On definit dans ce module python des classes associées à des objets aster,
##permettant leur manipulation en python (pour le calcul numérique).
## - Classe Resultat : regroupe tous les objets sd_resultat. Elle est dévouée
##   mode_meca, grace à des méthodes telles get_modes() qui récupère les
##   caractéristiques modales
## - CaraElem
## - ChampMateriau
## - InterSpectre : regroupe les matrices python et l'objet Aster. Les calculs inverses
##   d'effort sont faits sur des matrices python. Les concepts Aster sont créés par défaut.
##   TODO : la création des objets Aster étant assez longue, ne les créer que lorsque
##   demande explicitement de les saver

import aster
from Utilitai.Utmess import UTMESS
from Cata.cata import modele_sdaster , mode_meca, matr_asse_depl_r, maillage_sdaster
from Cata.cata import cara_elem, cham_mater, table_sdaster, table_fonction
from Cata.cata import mode_stat_forc, base_modale, nume_ddl_sdaster
import Numeric
from Numeric import array, transpose
from Meidee.meidee_iface import CreaTable
from Numeric import zeros
from Accas import _F



aster.onFatalError("EXCEPTION")

#---------------------------------------------------------------------------------


class Resultat:
    """!Gestion des sd_resultat d'aster

    Permet de conserver une référence sur un objet aster sd_resultat
    et permet aussi de récupérer facilement les concepts aster associés
    tels le modèle, le maillage, la numérotation, matrices de masse et raideur
    """
    def __init__(self,
                 objects   = None,    # macro MeideeObjects parente
                 nom       = None,    # nom du concept aster
                 obj_ast   = None,    # concept Aster
                 mess      = None,    # fenetre de messages
                 owned     = None
                 ):
        """Constructeur"""
        self.objects = objects
        self.nom = nom.strip()
        self.obj = obj_ast
        self.cara_mod = None
        self.modele_name = ""
        self.modele = None
        self.maya_name = ""
        self.maya = None
        self.nume_name = ""
        self.nume = None
        self.mass_name = ""
        self.mass = None
        self.kass_name = ""
        self.kass = None
        self.owned = owned
        self.mess = mess

        jdc = CONTEXT.get_current_step().jdc.sds_dict
        # recuperation des maillages associes aux resultats
        numerotation = aster.getvectjev( nom.ljust(19)+'.REFD' )
        #print ".REFD", numerotation
        if numerotation:
            self.kass_name = numerotation[0].strip()
            self.kass = jdc.get(self.kass_name)
            self.mass_name = numerotation[1].strip()
            self.mass = jdc.get(self.mass_name)
            if numerotation[3].strip():
                self.get_nume_1(jdc,numerotation[3])
            else:
                self.get_nume_2(jdc)
        else:
            self.get_nume_2(jdc)

        #self.show()

    def __del__(self):
        from Cata.cata import DETRUIRE
        if not self.owned:
            return
        if isinstance(self.owned, Meidee):
            self.owned.unregister_result( self )
        else:
            self.mess.disp_mess( ( "Destruction de " + self.nom ) )
            self.mess.disp_mess( ( " " ) )
            DETRUIRE(CONCEPT=_F(NOM=(self.obj,)), INFO=1)


    def get_nume_1(self, jdc, nume_name):
        """Recuperation de la numerotation et du nume_ddl"""
        assert nume_name.strip()
        self.nume_name = nume_name.strip()
        self.nume = jdc.get(self.nume_name)
        maillage = aster.getvectjev( nume_name[0:8].ljust(14)+".NUME.REFN")
        #print ".NUME.REFN", maillage
        if maillage:
            self.maya_name = maillage[0].strip()
            self.maya = jdc.get( self.maya_name )


    def get_nume_2(self, jdc ):
        """2e methode pour la recuperation"""
        resu = self.nom
        liste_ordre=aster.getvectjev(resu.ljust(19)+'.ORDR')
        if not liste_ordre:
            return
        for j in liste_ordre:
            if j==0:
                continue
            ordr = "%s.001.%06d" % (resu.ljust(8),j)
            info = aster.getvectjev(ordr.ljust(19)+".REFE")
            #print ".ORDR.REFE", info
            if info is not None:
                if info[1].strip():
                    nom = info[1][0:8].strip()
                    self.maya_name = info[0].strip()
                    self.maya = jdc.get(self.maya_name)


    def get_modele(self):
        """Recherche le modele associe au resultat"""
        if not self.modele:
            modele_name = aster.getvectjev( self.nom.ljust(19) + '.MODL')
            if not modele_name:
                pass
            elif len(modele_name[0].strip()) > 0 :
                self.modele_name = modele_name[0].strip()
                self.modele = self.objects.modeles[self.modele_name]
                return

        # Si cela ne marche pas, on passe par le maillage
        if not self.modele:
            for m, _mod in self.objects.modeles.items():
                if not _mod.maya_name:
                    _mod.get_maillage()
                if _mod.maya_name == self.maya_name:
                    self.modele_name = m
                    self.modele = _mod

    def show(self):
        """!Affichage du concept résultats et des concepts liés"""
        self.mess.disp_mess( ( self.nom + " : " ) )
        self.mess.disp_mess( ( ".modele" + self.modele_name ) )
        self.mess.disp_mess( ( ".maillage" +  self.maya_name ) )
        self.mess.disp_mess( ( ".nume" + self.nume_name ) )
        self.mess.disp_mess( ( ".mass" + self.mass_name ) )
        self.mess.disp_mess( ( ".kass" + self.kass_name ) )
        self.mess.disp_mess( ( " " ) )
   
    def get_modes(self):
        """!récupère les numéros et fréquences des modes
        d'un concept mode_meca"""
        from Cata.cata import RECU_TABLE
        __freq  = RECU_TABLE(CO=self.obj,
                             NOM_PARA='FREQ',);
        afreq  = __freq.EXTR_TABLE().Array('NUME_ORDRE','FREQ')
        
        resu = [afreq]
        
        return self._get_modes_data(resu)

    def _get_modes_data(self, resu):
        """!récupère les données définissant un mode de vibration"""
        from Cata.cata import RECU_TABLE
        __axsi  = RECU_TABLE(CO=self.obj,
                             NOM_PARA='AMOR_REDUIT',);

        __amge  = RECU_TABLE(CO=self.obj,
                             NOM_PARA='AMOR_GENE',);

        __mass  = RECU_TABLE(CO=self.obj,
                             NOM_PARA='MASS_GENE',);

        __modes = RECU_TABLE(CO=self.obj,
                             NOM_PARA='NUME_MODE',);

        __raid  = RECU_TABLE(CO=self.obj,
                             NOM_PARA='RIGI_GENE',);
        
        tables_data = [
            ('AMOR_REDUIT', __axsi),
            ('MASS_GENE', __mass),
            ('NUME_MODE', __modes),
            ('AMOR_GENE', __amge),
            ('RIGI_GENE', __raid)
            ]
        
        for nom, table in tables_data:
            try:
                resu_array = table.EXTR_TABLE().Array('NUME_ORDRE', nom)
            
            except TypeError:
                pass
                resu_array = Numeric.ones(resu[0].shape, Numeric.Float)
##                self.mess.disp_mess("!! il manque " \
##                                    "le paramètre modal %s !!" % nom)
##                self.mess.disp_mess("!! pour le résultat %s !!" % self.nom)
##                self.mess.disp_mess("!! les calculs risquent d'être faux " \
##                                    "ou incomplets !!")
##                self.mess.disp_mess(" ")

            resu.append(resu_array)
        
        return resu
    
    def get_modes_stat(self):
        """!récupère les num'eros et directions des modes d'un concept mode_stat"""

        nomno = self.nom.ljust(19)+".ORDR        "
        numemo = aster.getvectjev( nomno.ljust(32) )

        nomno = self.nom.ljust(19)+".NOEU        "
        resu = aster.getvectjev( nomno.ljust(32) )

        return numemo,resu

    def get_cara_mod(self):
        """!Retourne une matrice avec toutes les cara modales d'une instance de Resultat
            Attention : comme toutes les caras sont dans un array, elles sont toutes du
            meme type, donc les entiers sont transformes en reels"""
        afreq, axsi, amass, amodes, bid1, bid2 = self.get_modes()
        self.cara_mod = Numeric.concatenate((Numeric.transpose(Numeric.array([afreq[:,0]])),
                                             Numeric.transpose(Numeric.array([afreq[:,1]])),
                                             Numeric.transpose(Numeric.array([axsi[:,1]])),
                                             Numeric.transpose(Numeric.array([amass[:,1]])),
                                             Numeric.transpose(Numeric.array([amodes[:,1]]))),
                                             axis=1
                                            )
        return self.cara_mod


    def extr_matr(self, extract_ddl):
        """ Extrait les champs de deformees contenus dans le resultat"""

        self.nume_phy, self.nume_mat, all_ddls = nume_ddl_phy(self, extract_ddl)

        if not self.cara_mod:
            self.cara_mod = self.get_cara_mod()
        nb_mod = self.cara_mod.shape[0] # nb de modes

        matrice = []
        for ind_mod in range(1, nb_mod +1):
            defo  = []
            champ = crea_champ(self.obj, ind_mod, all_ddls)
            # champ est un dictionnaire avec les ddl en clés et les valeurs associees
            for ddl in self.nume_phy:
                defo.append(champ[ddl])
            matrice.append(defo)
            
        matrice = transpose(array(matrice))
        
        return matrice



    def show_cara_mod(self):
        cara_mod = self.get_cara_mod()
        self.mess.disp_mess(self.nom)
        self.mess.disp_mess("caracteristiques modales de" + self.nom)
        self.mess.disp_mess("NUME_ORDRE  FREQUENCE  MASS_GENE  AMOR_REDUIT  NUME_MODE")
        for ind in range(Numeric.size(cara_mod,0)):
            self.mess.disp_mess("%3i        %7.5g    %7.5g        %7.5g      %3i" %tuple(cara_mod[ind,:])  )





#--------------------------------------------------------------------------------------------------------------

class CaraElem:
    """!Gestions des cara modales"""
    CARTES = [
        "CARARCPO",
        "CARCABLE",
        "CARCOQUE",
        "CARDISCA",
        "CARDISCK",
        "CARDISCM",
        "CARGENBA",
        "CARGENPO",
        "CARGEOBA",
        "CARGEOPO",
        "CARMASSI",
        "CARORIEN",
        "CARPOUFL",
        ]

    def __init__(self, nom, obj, mess, owned=False):
        self.nom = nom
        self.obj = obj
        self.mess = mess
        self.owned = owned

    def maillages(self):
        maillages = []
        for carte in CaraElem.CARTES:
            ncham = self.nom.ljust(8)+"."+carte.ljust(10)+".NOMA"
            maill = aster.getvectjev( ncham.ljust(32) )
            if maill:
                for m in maill:
                    if m not in maillages:
                        maillages.append(m)
        return maillages


#-------------------------------------------------------------------------------------

class ChampMateriau:
    """!Gestion d'un concept champ_mater"""
    def __init__(self, nom, obj, mess, owned=False):
        self.nom = nom
        self.obj = obj
        self.mess = mess
        self.owned = owned

    def maillage(self):
        ncham = self.nom.ljust(8)+".CHAMP_MAT .NOMA        "
        maill = aster.getvectjev( ncham.ljust(32) )
        if maill is not None:
            return maill[0]
        return ""

#--------------------------------------------------------------------------------------

class InterSpectre:
    """!Gestion des concepts de type table interspectrale

    Regroupe les concepts aster de type table interspectrale :
    - Différencie les tabl_instp des autres tables tabl_sdaster,
    - Extrait les tabl_intsp sous forme d'une matrice python interspectrale
    - Crée un lien entre les numérotations des ddl de cette matrice
      avec les numérotations d'un modèle EF. ex : la ligne/colonne 3 de la
      matrice interspectrale correspond au noeud 1DZ du modèle
    - Crée une table inter-spectrale sd_aster à partir d'une matrice python
         - Crée une table inter-spectrale sd_aster à partir d'une matrice python
        On peut créer une table avec le nom, la table format aster (obj_ast) ou la matrice
        format python (mat)
    """
    def __init__(self,
                 nom        = None,
                 obj_ast    = None,
                 mat        = None,
                 frequences = [],
                 mess       = None,
                 var_opt    = None,
                 owned      = None):
        self.nom = nom.strip()                # nom aster de la sd
        self.obj = obj_ast                    # objet aster (à remplir ou à fabriquer)
        self.matr_inte_spec = mat             # matrice inter-spectrale format python
        self.intsp = 0                        # vaut 1 si la table est un intsp, 0 sinon
        self.var_opt(var_opt)                 # definit self.opt : vaut 1 si les modeles sont définis
        self.f = frequences
        self.resu_name = ""
        self.resu = None
        self.maya_name = ""
        self.maya = None
        self.nume_name = ""
        self.nume_phy = None
        self.nume_gene = None
        self.mass_name = ""
        self.mass = None
        self.kass_name = ""
        self.kass = None
        self.options_meth = ["Efforts discrets localises",
                             "Efforts et moments discrets"]
        self.mess = mess


        try:
            if len(self.f) == 0:
                self.extr_freq()
            self.intsp = 1

        except KeyError:
            # Cas où la table_sdaster n'est pas une tabl_intsp
            pass # TODO : faire en sorte que cette table ne soit pas visible
            

    def make_inte_spec(self, titre, paras_out):
        """
        Fabrique un objet aster de type table inter-spectre :
         - Creation de n(n+1)/2 fonctions, où n = dim(matrice interspectrale)
         - Creation d'une table
        """
        from Cata.cata import DEFI_FONCTION
        dim = self.matr_inte_spec.shape[1]
        nb_freq = len(self.f)
        l_fonc = []
        nume_ordr = []
        for i in range(dim):
            for j in range(i,dim):
                fonc = []
                for ind_freq in range(nb_freq):
                    fonc.append(self.f[ind_freq])
                    fonc.append(self.matr_inte_spec[ind_freq,i,j].real)
                    fonc.append(self.matr_inte_spec[ind_freq,i,j].imag)
                nume_ordr.append([int(i),int(j)])

                _fonc=DEFI_FONCTION( NOM_PARA   ="FREQ",
                                     NOM_RESU   ="DSP",
                                     INTERPOL   ="NON",
                                     INFO       =1,
                                     VALE_C     =fonc,)
                l_fonc.append(_fonc.nom)

        nume_ordr = Numeric.array(nume_ordr)
        nume_i = nume_ordr[:,0]
        nume_j = nume_ordr[:,1]
        mcfact=[]
        mcfact.append(_F(PARA='NOM_CHAM'    ,LISTE_K=('DSP')   ,NUME_LIGN=(1,)))
        mcfact.append(_F(PARA='OPTION'      ,LISTE_K=('TOUT',) ,NUME_LIGN=(1,)))
        mcfact.append(_F(PARA='DIMENSION'   ,LISTE_I=(dim,)    ,NUME_LIGN=(1,)))
        if isinstance(self.resu, Resultat):
            # Si on associe l'inter-spectre à un résultat,
            # on range les fonctions par rapport aux noeuds et composantes
            if not self.nume_phy:
                self.nume_phy, self.nume_mat, bid = nume_ddl_phy(self.resu)
            ddl = self.nume_phy
            noeu_i = []
            noeu_j = []
            cmp_i = []
            cmp_j = []
            for ind in range(len(nume_i)):
                li = ddl[nume_i[ind]].split("_")
                lj = ddl[nume_j[ind]].split("_")
                noeu_i.append(li[0])
                noeu_j.append(lj[0])
                cmp_i.append(li[1])
                cmp_j.append(lj[1])
            mcfact.append(_F(PARA='NOEUD_I',LISTE_K=noeu_i  ,NUME_LIGN=range(2,len(nume_i)+2)))
            mcfact.append(_F(PARA='NOEUD_J',LISTE_K=noeu_j  ,NUME_LIGN=range(2,len(nume_j)+2)))
            mcfact.append(_F(PARA='NOM_CMP_I',LISTE_K=cmp_i   ,NUME_LIGN=range(2,len(nume_i)+2)))
            mcfact.append(_F(PARA='NOM_CMP_J',LISTE_K=cmp_j   ,NUME_LIGN=range(2,len(nume_j)+2)))
        else:
            mcfact.append(_F(PARA='NUME_ORDRE_I',LISTE_I=(1+nume_i).tolist()    ,NUME_LIGN=range(2,len(nume_i)+2)))
            mcfact.append(_F(PARA='NUME_ORDRE_J',LISTE_I=(1+nume_j).tolist()    ,NUME_LIGN=range(2,len(nume_j)+2)))
        mcfact.append(_F(PARA='FONCTION_C'  ,LISTE_K=l_fonc             ,NUME_LIGN=range(2,len(l_fonc)+2)))

        self.obj = CreaTable(mcfact, titre,
                             paras_out,
                             self.mess,
                            )

    def def_inte_spec(self, intsp):
        """ Associe une table intsp aster à l'instance de InterSpectre"""
        self.obj = intsp

    def def_nom(self, nom):
        """ Associe un nom (self.nom) à l'InterSpectre"""
        self.nom = nom

    def var_opt(self, opt):
        if opt =='Efforts discrets localises':
            self.opt = 0
        elif opt =='Efforts et moments discrets':
            self.opt = 1
        else:
            self.opt = 0



    def set_model(self, resu):
        """Lie l'inter-spectre au concept mode_meca OBS. Permet de lier les
        lignes et colonnes de l'inter-spectre aux DDL des deformees modales
        et de tout ranger dans le bon ordre. Si l'inter-spectre est defini
        avec des numeros d'ordre, alors on suppose qu'ils sont rangés dans le bon
        ordre.
        """
        self.resu = resu


    def extr_inte_spec(self, resu, extract_ddl):
        """!Extraction d'une matrice inter-spectrale à partir d'une tabl_insp"""
        from Cata.cata import RECU_FONCTION
        from Cata.cata import DETRUIRE
        self.mess.disp_mess("Extraction de l'inter-spectre " + self.nom)
        self.mess.disp_mess(" ")
        tabl_py = self.obj.EXTR_TABLE()
        nom_fonc= tabl_py['FONCTION_C'].values()['FONCTION_C']
        nb_freq = len(self.f)

        self.set_model(resu)
        self.nume_phy, self.nume_mat, bid = nume_ddl_phy(resu, extract_ddl)
        nb_mes = len(self.nume_phy)
        
        # il doit y avoir coherence de longueur entre taille de l'inter-spectre et le nombre de DDL du resu
        if nb_mes*(nb_mes+1)/2 != len(nom_fonc):
            nb_mes_intsp = 0.5*(-1+Numeric.sqrt(1+8*len(nom_fonc)))
            self.mess.disp_mess(" Nombre de mesures de CPhi : " + str(int(nb_mes)))
            self.mess.disp_mess(" Nombre de mesures de l'inter-spectre : "
                                + str(int(nb_mes_intsp)))
            self.mess.disp_mess(" ")
            raise TypeError
        
        self.matr_inte_spec = Numeric.zeros((nb_freq, nb_mes, nb_mes),
                                             Numeric.Complex)

        coupl_ddl = []
        try:
            # Cas ou l'inter-spectre est defini par ses noeuds et composantes
            noeudi  = tabl_py['NOEUD_I'].values()['NOEUD_I']
            noeudj  = tabl_py['NOEUD_J'].values()['NOEUD_J']
            cmpi    = tabl_py['NOM_CMP_I'].values()['NOM_CMP_I']
            cmpj    = tabl_py['NOM_CMP_J'].values()['NOM_CMP_J']
            ddli = []
            ddlj = []
            for ind in range(len(cmpi)):
                coupl_ddl.append( (noeudi[ind].split()[0] + '_' + cmpi[ind].split()[0],
                                   noeudj[ind].split()[0] + '_' + cmpj[ind].split()[0]) )
            isnume = 1
        except KeyError:
            # l'inter-spectre n'est défini qu'avec des numéros d'ordre indépendants du modèle
            numi  = tabl_py['NUME_ORDRE_I'].values()['NUME_ORDRE_I']
            numj  = tabl_py['NUME_ORDRE_J'].values()['NUME_ORDRE_J']
            coupl_ddl.append((numi,numj))
            isnume = 0 

        
        try:
            # Methode de recherche rapide des fonctions
            ctx = CONTEXT.get_current_step().jdc.sds_dict
            fonc_py = [ ctx[fonc].convert('complex') for fonc in nom_fonc ]
        except KeyError:
            fonc_py = []
            ind_fonc = 0
            nb_fonc = nb_mes*(nb_mes+1)/2
            for ind_fonc in range(nb_fonc):
                __FONC = RECU_FONCTION( TABLE = self.obj,
                                        NOM_PARA_TABL = 'FONCTION_C',
                                        FILTRE = _F(NOM_PARA='FONCTION_C',VALE_K=nom_fonc[ind_fonc])
                                       )
                fonc_py.append( __FONC.convert('complex'))
                DETRUIRE( CONCEPT = _F( NOM = __FONC ),INFO=1 )
                ind_fonc = ind_fonc + 1

        # Rangement dans l'ordre des fonctions (par rapport à la numérotation du self.resu)
        nume = self.nume_phy
        for ind_coupl in range(len(coupl_ddl)):
            try:
                ind_l = nume.index(coupl_ddl[ind_coupl][0])
                ind_c = nume.index(coupl_ddl[ind_coupl][1])
            except ValueError:
                
                raise TypeError
            for ind_freq in range(nb_freq):
                self.matr_inte_spec[ind_freq,ind_l,ind_c] = fonc_py[ind_coupl].vale_y[ind_freq]
                if ind_l != ind_c:
                    self.matr_inte_spec[ind_freq,ind_c,ind_l] = Numeric.conjugate(self.matr_inte_spec[ind_freq,ind_l,ind_c])
                    


    def extr_freq(self):
        """Extraction des fréquences d'étude dans la tabl_intsp qui contient
        les inter-spectres mesurés"""
        from Cata.cata import RECU_FONCTION
        from Cata.cata import DETRUIRE
        tabl_py=self.obj.EXTR_TABLE()
        toto=tabl_py['FONCTION_C']
        nom_fonc = toto.values()['FONCTION_C'][0]
        __FONC = RECU_FONCTION(TABLE = self.obj,
                               NOM_PARA_TABL = 'FONCTION_C',
                               FILTRE = _F(NOM_PARA='FONCTION_C',VALE_K=nom_fonc)
                               )
        freq=__FONC.Absc()
        DETRUIRE(CONCEPT=_F(NOM=__FONC),INFO=1)
        self.f = freq
        self.intsp = 1


#---------------------------------------------------------------------------------------------

class Modele:
    """!Gestion des concepts de type modele_sdaster
        Notamment une routine qui permet de fabriquer un nume_ddl
        a partir d'un modele pour les rojtines de type PROJ_CHAMP
    """
    def __init__(self,
                 objects    = None,
                 nom        = None,
                 obj_ast    = None,
                 mess       = None,
                 nume_ddl   = None,
                 owned      = None):
        self.objects = objects                # les concepts existants dans le jdc
        self.nom = nom.strip()                # nom aster de la sd
        self.obj = obj_ast                    # objet aster
        self.nume_ddl = nume_ddl              # Nom d'un nume_ddl associe au modele
        self.mess = mess                      # fenetre de messages
        self.maya = None
        self.maya_name = ""


    def get_maillage(self):
        if self.obj.MODELE.LGRF.exists:
            _maillag = self.obj.MODELE.LGRF.get()
            self.maya_name = _maillag[0].strip()
            self.maya = self.objects.maillages[self.maya_name]
        else:
            pass
##            print "on ne trouve pas le maillage associe au modele", self.nom


    def get_nume(self):
        """Recherche des nume_ddl qui depend de ce modele
        """
        if self.nume_ddl == None:
            for nume_name, nume in self.objects.nume_ddl.items():
                model = aster.getvectjev(nume_name.ljust(14) + ".NUME.LILI")
                if not model:
                    pass
                elif model[1][:8] == self.nom:
                    self.nume_ddl = nume
                    self.nume_ddl_name = nume_name
                    return
##            print "pas de Nume_ddl trouve pour le modele", self.nom
            ## TODO : creation automatique d'un nume_ddl pour les resu exp
            ## avec des caras bidons.
        

    def make_nume(self):
        """Fabrication d'un nume ddl pour des modeles experimentaux
           avec des cara_elem et affe_materiau pipos
        """
        ## TODO : ce n'est pas tres simple : il faut aller chercher les modelisations
        ## de AFFE_MODELE, et associer les bons cara_elem : BARRE, DIS_T, DIS_TR...
        pass            

    def set_extraction_ddl(self, ddls):
        self.extraction_ddl = ddls



#---------------------------------------------------------------------------------------------


class MeideeObjects:
    """!Classe qui recupere les objets pouvant etre utilises par
    meidee dans le catalogue aster"""

    def __init__(self, macro, mess):
        """!Constructeur

        \param macro Le self de la macro qui utilise cet objet
        """
        self.mess = mess
        self.modeles = {}
        self.maillages = {}
        self.groupno = {}
        self.resultats = {}
        self.masses = {}
        self.maillage_modeles = {}
        self.groupno_maillage = {}
        self.cara_elem = {}
        self.cham_mater = {}
        self.inter_spec = {}
        self.nume_ddl = {}
        self.macro = macro
        self.weakref = []
        self.recup_objects()
        self.grno = {}


    def recup_objects( self ):
        self.del_weakref()
        jdc = CONTEXT.get_current_step().jdc

        for i, v in jdc.sds_dict.items():
            if isinstance( v, modele_sdaster ):
                self.modeles[i] = Modele(objects = self, nom = i,obj_ast = v,mess = self.mess)
            elif isinstance( v, mode_meca ) or isinstance( v, base_modale ):
                self.resultats[i] = Resultat(objects = self, nom = i,obj_ast = v,mess = self.mess)
            elif isinstance( v, matr_asse_depl_r ):
                self.masses[i] = v
            elif isinstance( v, maillage_sdaster ):
                self.maillages[i] = v
            elif isinstance( v, cara_elem ):
                self.cara_elem[i] = v
            elif isinstance( v, cham_mater ):
                self.cham_mater[i] = v
            elif isinstance( v, table_sdaster ):
                self.inter_spec[i] = InterSpectre(nom = i, obj_ast = v, mess = self.mess)
            elif isinstance( v, mode_stat_forc ):
                self.resultats[i] = Resultat(objects = self, nom = i,obj_ast = v,mess = self.mess)
            elif isinstance( v, nume_ddl_sdaster ):
                self.nume_ddl[i] = v

        #self.debug()
        ## Liaison des concepts entre eux (resu <=> maillage <=> modele)
        for resu_name, resu in self.resultats.items():
            resu.get_modele()

        for modele_name, modele in self.modeles.items():
            modele.get_maillage()
            modele.get_nume()


    def new_objects( self ):
        """! Récupération des nouveaux concepts, s'il y en a, lorsqu'on
             change d'onglet. Utilisé uniquement pour les concepts
             Resultat et InterSpectre"""
        self.del_weakref()
        jdc = CONTEXT.get_current_step().jdc

        for i, v in jdc.sds_dict.items():
            if isinstance( v, mode_meca ):
                if not self.resultats.has_key(i):
                    self.resultats[i] = Resultat(i,v,self.mess)
                    self.resultats[i].get_modele()
            elif isinstance( v, table_sdaster ):
                if not self.inter_spec.has_key(i):
                    self.inter_spec[i] = InterSpectre(nom = i, obj_ast = v, mess = self.mess)
            elif isinstance( v, mode_stat_forc ):
                if not self.resultats.has_key(i):
                    self.resultats[i] = Resultat(i,v,self.mess)
                    self.link_objects()


    def debug(self):
        self.mess.disp_mess( ( "Modeles" + self.modeles ) )
        self.mess.disp_mess( ("Maillages" + self.maillages ) )
        self.mess.disp_mess( ("Masses" + self.masses ) )
        self.mess.disp_mess( ("Resultats" ) )
        self.mess.disp_mess( ( " " ) ) 
        for v in self.resultats.values():
            v.show()



    def get_groupno(self):
        """!essaye de relier les concepts entre eux"""

        # recuperation des GROUP_NO associes aux maillages
        # et association des resultats a un modele/maillage (afaire)

        for m, _mail in self.maillages.items():
            dic_gpno=aster.getcolljev(m.ljust(8)+'.GROUPENO')
            for elem in dic_gpno.keys():
                self.groupno_maillage[elem] = m
            return dic_gpno.keys()

    def get_resu(self, name):
        """!Renvoie un objet resultat identifie par son nom"""
        return self.resultats[name]

    def get_cara_elem_obj(self, name):
        return self.cara_elem[name]

    def get_cham_mater_obj(self, name):
        return self.cham_mater[name]

    def get_model_name(self):
        """!Renvoie les noms de modeles dispos"""
        return self.modeles.keys()

    def get_model(self, name):
        """!Renvoie un modele"""
        return self.modeles[name]

    def get_inter_spec_name(self):
        inter_spec = []
        for i in self.inter_spec.keys():
            if self.inter_spec[i].intsp == 1:
                inter_spec.append(i)
        return inter_spec

    def get_inter_spec(self, name):
        return self.inter_spec[name]

    def get_matr(self, name):
        """!Renvoie une matrice de masse ou raideur ou None"""
        return self.masses.get(name)

    def get_resultats(self):
        """!Liste des objets resultat"""
        return self.resultats.keys()

    def get_resultats_num(self):
        """!Pas de difference avec get_resultats pour l'instant"""
        return self.get_resultats()

    def get_resultats_exp(self):
        """!Pas de difference avec get_resultats pour l'instant"""
        return self.get_resultats()

    def get_matr_norme(self):
        normes = self.masses.keys()
        normes[0:0] = ["Aucune"]
        return normes

    def get_matr_name(self):
        return self.masses.keys()

    def get_cara_elem(self, resultat=None):
        if resultat is None:
            return self.cara_elem.keys()
        l = []
        if resultat not in self.resultats:
            return []
        maya = self.resultats[resultat].maya_name
        for k, v in self.cara_elem.items():
            elem = CaraElem( k, v, self.mess )
            maillages = elem.maillages()
            if maya in maillages:
                l.append(k)
        return l

    def get_cham_mater(self, resultat=None):
        if resultat is None:
            return self.cham_mater.keys()
        l = []
        if resultat not in self.resultats:
            return []
        maya = self.resultats[resultat].maya_name
        for k, v in self.cham_mater.items():
            elem = ChampMateriau( k, v, self.mess )
            maillage = elem.maillage()
            if maya == maillage:
                l.append(k)
        return l

    def register_weakref(self,name):
        """ garde les NOMS des concepts destinés à être supprimés à chaque
            mse à jour de meidee_objects """
        self.weakref.append(name)

    def del_weakref(self):
        liste = ""
        if len(self.weakref) != 0:
            for obj in self.weakref:
                DETRUIRE(CONCEPT = _F(NOM = obj), INFO=1)
                liste = liste + ", " + obj
            self.weakref = []
            self.mess.disp_mess("Destruction des objects temporaires " + liste) 
        


##############################################################################
#
#                          PETITS UTILITAIRES
#
##############################################################################

def compt_cmp(champ, ddl_test):
    """! Permet de compter le nombre de composantes
    d'un champ aux noeuds.
    
    :param ddl_test: un degré de liberté existant pour le champ."""

    nbval = len(aster.getvectjev(champ.nom.ljust(19) + '.VALE'))
    champy  = champ.EXTR_COMP(ddl_test,[],1)
    nbno = len(champy.noeud)
    nbcmp = nbval/nbno

    return nbcmp

_DDL_CONV = {
    'DX' : .01,
    'DY' : .02,
    'DZ' : .03,
    'DRX' : .04,
    'DRY' : .05,
    'DRZ' : .06
    }

def tra_comp(nume):
    """! Transforme 'N23_DX' => 23.01 (float)"""
    tmp = nume.split('_')
    numno = int(tmp[0][1:])
    ddlno = tmp[1]
    
    return numno + _DDL_CONV[ddlno]


def set_extraction_ddl(resu, ddls):
    """ cf utilisation de nume_ddl_phy ci-dessous"""
    maya = resu.maya
    all_ddls = []
    extraction_ddl = []
    for grp in ddls:
        # grp = {'GROUP_NO':'nom','DDL_ACTIF':['DX','DY']}
        list_no = find_no(maya,grp)
        for ddl in grp['DDL_ACTIF']:
            if ddl not in all_ddls:
                all_ddls.append(ddl)
        extraction_ddl.append({'NOEUD':list_no,
                               'DDL_ACTIF':grp['DDL_ACTIF']})

    return extraction_ddl, all_ddls
    

def crea_champ(resu, ind_mod, all_ddls): 
    """!Extrait les champs de deplacement d'une sd_resultat aster
        a partir des DDL de mesure pour un mode donne.
        Ces DDL sont identiques a ceux de la macro OBSERVATION
        ayant servi a obtenir le resultat."""
    from Cata.cata import CREA_CHAMP
    from Cata.cata import DETRUIRE
    __CHANO = CREA_CHAMP( TYPE_CHAM = 'NOEU_DEPL_R',
                          OPERATION = 'EXTR',
                          RESULTAT = resu,
                          NOM_CHAM = 'DEPL',
                          NUME_ORDRE = ind_mod,
                        );
    
    champ = {}
    for ddl_key in all_ddls:
        champ_comp = __CHANO.EXTR_COMP(ddl_key, [], 1)
        noeud = champ_comp.noeud
        vale = champ_comp.valeurs
        for ind in range(len(champ_comp.noeud)):
            champ['N' + str(noeud[ind]) + '_' + ddl_key] = vale[ind]

    DETRUIRE(CONCEPT=_F(NOM=(__CHANO,),), INFO=1,)
    
    return champ


def nume_ddl_phy(resu, extract_ddl):
    """Fabrication de 2 numerotations liees au concept OBSERVATION cree
    pour le modele d'oservabilite. Les DDL a extraire sont donnes en entree
    sous la forme
    [{'GROUP_NO':'name1','DDL_ACTIF':['DX','DY']},{'GROUP_NO':'name2,...
    et sont changes en [{'NOEUD':('N1','N2'),'DDL_ACTIF':('DX','DY')...
    par la fonction set_extraction_ddl
    """
    nume = []
    nume_mat = []

    ddls, all_ddls = set_extraction_ddl( resu, extract_ddl )

    for ind1 in ddls:
        for noeud_id in ind1['NOEUD']:
            for ddl_key in ind1['DDL_ACTIF']:
                compo = noeud_id.strip() + "_" + ddl_key.strip()
                if compo not in nume:
                    nume.append(compo)
                    nume_mat.append(tra_comp(compo))
                else:
                    mess.disp_mess("Le concept observe possede"\
                                   "des noeuds en double. Calcul"\
                                   "impossible")
                    return

    return nume, nume_mat, all_ddls


def nume_ddl_gene(resu, extract_mode = None):
    """
    Crée le meme vecteur de numérotation avec une numérotation par modes.
    Retourne "MO"+#mode
    """
    modes = []
    afreq, axsi, amass, amodes, amor, arigi = resu.get_modes() # recup des cara modales du resultat associé à l'interspectre
    nb_mod = amodes.shape[0]
    for mod in amodes[:,1]:
        modes.append('MO'+str(int(mod)))
    return modes


def find_no(maya,mcsimp):
    """ mcsimp est de la forme : 
        {'GROUP_MA': ('CAPTEUR1','CAPTEUR2'), 'DDL_ACTIF': ('DX', 'DRZ')}
        ou {'GROUP_NO': 'CAPTEUR1', 'DDL_ACTIF': ('DX', 'DRZ')}
    """

    
    if mcsimp.has_key('GROUP_NO') and type(mcsimp['GROUP_NO']) != list :
        mcsimp['GROUP_NO'] = [mcsimp['GROUP_NO']]
    if mcsimp.has_key('GROUP_MA') and type(mcsimp['GROUP_MA']) != list :
        mcsimp['GROUP_MA'] = [mcsimp['GROUP_MA']]


    list_no = []
    if mcsimp.has_key('GROUP_NO') :
        for group in mcsimp['GROUP_NO'] :
            list_ind_no = list(Numeric.array(maya.GROUPENO.get()
                                             [group.ljust(8)]) - 1)
            for ind_no in list_ind_no :
                nomnoe = maya.NOMNOE.get()[ind_no]
                if nomnoe not in list_no :
                    list_no.append(nomnoe)
        
    elif mcsimp.has_key('GROUP_MA') :
        for group in mcsimp['GROUP_MA']:
            list_nu_ma = list(Numeric.array(maya.GROUPEMA.get()
                                            [group.ljust(8)]) - 1)
            tmp = list(maya.NOMMAI.get())
            for nu_ma in list_nu_ma:
                maille = tmp[nu_ma]
                for ind_no in maya.CONNEX.get()[nu_ma +1 ]:
                    nomnoe = maya.NOMNOE.get()[ind_no - 1]
                    if nomnoe not in list_no:
                        list_no.append(nomnoe) 

    return list_no
