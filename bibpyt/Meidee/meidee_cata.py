#@ MODIF meidee_cata Meidee  DATE 22/12/2006   AUTEUR BODEL C.BODEL 
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
        

from Utilitai.Utmess import UTMESS
##try:
from Cata.cata import modele_sdaster , mode_meca, matr_asse_depl_r, maillage_sdaster
from Cata.cata import cara_elem, cham_mater, table_sdaster, table_fonction
from Cata.cata import RECU_TABLE, RECU_FONCTION, DEFI_FONCTION, CREA_CHAMP, DETRUIRE
import aster
import Numeric
from Meidee.meidee_iface import CreaTable
from Numeric import zeros
from Accas import _F


    
##except ImportError:
##    UTMESS('F',  'MACRO_VISU_MEIDEE',
##           "ERREUR PENDANT L'IMPORTATION DES MODULES MEIDEE")    

aster.onFatalError("EXCEPTION")

#---------------------------------------------------------------------------------

class Resultat:
    """!Gestion des sd_resultat d'aster
    
    Permet de conserver une référence sur un objet aster sd_resultat
    et permet aussi de récupérer facilement les concepts aster associés
    tels le modèle, le maillage, la numérotation, matrices de masse et raideur
    """
    def __init__(self, nom, resultat, mess, owned=None):
        """Constructeur"""
        self.nom = nom.strip()
        self.obj = resultat
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
        if not self.owned:
            return
        if isinstance(self.owned, Meidee):
            self.owned.unregister_result( self )
        else:
            self.mess.disp_mess( ( "Destruction de " + self.nom ) )
            self.mess.disp_mess( ( " " ) ) 
            DETRUIRE(CONCEPT=_F(NOM=(self.obj,) ) )


    def get_nume_1(self, jdc, nume_name):
        """!Une première méthode pour essayer de récuperer la numérotation associée"""
        assert nume_name.strip()
        self.nume_name = nume_name.strip()
        self.nume = jdc.get(self.nume_name)
        maillage = aster.getvectjev( nume_name[0:8].ljust(14)+".NUME.REFN")
        #print ".NUME.REFN", maillage
        if maillage:
            self.maya_name = maillage[0].strip()
            self.maya = jdc.get( self.maya_name )


    def get_nume_2(self, jdc ):
        """!Une autre méthode pour essayer de récuperer la numérotation associée"""
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


    def show(self):
        """!Affichage du concept résultats et des concepts liés"""
        self.mess.disp_mess( ( self.nom + " : " ) )
        self.mess.disp_mess( (".modele" + self.modele_name ) ) 
        self.mess.disp_mess( ( ".maillage" +  self.maya_name ) )
        self.mess.disp_mess( ( ".nume" + self.nume_name ) )
        self.mess.disp_mess( ( ".mass" + self.mass_name ) ) 
        self.mess.disp_mess( ( ".kass" + self.kass_name ) )
        self.mess.disp_mess( ( " " ) )


    def get_modes(self):
        """!récupère les numéros et fréquences des modes d'un concept mode_meca"""
        __freq  = RECU_TABLE(CO=self.obj,
                             NOM_PARA='FREQ',);

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
        

        afreq  = __freq.EXTR_TABLE().Array('NUME_ORDRE','FREQ')
        axsi  = 1.0*zeros(afreq.shape)
        amor  = 1.0*zeros(afreq.shape)
        amodes= 1.0*zeros(afreq.shape)
        amass = 1.0*zeros(afreq.shape)
        arigi = 1.0*zeros(afreq.shape)
        tables = (__freq,__axsi,__mass,__modes,__amge,__raid)
        resu = [afreq, axsi, amass, amodes, amor, arigi]
        noms = ('FREQ','AMOR_REDUIT','MASS_GENE','NUME_MODE','AMOR_GENE','RIGI_GENE')
        for ind in range(len(resu)):
            try:
                table = tables[ind]
                resu[ind] = table.EXTR_TABLE().Array('NUME_ORDRE',noms[ind])
            except TypeError :
                self.mess.disp_mess("!! il manque le paramètre modal " +noms[ind]+ "     !!")
                self.mess.disp_mess("!! pour le resultat " +self.nom+ "              !!") 
                self.mess.disp_mess("!! les calculs risquent d'etre faux  ou incomplets      !!")
                self.mess.disp_mess("  ")
        return resu

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
        self.nume = None
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
            if self.matr_inte_spec == None:
                self.extr_inte_spec()
            self.intsp = 1
            
        except KeyError:
            # Cas où la table_sdaster n'est pas une tabl_intsp
            pass
            

    def make_inte_spec(self, titre, paras_out):
        """
        Fabrique un objet aster de type table inter-spectre :
         - Creation de n(n+1)/2 fonctions, où n = dim(matrice interspectrale)
         - Creation d'une table
        """
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
        if isinstance(self.resu,Resultat):
            ddl = self.nume_ddl_phy(self.resu)
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
        



    def link_model(self, resu):
        """!Lie une matrice interspectrale à un sd_resultat
        
        Lorsqu'on valide le choix des données d'entrée dans l'onglet turbulent,
        on appelle cette méthode qui lie les lignes et colonnes de la matrice
        inter-spectrale à une sd résultat
        """
        self.resu = resu # TODO : il serait plus logique de ne lier les inter
                         # spectres qu'à un modèle. Pas forcément besoin de nu
                         # mérotation, juste maillage + ddl mesurés de chaque
                         # noeud (à modifier éventuellement avec ON)

    def nume_ddl_phy(self, resu):
        """
        Fabrication d'une numérotation associée au modèle.
        Cas particulier : pour l'instant, on se place dans le cas où seuls les
        ddl DY (et éventuellement DRZ) sont utilisés.
        Retourne un vecteur dont les comp sont de la forme : #noeud + type ddl
        var_opt est le type de projection que l'on veut réaliser : efforts discrets
        (var_opt = 'Efforts discrets localisés' ou 'Efforts et moments discrets')
        """
        self.nume = []
        __CHANO = CREA_CHAMP( TYPE_CHAM = 'NOEU_DEPL_R',
                              OPERATION  = 'EXTR',
                              RESULTAT   = resu.obj,
                              NOM_CHAM   = 'DEPL',
                              NUME_ORDRE = 1,
                             );

        if self.opt == 0:
            chano_y  = __CHANO.EXTR_COMP('DX',[],1)
            DETRUIRE(CONCEPT=_F(NOM=(__CHANO,),),INFO=1,)
            for no in chano_y.noeud:
                self.nume.append('N'+str(no)+'_DX')
            return self.nume
    
        if self.opt == 1:
            chano_y  = __CHANO.EXTR_COMP('DX',[],1)
            chano_rz = __CHANO.EXTR_COMP('DRZ',[],1)
            DETRUIRE(CONCEPT=_F(NOM=(__CHANO,),),INFO=1,) 
            for no in chano_y.noeud:
                self.nume.append('N'+str(no)+'_DX')
                self.nume.append('N'+str(no)+'_DRZ')
            return self.nume

    def nume_ddl_gene(self, resu):
        """
        Crée le meme vecteur de numérotation avec une numérotation par modes.
        Retourne "MO"+#mode
        """
        self.modes = []
        afreq, axsi, amass, amodes, amor, arigi = resu.get_modes() # recup des cara modales du resultat associé à l'interspectre
        nb_mod = amodes.shape[0]
        for mod in amodes[:,1]:
            self.modes.append('MO'+str(int(mod)))
        return self.modes


    def extr_inte_spec(self):
        """!Extraction d'une matrice inter-spectrale à partir d'une tabl_insp"""
        self.mess.disp_mess("Extraction de l'inter-spectre " + self.nom)
        tabl_py = self.obj.EXTR_TABLE()
        nom_fonc= tabl_py['FONCTION_C'].values()['FONCTION_C']
        nb_mes  = tabl_py['DIMENSION'].values()['DIMENSION'][0]
        nb_freq = len(self.f)
        self.matr_inte_spec = Numeric.zeros((nb_freq, nb_mes, nb_mes),
                                             Numeric.Complex)

        try:
            # Methode de recherche rapide des fonctions
            ctx = CONTEXT.get_current_step().jdc.sds_dict
            fonc_py = [ ctx[fonc].convert('complex') for fonc in nom_fonc ]
        except KeyError:
            fonc_py = []
            start_c = 0
            ind_fonc = 0
            nb_fonc = nb_mes*(nb_mes+1)/2
            for ind_fonc in range(nb_fonc):
                __FONC = RECU_FONCTION(TABLE = self.obj,
                                        NOM_PARA_TABL = 'FONCTION_C',
                                        FILTRE = _F(NOM_PARA='FONCTION_C',VALE_K=nom_fonc[ind_fonc])
                                        )
                fonc_py.append(__FONC.convert('complex'))
                DETRUIRE(CONCEPT=_F(NOM=__FONC),INFO=1)
                ind_fonc = ind_fonc + 1
                    
##        for ind_freq in range(nb_freq):
##            start_c = 0
##            ind_fonc = 0
##            for ind_l in range(nb_mes):
##                for ind_c in range(start_c, nb_mes):
##                    self.matr_inte_spec[ind_freq,ind_l,ind_c] = fonc_py[ind_fonc].vale_y[ind_freq]
##                    ind_fonc = ind_fonc+1
##                start_c = start_c+1
##            self.matr_inte_spec[ind_freq,:,:] = 0.5*(self.matr_inte_spec[ind_freq,:,:] +
##                                                     Numeric.conjugate(Numeric.transpose(self.matr_inte_spec[ind_freq,:,:])))


        for ind_freq in range(nb_freq):
            ind_fonc = 0
            for ind_c in range(nb_mes):
                for ind_l in range(ind_c+1):
                    self.matr_inte_spec[ind_freq,ind_l,ind_c] = fonc_py[ind_fonc].vale_y[ind_freq]
                    ind_fonc = ind_fonc+1
            self.matr_inte_spec[ind_freq,:,:] = 0.5*(self.matr_inte_spec[ind_freq,:,:] +
                                                     Numeric.conjugate(Numeric.transpose(self.matr_inte_spec[ind_freq,:,:])))


    def extr_freq(self):
        """Extraction des fréquences d'étude dans la tabl_intsp qui contient
        les inter-spectres mesurés"""
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
        self.resultats = {}
        self.masses = {}
        self.maillage_modeles = {}
        self.cara_elem = {}
        self.cham_mater = {}
        self.inter_spec = {}
        self.macro = macro
        self.recup_objects()


    def recup_objects( self ):
        jdc = CONTEXT.get_current_step().jdc
        
        for i, v in jdc.sds_dict.items():
            if isinstance( v, modele_sdaster ):
                self.modeles[i] = v
            elif isinstance( v, mode_meca ):
                self.resultats[i] = Resultat(i,v,self.mess)
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


        #self.debug()
        self.link_objects()


    def new_objects( self ):
        """! Récupération des nouveaux concepts, s'il y en a, lorsqu'on
             change d'onglet. Utilse uniquempent pour les concepts
             Resultat et InterSpectre"""
        jdc = CONTEXT.get_current_step().jdc
        
        for i, v in jdc.sds_dict.items():
            if isinstance( v, mode_meca ):
                if not self.resultats.has_key(i):
                    self.resultats[i] = Resultat(i,v,self.mess)
                    self.link_objects()
            elif isinstance( v, table_sdaster ):
                if not self.inter_spec.has_key(i):
                    self.inter_spec[i] = InterSpectre(nom = i, obj_ast = v, mess = self.mess)


    def debug(self):
        self.mess.disp_mess( ( "Modeles" + self.modeles ) )
        self.mess.disp_mess( ("Maillages" + self.maillages ) )
        self.mess.disp_mess( ("Masses" + self.masses ) )
        self.mess.disp_mess( ("Resultats" ) )
        self.mess.disp_mess( ( " " ) ) 
        for v in self.resultats.values():
            v.show()

    def link_objects(self):
        """!essaye de relier les concepts entre eux"""

        # recuperation des maillages associes aux modeles
        for m, _mod in self.modeles.items():
            _maillag = aster.getvectjev( m.ljust(8) + '.MODELE    .NOMA        ' )
            maillage = _maillag[0].strip()
            # Cherche le(s) resultat(s) qui a ce maillage
            for n, res in self.resultats.items():
                if res.maya_name == maillage:
                    assert res.modele == None or res.modele is _mod
                    res.modele = _mod
                    res.modele_name = m
            self.maillage_modeles[m] = maillage
            assert maillage in self.maillages


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
