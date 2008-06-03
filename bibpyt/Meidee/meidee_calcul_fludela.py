#@ MODIF meidee_calcul_fludela Meidee  DATE 03/06/2008   AUTEUR BODEL C.BODEL 

#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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


import string
import Numeric

import aster
from Accas import _F
from Meidee.meidee_cata import Resultat, CaraElem, ChampMateriau, InterSpectre
from Meidee.meidee_cata import MeideeObjects, CreaTable
from Utilitai.Utmess import UTMESS

####################
#                  #
# CLASSE DE CALCUL #
#                  #
####################

class MeideeFludela:
    """!Classe qui gere les calculs de massamor/fludela

    On utilise cette classe pour tous les calculs concernant
    Meidee / fludela
    
    On commence par fournir les parametres du calcul avec les
    methodes set_fimen, set_res_longeq, set_results, set_param_phy,
    set_mode_pairs et set_speed

    on appelle ensuite la methode compute pour lancer le calcul et
    on recupere l'ensemble des resultats dans le dictionnaire
    self.calc

    register_massam, et register_longeq permettent aux interfaces graphiques
    de specifier des fonctions de rappel a appeler apres chaque etape du calcul
    de facon a pouvoir mettre a jour l'interface des qu'un calcul est effectue.

    Comme la methode compute verifie la validite de ses parametres avant de lancer
    chaque etape de calcul, on peut ainsi voir le debut du calcul (la longueur equivalente
    par exemple) meme si on n'a pas specifie de resultats experimentaux pour les ecoulements.
    """

    def __init__(self, mess, out):
        """!Constructeur

        Le constructeur initialise les attributs suivants:
          - RES_longeq: le sd_resultat utilise pour le calcul de la longueur equivalente
          - diam: diametre du tube
          - rho_flu_int: masse volumique du fluide interne
          - rho_flu_ext: masse volumique du fluide externe
          - res_air: resultat en air
          - res_eau: resultat en eau
          - res_longeq: sd_resultat utilise pour le calcul de la longueur equivalente
          - long_eq: longueur equivalente calculee
          - obs_longeq : interface a prevenir en cas de changement de la LEQ
          - obs_massam : interfaces a prevenir en cas de recalcul de massamor
          - output: (TEMP) fichier utilise pour la sauvegarde des resultats
        """
        # La fenetre de messages, les fichiers de sortie
        self.mess = mess
        self.out = out

        # Normalement un resultat etendu
        self.RES_longeq = None

        # Donnes physiques sur le tube, donnees en entree
        # D, rho_flu. On les donne par defaut, pour l'instant  :
        # diametre du tube
        self.diam_ext = 0.05
        self.diam_int = 0.048
        # fluide externe
        self.rho_flu_int = 1000.0
        self.rho_flu_ext = 1000.0
        self.res_longeq = None
        self.res_air = None
        self.res_eau = None
        self.res_vit = None
        self.long_eq = 0
        self.pairs = None # paires de modes (air,eau)
        self.calc = {} # resultats du calcul

        self.methodes_globales = ['Granger', 'Moyenne']
        self.methode_globale = self.methodes_globales[0]
        self.obs_longeq = []
        self.obs_massam = []
        self.fimen = None
        self.fimen_name = ""
        self.indref = 0
        self.saved_data = {} # dico dans lequel on enregistre toutes les donnees sauvegardees

    def register_longeq(self, cb ):
        """!Ajoute un callback a appeler en cas de recalcul de la LEQ
        \param cb la fonctio de callback a appeler
        """
        self.obs_longeq.append( cb )

    def notify_longeq(self):
        """!Appelle les callback apres le recalcul de la LEQ"""
        for cb in self.obs_longeq:
            cb()

    def register_massam(self, cb ):
        """!Ajoute un callback a appeler en cas de reclacul par massamor
        \param cb la fonction de callback a appeler
        """
        self.obs_massam.append( cb )

    def notify_massam(self):
        """!Appelle les callback apres le recalcul par massamor"""
        for cb in self.obs_massam:
            cb()

    def save_data(self):
        """!Recupere une copie des donnees calculees"""
        return self.calc.copy()

        
    def save_to_file( self, vitesse, data ):
        """!Ecrit les donnees calculees dans le fichier UNITE_RESU"""
        self.mess.disp_mess("AIR "+self.res_air.nom+"\n")
        self.mess.disp_mess("EAU "+self.res_eau.nom+"\n")
        self.mess.disp_mess("FIMEN "+self.fimen_name+"\n")

        
        for k, arr in data.items():
            self.mess.disp_mess(k + " " )
            if isinstance(arr,list):
                for v in arr:
                    self.mess.disp_mess( str(v) + " " )
                self.mess.disp_mess( " " )
            else:
                self.mess.disp_mess( str(arr)+"\n" )
        self.mess.disp_mess( " " )


    def save_to_table(self, data, nom="__tmp"):
        """! Sauvegarde de tous les resultats sous forme d'une table aster
             de type table_fonction
             Les indices de la table sont les noms des parametre, et, associes,
             des fonctions avec en abscisse les vitesses et en ordonnee les valeurs"""
        from Cata.cata import DEFI_FONCTION
        l_fonc = []
        vitesses = data.keys()
        vitesses.sort()
        param = data[vitesses[0]].keys()
        nb_mod = len(data[vitesses[0]][param[0]])
        for para in param:
            for ind_mod in range(nb_mod):
                vale  = []
                for vit in vitesses:
                    vale.append(string.atof(vit))
                    vale.append(data[vit][para][ind_mod])
                _fonc = DEFI_FONCTION( NOM_PARA   = 'ABSC',
                                       VALE       = vale)
                l_fonc.append(_fonc.nom)

        ind_1 = []
        for para in param:
            for ind in range(nb_mod):
                ind_1.append(para)
        ind_1 = tuple(ind_1)
        ind_2 = tuple(range(1,nb_mod+1)*len(param))
        mcfact = []
        mcfact.append(_F(PARA='NOM_PARA',LISTE_K=ind_1, TYPE_K='K16'))
        mcfact.append(_F(PARA='NUME_MODE',LISTE_I=ind_2))
        mcfact.append(_F(PARA='FONCTION', LISTE_K=l_fonc))

        # On creee un table dont le nom Aster est objout(compteur)
        CreaTable(mcfact, nom, self.out, self.mess)
        


    def set_fimen(self, name ):
        """!Specifie le fichier fimen a utiliser pour la methode globale
           \param name Le nom du fichier fimen a utiliser
        """
        if name == None:
            self.mess.disp_mess("!!Donner le fichier fimen !!")
            return
        self.fimen = None
        self.fimen_name = name
        self.fimen = Fimen(self.mess, name)
        self.fimen.lec_fimen()
    
    def compute(self):
        """!Lance le calcul, appele les fonctions de notification de l'interface
        a chaque etape"""
        self.mess.disp_mess( (  "MeideeFludela : calcul des parametres modaux ajoutes" ) )
        self.mess.disp_mess( (  " " ) )
        if self.res_longeq is not None and (self.long_eq is None or self.long_eq==0):
            # calcul des longueurs equivalentes
            self.long_eq = None
            self.mess.disp_mess( (  "MeideeFludela : calcul de la longueur equivalente" ) ) 
            self.longueur_equiv()
            self.mess.disp_mess( ( " " ) ) 
            self.notify_longeq()

        
        if (self.res_air is None or
            self.res_eau is None or
            self.long_eq is None ):
            self.mess.disp_mess("!!   Pour effectuer le calcul, il faut avoir   !!")
            self.mess.disp_mess("!! un resultat en air, en eau et en ecoulement !!")
            self.mess.disp_mess("   ")

            return

        if self.pairs:  
            "MeideeFludela:Prep"
            self.prep_massamor()
            self.mess.disp_mess( (  "MeideeFludela : Calc" ) )
            self.calc_modes_pairs()
            self.mess.disp_mess( (  "MeideeFludela : Notify" ) ) 
            self.notify_massam()


    def set_res_longeq( self, res ):
        """!Specifie la sd_resu a utilisre pour le calcul de la longueur equivalente
        \param res resultat a utiliser
        """
        self.res_longeq = res
        self.long_eq = None

    def set_results( self, res_air, res_eau, res_vit ):
        """!Specifie le sd_resultat a utiliser pour calcul en air et eau et ecoulement"""
        self.res_eau = res_eau
        self.res_air = res_air
        self.res_vit = res_vit

    def set_param_phy( self, rho_int=None, rho_ext=None, diam_int=None, diam_ext=None ):
        """!Specifie les parametres physiques a utiliser
        \param diam diametre du tube
        \param rho_int masse volumique du fluide interne
        \param rho_ext masse volumique du fluide externe
        """
        if rho_int is not None:
            self.rho_flu_int = float(rho_int)
        if rho_ext is not None:
            self.rho_flu_ext = float(rho_ext)
        if diam_ext is not None:
            self.diam_int = float(diam_int)
        if diam_int is not None:
            self.diam_ext = float(diam_ext)
        # XXX: annule les resultats qui dependent d'un de ces param



    def longueur_equiv(self):
        """!Calcul de la longueur equivalente

        Calcul preliminaire : on utilise pour tous les cas (meme pour les
        calculs en ecoulement) la longueur equivalente calculee a partir des
        modes etendus en air. Elle sera utilisee pour les calculs suivants.
        On essaye de calculer: \f[ \int_{\textrm{longueur poutre?}}{\phi^2(x)dx} \f]
        """
        RES_et = self.res_longeq
        try:
            __tabl = calc_meidee_longeq( self.res_longeq )
        except TypeError:
            self.mess.disp_mess("Longueur equivalente non calculee"/
                                "Voir fichier de messages")
        self.long_eq = __tabl.EXTR_TABLE().Array('MODE', 'LONGUEUR')
        self.calc['longeq'] = self.long_eq
        self.mess.disp_mess("Longueurs equivalentes pour les modes de" + RES_et.nom)
        for line in self.long_eq.tolist():
            self.mess.disp_mess("%2d   %13.5E" %(line[0], line[1]))
        


    def mat_gene(self, resu):
        """!Fonction utilitaire qui extrait les matrices K,M,C,f d'un sd_resultat"""
        from Cata.cata import RECU_TABLE
        try:
            __freq=RECU_TABLE(CO=resu.obj,
                              NOM_PARA='FREQ',
                              );
            __kgene=RECU_TABLE(CO=resu.obj,
                               NOM_PARA='RIGI_GENE'
                               );
            __mgene=RECU_TABLE(CO=resu.obj,
                               NOM_PARA='MASS_GENE'
                               );
            __cgene=RECU_TABLE(CO=resu.obj,
                               NOM_PARA='AMOR_GENE'
                               );
            __xsi  =RECU_TABLE(CO=resu.obj,
                               NOM_PARA='AMOR_REDUIT'
                               );
        except:
            return tuple([0]*5)

        # On convertit les tables format Aster en Numerical Array
        ftmp = __freq.EXTR_TABLE().Array('NUME_ORDRE','FREQ')
        ktmp = __kgene.EXTR_TABLE().Array('NUME_ORDRE','RIGI_GENE')
        mtmp = __mgene.EXTR_TABLE().Array('NUME_ORDRE','MASS_GENE')
        ctmp = __cgene.EXTR_TABLE().Array('NUME_ORDRE','AMOR_GENE')
        xsi  = __cgene.EXTR_TABLE().Array('NUME_ORDRE','AMOR_REDUIT')

        # Fabrication des amor
        if sum(ctmp) == 0:
            ctmp = (2*Mpy*(2*Numeric.pi*fpy))*xsi
        if sum(xsi) == 0:
            xsi = Cpy / (2*Mpy*(2*Numeric.pi*fpy))
        # On convertit le vecteur en une matrice diagonale
        #nm, _x = ftmp.shape
        #idn = Numeric.identity(nm, Float )
        fpy=ftmp[:,1]  # frequences propres
        Kpy=ktmp[:,1]
        Mpy=mtmp[:,1]
        Cpy=ctmp[:,1]

        return fpy, Kpy, Mpy, Cpy, xsi


    def set_exp(self, suffix, fpy, xsi, mass, modes, Cpy, Kpy ):
        """!Methode utilitaire, pour sauvegarder les parametres d'une experience"""
        self.calc['freq_'+suffix] = fpy
        self.calc['mass_'+suffix] = mass
        self.calc['raid_'+suffix] = Kpy
        self.calc['amor_'+suffix] = Cpy
        self.calc['xsi_'+suffix] = xsi

    def prep_massamor(self):
        """!Prepare les infos pour le calcul"""
        obj_air = self.res_air
        obj_eau = self.res_eau
        obj_vit = self.res_vit
        nm = obj_air.get_modes()[1].shape[0]
        try:
            list_caras = tuple([list[:,1] for list in obj_air.get_modes()])
            self.set_exp( "a", *list_caras )
        except AttributeError:
            self.mess.disp_mess( (  "!! Choisir le resultat en air !!" ) )
            self.mess.disp_mess( ( " " ) ) 
        try:
            list_caras = tuple([list[:,1] for list in obj_eau.get_modes()])
            self.set_exp( "e", *list_caras )
        except AttributeError:
            self.mess.disp_mess( ( "!! Choisir le resultat en eau !!" ) )
            self.mess.disp_mess( ( " " ) )
        try:
            list_caras = tuple([list[:,1] for list in obj_vit.get_modes()])
            self.set_exp( "v", *list_caras )
        except AttributeError:
            self.mess.disp_mess( ( "!! Choisir le resultat en ecoulement !!" ) )
            self.mess.disp_mess( ( "  " ) ) 

        try:
            longeq = self.long_eq[:,1]
        except TypeError:
            longeq = Numeric.array([1.0]*nm)
            self.mess.disp_mess("!! Calculer une longueur equivalente  !!")
            self.mess.disp_mess("!! pour avoir des resultats lineiques !!")

        # xmi = masse modale ajoutee par le fluide interne
        xmi = self.rho_flu_int * longeq * Numeric.pi * (self.diam_int/2)**2

        self.calc['xmi'] = xmi
        self.calc['longeq'] = longeq



    def set_modes_pairs(self, modes):
        """!Specifie les appariement des modes (air, eau, ecoulement) pour le calcul fludela
        """
        # L'affichage des modes se fait a partir de 1, on compte en python a partir de 0
        # on retire donc 1 partout
        tmp = modes[:]
        for ind1 in range(len(modes)):
            for ind2 in range(len(modes[ind1])):
                for ind3 in range(len(modes[ind1][ind2])):
                    modes[ind1][ind2][ind3] = tmp[ind1][ind2][ind3] - 1
        self.pairs = modes

    def set_speed(self, U):
        """!Specifie la vitesse du fluide en ecoulement"""
        self.calc['U'] = U

    def calc_modes_pairs(self):
        """!Fonction qui dirige le calcul
        
         - Correspondance entre les modes selectionnes pour les trois experiences,
         - Envoie les donnees a massamor et fludela_monomod pour le calcul des coefficients
           fluide-elastiques
        """
        modes = self.pairs
        NM = len(modes)
        xmi = Numeric.zeros( (NM,), 'd' )
        xm = Numeric.zeros( (NM,), 'd' )
        freq_air = Numeric.zeros( (NM,), 'd' )
        freq_eau = Numeric.zeros( (NM,), 'd')
        freq_vit = Numeric.zeros( (NM,), 'd' )

        xsi_air = Numeric.zeros( (NM,), 'd' )
        xsi_eau = Numeric.zeros( (NM,), 'd' )
        xsi_vit = Numeric.zeros( (NM,), 'd' )
        long_eq = Numeric.zeros( (NM,), 'd' )

        self.mess.disp_mess( (  'modes = ' +  str(modes) ) )
        is_valid = 1 # passe a 0 si une des longueurs equiv est nulle
        
        for i, (m1, m2, m3) in enumerate(modes):
            fa, xmia, xma, xsia, leqa  = self.calc_freq_air( m1 )
            fe, xsie  = self.calc_freq_eau( m2 )
            fv, xsiv  = self.calc_freq_vit( m3 )

            freq_air[i] = fa
            xsi_air[i] = xsia
            xmi[i] = xmia
            xm[i] = xma
            long_eq[i] = leqa
            self.mess.disp_mess(str(leqa))
            if leqa < 1E-15:
                is_valid = 0 # si pour un mode, la longueur equiv est 0, calcul adim impossible
                self.mess.disp_mess("!!  Attention : une des composantes de la longueur   !!")
                self.mess.disp_mess("!! equivalente est nulle. Impossible de calculer les !!")
                self.mess.disp_mess("!!              valeur lineiques                     !!")
                                     
            freq_eau[i] = fe
            xsi_eau[i] = xsie

            freq_vit[i] = fv
            xsi_vit[i] = xsiv

        self.calc['freq_eq_a'] = freq_air 
        self.calc['freq_eq_e'] = freq_eau
        self.calc['freq_eq_v'] = freq_vit
        
        # Cas ou les frequences n'ont pas ete calculees ("####") ou lorsque le calcul est impossible ("NaN")
        if not freq_eau or freq_eau == 0.0:
            self.calc['freq_eq_e'] = "####"
        if freq_eau == -1.0:
            self.calc['freq_eq_e'] = "NaN"
        if not freq_vit or freq_vit == 0.0:
            self.calc['freq_eq_v'] = "####"
        if freq_vit == -1.0:
            self.calc['freq_eq_v'] = "NaN"
        self.massamor( xmi, xm,
                       freq_air, xsi_air,
                       freq_eau, xsi_eau,
                       long_eq, is_valid)

        mass_ajou = self.calc['mass_ajou_dim_mod']

        self.fludela_monomod( self.calc["U"],
                              xmi, xm, mass_ajou,
                              freq_air, xsi_air,
                              freq_eau, xsi_eau,
                              freq_vit, xsi_vit,
                              long_eq, is_valid)

    def calc_freq_air( self, modes ):
        assert len(modes)==1
        mode = modes[0]
        xmi = self.calc['xmi'][mode]
        xm = self.calc['mass_a'][mode]
        xsi_air = self.calc['xsi_a'][mode]
        long_eq = self.calc['longeq'][mode] # mL
        freq = self.calc['freq_a'][mode]
        
        return freq, xmi, xm, xsi_air, long_eq

    def calc_freq_eau( self, modes ):
        if len(modes)==1:
            mode = modes[0]
            freq = self.calc['freq_e'][mode]
            xsi = self.calc['xsi_e'][mode]
        else:
            meth = getattr(self, "methode_"+self.methode_globale, None )
            if meth:
                freq, xsi = meth( modes )
            else:
                raise RuntimeError("Methode de calcul inconnue (%s)" % self.methode_globale )
        return freq, xsi

    def calc_freq_vit( self, modes ):
        if len(modes)==1:
            mode = modes[0]
            freq = self.calc['freq_v'][mode]
            xsi = self.calc['xsi_v'][mode]
        else:
            meth = getattr(self, "methode_"+self.methode_globale, None )
            if meth:
                freq, xsi = meth( modes )
            else:
                raise RuntimeError("Methode de calcul inconnue (%s)" % self.methode_globale )
        return freq, xsi

    def methode_Granger( self, modes ):
        """Calcul freq equivalente par la methode de Granger"""
        self.mess.disp_mess( (  "Methode Granger " + str(self.fimen) ) ) 
        if not self.fimen:
            self.mess.disp_mess(" !! Impossible de calculer des frequences !!")
            self.mess.disp_mess(" !!   globales : donner le fichier fimen  !!")
            self.mess.disp_mess("             ")
            return 0.0, 0.0
        return self.fimen.global_granger( self.indref, modes )

    def methode_Moyenne( self, modes ):
        """Methode de test qui prend la moyenne des frequences"""
        freq = 0.
        xsi = 0.
        for m in modes:
            freq += self.calc['freq_e'][m]
            xsi += self.calc['xsi_e'][m]
        return freq/len(modes), xsi/len(modes)


    def massamor( self,
                  xmi, xm,
                  freq_air, xsi_air,
                  freq_eau, xsi_eau,
                  long_eq, is_valid):
        r"""!MASSAMOR
        
        Calcul des masses ajoutees entre un fichier en air et un fichier en eau au repos.

        \param freq_air tableaux des modes en air
        \param freq_eau tableaux des modes en eau. Sont deux tableaux de meme taille (nombre de modes).
        \param xmi la masse modale du fluide interne. Vaut \f$\rho*\int_{longueur(tube)}{\phi^2}\f$
        \param xm mass modale du fluide interne pour l'etude en air
          
        Utilise self.rho_int_flu qui est la masse volumique du fluide interne
        et self.diam_ext qui est le diametre du tube (pour les adimensionnalisations)

        On calcule la masse ajoutee generalisee \e xma par:
        \f[ xma = xm.( \frac{f_a^2}{f_e^2} -1 )-xmi \f]

        Que l'on divise par :
        \f[ \frac{1}{2}\rho_{flu_ext}D^2.xl \f]

        Pour obtenir la masse ajoutee adimensionnelle lineique

        On note :
        \f[ sqrt_{xm} = \sqrt{\frac{xm}{xm+xmi+xma}} \f]
        et :
        \f[ \omega_{eau} = \omega_{air}*sqrt_{xm} = 2\pi . f_a . \sqrt{\frac{xm}{xm+xmi+xma}} \f]

        Le calcul de l'amortissement ajoute modal dimensionnel se fait par:
        \f[ cf_0  =  2\omega_{air} ( sqrt_{xm} \xi_{eau}- \sqrt{xm.(xm+xmi)}\xi_{air} ) \f]
        \f[ cf_0  =  2\omega_{air} ( \sqrt{\frac{xm}{xm+xmi+xma}} \xi_{eau}- \sqrt{xm.(xm+xmi)}\xi_{air} ) \f]
        """

        # xmi = mass_mod_int
        # xm = mass_mod_air
        ## \var D diametre du tube
        D = self.diam_ext
        nb_mod = Numeric.size(long_eq,0)        

        ## \var xma masse ajoutee generalisee
        xma = xm*( freq_air**2 / freq_eau**2 - 1) - xmi


        mass_ajou_dim_mod  = xma
        if is_valid:
            mass_ajou_dim_lin  = (1.0/long_eq)*mass_ajou_dim_mod
            mass_ajou_adim_lin = 1.0/((1./2.)*self.rho_flu_ext*D*D*long_eq)*mass_ajou_dim_mod
        else:
            mass_ajou_dim_lin  = ['####']*nb_mod
            mass_ajou_adim_lin = ['####']*nb_mod

        omega_air = freq_air*(2*Numeric.pi)
        omega_eau = omega_air * Numeric.sqrt(xm*(xm + xmi + xma))
        
        cf0 = 2*omega_air* ( Numeric.sqrt(xm*(xm + xmi + xma)) * xsi_eau -
                             Numeric.sqrt(xm*(xm + xmi))*xsi_air)
        amor_ajou_rep_dim_mod = cf0

        if is_valid:
            amor_ajou_rep_dim_lin = cf0/long_eq
        else:
            amor_ajou_rep_dim_lin = ['####']*nb_mod
        xsi_rep_ajou = amor_ajou_rep_dim_mod/(2*omega_eau*(xm + xmi + xma))

        self.calc['mass_ajou_dim_mod'] = mass_ajou_dim_mod
        self.calc['mass_ajou_dim_lin'] = mass_ajou_dim_lin
        self.calc['mass_ajou_adim_lin'] = mass_ajou_adim_lin
        self.calc['amor_ajou_rep_dim_mod'] = amor_ajou_rep_dim_mod
        self.calc['amor_ajou_rep_dim_lin'] = amor_ajou_rep_dim_lin
        self.calc['xsi_rep_ajou'] = xsi_rep_ajou


    def fludela_monomod(self, U, xmi, xm, xma,
                        freq_air, xsi_air,
                        freq_eau, xsi_eau,
                        freq_vit, xsi_vit,
                        long_eq, is_valid):
        """!FLUDELA

        Calcul des raideurs ajoutees et amortissements ajoutes entre un essai en ecoulement et un essai en air.
        La reference prise est ici l'essai en air (la plus souvent utilisee), mais il serait interessant de
        developper une option permettant de calculer les coef par rapport a l'essai en eau au repos.

        \param U vitesse de l'ecoulement
        \param xmi la masse modale du fluide interne. Vaut \f$\rho*\int_{longueur(tube)}{\phi^2}\f$
        \param xm masse modale du fluide interne pour l'etude en air
        \param xma masse ajoutee dimensionnelle, modale calculee par massamor
        \param freq_air frequences des modes en air
        \param xsi_air  amortissement global en air
        \param freq_eau frequences des modes en eau
        \param xsi_eau  amortissement global en eau
        \param freq_vit frequences des modes en ecoulement
        \param xsi_vit  amortissement global en ecoulement
        
        Donnees utilisees:
         - Un tableau contenant les vitesses de test du fluide
         - tableaux de frequences en air et en eau.
         - tableaux de masses modales en air et en eau (masse modale calculee avec massamor, en
           utilisant la meme normalisation des modes en air et en eau)
         - tableaux d'amortissements modaux en air et en eau.
        """
        D = self.diam_ext
        nb_mod = Numeric.size(long_eq,0)
        U = int(U)
        
        amor_ajou_vit_dim_mod  = Numeric.array(-2*xm*( xsi_air*(2*Numeric.pi)*freq_air -
                                               ( freq_air*freq_air/(freq_eau*freq_eau) ) *
                                               xsi_vit * (2*Numeric.pi) * freq_vit))
        if is_valid:
            amor_ajou_vit_dim_lin  = 1./(long_eq)*amor_ajou_vit_dim_mod
            ##amor_ajou_vit_adim_lin = (1./2)*self.rho_flu_ext*D*U*long_eq*amor_ajou_vit_dim_mod
            amor_ajou_vit_adim_lin = 1./((1./2)*self.rho_flu_ext*D*U*long_eq)*amor_ajou_vit_dim_mod
        else:
            amor_ajou_vit_dim_lin  = ['####']*nb_mod
            amor_ajou_vit_adim_lin = ['####']*nb_mod
        xsi_vit_ajou = 1./(2.*(xm+xmi+xma)*(2*Numeric.pi*freq_eau))*amor_ajou_vit_dim_mod

        mass_mod = xm # masse modale en air
        raid_ajou_dim_mod = mass_mod*(2*Numeric.pi*freq_air)*(2*Numeric.pi*freq_air)*((freq_vit*freq_vit)/(freq_eau*freq_eau) - 1)
        if is_valid:
            raid_ajou_dim_lin  = (1./long_eq)*raid_ajou_dim_mod 
            raid_ajou_adim_lin = 1./((1./2)*self.rho_flu_ext*U*U*long_eq)*raid_ajou_dim_mod
        else:
            raid_ajou_dim_lin  = ['####']*nb_mod
            raid_ajou_adim_lin = ['####']*nb_mod
        # Calcul de l'impedance ajoutee m.s^2 + c.s + k : a faire ou pas ?

        self.calc['raid_ajou_dim_mod'] = raid_ajou_dim_mod
        self.calc['raid_ajou_dim_lin'] = raid_ajou_dim_lin
        self.calc['raid_ajou_adim_lin'] = raid_ajou_adim_lin
        self.calc['amor_ajou_vit_dim_mod'] = amor_ajou_vit_dim_mod
        self.calc['amor_ajou_vit_dim_lin'] = amor_ajou_vit_dim_lin
        self.calc['amor_ajou_vit_adim_lin'] = amor_ajou_vit_adim_lin
        self.calc['xsi_vit_ajou'] = xsi_vit_ajou


#------------------------------------------------------------------------------

def calc_meidee_longeq( resultat,
                        *args ):

    """ Calcul de la longueur equivalente sur une poutre, a savoir
    int(phi^2(x).dc). On utilise le maillage associe au "resultat", pour
    avoir les coordonnes des noeuds. On classe les noeuds selon les y croissants
    """

    from Cata.cata import CREA_CHAMP, DETRUIRE, CREA_TABLE    
    __PHI=CREA_CHAMP(TYPE_CHAM='NOEU_DEPL_R',
                     NUME_ORDRE=1,
                     OPERATION='EXTR',
                     RESULTAT=resultat.obj,
                     NOM_CHAM='DEPL',
                  );

    chano_y = __PHI.EXTR_COMP('DY',[],1)
    numno = list(chano_y.noeud) # Les NUMEROS des noeuds du modele
    maya = resultat.maya
    coordos = maya.COORDO.VALE.get()

    coordo_x = [coordos[ind] for ind in range(0,len(coordos),3)]
    coordo_y = [coordos[ind] for ind in range(1,len(coordos),3)]
    coordo_z = [coordos[ind] for ind in range(2,len(coordos),3)]
    tab = []
    for ind_noe in numno:
        tab.append([ind_noe,coordo_x[ind_noe-1],
                    coordo_y[ind_noe-1],coordo_z[ind_noe-1]])
    tab = Numeric.array(tab)
    ordo = Numeric.argsort(tab[:,2])

    tab2 = []
    for place in ordo:
        tab2.append(tab[place,:])
    tab2 = Numeric.array(tab2)

    ind_mod  = [ int(m) for m in resultat.get_modes()[3][:,0] ]
    long_eq = []
    for ind in ind_mod:
        __PHI=CREA_CHAMP(TYPE_CHAM='NOEU_DEPL_R',
                         NUME_ORDRE=ind,
                         OPERATION='EXTR',
                         RESULTAT=resultat.obj,
                         NOM_CHAM='DEPL',
                      );
        
        chano_x = __PHI.EXTR_COMP('DX',[],1)
        chano_y = __PHI.EXTR_COMP('DY',[],1)
        chano_z = __PHI.EXTR_COMP('DZ',[],1)
        toto = []
        for ind in range(len(chano_x.noeud)):
            toto.append([chano_x.noeud[ind],chano_x.valeurs[ind],
                        chano_y.valeurs[ind], chano_z.valeurs[ind]])
        toto = Numeric.array(toto)
        
        calc = 0
        for ind_no in range(1,tab2.shape[0]):
            dist = Numeric.sqrt((tab2[ind_no,1]-tab2[ind_no-1,1])**2 +
                    (tab2[ind_no,2]-tab2[ind_no-1,2])**2 +
                    (tab2[ind_no,3]-tab2[ind_no-1,3])**2 )
            try:
                ligne = list(chano_x.noeud).index(tab2[ind_no,0])
                lignem1 = list(chano_x.noeud).index(tab2[ind_no-1,0])
            except ValueError:
                UTMESS('A', 'MEIDEE0_2')
                raise TypeError
            for direct in ['x','y','z']:
                exec('depl_'+direct+' = chano_'+direct+'.valeurs[ligne]')
                exec('depl_'+direct+'m1 = chano_'+direct+'.valeurs[lignem1]')
                # methode des trapezes : integrale = som(0.5*(b+B)*dist
                exec('calc = calc + 0.5*(depl_'+direct+'**2+depl_'+direct+'m1**2)*dist')
                               
        long_eq.append(calc)
        DETRUIRE(CONCEPT=_F(NOM=__PHI),INFO=1)



    __RESTAB=CREA_TABLE(LISTE=(_F(PARA="MODE",
                                  LISTE_I=ind_mod,),
                               _F(PARA="LONGUEUR",
                                  LISTE_R=long_eq,)
                              ))
    return __RESTAB

        
