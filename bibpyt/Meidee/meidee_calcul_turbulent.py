#@ MODIF meidee_calcul_turbulent Meidee  DATE 11/05/2009   AUTEUR NISTOR I.NISTOR 

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


from Utilitai.Utmess import UTMESS
from Numeric import array, zeros, conjugate, identity
from Numeric import transpose, ones, arctan, pi, log

from Meidee.meidee_cata import Resultat, InterSpectre, nume_ddl_phy, nume_ddl_gene
import MLab
import aster
from Matrix import Matrix
from Accas import _F
from Cata.cata import CREA_CHAMP, PROJ_CHAMP, DETRUIRE, OBSERVATION


class CalculTurbulent:
    """!Classe de methodes dirigeant des operateurs Code_Aster,
        ou faisant le lien entre sd aster et instances de classes python"""

    def __init__(self,
                 meidee_objects,
                 mess):
        self.meidee_objects = meidee_objects
        self.mess = mess
        
        self.inter_spec = None
        self.type_intsp = None
        self.res_base = None
        self.res_obs = None
        self.res_com = None

        self.alpha = None
        self.eps = None
        self.mcoeff = 0.
        self.var_opt = 0.
    
        # Variables d'existence des matrices resultats
        self.is_Syy = 0
        self.is_Syy_R = 0
        self.is_SQQ = 0
        self.is_SQQ_R = 0
        self.is_Sff = 0
        self.is_Syy_S = 0
        self.is_vs = 0 # Ajout : valeurs singulieres de la matrice C.Phi.Zm1
    
    def set_interspectre(self, intsp):
        """Trouve l'inter-spectre de mesures en fonctionnement"""
        self.inter_spec = intsp

    def set_type_intsp(self, type_intsp):
        self.type_intsp = type_intsp

    def set_observabilite(self, obs):
        self.res_obs = obs

    def set_commandabilite(self, com):
        self.res_com = com

    def set_base(self, base):
        self.res_base = base

    def set_alpha(self, alpha):
        self.alpha = alpha

    def set_epsilon(self, epsilon):
        self.epsilon = epsilon

    def set_mcoeff(self, mcoeff):
        self.mcoeff = mcoeff

    def set_var_opt(self, var_opt):
        """Une variable pour calculer l'interspectre."""
        self.var_opt = var_opt

    def set_extraction_ddl_obs(self, ddls):
        """Pour chaque groupe de noeuds ou groupe de mailles du concept
        observe, on donne les ddl actifs a extraire
        [{'GROUP_NO':'name1','DDL_ACTIF':['DX','DY']},{'GROUP_NO':'name2,...
        """
        self.extraction_ddl_obs = ddls
    
    def set_extraction_ddl_com(self, ddls):
        """Pour chaque groupe de noeuds ou groupe de mailles du concept
        de commandabilite, on donne les ddl actifs a extraire
        """
        self.extraction_ddl_com = ddls

    def calculate_force(self):
        self.is_Syy = 0
        self.is_Syy_R = 0
        self.is_SQQ = 0
        self.is_SQQ_R = 0
        self.is_Sff = 0
        self.is_Syy_S = 0
        self.is_vs = 0 

        # Extraction des donnees Aster u format matrices python
        try:
            self.inter_spec.extr_inte_spec(self.res_obs,
                                           self.extraction_ddl_obs)
        except TypeError:
            self.mess.disp_mess("Probleme de coherence entre l'inter-spectre et le concept d'observabilite")
            UTMESS('A', 'MEIDEE0_4')
            return

        self.f = self.inter_spec.f # Frequences de discretisation

        # Matrice C_Phi.
        self.modes_red = self.res_obs.extr_matr(self.extraction_ddl_obs)
        # Matrice B_Phi
        self.modes_act = self.res_com.extr_matr(self.extraction_ddl_com)
        self.Syy = self.inter_spec  # Matrice inter-spectrale
        self.is_Syy = 1
        # On range tous les resultats intermediaires et definitifs
        # dans une classe CalculInverse
        # Les resultats sont des instances de la classe InterSpectre
        resultat = CalculInverse(self, self.res_base, self.Syy, self.type_intsp,
                                 self.modes_red, transpose(self.modes_act), self.mess)
        try:
            # Pour tous les calculs inverses, on desactive les fpe
            # NB : on ne devrait avoir a les desactiver que pour les
            # operations "sensibles", type SVD, mais en calibre 5, on observe
            # des plantages sur de simples multiplications.
            aster.matfpe(-1)
            self.SQQ, self.val_sing, self.regul = resultat.calc_SQQ()
            self.SQQ.nume_gene = nume_ddl_gene(self.res_base)
            self.is_SQQ = 1
            self.Syy_R = resultat.verif_Syy()
            self.is_Syy_R = 1
            self.Syy_R.set_model(self.res_obs)
            self.Syy_R.nume_phy = self.Syy.nume_phy
            self.Sff = resultat.calc_Sff()
            self.is_Sff = 1
            self.Sff.set_model(self.res_com)
            self.Sff.nume_phy, bid, bid = nume_ddl_phy(self.res_com,
                                                       self.extraction_ddl_com)
            self.SQQ_R = resultat.verif_SQQ()
            self.is_SQQ_R = 1
            self.SQQ_R.nume_gene = nume_ddl_gene(self.res_base)
            self.Syy_S = resultat.synthes_Syy()
            self.is_Syy_S = 1
            self.Syy_S.set_model(self.res_obs)
            self.Syy_S.nume_phy = self.Syy.nume_phy
            aster.matfpe(1)

        except TypeError:
            self.mess.disp_mess("Calcul inverse non complete")
            UTMESS('A', 'MEIDEE0_4')
            return



#-------------------------------------------------------------------------------

class CalculInverse:
    """ classe rassemblant tous les resultats intermediaires et definitifs
    calcules par methode inverse en python uniquement"""

    def __init__(self,
                 calcturb,
                 modele_mod,
                 Syy, type_intsp, CPhi, PhiT_B,
                 mess):
        
        # dimensions du probleme
        self.calcturb = calcturb
        self.nb_freq  = len(calcturb.f)               # nombre de pas de frequence
        self.nb_mod   = calcturb.modes_red.shape[1]   # nb modes
        self.nb_mes   = calcturb.modes_red.shape[0]   # nb de points de mesure
        self.nb_act   = calcturb.modes_act.shape[0]   # nb de points de discretisation
        self.f        = calcturb.f
        self.var_opt  = calcturb.var_opt

        self.mess = mess
        
        # matrices inter-spectrales de donnees et de resultats
        self.modele_mod = modele_mod
        self.res_exp = Syy.resu
        self.CPhi = CPhi
        self.PhiT_B = PhiT_B
        self.Syy = Syy.matr_inte_spec
        self.SQQ = zeros((self.nb_freq,self.nb_mod,self.nb_mod),'D')
        self.Syy_R = zeros((self.nb_freq,self.nb_mes,self.nb_mes),'D')
        self.val_sing = zeros((self.nb_mod,self.nb_freq),'D')        
        self.regul = zeros((self.nb_mod,self.nb_freq),'D')
        self.Syy_S = zeros((self.nb_freq,self.nb_mes,self.nb_mes),'D')
        self.SQQ_R = zeros((self.nb_freq,self.nb_mod,self.nb_mod),'D')
        self.Sff = zeros((self.nb_freq,self.nb_act,self.nb_act),'D')
        self.Z, self.Zm1 = self.calc_Z(type_intsp)
        

        # parametres de calcul
        self.alpha   = float(calcturb.alpha)
        self.epsilon = float(calcturb.epsilon)
        self.mcoeff  = float(calcturb.mcoeff)


    def calc_Z(self,type_intsp):
        """ Calcul de la matrice d'impedence"""
        Z = zeros((self.nb_freq,self.nb_mod,self.nb_mod),'D')
        Zm1 = zeros((self.nb_freq,self.nb_mod,self.nb_mod),'D')
        
        # exposant : selon que l'inter-spectre soit en depl, vite ou acce, on change l'exposant
        if type_intsp == 'DEPL':exp = 0
        elif type_intsp == 'VITE':exp = 1
        elif type_intsp == 'ACCE':exp = 2
        else: exp = 0
        freq_i, xsi_i, mass_i, modes_i, amor_i, rigi_i = self.modele_mod.get_modes()
        l_freq_i = array([i for i in freq_i[:,1]])       # frequences propres
        l_omega_i = 2*pi*l_freq_i                        # pulsations propres equivalentes
        l_xsi_i  = array([j for j in  xsi_i[:,1]])       # amorstissements reduits propres
        l_mass_i  = array([j for j in  mass_i[:,1]])     # masses modales
        for ind_freq in range(self.nb_freq):
            omega = 2*pi*self.f[ind_freq]
            for ind_mod in range(self.nb_mod):
                Zm1[ind_freq,ind_mod,ind_mod] = omega**exp/(l_mass_i[ind_mod]*complex(-omega**2 + l_omega_i[ind_mod]**2 ,
                                                                             2*omega*l_omega_i[ind_mod]*l_xsi_i[ind_mod]))
                if omega == 0 : omega = 2*pi*self.f[ind_freq+1] # eviter la division par 0
                Z[ind_freq,ind_mod,ind_mod] = 1/(omega**exp)*l_mass_i[ind_mod]*complex(-omega**2 + l_omega_i[ind_mod]**2 ,
                                                                        2*omega*l_omega_i[ind_mod]*l_xsi_i[ind_mod])

        self.l_omega_i = l_omega_i
        
        return Z, Zm1
        
    
    def calc_SQQ(self):
        """ Calcul de la matrice d'impedance, affichage des cara modales
        et  calcul des excitations modales : [SQQ(om)] = [Z].[Sqq].[Z]^H"""

        CPhi = Matrix(self.CPhi)
        for ind_freq in range(self.nb_freq):
            omega = 2*pi*self.f[ind_freq]
            Zm1 = Matrix(self.Zm1[ind_freq,:,:])
            try:
                CPhiZm1 = CPhi*Zm1
            except ValueError:
                # produits matriciels impossibles
                self.mess_err("SQQ",{"CPhi":str(shape(CPhi)),"d'impedance mecanique":str(shape(Zm1))})
                raise TypeError
##            aster.matfpe(-1)
            U,S,VH = MLab.svd(CPhiZm1)
##            aster.matfpe(1)
            Smax = max(S)
            U = Matrix(U)
            V = Matrix(conjugate(transpose(VH)))
            l = len(S)
            S = array(S)
            inv_S = zeros(S.shape,'D')
            self.val_sing[:,ind_freq] = S
            alpha = self.choix_alpha(self.alpha,VH,omega)
            for ind in range(l):
                if S[ind] > self.epsilon*Smax:
                    inv_S[ind] = S[ind]/(S[ind]**2+alpha[ind,0])
                else:
                    inv_S[ind] = 0
                self.regul[ind,ind_freq] = alpha[ind,0]
            inv_S = Matrix(array(inv_S)*identity(l))
            inv_CPhiZm1 = V*inv_S*conjugate(transpose(U))
            inv_CPhiZm1_H = conjugate(transpose(inv_CPhiZm1))

            Syy_f = Matrix(self.Syy[ind_freq,:,:])
            SQQ_f = inv_CPhiZm1*Syy_f*inv_CPhiZm1_H
            self.SQQ[ind_freq,:,:] = SQQ_f

        SQQ = InterSpectre(nom        = 'SQQ',
                           mat        = self.SQQ,
                           frequences = self.f,
                           var_opt    = self.var_opt,
                           mess       = self.mess)
        
        self.mess.disp_mess("Calcul de SQQ : efforts modaux")

        # Attention ! SQQ est une instance de Interspectre, self.val_sing est un simple array
        return SQQ, self.val_sing, self.regul


    def verif_Syy(self):
        """ Recalcule l'inter-spectre Syy a partir de SQQ pour verification"""

        CPhi = Matrix(self.CPhi)
        CPhi_H = conjugate(transpose(Matrix(CPhi)))
        for ind_freq in range(self.nb_freq):
            SQQ_f = Matrix(self.SQQ[ind_freq,:,:])
            Syy_R_f = CPhi*SQQ_f*CPhi_H
            self.Syy_R[ind_freq,:,:] = Syy_R_f
            
        Syy_R = InterSpectre(nom        = 'SQQ_R',
                             mat        = self.Syy_R,
                             frequences = self.f,
                             var_opt    = self.var_opt,
                             mess       = self.mess)

        self.mess.disp_mess("Calcul de Syy_R : efforts modaux reconstitues")
        
        return Syy_R     
            

    def calc_Sff(self):
        """ Calcul de l'inter-spectre des efforts physiques a partir des
        deplacements physiques : [Sqq(om)] = ([CPhi]+)[Syy(om)]([CPhi]+)^H"""

        PhiT_B = Matrix(self.PhiT_B)
##        aster.matfpe(-1)
        U,S,VH = MLab.svd(PhiT_B)
##        aster.matfpe(1)
        Smax = max(S)
        U = Matrix(U)
        V = Matrix(conjugate(transpose(VH)))
        l = len(S)
        inv_S = zeros(S.shape,'D')
        self.mess.disp_mess("Valeurs singulieres de la matrice de commande")
        val_sing = "  "
        for s in S:
            val_sing = val_sing + '%5.3Ee' %s + ' ; '
        self.mess.disp_mess(val_sing)
        for ind in range(l):
            if S[ind] > self.epsilon*Smax:
                inv_S[ind] = S[ind]/(S[ind]**2+self.alpha)
            else:
                inv_S[ind] = 0
        inv_S = Matrix(array(inv_S)*identity(l))
        inv_PhiT_B = V*inv_S*conjugate(transpose(U))
        inv_PhiT_B_H = conjugate(transpose(inv_PhiT_B))
        for ind_freq in range(self.nb_freq):
            SQQ_f = Matrix(self.SQQ[ind_freq,:,:])
            try:
                Sff_f = inv_PhiT_B*SQQ_f*inv_PhiT_B_H
            except ValueError:
                self.mess_err("Sff",{"PhiT_B":str(PhiT_B.shape),"SQQ":str(SQQ_f.shape)})
                raise TypeError
            self.Sff[ind_freq,:,:] = Sff_f
        
        Sff = InterSpectre(nom        = 'Sff',
                           mat        = self.Sff,
                           frequences = self.f,
                           var_opt    = self.var_opt,
                           mess       = self.mess)


        self.mess.disp_mess("Calcul de Sff : efforts physiques ")

        return Sff


    def verif_SQQ(self):
        """ Recalcule l'inter-spectre SQQ a partir de Sff, calcule un coefficient d'erreur"""

        PhiT_B = Matrix(self.PhiT_B)
        PhiT_B_H = conjugate(transpose(Matrix(PhiT_B)))
        for ind_freq in range(self.nb_freq):
            Sff_f = Matrix(self.Sff[ind_freq,:,:])
            SQQ_R_f = PhiT_B*Sff_f*PhiT_B_H
            self.SQQ_R[ind_freq,:,:] = SQQ_R_f
            
        SQQ_R = InterSpectre(nom        = 'SQQ_R',
                             mat        = self.SQQ_R,
                             frequences = self.f,
                             var_opt    = self.var_opt,
                             mess       = self.mess)

        self.mess.disp_mess("Calcul de SQQ_R : efforts modaux reconstitues")
        
        return SQQ_R # TODO : le critere d'erreur



    def synthes_Syy(self):
        """! Synthese de l'inter-spectre a partir des efforts physiques identifies"""

        CPhi = Matrix(self.CPhi)
        CPhi_H = conjugate(transpose(CPhi))
        for ind_freq in range(self.nb_freq):
            Zm1_f = Matrix(self.Zm1[ind_freq,:,:])
            Zm1_f_H = conjugate(transpose(Zm1_f))
            SQQ_R_f = Matrix(self.SQQ_R[ind_freq,:,:])
            Syy_S_f = CPhi*Zm1_f*SQQ_R_f*Zm1_f_H*CPhi_H
            self.Syy_S[ind_freq,:,:] = Syy_S_f
            
        Syy_S = InterSpectre(nom        = 'Syy_S',
                             mat        = self.Syy_S,
                             frequences = self.f,
                             var_opt    = self.var_opt,
                             mess       = self.mess)

        self.mess.disp_mess("Calcul des Syy_S : Synthese modale des deplacements")
        self.mess.disp_mess(" ")

        return Syy_S
    

    def choix_alpha(self, alpha,VH,omega):
        """
        Creation pour un pas de frequence donne (omega), en fonction des pulsation
        propres du modele (omdega_i), de la matrice V de la SVD, du parametre alpha
        et d'un puissance m a determiner, du vecteur de regularisation de
        Tikhonov
        """
        m = self.mcoeff
        vect = []
        omega_i = self.l_omega_i
        V = Matrix(conjugate(transpose(VH)))
        nb_mod = len(omega_i)
        one = array([[1]]*nb_mod)
        vect1 = V*one
        ind = 0
        
        for om in omega_i:
            if omega <= om or m == 0.0:
                vect.append([vect1[ind,0]*1.])
            else:
                vect.append([vect1[ind,0]*(omega-om)**m+1.])
            ind = ind+1
        vect = alpha*Matrix(vect)
        regul = VH*vect
        

        return regul

    def mess_err(self, res, args):
        """ Renvoie un message d'erreur si les prosuits de matrices ne se font pas bien"""
        self.mess.disp_mess("!! Problemes dans les produits de matrices   !!")
        self.mess.disp_mess("!! pour le calcul de " + res + "                     !!")
        for arg in args.items():
            self.mess.disp_mess("La matrice "+arg[0]+ " a pour taille "+arg[1])
        self.mess.disp_mess(" ")
        UTMESS('A','MEIDEE0_4')


