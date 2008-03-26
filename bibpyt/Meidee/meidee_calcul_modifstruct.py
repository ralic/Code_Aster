#@ MODIF meidee_calcul_modifstruct Meidee  DATE 26/03/2008   AUTEUR BODEL C.BODEL 
# -*- coding: iso-8859-1 -*-
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

## \package meidee_mac Module de gestion des calculs %Meidee avec Aster
#
# La classe ModifStruct permet de gerer les calculs de modification structurale
# 
import aster
from Accas import _F

# XXX import done only for mode_meca, should it be removed?
import Cata.cata
from Cata.cata import MAC_MODES, MODE_ITER_INV, DEPL_INTERNE
from Cata.cata import MODE_STATIQUE, DEFI_BASE_MODALE, PROJ_MESU_MODAL
from Cata.cata import MACR_ELEM_STAT, DETRUIRE, NUME_DDL_GENE 
from Cata.cata import PROJ_MATR_BASE, MODE_ITER_SIMULT, REST_BASE_PHYS
from Cata.cata import EXTR_MODE, DEFI_MAILLAGE, ASSE_MAILLAGE, AFFE_MODELE
from Cata.cata import NUME_DDL, CALC_MATR_ELEM, AFFE_CARA_ELEM, CREA_MAILLAGE
from Cata.cata import PROJ_CHAMP, ASSE_MATRICE

from Meidee.meidee_cata import Resultat
from Meidee.meidee_calcul_correlation import extract_mac_array
from Meidee.modes import CopyModelMeca, convert_args


class CreateModeleCouple(CopyModelMeca):

    def reinit(self, modl, mail1, mail2):
        CopyModelMeca.reinit( self, modl )
        self.mail1 = mail1
        self.mail2 = mail2

    def create_modele(self, affe):
        """Creation du modele"""
        if self.modele:
            print "DESTRUCTION", self.modele.nom
        self.clear_concept( self.modele )
        # Concept de sortie, ne pas changer de nom sans changer le DeclareOut
        _MDLCPL = AFFE_MODELE( MAILLAGE=self.maillage,
                                AFFE=affe,
                                AFFE_SOUS_STRUC=_F( SUPER_MAILLE = 'SUMAIL',
                                                    PHENOMENE='MECANIQUE',),
                                )
        self.modele = _MDLCPL


    def create_maillage(self):
        """Creation du maillage"""
        self.clear_concept( self.maillage )
        # Concept de sortie, ne pas changer de nom sans changer le DeclareOut
        _MLCPL = ASSE_MAILLAGE( MAILLAGE_1=self.mail1,
                                 MAILLAGE_2=self.mail2,
                                 OPERATION='SOUS_STR' )
        self.maillage = _MLCPL
        self.concepts[self.mail1.nom] = _MLCPL


    def nume_ddl(self, args_lst):
        if len(args_lst)!=1:
            raise RuntimeError("Plusieurs NUME_DDL, presents, "\
                               "on ne peut en avoir qu'un")
        args, sd = args_lst[0]
        for nume in self.nume_lst:
            self.clear_concept( nume )
        self.nume_lst = []
        args = convert_args( args, self.concepts )
        _NUMECPL = NUME_DDL(**args)
        self.concepts[sd.nom] = _NUMECPL
        self.nume_lst.append( _NUMECPL )

    def asse_matrice(self, args_lst):
        # cleanup:
        for c in self.mat_rigi+self.mat_mass+self.mat_amor:
            self.clear_concept( c )
        self.mat_rigi = []
        self.mat_mass = []
        self.mat_amor = []
        for args, sd in args_lst:
            mat_elem = args['MATR_ELEM']
            typ_elem = self.matr_elem.get( mat_elem.nom, None )
            args = convert_args( args, self.concepts )
            if typ_elem=='RIGI_MECA':
                _RIGIMEC = ASSE_MATRICE(**args)
                self.mat_rigi.append( _RIGIMEC )
                self.concepts[sd.nom] = _RIGIMEC
            elif typ_elem=='MASS_MECA':
                _MASSMEC = ASSE_MATRICE(**args)
                self.mat_mass.append( _MASSMEC )
                self.concepts[sd.nom] = _MASSMEC
            elif typ_elem=='AMOR_MECA':
                _AMORMEC = ASSE_MATRICE(**args)
                self.mat_amor.append( _AMORMEC )
                self.concepts[sd.nom] = _AMORMEC


class ModifStruct:
    """!Classe qui s'occupe des calculs de modification structurale

    """
    def __init__(self, macro, meidee_objects, mess, outputs):
        """!Constructeur

        \param macro le self de l'objet macro provenant de macro_visu_meidee_ops
       
        :meidee_objects; objects de la memoire JEVEUX
        :parap mess: ecrit les erreurs dans un fichier.
       
        :param outputs: concepts Aster de l'utilisateur a afficher en sortie.
        """
        self.macro = macro
        self.meidee_objects = meidee_objects
        self.mess = mess

        self.modes_exp = {}
        self.base_proj = {}

        self.resu_exp = None
        self.support_modele = None
        self.matr_rig = None
        self.kassup = None
        self.method_name = None
        
        self.captor_groups = None
        self.ms_externe_groups = None
        
        self.modes_ide = None
        self.modes_expansion = None
        
        self.mailx = None
        self.modlx = None
        
        self.support_modele_res = None
        self.matr_masse = None
        self.cara_elem = None
        self.cham_mater = None

        self.calculated_modes = None

        self.cpl = CreateModeleCouple()
        self.x_modst = None  # pour ES
        self.x_bsmo = None   # pour ES
        self.x_numgen = None # pour LMME
        self.x_kproj = None  # pour LMME
        self.x_mproj = None  # pour LMME
        self.x_modgen = None
        self.x_resgen = None
        self.x_proj = None   # proj_mesu_modal (mesure/support)
        self.x_mide = None   # selection des modes mesures
        self.x_ssexp = None
        self.x_mailcond = None
        self.x_mailcpl = None
        self.x_modlcpl = None
        self.x_cara = None
        self.x_chmat = None
        self.base_expansion = None # x_resgen ou x_bsmo
        self.x_mexp = None # base d'expansion

        # resultats d'operations sur le modele/maillage interface
        self.x_mailint = None  # maillage
        self.x_modlint = None  # modele
        self.x_caraint = None  # cara_elem
        self.x_kelint = None   # kelem
        self.x_numint = None   # nume
        self.x_modstint = None # modes statiques
        self.x_baseint = None  # base modale sur l'interface
        self.x_projmsint = None
        self.x_deplpr = None
        self.x_deplint = None
        self.x_deplxint = None

        # modele couplage
        self.modes_couple = None  # modes calcules sur le modele couple
        self.modes_retr = None    # modes du modele couple sur la super maille

        self.mac_int = None # resultat de MAC_MODE

        self.dyh_retro = None



        step = CONTEXT.get_current_step()
        if outputs:
            if outputs['MODELE']:
                step.DeclareOut( "_MDLCPL", outputs['MODELE'] )
            if outputs['MAILLAGE']:
                step.DeclareOut( "_MLCPL", outputs['MAILLAGE'] )
            if outputs['MODE_MECA']:
                step.DeclareOut( "_MODCPL", outputs['MODE_MECA'] )
            if outputs['NUME_DDL']:
                step.DeclareOut( "_NUMECPL", outputs['NUME_DDL'] )
            if outputs['MASS_MECA']:
                step.DeclareOut( "_MASSMEC", outputs['MASS_MECA'] )
            if outputs['RIGI_MECA']:
                step.DeclareOut( "_RIGIMEC", outputs['RIGI_MECA'] )
            if outputs['AMOR_MECA']:
                step.DeclareOut( "_AMORMEC", outputs['AMOR_MECA'] )
            if outputs['MACR_ELEM']:
                step.DeclareOut( "_SSEXP", outputs['MACR_ELEM'] )
            if outputs['PROJ_MESU']:
                step.DeclareOut( "_PROJ", outputs['PROJ_MESU'] )

    def find_experimental_result_from(self, modes_exp_name):
        """Trouve le modele experimental dans la memoire JEVEUX."""
        self.resu_exp = self.meidee_objects.get_resu(modes_exp_name)
    
    def find_support_modele_from(self, supmodl_name):
        """Trouve le modele du support dans la memoire JEVEUX"""
        self.support_modele = self.meidee_objects.get_model(supmodl_name)
    
    def set_stiffness_matrix(self, stiffness_matrix):
        """Place la matrice de raideur et trouve celle correspondant
        au module Meidee dans la memoire JEVEUX."""
        self.matr_rig = stiffness_matrix
        self.kassup = self.meidee_objects.get_matr(stiffness_matrix.nom)

    def set_method_name(self, method_name):
        """Place le nom de methode pour calculer la base: 'ES' ou 'LMME'"""
        self.method_name = method_name

    def set_captor_groups(self, captor_groups):
        """Place les groupes de capteurs utilises, avec leur nom
        et degres de liberte."""
        self.captor_groups = captor_groups

    def set_ms_externe_groups(self, ms_externe_groups):
        """Place les groupes de la structure externe, avec leur nom
        et degres de liberte."""
        self.ms_externe_groups = ms_externe_groups
    
    def set_modes_ide(self, modes_ide):
        """Place les modes mesures sur la structure a utiliser pour le
        calcul."""
        self.modes_ide = modes_ide
            
    def set_modes_expansion(self, modes_expansion):
        """Place les modes d'expansion a utiliser pour le calcul.
        Ils peuvent etre proposes a partir de la selection des groupes
        'captor' et 'ms_externe'."""
        self.modes_expansion = modes_expansion
    
    def find_maillage_couple_from(self, maillc_name):
        """Trouve le maillage de couplage dans la memoire JEVEUX"""
        self.mailx = self.meidee_objects.maillages[maillc_name]
        
    def find_modele_couple_from(self, modc_name):
        """Trouve le modele du couplage dans la memoire JEVEUX"""
        self.modlx = self.meidee_objects.modeles[modc_name] 
    
    def set_coupling_method_name(self, cm_name):
        """Place le nom de la methode pour calculer les modes couples"""
        self.coupling_method_name = cm_name

    def clear_concept(self, cpt):
        """!Detruit un concept silencieusement"""
        if cpt is None:
            return
        DETRUIRE(CONCEPT=_F(NOM=cpt),ALARME='NON',INFO=1)
        
    def get_modes_exp(self):
        """!Recuperation des frequences mesure

        """
        self.modes_exp=self.resu_exp.get_modes()
        return self.modes_exp

    def get_mode_meca_modele_support(self):
        """Recherche dans les resultats des objects Meidee le modele mecanique
        du support. Remarque: ce resultat contient des modes mecaniques
        bidons. Une fois le modele trouve, la matrice de masse, ainsi que
        la caracteristique des elements et le champ de materiaux sont
        conserves en references.
        """
        for name, resu in self.meidee_objects.resultats.items():
            modele_name = aster.getvectjev(name.ljust(19) + '.MODL')
            if not modele_name:
                print "Modele non trouve pour:", name.strip()
                continue
            
            modllu = modele_name[0].strip()
            print "SD_RESU", name, "->", modllu
            
            if modllu == self.support_modele.nom:
                support_modele = resu.obj
                refd = support_modele.REFD.get()
                nom_raideur = refd[0].strip()
            
                if nom_raideur == self.matr_rig.nom:
                    print "FOUND:", name
                    nom_masse = refd[1].strip()
                    matr_masse = self.meidee_objects.get_matr(nom_masse)
                    
                    caraelem = aster.getvectjev(name.ljust(19)+'.CARA')
                    var_carelem = caraelem[0].strip()
            
                    champmat = aster.getvectjev(name.ljust(19)+'.MATE')
                    var_chmat = champmat[0].strip()
                    break
        else:
            self.mess.disp_mess( "Impossible de trouver le mode meca calcule " \
                                 "du modele support" )
        
        self.support_modele_res = support_modele
        self.matr_masse = matr_masse

        # Caracteristique de l'element et champ de materiaux
        # Utiliser pendant la condensation avec MACR_ELEM_STAT
        self.cara_elem = self.meidee_objects.get_cara_elem_obj(var_carelem)
        self.cham_mater = self.meidee_objects.get_cham_mater_obj(var_chmat)
    
    def _get_grp_captors_tot(self):
        """Retourne un groupe de capteurs en fonction de la methode.
        Pour ES on garde uniquement les DDL capteurs, pour LMME on utilise aussi
        les DDL interface."""
        # XXX: pour LMME est-ce qu'on passe tout pour calc_base_es ??
        if self.method_name == "ES":
            grpno_tot = self.captor_groups
        else:
            grpno_tot = self.captor_groups + self.ms_externe_groups
        
        grno = []
        for grp in grpno_tot:
            grno.append( _F(GROUP_NO=grp["GROUP_NO"], AVEC_CMP=grp["NOM_CMP"]) )
        return grno 

    def calc_base_es(self, opt_grnocapt=None):
        """!Calcul de la base de projection : par expansion statique
        grnocapt : le mot-clef facteur passe a FORCE_NODALE.
        """
        
        grnocapt = opt_grnocapt or self._get_grp_captors_tot()
        
        self.clear_concept(self.x_modst)
        _MODST = MODE_STATIQUE( MATR_RIGI = self.matr_rig,
                                FORCE_NODALE = grnocapt,)
        self.x_modst = _MODST

        r = Resultat(None, _MODST.nom, _MODST, self.mess, owned=False)
        nume_modes,base_proj = r.get_modes_stat()

        self.clear_concept(self.x_bsmo)
        _BSMO = DEFI_BASE_MODALE( RITZ = (
                _F(MODE_MECA = self.support_modele_res, NMAX_MODE = 0,),
                _F(MODE_STAT = _MODST, NMAX_MODE = len(nume_modes),),
                                        ),
                                  NUME_REF = r.nume,)

        self.x_bsmo = _BSMO
        base_mod_es = Resultat(None, _BSMO.nom, _BSMO, self.mess, owned=False)
        nume_modes_sup,base_proj = base_mod_es.get_modes_stat()
        self.base_expansion = _BSMO

        return nume_modes_sup, base_proj, _BSMO

    def calc_base_lmme(self, basemo, calc_freq=None):
        """!Calcul de la base de projection type Mathieu Corus
        """
        
        self.clear_concept(self.x_numgen)
        _NUMGEN = NUME_DDL_GENE( BASE = basemo,
                                 STOCKAGE = 'PLEIN',)
        self.x_numgen = _NUMGEN

        self.clear_concept(self.x_kproj)
        _KPROJ = PROJ_MATR_BASE( BASE = basemo,
                                 NUME_DDL_GENE = _NUMGEN,
                                 MATR_ASSE = self.matr_rig,);
        self.x_kproj = _KPROJ

        self.clear_concept(self.x_mproj)
        _MPROJ = PROJ_MATR_BASE( BASE = basemo,
                                 NUME_DDL_GENE = _NUMGEN,
                                 MATR_ASSE = self.matr_masse,)
        self.x_mproj = _MPROJ

        self.clear_concept(self.x_modgen)

        base_mod_es = Resultat(None, basemo.nom, basemo, self.mess, owned=False)
        nume_modes_sup,base_proj=base_mod_es.get_modes_stat()

        if calc_freq is None:
            calc_freq = { 'OPTION':'PLUS_PETITE',
                          'NMAX_FREQ':len(nume_modes_sup),
                          'SEUIL_FREQ':1.E-4 }

        if calc_freq['NMAX_FREQ']==-1:
            calc_freq['NMAX_FREQ'] = len(nume_modes_sup)
            
        _MODGEN = MODE_ITER_SIMULT( MATR_A = _KPROJ,
                                    MATR_B = _MPROJ,
                                    VERI_MODE = _F(SEUIL = 1.E-05,
                                                   STOP_ERREUR = 'OUI',),
                                    CALC_FREQ = calc_freq)
        self.x_modgen=_MODGEN

        self.clear_concept(self.x_resgen)
        _RESGEN = REST_BASE_PHYS( RESU_GENE = _MODGEN,
                                  TOUT_ORDRE = 'OUI',
                                  NOM_CHAM ='DEPL')
        self.x_resgen = _RESGEN


        self.base_mod_lmme = Resultat(None, _RESGEN.nom, _RESGEN,
                                      self.mess, owned=False)
        
        modes = self.base_mod_lmme.get_modes()
        self.nume_modes_sup, bid1, bid2, self.base_proj, bid3, bid4 = modes 

        self.base_expansion = _RESGEN
        
        return self.nume_modes_sup,self.base_proj,_RESGEN


    def condensation(self, nomcham = 'DEPL'):
        """Calcul la condensation des modes sur la structure modifiee."""
        modmesu = self.resu_exp
        modlexp = modmesu.modele
        modlsup = self.support_modele
    
        modes_mesure_retenus = self.modes_ide
        modes_expansion_retenus = self.modes_expansion
        
        noeuds_interface = [grp["GROUP_NO"] for grp in self.ms_externe_groups]
        
        # Reduction des modes mesures
        self.clear_concept( self.x_mide )
        _MIDE = EXTR_MODE( FILTRE_MODE = _F( MODE = modmesu.obj,
                                             NUME_MODE = modes_mesure_retenus,
                                        ),)
        self.x_mide = _MIDE
        # cas LMME, une fonction existe pour calculer la base d'expansion
        if isinstance( self.base_expansion, Cata.cata.mode_meca):
            self.clear_concept( self.x_mexp )
            _MEXP = EXTR_MODE( FILTRE_MODE = _F( MODE = self.base_expansion,
                                                 NUME_MODE = modes_expansion_retenus))
            self.x_mexp = _MEXP
        # cas ES, il faut re-calculer la base a partir des noeuds choisis
        else:
            self.mess.disp_mess("Recalcul de la base modale par ES")
            nodes = {}
            for num in modes_expansion_retenus:
                node,comp = self.base_proj[num-1].split()
                nodes.setdefault( node, [] ).append( comp )
            grno = []
            for node, comps in nodes.items():
                grno.append( _F( NOEUD = node, AVEC_CMP = comps) )
            self.calc_base_es(grno)
            _MEXP = self.base_expansion
        self.clear_concept( self.x_proj )
        
        try:
            _PROJ = PROJ_MESU_MODAL( MODELE_CALCUL = _F( MODELE = modlsup.obj,
                                                         BASE = _MEXP,),
                                     MODELE_MESURE = _F( MODELE = modlexp.obj,
                                                         MESURE = _MIDE,
                                                         NOM_CHAM = nomcham,),
                                     RESOLUTION = _F( METHODE = 'SVD',
                                                      EPS = 1.E-5),
                                   );
        except Exception, err:
            self.mess.disp_mess("Condensation de la mesure: " \
                                "Erreur dans PROJ_MESU_MODAL")
            self.mess.disp_mess(str(err))
            return
        self.x_proj = _PROJ
        
        # Condensation de la mesure sur les DDL INTERFACES
        self.clear_concept( self.x_ssexp )
        try:
            _SSEXP = MACR_ELEM_STAT( DEFINITION = _F( MODELE = modlsup.obj,
                                                      PROJ_MESU = _PROJ,
                                                      MODE_MESURE = _MIDE,
                                                      CARA_ELEM = self.cara_elem,
                                                      CHAM_MATER = self.cham_mater,
                                                    ),
                                      EXTERIEUR = _F(GROUP_NO = noeuds_interface,),
                                      RIGI_MECA = _F(),
                                      MASS_MECA = _F(),
                                     )
        except Exception, err:
            self.mess.disp_mess("Condensation de la mesure: " \
                                "Erreur dans MACR_ELEM_STAT")
            self.mess.disp_mess(str(err))
            self.mess.disp_mess("OK")
            return
        self.x_ssexp = _SSEXP

        self.clear_concept( self.x_mailcond )
        _MAILCD = DEFI_MAILLAGE( DEFI_SUPER_MAILLE = _F( MACR_ELEM = _SSEXP,
                                                         SUPER_MAILLE = 'SUMAIL',),
                                 DEFI_NOEUD = _F( TOUT = 'OUI',
                                                  INDEX = (1,0,1,8,)) # XXX
                                  )

        self.x_mailcond = _MAILCD

        # XXX mailcond maillage de la modification??

    def modele_couple(self):
        """Creation du modele couple"""
        print "CREATION MODELE COUPLE"
        self.cpl.reinit(self.modlx, self.mailx, self.x_mailcond )
        
        try:
            self.cpl.copy()
        except Exception, err:
            self.mess.disp_mess("Une erreur est survenue lors " \
                                "de la duplication du modele couplage")
            self.mess.disp_mess(str(err))
            raise
            return

        if not self.cpl.mat_rigi:
            self.mess.disp_mess("Le modele couple n'a pas de matrice " \
                                "de rigidite assemblee")
        if not self.cpl.mat_mass:
            self.mess.disp_mess("Le modele couple n'a pas de matrice " \
                                "de masse assemblee")
        if not self.cpl.mat_amor:
            self.mess.disp_mess("Le modele couple n'a pas de matrice " \
                                "d'amortissement assemblee")
        # ...


    def maillage_iface(self, group_ma_sup):
        """Construit le maillage de l'interface."""
        print "CREATION MAILLAGE INTERFACE"
        self.clear_concept(self.x_mailint)
        self.group_ma_sup = group_ma_sup
        self.group_ma_int = group_ma = 'IFACE'

        __MAIL = CREA_MAILLAGE( MAILLAGE=self.mailx,
                                CREA_POI1 = _F( GROUP_MA = self.group_ma_sup,
                                                NOM_GROUP_MA = self.group_ma_int, ))
        self.x_mailint = __MAIL
        self.clear_concept( self.x_modlint )
        
        __MODL=AFFE_MODELE(MAILLAGE=__MAIL,
                           AFFE=_F(GROUP_MA=group_ma,
                                   PHENOMENE='MECANIQUE',
                                   MODELISATION='DIS_T',),)
        
        self.x_modlint = __MODL
        
        self.clear_concept( self.x_caraint )
        __CARA=AFFE_CARA_ELEM(MODELE=__MODL,
                              DISCRET=_F(GROUP_MA=group_ma,
                                         REPERE='GLOBAL',
                                         CARA='K_T_D_N',
                                         VALE=(1e+12,1e+12,1e+12,),
                                         ),);
        self.x_caraint = __CARA
        self.clear_concept( self.x_kelint )
        __KEL=CALC_MATR_ELEM(OPTION='RIGI_MECA',
                             MODELE=__MODL,
                             CARA_ELEM=__CARA,
                             );
        self.x_kelint = __KEL
        self.clear_concept( self.x_numint )
        __NUM=NUME_DDL( MATR_RIGI=__KEL,);
        self.x_numint = __NUM


    def indicateur_choix_base_projection(self):
        """Expansion statique du champ de deplacements aux interfaces"""
        self.clear_concept( self.x_modstint )
        __MODST=MODE_STATIQUE(MATR_RIGI=self.kassup,
                              FORCE_NODALE=self._get_force_nodale())
        self.x_modstint = __MODST

        r=Resultat(None, __MODST.nom, __MODST, self.mess, owned=False)
        nume_modes,base_proj=r.get_modes_stat()


        self.clear_concept( self.x_baseint )
        __BASINT=DEFI_BASE_MODALE(RITZ=( _F(MODE_MECA=self.support_modele_res,
                                            NMAX_MODE=0,),
                                         _F(MODE_STAT=__MODST,
                                            NMAX_MODE=len(nume_modes),),
                                         ),
                                  NUME_REF=self.nume_support_modele,);
        self.x_baseint = __BASINT

        self.clear_concept( self.x_projmsint )
        __PROJMS=PROJ_MESU_MODAL(MODELE_CALCUL=_F(MODELE=self.support_modele.obj,
                                                BASE=__BASINT,),
                               MODELE_MESURE=_F(MODELE=self.resu_exp.modele.obj,
                                                MESURE=self.modes_retr,
                                                NOM_CHAM='DEPL',),
                               RESOLUTION=_F(METHODE='SVD',
                                             EPS=1.E-5),
                               );
        self.x_projmsint = __PROJMS

        self.clear_concept( self.x_deplpr )
        __DEPLPR=REST_BASE_PHYS(RESU_GENE=__PROJMS,
                              TOUT_ORDRE='OUI',
                              NOM_CHAM   ='DEPL');
        self.x_deplpr = __DEPLPR

        self.clear_concept( self.x_deplint )
        __DEPINT=PROJ_CHAMP(METHODE='ELEM',
                            RESULTAT=__DEPLPR,
                            MODELE_1=self.support_modele.obj,
                            MODELE_2=self.x_modlint,
                            NOM_CHAM='DEPL',
                            TOUT_ORDRE='OUI',
                            NUME_DDL=self.x_numint,
                            VIS_A_VIS=_F(GROUP_MA_1=self.group_ma_sup,
                                         GROUP_MA_2=self.group_ma_int,),
                            );
        self.x_deplint = __DEPINT

        self.clear_concept( self.x_deplxint )
        # CHAMP DE DEPL AUX INTERFACES SUR LE MODELE COUPLE
        __DPXINT=PROJ_CHAMP(METHODE='ELEM',
                            RESULTAT=self.modes_couple,
                            MODELE_1=self.cpl.modele,
                            MODELE_2=self.x_modlint,
                            NOM_CHAM='DEPL',
                            TOUT_ORDRE='OUI',
                            NUME_DDL=self.x_numint,
                            VIS_A_VIS=_F(GROUP_MA_1=self.group_ma_sup,
                                         GROUP_MA_2=self.group_ma_int,),
                            );
        self.x_deplxint = __DPXINT
        # INDICATEUR DE PROXIMITE DES MODES
        # LA BASE DE PROJECTION EST CORRECT SI DEPLINT = DEPLXINT
        # LES MODES SONT PROCHES SI LES TERMES DIAG DU MAC PROCHE DE 1
        self.clear_concept( self.mac_int )
        __MACINT=MAC_MODES(BASE_1=__DEPINT,
                       BASE_2=__DPXINT,
                       INFO  =2,
                      );
        self.mac_int = __MACINT
        self.mac_val = extract_mac_array( __MACINT )

    def modes_modele_couple(self, mode_simult, calc_freq):
        # ne traite qu'une seule masse/raideur
        cpl = self.cpl
        kcouple = cpl.mat_rigi[0]
        mcouple = cpl.mat_mass[0]
        acouple = cpl.mat_amor[0]
        
        self.clear_concept( self.modes_couple )
        if mode_simult :
            _MODCPL = MODE_ITER_SIMULT( MATR_A = kcouple,
                                        MATR_B = mcouple,
                                        VERI_MODE = _F( SEUIL = 1.E-05,
                                                        STOP_ERREUR = 'OUI',),
                                        CALC_FREQ = calc_freq,);
        else :
            _MODCPL = MODE_ITER_INV( MATR_A = kcouple,
                                     MATR_B = mcouple,
                                     CALC_FREQ = calc_freq,
                                    );
        self.modes_couple = _MODCPL

        # RETROPROJECTION SUR LE MODELE EXPERIMENTAL (INTERFACE -> DDL MESURE)
        self.clear_concept( self.modes_retr )
        # XXX Should not we return the DEPL_INTERNE in the DeclareOut?
        __MDRETR=DEPL_INTERNE(DEPL_GLOBAL=_MODCPL,SUPER_MAILLE='SUMAIL')
        self.modes_retr = __MDRETR

    def retroprojection(self, mode_simult, calc_freq, interv, chargement):
        # Ca vire...
        I_DEBUT, I_FIN, I_NOMBRE = interv
        __LIFREQ=DEFI_LIST_REEL(DEBUT=I_DEBUT,
                                INTERVALLE=_F(JUSQU_A = I_FIN,
                                              NOMBRE = I_NOMBRE))

        __DYH=DYNA_LINE_HARM( MODELE=cpl.modele,
                              MATR_MASS=mcouple,
                              MATR_RIGI=kcouple,
                              MATR_AMOR=acouple,
                              LIST_FREQ=__LIFREQ,
                              TOUT_CHAM='OUI',
                              EXCIT=(_F(COEF_MULT_C=('RI',1.,0.),
                                        CHARGE=chargement,),),
                            )
        self.clear_concept( self.dyh_retro )
        __DYHRTR=DEPL_INTERNE(DEPL_GLOBAL=__DYH,SUPER_MAILLE='SUMAIL')
        self.dyh_retro = __DYHRTR

    def calc_base_proj(self, calc_freq=None):
        """Calcule la base d'expansion en fonction de la methode, ES ou LMME"""
        num_sup, base_proj, x_bsmo = self.calc_base_es()
        
        # Conserve la base initiale (pour retrouver les noeuds a filter)
        self.base_proj = base_proj 
        
        # Expansion statique projetee
        if self.method_name == "LMME": 
            nume_modes_sup, base_proj, x_resgen = self.calc_base_lmme(x_bsmo,
                                                                      calc_freq)
            self.calculated_modes = [ (n, '%8.2f Hz' % (f,) ) 
                                      for n, f in nume_modes_sup ]
        else:
            self.calculated_modes = [ (n, '%d %12s' % (n, t)) 
                                      for n, t in zip(num_sup, base_proj) ]
            
    def calcul_mesure_support_corresp(self):                 
        """Demarre le calcul Meidee sur la structure modifiee."""
        
        self.get_mode_meca_modele_support()
        self.calc_base_proj()

    def _can_get_nume_support_model(self):
        """Retourne True si la numerotation du modele du support est
        retrouvee. Remorque, c'est le modele provenant des resulats 
        des objects Meidee."""
        numesup = Resultat(None, self.support_modele_res.nom,
                           self.support_modele_res,
                           self.mess, owned=False).nume
        if not numesup:
            self.mess.disp_mess("Impossible de retrouver la numerotation " \
                                "utilisee pour le modele support")
            return False
        self.nume_support_modele = numesup
        return True

    def _get_force_nodale(self):
        """Renvoit la force nodale pour la macro: MODE_STATIQUE"""
        force_nodale = []
        for grp in self.captor_groups:
            force_nodale.append( _F(GROUP_NO=grp["GROUP_NO"],
                                    AVEC_CMP=grp["NOM_CMP"]) )
        return force_nodale

    def calcul_condensation(self):
        if not self._can_get_nume_support_model():
            return

        self.condensation()
        self.modele_couple()
        self.maillage_iface(group_ma_sup='VISUAL')
        
    def calcul_coupling_model_modes(self, mode_simult, calc_freq):
        print "PROJECTION MODES INTERFACE"
        if "NMAX_FREQ" in calc_freq:
            if calc_freq['NMAX_FREQ'] <= 0:
                calc_freq['NMAX_FREQ'] == 10 # valeur par defaut
        
        print "CALC FREQ", repr(calc_freq), type(calc_freq), \
                           calc_freq.__class__, dir(calc_freq)

        self.modes_modele_couple(mode_simult, calc_freq)

        self.indicateur_choix_base_projection()


    

