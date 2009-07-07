#@ MODIF meidee_calcul_modifstruct Meidee  DATE 06/07/2009   AUTEUR COURTOIS M.COURTOIS 
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
from Accas import _F, ASSD

import Cata.cata
from Cata.cata import MAC_MODES, MODE_ITER_INV, DEPL_INTERNE, AFFE_CHAR_MECA
from Cata.cata import MODE_STATIQUE, DEFI_BASE_MODALE, PROJ_MESU_MODAL
from Cata.cata import MACR_ELEM_STAT, DETRUIRE, NUME_DDL_GENE, AFFE_MATERIAU
from Cata.cata import PROJ_MATR_BASE, MODE_ITER_SIMULT, REST_GENE_PHYS
from Cata.cata import EXTR_MODE, DEFI_MAILLAGE, ASSE_MAILLAGE, AFFE_MODELE
from Cata.cata import NUME_DDL, CALC_MATR_ELEM, AFFE_CARA_ELEM, CREA_MAILLAGE
from Cata.cata import PROJ_CHAMP, ASSE_MATRICE, CO

from Meidee.meidee_cata import Resultat
from Meidee.meidee_calcul_correlation import extract_mac_array




class ModifStruct:
    """!Classe qui s'occupe des calculs de modification structurale

    """
    def __init__(self, macro, meidee_objects, mess, outputs):
        """!Constructeur

        \param macro le self de l'objet macro provenant de calc_essai_meidee_ops

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
        self.support_maillage = None
        self.matr_rig = None
        self.kassup = None
        self.method_name = None

        self.captor_groups = None
        self.ms_externe_groups = None

        self.modes_ide = None
        self.modes_expansion = None
        
        self.sumail = None
        self.mailx = None
        self.modlx = None

        self.support_modele_res = None
        self.matr_masse = None
        self.cara_elem = None
        self.cham_mater = None

        self.calculated_modes = None

        self.param_condens = None

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
        self.x_mailsstr = None  # maillage
        self.x_modlint = None  # modele
        self.x_caraint = None  # cara_elem
        self.x_kelint = None   # kelem
        self.x_melint = None   # melem
        self.x_numint = None   # nume
        self.x_kas = None   # kas
        self.x_mas = None   # mas
        self.x_ponder = None   # kas ou mas
        self.x_modstint = None # modes statiques
        self.x_baseint = None  # base modale sur l'interface
        self.x_projmsint = None
        self.x_deplpr = None
        self.x_deplint = None
        self.x_deplxint = None

        # modele couple
        self.modes_couple = None  # modes calcules sur le modele couple
        self.modes_retr = None    # modes du modele couple sur la super maille

        self.mac_int = None # resultat de MAC_MODE
        self.mac_val = None # resultat de MAC_MODE
        self.mac_ponder = None # ponderation pour MAC_MODES : RIGIDITE / MASSE
        self.mac_method = None # choix methode pour MAC_MODES

        self.dyh_retro = None



        step = CONTEXT.get_current_step()
        if outputs:
            if outputs['MODELE']:
                step.DeclareOut( "__MDLCPL", outputs['MODELE'] )
            if outputs['MAILLAGE']:
                step.DeclareOut( "__MLCPL", outputs['MAILLAGE'] )
            if outputs['MODE_MECA']:
                step.DeclareOut( "__MODCPL", outputs['MODE_MECA'] )
            if outputs['NUME_DDL']:
                step.DeclareOut( "__NUMCPL", outputs['NUME_DDL'] )
            if outputs['MASS_MECA']:
                step.DeclareOut( "__MASSME", outputs['MASS_MECA'] )
            if outputs['RIGI_MECA']:
                step.DeclareOut( "__RIGIME", outputs['RIGI_MECA'] )
            if outputs['AMOR_MECA']:
                step.DeclareOut( "__AMORME", outputs['AMOR_MECA'] )
            if outputs['MACR_ELEM']:
                step.DeclareOut( "__SSEXP", outputs['MACR_ELEM'] )
            if outputs['PROJ_MESU']:
                step.DeclareOut( "__PROJ", outputs['PROJ_MESU'] )
            if outputs['BASE_ES']:
                step.DeclareOut( "__BSMO", outputs['BASE_ES'] )
            if outputs['MODE_STA']:
                step.DeclareOut( "__MODST", outputs['MODE_STA'] )
            if outputs['BASE_LMME']:
                step.DeclareOut( "__MEXP", outputs['BASE_LMME'] )
    #        if outputs['CH_MAT']:
    #            step.DeclareOut( "__CHMAT", outputs['CH_MAT'] )
    #        if outputs['CH_CAR']:
    #            step.DeclareOut( "__CHCAR", outputs['CH_CAR'] )

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
    
    def find_maillage_modif_from(self, modc_name):
        """Trouve le maillage modif dans la memoire JEVEUX"""
        for modele_name, modele in self.meidee_objects.modeles.items():
            if modele_name == modc_name :
                mailx_name = modele.maya_name
                self.mailx = self.meidee_objects.maillages[mailx_name]

    def find_maillage_support_from(self, modc_name):
        """Trouve le maillage modif dans la memoire JEVEUX"""
        for modele_name, modele in self.meidee_objects.modeles.items():
            if modele_name == modc_name :
                mailx_name = modele.maya_name
                self.support_maillage = self.meidee_objects.maillages[mailx_name]

    def find_modele_couple_from(self, modc_name):
        """Trouve le modele du couplage dans la memoire JEVEUX"""
        self.modlx = self.meidee_objects.modeles[modc_name]

    def set_param_condens(self, param):
        """ parametres de PROJ_MESU_MODAL pour la condensation :
            {'methode':'SVD'ou'LU','eps':eps,'regul_meth':'NON' ou
             'TIK_RELA' ou 'NORM_MIN','regul':alpha}"""
        self.param_condens = param

    def set_coupling_method_name(self, cm_name):
        """Place le nom de la methode pour calculer les modes couples"""
        self.coupling_method_name = cm_name

    def set_sumail_name(self, sumail_name):
        """Place le nom de de la super maille """
        self.sumail = sumail_name

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

    def set_mac_method(self, mac_meth):
        """Place le nom de methode pour le calcul de MAC : 'MAC' ou 'IERI'"""
        self.mac_method = mac_meth

    def set_mac_ponder(self, mac_ponder):
        """Place le type de matrice de ponderation pour le MAC : 'RIGIDITE' ou 'MASSE'"""
        self.mac_ponder = mac_ponder

    def get_mode_meca_modele_support(self):
        """Recherche dans les resultats des objects Meidee le modele mecanique
        du support. Remarque: ce resultat contient des modes mecaniques
        bidons. Une fois le modele trouve, la matrice de masse, ainsi que
        la caracteristique des elements et le champ de materiaux sont
        conserves en references.
        """
        for name, resu in self.meidee_objects.resultats.items():
            if aster.jeveux_exists(name.ljust(19)+'.NOVA'):
                iret,ibid,modele_name = aster.dismoi('F','MODELE',name,'RESULTAT')
                modele_name=modele_name.rstrip()
                if modele_name[0:1] == "#" :  continue

            else :
                continue
            modllu = modele_name

            if modllu == self.support_modele.nom:
                support_modele = resu.obj
                refd = support_modele.REFD.get()
                nom_raideur = refd[0].strip()

                if nom_raideur == self.matr_rig.nom:
                    nom_masse = refd[1].strip()
                    matr_masse = self.meidee_objects.get_matr(nom_masse)

                    nume_name = refd[3].strip()
                    numesup = self.meidee_objects.nume_ddl[nume_name]

                    iret,ibid,var_carelem = aster.dismoi('F','CARA_ELEM',name,'RESULTAT')
                    var_carelem=var_carelem.strip()

                    iret,ibid,var_chmat = aster.dismoi('F','CHAM_MATER',name,'RESULTAT')
                    var_chmat=var_chmat.strip()
                    break
        else:
            self.mess.disp_mess( "Impossible de trouver le mode meca calcule " \
                                 "du modele support" )

            return
        
        self.support_modele_res = support_modele
        self.nume_support_modele = numesup
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
            grno.append( _F(GROUP_NO=grp["NOM"], AVEC_CMP=grp["NOM_CMP"]) )
        return grno

    def calc_base_es(self, opt_grnocapt=None):
        """!Calcul de la base de projection : par expansion statique
        grnocapt : le mot-clef facteur passe a FORCE_NODALE.
        """

        grnocapt = opt_grnocapt or self._get_grp_captors_tot()

        self.clear_concept(self.x_modst)
        __MODST = MODE_STATIQUE( MATR_RIGI = self.matr_rig,
                                FORCE_NODALE = grnocapt,)
        self.x_modst = __MODST

        r = Resultat(None, __MODST.nom, __MODST, self.mess, owned=False)
        nume_modes,base_proj = r.get_modes_stat()

        self.clear_concept(self.x_bsmo)
        __BSMO = DEFI_BASE_MODALE( RITZ = (
                _F(MODE_MECA = self.support_modele_res, NMAX_MODE = 0,),
                _F(MODE_STAT = __MODST, NMAX_MODE = len(nume_modes),),
                                        ),
                                  NUME_REF = self.nume_support_modele,)

        self.x_bsmo = __BSMO
        base_mod_es = Resultat(None, __BSMO.nom, __BSMO, self.mess, owned=False)
        nume_modes_sup,base_proj = base_mod_es.get_modes_stat()
        self.base_expansion = __BSMO

        return nume_modes_sup, base_proj, __BSMO

    def calc_base_lmme(self, basemo, calc_freq=None):
        """!Calcul de la base de projection type Mathieu Corus
        """

        self.clear_concept(self.x_numgen)
        __NUMGEN = NUME_DDL_GENE( BASE = basemo,
                                 STOCKAGE = 'PLEIN',)
        self.x_numgen = __NUMGEN

        self.clear_concept(self.x_kproj)
        __KPROJ = PROJ_MATR_BASE( BASE = basemo,
                                 NUME_DDL_GENE = __NUMGEN,
                                 MATR_ASSE = self.matr_rig,);
        self.x_kproj = __KPROJ

        self.clear_concept(self.x_mproj)
        __MPROJ = PROJ_MATR_BASE( BASE = basemo,
                                 NUME_DDL_GENE = __NUMGEN,
                                 MATR_ASSE = self.matr_masse,)
        self.x_mproj = __MPROJ

        self.clear_concept(self.x_modgen)

        base_mod_es = Resultat(None, basemo.nom, basemo, self.mess, owned=False)
        nume_modes_sup,base_proj=base_mod_es.get_modes_stat()

        if calc_freq is None:
            calc_freq = { 'OPTION':'PLUS_PETITE',
                          'NMAX_FREQ':len(nume_modes_sup),
                          'SEUIL_FREQ':1.E-4 }


        if calc_freq.has_key('NMAX_FREQ'):
            if calc_freq['NMAX_FREQ']==-1:
                calc_freq['NMAX_FREQ'] = len(nume_modes_sup)

            
        __MODGEN = MODE_ITER_SIMULT( MATR_A = __KPROJ,
                                    MATR_B = __MPROJ,
                                    VERI_MODE = _F(SEUIL = 1.E-05,
                                                   STOP_ERREUR = 'OUI',),
                                    CALC_FREQ = calc_freq)
        self.x_modgen=__MODGEN

        self.clear_concept(self.x_resgen)
        __RESGEN = REST_GENE_PHYS( RESU_GENE = __MODGEN,
                                  TOUT_ORDRE = 'OUI',
                                  NOM_CHAM ='DEPL')
        self.x_resgen = __RESGEN


        self.base_mod_lmme = Resultat(None, __RESGEN.nom, __RESGEN,
                                      self.mess, owned=False)

        modes = self.base_mod_lmme.get_modes()
        self.nume_modes_sup, bid1, bid2, self.base_proj, bid3, bid4 = modes 

        self.base_expansion = __RESGEN
        
        return self.nume_modes_sup,self.base_proj,__RESGEN


    def condensation(self, resolution = None, nomcham = 'DEPL'):
        """Calcul la condensation des modes sur la structure modifiee."""
        modmesu = self.resu_exp
        modlexp = modmesu.modele
        modlsup = self.support_modele

        modes_mesure_retenus = self.modes_ide
        modes_expansion_retenus = self.modes_expansion

        noeuds_interface = [grp["NOM"] for grp in self.ms_externe_groups]

        # Reduction des modes mesures
        self.clear_concept( self.x_mide )
        __MIDE = EXTR_MODE( FILTRE_MODE = _F( MODE = modmesu.obj,
                                             NUME_MODE = modes_mesure_retenus,
                                        ),)
        self.x_mide = __MIDE
        name = obj_get_name(self.x_mide)
        self.meidee_objects.update(name, self.x_mide )

        self.clear_concept( self.x_mexp )
        # cas LMME, une fonction existe pour calculer la base d'expansion
        refd_base=self.base_expansion.REFD.get()

        if self.method_name == "LMME":
            __MEXP = EXTR_MODE( FILTRE_MODE = _F( MODE = self.base_expansion,
                                                 NUME_MODE = modes_expansion_retenus))
            self.x_mexp = __MEXP

        # cas ES, il faut re-calculer la base a partir des noeuds choisis
        else:
            # self.mess.disp_mess("Recalcul de la base modale par ES")
            nodes = {}
            for num in modes_expansion_retenus:
                node,comp = self.base_proj[num-1].split()
                nodes.setdefault( node, [] ).append( comp )
            grno = []
            for node, comps in nodes.items():
                grno.append( _F( NOEUD = node, AVEC_CMP = comps) )
            self.calc_base_es(grno)
            __MEXP = self.base_expansion
        self.x_mexp = __MEXP
        self.clear_concept( self.x_proj )
        try:
            __PROJ = PROJ_MESU_MODAL( MODELE_CALCUL = _F( MODELE = modlsup.obj,
                                                         BASE = __MEXP,),
                                     MODELE_MESURE = _F( MODELE = modlexp.obj,
                                                         MESURE = self.x_mide,
                                                         NOM_CHAM = nomcham,),
                                     RESOLUTION = self.param_condens
                                   );
        except aster.error:
            self.mess.disp_mess("Condensation de la mesure: " \
                                "Erreur dans PROJ_MESU_MODAL")
            self.mess.disp_mess(str(err))
            return
        self.x_proj = __PROJ
        
        # Condensation de la mesure sur les DDL INTERFACES
        self.clear_concept( self.x_ssexp )
        try:
            __SSEXP = MACR_ELEM_STAT( DEFINITION = _F( MODELE = modlsup.obj,
                                                      PROJ_MESU = __PROJ,
                                                      MODE_MESURE = __MIDE,
                                                      CARA_ELEM = self.cara_elem,
                                                      CHAM_MATER = self.cham_mater,
                                                    ),
                                      EXTERIEUR = _F(GROUP_NO = noeuds_interface,),
                                      RIGI_MECA = _F(),
                                      MASS_MECA = _F(),
                                      AMOR_MECA = _F(),
                                     )
        except Exception, err:
            self.mess.disp_mess("Condensation de la mesure: " \
                                "Erreur dans MACR_ELEM_STAT")
            self.mess.disp_mess(str(err))
            self.mess.disp_mess("OK")
            return
        self.x_ssexp = __SSEXP

        self.clear_concept( self.x_mailcond )
        __MAILCD = DEFI_MAILLAGE( DEFI_SUPER_MAILLE = _F( MACR_ELEM = __SSEXP,
                                                         SUPER_MAILLE = self.sumail,),
                                 DEFI_NOEUD = _F( TOUT = 'OUI',
                                                  INDEX = (1,0,1,8,)) # XXX
                                  )

        self.x_mailcond = __MAILCD

    def modele_couple(self):
        """Creation du modele couple"""
        self.cpl.reinit(self.modlx, self.mailx, self.x_mailcond, self.sumail )
        
        try:
            self.cpl.copy()
        except Exception, err:
            self.mess.disp_mess("Une erreur est survenue lors " \
                                "de la creation du modele couple")
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


    def maillage_iface(self):
        """Construit le maillage de l'interface."""
        self.clear_concept(self.x_mailint)
        self.group_ma_int = ['IFACE']

        group_no_ext = []
        for grp in self.ms_externe_groups:
            group_no_ext.append( grp["NOM"])

        if not self.support_maillage :
            self.find_maillage_support_from(self.support_modele.nom)

 

        __MAIL = CREA_MAILLAGE( MAILLAGE=self.support_maillage,
                                CREA_POI1 = _F( GROUP_NO = group_no_ext,
                                                NOM_GROUP_MA = self.group_ma_int, ))
        self.x_mailint = __MAIL
        self.clear_concept(self.x_mailsstr)
        __MAILM=ASSE_MAILLAGE(MAILLAGE_1=self.x_mailcond,
                             MAILLAGE_2=self.x_mailint,
                             OPERATION='SOUS_STR',)
        self.x_mailsstr = __MAILM

        self.clear_concept( self.x_modlint )

        __MODL=AFFE_MODELE(MAILLAGE=self.x_mailsstr,
                           AFFE=_F(GROUP_MA=self.group_ma_int,
                                   PHENOMENE='MECANIQUE',
                                   MODELISATION='DIS_T',),
                           AFFE_SOUS_STRUC=_F(SUPER_MAILLE=self.sumail,
                                   PHENOMENE='MECANIQUE',),
                           )
        self.x_modlint = __MODL

        self.clear_concept( self.x_caraint )
        __CARA=AFFE_CARA_ELEM(MODELE=self.x_modlint,
                              DISCRET=_F(GROUP_MA=self.group_ma_int,
                                         REPERE='GLOBAL',
                                         CARA='K_T_D_N',
                                         VALE=(0.,0.,0.,),
                                         ),);
        self.x_caraint = __CARA
        self.clear_concept( self.x_kelint )
        __KEL=CALC_MATR_ELEM(OPTION='RIGI_MECA',
                             MODELE=self.x_modlint,
                             CARA_ELEM=self.x_caraint,
                             );
        self.x_kelint = __KEL
        self.clear_concept( self.x_melint )
        __MEL=CALC_MATR_ELEM(OPTION='MASS_MECA',
                             MODELE=self.x_modlint,
                             CARA_ELEM=self.x_caraint,
                             );
        self.x_melint = __MEL
        self.clear_concept( self.x_numint )
        __NUM=NUME_DDL( MATR_RIGI=self.x_kelint,);
        self.x_numint = __NUM
        self.clear_concept( self.x_kas )
        __KAS=ASSE_MATRICE( MATR_ELEM=self.x_kelint,NUME_DDL=self.x_numint,);
        self.x_kas = __KAS
        self.clear_concept( self.x_mas )
        __MAS=ASSE_MATRICE( MATR_ELEM=self.x_melint,NUME_DDL=self.x_numint,);
        self.x_mas = __MAS
        self.clear_concept( self.x_ponder )
        if self.mac_ponder == 'RIGIDITE' :
            self.x_ponder = self.x_kas
        elif self.mac_ponder == 'MASSE' :
            self.x_ponder = self.x_mas
        else :
            self.x_ponder = None


    def indicateur_choix_base_projection(self):
        """Expansion statique du champ de deplacements aux interfaces"""
        self.clear_concept( self.x_modstint )
        __MODSTI=MODE_STATIQUE(MATR_RIGI=self.kassup,
                              FORCE_NODALE=self._get_force_nodale())
        self.x_modstint = __MODSTI

        r=Resultat(None, __MODSTI.nom, __MODSTI, self.mess, owned=False)
        nume_modes,base_proj=r.get_modes_stat()


        self.clear_concept( self.x_baseint )
        __BASINT=DEFI_BASE_MODALE(RITZ=( _F(MODE_MECA=self.support_modele_res,
                                            NMAX_MODE=0,),
                                         _F(MODE_STAT=self.x_modstint,
                                            NMAX_MODE=len(nume_modes),),
                                         ),
                                  NUME_REF=self.nume_support_modele,);
        self.x_baseint = __BASINT

        self.clear_concept( self.x_projmsint )
        try:
            __PROJMS=PROJ_MESU_MODAL(MODELE_CALCUL=_F(MODELE=self.support_modele.obj,
                                                    BASE=__BASINT,),
                                   MODELE_MESURE=_F(MODELE=self.resu_exp.modele.obj,
                                                    MESURE=self.modes_retr,
                                                    NOM_CHAM='DEPL',),
                                   RESOLUTION=self.param_condens
                                   );
            self.x_projmsint = __PROJMS
        except aster.error:
            self.mess.disp_mess("Condensation de la mesure: " \
                                "Erreur dans PROJ_MESU_MODAL")
            self.mess.disp_mess(str(err))
            return            

        self.clear_concept( self.x_deplpr )
        __DEPLPR=REST_GENE_PHYS(RESU_GENE=__PROJMS,
                              TOUT_ORDRE='OUI',
                              NOM_CHAM   ='DEPL');
        self.x_deplpr = __DEPLPR

        self.clear_concept( self.x_deplint )
        __DEPINT=PROJ_CHAMP(METHODE='ELEM',
                            RESULTAT=self.x_deplpr,
                            MODELE_1=self.support_modele.obj,
                            MODELE_2=self.x_modlint,
                            NOM_CHAM='DEPL',
                            TOUT_ORDRE='OUI',
                            NUME_DDL=self.x_numint,
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
                            );
        self.x_deplxint = __DPXINT
        # INDICATEUR DE PROXIMITE DES MODES
        # LA BASE DE PROJECTION EST CORRECT SI DEPLINT = DEPLXINT
        # LES MODES SONT PROCHES SI LES TERMES DIAG DU MAC PROCHE DE 1
        self.clear_concept( self.mac_int )
        if self.mac_method == 'MAC' :
            if self.mac_ponder == 'SANS' :
                __MACINT=MAC_MODES(BASE_1=__DEPINT,
                       BASE_2=__DPXINT,
                       INFO  =2,
                      );
            else :
                __MACINT=MAC_MODES(BASE_1=__DEPINT,
                       BASE_2=__DPXINT,
                       MATR_ASSE=self.x_ponder,
                       INFO  =2,
                      );
        else :    # IERI
            if self.mac_ponder == 'SANS' or self.mac_ponder == 'Choisir':
                self.mess.disp_mess("Il faut une matrice de ponderation " \
                                "pour le critere IERI.")
                self.mac_int = None
                self.mac_val = None
                return
            if self.x_ponder :
                __MACINT=MAC_MODES(BASE_1=__DEPINT,
                       BASE_2=__DPXINT,
                       MATR_ASSE=self.x_ponder,
                       IERI  ="OUI",
                       INFO  =2,
                      );
            else :  # pour le cas non-interactif : self.x_ponder initialise a None
                self.mac_int = None
                self.mac_val = None
                return
        self.mac_int = __MACINT
        self.mac_val = extract_mac_array( __MACINT,self.mac_method )

    def modes_modele_couple(self, mode_simult, calc_freq):
        # ne traite qu'une seule masse/raideur
        cpl = self.cpl
        kcouple = cpl.mat_rigi[0]
        mcouple = cpl.mat_mass[0]
##        acouple = cpl.mat_amor[0]

        self.clear_concept( self.modes_couple )
        if mode_simult :
            __MODCPL = MODE_ITER_SIMULT( MATR_A = kcouple,
                                        MATR_B = mcouple,
                                        VERI_MODE = _F( SEUIL = 1.E-05,
                                                        STOP_ERREUR = 'OUI',),
                                        CALC_FREQ = calc_freq,);
        else :
            __MODCPL = MODE_ITER_INV( MATR_A = kcouple,
                                     MATR_B = mcouple,
                                     CALC_FREQ = calc_freq,
                                    );
        self.modes_couple = __MODCPL
        name = obj_get_name(self.modes_couple)
        self.meidee_objects.update(name, self.modes_couple)

        # RETROPROJECTION SUR LE MODELE EXPERIMENTAL (INTERFACE -> DDL MESURE)
        self.clear_concept( self.modes_retr )
        # XXX Should not we return the DEPL_INTERNE in the DeclareOut?
        __MDRETR=DEPL_INTERNE(DEPL_GLOBAL=__MODCPL,SUPER_MAILLE=self.sumail)
        self.modes_retr = __MDRETR
        name = obj_get_name(self.modes_retr)
        self.meidee_objects.update(name, self.modes_retr)


    def calc_base_proj(self, calc_freq=None):
        """Calcule la base d'expansion en fonction de la methode, ES ou LMME"""
        num_sup, base_proj, x_bsmo = self.calc_base_es()

        # Conserve la base initiale (pour retrouver les noeuds a filter)
        self.base_proj = base_proj

        # Expansion statique projetee
        if self.method_name == "LMME":
            nume_modes_sup, base_proj, x_resgen = self.calc_base_lmme(x_bsmo,
                                                                      calc_freq)
            self.calculated_modes = [ ('%3i' %n , '%8.2f Hz' %f)
                                      for n,f in nume_modes_sup ]
        else:
            self.calculated_modes = [ ( '%3i' %n, '%12s' % t) 
                                      for n, t in zip(num_sup, base_proj) ]

    def calcul_mesure_support_corresp(self):
        """Demarre le calcul Meidee sur la structure modifiee."""

        self.get_mode_meca_modele_support()
        self.calc_base_proj()

    def _can_get_nume_support_model(self):
        """Retourne True si la numerotation du modele du support est
        retrouvee. Remorque, c'est le modele provenant des resulats
        des objects Meidee."""

        numesup = None
        if not self.nume_support_modele :
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
            force_nodale.append( _F(GROUP_NO=grp["NOM"],
                                    AVEC_CMP=grp["NOM_CMP"]) )
        return force_nodale

    def calcul_condensation(self):
        if not self._can_get_nume_support_model():
            return

        self.condensation()
        self.modele_couple()
        self.maillage_iface()
        
    def calcul_coupling_model_modes(self, mode_simult, calc_freq):
        if "NMAX_FREQ" in calc_freq:
            if calc_freq['NMAX_FREQ'] <= 0:
                calc_freq['NMAX_FREQ'] == 10 # valeur par defaut

        self.modes_modele_couple(mode_simult, calc_freq)

        self.indicateur_choix_base_projection()
    def is_valid( self ):
        """!Indique si on a calcule le modele couple
            et on a effectue la retro projection sur le modele mesure
        """
        bool = self.modes_retr is not None
        return bool



class CopyModelMeca:
    """Cette classe tente de recreer un sd_modele et toutes ses
    caracteristique a partir d'un 'modele'.
    on peut surcharger chaque etape pour adapter cette creation
    """
    def __init__(self):
        # concepts produits par l'objet qui doivent etre detruits
        # avant reutilisation
        self.modele = None
        self.maillage = None
        self.nume_lst = []
        self.mat_rigi = []
        self.mat_mass = []
        self.mat_amor = []

    def reinit(self, modl):
        """Copie (essaye) un modele"""
        self.orig_modl = modl
        self.orig_mail = None
        self.concepts = {} # mapping nom concepts orig->nouvel obj
        self.matr_elem = {}  # mapping matr_elem_orig.nom -> option

    def copy(self):
        self.create_maillage()
        if self.orig_mail:
            self.concepts[self.orig_mail.nom] = self.maillage

        affe_modl = self.retrieve_affe_model( self.orig_modl )
        self.create_modele( affe_modl )
        self.concepts[ self.orig_modl.nom ] = self.modele

        # Essaye de reconstruire le modele support+modif en utilisant modlsup comme `modele`
        etapes = self.retrieve_model_param( self.orig_modl.nom.strip() )
        self.affe_cara_elem( etapes.get('AFFE_CARA_ELEM', []) )
        self.affe_materiau( etapes.get('AFFE_MATERIAU',[]) )
        self.affe_char_meca( etapes.get('AFFE_CHAR_MECA',[]) )
        self.calc_matr_elem( etapes.get('CALC_MATR_ELEM',[]) )

        # on recupere les noms de concepts produits par matr_elem pour detecter ceux utilises
        # par nume_ddl
        nume_args = self.retrieve_nume_ddl( self.orig_modl.nom, self.matr_elem.keys() )
        self.nume_ddl( nume_args )
        nume_names = [ sd.nom for args, sd in nume_args ]
        asse_matr = self.retrieve_asse_matrice( nume_names )
        self.asse_matrice( asse_matr )


    def create_maillage(self):
        """Creation du maillage"""
        # version par defaut suppose que self.maillage est initialise
        assert self.maillage is not None

    def create_modele(self, affe):
        """Creation du modele"""
        self.clear_concept( self.modele.obj )
        affe = convert_args( affe, self.concepts )
        __MDLCPL = AFFE_MODELE( MAILLAGE=self.maillage,
                                AFFE=affe )
        self.modele = __MDLCPL

    def affe_cara_elem(self, cara):
        # replication des AFFE_CARA_ELEM
        # ------------------------------
        for args, sd in cara:
            args = convert_args( args, self.concepts )
            _TMP = AFFE_CARA_ELEM( **args )
            self.concepts[sd.nom] = _TMP

    def affe_materiau(self, mater):
        # replication des AFFE_MATERIAU
        # -----------------------------
        if not mater:
            raise RuntimeError("MODELE est un attribut facultatif de AFFE_MATERIAU \n"
                                  "mais nécessaire pour cette méthode!"
                               )
        for args, sd in mater:
            args = convert_args( args, self.concepts )
            _TMP = AFFE_MATERIAU( **args )
            self.concepts[sd.nom] = _TMP

    def affe_char_meca(self, charge):
        # replication de AFFE_CHAR_MECA
        # -----------------------------
        for args, sd in charge:
            args = convert_args(args, self.concepts)
            _TMP = AFFE_CHAR_MECA( **args )
            self.concepts[sd.nom] = _TMP

    def calc_matr_elem(self, matr_elem):
        for args, sd in matr_elem:
            args = convert_args(args, self.concepts)
            _TMP=CALC_MATR_ELEM(**args)
            self.concepts[sd.nom] = _TMP
            self.matr_elem[ sd.nom ] = args['OPTION']

    def nume_ddl(self, args_lst):
        for args, sd in args_lst:
            args = convert_args( args, self.concepts )
            _TMP = NUME_DDL(**args)
            self.concepts[sd.nom] = _TMP
            self.nume_lst.append( _TMP )

    def asse_matrice(self, args_lst):
        for args, sd in args_lst:
            #print "ASSE_MATRICE"
            #dump_mc( args )
            mat_elem = args['MATR_ELEM']
            args = convert_args( args, self.concepts )
            #dump_mc( args )
            _TMP = ASSE_MATRICE(**args)
            self.concepts[sd.nom] = _TMP
            typ_elem = self.matr_elem.get( mat_elem.nom, None )
            if typ_elem=='RIGI_MECA':
                self.mat_rigi.append( _TMP )
            elif typ_elem=='MASS_MECA':
                self.mat_mass.append( _TMP )
            elif typ_elem=='AMOR_MECA':
                self.mat_amor.append( _TMP )


    def retrieve_affe_model( self, modl ):
        name = modl.nom.strip()
        jdc = CONTEXT.get_current_step().jdc
        for etape in jdc.etapes:
            if not hasattr( etape, 'sd'):
                continue
            if not hasattr( etape.sd, 'nom' ):
                continue
            if etape.sd.nom == name:
                cara = etape.valeur.copy()
                return cara['AFFE']

    def retrieve_model_param( self, modname ):
        """Renvoie les parametres des macros affectees a un modele

        XXX: le param modele est facultatif dans AFFE_MATERIAU il faut
        donc rechercher aussi les etapes portant sur le maillage (mais
        on peut aussi contraindre l'utilisateur a utiliser MODELE=...
        pour l'instant)
        """
        jdc = CONTEXT.get_current_step().jdc
        etapes = {}

##        print "SELECTION", modname

        for etape in jdc.etapes:
            if etape.nom not in ('AFFE_MATERIAU', 'AFFE_CARA_ELEM',
                                 'AFFE_CHAR_MECA', 'CALC_MATR_ELEM',):
                continue
            args = etape.valeur
            modl = etape.valeur.get('MODELE', None)
            etape_modl_name = obj_get_name( modl )

            if etape_modl_name != modname:
                continue
            lst = etapes.setdefault( etape.nom, [] )
            lst.append( (args.copy(), etape.sd) )
        return etapes

    def retrieve_nume_ddl( self, modname, noms_matr_elem ):
        """Renvoie les parametres des macros affectees a un modele

        XXX: le param modele est facultatif dans AFFE_MATERIAU il faut
        donc rechercher aussi les etapes portant sur le maillage (mais
        on peut aussi contraindre l'utilisateur a utiliser MODELE=...
        pour l'instant)
        """
        jdc = CONTEXT.get_current_step().jdc
        nume_ddls = []
        modname = modname.strip()
        noms_matr_elem = [ nom.strip() for nom in noms_matr_elem ]
        for etape in jdc.etapes:
            if etape.nom != 'NUME_DDL':
                continue
            args = etape.valeur
            modl = etape.valeur.get('MODELE', None)
            etape_modl_name = obj_get_name( modl )
            rigi = etape.valeur.get('MATR_RIGI', None)
            rigi_meca_name = obj_get_name( rigi )

            if etape_modl_name == modname or rigi_meca_name in noms_matr_elem:
                nume_ddls.append( (args.copy(), etape.sd) )
        return nume_ddls

    def retrieve_asse_matrice( self, nume_names ):
        """Renvoie les parametres des macros affectees a un modele

        XXX: le param modele est facultatif dans AFFE_MATERIAU il faut
        donc rechercher aussi les etapes portant sur le maillage (mais
        on peut aussi contraindre l'utilisateur a utiliser MODELE=...
        pour l'instant)
        """
        jdc = CONTEXT.get_current_step().jdc
        asse_matrices = []
        nume_names = [ name.strip() for name in nume_names ]
        for etape in jdc.etapes:
            if etape.nom != 'ASSE_MATRICE':
                continue
            args = etape.valeur
            #print "FOUND ASSE_MATRICE:"
            #dump_mc( args )
            numeddl = etape.valeur.get('NUME_DDL', None)
            nume_ddl_name = obj_get_name( numeddl )

            if nume_ddl_name in nume_names:
                asse_matrices.append( (args.copy(), etape.sd) )
        return asse_matrices

    def clear_concept(self, cpt):
        """!Detruit un concept silencieusement"""
        if cpt is None:
            return
        DETRUIRE(CONCEPT=_F(NOM=cpt),ALARME='NON',INFO=1)


class CreateModeleCouple(CopyModelMeca):

    def reinit(self, modl, mail1, mail2, mail3):
        CopyModelMeca.reinit( self, modl )
        self.mail1 = mail1
        self.mail2 = mail2
        self.sumail = mail3

    def create_maillage(self):
        """Creation du maillage"""
        self.clear_concept( self.maillage )
        # Concept de sortie, ne pas changer de nom sans changer le DeclareOut
        __MLCPL = ASSE_MAILLAGE( MAILLAGE_1=self.mail1,
                                 MAILLAGE_2=self.mail2,
                                 OPERATION='SOUS_STR' )
        self.maillage = __MLCPL
        self.concepts[self.mail1.nom] = __MLCPL


    def create_modele(self, affe):
        """Creation du modele"""
        self.clear_concept( self.modele )
        # Concept de sortie, ne pas changer de nom sans changer le DeclareOut
        __MDLCPL = AFFE_MODELE( MAILLAGE=self.maillage,
                                AFFE=affe,
                                AFFE_SOUS_STRUC=_F( SUPER_MAILLE = self.sumail,
                                                    PHENOMENE='MECANIQUE',),
                                )
        self.modele = __MDLCPL


    def nume_ddl(self, args_lst):
        if len(args_lst)!=1:
            raise RuntimeError("Plusieurs, ou aucun, NUME_DDL presents, "\
                               "on ne peut en avoir qu'un")
        args, sd = args_lst[0]
        for nume in self.nume_lst:
            self.clear_concept( nume )
        self.nume_lst = []
        args = convert_args( args, self.concepts )
        __NUMCPL = NUME_DDL(**args)
        self.concepts[sd.nom] = __NUMCPL
        self.nume_lst.append( __NUMCPL )

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
                __RIGIME = ASSE_MATRICE(**args)
                self.mat_rigi.append( __RIGIME )
                self.concepts[sd.nom] = __RIGIME
            elif typ_elem=='MASS_MECA':
                __MASSME = ASSE_MATRICE(**args)
                self.mat_mass.append( __MASSME )
                self.concepts[sd.nom] = __MASSME
            elif typ_elem=='AMOR_MECA':
                __AMORME = ASSE_MATRICE(**args)
                self.mat_amor.append( __AMORME )
                self.concepts[sd.nom] = __AMORME


######################
#                    #
# PETITS UTILITAIRES #
#                    #
######################


def convert_args( mc, concepts ):
    """convertit un (ensemble) de mot-clefs en remplacant les
    objets presents dans 'concepts' par les valeurs associees
    """
    if isinstance( mc, (dict,_F) ):
        if isinstance(mc,dict):
            dest = {}
        else:
            dest = _F()
        for k,v in mc.items():
            v = convert_args( v, concepts )
            dest[k] = v
        return dest
    elif isinstance( mc, (int,float,str,unicode) ):
        return mc
    elif isinstance( mc, ASSD ):
        if mc.nom in concepts:
            return concepts[mc.nom]
        return mc
    elif isinstance( mc, (list,tuple) ):
        lst = []
        for obj in mc:
            lst.append( convert_args( obj, concepts ) )
        if isinstance( mc, tuple ):
            lst = tuple(lst)
        return lst
    else:
        raise NotImplementedError("type non reconnu : %r" % mc)



def dump_mc( mc, indent="" ):
    if isinstance( mc, (dict, _F) ):
        mck = mc.keys()
        mck.sort()
        for k in mck:
            print indent, k, ":"
            dump_mc( mc[k], indent+" " )

    elif isinstance( mc, (int,float,str,unicode) ):
        print indent, mc
    elif hasattr( mc, 'nom' ):
        print indent, "OBJ(", mc.nom, ")"
    elif isinstance( mc, (list,tuple) ):
        print indent, "("
        for obj in mc:
            dump_mc( obj, indent+"  " )
        print indent, ")"
    else:
        print indent, repr(mc)


def retrieve_model_param( modname ):
    """Renvoie les parametres des macros affectees a un modele

    """
    #jdc = CONTEXT.get_current_step().jdc
    jdc = CONTEXT.get_current_step()
    etapes = []

    for etape in jdc.etapes:
        if etape.nom not in ('AFFE_MATERIAU', 'AFFE_CARA_ELEM', 'AFFE_CHAR_MECA', 'CALC_MATR_ELEM',
                             ):
            continue
        args = etape.valeur
        modl = etape.valeur['MODELE']
        if isinstance(modl,str):
            modlname = modl
        else:
            modlname = modl.nom.strip()

        if modlname != modname:
            continue
        etapes.append( (etape.nom, args, etape.sd) )
    return etapes


def obj_get_name( obj ):
    """ utilise par CopyModelMeca """
    if isinstance(obj,str):
        return obj
    elif hasattr(obj,'nom'):
        return obj.nom.strip()
    return None


def dumpobj(name, obj):
    """ utilitaire non utilise actuellement """
    for n in dir(obj):
        attr = getattr( obj, n )
        if isinstance(attr, (int,str,unicode,float)):
            print "%s.%s=%r" % (name,n,attr)
        else:
            print "%s.%s=%s" % (name,n,type(attr))
