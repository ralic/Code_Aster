# coding=utf-8
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
# person_in_charge: albert.alarcon at edf.fr

# La classe CalcEssaiModifStruct permet de gerer les calculs de modification structurale
#
import aster
from Accas import _F, ASSD

import Cata.cata
from Cata.cata import MODE_STATIQUE, PROJ_MESU_MODAL
from Modal.mode_iter_simult import MODE_ITER_SIMULT
from Cata.cata import REST_GENE_PHYS
from Cata.cata import ASSE_MAILLAGE, AFFE_MODELE
from Cata.cata import NUME_DDL, CALC_MATR_ELEM, AFFE_CARA_ELEM
from Cata.cata import ASSE_MATRICE
from Cata.cata import DETRUIRE

from Calc_essai.cata_ce import Resultat, ModeMeca
from Calc_essai.ce_calcul_expansion import extract_mac_array

from Utilitai.Utmess import MESSAGE_LOGGER

# MESSAGE_LOGGER = classe permettant de formatter et d'afficher les
# messages d'erreur
mess = MESSAGE_LOGGER()


class CalcEssaiModifStruct:

    """!Classe qui s'occupe des calculs de modification structurale

    """

    def __init__(self, macro, ce_objects, mess, outputs):
        """!Constructeur

        \param macro le self de l'objet macro provenant de calc_essai_ops

        :ce_objects; objects de la memoire JEVEUX
        :parap mess: ecrit les erreurs dans un fichier.

        :param outputs: concepts Aster de l'utilisateur a afficher en sortie.
        """
        self.macro = macro
        self.objects = ce_objects
        self.mess = mess

        self.modes_exp = {}
        self.base_proj = {}

        self.resu_exp = None
        self.support_modele = None
        self.support_maillage = None
        self.matr_rig = None
        self.kassup = None
        self.method_name = None

        self.sensor_groups = None
        self.interface_groups = None

        self.modes_ide = None
        self.modes_expansion = None

        self.sumail = None
        self.mailx = None
        self.modlx = None

        self.support_modele_res = None
        self.matr_masse = None
        self.cara_elem = None
        self.cham_mater = None

        self.param_condens = None

        self.cpl = CreateModeleCouple(ce_objects)
        self.x_modst = None  # pour ES
        self.x_bsmo = None   # pour ES
        self.x_resgen = None
        self.x_proj = None   # proj_mesu_modal (mesure/support)
        self.x_mide = None   # selection des modes mesures
        self.x_ssexp = None
        self.x_mailcond = None
        self.base_expansion = None  # x_resgen ou x_bsmo
        self.x_mexp = None  # base d'expansion

        # resultats d'operations sur le modele/maillage interface
        self.i_modlint = None   # modele
        self.i_numint = None   # nume
        self.i_kas = None   # kas
        self.i_mas = None   # mas
        self.mat_ponder = None   # kas ou mas
        self.modstint = None  # modes statiques
        self.projmsint = None
        self.i_deplpr = None
        self.i_deplint = None
        self.i_deplxint = None

        # modele couple
        self.modes_couple = None  # modes calcules sur le modele couple
        self.modes_retr = None    # modes du modele couple sur la super maille

        self.mac_int = None  # resultat de MAC_MODES
        self.mac_val = None  # resultat de MAC_MODES
        self.crit_ponder = None  # ponderation pour MAC_MODES : RIGIDITE / MASSE
        self.crit_method = None  # choix methode pour MAC_MODES

        step = CONTEXT.get_current_step()
        if outputs:
            if outputs['MODELE']:
                step.DeclareOut("_MDLCPL", outputs['MODELE'])
            if outputs['MAILLAGE']:
                step.DeclareOut("_MLCPL", outputs['MAILLAGE'])
            if outputs['MODE_MECA']:
                step.DeclareOut("_MODCPL", outputs['MODE_MECA'])
            if outputs['NUME_DDL']:
                step.DeclareOut("_NUMCPL", outputs['NUME_DDL'])
            if outputs['MASS_MECA']:
                step.DeclareOut("_MASSME", outputs['MASS_MECA'])
            if outputs['RIGI_MECA']:
                step.DeclareOut("_RIGIME", outputs['RIGI_MECA'])
            if outputs['AMOR_MECA']:
                step.DeclareOut("_AMORME", outputs['AMOR_MECA'])
            if outputs['MACR_ELEM']:
                step.DeclareOut("_SSEXP", outputs['MACR_ELEM'])
            if outputs['PROJ_MESU']:
                step.DeclareOut("_PROJ", outputs['PROJ_MESU'])
            if outputs['BASE_ES']:
                step.DeclareOut("_MEXP", outputs['BASE_ES'])
            if outputs['BASE_LMME']:
                step.DeclareOut("_MEXP", outputs['BASE_LMME'])
            if outputs['MODE_STA']:
                step.DeclareOut("_MODST", outputs['MODE_STA'])

    def find_experimental_result_from(self, modes_exp_name):
        """Trouve le mode experimental dans la memoire JEVEUX."""
        self.resu_exp = self.objects.get_mode_meca(modes_exp_name)

    def find_support_modele_from(self, supmodl_name):
        """Trouve le modele du support dans la memoire JEVEUX"""
        self.support_modele = self.objects.get_model(supmodl_name)

    def set_stiffness_matrix(self, stiffness_matrix):
        """Place la matrice de raideur et trouve celle correspondant
        au module CALC_ESSAI dans la memoire JEVEUX."""
        self.matr_rig = stiffness_matrix
        self.kassup = self.objects.get_matr(stiffness_matrix.nom)

    def set_method_name(self, method_name):
        """Place le nom de methode pour calculer la base: 'ES' ou 'LMME'"""
        self.method_name = method_name

    def set_sensor_groups(self, sensor_groups):
        """Place les groupes de capteurs utilises, avec leur nom
        et degres de liberte."""
        self.sensor_groups = sensor_groups

    def set_interface_groups(self, interface_groups):
        """Place les groupes de la structure externe, avec leur nom
        et degres de liberte."""
        self.interface_groups = interface_groups

    def set_modes_ide(self, modes_ide):
        """Place les modes mesures sur la structure a utiliser pour le
        calcul."""
        self.modes_ide = modes_ide

    def set_modes_expansion(self, modes_expansion):
        """Place les modes d'expansion a utiliser pour le calcul.
        Ils peuvent etre proposes a partir de la selection des groupes
        'capteur' et 'interface'."""
        self.modes_expansion = modes_expansion

    def find_maillage_modif_from(self, modc_name):
        """Trouve le maillage modif dans la memoire JEVEUX"""
        # existence du modele
        modele = self.objects.get_model(modc_name)
        for modele_name, modele in self.objects.modeles.items():
            if modele_name == modc_name:
                mailx_name = modele.maya_name
                self.mailx = self.objects.maillages[mailx_name]

    def find_maillage_support_from(self, modc_name):
        """Trouve le maillage modif dans la memoire JEVEUX"""
        for modele_name, modele in self.objects.modeles.items():
            if modele_name == modc_name:
                mailx_name = modele.maya_name
                self.support_maillage = self.objects.maillages[mailx_name]

    def find_modele_modif_from(self, modc_name):
        """Trouve le modele de la modification dans la memoire JEVEUX"""
        self.modlx = self.objects.modeles[modc_name]

    def set_param_condens(self, param):
        """ parametres de PROJ_MESU_MODAL pour la condensation :
            {'methode':'SVD'ou'LU','eps':eps,'regul_meth':'NON' ou
             'TIK_RELA' ou 'NORM_MIN','regul':alpha}"""
        self.param_condens = param

    def set_resolution_calc_modal(self, cm_name):
        """Place le nom de la methode pour calculer les modes couples"""
        self.resolution_calc_modal = cm_name

    def set_sumail_name(self, sumail_name):
        """Place le nom de de la super maille """
        self.sumail = sumail_name

    def set_crit_method(self, crit_meth):
        """Place le nom de methode pour le calcul de MAC : 'MAC' ou 'IERI'"""
        self.crit_method = crit_meth

    def set_crit_ponder(self, crit_ponder):
        """Place le type de matrice de ponderation pour le MAC : 'RIGIDITE' ou 'MASSE'"""
        self.crit_ponder = crit_ponder

    def get_modele_support(self):
        """Recherche dans les resultats des objects CALC_ESSAI le modele mecanique
        du support. Remarque: ce resultat contient des modes mecaniques
        bidons. Une fois le modele trouve, la matrice de masse, ainsi que
        la caracteristique des elements et le champ de materiaux sont
        conserves en references.
        """
        for name, resu in self.objects.resultats.items():
            if aster.jeveux_exists(name.ljust(19) + '.NOVA'):
                iret, ibid, modele_name = aster.dismoi(
                    'MODELE', name, 'RESULTAT', 'F')
                modele_name = modele_name.rstrip()
                if modele_name[0:1] == "#":
                    continue

            else:
                continue
            modllu = modele_name

            if modllu == self.support_modele.nom:
                support_modele = resu.obj

                iret, ibid, nom_raideur = aster.dismoi(
                    'REF_RIGI_PREM', support_modele.nom, 'RESU_DYNA', 'F')
                nom_raideur = nom_raideur.strip()

                if nom_raideur == self.matr_rig.nom:

                    iret, ibid, nom_masse = aster.dismoi(
                        'REF_MASS_PREM', support_modele.nom, 'RESU_DYNA', 'F')
                    nom_masse = nom_masse.strip()
                    matr_masse = self.objects.get_matr(nom_masse)

                    iret, ibid, nume_name = aster.dismoi(
                        'NUME_DDL', support_modele.nom, 'RESU_DYNA', 'F')
                    nume_name = nume_name.strip()
                    numesup = self.objects.nume_ddl[nume_name]

                    iret, ibid, var_carelem = aster.dismoi(
                        'CARA_ELEM', name, 'RESULTAT', 'F')
                    var_carelem = var_carelem.strip()

                    iret, ibid, var_chmat = aster.dismoi(
                        'CHAM_MATER', name, 'RESULTAT', 'F')
                    var_chmat = var_chmat.strip()
                    break
        else:
            self.mess.disp_mess("Impossible de trouver le mode meca calcule "
                                "du modele support")

            return

        self.support_modele_res = support_modele
        self.nume_support_modele = numesup
        self.matr_masse = matr_masse

        # Caracteristique de l'element et champ de materiaux
        # Utiliser pendant la condensation avec MACR_ELEM_STAT
        self.cara_elem = self.objects.get_cara_elem(var_carelem)
        self.cham_mater = self.objects.get_cham_mater(var_chmat)

    def _get_grp_sensors_tot(self):
        """Retourne un groupe de capteurs en fonction de la methode.
        Pour ES on garde uniquement les DDL capteurs, pour LMME on utilise aussi
        les DDL interface."""
        # XXX: pour LMME est-ce qu'on passe tout pour calc_base_es ??
        if self.method_name == "ES":
            grpno_tot = self.sensor_groups
        else:
            grpno_tot = self.sensor_groups + self.interface_groups

        grno = []
        for grp in grpno_tot:
            grno.append(_F(GROUP_NO=grp["NOM"], AVEC_CMP=grp["NOM_CMP"]))
        return grno

    def calc_base_es(self, opt_grnocapt=None):
        """!Calcul de la base d'expansion : par expansion statique
        grnocapt : le mot-clef facteur passe a FORCE_NODALE.
        """

        grnocapt = opt_grnocapt or self._get_grp_sensors_tot()

        clear_concept(self.x_bsmo)
        self.x_bsmo = None

        _MODST = MODE_STATIQUE(MATR_RIGI=self.matr_rig,
                               FORCE_NODALE=grnocapt,)

        self.x_bsmo = ModeMeca(None, _MODST.nom, _MODST, self.mess)
        self.base_expansion = self.x_bsmo

        return self.x_bsmo

    def calc_base_lmme(self, base_mod_es, calc_freq=None):
        """!Calcul de la base de projection type Mathieu Corus
        """

        from Cata.cata import NUME_DDL_GENE, PROJ_MATR_BASE

        __NUMGEN = NUME_DDL_GENE(BASE=base_mod_es.obj,
                                 STOCKAGE='PLEIN',)

        __KPROJ = PROJ_MATR_BASE(BASE=base_mod_es.obj,
                                 NUME_DDL_GENE=__NUMGEN,
                                 MATR_ASSE=self.matr_rig,)

        __MPROJ = PROJ_MATR_BASE(BASE=base_mod_es.obj,
                                 NUME_DDL_GENE=__NUMGEN,
                                 MATR_ASSE=self.matr_masse,)

        nume_modes_sup = base_mod_es.get_modes_data()['NUME_ORDRE']

        if calc_freq is None:
            calc_freq = {'OPTION': 'PLUS_PETITE',
                         'NMAX_FREQ': len(nume_modes_sup),
                         'SEUIL_FREQ': 1.E-4}

        if calc_freq.has_key('NMAX_FREQ'):
            if calc_freq['NMAX_FREQ'] == -1:
                calc_freq['NMAX_FREQ'] = len(nume_modes_sup)

        try:
            __MODGEN = MODE_ITER_SIMULT(MATR_RIGI=__KPROJ,
                                        MATR_MASS=__MPROJ,
                                        VERI_MODE=_F(SEUIL=1.E-05,
                                                     STOP_ERREUR='OUI',),
                                        PARA_ORTHO_SOREN=-0.717,
                                        CALC_FREQ=calc_freq)
        except aster.error, err:
            message = "ERREUR ASTER : " + \
                mess.GetText('I', err.id_message, err.valk, err.vali, err.valr)
            self.mess.disp_mess(message)

        clear_concept(self.x_resgen)
        self.x_resgen = None
        __RESGEN = REST_GENE_PHYS(RESU_GENE=__MODGEN,
                                  TOUT_ORDRE='OUI',
                                  NOM_CHAM='DEPL')

        self.x_resgen = ModeMeca(None, __RESGEN.nom, __RESGEN, self.mess)
        self.base_expansion = self.x_resgen

        clear_concept(__NUMGEN)
        clear_concept(__KPROJ)
        clear_concept(__MPROJ)
        clear_concept(__MODGEN)

        return self.x_resgen

    def condensation(self, resolution=None, nomcham='DEPL'):
        """Calcul la condensation des modes sur la structure modifiee."""

        from Cata.cata import MACR_ELEM_STAT, EXTR_MODE, DEFI_MAILLAGE

        modmesu = self.resu_exp
        modlexp = modmesu.modele
        modlsup = self.support_modele

        modes_mesure_retenus = self.modes_ide
        modes_expansion_retenus = self.modes_expansion

        noeuds_interface = [grp["NOM"] for grp in self.interface_groups]

        # Reduction des modes mesures
        clear_concept(self.x_mide)
        self.x_mide = None
        __MIDE = EXTR_MODE(FILTRE_MODE=_F(MODE=modmesu.obj,
                                          NUME_MODE=modes_mesure_retenus,
                                          ),)
        self.x_mide = __MIDE
        name = obj_get_name(self.x_mide)
        self.objects.update(name, self.x_mide)

        clear_concept(self.x_mexp)
        self.x_mexp = None
        # cas LMME, une fonction existe pour calculer la base d'expansion
        if self.method_name == "LMME":
            _MEXP = EXTR_MODE(FILTRE_MODE=_F(MODE=self.base_expansion.obj,
                                             NUME_MODE=modes_expansion_retenus))

        # cas ES, il faut re-calculer la base a partir des noeuds choisis
        else:
            # self.mess.disp_mess("Recalcul de la base modale par ES")
            nodes = {}
            for num in modes_expansion_retenus:
                node, comp = self.calc_base_es().get_modes_data()[
                    'NOEUD_CMP'][num - 1].split()
                nodes.setdefault(node, []).append(comp)
            grno = []
            for node, comps in nodes.items():
                grno.append(_F(NOEUD=node, AVEC_CMP=comps))
            self.calc_base_es(grno)
            _MEXP = self.base_expansion.obj
        self.x_mexp = _MEXP

        clear_concept(self.x_proj)
        self.x_proj = None
        try:
            _PROJ = PROJ_MESU_MODAL(MODELE_CALCUL=_F(MODELE=modlsup.obj,
                                                     BASE=self.x_mexp,),
                                    MODELE_MESURE=_F(MODELE=modlexp.obj,
                                                     MESURE=self.x_mide,
                                                     NOM_CHAM=nomcham,),
                                    RESOLUTION=self.param_condens
                                    )
        except aster.error, err:
            message = "ERREUR ASTER : " + \
                mess.GetText('I', err.id_message, err.valk, err.vali, err.valr)
            self.mess.disp_mess(message)
            return
        self.x_proj = _PROJ

        # Condensation de la mesure sur les DDL INTERFACES
        clear_concept(self.x_ssexp)
        self.x_ssexp = None
        try:
            _SSEXP = MACR_ELEM_STAT(DEFINITION=_F(MODELE=modlsup.obj,
                                                  PROJ_MESU=self.x_proj,
                                                  MODE_MESURE=self.x_mide,
                                                  CARA_ELEM=self.cara_elem,
                                                  CHAM_MATER=self.cham_mater,
                                                  ),
                                    EXTERIEUR=_F(
                                    GROUP_NO=noeuds_interface,),
                                    RIGI_MECA=_F(),
                                    MASS_MECA=_F(),
                                    AMOR_MECA=_F(),
                                    )
        except aster.error, err:
            message = "ERREUR ASTER : " + \
                mess.GetText('I', err.id_message, err.valk, err.vali, err.valr)
            self.mess.disp_mess(message)
            return
        self.x_ssexp = _SSEXP

        clear_concept(self.x_mailcond)
        self.x_mailcond = None
        __MAILCD = DEFI_MAILLAGE(
            DEFI_SUPER_MAILLE=_F(MACR_ELEM=self.x_ssexp,
                                 SUPER_MAILLE=self.sumail,),
            DEFI_NOEUD=_F(TOUT='OUI',
                          INDEX=(1, 0, 1, 8,))  # XXX
        )

        self.x_mailcond = __MAILCD

    def modele_couple(self):
        """Creation du modele couple"""
        self.cpl.reinit(self.modlx, self.mailx, self.x_mailcond, self.sumail)

        try:
            self.cpl.copy()
        except Exception, err:
            self.mess.disp_mess("Une erreur est survenue lors "
                                "de la creation du modele couple")
            self.mess.disp_mess(str(err))
            raise
            return

        if not self.cpl.mat_rigi:
            self.mess.disp_mess("Le modele couple n'a pas de matrice "
                                "de rigidite assemblee")
        if not self.cpl.mat_mass:
            self.mess.disp_mess("Le modele couple n'a pas de matrice "
                                "de masse assemblee")
        if not self.cpl.mat_amor:
            self.mess.disp_mess("Le modele couple n'a pas de matrice "
                                "d'amortissement assemblee")
        # ...

    def maillage_iface(self):
        """Construit le maillage de l'interface."""

        from Cata.cata import CREA_MAILLAGE

        self.group_ma_int = ['IFACE']

        group_no_ext = []
        for grp in self.interface_groups:
            group_no_ext.append(grp["NOM"])

        if not self.support_maillage:
            self.find_maillage_support_from(self.support_modele.nom)

        __MAIL = CREA_MAILLAGE(MAILLAGE=self.support_maillage,
                               CREA_POI1=_F(GROUP_NO=group_no_ext,
                                            NOM_GROUP_MA=self.group_ma_int, ))

        __MAILM = ASSE_MAILLAGE(MAILLAGE_1=self.x_mailcond,
                                MAILLAGE_2=__MAIL,
                                OPERATION='SOUS_STR',)

        clear_concept(self.i_modlint)
        self.i_modlint = None
        __MODL = AFFE_MODELE(MAILLAGE=__MAILM,
                             AFFE=_F(GROUP_MA=self.group_ma_int,
                                     PHENOMENE='MECANIQUE',
                                     MODELISATION='DIS_T',),
                             AFFE_SOUS_STRUC=_F(SUPER_MAILLE=self.sumail,
                                                PHENOMENE='MECANIQUE',),
                             )
        self.i_modlint = __MODL

        __CARA = AFFE_CARA_ELEM(MODELE=self.i_modlint,
                                DISCRET=(
                                _F(GROUP_MA=self.group_ma_int, REPERE='GLOBAL',
                                    CARA='K_T_D_N', VALE=(0., 0., 0.,),),
                                _F(GROUP_MA=self.group_ma_int, REPERE='GLOBAL',
                                   CARA='M_T_D_N', VALE=(0.,),),
                                ),
                                )

        __KEL = CALC_MATR_ELEM(OPTION='RIGI_MECA',
                               MODELE=self.i_modlint,
                               CARA_ELEM=__CARA,
                               )

        __MEL = CALC_MATR_ELEM(OPTION='MASS_MECA',
                               MODELE=self.i_modlint,
                               CARA_ELEM=__CARA,
                               )

        clear_concept(self.i_numint)
        self.i_numint = None

        __NUM = NUME_DDL(MATR_RIGI=__KEL,)
        self.i_numint = __NUM

        clear_concept(self.i_kas)
        self.i_kas = None
        __KAS = ASSE_MATRICE(MATR_ELEM=__KEL, NUME_DDL=self.i_numint,)
        self.i_kas = __KAS

        clear_concept(self.i_mas)
        self.i_mas = None
        __MAS = ASSE_MATRICE(MATR_ELEM=__MEL, NUME_DDL=self.i_numint,)
        self.i_mas = __MAS

    def indicateur_choix_base_expansion(self):
        """Expansion statique du champ de deplacements aux interfaces"""

        from Cata.cata import MAC_MODES, PROJ_CHAMP

        clear_concept(self.modstint)
        self.modstint
        __MODSTI = MODE_STATIQUE(MATR_RIGI=self.kassup,
                                 FORCE_NODALE=self._get_force_nodale())
        self.modstint = __MODSTI

        clear_concept(self.projmsint)
        self.projmsint = None
        try:
            __PROJMS = PROJ_MESU_MODAL(
                MODELE_CALCUL=_F(MODELE=self.support_modele.obj,
                                 BASE=self.modstint,),
                MODELE_MESURE=_F(
                    MODELE=self.resu_exp.modele.obj,
                    MESURE=self.modes_retr.obj,
                    NOM_CHAM='DEPL',),
                RESOLUTION=self.param_condens
            )
        except aster.error, err:
            message = "ERREUR ASTER : " + \
                mess.GetText('I', err.id_message, err.valk, err.vali, err.valr)
            self.mess.disp_mess(message)
            return

        self.projmsint = __PROJMS
        clear_concept(self.i_deplpr)
        self.i_deplpr = None
        __DEPLPR = REST_GENE_PHYS(RESU_GENE=self.projmsint,
                                  TOUT_ORDRE='OUI',
                                  NOM_CHAM='DEPL')
        self.i_deplpr = __DEPLPR

        clear_concept(self.i_deplint)
        self.i_deplint = None
        try:
            __DEPINT = PROJ_CHAMP(METHODE='COLLOCATION',
                                  RESULTAT=self.i_deplpr,
                                  MODELE_1=self.support_modele.obj,
                                  MODELE_2=self.i_modlint,
                                  NOM_CHAM='DEPL',
                                  TOUT_ORDRE='OUI',
                                  NUME_DDL=self.i_numint,
                                  )
        except aster.error, err:
            message = "ERREUR ASTER : " + \
                mess.GetText('I', err.id_message, err.valk, err.vali, err.valr)
            self.mess.disp_mess(message)
            return
        self.i_deplint = ModeMeca(None, __DEPINT.nom, __DEPINT)

        clear_concept(self.i_deplxint)
        self.i_deplxint = None
        # CHAMP DE DEPL AUX INTERFACES SUR LE MODELE COUPLE
        try:
            __DPXINT = PROJ_CHAMP(METHODE='COLLOCATION',
                                  RESULTAT=self.modes_couple.obj,
                                  MODELE_1=self.cpl.modele,
                                  MODELE_2=self.i_modlint,
                                  NOM_CHAM='DEPL',
                                  TOUT_ORDRE='OUI',
                                  NUME_DDL=self.i_numint,
                                  )
        except aster.error, err:
            message = "ERREUR ASTER : " + \
                mess.GetText('I', err.id_message, err.valk, err.vali, err.valr)
            self.mess.disp_mess(message)
            return
        self.i_deplxint = ModeMeca(None, __DPXINT.nom, __DPXINT)

        # INDICATEUR DE PROXIMITE DES MODES
        # LA BASE DE PROJECTION EST CORRECT SI DEPLINT = DEPLXINT
        # LES MODES SONT PROCHES SI LES TERMES DIAG DU MAC PROCHE DE 1

        if self.crit_ponder == 'RIGIDITE':
            self.mat_ponder = self.i_kas
        elif self.crit_ponder == 'MASSE':
            self.mat_ponder = self.i_mas
        else:
            self.mat_ponder = None

        clear_concept(self.mac_int)
        self.mac_int = None
        if self.crit_method == 'MAC':
            if self.mat_ponder:
                __MACINT = MAC_MODES(BASE_1=self.i_deplint.obj,
                                     BASE_2=self.i_deplxint.obj,
                                     MATR_ASSE=self.mat_ponder,
                                     INFO=2,
                                     )
            else:
                __MACINT = MAC_MODES(BASE_1=self.i_deplint.obj,
                                     BASE_2=self.i_deplxint.obj,
                                     INFO=2,
                                     )
        else:    # IERI
            if self.crit_ponder == 'SANS' or self.crit_ponder == 'Choisir':
                self.mess.disp_mess("Il faut une matrice de ponderation "
                                    "pour le critere IERI.")
                self.mac_int = None
                self.mac_val = None
                return
            if self.mat_ponder:
                __MACINT = MAC_MODES(BASE_1=self.i_deplint.obj,
                                     BASE_2=self.i_deplxint.obj,
                                     MATR_ASSE=self.mat_ponder,
                                     IERI="OUI",
                                     INFO=2,
                                     )
            else:  # pour le cas non-interactif : self.mat_ponder initialise a None
                self.mac_int = None
                self.mac_val = None
                return
        self.mac_int = __MACINT
        self.mac_val = extract_mac_array(__MACINT, self.crit_method)

    def modes_modele_couple(self, mode_simult, calc_freq):
        # ne traite qu'une seule masse/raideur

        from Cata.cata import DEPL_INTERNE
        from Modal.mode_iter_inv import MODE_ITER_INV

        cpl = self.cpl
        kcouple = cpl.mat_rigi[0]
        mcouple = cpl.mat_mass[0]
# acouple = cpl.mat_amor[0]

        clear_concept(self.modes_couple)
        self.modes_couple = None
        if mode_simult:
            try:
                _MODCPL = MODE_ITER_SIMULT(MATR_RIGI=kcouple,
                                           MATR_MASS=mcouple,
                                           VERI_MODE=_F(SEUIL=1.E-05,
                                                        STOP_ERREUR='OUI',),
                                           PARA_ORTHO_SOREN=-0.717,
                                           CALC_FREQ=calc_freq,)
            except aster.error, err:
                message = "ERREUR ASTER : " + \
                    mess.GetText(
                        'I', err.id_message, err.valk, err.vali, err.valr)
                self.mess.disp_mess(message)

        else:
            try:
                _MODCPL = MODE_ITER_INV(MATR_RIGI=kcouple,
                                        MATR_MASS=mcouple,
                                        CALC_FREQ=calc_freq,
                                        )
            except aster.error, err:
                message = "ERREUR ASTER : " + \
                    mess.GetText(
                        'I', err.id_message, err.valk, err.vali, err.valr)
                self.mess.disp_mess(message)

        self.modes_couple = ModeMeca(
            self.objects, _MODCPL.nom, _MODCPL, self.mess)
        self.modes_couple.kass = kcouple
        self.modes_couple.mass = mcouple
        self.objects.update(_MODCPL.nom, self.modes_couple)

        # RETROPROJECTION SUR LE MODELE EXPERIMENTAL (INTERFACE -> DDL MESURE)
        clear_concept(self.modes_retr)
        self.modes_retr = None
        # XXX Should not we return the DEPL_INTERNE in the DeclareOut?
        try:
            __MDRETR = DEPL_INTERNE(
                DEPL_GLOBAL=_MODCPL, SUPER_MAILLE=self.sumail)
        except aster.error, err:
            message = "ERREUR ASTER : " + \
                mess.GetText('I', err.id_message, err.valk, err.vali, err.valr)
            self.mess.disp_mess(message)

        self.modes_retr = ModeMeca(None, __MDRETR.nom, __MDRETR, self.mess)
        name = obj_get_name(self.modes_retr)
        self.objects.update(name, self.modes_retr)

    def calc_base_proj(self, calc_freq=None):
        """Calcule la base d'expansion en fonction de la methode, ES ou LMME"""
        x_bsmo = self.calc_base_es()

        # Expansion statique projetee
        if self.method_name == "LMME":
            self.base_proj = self.calc_base_lmme(x_bsmo, calc_freq)
        else:
            self.base_proj = x_bsmo
        return self.base_proj

    def _can_get_nume_support_model(self):
        """Retourne True si la numerotation du modele du support est
        retrouvee. Remarque, c'est le modele provenant des resulats
        des objects CALC_ESSAI."""

        numesup = None
        if not self.nume_support_modele:
            numesup = ModeMeca(None, self.support_modele_res.nom,
                               self.support_modele_res, self.mess).nume
            if not numesup:
                self.mess.disp_mess("Impossible de retrouver la numerotation "
                                    "utilisee pour le modele support")
                return False
            self.nume_support_modele = numesup
        return True

    def _get_force_nodale(self):
        """Renvoit la force nodale pour la macro: MODE_STATIQUE"""
        force_nodale = []
        for grp in self.sensor_groups:
            force_nodale.append(_F(GROUP_NO=grp["NOM"],
                                   AVEC_CMP=grp["NOM_CMP"]))
        return force_nodale

    def creation_modele_couple(self):
        if not self._can_get_nume_support_model():
            return

        self.condensation()
        self.modele_couple()
        self.maillage_iface()

    def calc_modes_modele_couple(self, mode_simult, calc_freq):
        if "NMAX_FREQ" in calc_freq:
            if calc_freq['NMAX_FREQ'] <= 0:
                calc_freq['NMAX_FREQ'] == 10  # valeur par defaut

        self.modes_modele_couple(mode_simult, calc_freq)

    def is_valid(self):
        """!Indique si on a calcule le modele couple
            et on a effectue la retro projection sur le modele mesure
        """
        calcul_fait = self.modes_retr is not None
        return calcul_fait


class CopyModelMeca:

    """Cette classe tente de recreer un sd_modele et toutes ses
    caracteristiques a partir d'un 'modele'.
    on peut surcharger chaque etape pour adapter cette creation
    """

    def __init__(self, objects):
        # concepts produits par l'objet qui doivent etre detruits
        # avant reutilisation
        self.modele = None
        self.maillage = None
        self.objects = objects
        self.nume_lst = []
        self.mat_rigi = []
        self.mat_mass = []
        self.mat_amor = []

    def reinit(self, modl):
        """Copie (essaye) un modele"""
        self.orig_modl = modl
        self.orig_mail = None
        self.concepts = {}  # mapping nom concepts orig->nouvel obj
        self.matr_elem = {}  # mapping matr_elem_orig.nom -> option

    def copy(self):
        self.create_maillage()
        if self.orig_mail:
            self.concepts[self.orig_mail.nom] = self.maillage

        affe_modl = self.retrieve_affe_model(self.orig_modl)
        self.create_modele(affe_modl)
        self.concepts[self.orig_modl.nom] = self.modele

        # Essaye de reconstruire le modele support+modif en utilisant modlsup
        # comme `modele`
        etapes = self.retrieve_model_param(self.orig_modl.nom.strip())
        self.affe_cara_elem(etapes.get('AFFE_CARA_ELEM', []))
        self.affe_materiau(etapes.get('AFFE_MATERIAU', []))
        self.affe_char_meca(etapes.get('AFFE_CHAR_MECA', []))
        self.calc_matr_elem(etapes.get('CALC_MATR_ELEM', []))

        # on recupere les noms de concepts produits par matr_elem pour detecter ceux utilises
        # par nume_ddl
        nume_args = self.retrieve_nume_ddl(
            self.orig_modl.nom, self.matr_elem.keys())
        self.nume_ddl(nume_args)
        nume_names = [sd.nom for args, sd in nume_args]
        asse_matr = self.retrieve_asse_matrice(nume_names)
        self.asse_matrice(asse_matr)

    def create_maillage(self):
        """Creation du maillage"""
        # version par defaut suppose que self.maillage est initialise
        assert self.maillage is not None

    def create_modele(self, affe):
        """Creation du modele"""
        clear_concept(self.modele.obj)
        affe = convert_args(affe, self.concepts)
        _MDLCPL = AFFE_MODELE(MAILLAGE=self.maillage,
                              AFFE=affe)
        self.modele = _MDLCPL
        self.objects.update(_MDLCPL.nom, _MDLCPL)

    def affe_cara_elem(self, cara):
        # replication des AFFE_CARA_ELEM
        # ------------------------------
        for args, sd in cara:
            args = convert_args(args, self.concepts)
            _TMP = AFFE_CARA_ELEM(**args)
            self.concepts[sd.nom] = _TMP

    def affe_materiau(self, mater):
        # replication des AFFE_MATERIAU
        # -----------------------------

        from Cata.cata import AFFE_MATERIAU

        if not mater:
            raise RuntimeError("MODELE est un attribut facultatif de AFFE_MATERIAU \n"
                               "mais necessaire pour cette methode!"
                               )
        for args, sd in mater:
            args = convert_args(args, self.concepts)
            _TMP = AFFE_MATERIAU(**args)
            self.concepts[sd.nom] = _TMP

    def affe_char_meca(self, charge):
        # replication de AFFE_CHAR_MECA
        # -----------------------------

        from Cata.cata import AFFE_CHAR_MECA

        for args, sd in charge:
            args = convert_args(args, self.concepts)
            _TMP = AFFE_CHAR_MECA(**args)
            self.concepts[sd.nom] = _TMP

    def calc_matr_elem(self, matr_elem):
        for args, sd in matr_elem:
            args = convert_args(args, self.concepts)
            _TMP = CALC_MATR_ELEM(**args)
            self.concepts[sd.nom] = _TMP
            self.matr_elem[sd.nom] = args['OPTION']

    def nume_ddl(self, args_lst):
        for args, sd in args_lst:
            args = convert_args(args, self.concepts)
            _TMP = NUME_DDL(**args)
            self.concepts[sd.nom] = _TMP
            self.nume_lst.append(_TMP)

    def asse_matrice(self, args_lst):
        for args, sd in args_lst:
            # dump_mc( args )
            mat_elem = args['MATR_ELEM']
            args = convert_args(args, self.concepts)
            # dump_mc( args )
            _TMP = ASSE_MATRICE(**args)
            self.concepts[sd.nom] = _TMP
            typ_elem = self.matr_elem.get(mat_elem.nom, None)
            if typ_elem == 'RIGI_MECA':
                self.mat_rigi.append(_TMP)
            elif typ_elem == 'MASS_MECA':
                self.mat_mass.append(_TMP)
            elif typ_elem == 'AMOR_MECA':
                self.mat_amor.append(_TMP)

    def retrieve_affe_model(self, modl):
        name = modl.nom.strip()
        jdc = CONTEXT.get_current_step().jdc
        for etape in jdc.etapes:
            if not hasattr(etape, 'sd'):
                continue
            if not hasattr(etape.sd, 'nom'):
                continue
            if etape.sd.nom == name:
                cara = etape.valeur.copy()
                return cara['AFFE']

    def retrieve_model_param(self, modname):
        """Renvoie les parametres des macros affectees a un modele

        XXX: le param modele est facultatif dans AFFE_MATERIAU il faut
        donc rechercher aussi les etapes portant sur le maillage (mais
        on peut aussi contraindre l'utilisateur a utiliser MODELE=...
        pour l'instant)
        """
        jdc = CONTEXT.get_current_step().jdc
        etapes = {}

        for etape in jdc.etapes:
            if etape.nom not in ('AFFE_MATERIAU', 'AFFE_CARA_ELEM',
                                 'AFFE_CHAR_MECA', 'CALC_MATR_ELEM',):
                continue
            args = etape.valeur
            modl = etape.valeur.get('MODELE', None)
            etape_modl_name = obj_get_name(modl)

            if etape_modl_name != modname:
                continue
            lst = etapes.setdefault(etape.nom, [])
            lst.append((args.copy(), etape.sd))
        return etapes

    def retrieve_nume_ddl(self, modname, noms_matr_elem):
        """Renvoie les parametres des macros affectees a un modele

        XXX: le param modele est facultatif dans AFFE_MATERIAU il faut
        donc rechercher aussi les etapes portant sur le maillage (mais
        on peut aussi contraindre l'utilisateur a utiliser MODELE=...
        pour l'instant)
        """
        jdc = CONTEXT.get_current_step().jdc
        nume_ddls = []
        modname = modname.strip()
        noms_matr_elem = [nom.strip() for nom in noms_matr_elem]
        for etape in jdc.etapes:
            if etape.nom != 'NUME_DDL':
                continue
            args = etape.valeur
            modl = etape.valeur.get('MODELE', None)
            etape_modl_name = obj_get_name(modl)
            rigi = etape.valeur.get('MATR_RIGI', None)
            rigi_meca_name = obj_get_name(rigi)

            if etape_modl_name == modname or rigi_meca_name in noms_matr_elem:
                nume_ddls.append((args.copy(), etape.sd))
        return nume_ddls

    def retrieve_asse_matrice(self, nume_names):
        """Renvoie les parametres des macros affectees a un modele

        XXX: le param modele est facultatif dans AFFE_MATERIAU il faut
        donc rechercher aussi les etapes portant sur le maillage (mais
        on peut aussi contraindre l'utilisateur a utiliser MODELE=...
        pour l'instant)
        """
        jdc = CONTEXT.get_current_step().jdc
        asse_matrices = []
        nume_names = [name.strip() for name in nume_names]
        for etape in jdc.etapes:
            if etape.nom != 'ASSE_MATRICE':
                continue
            args = etape.valeur
            # print "FOUND ASSE_MATRICE:"
            # dump_mc( args )
            numeddl = etape.valeur.get('NUME_DDL', None)
            nume_ddl_name = obj_get_name(numeddl)

            if nume_ddl_name in nume_names:
                asse_matrices.append((args.copy(), etape.sd))
        return asse_matrices


class CreateModeleCouple(CopyModelMeca):

    def reinit(self, modl, mail1, mail2, mail3):
        CopyModelMeca.reinit(self, modl)
        self.mail1 = mail1
        self.mail2 = mail2
        self.sumail = mail3

    def create_maillage(self):
        """Creation du maillage"""
        clear_concept(self.maillage)
        self.maillage = None
        # Concept de sortie, ne pas changer de nom sans changer le DeclareOut
        _MLCPL = ASSE_MAILLAGE(MAILLAGE_1=self.mail1,
                               MAILLAGE_2=self.mail2,
                               OPERATION='SOUS_STR')
        self.maillage = _MLCPL
        self.concepts[self.mail1.nom] = _MLCPL
        self.objects.update(_MLCPL.nom, _MLCPL)

    def create_modele(self, affe):
        """Creation du modele"""
        clear_concept(self.modele)
        self.modele = None
        # Concept de sortie, ne pas changer de nom sans changer le DeclareOut
        _MDLCPL = AFFE_MODELE(MAILLAGE=self.maillage,
                              AFFE=affe,
                              AFFE_SOUS_STRUC=_F(SUPER_MAILLE=self.sumail,
                                                 PHENOMENE='MECANIQUE',),
                              )
        self.modele = _MDLCPL
        self.objects.update(_MDLCPL.nom, _MDLCPL)

    def nume_ddl(self, args_lst):
        if len(args_lst) != 1:
            raise RuntimeError("Plusieurs, ou aucun, NUME_DDL presents, "
                               "on ne peut en avoir qu'un")
        args, sd = args_lst[0]
        for nume in self.nume_lst:
            clear_concept(nume)
        self.nume_lst = []
        args = convert_args(args, self.concepts)
        _NUMCPL = NUME_DDL(**args)
        self.concepts[sd.nom] = _NUMCPL
        self.nume_lst.append(_NUMCPL)
        self.objects.update(_NUMCPL.nom, _NUMCPL)

    def asse_matrice(self, args_lst):
        # cleanup:
        for c in self.mat_rigi + self.mat_mass + self.mat_amor:
            clear_concept(c)
        self.mat_rigi = []
        self.mat_mass = []
        self.mat_amor = []
        for args, sd in args_lst:
            mat_elem = args['MATR_ELEM']
            typ_elem = self.matr_elem.get(mat_elem.nom, None)
            args = convert_args(args, self.concepts)
            if typ_elem == 'RIGI_MECA':
                _RIGIME = ASSE_MATRICE(**args)
                self.mat_rigi.append(_RIGIME)
                self.concepts[sd.nom] = _RIGIME
            elif typ_elem == 'MASS_MECA':
                _MASSME = ASSE_MATRICE(**args)
                self.mat_mass.append(_MASSME)
                self.concepts[sd.nom] = _MASSME
            elif typ_elem == 'AMOR_MECA':
                _AMORME = ASSE_MATRICE(**args)
                self.mat_amor.append(_AMORME)
                self.concepts[sd.nom] = _AMORME


#
#
# PETITS UTILITAIRES #
#
#

def clear_concept(objet):
    """!Detruit un concept aster directement, ou encapsule
        une classe CALC_ESSAI"""

    if objet is None:
        return
    try:
        cpt = objet.obj
    except AttributeError:
        cpt = objet
    DETRUIRE(CONCEPT=_F(NOM=cpt), INFO=1)


def convert_args(mc, concepts):
    """convertit un (ensemble) de mot-clefs en remplacant les
    objets presents dans 'concepts' par les valeurs associees
    """
    if isinstance(mc, (dict, _F)):
        if isinstance(mc, dict):
            dest = {}
        else:
            dest = _F()
        for k, v in mc.items():
            v = convert_args(v, concepts)
            dest[k] = v
        return dest
    elif isinstance(mc, (int, float, str, unicode)):
        return mc
    elif isinstance(mc, ASSD):
        if mc.nom in concepts:
            return concepts[mc.nom]
        return mc
    elif isinstance(mc, (list, tuple)):
        lst = []
        for obj in mc:
            lst.append(convert_args(obj, concepts))
        if isinstance(mc, tuple):
            lst = tuple(lst)
        return lst
    else:
        raise NotImplementedError("type non reconnu : %r" % mc)


def dump_mc(mc, indent=""):
    if isinstance(mc, (dict, _F)):
        mck = mc.keys()
        mck.sort()
        for k in mck:
            print indent, k, ":"
            dump_mc(mc[k], indent + " ")

    elif isinstance(mc, (int, float, str, unicode)):
        print indent, mc
    elif hasattr(mc, 'nom'):
        print indent, "OBJ(", mc.nom, ")"
    elif isinstance(mc, (list, tuple)):
        print indent, "("
        for obj in mc:
            dump_mc(obj, indent + "  ")
        print indent, ")"
    else:
        print indent, repr(mc)


def retrieve_model_param(modname):
    """Renvoie les parametres des macros affectees a un modele

    """
    # jdc = CONTEXT.get_current_step().jdc
    jdc = CONTEXT.get_current_step()
    etapes = []

    for etape in jdc.etapes:
        if etape.nom not in (
            'AFFE_MATERIAU', 'AFFE_CARA_ELEM', 'AFFE_CHAR_MECA', 'CALC_MATR_ELEM',
        ):
            continue
        args = etape.valeur
        modl = etape.valeur['MODELE']
        if isinstance(modl, str):
            modlname = modl
        else:
            modlname = modl.nom.strip()

        if modlname != modname:
            continue
        etapes.append((etape.nom, args, etape.sd))
    return etapes


def obj_get_name(obj):
    """ utilise par CopyModelMeca """
    if isinstance(obj, str):
        return obj
    elif hasattr(obj, 'nom'):
        return obj.nom.strip()
    return None
