# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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

from Accas import _F
from Utilitai.Utmess import UTMESS
import aster_core
import aster
from Noyau.N_utils import AsType
from Cata.cata import matr_asse_depl_r


def calc_modes_multi_bandes( self, SOLVEUR_MODAL, SOLVEUR,
                                   VERI_MODE, INFO, TITRE, **args):
    """
       Macro-command CALC_MODES, case of the simultaneous iterations method
       over several frequency bands, with optional parallelization.
       Can be used only in the case of vibration modes (TYPE_RESU='DYNAMIQUE')
    """

    MATR_RIGI = args['MATR_RIGI']
    MATR_MASS = args['MATR_MASS']
    CALC_FREQ = args['CALC_FREQ']
    METHODE = SOLVEUR_MODAL['METHODE']

    # ----------------------------------------------------------------------
    #
    # 1a. INITIALISATIONS
    #
    # ----------------------------------------------------------------------
    ier = 0
    dbg = False  # True si on souhaite faire un IMPR_CO intermediaire, False sinon
    lmatphys = True   # True si matrices d'entrée de type matr_asse_depl_r, False sinon
    if (AsType(MATR_RIGI) != matr_asse_depl_r):
        lmatphys = False

    # On importe les definitions des commandes a utiliser dans la macro
    from Modal.mode_iter_simult import MODE_ITER_SIMULT
    EXTR_MODE = self.get_cmd('EXTR_MODE')
    INFO_MODE = self.get_cmd('INFO_MODE')
    MODI_MODELE = self.get_cmd('MODI_MODELE')
    NUME_DDL = self.get_cmd('NUME_DDL')
    if (dbg):
        IMPR_CO = self.get_cmd('IMPR_CO')

    # Recuperation parametres solveur lineaire
    dSolveur = SOLVEUR[0].cree_dict_valeurs(SOLVEUR[0].mc_liste)
    for i in dSolveur.keys():
        if dSolveur[i] == None:
            del dSolveur[i]
    if dSolveur.has_key('TYPE_RESU'):  # because TYPE_RESU is a keyword with a 'global' position
        del dSolveur['TYPE_RESU']
    if dSolveur.has_key('OPTION'):    # because OPTION is a keyword with a 'global' position
        del dSolveur['OPTION']
    if dSolveur.has_key('FREQ'):      # because FREQ can be a keyword with a 'global' position
        del dSolveur['FREQ']
    solveur_lineaire = dSolveur.get('METHODE').strip()
    dSolveur_infomode = dSolveur
    # pour INFO_MODE, le mot-clé facteur SOLVEUR ne doit pas contenir les
    # mot-clés POSTTRAITEMENTS et RESI_RELA
    if dSolveur_infomode.has_key('POSTTRAITEMENTS'):
        del dSolveur_infomode['POSTTRAITEMENTS']
    if dSolveur_infomode.has_key('RESI_RELA'):
        del dSolveur_infomode['RESI_RELA']

    nompro = None
    iocc = 0
    # Rang du processus MPI et taille du MPI_COMM_WORLD
    # Lorsqu'on ne veut q'un niveau de parallelisme (celui du solveur lineaire)
    # on bluffe l'algo en posant rang=0/nbproc=1 pour tous les procs.
    # Cependant si le solveur est autre que MUMPS on s'arrete en erreur.
    if (args['NIVEAU_PARALLELISME'] == 'COMPLET'):
        rang, nbproc = aster_core.MPI_CommRankSize()
        niv_par = 'COMPLET'
    elif (args['NIVEAU_PARALLELISME'] == 'PARTIEL'):
        rang = 0
        nbproc = 1
        niv_par = 'PARTIEL'
        nbproc_real = aster_core.MPI_CommRankSize()[1]
        if ((nbproc_real > 1) & (solveur_lineaire != 'MUMPS')):
            aster.affiche('MESSAGE', 72 * '-')
            UTMESS('F', 'MODAL_14', vali=nbproc_real, valk=solveur_lineaire)
            aster.affiche('MESSAGE', 72 * '-')
    else:
        assert(False)  # Pb parametrage NIVEAU_PARALLELISME

    # Construction de la liste de frequences
    if CALC_FREQ['FREQ']:
        lborne = []
        nnfreq = len(CALC_FREQ['FREQ'])
        for i in range(0, nnfreq):
            lborne.append(CALC_FREQ['FREQ'][i])
    else:
        assert(False)  # Pb parametrage CALC_FREQ

    # ----------------------------------------------------------------------
    #
    # 1b. TRAVAUX PREPARATOIRES AU LANCEMENT DE LA BOUCLE
    #
    # ----------------------------------------------------------------------

    # Modification de la sd_partition pour rendre compatible les niveaux de
    # parallelisme: celui en frequence et celui des calculs ele/assemblage.
    # Pour l'instant on fonctionne en mode 'CENTRALISE' au sein de la macro
    # (ds les sous-communicateurs, les calculs elem/assemblages
    # ne sont pas parallelises).
    # On remettra le mode de fonctionnement initial en fin de Macro.
    if (nbproc > 1):
        nommod, old_prtk1 = recup_modele_partition(MATR_RIGI, dbg)
        sd_modele = None
        sd_modele = self.get_concept(nommod)
        if (sd_modele == None):
            assert(False)  # Pb, on arrive pas a recuperer le nom du modele
        if (old_prtk1 != None):
            motdimo = {}
            motdimo['reuse'] = sd_modele
            motdimo['MODELE'] = sd_modele
            motdimo['PARTITION'] = _F(PARALLELISME='CENTRALISE')
            __modimo = MODI_MODELE(**motdimo)

    # INFO_MODE global sur la liste de frequences
    # Parametres basiques
    motfaci = {}
    motfaci['COMPTAGE'] = _F(METHODE='AUTO',
                             SEUIL_FREQ=CALC_FREQ['SEUIL_FREQ'],
                             NMAX_ITER_SHIFT=CALC_FREQ['NMAX_ITER_SHIFT'],
                             PREC_SHIFT=CALC_FREQ['PREC_SHIFT'],)

    # Gestion des frequences
    ier = gestion_frequence(solveur_lineaire, nnfreq, nbproc)

    # Parametrage du parallelisme pour la couche FORTRAN/MPI
    if (nbproc > 1):
        motfaci['PARALLELISME_MACRO'] = _F(TYPE_COM=1)
    __nbmodi = INFO_MODE(MATR_RIGI=MATR_RIGI,
                         MATR_MASS=MATR_MASS,
                         INFO=INFO,
                         FREQ=lborne,
                         NIVEAU_PARALLELISME=niv_par,
                         SOLVEUR=dSolveur_infomode,
                         **motfaci)

    # Gestion des sous-bandes de frequences et construction (si //) de l'objet
    nbmodeth, nbsb_nonvide, proc_sb_nvide = gestion_sous_bande(
        solveur_lineaire, __nbmodi, nnfreq, nbproc)

    # ----------------------------------------------------------------------
    #
    # 2. BOUCLE DE MODE_ITER_SIMULT(OPTION=BANDE) + NORM_MODE/IMPR_RESU
    #
    # ----------------------------------------------------------------------
    # On ne traite pas les sous-bandes vides (gain de temps et d'affichage!)
    # On affiche un message a la place.
    # Tous les processeurs font la meme chose pour ces sous-bandes vides, par
    # contre, ils se repartissent bien sur les non-vides.
    #
    # RQ MPI:
    # Les procs s'attendent via le MPI_COMM_SPLIT opere en fin de l'INFO_MODE
    # precedent. Ce n'est pas la peine de commencer la boucle si un proc
    # fait defaut.
    # ----------------------------------------------------------------------
    nbmodeef = None
    freq_ini = 1.E+99
    freq_fin = -1.E+99
    motscles = {}
    motscles['FILTRE_MODE'] = []
    numsb_nonvide = 0
    for i in range(0, nnfreq - 1):

        #--------------------------------------------------------------------
        #
        # 2. SOUS-BANDE NON VIDE
        #
        #--------------------------------------------------------------------
        sb_vide = None
        do_loop = None
        if (__nbmodi['NB_MODE', i + 1] == 0):
            sb_vide = True
            do_loop = False
        else:
            numsb_nonvide = numsb_nonvide + 1
            sb_vide = False
            if (((nbproc > 1) & (proc_sb_nvide[rang] == numsb_nonvide)) | (nbproc == 1)):
                do_loop = True
            else:
                do_loop = False

        if ((not sb_vide) & do_loop):
            motscit = {}
            motscfa = {}
            if SOLVEUR_MODAL['DIM_SOUS_ESPACE']:
                motscfa['DIM_SOUS_ESPACE'] = SOLVEUR_MODAL['DIM_SOUS_ESPACE']
            if SOLVEUR_MODAL['COEF_DIM_ESPACE']:
                motscfa['COEF_DIM_ESPACE'] = SOLVEUR_MODAL['COEF_DIM_ESPACE']
            motscfa['FREQ'] = (lborne[i], lborne[i + 1])
            motscfa['TABLE_FREQ'] = __nbmodi
            motscit['CALC_FREQ'] = _F(OPTION='BANDE',
                                      SEUIL_FREQ=CALC_FREQ['SEUIL_FREQ'],
                                      NMAX_ITER_SHIFT=CALC_FREQ[
                                      'NMAX_ITER_SHIFT'],
                                      PREC_SHIFT=CALC_FREQ['PREC_SHIFT'],
                                      **motscfa)

            if VERI_MODE['STURM'] == 'LOCAL':
                motveri = 'OUI'
            else:
                motveri = 'NON'

            motscit['VERI_MODE'] = _F(STOP_ERREUR=VERI_MODE['STOP_ERREUR'],
                                      SEUIL=VERI_MODE['SEUIL'],
                                      STURM=motveri,
                                      PREC_SHIFT=VERI_MODE['PREC_SHIFT'])

            motscit['STOP_BANDE_VIDE'] = args['STOP_BANDE_VIDE']

            OPTION = 'SANS'  # option for detecting rigid body modes
            if METHODE == 'TRI_DIAG':
                if args.has_key('NMAX_ITER_ORTHO'):
                    motscit['NMAX_ITER_ORTHO'] = SOLVEUR_MODAL[
                        'NMAX_ITER_ORTHO']
                if args.has_key('PREC_ORTHO'):
                    motscit['PREC_ORTHO'] = SOLVEUR_MODAL['PREC_ORTHO']
                if args.has_key('PREC_LANCZOS'):
                    motscit['PREC_LANCZOS'] = SOLVEUR_MODAL['PREC_LANCZOS']
                if args.has_key('MAX_ITER_QR'):
                    motscit['NMAX_ITER_QR'] = SOLVEUR_MODAL['NMAX_ITER_QR']
                    if SOLVEUR_MODAL['MODE_RIGIDE'] == 'OUI':
                        OPTION = 'MODE_RIGIDE'
            elif METHODE == 'JACOBI':
                if args.has_key('NMAX_ITER_BATHE'):
                    motscit['NMAX_ITER_BATHE'] = SOLVEUR_MODAL[
                        'NMAX_ITER_BATHE']
                if args.has_key('PREC_BATHE'):
                    motscit['PREC_BATHE'] = SOLVEUR_MODAL['PREC_BATHE']
                if args.has_key('NMAX_ITER_JACOBI'):
                    motscit['NMAX_ITER_JACOBI'] = SOLVEUR_MODAL[
                        'NMAX_ITER_JACOBI']
                if args.has_key('PREC_JACOBI'):
                    motscit['PREC_JACOBI'] = SOLVEUR_MODAL['PREC_JACOBI']
            elif METHODE == 'SORENSEN':
                if args.has_key('NMAX_ITER_SOREN'):
                    motscit['NMAX_ITER_SOREN'] = SOLVEUR_MODAL[
                        'NMAX_ITER_SOREN']
                if args.has_key('PARA_ORTHO_SOREN'):
                    motscit['PARA_ORTHO_SOREN'] = SOLVEUR_MODAL[
                        'PARA_ORTHO_SOREN']
                if args.has_key('PREC_SOREN'):
                    motscit['PREC_SOREN'] = SOLVEUR_MODAL['PREC_SOREN']
            elif METHODE == 'QZ':
                motscit['TYPE_QZ'] = SOLVEUR_MODAL['TYPE_QZ']
            else:
                assert(False)  # Pb parametrage METHODE

            # Parametrage du parallelisme pour la couche FORTRAN/MPI
            # afin d'operer les comm des modes propres et des parametres
            # modaux.
            if (nbproc > 1):
                motscit['PARALLELISME_MACRO'] = _F(TYPE_COM=1,
                                                   IPARA1_COM=numsb_nonvide,
                                                   IPARA2_COM=nbsb_nonvide,
                                                   )

            #-----------------------------------------------------------------
            #
            # 2a. Calcul des modes
            #
            #-----------------------------------------------------------------                                      )
            __nomre0 = MODE_ITER_SIMULT(MATR_RIGI=MATR_RIGI,
                                        MATR_MASS=MATR_MASS,
                                        INFO=INFO,
                                        METHODE=METHODE,
                                        OPTION=OPTION,
                                        SOLVEUR=dSolveur,
                                        **motscit)

            #-----------------------------------------------------------------
            #
            # 2b. Preparation du test de Sturm de l'etape 3a.
            #
            #-----------------------------------------------------------------
            if VERI_MODE['STURM'] in ('GLOBAL', 'OUI'):  # in the case of CALC_MODES on several bands, OUI is reset to GLOBAL
                dicomode = {}
                dicomode = __nomre0.LIST_VARI_ACCES()
                if (len(dicomode['FREQ']) != 0):
                    raux_ini = dicomode['FREQ'][0]
                    raux_fin = dicomode['FREQ'][-1]
                    if (raux_ini < freq_ini):
                        freq_ini = raux_ini
                    if (raux_fin > freq_fin):
                        freq_fin = raux_fin
                else:
                    assert(False)  # Ce test ne marche pas (PB LIST_VARI_ACCES)

            #-----------------------------------------------------------------
            #
            # 2c. Préparation pour la concaténation de l'étape 3b.
            #
            #-----------------------------------------------------------------
            motscles['FILTRE_MODE'].append(_F(MODE=__nomre0,
                                              TOUT_ORDRE='OUI'))

            # Pour verification
            if (dbg):
                IMPR_CO(CONCEPT=_F(NOM=__nomre0))

        #--------------------------------------------------------------------
        #
        # 2. SOUS-BANDE VIDE
        #
        #--------------------------------------------------------------------
        elif (sb_vide):
            if (args['STOP_BANDE_VIDE'] == 'OUI'):
                UTMESS('F', 'MODAL_6', vali=(i + 1,))
            elif (args['STOP_BANDE_VIDE'] == 'NON'):
                aster.affiche('MESSAGE', 72 * '-')
                UTMESS('I', 'MODAL_6', vali=(i + 1,))
                aster.affiche('MESSAGE', 72 * '-')
            else:
                assert(False)  # Pb parametrage STOP_BANDE_VIDE

    #-----------------------------------------------------------------------
    #
    # 3a. Test de sturm effectif
    #
    #-----------------------------------------------------------------------
    if VERI_MODE['STURM'] == 'NON':
        aster.affiche('MESSAGE', 72 * '-')
        UTMESS('I', 'MODAL_2')
        aster.affiche('MESSAGE', 72 * '-')

    elif VERI_MODE['STURM'] == 'LOCAL':
        aster.affiche('MESSAGE', 72 * '-')
        UTMESS('I', 'MODAL_3')
        aster.affiche('MESSAGE', 72 * '-')

    elif VERI_MODE['STURM'] in ('GLOBAL', 'OUI'):  # in the case of CALC_MODES on several bands, OUI is reset to GLOBAL

        # Construction des 2 bornes de la bande a tester
        if (nbmodeth != 0):
            omecor = CALC_FREQ['SEUIL_FREQ']
            precshift = VERI_MODE['PREC_SHIFT']
            freq_ini = (1.0 - precshift) * freq_ini
            freq_fin = (1.0 + precshift) * freq_fin
            if (abs(freq_ini) < omecor):
                freq_ini = -omecor
            if (abs(freq_fin) < omecor):
                freq_fin = omecor

            # Parametrage du parallelisme pour la couche FORTRAN/MPI
            if (nbproc > 1):
                motfaci['PARALLELISME_MACRO'] = _F(TYPE_COM=2)
            __nbmodf = INFO_MODE(MATR_RIGI=MATR_RIGI,
                                 MATR_MASS=MATR_MASS,
                                 INFO=INFO,
                                 FREQ=(freq_ini, freq_fin),
                                 NIVEAU_PARALLELISME = niv_par,
                                 SOLVEUR =dSolveur_infomode,
                                 **motfaci)

            # Recuperation du nbre de modes donnes par STURM global
            nbmodesg = __nbmodf['NB_MODE', 1]
            if (nbmodeth == nbmodesg):
                aster.affiche('MESSAGE', 72 * '-')
                UTMESS('I', 'MODAL_4', valr=(
                    freq_ini, freq_fin), vali=(nbmodesg, nbmodeth))
                aster.affiche('MESSAGE', 72 * '-')
            else:
                # Message similaire a ALGELINE5_24 pour le FORTRAN
                if (VERI_MODE['STOP_ERREUR'] == 'OUI'):
                    UTMESS('F', 'MODAL_5', valr=(
                        freq_ini, freq_fin, precshift), vali=(nbmodesg, nbmodeth))
                elif (VERI_MODE['STOP_ERREUR'] == 'NON'):
                    UTMESS('A', 'MODAL_5', valr=(
                        freq_ini, freq_fin, precshift), vali=(nbmodesg, nbmodeth))
                else:
                    assert(False)  # Pb parametrage VERI_MODE

        # La bande globale est vide
        else:
            aster.affiche('MESSAGE', 72 * '-')
            UTMESS('I', 'MODAL_7', valr=(lborne[0], lborne[nnfreq - 1]))
            aster.affiche('MESSAGE', 72 * '-')
    else:
        assert(False)  # Pb parametrage STURM

    #-----------------------------------------------------------------------
    #
    # 3b. Concaténation des résultats
    #
    #-----------------------------------------------------------------------
    self.DeclareOut('modes', self.sd)

    modes = EXTR_MODE(**motscles)

    # ----------------------------------------------------------------------
    #
    # 3c. Epilogue
    #
    # RQ MPI:
    # Si la SD_PARTITION existait avant de rentrer ds la Macro on remet ses
    # anciennes valeurs pour continuer la suite du fichier de commande.
    #----------------------------------------------------------------------
    if (nbproc > 1):
        if (old_prtk1 != None):
            motdimo = {}
            motdimo['reuse'] = sd_modele
            motdimo['MODELE'] = sd_modele
            motdimo['PARTITION'] = _F(PARALLELISME=old_prtk1)
            __modimo = MODI_MODELE(**motdimo)

    return modes


#----------------------
# Routines auxiliaires
#----------------------
# Routine pour recuperer sd_modele + option de la sd_partition (si elle existe)
def recup_modele_partition(MATR_RIGI, dbg):

    nommod = None
    old_prtk1 = None
    # Recuperation de la sd_modele a partir de la matrice de rigidite
    # sur le schema des routines DISMOI/DISMMO/DISMMS/DISMNU/DISMPN/DISMLG
    matr_refa = MATR_RIGI.sdj.REFA.get()
    nume_lili = matr_refa[1][0:14] + '.NUME.LILI'
    vlili = aster.getvectjev(nume_lili[0:24])
    buff_ligrel = vlili[1]
    if (buff_ligrel == 'LIAISONS'):
        assert(False)  # Pb, on arrive pas a recuperer le nom du modele
    vlgrf = buff_ligrel[0:19] + '.LGRF'
    buff_modele = aster.getvectjev(vlgrf[0:24])
    nommod = buff_modele[1]

    if (dbg):
        aster.affiche('MESSAGE', "Nom du modele retenu: " + str(nommod))

    # Si parallele non centralise recuperation de lasd_partition et de son
    # option de partitionnement: old_prtk1
    sd_partitb = None
    nompart = None
    vprtk = None
    vpartit = nommod[0:8] + '.PARTIT'
    sd_partitb = aster.getvectjev(vpartit)
    if (sd_partitb != None):
        nompart = sd_partitb[0]
        if (nompart != None):
            vprtk = aster.getvectjev(nompart[0:8] + '.PRTK')
            if (vprtk != None):
                old_prtk_buff = vprtk[0]
                old_prtk1 = old_prtk_buff.strip()
    if (dbg):
        aster.affiche(
            'MESSAGE', "Nom de la partition retenue: " + str(nompart))
        aster.affiche(
            'MESSAGE', "Ancienne option de distribution: " + str(old_prtk1))

    return nommod, old_prtk1

# Routine pour recuperer nbre de modes theorique (nbmodeth) determine par l'INFO_MODE
# prealable + le nbre de sous_bandes non vides (nbsd_nonvide) + msg d'erreurs associes.
# De plus, lorsque le mode parallele est active, elle gere les incompatibilites
# entre le nombre de sous-bandes, le nombre de processeurs et la methode utilisee
# pour le solveur lineaire.
# En mode parallele, elle remplit le vecteur proc_sb_nvide de taille le nbproc.
# proc_sb_nvide(i)=numero de la sbande non vide traitee par le proc de rang i.
# Regle 1: On garde les procs travaillant sur la meme sbande contigues.
# Regle 2: En cas de desequilibre de charge, on surcharge les premieres sbandes.
# RQ. Meme regle que pour la distribution frequence/proc operee ds le F77:
# cf. op0032.


def gestion_sous_bande(solveur_lineaire, __nbmodi, nnfreq, nbproc):

    nbsb_nonvide = None
    proc_sb_nvide = []
    # Recuperation du nbre de modes total theorique
    nbmodeth = 0
    for i in range(0, nnfreq - 1):
        nbmodeth = nbmodeth + __nbmodi['NB_MODE', i + 1]

    # Recuperation du nbre de sous-bandes non vides
    nbsb_nonvide = 0
    for i in range(0, nnfreq - 1):
        if (__nbmodi['NB_MODE', i + 1] != 0):
            nbsb_nonvide = nbsb_nonvide + 1

    if ((nbmodeth == 0) | (nbsb_nonvide == 0)):
        aster.affiche('MESSAGE', 72 * '-')
        if (args['STOP_BANDE_VIDE'] == 'OUI'):
            UTMESS('F', 'MODAL_8', valr=(lborne[0], lborne[nnfreq - 1]))
        elif (args['STOP_BANDE_VIDE'] == 'NON'):
            UTMESS('A', 'MODAL_8', valr=(lborne[0], lborne[nnfreq - 1]))
        else:
            assert(False)  # Pb parametrage STOP_BANDE_VIDE
        aster.affiche('MESSAGE', 72 * '-')

    if (nbproc > 1):
        if ((nbproc < nbsb_nonvide) | ((nbproc > nbsb_nonvide) & (solveur_lineaire != 'MUMPS'))):
            aster.affiche('MESSAGE', 72 * '-')
            UTMESS('F', 'MODAL_9', vali=(
                nbproc, nbsb_nonvide), valk=solveur_lineaire)
            aster.affiche('MESSAGE', 72 * '-')
        div = None
        reste = None
        div = nbproc / nbsb_nonvide
        reste = nbproc - nbsb_nonvide * div
        if ((nbproc > nbsb_nonvide) & (reste != 0)):
            aster.affiche('MESSAGE', 72 * '-')
            UTMESS('I', 'MODAL_12', vali=(nbsb_nonvide, div, div + 1),)
            aster.affiche('MESSAGE', 72 * '-')

        l1 = nbproc / nbsb_nonvide
        l11 = l1 + 1
        l2 = nbproc - (l1 * nbsb_nonvide)
        l21 = l2 + 1
        num_sb = 0
        for i in range(0, l2):
            num_sb = num_sb + 1
            for j in range(0, l11):
                proc_sb_nvide.append(num_sb)

        for i in range(l2, nbsb_nonvide):
            num_sb = num_sb + 1
            for j in range(0, l1):
                proc_sb_nvide.append(num_sb)

    else:
        proc_sb_nvide.append(-999)

    return nbmodeth, nbsb_nonvide, proc_sb_nvide

# Routine pour gerer, lorsque le mode parallele est active, les incompatibilites
# entre le nombre de frequences, le nombre de processeurs et la methode utilisee
# pour le solveur lineaire.


def gestion_frequence(solveur_lineaire, nnfreq, nbproc):

    ier = 0
    if (nbproc > 1):
        if ((nbproc < nnfreq - 1) | ((nbproc > nnfreq - 1) & (solveur_lineaire != 'MUMPS'))):
            aster.affiche('MESSAGE', 72 * '-')
            UTMESS('F', 'MODAL_10', vali=(
                nbproc, nnfreq), valk=solveur_lineaire)
            aster.affiche('MESSAGE', 72 * '-')
        div = None
        reste = None
        div = nbproc / (nnfreq - 1)
        reste = nbproc - (nnfreq - 1) * div
        if ((nbproc > nnfreq - 1) & (reste != 0)):
            aster.affiche('MESSAGE', 72 * '-')
            UTMESS('I', 'MODAL_11', vali=(nnfreq - 1, div, div + 1),)
            aster.affiche('MESSAGE', 72 * '-')

    return ier
