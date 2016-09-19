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
# person_in_charge: serguei.potapov at edf.fr

"""
    CALC_EUROPLEXUS
"""

debug = False

import types, string
import os
import os.path as osp
import copy
import tempfile

# Protection pour Eficas
try:
    import aster
    from Accas import _F
    from Utilitai.partition import MAIL_PY
    from Utilitai.Utmess import UTMESS
    from Calc_epx.calc_epx_utils import tolist
except:
    pass

#-----------------------------------------------------------------------
#----------------------------- Operateur de la Macro-commande ----------
#-----------------------------------------------------------------------


def calc_europlexus_ops(self, EXCIT, COMPORTEMENT, ARCHIVAGE, CALCUL,
                        CARA_ELEM=None, MODELE=None,
                        CHAM_MATER=None, FONC_PARASOL=None,
                        OBSERVATION=None, COURBE=None,
                        DOMAINES=None, INTERFACES=None,
                        ETAT_INIT=None, AMORTISSEMENT=None,
                        INFO=1, **args):
    """
        Macro-commande CALC_EUROPLEXUS.
    """

    #
    # PREPARATION
    #

    ier = 0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    global DEFI_FICHIER
    DEFI_FICHIER = self.get_cmd('DEFI_FICHIER')

    # Pour la gestion des Exceptions
    prev_onFatalError = aster.onFatalError()
    aster.onFatalError('EXCEPTION')


    # Pour masquer certaines alarmes
    from Utilitai.Utmess import MasquerAlarme, RetablirAlarme
    MasquerAlarme('MED_1')
    MasquerAlarme('ALGELINE4_43')
    MasquerAlarme('JEVEUX_57')

    # Ligne de commande d'Europlexus
    EXEC = args['LOGICIEL']

    # Version d'Europlexus
    VERS = args['VERSION_EUROPLEXUS']

    # Chemin du repertoire REPE_OUT de l'execution courante d'Aster
    REPE_OUT = os.path.join(os.getcwd(), 'REPE_OUT')

    # Chemin du repertoire temporaire pour l'execution d'EPX
    # (un lien vers REPE_OUT)
    REPE_epx = tempfile.mkdtemp(suffix='_epx')
    os.rmdir(REPE_epx)
    os.symlink(REPE_OUT, REPE_epx)

    # Le concept sortant (de type evol_noli) est nomme 'resu'.
    # Le nom de ce concept sera celui defini par l'utilisateur.
    self.DeclareOut('resu', self.sd)
    global resu

    # On récupère ce nom pour le nommage des fichiers dans REPE_OUT.
    nom_resu = self.sd.get_name()

    #
    # TRADUCTION DES INFORMATIONS
    #

    EPX = EUROPLEXUS(ETAT_INIT, MODELE, CARA_ELEM, CHAM_MATER, COMPORTEMENT,
                     FONC_PARASOL, EXCIT, OBSERVATION, ARCHIVAGE, COURBE,
                     CALCUL, AMORTISSEMENT, DOMAINES, INTERFACES, REPE='REPE_OUT',
                     EXEC=EXEC, VERS=VERS, INFO=INFO, REPE_epx=REPE_epx, NOM_RESU=nom_resu,
                     args=args)

    #
    # ERITURE DU FICHIER DE COMMANDE EUROPLEXUS
    #

    EPX.ecrire_fichier()

    #
    # LANCEMENT DU CALCUL
    #

    if args['LANCEMENT'] == 'OUI':

        EPX.lancer_calcul()

        #
        # COPIE DES RESULTATS EPX DANS LE CONCEPT ASTER
        #

        EPX.get_resu()

        #
        # RECUPERER LES CONCEPTS TABLE
        #

        if COURBE is not None:
            global table
            self.DeclareOut('table', args['TABLE_COURBE'])
            EPX.get_table()


    #
    # MENAGE
    #

    # Pour la gestion des Exceptions
    aster.onFatalError(prev_onFatalError)

    # Pour la gestion des alarmes
    RetablirAlarme('MED_1')
    RetablirAlarme('ALGELINE4_43')
    RetablirAlarme('JEVEUX_57')

    # Suppression du lien symbolique
    os.remove(REPE_epx)

    return ier

#-----------------------------------------------------------------------
#----------------------------- class EUROPLEXUS ------------------------
#-----------------------------------------------------------------------

class EUROPLEXUS:
    """
        Classe gérant la traduction d'un calcul Code_Aster en EPX,
        le lancement du calcul,
        la traduction des résultats dans un concept Aster.
    """

    def __init__(self, ETAT_INIT, MODELE, CARA_ELEM, CHAM_MATER, COMPORTEMENT,
                 FONC_PARASOL, EXCIT, OBSERVATION, ARCHIVAGE, COURBE,
                 CALCUL, AMORTISSEMENT, DOMAINES, INTERFACES, REPE, EXEC, VERS,
                  INFO, REPE_epx, NOM_RESU, args):
        """
            Met toutes les entrées en attributs.
            Crée les directives EPX.
            Définie les fichiers de sortie.
        """
        import aster_core
        from Calc_epx.calc_epx_cata import cata_directives
        from Calc_epx.calc_epx_struc import DIRECTIVE

        if debug:
            print 'args_key %s'%args.keys()

        # Récuperation des concepts de la base
        macro = CONTEXT.get_current_step()

        # Résultat initial
        # MODELE, CARA_ELEM, CHAM_MATER
        self.ETAT_INIT = ETAT_INIT
        if ETAT_INIT is not None:

            RESULTAT = ETAT_INIT['RESULTAT']
            nom_RESU_INIT = RESULTAT.get_name()

            # MODELE
            iret, ibid, nomsd = aster.dismoi('MODELE', nom_RESU_INIT,
                                             'RESULTAT', 'F')
            nomsd = nomsd.strip()
            if nomsd[0] == '#':
                UTMESS('F', 'PLEXUS_37', valk='MODELE')
            self.MODELE = macro.get_concept(nomsd)

            # CARA_ELEM
            if CARA_ELEM is None :
                iret, ibid, nomsd = aster.dismoi('CARA_ELEM', nom_RESU_INIT, 'RESULTAT', 'F')
                nomsd = nomsd.strip()
                if nomsd[:8] == '#PLUSIEU':
                    UTMESS('F', 'PLEXUS_37', valk=[nom_RESU_INIT, 'CARA_ELEM'])
                elif nomsd[:6] == '#AUCUN':
                    self.CARA_ELEM = None
                else:
                    self.CARA_ELEM = macro.get_concept(nomsd)
            else:
                self.CARA_ELEM = CARA_ELEM
                UTMESS('A','PLEXUS_53')

            # CHAM_MATER
            iret, ibid, nomsd = aster.dismoi('CHAM_MATER', nom_RESU_INIT, 'RESULTAT', 'F')
            nomsd = nomsd.strip()
            if nomsd[:8] == '#PLUSIEU':
                UTMESS('F', 'PLEXUS_37', valk=[nom_RESU_INIT, 'CHAM_MATER'])
            self.CHAM_MATER = macro.get_concept(nomsd)
        else:
            self.MODELE     = MODELE
            self.CARA_ELEM  = CARA_ELEM
            self.CHAM_MATER = CHAM_MATER
        #
        # Recherche dans le jdc la création du concept CARA_ELEM
        if ( self.CARA_ELEM != None ):
            FindEtape = False
            self.CARA_ELEM_CONCEPT = self.CARA_ELEM
            nomsd = self.CARA_ELEM.get_name()
            jdc = CONTEXT.get_current_step().jdc
            for UneEtape in jdc.etapes:
                if (UneEtape.nom=='AFFE_CARA_ELEM') and (UneEtape.sdnom==nomsd):
                    self.CARA_ELEM = UneEtape
                    FindEtape = True
                    break
            #
            if ( not FindEtape ):
                UTMESS('F', 'PLEXUS_20', valk=[nomsd, 'CARA_ELEM'])
            #
        else:
            self.CARA_ELEM_CONCEPT = None
        #
        # récuperation du maillage
        nom_MODELE = self.MODELE.get_name()
        iret, ibid, nomsd = aster.dismoi('NOM_MAILLA', nom_MODELE, 'MODELE', 'F')
        nomsd = nomsd.strip()
        self.MAILLAGE = macro.get_concept(nomsd)

        # Autres entrées
        self.FONC_PARASOL = FONC_PARASOL
        self.EXCIT = EXCIT
        self.OBSERVATION = OBSERVATION
        self.ARCHIVAGE = ARCHIVAGE
        self.COURBE = COURBE
        self.CALCUL = CALCUL
        self.DOMAINES = DOMAINES
        self.INTERFACES = INTERFACES
        self.VERS = VERS
        self.INFO = INFO
        self.COMPORTEMENT = COMPORTEMENT
        self.AMORTISSEMENT = AMORTISSEMENT

        self.REPE_epx = REPE_epx

        # Commande d'execution de Europlexus
        tooldir = aster_core.get_option('repout')
        epxExec = EXEC
        if not epxExec:
            epxExec = os.environ.get('ASTER_EUROPLEXUS')
            if epxExec:
                UTMESS('I', 'PLEXUS_13', valk=epxExec)
            else:
                epxExec = osp.join(tooldir, 'europlexus')
        self.EXEC = epxExec

        # COURBE
        if args.has_key('UNITE_COURBE'):
            self.UNITE_COURBE = args['UNITE_COURBE']
        else:
            self.UNITE_COURBE = None

        if args.has_key('PAS_INST_COURBE'):
            self.PAS_INST_COURBE = args['PAS_INST_COURBE']
        else:
            self.PAS_INST_COURBE = None

        if args.has_key('PAS_NBRE_COURBE'):
            self.PAS_NBRE_COURBE = args['PAS_NBRE_COURBE']
        else:
            self.PAS_NBRE_COURBE = None
            
        if args.has_key('INST_COURBE'):
            self.INST_COURBE = args['INST_COURBE']
        else:
            self.INST_COURBE = None
        
        if args.has_key('NUME_ORDRE_COURBE'):
            self.NUME_ORDRE_COURBE = args['NUME_ORDRE_COURBE']
        else:
            self.NUME_ORDRE_COURBE = None

        if args.has_key('TABLE_COURBE'):
            self.TABLE_COURBE = args['TABLE_COURBE']
        else:
            self.TABLE_COURBE = None

        # Création des directives EPX
        self.epx = {}
        for direc in cata_directives:
            titre = cata_directives[direc]['TITRE']
            type_dir = cata_directives[direc]['TYPE_DIR']
            self.epx[direc] = DIRECTIVE(direc, titre, type_dir)

        # Nom des fichiers de Europlexus (commande et sorties)
        nom_fichiers = {'COMMANDE': 'commandes_%s.epx'%NOM_RESU,
                        'MAILLAGE': 'commandes_%s.msh'%NOM_RESU,
                        'SAUV': 'resu_%s.sau'%NOM_RESU,
                        'MED': 'champ_%s.e2m'%NOM_RESU,
                        'PUN': 'courbes_%s.pun'%NOM_RESU,
                        }
        for fic in nom_fichiers.keys():
            nom_fic = nom_fichiers[fic]
            nom_fichiers[fic] = os.path.join(self.REPE_epx, nom_fic)
        self.nom_fichiers = nom_fichiers

        # creation du dictionnaire de données complementaires sur les modélisations
        self.info_mode_compl = {}


  #-----------------------------------------------------------------------
    def get_unite_libre(self,):
        """
            Retoune une unité de fichier libre.
        """
        from Cata.cata import DETRUIRE, INFO_EXEC_ASTER
        _UL = INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
        unite = _UL['UNITE_LIBRE', 1]
        DETRUIRE(CONCEPT=(_F(NOM=_UL),), INFO=1)
        return unite
  #-----------------------------------------------------------------------
    def export_DEBUT(self):
        """
            Ecrit les mot-clés d'initialisation du calcul dans la fausse
            directive DEBUT.
            Active la directive FIN
        """

        from Calc_epx.calc_epx_struc import BLOC_DONNEES
        epx = self.epx

        directive = 'DEBUT'

        vale = ''
        for mcle in ['TITRE', 'ECHO', 'TRID']:
            bloc = BLOC_DONNEES(mcle)
            epx[directive].add_bloc(bloc)

        # lecture fichier MED
        bloc = BLOC_DONNEES('MEDL', cle='28')
        epx[directive].add_bloc(bloc)

        # écriture des résultats EPX en MED
        champ_fact = self.ARCHIVAGE
        if champ_fact is not None:
            bloc = BLOC_DONNEES('MEDE')
            epx[directive].add_bloc(bloc)

        # on traite la directive fin maintenant car elle est toujours présente
        # à la fin
        directive = 'FIN'
        epx[directive].add_void()

  #-----------------------------------------------------------------------
    def export_MAILLAGE(self,):
        """
            Imprime le maillage et le résultat initial aster au format MED
            s'il y a un état initial.
        """
        from Utilitai.Utmess import MasquerAlarme, RetablirAlarme
        from Calc_epx.trans_var_int import var_int_a2e
        from Cata.cata import IMPR_RESU, MODI_REPERE

        epx = self.epx

        # Donner un nom au fichier de maillage parce que le fort.unite peut
        # être ecrasée par d'autre operation d'ecriture.
        unite = self.get_unite_libre()
        fichier_maillage = self.nom_fichiers['MAILLAGE']
        DEFI_FICHIER(UNITE=unite, FICHIER=fichier_maillage, ACTION='ASSOCIER')

        if self.ETAT_INIT is not None:
            RESULTAT = self.ETAT_INIT['RESULTAT']
            res_imp = RESULTAT
            list_cham = ['DEPL']
            if self.ETAT_INIT['CONTRAINTE'] == 'OUI':
                if self.etat_init_cont != []:
                    valk = ', '.join(self.etat_init_cont)
                    UTMESS('A', 'PLEXUS_17', valk = valk)
                list_cham.append('SIEF_ELGA')
                nume_ordre = RESULTAT.LIST_PARA()['NUME_ORDRE'][-1]
                if self.modi_repere['COQUE']:
                    MODI_REPERE(RESULTAT=RESULTAT, reuse=RESULTAT,
                                REPERE='COQUE_UTIL_INTR',
                                NUME_ORDRE = nume_ordre,
                                MODI_CHAM=_F(TYPE_CHAM='COQUE_GENE',
                                             NOM_CHAM='SIEF_ELGA',
                                             NOM_CMP=('NXX', 'NYY', 'NXY',
                                                      'MXX', 'MYY', 'MXY',
                                                      'QX', 'QY')))
                if self.ETAT_INIT['VARI_INT'] == 'OUI':
                    list_cham.append('VARI_ELGA')
                    res_imp = var_int_a2e(self.compor_gr, RESULTAT, self.MODELE,
                                          nume_ordre)
                    nume_ordre = 1

            # Impression des champs du dernier instant de calcul.
            nume_ordre = RESULTAT.LIST_PARA()['NUME_ORDRE'][-1]
            IMPR_RESU(UNITE=unite,
                      FORMAT='MED',
                      RESU=_F(NUME_ORDRE=nume_ordre, RESULTAT=res_imp,
                              NOM_CHAM=list_cham)
                 )

            # on remet les contraintes des coques dans le repère utilisateur
            # pour ne pas modifier le resultat.
            if self.ETAT_INIT['CONTRAINTE'] == 'OUI':
                if self.modi_repere['COQUE']:
                    MODI_REPERE(RESULTAT=RESULTAT, reuse=RESULTAT,
                                REPERE='COQUE_INTR_UTIL',
                                NUME_ORDRE = nume_ordre,
                                MODI_CHAM=_F(TYPE_CHAM='COQUE_GENE',
                                             NOM_CHAM='SIEF_ELGA',
                                             NOM_CMP=('NXX', 'NYY', 'NXY',
                                                      'MXX', 'MYY', 'MXY',
                                                      'QX', 'QY')))
        else:
            # Impression
            IMPR_RESU(UNITE=unite,
                  FORMAT='MED',
                  RESU=_F(MAILLAGE=self.MAILLAGE)
                 )

        DEFI_FICHIER(UNITE=unite, ACTION='LIBERER')

  #-----------------------------------------------------------------------
    def export_MODELE(self):
        """
            Traduction du modèle Code_Aster dans la directive GEOM d'EPX.
        """
        from Calc_epx.calc_epx_geom import export_modele

        [self.epx, self.dic_epx_geom, self.modi_repere,
         self.etat_init_cont] = export_modele(self.epx, self.MAILLAGE,
                                              self.MODELE, self.gmaInterfaces,
                                              self.info_mode_compl)

   #-----------------------------------------------------------------------
    def export_CARA_ELEM(self):
        """
            Traduction des caractéristiques élémentaires de Code_Aster dans
            les directives EPX correspondantes.
        """

        from Calc_epx.calc_epx_cata import cata_cara_elem
        from Calc_epx.calc_epx_cara import export_cara, get_FONC_PARASOL
        from Calc_epx.calc_epx_utils import recupere_structure, angle2vectx
        from Calc_epx.calc_epx_utils import get_group_ma, tolist
        from Calc_epx.calc_epx_poutre import POUTRE
        epx = self.epx

        dic_gr_cara_supp = {}
        # Récuperer s'il a lieu les fonctions de ressorts de sol et discrets
        if self.FONC_PARASOL is not None:
            dic_gr_cara_supp = get_FONC_PARASOL(epx, self.FONC_PARASOL,
                                                dic_gr_cara_supp)
        # récupérer les orientations des poutres
        if self.CARA_ELEM:
            class_poutre = POUTRE(MAILLAGE=self.MAILLAGE, CARA_ELEM=self.CARA_ELEM)
            dic_gr_cara_supp = class_poutre.get_orie_poutre(dic_gr_cara_supp)

        mode_from_cara = {}

        self.dicOrthotropie = None
        # Recuperer la structure du concept sorti de AFFE_CARA_ELEM
        if self.CARA_ELEM is not None:
            cara_elem_struc = recupere_structure(self.CARA_ELEM)

            for cle in cara_elem_struc.keys():
                if cle in ['INFO', 'MODELE']:
                    continue
                if not cata_cara_elem.has_key(cle):
                    UTMESS('F', 'PLEXUS_18', valk=cle)
                if cata_cara_elem[cle] == None:
                    continue
                [epx, mode_from_cara] = export_cara(cle, epx,
                                              cara_elem_struc[cle],
                                              self.MAILLAGE, self.CARA_ELEM_CONCEPT,
                                              dic_gr_cara_supp, mode_from_cara)

                if cle == 'COQUE':
                    # récupérer les orientations des coques
                    # utilisées pour GLRC_DAMAGE
                    dicOrthotropie = {}
                    donnees_coque = tolist(cara_elem_struc[cle])
                    for elem in donnees_coque:
                        l_group = get_group_ma(elem)

                        if elem.has_key('VECTEUR'):
                            for group in l_group:
                                dicOrthotropie[group] = elem['VECTEUR']
                        elif elem.has_key('ANGL_REP'):
                            alpha, beta = elem['ANGL_REP']
                            vect = angle2vectx(alpha, beta)
                            for group in l_group:
                                dicOrthotropie[group] = vect

                    self.dicOrthotropie = dicOrthotropie

        self.info_mode_compl.update(mode_from_cara)


    #-----------------------------------------------------------------------
    def export_CHAM_MATER(self):
        """
            Traduction des comportements de Code_Aster dans la directive MATE.
            Impression des fonctions s'il a lieu.
            Traduction des orientations pour GLRC.
        """
        from Calc_epx.calc_epx_mate import export_mate

        self.epx, self.compor_gr, mode_from_compor, self.gmaInterfaces = export_mate(self.epx, self.CHAM_MATER,
                  self.COMPORTEMENT,self.INTERFACES, self.dicOrthotropie)

        self.info_mode_compl.update(mode_from_compor)

  #-----------------------------------------------------------------------
    def export_EXCIT(self):
        """
            Traduction des chargements et conditions limites de Code_Aster dans
            les directives EPX correspondantes.
        """
        from Calc_epx.calc_epx_char import export_charge
        epx = self.epx
        epx = export_charge(epx, self.EXCIT, self.MAILLAGE)
  #-----------------------------------------------------------------------
    def export_ECRITURE(self):
        """
            Gestion de l'écriture des résultats dans les différents formats
            et fichiers.
        """
        from Calc_epx.calc_epx_struc import BLOC_DONNEES
        from Calc_epx.calc_epx_cata import cata_champs
        from Calc_epx.calc_epx_utils import ctime
        epx = self.epx

        directive = 'ECRITURE'
        # blocs d'écriture de tous les noeuds et toutes les mailles
        [bloc_poin, bloc_elem] = self.write_all_gr()

        # Traitement du mot-cle facteur OBSERVATION (EPX = LISTING)
        # Ecriture LISTING
        if self.OBSERVATION is not None:
            listing_fact = self.OBSERVATION.List_F()[0]
            nom_cham = tolist(listing_fact['NOM_CHAM'])
            
            # champs
            for cham_aster in nom_cham:
                cham_epx = cata_champs[cham_aster]
                bloc_champ = BLOC_DONNEES(cham_epx)
                epx[directive].add_bloc(bloc_champ)
            
            # instants
            blocs_inst = ctime(listing_fact)
            for bloc in blocs_inst:
                epx[directive].add_bloc(bloc)

            # noeuds
            if listing_fact.has_key('TOUT_GROUP_NO'):
                # tous les noeuds du modèle
                if bloc_poin is not None:
                    epx[directive].add_bloc(bloc_poin)
                else:
                    bloc = BLOC_DONNEES('NOPO')
                    epx[directive].add_bloc(bloc)
            elif listing_fact.has_key('GROUP_NO'):
                gr_no = tolist(listing_fact['GROUP_NO'])
                bloc = BLOC_DONNEES('POIN', l_group=gr_no,)
                epx[directive].add_bloc(bloc)
            else:
                bloc = BLOC_DONNEES('NOPO')
                epx[directive].add_bloc(bloc)

            # mailles
            if listing_fact.has_key('TOUT_GROUP_MA'):
                # toutes les mailles du modèle
                if bloc_elem is not None:
                    epx[directive].add_bloc(bloc_elem)
                else:
                    bloc = BLOC_DONNEES('NOEL')
                    epx[directive].add_bloc(bloc)
            elif listing_fact.has_key('GROUP_MA'):
                gr_ma = tolist(listing_fact['GROUP_MA'])
                bloc = BLOC_DONNEES('ELEM', l_group=gr_ma,)
                epx[directive].add_bloc(bloc)
            else:
                bloc = BLOC_DONNEES('NOEL')
                epx[directive].add_bloc(bloc)


        # Ecriture FICHIER ALICE utilisé par le mot-cle facteur COURBE
        courbe_fact = self.COURBE
        if courbe_fact is not None:

            concept_bid = {}
            if self.PAS_NBRE_COURBE:
                concept_bid['PAS_NBRE'] = self.PAS_NBRE_COURBE
            if self.PAS_INST_COURBE:
                concept_bid['PAS_INST'] = self.PAS_INST_COURBE
            if self.INST_COURBE:
                concept_bid['INST'] = self.INST_COURBE
            if self.NUME_ORDRE_COURBE:
                concept_bid['NUME_ORDRE'] = self.NUME_ORDRE_COURBE

            mot_cle = "FICHIER ALIT 11"
            objet = epx[directive].add_mcfact(mot_cle)
            
            # instants
            blocs_inst = ctime(concept_bid)
            for bloc in blocs_inst:
                objet.add_bloc(bloc)

            # Liste les noeuds a postraiter
            lnoeuds = set()
            lmailles = set()
            for courbe in courbe_fact:
                if courbe['GROUP_NO'] != None:
                    grno = courbe['GROUP_NO']
                    if type(grno) == tuple:
                        for el in grno:
                            lnoeuds.add(el)
                    else:
                        lnoeuds.add(grno)
                elif courbe['GROUP_MA'] != None:
                    grma = courbe['GROUP_MA']
                    if type(grma) == tuple:
                        for el in grma:
                            lmailles.add(el)
                    else:
                        lmailles.add(grma)
                else:
                    raise Exception('Erreur : ni noeud ni maille')

            if lnoeuds:
                bloc = BLOC_DONNEES('POIN', l_group=lnoeuds,)
                objet.add_bloc(bloc)
            if lmailles:
                bloc = BLOC_DONNEES('ELEM', l_group=lmailles,)
                objet.add_bloc(bloc)

        # FICHIER MED
        champ_fact = self.ARCHIVAGE
        if champ_fact is not None:

            mot_cle = "FICHIER MED"
            objet = epx[directive].add_mcfact(mot_cle)

            fichier_med = "'%s'"%(self.nom_fichiers['MED'])
            bloc_fic = BLOC_DONNEES(fichier_med)
            objet.add_bloc(bloc_fic)
            # instants
            blocs_inst = ctime(champ_fact)
            for bloc in blocs_inst:
                objet.add_bloc(bloc)

            # tous les groupes de mailles du modèle
            if bloc_poin is not None:
                objet.add_bloc(bloc_poin)
            if bloc_elem is not None:
                objet.add_bloc(bloc_elem)

        # FICHIER SAUV
        mot_cle = 'FICHIER SAUV'
        nom_fic = "'%s'"%(self.nom_fichiers['SAUV'])
        data = [nom_fic, 'LAST']
        bloc = BLOC_DONNEES(mot_cle, cara=data)
        epx[directive].add_bloc(bloc)


    def export_POST_COURBE(self):

        """
            Traitement du mot-clé COURBE dans la directive SORTIE.
        """
        from Calc_epx.calc_epx_struc import BLOC_DONNEES
        from Calc_epx.calc_epx_cata import cata_champs, cata_compo

        # Suite de postraitement permettant d'ecrire des fichiers ASCII
        # des grandeurs demandees

        # Tester si le mot_cle facteur COURBE a ete renseigne
        courbe_fact = self.COURBE
        if courbe_fact is None:
            return

        courbe_fact = courbe_fact.List_F()
        self.nb_COURBE = len(courbe_fact)
        epx = self.epx

        # SUITE
        directive = 'SUITE'
        epx[directive].add_void()

        # INFO_SORTIE
        directive = 'INFO_SORTIE'
        if self.UNITE_COURBE:
            fichier_courbes = os.path.join(self.REPE_epx, 'fort.%s'
                                           % str(self.UNITE_COURBE))
        else:
            fichier_courbes = self.nom_fichiers['PUN']

        bloc = BLOC_DONNEES('RESULTAT', cara='ALICE TEMPS', vale='11')
        epx[directive].add_bloc(bloc)
        bloc = BLOC_DONNEES('OPNF', cara=['FORMAT', "'%s'"%fichier_courbes],
                                    vale=['17', ''])
        epx[directive].add_bloc(bloc)

        # SORTIE
        directive = 'SORTIE'
        objet = epx[directive].add_mcfact('GRAPHIQUES')
        bloc = BLOC_DONNEES('AXTEMPS', cle="1. 'TEMPS(s)'")
        objet.add_bloc(bloc)

        # Dictionnaire décrivant les légendes des abscisses et ordodonnees
        # des courbes imprimées et utilisées dans get_tables.
        self.legend_courbes = {}
        dic_entite = {'GROUP_NO' : 'NOEUD', 'GROUP_MA' : 'ELEM'}
        nb_courbe = 0
        lnoeuds = []
        nb_char_lim_pun = 16
        for i_courbe,courbe in enumerate(courbe_fact):
            for entite_type in dic_entite.keys():
                if courbe.has_key(entite_type):
                    entite = courbe[entite_type]
                    cham_aster = courbe['NOM_CHAM']
                    cmp_aster = courbe['NOM_CMP']
                    cham_epx = cata_champs[cham_aster]
                    if not cata_compo[cham_aster].has_key(cmp_aster):
                        UTMESS('F', 'PLEXUS_38', valk=[cham_aster, cmp_aster])
                    cmp_epx = cata_compo[cham_aster][cmp_aster]
                    label = courbe['NOM_COURBE']
                    entite = tolist(entite)
                    ll = len(label)
                    if ll > nb_char_lim_pun:
                        UTMESS('A', 'PLEXUS_21', vali = [i_courbe+1, nb_char_lim_pun])
#                   on laisse la boucle meme s'il ne peut y avoir qu'un seul groupe
                    for el in entite:
                        # COURBE
                        nb_courbe += 1
                        mot_cle = 'COURBE'
                        cara = [cham_epx, 'COMP', ]
                        vale = ['', cmp_epx, ]
                        if entite_type == 'GROUP_MA':
                            cara.append('GAUSS')
                            num_gauss = courbe['NUM_GAUSS']
                            if type(num_gauss) is tuple:
                                num_gauss = num_gauss[0]
                            vale.append(num_gauss)
                        cara.append(dic_entite[entite_type])
                        vale.append('')
                        val_cle = "'%s'"%label
                        bloc_courbe = BLOC_DONNEES(mot_cle, l_group=el,
                                                   cle=nb_courbe,
                                                   val_cle=val_cle, cara=cara,
                                                   vale=vale)
                        objet.add_bloc(bloc_courbe)
                        # LIST
                        mot_cle = 'LIST'
                        cara = 'AXES 1.'
                        vale = "'%s'"%label
                        bloc_liste = BLOC_DONNEES(mot_cle, val_cle=nb_courbe,
                                                   cara=cara, vale=vale)
                        objet.add_bloc(bloc_liste)
                        self.legend_courbes[nb_courbe] = ['TEMPS', label]

  #-----------------------------------------------------------------------
    def export_CALCUL(self):
        """
            Traduit les informations de lancement du calcul.
        """

        from Calc_epx.calc_epx_struc import BLOC_DONNEES, BLOC_DONNEES_SUP
        from Calc_epx.calc_epx_cata import cata_calcul

        epx = self.epx

        liste_mots_cles_CALCUL = self.CALCUL.List_F()[0]

        # ETAT_INIT
        if self.ETAT_INIT is not None:
            directive = 'INIT'
            epx[directive].add_info_dir('MEDL')
            if self.ETAT_INIT['CONTRAINTE'] == 'OUI':
                bloc = BLOC_DONNEES('CONT')
                epx[directive].add_bloc(bloc)
                if self.ETAT_INIT['VARI_INT'] == 'OUI':
                    bloc = BLOC_DONNEES('ECRO')
                    epx[directive].add_bloc(bloc)
            else:
                niter = self.ETAT_INIT['NITER']
                bloc = BLOC_DONNEES('NITER', cle=niter)
                epx[directive].add_bloc(bloc)
            if self.ETAT_INIT['EQUILIBRE'] == 'OUI':
                bloc = BLOC_DONNEES('EQUI')
                epx[directive].add_bloc(bloc)

        # OPTION
        directive = 'OPTION'
        type_discr = liste_mots_cles_CALCUL['TYPE_DISCRETISATION']
        bloc = BLOC_DONNEES('PAS', cle=type_discr)
        epx[directive].add_bloc(bloc)

        if  type_discr == 'AUTO':
            cstab = liste_mots_cles_CALCUL['CSTAB']
            bloc = BLOC_DONNEES('CSTAB', cle=cstab)
            epx[directive].add_bloc(bloc)

        if self.AMORTISSEMENT is not None:
            liste_mots_cles_AMOR = self.AMORTISSEMENT.List_F()[0]
            type_amor = liste_mots_cles_AMOR['TYPE_AMOR']
            if type_amor == 'QUASI_STATIQUE':
                freq = liste_mots_cles_AMOR['FREQUENCE']
                coef = liste_mots_cles_AMOR['COEF_AMOR']
                if liste_mots_cles_AMOR.has_key('INST_DEB_AMOR'):
                    deb_amor = liste_mots_cles_AMOR['INST_DEB_AMOR']
                    fin_amor = liste_mots_cles_AMOR['INST_FIN_AMOR']
                    cara = ['FROM', 'UPTO']
                    vale = [deb_amor, fin_amor]
                else:
                    cara = []
                    vale = []
                coef = liste_mots_cles_AMOR['COEF_AMOR']
                bloc = BLOC_DONNEES('QUASI STATIQUE', cle=freq, val_cle=coef,
                                    cara=cara, vale=vale)
                epx[directive].add_bloc(bloc)
            else:
                raise Exception("Type d'amortissement non programmé")

        # STRUCTURE
        directive = 'STRUCTURE'
        listInterfaces = self.INTERFACES
        listDomaines = self.DOMAINES
        domaineInterfaces = {}
        if listDomaines:
            epx[directive].add_info_dir(len(listDomaines))
            for interface in listInterfaces:
                Lgma1 = tolist(interface['GROUP_MA_1'])
                Lgma2 = tolist(interface['GROUP_MA_2'])
                idS1 = interface['IDENT_DOMAINE_1']
                idS2 = interface['IDENT_DOMAINE_2']
                if not domaineInterfaces.has_key(idS1):
                    domaineInterfaces[idS1] = []
                if not domaineInterfaces.has_key(idS2):
                    domaineInterfaces[idS2] = []
                domaineInterfaces[idS1].extend(Lgma1)
                domaineInterfaces[idS2].extend(Lgma2)
        else:
            listDomaines = []
        for domaine in listDomaines:
            Lgma = tolist(domaine['GROUP_MA'])
            id = domaine['IDENTIFIANT']
            Lgma.extend(domaineInterfaces[id])

            mot_cle = 'DOMA'
            cara = 'IDENTIFIANT'
            vale = id
            bloc = BLOC_DONNEES(mot_cle, l_group=Lgma, cara=cara, vale=vale,
                              lect_term='debut')
            epx[directive].add_bloc(bloc)

        # INTERFACE
        directive = 'INTERFACE'
        if listInterfaces:
            epx[directive].add_info_dir(len(listInterfaces))
        else:
            listInterfaces = []
        for interface in listInterfaces:
            Lgma1 = tolist(interface['GROUP_MA_1'])
            Lgma2 = tolist(interface['GROUP_MA_2'])
            idS1 = interface['IDENT_DOMAINE_1']
            idS2 = interface['IDENT_DOMAINE_2']
            tole = interface['TOLE']
            mot_cle = 'DOMA'
            bloc1 = BLOC_DONNEES(mot_cle, l_group=Lgma1, val_cle=idS1)
            bloc2 = BLOC_DONNEES(mot_cle, l_group=Lgma2, val_cle=idS2)
            mot_cle = 'MORTAR'
            cle = 'TOLE'
            bloc_sup = BLOC_DONNEES_SUP(mot_cle, cle=cle, val_cle=tole,
                                      l_BD=[bloc1, bloc2])
            epx[directive].add_bloc(bloc_sup)

        # CALCUL
        directive = 'CALCUL'
        for cle in cata_calcul.keys():
            if liste_mots_cles_CALCUL.has_key(cle):
                vale = liste_mots_cles_CALCUL[cle]
                bloc = BLOC_DONNEES(cata_calcul[cle], cle=vale)
                epx[directive].add_bloc(bloc)

  #-----------------------------------------------------------------------
    def ecrire_fichier(self,):
        """
            Lance la traduction des données et leurs stockages.
            Ecrit le fichier de commande directive par directive dans l'ordre
            donné par la liste locale 'directives'.
        """
        fichier = self.nom_fichiers['COMMANDE']

        # Les modules MODELE et RIGI_PARASOL doivent être exécutés avant
        # MAILLAGE car le maillage peut être modifié dans ces modules (ajout de
        # groupes uniquement).
        # Les modules CARA_ELEM et CHAM_MATER doivent être exécutés avant MODELE
        # pour connaître la modelisation à affecter à certains éléments.
        # Le module CHAM_MATER doit être exécuté avant MAILLAGE pour avoir
        # les infos permettant de traduire les variables internes.

        modules_exe = ['DEBUT', 'CARA_ELEM', 'CHAM_MATER', 'MODELE',
                       'MAILLAGE', 'EXCIT', 'ECRITURE', 'CALCUL',
                       'POST_COURBE']
        directives = ['DEBUT', 'GEOM', 'COMPLEMENT', 'FONC', 'MATE',
                      'ORIENTATION', 'CHARGE', 'LINK', 'ECRITURE', 'OPTION',
                      'INIT', 'STRUCTURE', 'INTERFACE', 'CALCUL', 'SUITE',
                      'INFO_SORTIE', 'SORTIE', 'FIN']



        # Excecution des differentes modules
        for module in modules_exe:
            fct = 'export_%s' % module
            if hasattr(self, fct):
                eval('self.'+fct+'()')
            else:
                raise Exception("La classe EUROPLEXUS n'a pas de méthode %s"
                                                                       % fct)

        # Ecriture des directives
        fd = open(fichier, 'w')
        for directive in directives:
            liste_lignes = self.epx[directive].write()
            for ll in liste_lignes:
                fd.write('%s\n'%ll)
        fd.close()

  #-----------------------------------------------------------------------
    def get_table(self, icourbe=1,):
        """
            Transforme les courbes écrites dans le fichier .pun par EPX
            en table Code_Aster.
        """

        from Calc_epx.calc_epx_utils import lire_pun
        from Cata.cata import CREA_TABLE, IMPR_TABLE
        global table

        if not hasattr(self, 'courbes'):
            fichier = self.nom_fichiers['PUN']
            if not os.path.isfile(fichier):
                return
            self.courbes = lire_pun(fichier=fichier)

        if not os.path.isfile(fichier):
            return
        if debug:
            print self.courbes, type(self.courbes)
        nc = 0
        para_ordonnee = []
        dico = []
        for icourbe in self.courbes:
            valeurs = self.courbes[icourbe]
            if debug:
                print 'icourbe = %s ; valeurs = %s'%(icourbe, valeurs)
            if nc == 0:
                para_abscisse = self.legend_courbes[icourbe][0]
                vale_abscisse = valeurs[0,:].tolist()
                if len(para_abscisse ) > 16:
                    para_abscisse  =  para_abscisse[:17]
                dico.append({'TYPE_K': 'K16', 'LISTE_R' : vale_abscisse,
                           'PARA' : para_abscisse})
                para_ordonnee = self.legend_courbes[icourbe][1]
                vale_ordonnee = valeurs[1,:].tolist()
                if len(para_ordonnee) > 16:
                    para_ordonnee =  para_ordonnee[:17]
                dico.append({'TYPE_K':'K16', 'LISTE_R' : vale_ordonnee,
                           'PARA' : para_ordonnee})
                nc = 1
            else:
                if ((self.legend_courbes[icourbe][0] == para_abscisse) and
                  (vale_abscisse == valeurs[0,:].tolist())):
                    para_ordonnee = self.legend_courbes[icourbe][1]
                    vale_ordonnee = valeurs[1,:].tolist()
                    if len(para_ordonnee) > 16:
                        para_ordonnee =  para_ordonnee[:17]
                    dico.append({'TYPE_K':'K16', 'LISTE_R' : vale_ordonnee,
                               'PARA' : para_ordonnee})
                else:
                    raise Exception('Table non compatible')

        if len(dico)-1 != self.nb_COURBE:
            UTMESS('A', 'PLEXUS_39')
        table = CREA_TABLE(LISTE=dico)

        # test d'impression de la table
        if False:
            unite = self.get_unite_libre()
            unite = 90
            DEFI_FICHIER(UNITE=unite, ACTION='ASSOCIER')

            IMPR_TABLE(UNITE=unite,
                     FORMAT='XMGRACE',
                     TABLE=table,
                     LEGENDE_X=para_abscisse,
                     LEGENDE_Y=para_ordonnee,
                     LEGENDE='test'
                 )

            os.system('xmgrace fort.%i' % unite)

            DEFI_FICHIER(UNITE=unite, ACTION='LIBERER')


#-----------------------------------------------------------------------
    def get_resu(self,):
        """
            Construit un concept aster evol_noli à partir des résultats du
            calcul EPX contenus dans le fichier MED de sortie.
        """
        from Cata.cata import LIRE_EUROPLEXUS
        import med_aster
        fichier_med = self.nom_fichiers['MED']
        if not os.path.isfile(fichier_med):
            UTMESS('F', 'PLEXUS_14')
        # on complète le test car le fichier est créé au début du calcul
        # mais il est vide.
        dic_champ_med = med_aster.get_nom_champ_med(fichier_med)
        if len(dic_champ_med.keys()) == 0:
            UTMESS('F', 'PLEXUS_14')

        unite = self.get_unite_libre()
        # ca ne marche pas avec ca :
        # DEFI_FICHIER(UNITE=unite, FICHIER=fichier_med, ACTION='ASSOCIER')

        # mais ca marche avec ca
        fort = 'fort.%i' % unite
        if os.path.isfile(fort):
            os.remove(fort)
        os.symlink(fichier_med, fort)


        resu = LIRE_EUROPLEXUS(UNITE_MED=unite,
                            MODELE=self.MODELE,
                            CARA_ELEM=self.CARA_ELEM_CONCEPT,
                            CHAM_MATER=self.CHAM_MATER,
                            COMPORTEMENT=self.COMPORTEMENT,
                            EXCIT=self.EXCIT,
                            INFO=self.INFO,
                            )
        DEFI_FICHIER(UNITE=unite, ACTION='LIBERER')
        os.remove(fort)
#-----------------------------------------------------------------------
    def lancer_calcul(self):
        """Lancement du calcul EPX"""
        from Cata.cata import EXEC_LOGICIEL
        fichier_epx = osp.abspath(self.nom_fichiers['COMMANDE'])
        EXEC_LOGICIEL(LOGICIEL=self.EXEC,
                      ARGUMENT=(fichier_epx, self.VERS, self.REPE_epx),
                      CODE_RETOUR_MAXI=-1,
                      INFO=2)
#-----------------------------------------------------------------------
    def write_all_gr(self,):
        """
            Renvoie deux blocs de données indiquand que la commande
            s'applique à tous les noeuds et mailles du modèle.
        """

        from Calc_epx.calc_epx_struc import BLOC_DONNEES

        entite_geo = {}
        entite_geo['ELEM'] = []
        for model in self.dic_epx_geom.keys():
            if self.dic_epx_geom[model]['RESU_ELEM']:
                entite_geo['ELEM'].extend(self.dic_epx_geom[model]
                                                     ['GROUP_MA'])
        entite_geo['POIN'] = []
        for model in self.dic_epx_geom.keys():
            if self.dic_epx_geom[model]['RESU_POIN']:
                entite_geo['POIN'].extend(self.dic_epx_geom[model]
                                                       ['GROUP_MA'])
        li_blocs = []
        for cle in ['POIN', 'ELEM']:
            if entite_geo[cle] != []:
                bloc = BLOC_DONNEES(cle, l_group=entite_geo[cle],)
            else:
                bloc = None
            li_blocs.append(bloc)

        return li_blocs
