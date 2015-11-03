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

from cataelem.Tools.base_objects import InputParameter, OutputParameter
from cataelem.Tools.base_objects import objects_from_context
import cataelem.Commons.physical_quantities as PHY


# Parametres 'In' :
# -----------------

NOMFIS = InputParameter(phys=PHY.NEUT_K8,
                        comment="""""")

PABSCUR = InputParameter(phys=PHY.ABSC_R, container='MAIL!.ABSC_CURV',
                         comment="""  PABSCUR : ABSCISSES CURVILIGNES POUR UN MAILLAGE FILAIRE
 pour les elements de tuyau
""")

PACCELE = InputParameter(phys=PHY.DEPL_R,
                         comment="""""")

PACCELR = InputParameter(phys=PHY.DEPL_R,
                         comment="""""")

PACCE_M = InputParameter(phys=PHY.DEPL_R,
                         comment=""" ACCELERATIONS POUR T-
""")

PACCKM1 = InputParameter(phys=PHY.DEPL_R,
                         comment=""" ACCELERATION DU NOEUD ITERATION N+1 (POU_D_TGD)
""")

PACCPLU = InputParameter(phys=PHY.DEPL_R,
                         comment=""" ACCELERATION DU NOEUD PAS N (POU_D_TGD)
""")

PALPHAR = InputParameter(phys=PHY.NEUT_R,
                         comment="""""")

PANGREP = InputParameter(phys=PHY.CAORIE,
                         comment="""  PANGREP : ANGLES DEFINISSANT LE NOUVEAU REPERE
""")

PBORNPI = InputParameter(phys=PHY.PILO_R,
                         comment="""""")

PCAARPO = InputParameter(phys=PHY.CAARPO, container='CARA!.CARARCPO',
                         comment="""  PCAARPO : CARACTERISTIQUE DE COUDE (POUTRE),
           NECESSITE DE FOURNIR LE CONCEPT PRODUIT PAR AFFE_CARA_ELEM
  PCAARPO : CARACTERISTIQUE DE COUDE (POUTRE)
""")

PCACABL = InputParameter(phys=PHY.CACABL,
                         comment=""" CARACTERISTIQUES DES ELEMENTS DE CABLE
""")

PCACOQU = InputParameter(phys=PHY.CACOQU, container='CARA!.CARCOQUE',
                         comment=""" PCACOQU  :  PROPRIETES COQUES
 PCACOQU : CARACTERISTIQUE DE COQUE
 PCACOQU : CARACTERISTIQUE DE COQUE
  PCACOQU : CARACTERISTIQUE DE COQUE,
         NECESSITE DE FOURNIR LE CONCEPT PRODUIT PAR AFFE_CARA_ELEM
 Champ de caracteristiques pour les coques. Concept CARA_ELEM
  PCACOQU : CARACTERISTIQUES DE COQUE,
           NECESSITE DE FOURNIR LE CONCEPT PRODUIT PAR AFFE_CARA_ELEM
 CARACTERISTIQUES DES ELEMENTS DE COQUE
  PCACOQU : CARACTERISTIQUE DE COQUE
  PCACOQU : CARACTERISTIQUE DE COQUE,
           NECESSITE DE FOURNIR LE CONCEPT PRODUIT PAR AFFE_CARA_ELEM
 SHELLS: CARACTERISTIQUE DE COQUE
""")

PCADISA = InputParameter(phys=PHY.CADISA,
                         comment="""""")

PCADISK = InputParameter(phys=PHY.CADISK, container='CARA!.CARDISCK',
                         comment="""  PCADISK : CARACTERISTIQUE DE DISCRET,
           NECESSITE DE FOURNIR LE CONCEPT PRODUIT PAR AFFE_CARA_ELEM
  PCADISK : CARACTERISTIQUE DE DISCRET
 MATRICE DE RIGIDITE DES ELEMENTS DISCRET
""")

PCADISM = InputParameter(phys=PHY.CADISM, container='CARA!.CARDISCM',
                         comment=""" PCADISM : CARACTERISTIQUES DE DISCRET, FOURNIES PAR AFFE_CARA_ELEM
  PCADISM : CARACTERISTIQUE DE DISCRET,
           NECESSITE DE FOURNIR LE CONCEPT PRODUIT PAR AFFE_CARA_ELEM
""")

PCAGEPO = InputParameter(phys=PHY.CAGEPO, container='CARA!.CARGEOPO',
                         comment=""" RAYON ET EPAISSAUR POUR LES TUYAUX
 CARACTERISTIQUES DES SECTIONS DE POUTRE CERCLE OU RECTANGLE
  PCAGEPO : CARACTERISTIQUE POUTRES, SECTION RECTANGLE OU CERCLE,
           NECESSITE DE FOURNIR LE CONCEPT PRODUIT PAR AFFE_CARA_ELEM
  PCAGEPO : CARACTERISTIQUE POUTRES, SECTION RECTANGLE OU CERCLE
""")

PCAGNBA = InputParameter(phys=PHY.CAGNBA, container='CARA!.CARGENBA',
                         comment=""" CARACTERISTIQUES GEOMETRIQUES D UNE SECTION DE BARRE
  PCAGNBA : CARACTERISTIQUE DE BARRE,
         NECESSITE DE FOURNIR LE CONCEPT PRODUIT PAR AFFE_CARA_ELEM
  PCAGNBA : CARACTERISTIQUES GEOMETRIQUES D UNE SECTION DE BARRE
        NECESSITE DE FOURNIR LE CONCEPT PRODUIT PAR AFFE_CARA_ELEM
  PCAGNBA : CARACTERISTIQUE DE BARRE,
           NECESSITE DE FOURNIR LE CONCEPT PRODUIT PAR AFFE_CARA_ELEM
  PCAGNBA : CARACTERISTIQUE DE BARRE
""")

PCAGNPO = InputParameter(phys=PHY.CAGNPO, container='CARA!.CARGENPO',
                         comment=""" BEAMS: CARACTERISTIQUES GEOMETRIQUES D'UNE SECTION DE POUTRE
  PCAGNPO : CARACTERISTIQUES SECTION DE POUTRE
 Champ de caracteristiques generales pour les poutres. Concept CARA_ELEM
  PCAGNPO : CARACTERISTIQUES GEOMETRIQUES D'UNE SECTION DE POUTRE
  PCAGNPO : CARACTERISTIQUES GEOMETRIQUES DES SECTIONS DE POUTRES.
           CE CHAMP VIENT GENERALEMENT DE AFFE_CARA_ELEM / POUTRE
  PCAGNPO : CARACTERISTIQUES GEOMETRIQUES D'UNE SECTION DE POUTRE,
           NECESSITE DE FOURNIR LE CONCEPT PRODUIT PAR AFFE_CARA_ELEM
  PCAGNPO : CARACTERISTIQUES GEOMETRIQUES D UNE SECTION DE POUTRE,
           NECESSITE DE FOURNIR LE CONCEPT PRODUIT PAR AFFE_CARA_ELEM
 CARACTERISTIQUES GEOMETRIQUES D UNE SECTION DE POUTRE
""")

PCALCI = InputParameter(phys=PHY.NEUT_I,
                        comment=""" FLAG IF NORM OR NORM*NORM
  PCALCI: CARTE INDICATEUR SI NORME OU SON CARRE
""")

PCAMASS = InputParameter(phys=PHY.CAMASS, container='CARA!.CARMASSI',
                         comment=""" CARACTERISTIQUES ANGLE_MASSIF (ELEMENTS ANISOTROPES)
  PCAMASS : CARACTERISTIQUE DE MASSIF,
           NECESSITE DE FOURNIR LE CONCEPT PRODUIT PAR AFFE_CARA_ELEM
  PCAMASS : CARACTERISTIQUE DE MASSIF
 PCAMASS : CARACTERISTIQUE DE MASSIF
""")

PCAPOUF = InputParameter(phys=PHY.CAPOUF,
                         comment="""""")

PCARCRI = InputParameter(phys=PHY.CARCRI,
                         comment=""" PARAMETRES DE CONVERGENCE POUR MECA_NON_LINE
""")

PCAR_AI = InputParameter(phys=PHY.N480_R,
                         comment=""" XFEM
""")

PCAR_CF = InputParameter(phys=PHY.N120_I,
                         comment=""" XFEM
""")

PCAR_PI = InputParameter(phys=PHY.N120_R,
                         comment=""" XFEM
""")

PCAR_PT = InputParameter(phys=PHY.N120_R,
                         comment=""" XFEM - INFORMATIONS AUX POINTS
""")

PCDTAU = InputParameter(phys=PHY.PILO_R,
                        comment="""""")

PCHAMPG = InputParameter(phys=PHY.NEUT_R,
                         comment=""" GAUSS POINTS
  PCHAMPG : CHAMP AUX PG SUR LEQUEL LA NORME EST CALCULEE
""")

PCHARG = InputParameter(phys=PHY.NEUT_K24,
                        comment="""  PCHARG : CARTE D ADRESSE JEVEUX
""")

PCHDYNR = InputParameter(phys=PHY.DEPL_R,
                         comment="""  PCHDYNR : CHAMP D'ACCELERATIONS
  PCHDYNR : CHAMP D ACCELERATIONS
""")

PCINFDI = InputParameter(phys=PHY.CINFDI, container='CARA!.CARDINFO',
                         comment=""" PCINFDI : INFORMATIONS SUR LES DISCRETS, PAR AFFE_CARA_ELEM
  PCINFDI : INFORMATIONS SUR LES DISCRETS, PAR AFFE_CARA_ELEM
 DISCRETE : INFORMATIONS SUR LES DISCRETS
 PCINFDI : INFORMATIONS SUR LES DISCRETS, PAR AFFE_CARA_ELEM
""")

PCOEFFC = InputParameter(phys=PHY.IMPE_C,
                         comment="""  PCOEFFC : COEF_MULT_C DE LA CHARGE,
           PRESENCE DE CHARGE REPARTIE POUR UNE MODELISATION POUTRE
""")

PCOEFFR = InputParameter(phys=PHY.IMPE_R,
                         comment="""  PCOEFFR : COEF_MULT DE LA CHARGE,
           PRESENCE DE CHARGE REPARTIE POUR UNE MODELISATION POUTRE
""")

PCOEFHF = InputParameter(phys=PHY.COEH_F,
                         comment="""""")

PCOEFHR = InputParameter(phys=PHY.COEH_R,
                         comment="""""")

PCOEFR = InputParameter(phys=PHY.NEUT_R,
                        comment=""" COEFFICIENTS
  PCOEFR : CARTE DE COEFFICIENTS
""")

PCOGAIN = InputParameter(phys=PHY.SIEF_R,
                         comment="""  PCOGAIN : CHAMP DE CONTRAINTES ELGA ANCIEN REPERE
""")

PCOHES = InputParameter(phys=PHY.NEUT_R,
                        comment=""" XFEM
""")

PCONFR = InputParameter(phys=PHY.NEUT_R,
                        comment=""" INFORMATION AUX POINTS POUR LE CONTACT - VOIR MMCHML
""")

PCONOIN = InputParameter(phys=PHY.SIEF_R,
                         comment="""  PCONOIN : CHAMP DE CONTRAINTES ELNO ANCIEN REPERE
""")

PCONSTR = InputParameter(phys=PHY.NEUT_R,
                         comment="""  PCONSTR : CARTE CONSTANTE DU COEFFICIENT DE PONDERATION S
""")

PCONTGM = InputParameter(phys=PHY.SIEF_R,
                         comment="""  PCONTGM : CONTRAINTES INSTANT PRECEDENT (POINTS DE GAUSS)
""")

PCONTGP = InputParameter(phys=PHY.SIEF_R, container='RESU!SIGM_ELGA!N',
                         comment="""  PCONTGP : CONTRAINTES INSTANT ACTUEL (POINTS DE GAUSS)
  PCONTGP : CONTRAINTES INSTANT ACTUEL
""")

PCONTGR = InputParameter(phys=PHY.SIEF_R,
                         comment="""""")

PCONTNM = InputParameter(phys=PHY.SIEF_R,
                         comment="""  PCONTNM : CONTRAINTES INSTANT PRECEDENT
""")

PCONTNO = InputParameter(phys=PHY.SIEF_R,
                         comment="""  PCONTNO : CONTRAINTES AUX NOEUDS INSTANT ACTUEL
""")

PCONTNOD = InputParameter(phys=PHY.SIEF_R,
                          comment="""  PCONTNOD : CONTRAINTES PB. DUAL INSTANT ACTUEL
""")

PCONTNOP = InputParameter(phys=PHY.SIEF_R,
                          comment="""  PCONTNOP : CONTRAINTES PB. PRIMAL INSTANT ACTUEL
""")

PCONTR = InputParameter(phys=PHY.SIEF_R, container='RESU!SIEF_NOEU!N',
                        comment="""  PCONTR : VECTEUR FLUX HYDRAULIQUE AUX NOEUDS
""")

PCONTRG = InputParameter(phys=PHY.SIEF_R,
                         comment="""""")

PCOOR1R = InputParameter(phys=PHY.N120_R,
                         comment="""  PCOOR1R : COORDONNEES DES NOEUDS MAILLE 1
""")

PCOOR2R = InputParameter(phys=PHY.N120_R,
                         comment="""  PCOOR2R : COORDONNEES DES NOEUDS MAILLE 2
""")

PCOURB = InputParameter(phys=PHY.NEUT_R,
                        comment="""""")

PDDEPLA = InputParameter(phys=PHY.DEPL_R,
                         comment=""" INCREMENT DE DEPLACEMENT SOLUTION DE NEWTON
""")

PDDEPLR = InputParameter(phys=PHY.DEPL_R,
                         comment="""""")

PDDLIMC = InputParameter(phys=PHY.DDLI_C,
                         comment="""""")

PDDLIMF = InputParameter(phys=PHY.DDLI_F,
                         comment="""""")

PDDLMUC = InputParameter(phys=PHY.DDLM_C,
                         comment="""""")

PDDLMUR = InputParameter(phys=PHY.DDLM_R,
                         comment="""""")

PDECOU = InputParameter(phys=PHY.NEUT_K8,
                        comment="""""")

PDEFOPL = InputParameter(phys=PHY.EPSI_R,
                         comment="""""")

PDEGAIN = InputParameter(phys=PHY.EPSI_R,
                         comment="""  PDEGAIN : CHAMP DE DEFORMATIONS ELGA ANCIEN REPERE
""")

PDENOIN = InputParameter(phys=PHY.EPSI_R,
                         comment="""  PDENOIN : CHAMP DE DEFORMATIONS ELNO ANCIEN REPERE
""")

PDEPENT = InputParameter(phys=PHY.DEPL_R,
                         comment=""" DEPLACEMENT POUR IMPEDANCE SOL
""")

PDEPINR = InputParameter(phys=PHY.DEPL_R,
                         comment="""""")

PDEPKM1 = InputParameter(phys=PHY.DEPL_R,
                         comment=""" DEPLACEMENT DU NOEUD ITERATION N+1 (POU_D_TGD)
""")

PDEPL0R = InputParameter(phys=PHY.DEPL_R,
                         comment="""""")

PDEPL1R = InputParameter(phys=PHY.DEPL_R,
                         comment="""""")

PDEPLA = InputParameter(phys=PHY.DEPL_R,
                        comment="""""")

PDEPLAC = InputParameter(phys=PHY.DEPL_C, container='RESU!DEPL!N',
                         comment="""  PDEPLAC : DEPLACEMENTS
""")

PDEPLAR = InputParameter(phys=PHY.DEPL_R, container='RESU!DEPL!N',
                         comment="""  PDEPLAR : DEPLACEMENTS INSTANT ACTUEL
 PDEPLAR : DEPLACEMENTS GENERALISES EN THM INSTANT ACTUEL
 PDEPLAR  :  DEPLACEMENT
""")

PDEPLAU = InputParameter(phys=PHY.DEPL_R,
                         comment="""""")

PDEPLAV = InputParameter(phys=PHY.DEPL_R,
                         comment="""""")

PDEPLM = InputParameter(phys=PHY.DEPL_R, container='RESU!DEPL!NM1T',
                        comment="""  PDEPLM : DEPLACEMENTS INSTANT PRECEDENT
""")

PDEPLMR = InputParameter(phys=PHY.DEPL_R,
                         comment=""" PDEPLMR : DEPLACEMENTS GENERALISES EN THM INSTANT PRECEDENT
 DEPLACEMENTS POUR T-
  PDEPLMR : DEPLACEMENTS INSTANT PRECEDENT
 DISPLACEMENT AT BEGINNING OF STEP
""")

PDEPLNO = InputParameter(phys=PHY.DEPL_R,
                         comment=""" champ de deplacement nodal avec degres de libertes XFEM
""")

PDEPLPR = InputParameter(phys=PHY.DEPL_R,
                         comment=""" INCREMENT DE DEPLACEMENT CUMULE DEPUIS LE DEBUT DU PAS DE TEMPS
 CHAMP INUTILE INITIALISE A 0 DANS NMMATR
 DISPLACEMENT FROM BEGINNING OF STEP
  PDEPLPR : DEPLACEMENTS INSTANT ACTUEL
""")

PDEPLR = InputParameter(phys=PHY.DEPL_R, container='RESU!DEPL!N',
                        comment="""  PDEPLR : DEPLACEMENTS INSTANT ACTUEL
 PDEPLR  :  DEPLACEMENT
""")

PDEPL_M = InputParameter(phys=PHY.DEPL_R,
                         comment=""" DEPLACEMENTS POUR T-
""")

PDEPL_P = InputParameter(phys=PHY.DEPL_R,
                         comment=""" INCREMENT DE DEPLACEMENT CUMULE DEPUIS LE DEBUT DU PAS DE TEMPS
""")

PDEPPLU = InputParameter(phys=PHY.DEPL_R, container='RESU!DEPL!N',
                         comment="""  PDEPPLU : DEPLACEMENT
""")

PDERAMG = InputParameter(phys=PHY.DERA_R, container='RESU!DERA_ELGA!NM1',
                         comment="""  PDERAMG : INDICATEUR LOCAL DE DECHARGE INSTANT PRECEDENT
""")

PDGGAIN = InputParameter(phys=PHY.EPSI_R,
                         comment="""  PDGGAIN : CHAMP DE DEFORMATIONS GENE ELGA ANCIEN REPERE
""")

PDGGAINC = InputParameter(phys=PHY.EPSI_C,
                          comment="""  PDGGAIN : CHAMP DE DEFORMATIONS COMPLEXE GENE ELGA ANCIEN REPERE
""")

PDGNOIN = InputParameter(phys=PHY.EPSI_R,
                         comment="""  PDGNOIN : CHAMP DE DEFORMATIONS GENE ELNO ANCIEN REPERE
""")

PDGNOINC = InputParameter(phys=PHY.EPSI_C,
                          comment="""  PDGNOIN : CHAMP DE DEFORMATIONS COMPLEXE GENE ELNO ANCIEN REPERE
""")

PDONCO = InputParameter(phys=PHY.XCONTAC,
                        comment=""" XFEM
""")

PEFFOGC = InputParameter(phys=PHY.SIEF_C,
                         comment="""  PEFFONC : EFGE_ELGA DANS LE "PLAN" DU MAILLAGE (CAS COMPLEXE)
""")

PEFFOGR = InputParameter(phys=PHY.SIEF_R,
                         comment="""  PEFFONR : EFGE_ELGA DANS LE "PLAN" DU MAILLAGE (CAS REEL)
""")

PEFFONC = InputParameter(phys=PHY.SIEF_C,
                         comment="""  PEFFONC : EFGE_ELNO DANS LE "PLAN" DU MAILLAGE (CAS COMPLEXE)
""")

PEFFONR = InputParameter(phys=PHY.SIEF_R,
                         comment="""  PEFFONR : EFGE_ELNO DANS LE "PLAN" DU MAILLAGE (CAS REEL)
""")

PEFGAIN = InputParameter(phys=PHY.SIEF_R,
                         comment="""  PEFGAIN : CHAMP D'EFFORTS GENE ELGA ANCIEN REPERE
""")

PEFGAINC = InputParameter(phys=PHY.SIEF_C,
                          comment="""  PEFGAIN : CHAMP D'EFFORTS COMPLEXE GENE ELGA ANCIEN REPERE
""")

PEFNOIN = InputParameter(phys=PHY.SIEF_R,
                         comment="""  PEFNOIN : CHAMP D'EFFORTS GENE ELNO ANCIEN REPERE
""")

PEFNOINC = InputParameter(phys=PHY.SIEF_C,
                          comment="""  PEFNOIN : CHAMP D'EFFORTS COMPLEXE GENE ELNO ANCIEN REPERE
""")

PEFOND = InputParameter(phys=PHY.NEUT_R,
                        comment=""" COEFFICIENT FOR EFFE_FOND
""")

PEPCON1 = InputParameter(phys=PHY.SIEF_R,
                         comment=""" Contraintes intiales
""")

PEPCON2 = InputParameter(phys=PHY.SIEF_R,
                         comment=""" DEFI_CABL_BP / TENSION_INIT
""")

PEPSINF = InputParameter(phys=PHY.EPSI_F,
                         comment="""""")

PEPSINR = InputParameter(phys=PHY.EPSI_R,
                         comment="""""")

PERREM = InputParameter(phys=PHY.ERRE_R,
                        comment="""  PERREM : CARTE DE L INDICATEUR SPATIAL A L INSTANT PRECEDENT
""")

PFAMILK = InputParameter(phys=PHY.NEUT_K8,
                         comment="""  PFAMILK : FAMILLE D'INTEGRATION
""")

PFC1D1D = InputParameter(phys=PHY.FORC_C,
                         comment="""""")

PFERRA1 = InputParameter(phys=PHY.FER1_R,
                         comment=""" PFERRA1 : DONNEES UTILISATEUR POUR LE CALCUL DE FERRAILLAGE
""")

PFF1D1D = InputParameter(phys=PHY.FORC_F,
                         comment="""  PFF1D1D : CHARGE REPARTIE DE TYPE FONCTION POUR UNE MODELISATION POUTRE
""")

PFF1D2D = InputParameter(phys=PHY.FORC_F,
                         comment="""""")

PFF1D3D = InputParameter(phys=PHY.FORC_F,
                         comment="""""")

PFF2D2D = InputParameter(phys=PHY.FORC_F,
                         comment="""""")

PFF2D3D = InputParameter(phys=PHY.FORC_F,
                         comment="""""")

PFF3D3D = InputParameter(phys=PHY.FORC_F,
                         comment="""""")

PFFCO2D = InputParameter(phys=PHY.FORC_F,
                         comment="""""")

PFFCO3D = InputParameter(phys=PHY.FORC_F,
                         comment="""""")

PFFVOLU = InputParameter(phys=PHY.FORC_F,
                         comment="""  PFFVOLU : CHARGEMENT INTERNE DE TYPE VOLUMIQUE (FONCTION)
""")

PFFVOLUD = InputParameter(phys=PHY.FORC_F,
                          comment="""  PFFVOLUD : CHARGEMENT INTERNE DE TYPE VOLUMIQUE PB. DUAL (FONCTION)
""")

PFFVOLUP = InputParameter(phys=PHY.FORC_F,
                          comment="""  PFFVOLUP : CHARGEMENT INTERNE DE TYPE VOLUMIQUE PB. PRIMAL (FONCTION)
""")

PFIBRES = InputParameter(phys=PHY.CAFI_R, container='CARA!.CAFIBR',
                         comment="""  PFIBRES :  COORDONNEES ET AIRE DES FIBRES
 MULTIFIBER: COORDONNEES ET AIRE DES FIBRES
  PFIBRES :  CARACTERISTIQUE DES FIBRES POUR POUTRES MULTI FIBRES,
           NECESSITE DE FOURNIR LE CONCEPT PRODUIT PAR AFFE_CARA_ELEM
  PFIBRES :  CARACTERISTIQUE DES FIBRES POUR POUTRES MULTI FIBRES
 CARACTERISTIQUES DES FIBRES (PMF)
  PFIBRES :  CARACTERISTIQUE DES FIBRES POUR POUTRES MULTIFIBRES
 CARACTERISTIQUES DES FIBRES POUR LES ELEMENTS A FIBRES
""")

PFISCO = InputParameter(phys=PHY.NEUT_I,
                        comment="""""")

PFISSR = InputParameter(phys=PHY.FISS_R,
                        comment="""""")

PFLAPLA = InputParameter(phys=PHY.FLAPLA,
                         comment="""""")

PFLUXF = InputParameter(phys=PHY.FTHM_F,
                        comment="""""")

PFLUXNF = InputParameter(phys=PHY.FLUN_F,
                         comment="""""")

PFLUXNL = InputParameter(phys=PHY.FLUN_F,
                         comment="""""")

PFLUXNR = InputParameter(phys=PHY.FLUN_R,
                         comment="""""")

PFLUXR = InputParameter(phys=PHY.FTHM_R,
                        comment="""""")

PFLUXVF = InputParameter(phys=PHY.FLUX_F,
                         comment="""""")

PFLUX_M = InputParameter(phys=PHY.FLUX_R,
                         comment="""  PFLUX_M : FLUX A L INSTANT PRECEDENT
""")

PFLUX_P = InputParameter(phys=PHY.FLUX_R,
                         comment="""  PFLUX_P : FLUX A L INSTANT COURANT
""")

PFORCE = InputParameter(phys=PHY.NEUT_I,
                        comment="""  PFORCE : CHARGEMENT DES BORDS DE TYPE FORCE
""")

PFORCED = InputParameter(phys=PHY.NEUT_I,
                         comment="""  PFORCED : CHARGEMENT DES BORDS DE TYPE FORCE PB. DUAL
""")

PFORCEP = InputParameter(phys=PHY.NEUT_I,
                         comment="""  PFORCEP : CHARGEMENT DES BORDS DE TYPE FORCE PB. PRIMAL
""")

PFORNOF = InputParameter(phys=PHY.FORC_F,
                         comment="""""")

PFORNOR = InputParameter(phys=PHY.FORC_R,
                         comment="""""")

PFR1D1D = InputParameter(phys=PHY.FORC_R,
                         comment="""  PFR1D1D : CHARGE REPARTIE DE TYPE REEL POUR UNE MODELISATION POUTRE
""")

PFR1D2D = InputParameter(phys=PHY.FORC_R,
                         comment="""""")

PFR1D3D = InputParameter(phys=PHY.FORC_R,
                         comment="""""")

PFR2D2D = InputParameter(phys=PHY.FORC_R,
                         comment="""""")

PFR2D3D = InputParameter(phys=PHY.FORC_R,
                         comment="""""")

PFR3D3D = InputParameter(phys=PHY.FORC_R,
                         comment="""""")

PFRCO2D = InputParameter(phys=PHY.FORC_R,
                         comment="""""")

PFRCO3D = InputParameter(phys=PHY.FORC_R,
                         comment="""""")

PFRELEC = InputParameter(phys=PHY.FELECR,
                         comment="""""")

PFREQR = InputParameter(phys=PHY.FREQ_R, container='VOLA!&&CCPARA.FREQ',
                        comment="""  PFREQR : FREQUENCE DU MODE
""")

PFRVOLU = InputParameter(phys=PHY.FORC_R,
                         comment="""  PFRVOLU : CHARGEMENT INTERNE DE TYPE VOLUMIQUE (REELS)
""")

PFRVOLUD = InputParameter(phys=PHY.FORC_R,
                          comment="""  PFRVOLUD : CHARGEMENT INTERNE DE TYPE VOLUMIQUE PB. DUAL
""")

PFRVOLUP = InputParameter(phys=PHY.FORC_R,
                          comment="""  PFRVOLUP : CHARGEMENT INTERNE DE TYPE VOLUMIQUE PB. PRIMAL
""")

PFTRC = InputParameter(phys=PHY.ADRSJEVN,
                       comment="""""")

PGEOMER = InputParameter(phys=PHY.GEOM_R, container='MAIL!.COORDO',
                         comment="""  PGEOMER : COORDONNEES DES NOEUDS
  PGEOMER :  INITIAL GEOMETRY
 INITIAL GEOMETRY
  PGEOMER : COORDONNEES DES NOEUDS
 COORDONNEES INITIALES DES NOEUDS
 PGEOMER : COORDONNEES DES NOEUDS
""")

PGLISS = InputParameter(phys=PHY.NEUT_I,
                        comment="""""")

PGRADLN = InputParameter(phys=PHY.NEUT_R,
                         comment="""""")

PGRADLT = InputParameter(phys=PHY.NEUT_R,
                         comment="""""")

PGRAINF = InputParameter(phys=PHY.FLUX_F,
                         comment="""""")

PGRAINR = InputParameter(phys=PHY.FLUX_R,
                         comment="""""")

PGRDCA = InputParameter(phys=PHY.NEUT_R,
                        comment="""  PGRDCA : GRANDEURS CARACTERISTIQUES POUR ADIMENSIONNEMENT
  PGRDCA : GRDEURS CARACTERISTIQUES POUR ADIMENSIONNEMENT (HM)
""")

PHARMON = InputParameter(phys=PHY.HARMON, container='VOLA!&&CCPARA.NUME_MODE',
                         comment="""  PHARMON : NUMERO D'HARMONIQUE DE FOURIER
""")

PHEAVNO = InputParameter(phys=PHY.NEUT_I,
                         comment=""" PHEVNO : CONNECTIVITE INVERSE DE PFISNO
 XFEM
""")

PHECHPF = InputParameter(phys=PHY.COEH_F,
                         comment="""""")

PHECHPR = InputParameter(phys=PHY.COEH_R,
                         comment="""""")

PIMPEDC = InputParameter(phys=PHY.IMPE_C,
                         comment="""""")

PIMPEDR = InputParameter(phys=PHY.IMPE_R,
                         comment="""""")

PINDCOI = InputParameter(phys=PHY.NEUT_I,
                         comment=""" XFEM
""")

PINFORR = InputParameter(phys=PHY.NEUT_R,
                         comment="""  PINFORR : INFO SUR MAILLES COUPLEES
""")

PINSTMR = InputParameter(phys=PHY.INST_R,
                         comment=""" VALEUR DE L'INSTANT T-
  PINSTMR :  INSTANT PRECEDENT
""")

PINSTPR = InputParameter(phys=PHY.INST_R,
                         comment="""  PINSTPR :  INSTANT ACTUEL
 VALEUR DE L'INSTANT T+
""")

PITERAT = InputParameter(phys=PHY.NEUT_R,
                         comment=""" ITERATION DE NEWTON COURANTE (ASSE_CORN/PMF)
""")

PLAGRM = InputParameter(phys=PHY.NEUT_R,
                        comment="""""")

PLEVSET = InputParameter(phys=PHY.NEUT_R,
                         comment="""""")

PLISTMA = InputParameter(phys=PHY.LISTMA,
                         comment="""""")

PLONFA = InputParameter(phys=PHY.N120_I,
                        comment="""  CADRE X-FEM : PPINTTO : COORDONNEES DES POINTS D INTERSECTION
                         PHEAVTO : VALEURS DE L HEAVISIDE SUR LES SS-ELTS
                         PBASLOR : BASE LOCALE AU FOND DE FISSURE
                         PLSN    : LEVEL SET NORMALE
                         PLST    : LEVEL SET TANGENTE
""")

PMASDIA = InputParameter(phys=PHY.POSI, container='VOLA!&&CCPARA.MASS_MECA_D',
                         comment="""  PMASDIA : OPTION MASS_MECA_DIAG OU NON
""")

PMASSEL = InputParameter(phys=PHY.MDEP_R,
                         comment="""""")

PMATERC = InputParameter(phys=PHY.ADRSJEVE, container='CHMA!.MATE_CODE',
                         comment=""" PMATERC : CHAMP DE MATERIAU ISSU DE AFFE_MATERIAU
  PMATERC : CHAMP DE MATERIAU
 PMATERC : MATERIAU_CODE
  PMATERC : CHAMP DE MATERIAU
 MATERIAL INFO
 PMATERC: MATERIAU CODE
 CHAMP DE MATERIAU
  PMATERC : CHAMP DE MATERIAU  BECAUSE DEFI_COQU_MULT
""")

PMEMCON = InputParameter(phys=PHY.NEUT_I,
                         comment="""""")

PNEUK24 = InputParameter(phys=PHY.NEUT_K24,
                         comment="""""")

PNEUTER = InputParameter(phys=PHY.NEUT_R,
                         comment="""""")

PNONLIN = InputParameter(phys=PHY.NEUT_I, container='VOLA!&&CCACCL.PNONLIN',
                         comment="""  PNONLIN : POUR INDIQUER SI LE CALCUL EST LINEAIRE OU NON
""")

PNOVARI = InputParameter(
    phys=PHY.NEUT_K24, container='VOLA!&&CCPARA.NOM_VARI',
    comment="""  PNOVARI : CARTE DU NOM DE CHAMP A EXTRAIRE
""")

PNUMMOD = InputParameter(phys=PHY.NUMMOD,
                         comment="""""")

POMEGA2 = InputParameter(phys=PHY.OME2_R, container='VOLA!&&CCPARA.OMEGA2',
                         comment="""  POMEGA2 : PULSATION AU CARRE
""")

PONDECR = InputParameter(phys=PHY.ONDE_R,
                         comment="""""")

PONDPLA = InputParameter(phys=PHY.NEUT_K8,
                         comment=""" WAVE FUNCTION
""")

PONDPLR = InputParameter(phys=PHY.NEUT_R,
                         comment=""" WAVE TYPE AND DIRECTION
""")

PORIGFI = InputParameter(phys=PHY.GEOM_R,
                         comment="""""")

PORIGIN = InputParameter(phys=PHY.GEOM_R,
                         comment="""""")

PPESANR = InputParameter(phys=PHY.PESA_R,
                         comment=""" Champ de pesanteur. Concept CHARGE
  PPESANR : CHARGEMENT INTERNE DE TYPE PESANTEUR (REELS)
  PPESANR : CHARGE PESANTEUR POUR UNE MODELISATION POUTRE
""")

PPESANRD = InputParameter(phys=PHY.PESA_R,
                          comment="""  PPESANRD : CHARGEMENT INTERNE DE TYPE PESANTEUR PB. DUAL
""")

PPESANRP = InputParameter(phys=PHY.PESA_R,
                          comment="""  PPESANRP : CHARGEMENT INTERNE DE TYPE PESANTEUR PB. PRIMAL
""")

PPREFFF = InputParameter(phys=PHY.PRES_F,
                         comment=""" FUNCTION OF PRESSURE
""")

PPREFFR = InputParameter(phys=PHY.PRES_R,
                         comment=""" PRESSURE
""")

PPRESS = InputParameter(phys=PHY.NEUT_I,
                        comment="""  PPRESS : CHARGEMENT DES BORDS DE TYPE PRESSION
""")

PPRESSC = InputParameter(phys=PHY.PRES_C, container='RESU!PRES!N',
                         comment="""  PPRESSC : PRESSION ACOUSTIQUE
  PPRESSC : PRESSION ACOUSTIQUE  AUX NOEUDS
""")

PPRESSD = InputParameter(phys=PHY.NEUT_I,
                         comment="""  PPRESSD : CHARGEMENT DES BORDS DE TYPE PRESSION PB. DUAL
""")

PPRESSF = InputParameter(phys=PHY.PRES_F,
                         comment="""""")

PPRESSP = InputParameter(phys=PHY.NEUT_I,
                         comment="""  PPRESSP : CHARGEMENT DES BORDS DE TYPE PRESSION PB. PRIMAL
""")

PPRESSR = InputParameter(phys=PHY.PRES_R,
                         comment="""""")

PPULPRO = InputParameter(phys=PHY.FREQ_R,
                         comment="""""")

PRAYONF = InputParameter(phys=PHY.RAYO_F,
                         comment="""""")

PRAYONR = InputParameter(phys=PHY.RAYO_R,
                         comment="""""")

PREFCO = InputParameter(phys=PHY.PREC,
                        comment="""  PREFCO :  REFERENCE DE CONTRAINTE
""")

PREFE1K = InputParameter(phys=PHY.NEUT_K8,
                         comment="""  PREFE1K : MAILLE COUPLEE 1
""")

PREFE2K = InputParameter(phys=PHY.NEUT_K8,
                         comment="""  PREFE2K : MAILLE COUPLEE 2
""")

PRIGIEL = InputParameter(phys=PHY.MDEP_R,
                         comment="""""")

PRIGINS = InputParameter(phys=PHY.MDNS_R,
                         comment="""""")

PROMK = InputParameter(phys=PHY.DEPL_R,
                       comment=""" ROTATION DU NOEUD ITERATION N (POU_D_TGD)
""")

PROMKM1 = InputParameter(phys=PHY.DEPL_R,
                         comment=""" ROTATION DU NOEUD ITERATION N+1 (POU_D_TGD)
""")

PROTATR = InputParameter(phys=PHY.ROTA_R,
                         comment="""  PROTATR : CHARGEMENT INTERNE DE TYPE ROTATION (REELS)
 PARAMETERS FOR ROTATION
""")

PROTATRD = InputParameter(phys=PHY.ROTA_R,
                          comment="""  PROTATRD : CHARGEMENT INTERNE DE TYPE ROTATION PB. DUAL
""")

PROTATRP = InputParameter(phys=PHY.ROTA_R,
                          comment="""  PROTATRP : CHARGEMENT INTERNE DE TYPE ROTATION PB. PRIMAL
""")

PSDRMR = InputParameter(phys=PHY.NEUT_R,
                        comment="""""")

PSIEFD_R = InputParameter(phys=PHY.SIEF_R,
                          comment="""  PSIEFD_R : CONTRAINTES PB. DUAL
""")

PSIEFP_R = InputParameter(phys=PHY.SIEF_R,
                          comment="""  PSIEFP_R : CONTRAINTES PB. PRIMAL
""")

PSIEFR = InputParameter(phys=PHY.SIEF_R, container='RESU!SIEF_ELGA!N',
                        comment=""" PSIEFR   : CONTRAINTES  INITIALES
  PSIEFR : ETAT DE CONTRAINTE AUX POINTS DE GAUSS
""")

PSIG3D = InputParameter(phys=PHY.SIEF_R, container='VOLA!&&CCPARA.CHAM_SI2D',
                        comment="""  PSIG3D : CONTRAINTES 3D PROJETEES SUR LES FACES
""")

PSIGINR = InputParameter(phys=PHY.SIEF_R,
                         comment="""""")

PSIGISE = InputParameter(phys=PHY.N1920R,
                         comment="""""")

PSIGMA = InputParameter(phys=PHY.SIEF_R,
                        comment="""  PSIGMA : CONTRAINTES LISSEES AUX NOEUDS
""")

PSIGMAD = InputParameter(phys=PHY.SIEF_R,
                         comment="""  PSIGMAD : CONTRAINTES LISSEES PB. DUAL
""")

PSIGMAP = InputParameter(phys=PHY.SIEF_R,
                         comment="""  PSIGMAP : CONTRAINTES LISSEES PB. PRIMAL
""")

PSOURCF = InputParameter(phys=PHY.SOUR_F,
                         comment="""  PSOURCF : SOURCE FONCTION
""")

PSOURCR = InputParameter(phys=PHY.SOUR_R,
                         comment="""  PSOURCR : SOURCE REELLE
""")

PSOURNL = InputParameter(phys=PHY.SOUR_F,
                         comment="""  PSOURNL :
""")

PSOUSOP = InputParameter(phys=PHY.NEUT_K24,
                         comment="""""")

PSTADYN = InputParameter(phys=PHY.STAOUDYN,
                         comment=""" CHAMP POUR DYNAMIQUE NEWMARK (POU_D_TGD)
""")

PSTRXMP = InputParameter(phys=PHY.STRX_R,
                         comment=""" PSTRXMP : CHAMPS SPECIAL ELEMENTS DE STRUCTURE
  PSTRXMP : CHAMPS SPECIAL ELEMENTS DE STRUCTURE
""")

PSTRXMR = InputParameter(phys=PHY.STRX_R,
                         comment=""" PSTRXMR : CHAMPS SPECIAL ELEMENTS DE STRUCTURE
  PSTRXMR : CHAMPS SPECIAL ELEMENTS DE STRUCTURE
""")

PSUROPT = InputParameter(phys=PHY.NEUT_K24,
                         comment="""  PSUROPT : OPTION DE CALCUL DE LA MASSE POUR UNE MODELISATION POUTRE
""")

PTEMPAR = InputParameter(phys=PHY.TEMP_R,
                         comment="""""")

PTEMPE1 = InputParameter(phys=PHY.TEMP_R,
                         comment="""""")

PTEMPE2 = InputParameter(phys=PHY.TEMP_R,
                         comment="""""")

PTEMPEF = InputParameter(phys=PHY.TEMP_F,
                         comment="""""")

PTEMPEI = InputParameter(phys=PHY.TEMP_R,
                         comment="""  PTEMPER :
""")

PTEMPER = InputParameter(phys=PHY.TEMP_R, container='RESU!TEMP!N',
                         comment="""  PTEMPER :
  PTEMPER : POTENTIEL ELECTRIQUE INSTANT ACTUEL
  PTEMPER : TEMPERATURES INSTANT ACTUEL
""")

PTEMPIR = InputParameter(phys=PHY.TEMP_R,
                         comment="""""")

PTEMPSR = InputParameter(phys=PHY.INST_R, container='VOLA!&&CCPARA.CH_INST_R',
                         comment="""  PTEMPSR :  INSTANT ACTUEL
 TIME
  PSTEMPSR :
  PTEMPSR : INSTANT ACTUEL
  PTEMPSR :  INSTANT ACTUEL
 PTEMPSR: INSTANT ACTUEL
  PTEMPSR :  TIME
""")

PTEMP_M = InputParameter(phys=PHY.TEMP_R,
                         comment="""  PTEMP_M : TEMPERATURE A L INSTANT PRECEDENT
""")

PTEMP_P = InputParameter(phys=PHY.TEMP_R,
                         comment="""  PTEMP_P: TEMPERATURE A L INSTANT COURANT
""")

PTHETAR = InputParameter(phys=PHY.DEPL_R,
                         comment="""""")

PTMPCHF = InputParameter(phys=PHY.TEMP_R,
                         comment="""""")

PTMPCHI = InputParameter(phys=PHY.TEMP_R,
                         comment="""""")

PTRIAGM = InputParameter(phys=PHY.ENDO_R, container='RESU!ENDO_ELGA!NM1',
                         comment="""  PTRIAGM : TRIAXIALITE, CONTRAINTE ENDOMMAGEMENT,
           ET DOMMAGE DE LEMAITRE-SERMAGE
""")

PTYPDIS = InputParameter(phys=PHY.NEUT_I,
                         comment="""""")

PTYPEPI = InputParameter(phys=PHY.PILO_K,
                         comment="""""")

PT_EXTF = InputParameter(phys=PHY.TEMP_F,
                         comment="""""")

PT_EXTR = InputParameter(phys=PHY.TEMP_R,
                         comment="""""")

PVARCMR = InputParameter(
    phys=PHY.VARI_R, container='VOLA!&&CCPARA.VARI_INT_N',
    comment=""" PVARCMR: VARIABLES DE COMMANDES POUR T-
 VARIABLES DE COMMANDES  POUR T-
  PVARCPR : VARIABLES DE COMMANDE  A L INSTANT +
 PVARCMR : VARIABLES DE COMMANDES  POUR T-
  PVARCMR : TEMPERATURES INSTANT PRECEDENT
""")

PVARCRR = InputParameter(
    phys=PHY.VARI_R, container='VOLA!&&CCPARA.VARI_INT_REF',
    comment=""" PVARCPR : VARIABLES DE COMMANDES  DE REFERENCE
 PVARCRR : VARIABLES DE COMMANDES  DE REFERENCE
 VARIABLES DE COMMANDES  DE REFERENCE
 PVARCRR : VARIABLES DE COMMANDES  DE REFERENCE
  PVARCRR : VARIABLE DE COMMANDE DE REFERENCE
  PVARCRR : VARIABLES DE COMMANDE  DE REFERENCE
  PVARCRR : TEMPERATURE DE REFERENCE
  PVARCRR : VARIABLES DE COMMANDES  DE REFERENCE
  PVARCRR : VARIABLES DE COMMANDE  DE REFERENCE
  PVARCPR : VARIABLES DE COMMANDE  DE REFERENCE
""")

PVARIGR = InputParameter(phys=PHY.VARI_R, container='RESU!VARI_ELGA!N',
                         comment="""  PVARIGR : VARIABLES INTERNES
  PVARIGR : VARIABLES INTERNES AUX POINTS DE GAUSS
""")

PVARIMP = InputParameter(phys=PHY.VARI_R,
                         comment=""" VARIABLES INTERNES APRES ITERATIONS DE NEWTON PRECEDENTE POUR DE BORST
""")

PVARIPG = InputParameter(phys=PHY.VARI_R,
                         comment="""""")

PVENTCX = InputParameter(phys=PHY.VENTCX_F,
                         comment="""""")

PVITENC = InputParameter(phys=PHY.VNOR_C,
                         comment="""""")

PVITENT = InputParameter(phys=PHY.DEPL_R,
                         comment=""" VITESSE POUR IMPEDANCE SOL
""")

PVITER = InputParameter(phys=PHY.DEPL_R,
                        comment="""""")

PVITESR = InputParameter(phys=PHY.DEPL_R, container='RESU!VITE!N',
                         comment="""  PVITESR : VITESSES INSTANT ACTUEL
""")

PVITESS = InputParameter(phys=PHY.DEPL_R,
                         comment="""""")

PVITE_M = InputParameter(phys=PHY.DEPL_R,
                         comment=""" VITESSES POUR T-
""")

PVITE_P = InputParameter(phys=PHY.DEPL_R,
                         comment=""" VITESSES POUR T+
""")

PVITKM1 = InputParameter(phys=PHY.DEPL_R,
                         comment=""" VITESSE DU NOEUD ITERATION N+1 (POU_D_TGD)
""")

PVITPLU = InputParameter(phys=PHY.DEPL_R,
                         comment=""" VITESSE DU NOEUD PAS N  (POU_D_TGD+DIS_*)
""")

UEPSINF = InputParameter(phys=PHY.EPSI_F,
                         comment="""""")

UEPSINR = InputParameter(phys=PHY.EPSI_R,
                         comment="""""")

UPESANR = InputParameter(phys=PHY.PESA_R,
                         comment="""""")

UPFF23D = InputParameter(phys=PHY.FORC_F,
                         comment="""""")

UPFFVOL = InputParameter(phys=PHY.FORC_F,
                         comment="""""")

UPFR23D = InputParameter(phys=PHY.FORC_R,
                         comment="""""")

UPFRVOL = InputParameter(phys=PHY.FORC_R,
                         comment="""""")

UPRESSF = InputParameter(phys=PHY.PRES_F,
                         comment="""""")

UPRESSR = InputParameter(phys=PHY.PRES_R,
                         comment="""""")

UROTATR = InputParameter(phys=PHY.ROTA_R,
                         comment="""""")

UTEMPSR = InputParameter(phys=PHY.INST_R,
                         comment="""""")

VEPSINF = InputParameter(phys=PHY.EPSI_F,
                         comment="""""")

VEPSINR = InputParameter(phys=PHY.EPSI_R,
                         comment="""""")

VPESANR = InputParameter(phys=PHY.PESA_R,
                         comment="""""")

VPFF23D = InputParameter(phys=PHY.FORC_F,
                         comment="""""")

VPFFVOL = InputParameter(phys=PHY.FORC_F,
                         comment="""""")

VPFR23D = InputParameter(phys=PHY.FORC_R,
                         comment="""""")

VPFRVOL = InputParameter(phys=PHY.FORC_R,
                         comment="""""")

VPRESSF = InputParameter(phys=PHY.PRES_F,
                         comment="""""")

VPRESSR = InputParameter(phys=PHY.PRES_R,
                         comment="""""")

VROTATR = InputParameter(phys=PHY.ROTA_R,
                         comment="""""")

VTEMPSR = InputParameter(phys=PHY.INST_R,
                         comment="""""")

XXXXXX = InputParameter(phys=PHY.SIEF_R, container='RESU!SIGM_ELNO!N',
                        comment="""""")


# Parametres 'Out' :
# ------------------
PBASESC = OutputParameter(phys=PHY.N2448R, type='ELEM',
                          comment="""""")

PBASMAI = OutputParameter(phys=PHY.N2448R, type='ELEM',
                          comment="""""")

PBIDON = OutputParameter(phys=PHY.NEUT_R, type='ELEM',
                         comment="""""")

PCAFI_R = OutputParameter(phys=PHY.CAFI_R, type='ELEM',
                          comment="""""")

PCARAGE = OutputParameter(phys=PHY.MASS_R, type='ELEM',
                          comment="""""")

PCASECT = OutputParameter(phys=PHY.NEUT_R, type='ELEM',
                          comment="""""")

PCODRET = OutputParameter(phys=PHY.CODE_I, type='ELEM',
                          comment=""" CODE RETOUR INTEGRATION COMPORTEMENT
 CODE RETOUR : 0 -> OK , 1 SINON
""")

PCOGAOUT = OutputParameter(phys=PHY.SIEF_R, type='ELGA',
                           comment="""  PCOGAOUT : CHAMP DE CONTRAINTES ELGA NOUVEAU REPERE
""")

PCONOOUT = OutputParameter(phys=PHY.SIEF_R, type='ELNO',
                           comment="""  PCONOOUT : CHAMP DE CONTRAINTES ELNO NOUVEAU REPERE
""")

PCONTPC = OutputParameter(phys=PHY.SIEF_C, type='ELNO',
                          comment="""""")

PCONTPO = OutputParameter(phys=PHY.SIEF_R, type='ELNO',
                          comment="""""")

PCONTRC = OutputParameter(phys=PHY.SIEF_C, type='ELGA',
                          comment="""""")

PCONTRT = OutputParameter(phys=PHY.SIEF_R, type='ELGA',
                          comment="""  PCONTRT : CHAMP SPECIFIQUE XFEM
""")

PCONTXR = OutputParameter(phys=PHY.SIEF_R, type='ELGA',
                          comment="""""")

PCOOPGM = OutputParameter(phys=PHY.GEOM_R, type='ELGA',
                          comment="""""")

PCOURAN = OutputParameter(phys=PHY.NEUT_R, type='ELEM',
                          comment="""""")

PDCEL_I = OutputParameter(phys=PHY.DCEL_I, type='ELEM',
                          comment="""""")

PDEFOGR = OutputParameter(phys=PHY.EPSI_R, type='ELNO',
                          comment="""""")

PDEFONC = OutputParameter(phys=PHY.EPSI_C, type='ELNO',
                          comment="""  PDEFONC : DEFORMATIONS COMPLEXES AUX NOEUDS
""")

PDEFONO = OutputParameter(phys=PHY.EPSI_R, type='ELNO',
                          comment="""  PDEFONO : DEFORMATIONS LIEES AUX VARIABLES DE COMMANDE
                     AUX NOEUDS
  PDEFONO : DEFORMATIONS MECANIQUES A PARTIR DES DEPLACEMENTS
                     AUX NOEUDS
  PDEFONO : DEFORMATIONS DE FLUAGE PROPRE AUX NOEUDS
  PDEFONO : DEFORMATIONS AUX NOEUDS
  PDEFONO : DEFORMATIONS DUES AU FLUAGE DE DESSICCATION AUX NOEUDS
  PDEFONO : DEFORMATIONS ANELASTIQUES AUX NOEUDS
  PDEFONO : DEFORMATIONS DE GREEN LAGRANGE AUX NOEUDS
""")

PDEFOPC = OutputParameter(phys=PHY.EPSI_C, type='ELGA',
                          comment="""  PDEFOPC : DEFORMATIONS COMPLEXES AUX POINTS DE GAUSS
""")

PDEGAOUT = OutputParameter(phys=PHY.EPSI_R, type='ELGA',
                           comment="""  PDEGAOUT : CHAMP DE DEFORMATIONS ELGA NOUVEAU REPERE
""")

PDENOOUT = OutputParameter(phys=PHY.EPSI_R, type='ELNO',
                           comment="""  PDENOOUT : CHAMP DE DEFORMATIONS ELNO NOUVEAU REPERE
""")

PDEPLPG = OutputParameter(phys=PHY.DEPL_R, type='ELGA',
                          comment="""""")

PDEPL_C = OutputParameter(phys=PHY.DEPL_C, type='ELGA',
                          comment="""""")

PDERANO = OutputParameter(phys=PHY.DERA_R, type='ELNO',
                          comment="""  PDERANO : INDICATEUR LOCAL DE DECHARGE  ET
           DE PERTE DE RADIALITE
""")

PDGGAOUC = OutputParameter(phys=PHY.EPSI_C, type='ELGA',
                           comment="""  PDGGAOUC : CHAMP DE DEFORMATIONS COMPLEXE GENE ELGA NOUVEAU REPERE
""")

PDGGAOUT = OutputParameter(phys=PHY.EPSI_R, type='ELGA',
                           comment="""  PDGGAOUT : CHAMP DE DEFORMATIONS GENE ELGA NOUVEAU REPERE
""")

PDGNOOUC = OutputParameter(phys=PHY.EPSI_C, type='ELNO',
                           comment="""  PDGNOOUC : CHAMP DE DEFORMATIONS COMPLEXE GENE ELNO NOUVEAU REPERE
""")

PDGNOOUT = OutputParameter(phys=PHY.EPSI_R, type='ELNO',
                           comment="""  PDGNOOUT : CHAMP DE DEFORMATIONS GENE ELNO NOUVEAU REPERE
""")

PDISSD1 = OutputParameter(phys=PHY.DISS_R, type='ELEM',
                          comment="""  PDISSD1 : ENERGIE DE DISSIPATION PAR ELEMENT
""")

PDISSNO = OutputParameter(phys=PHY.DISS_R, type='ELNO',
                          comment="""  PDISSNO : DENSITE DE DISSIPATION AUX NOEUDS
""")

PDURT_R = OutputParameter(phys=PHY.DURT_R, type='ELNO',
                          comment="""""")

PECHLI = OutputParameter(phys=PHY.CHLI_R, type='ELEM',
                         comment="""""")

PEFFOEGC = OutputParameter(phys=PHY.SIEF_C, type='ELGA',
                           comment="""""")

PEFFOEGR = OutputParameter(phys=PHY.SIEF_R, type='ELGA',
                           comment="""""")

PEFFOENC = OutputParameter(phys=PHY.SIEF_C, type='ELNO',
                           comment="""""")

PEFFOENR = OutputParameter(phys=PHY.SIEF_R, type='ELNO',
                           comment="""""")

PEFFORC = OutputParameter(phys=PHY.SIEF_C, type='ELNO',
                          comment="""""")

PEFGAOUC = OutputParameter(phys=PHY.SIEF_C, type='ELGA',
                           comment="""  PEFGAOUC : CHAMP D'EFFORTS COMPLEXE GENE ELGA NOUVEAU REPERE
""")

PEFGAOUT = OutputParameter(phys=PHY.SIEF_R, type='ELGA',
                           comment="""  PEFGAOUT : CHAMP D'EFFORTS GENE ELGA NOUVEAU REPERE
""")

PEFGEC = OutputParameter(phys=PHY.SIEF_C, type='ELGA',
                         comment="""""")

PEFGER = OutputParameter(phys=PHY.SIEF_R, type='ELGA',
                         comment="""""")

PEFNOOUC = OutputParameter(phys=PHY.SIEF_C, type='ELNO',
                           comment="""  PEFNOOUC : CHAMP D'EFFORTS COMPLEXE GENE ELNO NOUVEAU REPERE
""")

PEFNOOUT = OutputParameter(phys=PHY.SIEF_R, type='ELNO',
                           comment="""  PEFNOOUT : CHAMP D'EFFORTS GENE ELNO NOUVEAU REPERE
""")

PENERCR = OutputParameter(phys=PHY.ENER_R, type='ELEM',
                          comment="""  PENERCR : ENERGIE CINETIQUE PAR ELEMENT
""")

PENERD1 = OutputParameter(phys=PHY.ENER_R, type='ELEM',
                          comment="""  PENERD1 : ENERGIE ELASTIQUE PAR ELEMENT
""")

PENERD2 = OutputParameter(phys=PHY.ENER_R, type='ELEM',
                          comment="""""")

PENERNO = OutputParameter(phys=PHY.ENER_R, type='ELNO',
                          comment="""  PENERNO : DENSITE D'ENERGIE TOTALE AUX NOEUDS
  PENERNO : DENSITE D'ENERGIE ELASTIQUE AUX NOEUDS
""")

PEPCON3 = OutputParameter(phys=PHY.SIEF_R, type='ELGA',
                          comment="""""")

PERRENO = OutputParameter(phys=PHY.ERRE_R, type='ELNO',
                          comment="""  PERRENO : ESTIMATEUR D ERREUR AUX NOEUDS PAR ELEMENT
  PERRENO : ESTIMATEUR D ERREUR EN QI AUX NOEUDS PAR ELEMENT
""")

PFACY_R = OutputParameter(phys=PHY.FACY_R, type='ELGA',
                          comment="""""")

PFERRA2 = OutputParameter(phys=PHY.FER2_R, type='ELEM',
                          comment="""""")

PFLHN = OutputParameter(phys=PHY.FLHN_R, type='ELGA',
                        comment="""""")

PFLUXNO = OutputParameter(phys=PHY.FLUX_R, type='ELNO',
                          comment="""  PFLUXNO : FLUX AUX NOEUDS PAR ELEMENT
""")

PFORC_R = OutputParameter(phys=PHY.FORC_R, type='ELEM',
                          comment="""""")

PGAMIMA = OutputParameter(phys=PHY.SPMX_R, type='ELGA',
                          comment="""""")

PGESCLA = OutputParameter(phys=PHY.N816_R, type='ELEM',
                          comment="""""")

PGMAITR = OutputParameter(phys=PHY.N816_R, type='ELEM',
                          comment="""""")

PGTHETA = OutputParameter(phys=PHY.G, type='ELEM',
                          comment="""""")

PHYDMAT = OutputParameter(phys=PHY.HYDR_R, type='ELGA',
                          comment="""""")

PHYDRPP = OutputParameter(phys=PHY.HYDR_R, type='ELNO',
                          comment="""""")

PINCOCA = OutputParameter(phys=PHY.NEUT_I, type='ELEM',
                          comment="""""")

PINDCOO = OutputParameter(phys=PHY.NEUT_I, type='ELEM',
                          comment="""""")

PINDLOC = OutputParameter(phys=PHY.INDL_R, type='ELGA',
                          comment="""  PINDLOC : INDICATEUR DE LOCALISATION
""")

PINDMEM = OutputParameter(phys=PHY.NEUT_I, type='ELEM',
                          comment="""""")

PINTER = OutputParameter(phys=PHY.INTE_R, type='ELNO',
                         comment="""""")

PLAGRP = OutputParameter(phys=PHY.NEUT_R, type='ELGA',
                         comment="""""")

PMASSINE = OutputParameter(phys=PHY.MASS_R, type='ELEM',
                           comment="""""")

PMATTTC = OutputParameter(phys=PHY.MPRE_C, type='RESL',
                          comment="""""")

PMATUN1 = OutputParameter(phys=PHY.MZNS_R, type='RESL',
                          comment="""  PMATUN1 : MATRICE DE COUPLAGE 1
""")

PMATUN2 = OutputParameter(phys=PHY.MZNS_R, type='RESL',
                          comment="""  PMATUN2 : MATRICE DE COUPLAGE 2
""")

PMATUNS = OutputParameter(phys=PHY.MDNS_R, type='RESL',
                          comment=""" MATRICE RIGIDITE TANGENTE NON-SYMETRIQUE
 MATRICE ELEMENTAIRE DE RAIDEUR GYROSCOPIQUE
 CE PARAMETRE EST UTILISE PAR LES ELEMENTS QUI CALCULENT
   UNE MATRICE TANGENTE NON-SYMETRIQUE. CERTAINS ELEMENTS
   (PAR EXEMPLE MEC3TR7H) CALCULENT PARFOIS UNE MATRICE
   SYMETRIQUE PARFOIS UNE MATRICE NON-SYMETRIQUE
 MATRICE ELEMENTAIRE DE GYROSCOPIE ANTISYMETRIQUE
""")

PMATUUC = OutputParameter(phys=PHY.MDEP_C, type='RESL',
                          comment="""""")

PMATUUR = OutputParameter(phys=PHY.MDEP_R, type='RESL',
                          comment=""" MATRICE RIGIDITE TANGENTE SYMETRIQUE
""")

PMATZZR = OutputParameter(phys=PHY.MSIZ_R, type='RESL',
                          comment="""  PMATZZR : MATRICE DE MASSE ELEMENTAIRE
""")

PNEU1_R = OutputParameter(phys=PHY.NEUT_R, type='ELEM',
                          comment="""""")

PNEUMAT = OutputParameter(phys=PHY.NEUT_R, type='ELGA',
                          comment="""""")

PNEWGEM = OutputParameter(phys=PHY.N816_R, type='ELEM',
                          comment="""""")

PNEWGES = OutputParameter(phys=PHY.N816_R, type='ELEM',
                          comment="""""")

PNOMIMA = OutputParameter(phys=PHY.SPMX_R, type='ELNO',
                          comment="""""")

PNORME = OutputParameter(phys=PHY.NEUT_R, type='ELEM',
                         comment="""  PNORME : NORME L2
""")

PPDIL = OutputParameter(phys=PHY.PDIL_R, type='ELGA',
                        comment="""  PKDIL : MODULE DE RIGIDITE DE MICRO-DILATATION
""")

PPHASNOU = OutputParameter(phys=PHY.VARI_R, type='ELNO',
                           comment="""""")

PPJSIGM = OutputParameter(phys=PHY.SIEF_R, type='ELEM',
                          comment="""  PPJSIGM : ROSETTE DE CONTRAINTES PAR ELEMENT
""")

PPRAC_R = OutputParameter(phys=PHY.PRAC_R, type='ELNO',
                          comment="""""")

PPRME_R = OutputParameter(phys=PHY.PRME_R, type='ELNO',
                          comment="""""")

PRAYONM = OutputParameter(phys=PHY.NEUT_R, type='ELEM',
                          comment="""""")

PREPLO1 = OutputParameter(phys=PHY.GEOM_R, type='ELEM',
                          comment="""""")

PREPLO2 = OutputParameter(phys=PHY.GEOM_R, type='ELEM',
                          comment="""""")

PREPLO3 = OutputParameter(phys=PHY.GEOM_R, type='ELEM',
                          comment="""""")

PRESIDU = OutputParameter(phys=PHY.VTEM_R, type='RESL',
                          comment="""""")

PRICTRA = OutputParameter(phys=PHY.RICE_TRA, type='ELEM',
                          comment="""""")

PSDRPR = OutputParameter(phys=PHY.NEUT_R, type='ELGA',
                         comment="""""")

PSIEFNOC = OutputParameter(phys=PHY.SIEF_C, type='ELNO',
                           comment="""  PSIEFNOC : CONTRAINTES COMPLEXES PAR ELEMENT AUX NOEUDS
""")

PSIGISG = OutputParameter(phys=PHY.DOMA_R, type='ELGA',
                          comment="""""")

PSIGMC = OutputParameter(phys=PHY.SIEF_C, type='ELGA',
                         comment="""""")

PSIGMR = OutputParameter(phys=PHY.SIEF_R, type='ELGA',
                         comment="""""")

PSIMXRC = OutputParameter(phys=PHY.SIEFMX_C, type='ELNO',
                          comment="""""")

PSIMXRR = OutputParameter(phys=PHY.SIEFMX_R, type='ELNO',
                          comment="""""")

PSINGNO = OutputParameter(phys=PHY.SING_R, type='ELNO',
                          comment="""  PSING_R : SINGULARITE ET CARTE DE TAILLE
""")

PSING_R = OutputParameter(phys=PHY.SING_R, type='ELEM',
                          comment="""  PSING_R : SINGULARITE ET CARTE DE TAILLE
""")

PSTRXPR = OutputParameter(phys=PHY.STRX_R, type='ELGA',
                          comment="""  PSTRXPR : CHAMPS SPECIAL ELEMENTS DE STRUCTURE
""")

PSTRX_R = OutputParameter(phys=PHY.STRX_R, type='ELGA',
                          comment="""  PSTRX_R : CHAMPS SPECIAL ELEMENTS DE STRUCTURE
""")

PTEMMAT = OutputParameter(phys=PHY.TEMP_R, type='ELGA',
                          comment="""""")

PTEMPCR = OutputParameter(phys=PHY.TEMP_R, type='ELEM',
                          comment="""""")

PTEMP_R = OutputParameter(phys=PHY.TEMP_R, type='ELGA',
                          comment="""""")

PTRIANO = OutputParameter(phys=PHY.ENDO_R, type='ELNO',
                          comment="""  PTRIANO : TRIAXIALITE, CONTRAINTE ENDOMMAGEMENT,
           ET DOMMAGE DE LEMAITRE-SERMAGE INSTANT ACTUEL
           AUX NOEUDS PAR ELEMENT
""")

PVALO_R = OutputParameter(phys=PHY.VALO_R, type='ELGA',
                          comment="""""")

PVARC_R = OutputParameter(phys=PHY.VARC_R, type='ELGA',
                          comment=""" VARIABLES DE COMMANDE (NOMMEES)
""")

PVARIGS = OutputParameter(phys=PHY.NEUT_R, type='ELGA',
                          comment="""""")

PVARINS = OutputParameter(phys=PHY.NEUT_R, type='ELNO',
                          comment="""""")

PVECTR1 = OutputParameter(phys=PHY.VSIZ_R, type='RESL',
                          comment="""  PVECTR1 : VECTEUR SECOND MEMBRE ELEMENTAIRE POUR LA COMPOSANTE 1
""")

PVECTR2 = OutputParameter(phys=PHY.VSIZ_R, type='RESL',
                          comment="""  PVECTR2 : VECTEUR SECOND MEMBRE ELEMENTAIRE POUR LA COMPOSANTE 2
""")

PVECTR3 = OutputParameter(phys=PHY.VSIZ_R, type='RESL',
                          comment="""  PVECTR3 : VECTEUR SECOND MEMBRE ELEMENTAIRE POUR LA COMPOSANTE 3
""")

PVECTR4 = OutputParameter(phys=PHY.VSIZ_R, type='RESL',
                          comment="""  PVECTR4 : VECTEUR SECOND MEMBRE ELEMENTAIRE POUR LA COMPOSANTE 4
""")

PVECTR5 = OutputParameter(phys=PHY.VSIZ_R, type='RESL',
                          comment="""  PVECTR5 : VECTEUR SECOND MEMBRE ELEMENTAIRE POUR LA COMPOSANTE 5
""")

PVECTR6 = OutputParameter(phys=PHY.VSIZ_R, type='RESL',
                          comment="""  PVECTR6 : VECTEUR SECOND MEMBRE ELEMENTAIRE POUR LA COMPOSANTE 6
""")

PVECTTC = OutputParameter(phys=PHY.VPRE_C, type='RESL',
                          comment="""""")

PVECTTI = OutputParameter(phys=PHY.VTEM_R, type='RESL',
                          comment="""""")

PVECTTR = OutputParameter(phys=PHY.VTEM_R, type='RESL',
                          comment="""""")

PVECTU1 = OutputParameter(phys=PHY.VDEP_R, type='RESL',
                          comment="""""")

PVECTU2 = OutputParameter(phys=PHY.VDEP_R, type='RESL',
                          comment="""""")

PVECTU3 = OutputParameter(phys=PHY.VDEP_R, type='RESL',
                          comment="""""")

PVECTU4 = OutputParameter(phys=PHY.VDEP_R, type='RESL',
                          comment="""""")

PVECTU5 = OutputParameter(phys=PHY.VDEP_R, type='RESL',
                          comment="""""")

PVECTU6 = OutputParameter(phys=PHY.VDEP_R, type='RESL',
                          comment="""""")

PVECTUC = OutputParameter(phys=PHY.VDEP_C, type='RESL',
                          comment="""""")

PVECTUR = OutputParameter(phys=PHY.VDEP_R, type='RESL',
                          comment=""" VECTEUR FORCES INTERNES + BT.LAMBDA
""")

PWEIBUL = OutputParameter(phys=PHY.WEIBULL, type='ELEM',
                          comment="""""")


# store all InputParameter & OutputParameter objects
INPUTS = objects_from_context(globals(), InputParameter)
OUTPUTS = objects_from_context(globals(), OutputParameter)
