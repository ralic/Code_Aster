subroutine assvsu(nno, nnos, nface, geom, crit,&
                  deplm, ddepl, congem, congep, vintm,&
                  vintp, defgem, defgep, dsde, matuu,&
                  vectu, rinstm, rinstp, option, imate,&
                  mecani, press1, press2, tempe, dimdef,&
                  dimcon, dimuel, nbvari, ndim, compor,&
                  typmod, typvf, axi, perman, nvoima,&
                  nscoma, nbvois, livois, nbnovo, nbsoco,&
                  lisoco)
! aslint: disable=W1501,W1504
    implicit none
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/cabhvf.h"
#include "asterfort/cacdsu.h"
#include "asterfort/cafmsu.h"
#include "asterfort/cafvsu.h"
#include "asterfort/comthm.h"
#include "asterfort/inicsu.h"
#include "asterfort/mofick.h"
#include "asterfort/nufloc.h"
#include "asterfort/nvithm.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecac2.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/vfcfks.h"
    integer :: maxfa
    parameter (maxfa=6)
!
    integer :: nno, nnos, nface, kpg, spt
    integer :: imate, dimdef, dimcon, dimuel
    integer :: mecani(5), press1(7), press2(7), tempe(5)
    integer :: nbvari, ndim, typvf
    integer :: nvoima, nscoma, nbvois
    integer :: livois(nvoima), nbnovo(nvoima)
    integer :: nbsoco(nvoima), lisoco(nvoima, nscoma, 2)
    real(kind=8) :: geom(ndim, nno), crit(*)
    real(kind=8) :: ddepl(dimuel), deplm(dimuel)
    real(kind=8) :: congem(dimcon, maxfa+1), congep(dimcon, maxfa+1)
    real(kind=8) :: vintm(nbvari, maxfa+1), vintp(nbvari, maxfa+1)
    real(kind=8) :: defgem(dimdef), defgep(dimdef)
    real(kind=8) :: dsde(dimcon, dimdef)
    real(kind=8) :: matuu((nbvois+1)*dimuel*dimuel)
    real(kind=8) :: rinstp, rinstm, vectu(dimuel)
    character(len=8) :: typmod(2), fami, poum
    character(len=16) :: option, compor(*)
    logical :: axi, perman
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!      BUT :
!           CALCUL DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
!           EN MECANIQUE DES MILIEUX POREUX AVEC COUPLAGE THM
!
!
! IN AXI AXISYMETRIQUE?
! IN TYPMOD MODELISATION (D_PLAN, AXI, 3D ?)
! IN MODINT METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
! IN NNO NB DE NOEUDS DE L'ELEMENT
! IN NNOS NB DE NOEUDS SOMMETS DE L'ELEMENT
! IN NFACE NB DE FACES AU SENS BORD DE DIMENSION DIM-1 NE SERT
! QU EN VF
! IN NNOM NB DE NOEUDS MILIEUX DE FACE OU D ARRETE NE SERT QU EN EF
! IN NDDLS NB DE DDL SUR LES SOMMETS
! IN NDDLM NB DE DDL SUR LES MILIEUX DE FACE OU D ARRETE
! NE SERT QU EN EF
! IN NDDLFA NB DE DDL SUR LES FACE DE DIMENSION DIM-1 NE SERT
! QU EN VF
! IN NDDLK NB DE DDL AU CENTRE
! IN TYPVF 1 OU 2 : SCHEMA A DEUX POINTS (NON DISPONIBLE) OU SUDM
! IN NDIM DIMENSION DE L'ESPACE
! IN DIMUEL NB DE DDL TOTAL DE L'ELEMENT
! IN DIMCON DIMENSION DES CONTRAINTES GENERALISEES ELEMENTAIRES
! IN DIMDEF DIMENSION DES DEFORMATIONS GENERALISEES ELEMENTAIRES
! IN GEOM : COORDONNEES DES NOEUDS
! IN OPTION : OPTION DE CALCUL
! IN IMATE : MATERIAU CODE
! IN COMPOR : COMPORTEMENT
! IN CRIT : CRITERES DE CONVERGENCE LOCAUX + ALPHA
! IN DDEPL : DELTA DEPL
! IN DEPLP : DEPLACEMENT A L INSTANT PLUS
! IN DEPLM : DEPLACEMENT A L INSTANT MOINS
! IN MECANI : TABLEAU CONTENANT
! YAMEC = MECA(1), YAMEC = 1 >> IL Y A UNE EQUATION MECANI
! ADDEME = MECA(2), ADRESSE DANS LES TABLEAUX DES DEFORMAT
! GENERALISEES AU POINT DE GAUSS DEFGEP ET DEFGEM DES
! DEFORMATIONS CORRESPONDANT A LA MECANIQUE
! ADCOME = MECA(3), ADRESSE DANS LES TABLEAUX DES CONTRAIN
! GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
! CONTRAINTES CORRESPONDANT A LA MECANIQUE
! NDEFME = MECA(4), NOMBRE DE DEFORMATIONS MECANIQUES
! NCONME = MECA(5), NOMBRE DE CONTRAINTES MECANIQUES
! IN PRESS1 : TABLEAU CONTENANT
! YAP1 = PRESS1(1), YAP1 = 1 >> IL Y A UNE EQUATION DE PRE
! ADDEP1 = PRESS1(3), ADRESSE DANS LES TABLEAUX DES DEFORM
! GENERALISEES AU POINT DE GAUSS DEFGEP ET DEFGEM DES
! DEFORMATIONS CORRESPONDANT A LA PREMIERE PRESSION
! ADCP11=PRESS1(4), ADRESSE DANS LES TABLEAUX DES CONTRAIN
! GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
! CONTRAINTES CORRESPONDANT A LA PREMIERE PHASE DU
! PREMIER CONSTITUANT
! ADCP12=PRESS1(5), ADRESSE DANS LES TABLEAUX DES CONTRAIN
! GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
! CONTRAINTES CORRESPONDANT A LA DEUXIEME PHASE DU
! PREMIER CONSTITUANT
! NDEFP1 = PRESS1(6), NOMBRE DE DEFORMATIONS PRESSION 1
! NCONP1 = PRESS1(7), NOMBRE DE CONTRAINTES POUR
! CHAQUE PHASE DU CONSTITUANT 1
! IN PRESS2 : TABLEAU CONTENANT
! YAP2 = PRESS2(1), YAP2 = 1 >> IL Y A UNE EQUATION DE PRE
!C ADDEP2 = PRESS2(3), ADRESSE DANS LES TABLEAUX DES DEFORM
! GENERALISEES AU POINT DE GAUSS DEFGEP ET DEFGEM DES
! DEFORMATIONS CORRESPONDANT A LA PREMIERE PRESSION
! ADCP21=PRESS2(4), ADRESSE DANS LES TABLEAUX DES CONTRAIN
! GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
! CONTRAINTES CORRESPONDANT A LA PREMIERE PHASE DU
! SECOND CONSTITUANT
! ADCP22=PRESS2(5), ADRESSE DANS LES TABLEAUX DES CONTRAIN
! GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
! CONTRAINTES CORRESPONDANT A LA DEUXIEME PHASE DU
! SECOND CONSTITUANT
! NDEFP2 = PRESS2(6), NOMBRE DE DEFORMATIONS PRESSION 2
! NCONP2 = PRESS2(7), NOMBRE DE CONTRAINTES POUR
! CHAQUE PHASE DU CONSTITUANT 2
!
! IN TEMPE : TABLEAU CONTENANT
! YATE = TEMPE(1), YAMEC = 1 >> IL Y A UNE EQUATION THERMI
! ADDETE = TEMPE(2), ADRESSE DANS LES TABLEAUX DES DEFORMA
! GENERALISEES AU POINT DE GAUSS DEFGEP ET DEFGEM DES
! DEFORMATIONS CORRESPONDANT A LA THERMIQUE
! ADCOTE = TEMPE(3), ADRESSE DANS LES TABLEAUX DES CONTRAI
! GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
! CONTRAINTES CORRESPONDANT A LA THERMIQUE
! NDEFTE = TEMPE(4), NOMBRE DE DEFORMATIONS THERMIQUES
! NCONTE = TEMPE(5), NOMBRE DE CONTRAINTES THERMIQUES
! OUT CODRET : CODE RETOUR LOIS DE COMPORTEMENT
! OUT CONGEP : CONTRAINTES
! OUT VINTP : VARIABLES INTERNES
! OUT MATUU : MATRICE DE RIGIDITE PROFIL(RIGI_MECA_TANG ET FULL_MECA)
! OUT VECTU : FORCES NODALES (RAPH_MECA ET FULL_MECA)
!
! ......................................................................
!
!
!
!
    integer :: con, dconp1, dconp2, diffu, ddifp1, ddifp2
    parameter(con=1,dconp1=2,dconp2=3,diffu=4,ddifp1=5,ddifp2=6)
!
    integer :: mob, dmobp1, dmobp2, masse, dmasp1, dmasp2
    parameter(mob=7,dmobp1=8,dmobp2=9,masse=10,dmasp1=11,dmasp2=12)
!
    integer :: wliq, wvap, airdis, airsec, eau, air, densit
    parameter(wliq=1,wvap=2,airdis=3,airsec=4,eau=1,air=2,densit=14)
!
    integer :: vkint, kxx, kyy, kzz, kxy, kyz, kzx
    parameter(vkint=13,kxx=1,kyy=2,kzz=3,kxy=4,kyz=5,kzx=6)
!
    integer :: rhoga, rholq, rhoga1, rhoga2, rholq1, rholq2
    parameter(rhoga=1,rholq=2,rhoga1=3,rhoga2=4,rholq1=5,rholq2=6)
!
    integer :: maxdim
    parameter (maxdim=3)
!
    integer :: nscom1
    parameter (nscom1=4)
!
    integer :: yamec, yap1, yap2, yate
    integer :: addeme, addep1, addep2, addete, adcome, adcp11, adcp12
    integer :: adcp21, adcp22, adcote
    integer :: nvim, nvit, nvih, nvic
    integer :: advime, advith, advihy, advico
    integer :: vihrho, vicphi, vicpvp, vicsat, vicpr1, vicpr2
    integer :: ipg, nsc, isc, iscl(nscom1)
    integer :: igeomv, imatev, idepmv, iddepv, icompv, icarcv
    integer :: ivarmv, iconmv, iconpv, ivarpv
    integer :: retcom, iret, kvois, fa, i, j
!
    real(kind=8) :: pesa(3)
    real(kind=8) :: rthmc(1)
    real(kind=8) :: valcen(14, 6), valcev(14, 6, maxfa)
    real(kind=8) :: valfac(maxfa, 14, 6)
    real(kind=8) :: valfav(maxfa, 14, 6, maxfa)
    real(kind=8) :: kintvf(6)
    real(kind=8) :: p10, p10v, p20, p20v
!
    integer :: codmes(1)
    character(len=16) :: thmc, loi, meca, ther, hydr
    character(len=24) :: valk(2)
!
!
! ==============================================
! VARIABLES LOCALES POUR CALCULS VF
! ==============================================
!
! PCP PRESSION CAPILLAIRE AU CENTRE DE LA MAILLE
! PCPV PRESSION CAPILLAIRE CENTRE VOISIN
!
! PWP PRESSION EAU
! DPWP1 DERIVEE PRESSION EAU PAR P1
! DPWP2 DERIVEE PRESSION EAU PAR P2
!
! PGP PRESSION DE GAZ AU CENTRE DE LA MAILLE
!
! CVP CONCENTRATION VAPEUR DANS PHASE GAZEUSE
! DCVP1 DERIVEE CVP /P1
! DCVP2 DERIVEE CVP /P2
! CVPV CONCENTRATION VAPEUR DANS PHASE GAZEUSE VOISIN
! DCVP1V DERIVEE CVPV /P1V
! DCVP2V DERIVEE CVPV /P2V
!
!
! CAD ENTRATION AIR DISSOUS
! DCAD1 DERIVEE CAD /P1
! DCAD2 DERIVEE CAD /P2
! CADV CONCENTRATION AIR DISSOUS VOISIN
! DCAD1V DERIVEE CADV /P1V
! DCAD2V DERIVEE CADV /P2V
!
! KINTFA PERMEABILITE INTRINSEQUE SUR UNE FACE
! NT*K*N CACULEE PAR MOYENNE HARMONIQUE
!
!
! =====================================================================
! VARIABLES LOCALES POUR CALCULS VF SUSHI
! =====================================================================
!
! PCPF PRESSION CAPILLAIRE SUR LA FACE
!
! PGPF PRESSION DE GAZ SUR LA FACE
! DPGP1F DERIVEE DE PRESSION DE GAZ /P1 SUR LA FACE
! DPGP2F DERIVEE DE PRESSION DE GAZ /P2 SUR LA FACE
!
! PWPF PRESSION EAU SUR LA FACE
! DPWP1F DERIVEE DE PRESSION EAU /P1 SUR LA FACE
! DPWP2F DERIVEE DE PRESSION EAU /P2 SUR LA FACE
!
! CVPF CONCENTRATION VAPEUR DANS PHASE GAZEUSE SUR LA FACE
! DCVP1F DERIVEE CVP /P1 SUR LA FACE
! DCVP2F DERIVEE CVP /P2 SUR LA FACE
!
! CADF CONCENTRATION AIR DISSOUS SUR LA FACE
! DCAD1F DERIVEE CAD /P1 SUR LA FACE
! DCAD2F DERIVEE CAD /P2 SUR LA FACE
!
! MOBWF(KVOIS,MAXFA) MOBILITE EAU SUR FACE
! DW1F DERIVEE MOBILITE EAU /P1 SUR K
! DW2F DERIVEE MOBILITE EAU /P2 SUR K
! DW1FV DERIVEE MOBILITE EAU /P1 SUR L
! DW2FV DERIVEE MOBILITE EAU /P2 SUR L
!
! MOADF(KVOIS,MAXFA) MOBILITE AIR DISSOUS SUR FACE
! DAD1F DERIVEE MOBILITE AIR DISSOUS /P1 SUR K
! DAD2F DERIVEE MOBILITE AIR DISSOUS /P2 SUR K
! DAD1FV DERIVEE MOBILITE AIR DISSOUS /P1 SUR L
! DAD2FV DERIVEE MOBILITE AIR DISSOUS /P2 SUR L
!
! MOASF(KVOIS,MAXFA) MOBILITE AIR SEC SUR FACE
!
! MOVPF(KVOIS,MAXFA) MOBILITE VAPEUR SUR FACE
! DVP1F DERIVEE MOBILITE VAPEUR /P1 SUR K
! DVP2F DERIVEE MOBILITE VAPEUR /P2 SUR K
! DVP1FV DERIVEE MOBILITE VAPEUR /P1 SUR L
! DVP2FV DERIVEE MOBILITE VAPEUR /P2 SUR L
!
! FLKS FLUX (VOLUMIQUE) LIQUIDE F_{K,SIGMA}(PWP)
! DFLKS1 DERIVEE DE FLKS/P1
! DFLKS2 DERIVEE DE FLKS/P2
!
! FTGKS(IFA) FLUX (VOLUMIQUE)GAZ ~F_{K,SIGMA}
! SUR FACE IFA EN NUM DE K
! FTGKS1 DERIVEE DE FTGKS/P1 :
! FTGKS1(MAXFA+1,MAXFA,)
! FTGKS1( 1,IFA ) D_FTGKS(IFA)/DP1K
! FTGKS1(JFA+1,IFA ) D_FTGKS(IFA)/DP1_FACE_JFA_DE_K
! FTGKS2 DERIVEE DE FTGKS/P2
!
! FCLKS FLUX (VOLUMIQUE)LIQUIDE ^F_{K,SIGMA}(CAD)
! DFCLKS1 DERIVEE DE FCLKS/P1
! DFCLKS2 DERIVEE DE FCLKS/P2
!
! FTGKS FLUX (VOLUMIQUE)GAZ ~F_{K,SIGMA}(CVP)
! FTGKS1 DERIVEE DE FTGKS/P1 :
! FTGKS2 DERIVEE DE FTGKS/P2
!
! C MATRICE INTERVENANT DS LE CALCUL DES FLUX FGKS,FLKS
! D MATRICE INTERVENANT DS LE CALCUL DES FLUX FTGKS,FCLKS
! YSS MATRICE INTERVENANT DS LE CALCUL DES MATRICES C ET D
!
! FLUWS FLUX MASSIQUE EAU TOTAL DANS MAILLE SUSHI
! FLUVPS FLUX MASSIQUE VAPEUR TOTAL DANS MAILLE SUSHI
! FLUASS FLUX MASSIQUE AIR SEC TOTAL DANS MAILLE SUSHI
! FLUADS FLUX MASSIQUE AIR DISSOUS TOTAL DANS MAILLE SUSHI
!
! FW1S(MAXFA+1) DERIVEE FLUW / P1_K PUIS P_1,SIGMA
! FW2S(MAXFA+1) DERIVEE FLUW / P2_K PUIS P_2,SIGMA
! FW1SV(IFA DE 1A MAXFA) D_FLUW / D_P1_L
! L : VOSIN DE K PAR IFA
! FW2SV((IFA DE 1A MAXFA) D_FLUW / D_P2_L
! L : VOSIN DE K PAR IFA
!
! FVP1S(MAXFA+1) DERIVEE FLUVP / P1_K PUIS P_1,SIGMA
! FVP2S(MAXFA+1) DERIVEE FLUVP / P2_K PUIS P_2,SIGMA
! FVP1SV(IFA DE 1A MAXFA) D_FLUVP / D_P1_L
! L : VOSIN DE K PAR IFA
! FVP2SV(IFA DE 1A MAXFA) D_FLUVP / D_P2_L
! L : VOSIN DE K PAR IFA
!
! FAS1S(MAXFA+1) DERIVEE FLUAS / P1_K PUIS P_1,SIGMA
! FAS2S(MAXFA+1) DERIVEE FLUAS / P2_K PUIS P_2,SIGMA
! FAS1SV(IFA DE 1A MAXFA) D_FLUW / D_P1_L
! L : VOSIN DE K PAR IFA
! FAS2SV(IFA DE 1A MAXFA) D_FLUW / D_P1_L
! L : VOSIN DE K PAR IFA
!
! FAD1S(MAXFA+1) DERIVEE FLUAD / P1_K PUIS P_1,SIGMA
! FAD2S(MAXFA+1) DERIVEE FLUAD / P2_K PUIS P_2,SIGMA
! FAD1SV(IFA DE 1A MAXFA) D_FLAD / D_P1_L
! L : VOSIN DE K PAR IFA
! FAD2SV(IFA DE 1A MAXFA) D_FLAD / D_P2_L
! L : VOSIN DE K PAR IFA
!
!
! FMVPS FLUX MASSIQUE VAPEUR INTERVENANT DS EQ DE CONTINUITE
! POUR UNE ARETE EXTERNE
!
! FMWS FLUX MASSIQUE EAU INTERVENANT DS EQ
! DE CONTINUITE POUR UNE ARETE EXTERNE
!
! FMASS FLUX MASSIQUE AIR SEC INTERVENANT DS EQ
! DE CONTINUITE POUR UNE ARETE EXTERNE
!
! FMADS FLUX MASSIQUE AIR DISSOUS INTERVENANT DS EQ
! DE CONTINUITE POUR UNE ARETE EXTERNE
!
! FM1VPS(MAXFA+1,NFACE) DERIVEE DE FMVPS / P_K PUIS P_1,SIGMA
! FM2VPS(MAXFA+1,NFACE) DERIVEE DE FMVPS / P_K PUIS P_2,SIGMA
! FM1WS(MAXFA+1,NFACE) DERIVEE DE FMWS / P_K PUIS P_1,SIGMA
! FM2WS(MAXFA+1,NFACE) DERIVEE DE FMWS / P_K PUIS P_2,SIGMA
! FM1ASS(MAXFA+1,NFACE) DERIVEE DE FMASS / P_K PUIS P_1,SIGMA
! FM2ASS(MAXFA+1,NFACE) DERIVEE DE FMASS / P_K PUIS P_2,SIGMA
! FM1ADS(MAXFA+1,NFACE) DERIVEE DE FMADS / P_K PUIS P_1,SIGMA
! FM2ADS(MAXFA+1,NFACE) DERIVEE DE FMADS / P_K PUIS P_2,SIGMA
!
!
!**********************************************************************
! JE TRAITE DANS CETTE VERSION DE ASSHVF LES TERMES DE DIFFUSION EN
! RÉALISANT UNE MOYENNE ENTRE LA MAILLE COURANTE (K) ET LA MAILLE
! VOISINE L.
!
! REMARQUE :
! ON UTILISE LA NUMEROTATION LOCALE DE LA MAILLE COURANTE POUR
! LES VARIABLES INDENTES PAR K ET ON UTILISE LA NUMEROTATION
! LOCALE DE LA AMILLE L POUR LES VARIABLES INDENTES PAR L
! EXEMPLE : FCLKS => NUMEROTATION LOCALE DE K
! FCLLS => NUMEROTATION LOCALE DE L
!**********************************************************************
!
! FCLLS FLUX (VOLUMIQUE)LIQUIDE DE LA MAILLE L: ^F_{L,SIGMA}(CAD)
! DFCLKS1 DERIVEE DE FCLLS/P1
! DFCLKS2 DERIVEE DE FCLLS/P
!
! FTGLS(IFAV) FLUX (VOLUMIQUE)GAZ DE LA MAILLE L: ~F_{L,SIGMA}(CVP)
! SUR FACE IFAV EN NUM DE L =VOISIN DE K
! FTGLS1 DERIVEE DE FTGLS/P1
! FTGLS1(MAXFA+1,MAXFA,)
! FTGLS1( 1,IFAV ) D_FTGLS(IFAV)/DP1L
! FTGLS1(JFAV+1,IFAV ) D_FTGLS(IFAV)/DP1_FACE_JFAV_DE_L
! JFAV EST EN NUMEROTATION LOCALE DE L
! FTGLS2 DERIVEE DE FTGLS/P2
!
!
! FLVPSK(IFA) PRODUIT DE LA DIFFUSION DE VAPEUR D'EAU PAR FTGKS
! SUR FACE IFA EN NUM DE K
! FVP1SK DERIVEE DE FLVPSK PAR RAPPORT A PRE1 AU CENTRE ET
! AUX ARETES :
! FVP1SK(MAXFA+1,MAXFA,)
! FVP1SK( 1,IFA ) D_FLVPSK(IFA)/DP1K
! FVP1SK(JFA+1,IFA ) D_FLVPSK(IFA)/DP1_FACE_JFA_DE_K
! FVP2SK DERIVEE DE FLVPSK PAR RAPPORT A PRE2 AU CENTRE ET
! AUX ARETES
!
! FLASSK PRODUIT DE LA DIFFUSION DE L AIR SEC PAR FTGKS
! FAS1SK DERIDEE DE FLASSK PAR RAPPORT A PRE1 AU CENTRE ET
! AUX ARETES
! FAS2SK DERIDEE DE FLASSK PAR RAPPORT A PRE2 AU CENTRE ET
! AUX ARETES
!
! FLADSK PRODUIT DE LA DIFFUSION DE L AIR DISSOUS PAR FCLKS
! FAD1SK DERIDEE DE FLADSK PAR RAPPORT A PRE1 AU CENTRE ET
! AUX ARETES
! FAD2SK DERIDEE DE FLADSK PAR RAPPORT A PRE2 AU CENTRE ET
! AUX ARETES
!
! FLVPSL(IFAV) PRODUIT DE LA DIFFUSION DE VAPEUR D'EAU PAR FTGKS
! SUR FACE IFAV EN NUM DE L =VOISIN DE K
! FVP1SL DERIVEE DE FLVPSL PAR RAPPORT A PRE1 AU CENTRE ET
! AUX ARETES :
! FVP1SL(MAXFA+1,MAXFA,)
! FVP1SL( 1,IFAV ) D_FLVPSL(IFAV)/DP1L
! FVP1SL(JFAV+1,IFAV ) D_FLVPSL(IFAV)/DP1_FACE_JFAV_DE_L
! JFAV EST EN NUMEROTATION LOCALE DE L
! FVP2SL DERIVEE DE FLVPSK PAR RAPPORT A PRE2 AU CENTRE ET
! AUX ARETES
!
! FLASSL PRODUIT DE LA DIFFUSION DE L AIR SEC PAR FTGLS
! FAS1SL DERIDEE DE FLASSL PAR RAPPORT A PRE1 AU CENTRE ET
! AUX ARETES
! FAS2SL DERIDEE DE FLASSL PAR RAPPORT A PRE2 AU CENTRE ET
! AUX ARETES
!
! FLADSL PRODUIT DE LA DIFFUSION DE L AIR DISSOUS PAR FCLLS
! FAD1SL DERIDEE DE FLADSL PAR RAPPORT A PRE1 AU CENTRE ET
! AUX ARETES
! FAD2SL DERIDEE DE FLADSL PAR RAPPORT A PRE2 AU CENTRE ET
! AUX ARETES
!
! MOYVP(MAXFA) DEMI SOMME DE FLVPSK ET FLVPSL
! FA ETANT CONJUGUEES / FA DAN K = FAV DANS L
! MOYVP(FA) 0.5(FLVPSK(FA) ET FLVPSL(FAV))
! MOYVP1(MAXFA,MAXFA+1,0:1)
! DERIVEES DE MOYVP POUR LA VARIABLE P1 :
! MOYVP1(IFA,1,0 ) D_MOYVP(IFA)/DP1K
! MOYVP1(IFA,JFA+1,0 ) D_MOYVP(IFA)/DP1_FACE_JFA_DE_K
! MOYVP1(IFA,1,1 ) D_MOYVP(IFA)/DP1L
! MOYVP1(IFA,JFAV+1,1 ) D_MOYVP(IFA)/DP1_FACE_JFAV_DE_L
!
! MOYVP2(MAXFA,MAXFA+1,0:1) DERIVEES DE MOYVP POUR UNE ARETE
! COMMUNE A K ET L PAR RAPPORT A PRE2 AU CENTRE PUIS
! AU ARETE DE K PUIS PAR RAPPORT A PRE2 AU CENTRE PUIS
! AU ARETE DE L
!
! MOYAS TABLEAU STOCKANT LA DEMI SOMME DE FLASSK ET FLASSL
! MOYAS1(MAXFA,MAXFA+1,0:1) DERIVEES DE MOYAS
! MOYAS2(MAXFA,MAXFA+1,0:1) DERIVEES DE MOYAS
!
! MOYAD TABLEAU STOCKANT LA DEMI SOMME DE FLADSK ET FLADSL
! MOYAD1(MAXFA,MAXFA+1,0:1) DERIVEES DE MOYAD
! MOYAD2(MAXFA,MAXFA+1,0:1) DERIVEES DE MOYAD
!
! FAV NUMERO DE L ARETE DANS LA NUMEROTATION LOCALE DE
! LA MAILLE VOISINE L
!
! VOIFA(IFA,1) = NUMERO DE MAILLE PARTAGEANT LA FACE IFA
! VOIFA(IFA,2) = NUMERO DE LA FACE IFA
! DANS LA NUMEROTATION LOCALE DU VOISIN
!
!  VALFAC(I,CON,WLIQ)     CONCENTRATION DE L EAU LIQUIDE SUR ARRETE I
!  VALFAC(I,DCONP1,WLIQ)  D_CON_EAU_LIQU_I /P1
!  VALFAC(I,DCONP2,WLIQ)  D_CON_EAU_LIQU_I /P2
!  VALFAC(I,DIFFU,WLIQ)   DIFFUW SUR ARETE I
!  VALFAC(I,DDIFP1,WLIQ)  D_DIFFUW_I /P1
!  VALFAC(I,DDIFP2,WLIQ)  D_DIFFUW_I /P2
!  VALFAC(I,MOB,WLIQ)     MOBILITE DE L EAU LIQUIDE SUR ARETE I
!  VALFAC(I,DMOBP1,WLIQ)  D_MO_LIQU /P1_CENTRE
!  VALFAC(I,DMOBP2,WLIQ)  D_MO_LIQU /P2_CENTRE
!
! NB: DE MEME POUR WVAP(EAU VAPEUR),AIRDIS(AIR DISSOUS),AIRSEC(AIR SEC)
!
! VALFAV  MEME TABLEAU MAIS POUR LES VOISINS
!  VALCEN(MOB,WLIQ)       MOBILITE EAU LIQUIDE CENTRE
!  VALCEN(DMOBP1,WLIQ)    D_MO_LIQU /P1_CENTRE
!  VALCEN(DMOBP2,WLIQ)    D_MO_LIQU /P2_CENTRE
! NB: DE MEME POUR WVAP(EAU VAPEUR),AIRDIS(AIR DISSOUS),AIRSEC(AIR SEC)
!  VALCEN(CON,WVAP)       CONCENTRATION EAU VAPEUR
!  VALCEN(DCONP1,WVAP)
!  VALCEN(DCONP2,WVAP)
!  VALCEN(CON,AIRDIS)     CONCENTRATION AIR DISSOUS
!  VALCEN(DCONP1,AIRDIS)
!  VALCEN(DCONP2,AIRDIS)
!  VALCEN(DIFFU,WLIQ)     DIFFU EAU LIQUIDE
!  VALCEN(DDIFP1,WLIQ)
!  VALCEN(DDIFP2,WLIQ)
! NB: DE MEME POUR WVAP(EAU VAPEUR),AIRDIS(AIR DISSOUS),AIRSEC(AIR SEC)
!
! VARIATION MASSE TOTALE EAU/DTEMPS
!
!   VALCEN(MASSE ,EAU) MASSE EAU
!   VALCEN(DMASP1,EAU)
!   VALCEN(DMASP2,EAU)
!
! VARIATION MASSE TOTALE AIR/DTEMPS
!
!  VALCEN(MASSE ,AIR) MASSE AIR
!  VALCEN(DMASP1,AIR)
!  VALCEN(DMASP2,AIR)
!
!   VALCEV IDEM POUR VOISIN
! =====================================================================
! VARIABLES COMMUNES
! =====================================================================
!
    logical :: tange, cont, vf, bool
    integer :: numav
    real(kind=8) :: mface(maxfa), dface(maxfa), xface(maxdim, maxfa), normfa(maxdim, maxfa), vol
    real(kind=8) :: volv
    integer :: ifa, jfa, jfav, idim
    real(kind=8) :: pcp, dpwp1, dpwp2
    real(kind=8) :: pgp, dpgp1, dpgp2
    real(kind=8) :: pwp
    real(kind=8) :: cvp, cvpv, dcvp1, dcvp2, dcvp1v, dcvp2v
    real(kind=8) :: cad, cadv, dcad1, dcad2, dcad1v, dcad2v
!
    integer :: nfacem
    integer :: voifa(maxfa, 2)
    logical :: finter(maxfa)
! =====================================================================
! VARIABLES VF SUSHI
! =====================================================================
!
    real(kind=8) :: fluws, fluvps, fluass, fluads
!
    real(kind=8) :: fw1s(maxfa+1), fw2s(maxfa+1)
    real(kind=8) :: fvp1s(maxfa+1), fvp2s(maxfa+1)
    real(kind=8) :: fas1s(maxfa+1), fas2s(maxfa+1)
    real(kind=8) :: fad1s(maxfa+1), fad2s(maxfa+1)
!
    real(kind=8) :: fw1sv(maxfa), fw2sv(maxfa)
    real(kind=8) :: fvp1sv(maxfa), fvp2sv(maxfa)
    real(kind=8) :: fas1sv(maxfa), fas2sv(maxfa)
    real(kind=8) :: fad1sv(maxfa), fad2sv(maxfa)
!
    real(kind=8) :: fmvps(maxfa), fmws(maxfa)
    real(kind=8) :: fmass(maxfa), fmads(maxfa)
!
    real(kind=8) :: fm1vps(maxfa+1, maxfa), fm2vps(maxfa+1, maxfa)
    real(kind=8) :: fm1ws(maxfa+1, maxfa), fm2ws(maxfa+1, maxfa)
    real(kind=8) :: fm1ass(maxfa+1, maxfa), fm2ass(maxfa+1, maxfa)
    real(kind=8) :: fm1ads(maxfa+1, maxfa), fm2ads(maxfa+1, maxfa)
!
    real(kind=8) :: pcpf(maxfa), pgpf(maxfa), dpgp1f(maxfa), dpgp2f(maxfa), pwpf(maxfa)
    real(kind=8) :: dpwp1f(maxfa), dpwp2f(maxfa), cvpf(maxfa), dcvp1f(maxfa), cadf(maxfa)
    real(kind=8) :: dcad1f(maxfa), dcad2f(maxfa), dcvp2f(maxfa)
!
    real(kind=8) :: mobwf(0:nvoima, maxfa), moadf(0:nvoima, maxfa), moasf(0:nvoima, maxfa)
    real(kind=8) :: movpf(0:nvoima, maxfa), dw1f(0:nvoima, maxfa), dw2f(0:nvoima, maxfa)
    real(kind=8) :: dw1fv(0:nvoima, maxfa), dw2fv(0:nvoima, maxfa), dvp1f(0:nvoima, maxfa)
    real(kind=8) :: dvp2f(0:nvoima, maxfa), dvp1fv(0:nvoima, maxfa), dvp2fv(0:nvoima, maxfa)
    real(kind=8) :: das1f(0:nvoima, maxfa), das2f(0:nvoima, maxfa), das1fv(0:nvoima, maxfa)
    real(kind=8) :: das2fv(0:nvoima, maxfa), dad1f(0:nvoima, maxfa), dad2f(0:nvoima, maxfa)
    real(kind=8) :: dad1fv(0:nvoima, maxfa), dad2fv(0:nvoima, maxfa)
!
!
    real(kind=8) :: yss(maxdim, maxfa, maxfa)
    real(kind=8) :: c(maxfa, maxfa), d(maxfa, maxfa)
!
    real(kind=8) :: flks(maxfa), dflks1(maxfa+1, maxfa), dflks2(maxfa+1, maxfa), fgks(maxfa)
    real(kind=8) :: dfgks1(maxfa+1, maxfa), dfgks2(maxfa+1, maxfa), ftgks(maxfa)
    real(kind=8) :: ftgks1(maxfa+1, maxfa), ftgks2(maxfa+1, maxfa), fclks(maxfa)
    real(kind=8) :: fclks1(maxfa+1, maxfa), fclks2(maxfa+1, maxfa)
!
!
! SI FICKFA =TRUE LES DIFFUSIVITES DE FICK SONT CALCULEES A PARTIR
! DES INCONNUES AUX FACES SINON ELLES SONT CALCULEES AU CENTRE
!
    logical :: fickfa
! SI DECEN=.FALSE. ALORS ON NE FAIT AUCUN DECENTRAGE
    logical :: decen
!=====================================================================
! NOUVELLE VARIABLES POUR SUSHI POUR TRAITER LA DIFFUSION PAR MOYENNE
    real(kind=8) :: fclls(maxfa), fclls1(maxfa+1, maxfa), fclls2(maxfa+1, maxfa), ftgls(maxfa)
    real(kind=8) :: ftgls2(maxfa+1, maxfa), ftgls1(maxfa+1, maxfa)
!
    real(kind=8) :: flvpsk(maxfa), fvp1sk(maxfa+1, maxfa), fvp2sk(maxfa+1, maxfa), flassk(maxfa)
    real(kind=8) :: fas1sk(maxfa+1, maxfa), fas2sk(maxfa+1, maxfa), fladsk(maxfa)
    real(kind=8) :: fad1sk(maxfa+1, maxfa), fad2sk(maxfa+1, maxfa)
!
    real(kind=8) :: flvpsl(maxfa), fvp1sl(maxfa+1, maxfa), fvp2sl(maxfa+1, maxfa), flassl(maxfa)
    real(kind=8) :: fas1sl(maxfa+1, maxfa), fas2sl(maxfa+1, maxfa), fladsl(maxfa)
    real(kind=8) :: fad1sl(maxfa+1, maxfa), fad2sl(maxfa+1, maxfa)
!
    real(kind=8) :: moyvp(maxfa), moyvp1(maxfa, maxfa+1, 0:1), moyvp2(maxfa, maxfa+1, 0:1)
    real(kind=8) :: moyas(maxfa), moyas1(maxfa, maxfa+1, 0:1), moyas2(maxfa, maxfa+1, 0:1)
    real(kind=8) :: moyad(maxfa), moyad1(maxfa, maxfa+1, 0:1), moyad2(maxfa, maxfa+1, 0:1)
!
    real(kind=8) :: dl (maxfa, maxfa)
    real(kind=8) :: yssl (maxdim, maxfa, maxfa)
    real(kind=8) :: cl (maxfa, maxfa)
!
    integer :: fav, ifav
    real(kind=8) :: mfacel(maxfa), dfacel(maxfa), xfacel(maxdim, maxfa), norfal(maxdim, maxfa)
!
    real(kind=8) :: cvpfv(maxfa), cvpfv1(maxfa), cvpfv2(maxfa)
    real(kind=8) :: cadfv(maxfa), cadfv1(maxfa), cadfv2(maxfa)
    integer :: nfacev
    integer :: nnov, nnosv
    real(kind=8) :: alpha
    logical :: uticer
    real(kind=8) :: xg(maxdim), xl(maxdim)
    real(kind=8) :: rhol, rhog, drhol1, drhol2, drhog1, drhog2
    real(kind=8) :: zero
!     DANS LE CAS DES ELEMENTS FINIS ANGMAS EST NECESSAIRE
!     DANS LE CAS DES VOLUMES FINIS ON INITIALISE À 0 ANGMAS(7)
    real(kind=8) :: angbid(3)
    integer :: ivois
    integer :: iadp1k, iadp2k
    integer :: adcm1, adcm2
!
!---------------------------------------
! FONCTIONS FORMULES D ADRESSAGE DES DDL
!---------------------------------------
#define zzadma(ivois,lig,col) (ivois)*(dimuel)*(dimuel)+(lig-1)*(dimuel)+col
#define iadp1(fa) 2*(fa-1)+1
#define iadp2(fa) 2*(fa-1)+2
#define adcf1(fa) 2*(fa-1)+1
#define adcf2(fa) 2*(fa-1)+2
    iadp1k=2*nface+1
    iadp2k=2*nface+2
    adcm1 = 2*nface+1
    adcm2 = 2*nface+2
    call inicsu(valcen, valcev, valfac, valfav, maxfa)
!
! ================================================================
! --- INITIALISATION
! ================================================================
    zero=0.d0
    call vecini(maxdim, zero, xg)
!
    call vecini(maxfa, zero, pcpf)
    call vecini(maxfa, zero, pgpf)
    call vecini(maxfa, zero, dpgp1f)
    call vecini(maxfa, zero, dpgp2f)
    call vecini(maxfa, zero, pwpf)
    call vecini(maxfa, zero, dpwp1f)
    call vecini(maxfa, zero, dpwp2f)
    call vecini(maxfa, zero, cvpf)
    call vecini(maxfa, zero, dcvp1f)
    call vecini(maxfa, zero, dcvp2f)
    call vecini(maxfa, zero, cadf)
    call vecini(maxfa, zero, dcad1f)
    call vecini(maxfa, zero, dcad2f)
!
    alpha = crit(13)
!---------------------------------------
! INITIALISATION DE ANGMAS(3) À ZERO
    do 511 i = 1, 3
        angbid(i)=0.d0
511  end do
!============================
! ACTUELLEMENT ON OBLIGE A PRENDRE LE CENTRE
! AU CENTRE DE GRAVITE
! SI ON VEUT TESTER LE CENTRE DU CERCLE CIRCONSCRIT
! ON PRENDRA UTICER = TRUE
!===============================
    uticer = .false.
!
    bool = (option(1:9).eq.'RIGI_MECA' ) .or. (option(1:9).eq.'RAPH_MECA' ) .or.&
           (option(1:9).eq.'FULL_MECA' )
    ASSERT(bool)
!
    fickfa = .false.
    vf = .true.
    perman = .false.
!
    decen = .true.
!
    do 1 ifa = 1, nface
        finter(ifa)=.false.
        voifa(ifa,1)=0
        voifa(ifa,2)=0
 1  end do
!
    do 100 idim = 1, ndim
        xg(idim)=geom(idim,nno)
100  end do
!
!
! ====================================================================
! --- DETERMINATION DES VARIABLES CARACTERISANT LE MILIEU ------------
! ====================================================================
    yamec = mecani(1)
    addeme = mecani(2)
    adcome = mecani(3)
    yap1 = press1(1)
    addep1 = press1(3)
    adcp11 = press1(4)
    adcp12 = press1(5)
    yap2 = press2(1)
    addep2 = press2(3)
    adcp21 = press2(4)
    adcp22 = press2(5)
    yate = tempe(1)
    addete = tempe(2)
    adcote = tempe(3)
! ====================================================================
! --- CALCUL DE CONSTANTES TEMPORELLES -------------------------------
! ====================================================================
!
    loi = ' '
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, imate,&
                ' ', 'THM_INIT', 0, ' ', [0.d0],&
                1, 'COMP_THM', rthmc, codmes, 1)
    thmc = compor(8)
    if ((rthmc(1)-1.0d0) .lt. r8prem()) then
        loi = 'LIQU_SATU'
    else if ((rthmc(1)-2.0d0).lt.r8prem()) then
        loi = 'GAZ'
    else if ((rthmc(1)-3.0d0).lt.r8prem()) then
        loi = 'LIQU_VAPE'
    else if ((rthmc(1)-4.0d0).lt.r8prem()) then
        loi = 'LIQU_VAPE_GAZ'
    else if ((rthmc(1)-5.0d0).lt.r8prem()) then
        loi = 'LIQU_GAZ'
    else if ((rthmc(1)-6.0d0).lt.r8prem()) then
        loi = 'LIQU_GAZ_ATM'
    else if ((rthmc(1)-9.0d0).lt.r8prem()) then
        loi = 'LIQU_AD_GAZ_VAPE'
    else if ((rthmc(1)-10.0d0).lt.r8prem()) then
        loi = 'LIQU_AD_GAZ'
    endif
    if (thmc .ne. loi) then
        valk(1) = loi
        valk(2) = thmc
        call utmess('F', 'ALGORITH_34', nk=2, valk=valk)
    endif
! ====================================================================
! DECLARATION DE DEUX LOGIQUES POUR SAVOIR CE QUE L ON DOIT CALCULER
! TANGE => CALCUL OPERATEUR TANGENT => MATUU
! CONT => CALCUL RESIDU => VECTU
! ====================================================================
    cont = .false.
    tange = .false.
!
    if (option(1:9) .eq. 'RIGI_MECA') then
        tange = .true.
    else if (option(1:9).eq.'RAPH_MECA') then
        cont = .true.
    else if (option(1:9).eq.'FULL_MECA') then
        tange = .true.
        cont = .true.
    else
        valk(1) = option
        call utmess('F', 'VOLUFINI_8', sk=valk(1))
    endif
! ====================================================================
! --- INITIALISATION A ZERO MATUU ET VECTU
! ====================================================================
    if (tange) then
        do 2, i = 1 , (nbvois+1)*dimuel*dimuel
        matuu(i)=0.d0
        2     end do
    endif
    if (cont) then
        do 3 i = 1, dimuel
            vectu(i)=0.d0
 3      end do
    endif
! ================================================================
! --- CALCUL DES QUANTITES GEOMETRIQUES
! ================================================================
    call cabhvf(maxfa, maxdim, ndim, nno, nnos,&
                nface, axi, geom, vol, mface,&
                dface, xface, normfa, uticer)
! ================================================================
! --- CALCUL DES DEFORMATIONS GENERALISEES ----------------------
! ON MET DANS LE TABLEAU DES DEF GENERALISES LES PRESSIONS
! LES GRADIENTS SONT MIS A ZERO CAR ON NE SAIT PAS LES CALCULER
! A CE NIVEAU EN VF4
! ================================================================
!
    if (typvf .eq. 2) then
        if (yap1 .eq. 1) then
            defgem(addep1)= deplm(iadp1k)
            defgep(addep1)= ddepl(iadp1k)+deplm(iadp1k)
            do 4 i = 1, ndim
                defgem(addep1+i)=0.d0
                defgep(addep1+i)=0.d0
 4          continue
            if (yap2 .eq. 1) then
                defgem(addep2)= deplm(iadp2k)
                defgep(addep2)= ddepl(iadp2k)+deplm(iadp2k)
                do 5 i = 1, ndim
                    defgem(addep2+i)=0.d0
                    defgep(addep2+i)=0.d0
 5              continue
            endif
        endif
    else
        call utmess('F', 'VOLUFINI_9', si=typvf)
    endif
! ===============================================
! ==== INITIALISATION DE DSDE ================
! ===============================================
    do 6 i = 1, dimcon
        do 6 j = 1, dimdef
            dsde(i,j)=0.d0
 6      continue
!
    call comthm(option, perman, vf, 0, valfac,&
                valcen, imate, typmod, compor, crit,&
                rinstm, rinstp, ndim, dimdef, dimcon,&
                nbvari, yamec, yap1, yap2, yate,&
                addeme, adcome, addep1, adcp11, adcp12,&
                addep2, adcp21, adcp22, addete, adcote,&
                defgem, defgep, congem, congep, vintm(1, 1),&
                vintp(1, 1), dsde, pesa, retcom, 1,&
                1, p10, p20, angbid)
    if (retcom .ne. 0) then
        call utmess('F', 'COMPOR1_9')
    endif
    if ((typvf.eq.2)) then
        do 7 fa = 1, nface
            if (yap1 .eq. 1) then
                defgem(addep1)= deplm(iadp1(fa))
                defgep(addep1)= ddepl(iadp1(fa))+deplm(iadp1(fa))
                do 701 i = 1, ndim
                    defgem(addep1+i)=0.d0
                    defgep(addep1+i)=0.d0
701              continue
                if (yap2 .eq. 1) then
                    defgem(addep2)= deplm(iadp2(fa))
                    defgep(addep2)= ddepl(iadp2(fa))+deplm(iadp2(fa))
                    do 702 i = 1, ndim
                        defgem(addep2+i)=0.d0
                        defgep(addep2+i)=0.d0
702                  continue
                endif
            else
                call utmess('F', 'VOLUFINI_9', si=typvf)
            endif
! ===============================================
! ==== INITIALISATION DE DSDE ================
! ===============================================
            do 703 i = 1, dimcon
                do 61 j = 1, dimdef
                    dsde(i,j)=0.d0
61              continue
703          continue
            call comthm(option, perman, vf, fa, valfac,&
                        valcen, imate, typmod, compor, crit,&
                        rinstm, rinstp, ndim, dimdef, dimcon,&
                        nbvari, yamec, yap1, yap2, yate,&
                        addeme, adcome, addep1, adcp11, adcp12,&
                        addep2, adcp21, adcp22, addete, adcote,&
                        defgem, defgep, congem, congep, vintm(1, fa+1),&
                        vintp(1, fa+1), dsde, pesa, retcom, 1,&
                        1, p10, p20, angbid)
            if (retcom .ne. 0) then
                call utmess('F', 'COMPOR1_9')
            endif
 7      continue
    endif
    if (cont) then
        vectu(adcm1)=valcen(masse ,eau)*vol
        vectu(adcm2)=valcen(masse ,air)*vol
    endif
    if (tange) then
        matuu(zzadma(0,adcm1,iadp1k))=valcen(dmasp1,eau)*vol
        matuu(zzadma(0,adcm2,iadp1k))=valcen(dmasp1,air)*vol
        matuu(zzadma(0,adcm1,iadp2k))=valcen(dmasp2,eau)*vol
        matuu(zzadma(0,adcm2,iadp2k))=valcen(dmasp2,air)*vol
    endif
    rhol=valcen(densit ,rholq)
    drhol1=valcen(densit ,rholq1)
    drhol2=valcen(densit ,rholq2)
!
    rhog=valcen(densit ,rhoga)
    drhog1=valcen(densit ,rhoga1)
    drhog2=valcen(densit ,rhoga2)
!
! CALCUL DES QUANTITES RELATIVES AUX FACES POUR LE VF SUSHI
!
    if ((typvf.eq.2)) then
!
! CALCUL DES MATRICES YSS C ET D
!
        if (ndim .eq. 2) then
            kintvf(1) = valcen(vkint ,kxx)
            kintvf(2) = valcen(vkint ,kyy)
            kintvf(3) = valcen(vkint ,kxy)
            kintvf(4) = 0.d0
            kintvf(5) = 0.d0
            kintvf(6) = 0.d0
        else
            kintvf(1) = valcen(vkint ,kxx)
            kintvf(2) = valcen(vkint ,kyy)
            kintvf(3) = valcen(vkint ,kzz)
            kintvf(4) = valcen(vkint ,kxy)
            kintvf(5) = valcen(vkint ,kyz)
            kintvf(6) = valcen(vkint ,kzx)
        endif
        call cacdsu(maxfa, maxdim, alpha, ndim, nno,&
                    nface, geom, vol, mface, dface,&
                    xface, normfa, kintvf, yss, c,&
                    d)
        pcp = ddepl(iadp1k)+deplm(iadp1k)+p10
        pgp = ddepl(iadp2k)+deplm(iadp2k)+p20
        if (loi .eq. 'LIQU_AD_GAZ') then
!
! ON STOCK PC ET PG DU CENTRE SUR TOUS LES POINTS DE GAUSS
! EN VUE DE POST TRAITEMENT
! CECI EST PROVISOIRE
!
            call nvithm(compor, meca, thmc, ther, hydr,&
                        nvim, nvit, nvih, nvic, advime,&
                        advith, advihy, advico, vihrho, vicphi,&
                        vicpvp, vicsat, vicpr1, vicpr2)
            do 71 ipg = 1, nface+1
                vintp(advico+vicpr1,ipg) = pcp
                vintp(advico+vicpr2,ipg) = pgp
71          continue
        endif
        dpgp1 = 0.d0
        dpgp2 = 1.d0
        do 8 ifa = 1, nface
            pcpf(ifa) = ddepl(iadp1(ifa))+deplm(iadp1(ifa))+p10
            pgpf(ifa) = ddepl(iadp2(ifa))+deplm(iadp2(ifa))+p20
            dpgp1f(ifa) = 0.d0
            dpgp2f(ifa) = 1.d0
 8      continue
        pwp = pgp-pcp
        dpwp1 = -1.d0
        dpwp2 = +1.d0
        do 9 ifa = 1, nface
            pwpf(ifa) = pgpf(ifa)-pcpf(ifa)
            dpwp1f(ifa) = -1.d0
            dpwp2f(ifa) = 1.d0
 9      continue
        cvp = valcen(con,wvap)
        dcvp1 = valcen(dconp1,wvap)
        dcvp2 = valcen(dconp2,wvap)
        do 10 ifa = 1, nface
            cvpf (ifa) = valfac(ifa,con,wvap)
            dcvp1f(ifa) = valfac(ifa,dconp1,wvap)
            dcvp2f(ifa) = valfac(ifa,dconp2,wvap)
10      continue
        cad = valcen(con,airdis)
        dcad1 = valcen(dconp1,airdis)
        dcad2 = valcen(dconp2,airdis)
        do 11 ifa = 1, nface
            cadf(ifa) = valfac(ifa,con,airdis)
            dcad1f(ifa) = valfac(ifa,dconp1,airdis)
            dcad2f(ifa) = valfac(ifa,dconp2,airdis)
11      continue
! ===========================================================
! INITIALISATION
! ===========================================================
        do 12 ifa = 1, maxfa+1
            fw1s(ifa)=0.d0
            fw2s(ifa)=0.d0
            fvp1s(ifa)=0.d0
            fvp2s(ifa)=0.d0
            fas1s(ifa)=0.d0
            fas2s(ifa)=0.d0
            fad1s(ifa)=0.d0
            fad2s(ifa)=0.d0
12      continue
        do 121 ifa = 1, maxfa
            fw1sv(ifa)=0.d0
            fw2sv(ifa)=0.d0
            fvp1sv(ifa)=0.d0
            fvp2sv(ifa)=0.d0
            fas1sv(ifa)=0.d0
            fas2sv(ifa)=0.d0
            fad1sv(ifa)=0.d0
            fad2sv(ifa)=0.d0
121      continue
        fluws=0.d0
        fluvps=0.d0
        fluass=0.d0
        fluads=0.d0
        do 13 ifa = 1, maxfa
            fmvps(ifa)=0.d0
            fmws(ifa)=0.d0
            fmass(ifa)=0.d0
            fmads(ifa)=0.d0
!
            flks(ifa) =0.d0
            fgks(ifa) =0.d0
            fclks(ifa) =0.d0
            ftgks(ifa) =0.d0
!
            flvpsk(ifa)=0.d0
            flassk(ifa)=0.d0
            fladsk(ifa)=0.d0
!
            moyvp(ifa)=0.d0
            moyas(ifa)=0.d0
            moyad(ifa)=0.d0
13      continue
        do 14 jfa = 1, maxfa
            do 14 ifa = 1, maxfa+1
                dflks1(ifa,jfa)=0.d0
                dflks2(ifa,jfa)=0.d0
                dfgks1(ifa,jfa)=0.d0
                dfgks2(ifa,jfa)=0.d0
                ftgks1(ifa,jfa)=0.d0
                ftgks2(ifa,jfa)=0.d0
                fclks1(ifa,jfa)=0.d0
                fclks2(ifa,jfa)=0.d0
!
                fm1ws(ifa,jfa)=0.d0
                fm2ws(ifa,jfa)=0.d0
                fm1vps(ifa,jfa)=0.d0
                fm2vps(ifa,jfa)=0.d0
                fm1ass(ifa,jfa)=0.d0
                fm2ass(ifa,jfa)=0.d0
                fm1ads(ifa,jfa)=0.d0
                fm2ads(ifa,jfa)=0.d0
!
                fvp1sk(ifa,jfa)=0.d0
                fvp2sk(ifa,jfa)=0.d0
                fas1sk(ifa,jfa)=0.d0
                fas2sk(ifa,jfa)=0.d0
                fad1sk(ifa,jfa)=0.d0
                fad2sk(ifa,jfa)=0.d0
14          continue
        do 15 ivois = 0, nvoima
            do 16 jfa = 1, maxfa
                mobwf(ivois,jfa)=0.d0
                moasf(ivois,jfa)=0.d0
                moadf(ivois,jfa)=0.d0
                movpf(ivois,jfa)=0.d0
!
                dw1f(ivois,jfa)=0.d0
                dw2f(ivois,jfa)=0.d0
                dw1fv(ivois,jfa)=0.d0
                dw2fv(ivois,jfa)=0.d0
!
                dad1f(ivois,jfa)=0.d0
                dad2f(ivois,jfa)=0.d0
                dad1fv(ivois,jfa)=0.d0
                dad2fv(ivois,jfa)=0.d0
!
                das1f(ivois,jfa)=0.d0
                das2f(ivois,jfa)=0.d0
                das1fv(ivois,jfa)=0.d0
                das2fv(ivois,jfa)=0.d0
!
                dvp1f(ivois,jfa)=0.d0
                dvp2f(ivois,jfa)=0.d0
                dvp1fv(ivois,jfa)=0.d0
                dvp2fv(ivois,jfa)=0.d0
16          continue
15      continue
        do 17 ifa = 1, maxfa
            do 18 jfa = 1, maxfa+1
                do 19 ivois = 0, 1
                    moyvp1(ifa,jfa,ivois)=0.d0
                    moyvp2(ifa,jfa,ivois)=0.d0
                    moyas1(ifa,jfa,ivois)=0.d0
                    moyas2(ifa,jfa,ivois)=0.d0
                    moyad1(ifa,jfa,ivois)=0.d0
                    moyad2(ifa,jfa,ivois)=0.d0
19              continue
18          continue
17      continue
! ========================================
! FLUX VOLUMIQUES
! ========================================
        call vfcfks(.true., tange, maxfa, nface, cvp,&
                    dcvp1, dcvp2, cvpf, dcvp1f, dcvp2f,&
                    d, pesa, zero, zero, zero,&
                    xg, xface, maxdim, ndim, ftgks,&
                    ftgks1, ftgks2)
!
        call vfcfks(.true., tange, maxfa, nface, cad,&
                    dcad1, dcad2, cadf, dcad1f, dcad2f,&
                    d, pesa, zero, zero, zero,&
                    xg, xface, maxdim, ndim, fclks,&
                    fclks1, fclks2)
        if (.not.fickfa) then
            do 20 ifa = 1, nface
                valfac(ifa,diffu,wvap)= valcen(diffu,wvap)
                valfac(ifa,diffu,airsec)= valcen(diffu,airsec)
!
                valfac(ifa,diffu,airdis)= valcen(diffu,airdis)
                valfac(ifa,diffu,wliq) = valcen(diffu,wliq)
!
                valfac(ifa,ddifp1,wvap) = valcen(ddifp1,wvap)
                valfac(ifa,ddifp2,wvap) = valcen(ddifp2,wvap)
!
                valfac(ifa,ddifp1,airsec) = valcen(ddifp1,airsec)
                valfac(ifa,ddifp2,airsec) = valcen(ddifp2,airsec)
!
                valfac(ifa,ddifp1,airdis) = valcen(ddifp1,airdis)
                valfac(ifa,ddifp2,airdis) = valcen(ddifp2,airdis)
!
                valfac(ifa,ddifp1,wliq) = valcen(ddifp1,wliq)
                valfac(ifa,ddifp2,wliq) = valcen(ddifp2,wliq)
20          continue
        endif
! ========================================
! FLUX MASSIQUES
! ========================================
        do 21 ifa = 1, nface
            call cafmsu(ifa, cont, tange, maxfa, nface,&
                        ftgks(ifa), ftgks1, ftgks2, valfac(ifa, diffu, wvap),&
                        valfac(ifa, ddifp1, wvap), valfac(ifa, ddifp2, wvap), flvpsk, fvp1sk,&
                        fvp2sk)
            call cafmsu(ifa, cont, tange, maxfa, nface,&
                        ftgks(ifa), ftgks1, ftgks2, -valfac(ifa, diffu, airsec),&
                        -valfac(ifa, ddifp1, airsec), -valfac(ifa, ddifp2, airsec), flassk,&
                        fas1sk, fas2sk)
            call cafmsu(ifa, cont, tange, maxfa, nface,&
                        fclks(ifa), fclks1, fclks2, valfac(ifa, diffu, airdis),&
                        valfac(ifa, ddifp1, airdis), valfac(ifa, ddifp2, airdis), fladsk, fad1sk,&
                        fad2sk)
21      continue
    else
        ASSERT(.false.)
    endif
! ENDIF DE LA LIGNE      1098
!=====================================================================
! BOUCLE SUR LES VOISINS
!=====================================================================
    do 22 kvois = 1, nbvois
!
! NUMAV EST LE NUMERO DE MAILLE VOISINE CETTE VARIABLE SERT
! DANS TOUTES LES FONCTIONS D ACCES AUX TERMES ELEMENTAIRES
! DES VOISIN
!
        numav=livois(kvois)
!
! FA= NUMERO DE FACE DANS MAILLE MAITRE COMMUNE
! PAR CONVENTION UNE FACE IFA A POUR PREMIER SOMMET
! LE SOMMET DE NUMEROTATION LOCALE IFA
! FAV= NUMERO DE FACE DANS LA NUMEROTATION LOCALE DE LA MAILLE
! VOISINE
!
        nsc=nbsoco(kvois)
        do 220 isc = 1, nsc
            iscl(isc)=lisoco(kvois,isc,1)
220      continue
        fa = nufloc(ndim,nsc,iscl)
        do 228 isc = 1, nsc
            iscl(isc)=lisoco(kvois,isc,2)
228      continue
        fav = nufloc(ndim,nsc,iscl)
        nnov=nbnovo(kvois)
        if (nnov .eq. 7) then
            nnosv=3
        else if (nnov .eq. 9) then
            nnosv=4
        else if (nnov .eq. 10) then
            nnosv=4
        else if (nnov .eq. 27) then
            nnosv=4
        else if (nnov .eq. 27) then
            call utmess('F', 'VOLUFINI_15')
        endif
        nfacev=nnosv
        if (nface .gt. nfacev) then
            nfacem=nface
        else if (nface .lt. nfacev) then
            nfacem=nfacev
        else if (nface .eq. nfacev) then
            nfacem=nface
        endif
        do 229 idim = 1, ndim
            xl(idim)=geom(idim,nnov)
229      continue
        finter(fa)=.true.
        voifa(fa,1)=kvois
        voifa(fa,2)=fav
!
! ======INITIALISATION=================
        do 133 jfa = 1, maxfa
            flvpsl(jfa)=0.d0
            flassl(jfa)=0.d0
            fladsl(jfa)=0.d0
            fclls(jfa) =0.d0
            ftgls(jfa) =0.d0
            do 144 ifa = 1, maxfa+1
                fvp1sl(ifa,jfa)=0.d0
                fvp2sl(ifa,jfa)=0.d0
                fas1sl(ifa,jfa)=0.d0
                fas2sl(ifa,jfa)=0.d0
                fad1sl(ifa,jfa)=0.d0
                fad2sl(ifa,jfa)=0.d0
                ftgls1(ifa,jfa)=0.d0
                ftgls2(ifa,jfa)=0.d0
                fclls1(ifa,jfa)=0.d0
                fclls2(ifa,jfa)=0.d0
144          continue
133      continue
! ================================================================
! --- PARAMETRES EN ENTREE POUR LE VOISIN CONSIDERE
! ================================================================
        call tecac2('OOO', numav, 'PGEOMER', 'L', 1,&
                    igeomv, iret)
        ASSERT(iret.eq.0)
        call tecac2('OOO', numav, 'PMATERC', 'L', 1,&
                    imatev, iret)
        ASSERT(iret.eq.0)
        call tecac2('OOO', numav, 'PDEPLMR', 'L', 1,&
                    idepmv, iret)
        ASSERT(iret.eq.0)
        call tecac2('OOO', numav, 'PCOMPOR', 'L', 1,&
                    icompv, iret)
        ASSERT(iret.eq.0)
        call tecac2('OOO', numav, 'PCARCRI', 'L', 1,&
                    icarcv, iret)
        ASSERT(iret.eq.0)
        call tecac2('OOO', numav, 'PVARIMR', 'L', 1,&
                    ivarmv, iret)
        ASSERT(iret.eq.0)
        call tecac2('OOO', numav, 'PCONTMR', 'L', 1,&
                    iconmv, iret)
        ASSERT(iret.eq.0)
        if (cont) then
            call tecac2('OOO', numav, 'PDEPLPR', 'L', 1,&
                        iddepv, iret)
            ASSERT(iret.eq.0)
            call tecac2('OOO', numav, 'PCONTPR', 'E', 1,&
                        iconpv, iret)
            ASSERT(iret.eq.0)
            call tecac2('OOO', numav, 'PVARIPR', 'E', 1,&
                        ivarpv, iret)
            ASSERT(iret.eq.0)
        else
            ivarpv = ivarmv
            iconpv = iconmv
            iddepv = idepmv
        endif
        if ((typvf.eq.2)) then
! ================================================================
! --- CALCUL DES QUANTITES GEOMETRIQUES DE LA MAILLE VOISINE
! ================================================================
            call cabhvf(maxfa, maxdim, ndim, nnov, nnosv,&
                        nface, axi, zr( igeomv), volv, mfacel,&
                        dfacel, xfacel, norfal, uticer)
            call cacdsu(maxfa, maxdim, alpha, ndim, nnov,&
                        nfacev, zr( igeomv), volv, mfacel, dfacel,&
                        xfacel, norfal, kintvf, yssl, cl,&
                        dl)
        else
            ASSERT(.false.)
        endif
! ******************************************************
! CALCUL POUR LE CENTRE DE LA MAILLE VOISINE
! ******************************************************
        if (yap1 .eq. 1) then
            defgem(addep1)= zr(idepmv+iadp1k-1)
            defgep(addep1)= zr(iddepv+iadp1k-1)+zr(idepmv+iadp1k-1)
            if (yap2 .eq. 1) then
                defgem(addep2)= zr(idepmv+iadp2k-1)
                defgep(addep2)= zr(iddepv+iadp2k-1)+zr(idepmv+iadp2k-&
                1)
            endif
        endif
! ===============================================
! ==== INITIALISATION DE DSDE ================
! ===============================================
        do 221 i = 1, dimcon
            do 221 j = 1, dimdef
                dsde(i,j)=0.d0
221          continue
        call comthm(option, perman, vf, 0, valfav(1, 1, 1, fa),&
                    valcev(1, 1, fa), zi(imatev), typmod, zk16(icompv), zr(icarcv),&
                    rinstm, rinstp, ndim, dimdef, dimcon,&
                    nbvari, yamec, yap1, yap2, yate,&
                    addeme, adcome, addep1, adcp11, adcp12,&
                    addep2, adcp21, adcp22, addete, adcote,&
                    defgem, defgep, zr(iconmv), zr(iconpv), zr(ivarmv),&
                    zr(ivarpv), dsde, pesa, retcom, 1,&
                    1, p10v, p20v, angbid)
!
        if (retcom .ne. 0) then
            call utmess('F', 'COMPOR1_9')
        endif
! ******************************************************
! CALCUL POUR LES ARETES DE LA MAILLE VOISINE
! ******************************************************
        do 227 ifav = 1, nfacev
            if (yap1 .eq. 1) then
                defgem(addep1)= zr(idepmv+iadp1(ifav)-1)
                defgep(addep1)= zr(iddepv+iadp1(ifav)-1)+ zr(idepmv+&
                iadp1(ifav)-1)
                if (yap2 .eq. 1) then
                    defgem(addep2)= zr(idepmv+iadp2(ifav)-1)
                    defgep(addep2)= zr(iddepv+iadp2(ifav)-1)+ zr(&
                    idepmv+iadp2(ifav)-1)
                endif
            endif
! ===============================================
! ==== INITIALISATION DE DSDE ================
! ===============================================
            do 222 i = 1, dimcon
                do 222 j = 1, dimdef
                    dsde(i,j)=0.d0
222              continue
!
            call comthm(option, perman, vf, ifav, valfav(1, 1, 1, fa),&
                        valcev(1, 1, fa), zi(imatev), typmod, zk16(icompv), zr(icarcv),&
                        rinstm, rinstp, ndim, dimdef, dimcon,&
                        nbvari, yamec, yap1, yap2, yate,&
                        addeme, adcome, addep1, adcp11, adcp12,&
                        addep2, adcp21, adcp22, addete, adcote,&
                        defgem, defgep, zr(iconmv), zr( iconpv), zr(ivarmv+nbvari*ifav),&
                        zr(ivarpv+nbvari*ifav), dsde, pesa, retcom, 1,&
                        1, p10v, p20v, angbid)
!
            if (retcom .ne. 0) then
                call utmess('F', 'COMPOR1_9')
            endif
227      continue
! CALCUL DES VARIABLES POUR LA MAILLE VOISINE
! COMMUNES A VF 2PNTS ET SUSHI
        cvpv = valcev(con,wvap,fa)
        dcvp1v = valcev(dconp1,wvap,fa)
        dcvp2v = valcev(dconp2,wvap,fa)
!
!
        cadv = valcev(con,airdis,fa)
        dcad1v = valcev(dconp1,airdis,fa)
        dcad2v = valcev(dconp2,airdis,fa)
        if ((typvf.eq.2)) then
            do 223 ifav = 1, nfacev
                cvpfv(ifav)=valfav(ifav,con,wvap,fa)
                cvpfv1(ifav)=valfav(ifav,dconp1,wvap,fa)
                cvpfv2(ifav)=valfav(ifav,dconp2,wvap,fa)
                cadfv(ifav)=valfav(ifav,con,airdis,fa)
                cadfv1(ifav)=valfav(ifav,dconp1,airdis,fa)
                cadfv2(ifav)=valfav(ifav,dconp2,airdis,fa)
223          continue
            call vfcfks(.true., tange, maxfa, nfacev, cvpv,&
                        dcvp1v, dcvp2v, cvpfv, cvpfv1, cvpfv2,&
                        dl, pesa, zero, zero, zero,&
                        xl, xfacel, maxdim, ndim, ftgls,&
                        ftgls1, ftgls2)
!
            call vfcfks(.true., tange, maxfa, nfacev, cadv,&
                        dcad1v, dcad2v, cadfv, cadfv1, cadfv2,&
                        dl, pesa, zero, zero, zero,&
                        xl, xfacel, maxdim, ndim, fclls,&
                        fclls1, fclls2)
            if (.not.fickfa) then
                do 224 ifav = 1, nfacev
                    valfav(ifav,diffu,wvap,fa)= valcev(diffu,wvap,fa)
                    valfav(ifav,diffu,airsec,fa)= valcev(diffu,airsec,&
                    fa)
!
                    valfav(ifav,diffu,airdis,fa)= valcev(diffu,airdis,&
                    fa)
                    valfav(ifav,diffu,wliq,fa) = valcev(diffu,wliq,fa)
!
                    valfav(ifav,ddifp1,wvap,fa) = valcev(ddifp1,wvap, fa)
                    valfav(ifav,ddifp2,wvap,fa) = valcev(ddifp2,wvap, fa)
!
                    valfav(ifav,ddifp1,airsec,fa) = valcev(ddifp1, airsec,fa)
                    valfav(ifav,ddifp2,airsec,fa) = valcev(ddifp2, airsec,fa)
!
                    valfav(ifav,ddifp1,airdis,fa) = valcev(ddifp1, airdis,fa)
                    valfav(ifav,ddifp2,airdis,fa) = valcev(ddifp2, airdis,fa)
!
                    valfav(ifav,ddifp1,wliq,fa) = valcev(ddifp1,wliq, fa)
                    valfav(ifav,ddifp2,wliq,fa) = valcev(ddifp2,wliq, fa)
224              continue
            endif
! ========================================
! FLUX MASSIQUES
! ========================================
            call cafmsu(fav, cont, tange, maxfa, nfacev,&
                        ftgls(fav), ftgls1, ftgls2, valfav(fav, diffu, wvap, fa),&
                        valfav(fav, ddifp1, wvap, fa), valfav(fav, ddifp2, wvap, fa), flvpsl,&
                        fvp1sl, fvp2sl)
            call cafmsu(fav, cont, tange, maxfa, nfacev,&
                        ftgls(fav), ftgls1, ftgls2, -valfav(fav, diffu, airsec, fa),&
                        -valfav(fav, ddifp1, airsec, fa), -valfav(fav, ddifp2, airsec, fa),&
                        flassl, fas1sl, fas2sl)
            call cafmsu(fav, cont, tange, maxfa, nfacev,&
                        fclls(fav), fclls1, fclls2, valfav(fav, diffu, airdis, fa),&
                        valfav(fav, ddifp1, airdis, fa), valfav(fav, ddifp2, airdis, fa), fladsl,&
                        fad1sl, fad2sl)
            call mofick(fa, fav, cont, tange, maxfa,&
                        nface, nfacev, nfacem, flvpsk, fvp1sk,&
                        fvp2sk, flvpsl, fvp1sl, fvp2sl, moyvp,&
                        moyvp1, moyvp2)
            call mofick(fa, fav, cont, tange, maxfa,&
                        nface, nfacev, nfacem, flassk, fas1sk,&
                        fas2sk, flassl, fas1sl, fas2sl, moyas,&
                        moyas1, moyas2)
            call mofick(fa, fav, cont, tange, maxfa,&
                        nface, nfacev, nfacem, fladsk, fad1sk,&
                        fad2sk, fladsl, fad1sl, fad2sl, moyad,&
                        moyad1, moyad2)
        endif
!====================================================================
!===================================================================
! FIN DE LA BOUCLE SUR LES VOISINS
22  end do
!====================================================================
!====================================================================
! VF SUSHI
!====================================================================
!====================================================================
    if ((typvf.eq.2)) then
! ========================================
! FLUX VOLUMIQUES
! ========================================
        call vfcfks(.true., tange, maxfa, nface, pwp,&
                    dpwp1, dpwp2, pwpf, dpwp1f, dpwp2f,&
                    c, pesa, rhol, drhol1, drhol2,&
                    xg, xface, maxdim, ndim, flks,&
                    dflks1, dflks2)
        call vfcfks(.true., tange, maxfa, nface, pgp,&
                    dpgp1, dpgp2, pgpf, dpgp1f, dpgp2f,&
                    c, pesa, rhog, drhog1, drhog2,&
                    xg, xface, maxdim, ndim, fgks,&
                    dfgks1, dfgks2)
! -------------------------------------------------------------------
! DECEN
! -------------------------------------------------------------------
        do 23 ifa = 1, nface
            kvois=voifa(ifa,1)
            if (finter(ifa) .and. decen) then
                numav=livois(kvois)
!
! SI FLKS>=0 ON SE PLACE DS LA MAILLE COURANTE
! SINON DS LA MAILLE VOISINE (INVERSE DE VF HDI)
!
                if (flks(ifa) .ge. 0.d0) then
                    mobwf(0,ifa) = valcen(mob,wliq)
                    dw1f(0,ifa) = valcen(dmobp1,wliq)
                    dw2f(0,ifa) = valcen(dmobp2,wliq)
                    dw1fv(0,ifa) = 0.d0
                    dw2fv(0,ifa) = 0.d0
!
                    moadf(0,ifa) = valcen(mob,airdis)
                    dad1f(0,ifa) = valcen(dmobp1,airdis)
                    dad2f(0,ifa) = valcen(dmobp2,airdis)
                    dad1fv(0,ifa) =0.d0
                    dad2fv(0,ifa) =0.d0
                else
                    mobwf(kvois,ifa) = valcev(mob,wliq,ifa)
                    dw1f(kvois,ifa) = 0.d0
                    dw2f(kvois,ifa) = 0.d0
                    dw1fv(kvois,ifa) = valcev(dmobp1,wliq,ifa)
                    dw2fv(kvois,ifa) = valcev(dmobp2,wliq,ifa)
!
                    moadf(kvois,ifa) = valcev(mob,airdis,ifa)
                    dad1f(kvois,ifa) = 0.d0
                    dad2f(kvois,ifa) = 0.d0
                    dad1fv(kvois,ifa) = valcev(dmobp1,airdis,ifa)
                    dad2fv(kvois,ifa) = valcev(dmobp2,airdis,ifa)
                endif
! SI FGKS>=0 ON SE PLACE DS LA MAILLE COURANTE
! SINON DS LA MAILLE VOISINE(INVERSE DE VF HDI)
!
                if (fgks(ifa) .ge. 0.d0) then
                    moasf(0,ifa) = valcen(mob,airsec)
                    das1f(0,ifa) = valcen(dmobp1,airsec)
                    das2f(0,ifa) = valcen(dmobp2,airsec)
                    das1fv(0,ifa) = 0.d0
                    das2fv(0,ifa) = 0.d0
!
                    movpf(0,ifa) = valcen(mob,wvap)
                    dvp1f(0,ifa) = valcen(dmobp1,wvap)
                    dvp2f(0,ifa) = valcen(dmobp2,wvap)
                    dvp1fv(0,ifa) = 0.d0
                    dvp2fv(0,ifa) = 0.d0
                else
                    moasf(kvois,ifa) = valcev(mob,airsec,ifa)
                    das1f(kvois,ifa) = 0.d0
                    das2f(kvois,ifa) = 0.d0
                    das1fv(kvois,ifa) = valcev(dmobp1,airsec,ifa)
                    das2fv(kvois,ifa) = valcev(dmobp2,airsec,ifa)
!
                    movpf(kvois,ifa) = valcev(mob,wvap,ifa)
                    dvp1f(kvois,ifa) = 0.d0
                    dvp2f(kvois,ifa) = 0.d0
                    dvp1fv(kvois,ifa) = valcev(dmobp1,wvap,ifa)
                    dvp2fv(kvois,ifa) = valcev(dmobp2,wvap,ifa)
                endif
            else
                mobwf(0,ifa) = valcen(mob,wliq)
                dw1f(0,ifa) = valcen(dmobp1,wliq)
                dw2f(0,ifa) = valcen(dmobp2,wliq)
                dw1fv(0,ifa) = 0.d0
                dw2fv(0,ifa) = 0.d0
!
                moadf(0,ifa) = valcen(mob,airdis)
                dad1f(0,ifa) = valcen(dmobp1,airdis)
                dad2f(0,ifa) = valcen(dmobp2,airdis)
                dad1fv(0,ifa) =0.d0
                dad2fv(0,ifa) =0.d0
!
                moasf(0,ifa) = valcen(mob,airsec)
                das1f(0,ifa) = valcen(dmobp1,airsec)
                das2f(0,ifa) = valcen(dmobp2,airsec)
                das1fv(0,ifa) = 0.d0
                das2fv(0,ifa) = 0.d0
!
                movpf(0,ifa) = valcen(mob,wvap)
                dvp1f(0,ifa) = valcen(dmobp1,wvap)
                dvp2f(0,ifa) = valcen(dmobp2,wvap)
                dvp1fv(0,ifa) = 0.d0
                dvp2fv(0,ifa) = 0.d0
            endif
23      continue
!=======================================================
! CALCUL DE :FLUW,FLUVP,FLUAS,FLUAD
!=======================================================
        call cafvsu(.true., tange, maxfa, nface, flks,&
                    dflks1, dflks2, mobwf, dw1f, dw2f,&
                    dw1fv, dw2fv, fluws, fw1s, fw2s,&
                    fw1sv, fw2sv, nbvois, nvoima)
        call cafvsu(.true., tange, maxfa, nface, fgks,&
                    dfgks1, dfgks2, movpf, dvp1f, dvp2f,&
                    dvp1fv, dvp2fv, fluvps, fvp1s, fvp2s,&
                    fvp1sv, fvp2sv, nbvois, nvoima)
        call cafvsu(.true., tange, maxfa, nface, fgks,&
                    dfgks1, dfgks2, moasf, das1f, das2f,&
                    das1fv, das2fv, fluass, fas1s, fas2s,&
                    fas1sv, fas2sv, nbvois, nvoima)
        call cafvsu(.true., tange, maxfa, nface, flks,&
                    dflks1, dflks2, moadf, dad1f, dad2f,&
                    dad1fv, dad2fv, fluads, fad1s, fad2s,&
                    fad1sv, fad2sv, nbvois, nvoima)
        do 24 ifa = 1, nface
            if (.not.finter(ifa)) then
                call cafmsu(ifa, .true., tange, maxfa, nface,&
                            fgks(ifa), dfgks1, dfgks2, movpf(0, ifa), dvp1f(0, ifa),&
                            dvp2f(0, ifa), fmvps, fm1vps, fm2vps)
                call cafmsu(ifa, .true., tange, maxfa, nface,&
                            flks(ifa), dflks1, dflks2, mobwf(0, ifa), dw1f(0, ifa),&
                            dw2f(0, ifa), fmws, fm1ws, fm2ws)
                call cafmsu(ifa, .true., tange, maxfa, nface,&
                            fgks(ifa), dfgks1, dfgks2, moasf(0, ifa), das1f(0, ifa),&
                            das2f(0, ifa), fmass, fm1ass, fm2ass)
                call cafmsu(ifa, .true., tange, maxfa, nface,&
                            flks(ifa), dflks1, dflks2, moadf(0, ifa), dad1f(0, ifa),&
                            dad2f(0, ifa), fmads, fm1ads, fm2ads)
            endif
24      continue
        if (cont) then
! ********************************************************************
! EQUATION DE LA CONTINUITE DES FLUX
!                 SI ARETE DE BORD :
!                                 | FLKS |
!                                 | FGKS |
!                 SINON :
!                                 | FMWS + FMVPS + FLVPSK |
!                                 | FMASS + FMADS + FLASSK + FLADSK|
! ********************************************************************
            do 25 ifa = 1, nface
                if (finter(ifa)) then
                    congep(adcp11+1,ifa+1)= flks(ifa)
                    congep(adcp12+1,ifa+1)= fgks(ifa)
                else
                    congep(adcp11+1,ifa+1)=fmws(ifa)+fmvps(ifa)+&
                    flvpsk(ifa)
                    congep(adcp12+1,ifa+1)=fmass(ifa)+fmads(ifa)+&
                    flassk(ifa) +fladsk(ifa)
                endif
                vectu(adcf1(ifa))=congep(adcp11+1,ifa+1)
                vectu(adcf2(ifa))=congep(adcp12+1,ifa+1)
25          continue
! ********************************************************************
! EQUATION DE LA CONSERVATION DE LA MASSE
!                 SI ARETE DE BORD :
!                                 | FLUWS + FLUVPS + MOYVP |
!                                 | FLUASS + FLUADS + MOYAS + MOYAD |
!                 SINON :
!                 | FLUWS + FLUVPS + FLVPSK |
!                                 | FLUASS + FLUADS + FLASSK + FLADSK |
! ********************************************************************
            congep(adcp11+1,1)= fluws
            congep(adcp12+1,1)= fluvps
            congep(adcp21+1,1)= fluass
            congep(adcp22+1,1)= fluads
            do 26 ifa = 1, nface
                if (finter(ifa)) then
                    congep(adcp12+1,1)=congep(adcp12+1,1)+ moyvp(ifa)
                    congep(adcp21+1,1)=congep(adcp21+1,1)+ moyas(ifa)
                    congep(adcp22+1,1)=congep(adcp22+1,1)+ moyad(ifa)
                else
                    congep(adcp12+1,1)=congep(adcp12+1,1)+ flvpsk(ifa)
                    congep(adcp21+1,1)=congep(adcp21+1,1)+ flassk(ifa)
                    congep(adcp22+1,1)=congep(adcp22+1,1)+ fladsk(ifa)
                endif
26          continue
            vectu(adcm1)= vectu(adcm1)+congep(adcp11+1,1) +congep(&
            adcp12+1,1)
            vectu(adcm2)= vectu(adcm2)+congep(adcp21+1,1) +congep(&
            adcp22+1,1)
        endif
        if (tange) then
! *******************************************************************
! EQUATION DE LA CONSERVATION DE LA MASSE POUR K
!                 (DERIVEES % VARIABLES DU CENTRE)
!                 SANS LA PARTIE FICKIENNE POUR INTERNE ET EXTERNE
!                 | FW1S + FVP1S |
!                 | FW2S + FVP2S |
!                 | FAS1S + FAD1S |
!                 | FAS2S + FAD2S |
! *******************************************************************
            matuu(zzadma(0,adcm1,iadp1k))= matuu(zzadma(0,adcm1,iadp1k))+fw1s(1)+fvp1s(1)
!
            matuu(zzadma(0,adcm1,iadp2k))= matuu(zzadma(0,adcm1,iadp2k))+fw2s(1)+fvp2s(1)
!
            matuu(zzadma(0,adcm2,iadp1k))= matuu(zzadma(0,adcm2,iadp1k))+fas1s(1)+fad1s(1)
!
            matuu(zzadma(0,adcm2,iadp2k))= matuu(zzadma(0,adcm2,iadp2k))+fas2s(1)+fad2s(1)
            do 27 ifa = 1, nface
! ATTENTION QD LES MAILLES SERONT DIFFERENTES NFACEV PEUT ETRE DIFFERENT
! SELON LES MAILLES
                kvois=voifa(ifa,1)
                fav =voifa(ifa,2)
                numav=livois(kvois)
!
! *******************************************************************
! EQUATION DE LA CONSERVATION DE LA MASSE POUR K
!                 (DERIVEES % VARIABLES DE L ARETE)
!                 POUR INTERNE ET EXTERNE
!                 | FW1S + FVP1S |
!                 | FW2S + FVP2S |
!                 | FAS1S + FAD1S |
!                 | FAS2S + FAD2S |
! *******************************************************************
                matuu(zzadma(0,adcm1,iadp1(ifa)))= matuu(zzadma(0,adcm1,iadp1(ifa))) +&
                    fw1s(ifa+1)+fvp1s(ifa+1)
!
                matuu(zzadma(0,adcm1,iadp2(ifa)))= matuu(zzadma(0,adcm1,iadp2(ifa))) +&
                    fw2s(ifa+1)+fvp2s(ifa+1)
!
                matuu(zzadma(0,adcm2,iadp1(ifa)))= matuu(zzadma(0,adcm2,iadp1(ifa))) +&
                    fas1s(ifa+1)+fad1s(ifa+1)
!
                matuu(zzadma(0,adcm2,iadp2(ifa)))= matuu(zzadma(0,adcm2,iadp2(ifa))) +&
                    fas2s(ifa+1)+fad2s(ifa+1)
! *******************************************************************
! EQUATION DE LA CONSERVATION DE LA MASSE POUR L
!                 (DERIVEES % VARIABLES DU CENTRE)
!                 SANS LA PARTIE FICKIENNE PR INTERNE ET EXTERNE
!                 | FW1SV + FVP1SV |
!                 | FW2SV + FVP2SV |
!                 | FAS1SV + FAD1SV |
!                 | FAS2SV + FAD2SV |
! *******************************************************************
!
                matuu(zzadma(kvois,adcm1,iadp1k))= matuu(zzadma(kvois,adcm1,iadp1k))+&
                    fw1sv(ifa)+fvp1sv(ifa)
!
                matuu(zzadma(kvois,adcm1,iadp2k))= matuu(zzadma(kvois,adcm1,iadp2k))+&
                    fw2sv(ifa)+fvp2sv(ifa)
!
                matuu(zzadma(kvois,adcm2,iadp1k))= matuu(zzadma(kvois,adcm2,iadp1k))+&
                    fas1sv(ifa)+fad1sv(ifa)
!
                matuu(zzadma(kvois,adcm2,iadp2k))= matuu(zzadma(kvois,adcm2,iadp2k))+&
                    fas2sv(ifa)+fad2sv(ifa)
!
                if (finter(ifa)) then
                    nnov=nbnovo(kvois)
                    ASSERT(nnov.eq.nno)
! *******************************************************************
! EQUATION DE LA CONSERVATION DE LA MASSE POUR K
!                 (DERIVEES % VARIABLES DU CENTRE)
!                 RAJOUT DE LA PARTIE FICKIENNE
!                 | MOYVP1 |
!                 | MOYVP2 |
!                 | MOYAS1+ MOYAD1 |
!                 | MOYAS2+ MOYAD2 |
! *******************************************************************
                    matuu(zzadma(0,adcm1,iadp1k))= matuu(zzadma(0,adcm1,iadp1k))+&
                        moyvp1(ifa,1,0)
!
                    matuu(zzadma(0,adcm1,iadp2k))= matuu(zzadma(0,adcm1,iadp2k))+&
                        moyvp2(ifa,1,0)
!
                    matuu(zzadma(0,adcm2,iadp1k))= matuu(zzadma(0,adcm2,iadp1k))+&
                        moyas1(ifa,1,0) + moyad1(ifa,1,0)
!
                    matuu(zzadma(0,adcm2,iadp2k))= matuu(zzadma(0,adcm2,iadp2k))+&
                        moyas2(ifa,1,0) + moyad2(ifa,1,0)
! *******************************************************************
! EQUATION DE LA CONSERVATION DE LA MASSE POUR K
!                 (DERIVEES % VARIABLES DE L ARETE)
!                 RAJOUT DE LA PARTIE FICKIENNE
!                 | MOYVP1 |
!                 | MOYVP2 |
!                 | MOYAS1+ MOYAD1 |
!                 | MOYAS2+ MOYAD2 |
! ******************************************************************
                    do 996 jfa = 1, nface
                        matuu(zzadma(0,adcm1,iadp1(jfa)))= matuu(&
                        zzadma(0,adcm1,iadp1(jfa))) +moyvp1(ifa,jfa+1,&
                        0)
!
                        matuu(zzadma(0,adcm1,iadp2(jfa)))= matuu(&
                        zzadma(0,adcm1,iadp2(jfa))) +moyvp2(ifa,jfa+1,&
                        0)
!
                        matuu(zzadma(0,adcm2,iadp1(jfa)))= matuu(&
                        zzadma(0,adcm2,iadp1(jfa))) +moyas1(ifa,jfa+1,&
                        0)+moyad1(ifa,jfa+1,0)
!
                        matuu(zzadma(0,adcm2,iadp2(jfa)))= matuu(&
                        zzadma(0,adcm2,iadp2(jfa))) +moyas2(ifa,jfa+1,&
                        0)+moyad2(ifa,jfa+1,0)
996                  continue
! *******************************************************************
! EQUATION DE LA CONTINUITE DES FLUX POUR K
!                 (DERIVEES % VARIABLES DU CENTRE)
!                 | DFLKS1 |
!                 | DFLKS2 |
!                 | DFGKS1 |
!                 | DFGKS2 |
! *****************************************************************
                    matuu(zzadma(0,adcf1(ifa),iadp1k))= matuu(&
                        zzadma(0,adcf1(ifa),iadp1k)) +dflks1(1,ifa)
!
                    matuu(zzadma(0,adcf1(ifa),iadp2k))= matuu(&
                        zzadma(0,adcf1(ifa),iadp2k)) +dflks2(1,ifa)
!
                    matuu(zzadma(0,adcf2(ifa),iadp1k))= matuu(&
                        zzadma(0,adcf2(ifa),iadp1k)) +dfgks1(1,ifa)
!
                    matuu(zzadma(0,adcf2(ifa),iadp2k))= matuu(&
                        zzadma(0,adcf2(ifa),iadp2k)) +dfgks2(1,ifa)
! *******************************************************************
! EQUATION DE LA CONTINUITE DES FLUX POUR K
!                 (DERIVEES % VARIABLES DE L ARETE)
!                 | DFLKS1 |
!                 | DFLKS2 |
!                 | DFGKS1 |
!                 | DFGKS2 |
! *******************************************************************
                    do 271 jfa = 1, nface
                        matuu(zzadma(0,adcf1(ifa),iadp1(jfa)))=&
                        matuu(zzadma(0,adcf1(ifa),iadp1(jfa)))&
                        +dflks1(1+jfa,ifa)
!
                        matuu(zzadma(0,adcf1(ifa),iadp2(jfa)))=&
                        matuu(zzadma(0,adcf1(ifa),iadp2(jfa)))&
                        +dflks2(1+jfa,ifa)
!
                        matuu(zzadma(0,adcf2(ifa),iadp1(jfa)))=&
                        matuu(zzadma(0,adcf2(ifa),iadp1(jfa)))&
                        +dfgks1(1+jfa,ifa)
!
                        matuu(zzadma(0,adcf2(ifa),iadp2(jfa)))=&
                        matuu(zzadma(0,adcf2(ifa),iadp2(jfa)))&
                        +dfgks2(1+jfa,ifa)
271                  continue
! *******************************************************************
! EQUATION DE LA CONSERVATION DE LA MASSE POUR L
!                 (DERIVEES % VARIABLES DU CENTRE)
!                 RAJOUT DE LA PARTIE FICKIENNE
!                 | MOYVP1 |
!                 | MOYVP2 |
!                 | MOYAS1+ MOYAD1 |
!                 | MOYAS2+ MOYAD2 |
! *******************************************************************
                    matuu(zzadma(kvois,adcm1,iadp1k))= matuu(&
                        zzadma(kvois,adcm1,iadp1k)) + moyvp1(ifa,1,1)
!
                    matuu(zzadma(kvois,adcm1,iadp2k))= matuu(&
                        zzadma(kvois,adcm1,iadp2k)) + moyvp2(ifa,1,1)
!
                    matuu(zzadma(kvois,adcm2,iadp1k))= matuu(&
                        zzadma(kvois,adcm2,iadp1k)) + moyas1(ifa,1,1)+ moyad1(&
                    ifa,1,1)
!
                    matuu(zzadma(kvois,adcm2,iadp2k))= matuu(&
                        zzadma(kvois,adcm2,iadp2k)) + moyas2(ifa,1,1)+ moyad2(&
                    ifa,1,1)
! *******************************************************************
! EQUATION DE LA CONSERVATION DE LA MASSE POUR L
!                 (DERIVEES % VARIABLES DE L ARETE)
!                 | MOYVP1 |
!                 | MOYVP2 |
!                 | MOYAS1+ MOYAD1 |
!                 | MOYAS2+ MOYAD2 |
! *******************************************************************
                    do 997 jfav = 1, nfacev
                        matuu(zzadma(kvois,adcm1,iadp1(jfav)))=&
                        matuu(zzadma(kvois,adcm1,iadp1(jfav)))+&
                        moyvp1(ifa,jfav+1,1)
!
                        matuu(zzadma(kvois,adcm1,iadp2(jfav)))=&
                        matuu(zzadma(kvois,adcm1,iadp2(jfav)))+&
                        moyvp2(ifa,jfav+1,1)
!
                        matuu(zzadma(kvois,adcm2,iadp1(jfav)))=&
                        matuu(zzadma(kvois,adcm2,iadp1(jfav)))+&
                        moyas1(ifa,jfav+1,1)+moyad1(ifa,jfav+1,1)
!
                        matuu(zzadma(kvois,adcm2,iadp2(jfav)))=&
                        matuu(zzadma(kvois,adcm2,iadp2(jfav)))&
                        +moyas2(ifa,jfav+1,1)+moyad2(ifa,jfav+1,1)
997                  continue
! -------------------------
! -------------------------
                else
! -------------------------
! -------------------------
! *******************************************************************
! EQUATION DE LA CONSERVATION DE LA MASSE POUR K
!                 (DERIVEES % VARIABLES DU CENTRE)
!                 RAJOUT DE LA PARTIE FICKIENNE
!                 | FVP1SK |
!                 | FVP2SK |
!                 | FAS1SK+ FAD1SK |
!                 | FAS2SK+ FAD2SK |
! *******************************************************************
                    matuu(zzadma(0,adcm1,iadp1k))= matuu(&
                        zzadma(0,adcm1,iadp1k))+ fvp1sk(1,ifa)
!
                    matuu(zzadma(0,adcm1,iadp2k))= matuu(&
                        zzadma(0,adcm1,iadp2k))+ fvp2sk(1,ifa)
!
!
                    matuu(zzadma(0,adcm2,iadp1k))= matuu(&
                        zzadma(0,adcm2,iadp1k))+fas1sk(1,ifa) +fad1sk(1,ifa)
!
                    matuu(zzadma(0,adcm2,iadp2k))= matuu(&
                        zzadma(0,adcm2,iadp2k))+ fas2sk(1,ifa) + fad2sk(1,ifa)
!
! *******************************************************************
! EQUATION DE LA CONSERVATION DE LA MASSE POUR K
!                 (DERIVEES % VARIABLES DE L ARETE)
!                 | FVP1SK |
!                 | FVP2SK |
!                 | FAS1SK+ FAD1SK |
!                 | FAS2SK+ FAD2SK |
! ******************************************************************
                    do 998 jfa = 1, nface
                        matuu(zzadma(0,adcm1,iadp1(jfa)))= matuu(&
                        zzadma(0,adcm1,iadp1(jfa))) +fvp1sk(jfa+1,ifa)
!
!
                        matuu(zzadma(0,adcm1,iadp2(jfa)))= matuu(&
                        zzadma(0,adcm1,iadp2(jfa))) +fvp2sk(jfa+1,ifa)
!
                        matuu(zzadma(0,adcm2,iadp1(jfa)))= matuu(&
                        zzadma(0,adcm2,iadp1(jfa))) +fas1sk(jfa+1,ifa)&
                        +fad1sk(jfa+1,ifa)
!
                        matuu(zzadma(0,adcm2,iadp2(jfa)))= matuu(&
                        zzadma(0,adcm2,iadp2(jfa))) +fas2sk(jfa+1,ifa)&
                        +fad2sk(jfa+1,ifa)
998                  continue
! *******************************************************************
! EQUATION DE LA CONTINUITE DES FLUX POUR K
!                 (DERIVEES % VARIABLES DU CENTRE)
!                 | FM1WS + FM1VPS + FVP1SK |
!                 | FM2WS + FM2VPS + FVP2SK |
!                 | FM1ASS + FM1ADS + FAS1SK + FAD1SK |
!                 | FM2ASS + FM2ADS + FAS2SK + FAD2SK |
! *****************************************************************
                    matuu(zzadma(0,adcf1(ifa),iadp1k))= matuu(&
                        zzadma(0,adcf1(ifa),iadp1k))+ fm1ws(1,ifa)+fm1vps(1,ifa)+&
                    fvp1sk(1,ifa)
!
!
                    matuu(zzadma(0,adcf1(ifa),iadp2k))= matuu(&
                        zzadma(0,adcf1(ifa),iadp2k))+ fm2ws(1,ifa)+fm2vps(1,ifa)+&
                    fvp2sk(1,ifa)
!
                    matuu(zzadma(0,adcf2(ifa),iadp1k))= matuu(&
                        zzadma(0,adcf2(ifa),iadp1k))+ fm1ass(1,ifa)+fm1ads(1,ifa)&
                    +fas1sk(1,ifa) +fad1sk(1,ifa)
!
                    matuu(zzadma(0,adcf2(ifa),iadp2k))= matuu(&
                        zzadma(0,adcf2(ifa),iadp2k))+ fm2ass(1,ifa)+fm2ads(1,ifa)&
                    +fas2sk(1,ifa) +fad2sk(1,ifa)
! *******************************************************************
! EQUATION DE LA CONTINUITE DES FLUX POUR K
!                 (DERIVEES % VARIABLES DE L ARETE)
!                 | FM1WS + FM1VPS + FVP1SK |
!                 | FM2WS + FM2VPS + FVP2SK |
!                 | FM1ASS + FM1ADS + FAS1SK + FAD1SK |
!                 | FM2ASS + FM2ADS + FAS2SK + FAD2SK |
! *******************************************************************
                    do 272 jfa = 1, nface
                        matuu(zzadma(0,adcf1(ifa),iadp1(jfa)))=&
                        matuu(zzadma(0,adcf1(ifa),iadp1(jfa)))&
                        + fm1ws(jfa+1,ifa)+ fm1vps(jfa+1,ifa)+fvp1sk(&
                        jfa+1,ifa)
!
                        matuu(zzadma(0,adcf1(ifa),iadp2(jfa)))=&
                        matuu(zzadma(0,adcf1(ifa),iadp2(jfa)))&
                        +fm2ws(jfa+1,ifa)+ fm2vps(jfa+1,ifa)+fvp2sk(&
                        jfa+1,ifa)
!
                        matuu(zzadma(0,adcf2(ifa),iadp1(jfa)))=&
                        matuu(zzadma(0,adcf2(ifa),iadp1(jfa)))&
                        +fm1ass(jfa+1,ifa)+fm1ads(jfa+1,ifa) +fas1sk(&
                        jfa+1,ifa)+fad1sk(jfa+1,ifa)
!
                        matuu(zzadma(0,adcf2(ifa),iadp2(jfa)))=&
                        matuu(zzadma(0,adcf2(ifa),iadp2(jfa)))&
                        +fm2ass(jfa+1,ifa)+fm2ads(jfa+1,ifa) +fas2sk(&
                        jfa+1,ifa)+fad2sk(jfa+1,ifa)
272                  continue
                endif
27          continue
        endif
    endif
! ======================================================================
! ======================================================================
end subroutine
