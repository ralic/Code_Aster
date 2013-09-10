subroutine op0045()
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!        MODE_ITER_SIMULT
!        RECHERCHE DE MODES PAR ITERATION SIMULTANEE EN SOUS-ESPACE
!        (LANCZOS, JACOBI OU IRA-ARPACK) OU METHODE DE TYPE QR (LAPACK)
!-----------------------------------------------------------------------
!        - POUR LE PROBLEME GENERALISE AUX VALEURS PROPRES :
!                         2
!                        L (M) Y  + (K) Y = 0
!
!          LES MATRICES (C) ET (M) SONT REELLES SYMETRIQUES
!          LA MATRICE (K) EST REELLE OU COMPLEXE SYMETRIQUE
!          LES VALEURS PROPRES ET DES VECTEURS PROPRES SONT REELS
!
!        - POUR LE PROBLEME QUADRATIQUE AUX VALEURS PROPRES :
!                         2
!                        L (M) Y  + L (C) Y + (K) Y = 0
!
!          LES MATRICES (C) ET (M) SONT REELLES SYMETRIQUES
!          LA MATRICE (K) EST REELLE OU COMPLEXE SYMETRIQUE
!          LES VALEURS PROPRES ET DES VECTEURS PROPRES SONT REELS OU
!          COMPLEXES CONJUGUEES OU NON
!-----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
    implicit none
! aslint: disable=W1501
!
! VARIABLES LOCALES
#include "aster_types.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterc/asmpi_split_comm.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/isnnem.h"
#include "asterc/r8depi.h"
#include "asterc/r8prem.h"
#include "asterc/r8vide.h"
#include "asterfort/ajlagr.h"
#include "asterfort/asmpi_barrier.h"
#include "asterfort/asmpi_comm_vect.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/cresol.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/freqom.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerazo.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtcmbl.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/omega2.h"
#include "asterfort/rectfc.h"
#include "asterfort/rectfr.h"
#include "asterfort/rscrsd.h"
#include "asterfort/sspace.h"
#include "asterfort/titre.h"
#include "asterfort/tldlg2.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utexcp.h"
#include "asterfort/uttcpu.h"
#include "asterfort/vecini.h"
#include "asterfort/vecink.h"
#include "asterfort/vecint.h"
#include "asterfort/vp2ini.h"
#include "asterfort/vp2trd.h"
#include "asterfort/vpbosc.h"
#include "asterfort/vpbost.h"
#include "asterfort/vpcntl.h"
#include "asterfort/vpcrea.h"
#include "asterfort/vpddl.h"
#include "asterfort/vpfopc.h"
#include "asterfort/vpfopr.h"
#include "asterfort/vpordi.h"
#include "asterfort/vpordo.h"
#include "asterfort/vppara.h"
#include "asterfort/vpqzla.h"
#include "asterfort/vpreco.h"
#include "asterfort/vpsorc.h"
#include "asterfort/vpsorn.h"
#include "asterfort/vptabl.h"
#include "asterfort/vpwecf.h"
#include "asterfort/vrrefe.h"
#include "asterfort/wkvect.h"
#include "asterfort/wp2ini.h"
#include "asterfort/wp2vec.h"
#include "asterfort/wp3vec.h"
#include "asterfort/wp4vec.h"
#include "asterfort/wp5vec.h"
#include "asterfort/wpfopc.h"
#include "asterfort/wpfopr.h"
#include "asterfort/wpsorc.h"
#include "asterfort/wpsorn.h"
    mpi_int :: mpicou, mpicow, mrang, mnbproc
    integer :: nbpari, nbparr, nbpark, nbpara, mxddl
    parameter    ( nbpari=8 , nbparr=16 , nbpark=3, nbpara=27 )
    parameter    ( mxddl=1 )
    integer :: iadx, imet, i, iady, ierx, iret, iadrb, iadz, ier1, ifm, itemax, iadrh, ibid, ierd
    integer :: ifreq
    integer :: lmat(3), lselec, lresid, ldsor, lamor, lbrss, lmasse, iauxk, lmtpsc, lresur, ltypri
    integer :: lworkd, laux, lraide, lsign, lvalpr, lworkl, ldiagr, lresui, lsurdr, lvec, lworkv
    integer :: iauxi, iauxr, lborvp, lmf, lprod, lresuk, ltypre, lxrig, kqrnr, lddl, lmatra, lonwl
    integer :: lmet, nbvec2, icoef, npiv2(2)
    integer :: npivot, nbvect, priram(8), maxitr, neqact, mfreq, idet(2), nborto, nfreq, nitv
    integer :: nparr, nbcine, neq, nitqrm, izero, nbrss, nitbat, niv, mxresf, nblagr, nperm
    integer :: nitjac, n1, nstoc, nconv, iexin, lworkr, laur, qrn, qrlwor, iqrn, lqrn
    integer :: qrar, qrai, qrba, qrvl, kqrn, qrn2, ilscal, irscal, lauc, laul, icscal, ivscal
    integer :: iiscal, jrefa, islvi, nprec, islvk, krefa, nnvalp, iarg, rang
    integer :: nbproc, typeco, vali(5), nbvecg, nfreqg, rangl, icom1, icom2, l, l1, l2, l3, indf
!     &             ,IETFIN,IETDEB,IETRAT,IETMAX
    real(kind=8) :: prorto, fmin, fmax, alpha, tolsor, det(2), rzero, omemin, omemax, omeshi, undf
    real(kind=8) :: omecor, fcorig, precdc, seuil, vpinf, precsh, tol, vpmax, prsudg, rbid, toldyn
    real(kind=8) :: effmin, effmax, eps, quapi2, rtest
!     &             RETFIN
    complex(kind=8) :: sigma, cbid, czero, dcmplx
    character(len=1) :: ctyp, appr, ktyp
    character(len=8) :: modes, knega, method, arret
    character(len=9) :: typevp
    character(len=14) :: matra, matrb, matrc
    character(len=16) :: modrig, typcon, nomcmd, optiof, optiov, typres, typeqz, k16bid
    character(len=19) :: masse0, masse, raide0, raide, amor, matpsc, matopa, vecrig, numedd
    character(len=19) :: solveu, tabmod
    character(len=24) :: cborvp, valk(5), nopara(nbpara), metres, kzero
    logical :: flage, lqz, lkr, lc, lns, lnsc, lnsk, lnsm, ltabmo, lpg, lcomod
    logical(kind=4), pointer, dimension(:) :: bwork

!     ------------------------------------------------------------------
    data cborvp / '&&OP0045.BORNE.VALP.USR ' /
    data  nopara /&
     &  'NUME_MODE'       , 'ITER_QR'         , 'ITER_BATHE'      ,&
     &  'ITER_ARNO'       , 'ITER_JACOBI'     , 'ITER_SEPARE'     ,&
     &  'ITER_AJUSTE'     , 'ITER_INVERSE'    ,&
     &  'NORME'           , 'METHODE'         , 'TYPE_MODE'       ,&
     &  'FREQ'            ,&
     &  'OMEGA2'          , 'AMOR_REDUIT'     , 'ERREUR'          ,&
     &  'MASS_GENE'       , 'RIGI_GENE'       , 'AMOR_GENE'       ,&
     &  'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,&
     &  'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,&
     &  'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ' /
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
!     -------  LECTURE DES DONNEES  ET PREMIERES VERIFICATION   --------
!     ------------------------------------------------------------------
!
    call jemarq()
!     --- POUR MESURER LE TEMPS CONSOMMEE DS LES ETAPES DE OP0045
!     --- IL FAUT AUSSI DECOMMENTER LES APPELS A SYSTEM_CLOCK (UTILES
!     --- MAIS REFUSES PAR L'AGLA).
!      LCPU=.TRUE.
!      LCPU=.FALSE.
!      IF (LCPU) CALL SYSTEM_CLOCK(IETDEB,IETRAT,IETMAX)
    nullify (bwork)
    undf=r8vide()
    indf=isnnem()
    rzero=0.d0
    izero=0
    kzero=' '
    czero=dcmplx(0.d0,0.d0)
    flage = .false.
    nconv = 0
    icoef = 0
    lkr = .true.
    lpg = .false.
    call infmaj()
    call infniv(ifm, niv)
    if (niv .eq. 2) then
        priram(1) = 2
        priram(2) = 2
        priram(3) = 2
        priram(4) = 2
        priram(5) = 0
        priram(6) = 0
        priram(7) = 0
        priram(8) = 2
    else
        call vecint(8, izero, priram)
    endif
    eps=1.d+4*r8prem()
    quapi2 = r8depi() * r8depi()
!
!
!     --- RECUPERATION DU RESULTAT  ---
    call getres(modes, typcon, nomcmd)
!
!     ------------------------------------------------------------------
!     --------------------MACRO_MODE_MECA PARALLELE (PART I)------------
!     ------------------------------------------------------------------
!     --- RECUPERATION ET TEST DE VALIDITE DES PARAMETRES DEDIES
!     ------------------------------------------------------------------
    icom1=-999
    icom2=-999
    call asmpi_comm('GET_WORLD', mpicow)
    call asmpi_comm('GET', mpicou)
!     --- ON EST CENSE FONCTIONNER EN COMM_WORLD
    if (mpicow .ne. mpicou) ASSERT(.false.)
    call asmpi_info(mpicow, mrang, mnbproc)
    rang = to_aster_int(mrang)
    nbproc = to_aster_int(mnbproc)
!
    call getvis('PARALLELISME_MACRO', 'TYPE_COM', 1, iarg, 1,&
                typeco, l1)
    call getvis('PARALLELISME_MACRO', 'IPARA1_COM', 1, iarg, 1,&
                icom1, l2)
    call getvis('PARALLELISME_MACRO', 'IPARA2_COM', 1, iarg, 1,&
                icom2, l3)
    valk(1)='TYPE_COM'
    valk(2)='IPARA1_COM'
    valk(3)='IPARA2_COM'
    valk(4)='RANG'
    valk(5)='NBPROC'
    vali(1)=typeco
    vali(2)=icom1
    vali(3)=icom2
    vali(4)=rang
    vali(5)=nbproc
    if (l1*l2*l3 .ne. 1) call u2mesg('F', 'APPELMPI_6', 3, valk, 3,&
                                     vali, 0, rbid)
!
    if ((&
        ((typeco.ne.1).and.(typeco.ne.-999)) .or.&
        ((icom1.ne.-999).and.((icom1.lt.1).or.(icom1.gt.nbproc))) .or.&
        ((icom2.ne.-999).and.((icom2.lt.1).or.(icom2.gt.nbproc))) .or. (icom1.gt.icom2) .or.&
        (nbproc.lt.1) .or. (rang.lt.0)&
        )) call u2mesg('F', 'APPELMPI_8', 5, valk, 5,&
                       vali, 0, rbid)
!
    if ((typeco.eq.1) .and. (nbproc.gt.1)) then
        lcomod=.true.
!       --- DECOMPOSE LE COM GLOBAL MPICOW EN COM LOCAL MPICOU
!       --- PLUS AFFECTATION DE CE NOUVEAU COM AFIN DE NE PAS PERTURBER
!       --- LA FACTO DE LA DEMI-BANDE
        call asmpi_split_comm(mpicow, to_mpi_int(icom1), to_mpi_int(0), 'ipara1', mpicou)
        if (mpicow .eq. mpicou) ASSERT(.false.)
        call asmpi_barrier()
        call asmpi_comm('SET', mpicou)
!       --- RANG DANS LE SOUS-COMM MPICOU LIE A CHAQUE OCCURENCE
!       --- MUMPS: RANGL
        call asmpi_info(comm=mpicou, rank=mrang)
        rangl = to_aster_int(mrang)
    else
        rangl=-9999
        mpicou=-9999
        lcomod=.false.
    endif
!     ------------------------------------------------------------------
!
!     --- TYPE DE CALCUL : DYNAMIQUE OU FLAMBEMENT OU GENERAL  ---
!     TYPE_RESU : 'DYNAMIQUE' OU 'MODE_FLAMB' OU 'GENERAL'
    call getvtx(' ', 'TYPE_RESU', 1, iarg, 1,&
                typres, ltypre)
!
!
!     --- CATALOGUE DE COMMANDE, DIFFERENT SELON LE TYPE_RESU
!     -> ON STOCKE DANS DES VARIABLES POUR EVITER DE FAIRE DES GETXXX
!     POUR CHAQUE TYPE_RESU.
!     POUR L'INSTANT TYPE_RESU='GENERAL' REVIENT A 'MODE_FLAMB'
!     SAUF LE NOM DES MATRICES
    if (typres .eq. 'DYNAMIQUE') then
        matra = 'MATR_RIGI'
        matrb = 'MATR_MASS'
        matrc = 'MATR_AMOR'
        typevp= 'FREQ'
    else if (typres .eq. 'MODE_FLAMB') then
        matra = 'MATR_RIGI'
        matrb = 'MATR_RIGI_GEOM'
        typevp= 'CHAR_CRIT'
    else if (typres .eq. 'GENERAL') then
        matra = 'MATR_A'
        matrb = 'MATR_B'
        matrc = 'MATR_C'
        typevp= 'CHAR_CRIT'
        typres= 'MODE_FLAMB'
    endif
!
!
!     --- OPTION DEMANDEE : BANDE OU PLUS_PETITE OU PLUS_GRANDE OU
!         CENTRE OU TOUT ---
!     OPTIOF : 'BANDE' OU 'CENTRE' OU 'PLUS_PETITE' OU 'TOUT'
    call getvtx('CALC_'//typevp, 'OPTION', 1, iarg, 1,&
                optiof, lmf)
!
!
!     --- RECUPERATION DES ARGUMENTS MATRICIELS ---
    if (optiof .eq. 'PLUS_GRANDE') then
        optiof = 'PLUS_PETITE'
        lpg = .true.
        call getvid(' ', matra, 1, iarg, 1,&
                    masse0, l)
        call getvid(' ', matrb, 1, iarg, 1,&
                    raide0, l)
        raide = 'MATR'
        masse = 'MATM'
!       - TRANSMISSION DES LAGRANGES DE LA RAIDEUR VERS LA MASSE
        call ajlagr(masse0, raide0, raide)
!       - SUPPRESSION DES LAGRANGES DE LA RAIDEUR
        call mtdefs(masse, masse0, 'V', ' ')
        call mtcmbl(1, 'R', 1.d0, masse0, masse,&
                    'LAGR', ' ', 'ELIM=')
    else
        call getvid(' ', matra, 1, iarg, 1,&
                    raide, l)
        call getvid(' ', matrb, 1, iarg, 1,&
                    masse, l)
    endif
    amor = ' '
    lamor = 0
    if (typres .ne. 'MODE_FLAMB') call getvid(' ', matrc, 1, iarg, 1,&
                                              amor, lamor)
    if (lamor .eq. 0) then
        lc=.false.
    else
        lc=.true.
    endif
!
!     --- TEST DU TYPE (COMPLEXE OU REELLE) DE LA MATRICE DE RAIDEUR ---
    call jelira(raide//'.VALM', 'TYPE', cval=ktyp)
    if (ktyp .eq. 'R') then
        lkr=.true.
    else if (ktyp.eq.'C') then
        lkr=.false.
    else
        ASSERT(.false.)
    endif
!
!     --- METHODE DE RESOLUTION CHOISIE ---
!     METHOD : 'TRI_DIAG','JACOBI' OU 'SORENSEN' OU 'QZ'
    call getvtx(' ', 'METHODE', 1, iarg, 1,&
                method, lmet)
    if (method(1:2) .eq. 'QZ') then
        lqz=.true.
    else
        lqz=.false.
    endif
!
!     --- DETECTION DES MODES DE CORPS RIGIDE ---
!     MODRIG : 'MODE_RIGIDE' OU 'SANS'
    call getvtx(' ', 'OPTION', 1, iarg, 1,&
                modrig, ltypri)
!
!     --- RECUPERATION DES ARGUMENTS CONCERNANT LE NOMBRE DE SHIFT ---
    call getvis('CALC_'//typevp, 'NMAX_ITER_SHIFT', 1, iarg, 1,&
                nbrss, lbrss)
!
!     --- RECUPERATION NFREQ ---
!     NFREQ : NOMBRE DE MODES DEMANDES
    call getvis('CALC_'//typevp, 'NMAX_'//typevp, 1, iarg, 1,&
                nfreq, l)
!
!     --- RECUPERATION PARAM ESPACE REDUIT ---
!     NBVECT, NBVEC2 : DIMENSION/COEF MULTIPLICATEUR DE L'ESPACE REDUIT
    nbvect = 0
    call getvis('CALC_'//typevp, 'DIM_SOUS_ESPACE', 1, iarg, 1,&
                nbvect, l)
    nbvec2 = 0
    call getvis('CALC_'//typevp, 'COEF_DIM_ESPACE', 1, iarg, 1,&
                nbvec2, l)
!
!     --- RECUPERATION DECALAGE POUR DETERMINER LE SHIFT ---
!     PRECSH : POUR PRE TRAITEMENT
    call getvr8('CALC_'//typevp, 'PREC_SHIFT', 1, iarg, 1,&
                precsh, l)
!     PRECDC : POUR POST TRAITEMENT
    call getvr8('VERI_MODE', 'PREC_SHIFT', 1, iarg, 1,&
                precdc, lmf)
!
!     --- RECUPERATION PARAM LANCZOS ---
    if (method .eq. 'TRI_DIAG') then
        call getvis(' ', 'NMAX_ITER_ORTHO', 1, iarg, 1,&
                    nborto, l)
        call getvr8(' ', 'PREC_ORTHO', 1, iarg, 1,&
                    prorto, l)
        call getvr8(' ', 'PREC_LANCZOS', 1, iarg, 1,&
                    prsudg, l)
        call getvis(' ', 'NMAX_ITER_QR', 1, iarg, 1,&
                    nitv, l)
!
!     --- RECUPERATION PARAM JACOBI ---
    else if (method .eq. 'JACOBI') then
        call getvis(' ', 'NMAX_ITER_BATHE ', 1, iarg, 1,&
                    itemax, l)
        call getvr8(' ', 'PREC_BATHE', 1, iarg, 1,&
                    tol, l)
        call getvis(' ', 'NMAX_ITER_JACOBI', 1, iarg, 1,&
                    nperm, l)
        call getvr8(' ', 'PREC_JACOBI', 1, iarg, 1,&
                    toldyn, l)
!
!     --- RECUPERATION PARAM SORENSEN ---
    else if (method .eq. 'SORENSEN') then
        call getvr8(' ', 'PREC_SOREN', 1, iarg, 1,&
                    tolsor, l)
        call getvis(' ', 'NMAX_ITER_SOREN', 1, iarg, 1,&
                    maxitr, l)
        call getvr8(' ', 'PARA_ORTHO_SOREN', 1, iarg, 1,&
                    alpha, l)
        if ((alpha.lt.1.2d0*eps) .or. (alpha.gt.0.83d0-eps)) call u2mess('E', 'ALGELINE2_64')
!     --- RECUPERATION PARAM QZ ---
    else if (lqz) then
        call getvtx(' ', 'TYPE_QZ', 1, iarg, 1,&
                    typeqz, l)
    endif
!
!     --- RECUPERATION PARAM MODES RIGIDES ---
!     FCORIG : SEUIL DE FREQUENCE CORPS RIGIDE
    call getvr8('CALC_'//typevp, 'SEUIL_'//typevp, 1, iarg, 1,&
                fcorig, l)
    omecor = rzero
    if (typres .eq. 'DYNAMIQUE') omecor = omega2(fcorig)
!
!     --- LISTES DES FREQUENCES/CHARGES CRITIQUES ---
    nnvalp = 0
    call getvr8('CALC_'//typevp, typevp, 1, iarg, 0,&
                rbid, nnvalp)
    if (nnvalp .lt. 0) then
        nnvalp = -nnvalp
        call wkvect(cborvp, ' V V R', nnvalp, lborvp)
        call getvr8('CALC_'//typevp, typevp, 1, iarg, nnvalp,&
                    zr(lborvp), l)
    else
        call wkvect(cborvp, ' V V R', 1, lborvp)
        zr(lborvp)=rzero
    endif
!
!     --- APPROCHE (CAS AVEC AMORTISSEMENT) ---
!     UTILISE SI LMAMOR.NE.0 ET SI METHODE.NE.QZ
    call getvtx('CALC_'//typevp, 'APPROCHE', 1, iarg, 1,&
                appr, ibid)
!
!     ------------------------------------------------------------------
!     --------------------  REGLES D'EXCLUSION   -----------------------
!     ------------------------------------------------------------------
!     --- MODES RIGIDES---
    if ((modrig.eq.'MODE_RIGIDE') .and. (method.ne.'TRI_DIAG')) call u2mess('F', 'ALGELINE2_65')
!
    if (lc) then
        valk(1) = matra
        valk(2) = matrc
        if (lpg) call u2mess('F', 'ALGELINE5_82')
        if (optiof .eq. 'BANDE') call u2mesk('F', 'ALGELINE2_66', 2, valk)
        if (((appr.eq.'I').or.(appr.eq.'C')) .and. (zr(lborvp).eq.0.d0)) then
            call u2mess('F', 'ALGELINE2_67')
        endif
        if (modrig .eq. 'MODE_RIGIDE') call u2mesk('F', 'ALGELINE2_68', 2, valk)
        if ((method.eq.'SORENSEN') .and. (zr(lborvp).eq.0.d0)) call u2mess('F', 'ALGELINE2_71')
        if (method(1:6) .eq. 'JACOBI') call u2mesk('F', 'ALGELINE5_64', 1, matrc)
    endif
!
!     --- MATRICE K COMPLEXE ---
    if (.not.lkr) then
        valk(1) = matra
        valk(2) = matrc
        if (lpg) call u2mess('F', 'ALGELINE5_82')
        if ((method.ne.'SORENSEN') .and. (.not.lqz)) call u2mess('F', 'ALGELINE2_69')
        if (optiof .eq. 'BANDE') call u2mesk('F', 'ALGELINE2_66', 2, valk)
        if (zr(lborvp) .eq. 0.d0) call u2mess('F', 'ALGELINE2_70')
        if (modrig .eq. 'MODE_RIGIDE') call u2mesk('F', 'ALGELINE2_68', 2, valk)
        if (typres .eq. 'MODE_FLAMB') call u2mesk('F', 'ALGELINE2_46', 1, matra)
    endif
!
!     --- METHODE QZ ---
    if (lqz) then
        if ((typeqz(1:5).eq.'QZ_QR') .and.&
            ((typres(1:10) .eq.'FLAMBEMENT') .or.lc.or.(.not.lkr))) then
            valk(1) = matra
            valk(2) = matrc
            call u2mesk('F', 'ALGELINE5_60', 2, valk)
        endif
    endif
    if ((optiof.eq.'TOUT') .and. (.not.lqz)) call u2mesk('F', 'ALGELINE5_65', 1, 'CALC_'//typevp)
!
!     --- COMPATIBILITE DES MODES (DONNEES ALTEREES) ---
    call exisd('MATR_ASSE', raide, ibid)
    if (ibid .ne. 0) then
        call dismoi('F', 'NOM_NUME_DDL', raide, 'MATR_ASSE', ibid,&
                    numedd, iret)
    else
        numedd=' '
    endif
!
    if (lpg) then
        call vpcrea(0, modes, raide0, amor, masse0,&
                    numedd, ier1)
    else
        call vpcrea(0, modes, masse, amor, raide,&
                    numedd, ier1)
    endif
!
!
!     --- VERIFICATION DES "REFE" ---
    call vrrefe(masse, raide, iret)
    if (iret .gt. 0) then
        valk(1) = raide
        valk(2) = masse
        call u2mesk('F', 'ALGELINE2_58', 2, valk)
    endif
    if (lc) then
        call vrrefe(raide, amor, iret)
        if (iret .gt. 0) then
            valk(1) = raide
            valk(2) = amor
            call u2mesk('F', 'ALGELINE2_58', 2, valk)
        endif
    endif
!
!     --- DESCRIPTEUR DES MATRICES ---
    lnsk=.false.
    lnsm=.false.
    lnsc=.false.
    call mtdscr(masse)
    call jeveuo(masse(1:19)//'.&INT', 'E', lmasse)
    if (zi(lmasse+4) .eq. 0) lnsm=.true.
    call mtdscr(raide)
    call jeveuo(raide(1:19)//'.&INT', 'E', lraide)
    if (zi(lraide+4) .eq. 0) lnsk=.true.
    if (lc) then
        call mtdscr(amor)
        call jeveuo(amor(1:19)//'.&INT', 'E', lamor)
        if (zi(lamor+4) .eq. 0) lnsc=.true.
    else
        lamor=0
    endif
!
!     --- MATRICE K ET/OU M ET OU C NON SYMETRIQUE(S)
    if (lnsc .or. lnsk .or. lnsm) then
        lns=.true.
        if (lpg) call u2mess('F', 'ALGELINE5_82')
        if ((.not.lqz) .and. (method.ne.'SORENSEN')) call u2mess('F', 'ALGELINE5_69')
        if (.not.lkr) call u2mesk('F', 'ALGELINE5_70', 1, matra)
        if (optiof .eq. 'BANDE') call u2mess('F', 'ALGELINE4_39')
        if (modrig .eq. 'MODE_RIGIDE') call u2mess('F', 'ALGELINE4_40')
        if (typres .eq. 'MODE_FLAMB') call u2mess('F', 'ALGELINE4_41')
    else
        lns=.false.
    endif
!
!     ------------------------------------------------------------------
!     ----------- DDL : LAGRANGE, BLOQUE PAR AFFE_CHAR_CINE  -----------
!     ------------------------------------------------------------------
!
!     --- NOMBRE D'EQUATIONS ---
    neq = zi(lraide+2)
    call wkvect('&&OP0045.POSITION.DDL', 'V V I', neq*mxddl, lddl)
    call wkvect('&&OP0045.DDL.BLOQ.CINE', 'V V I', neq, lprod)
    call vpddl(raide, masse, neq, nblagr, nbcine,&
               neqact, zi(lddl), zi(lprod), ierd)
    if (ierd .ne. 0) goto 888
!
!       -- TRAITEMENTS PARTICULIERS PROPRES A QZ
    if (lqz) then
        if (optiof(1:4) .eq. 'TOUT') nfreq = neqact
        if ((typeqz(1:5).eq.'QZ_QR') .and. ((nblagr.ne.0).or.lns)) then
            valk(1) = matra
            valk(2) = matrc
            call u2mesk('F', 'ALGELINE5_60', 2, valk)
        endif
    endif
!
!     ------------------------------------------------------------------
!     ----------- LECTURE/TRAITEMENT SD SOLVEUR LINEAIRE  -----------
!     ------------------------------------------------------------------
!     -- LECTURE DES PARAMETRES SOLVEURS LINEAIRES ET CREATION DE
!        LA SD SOLVEUR ASSOCIEE. CETTE SD SOLVEUR EST LOCALE A L'OPERA
!        TEUR. POUR CE CALCUL, C'EST ELLE QUI EST UTILISEE POUR PARAME
!        TREE LE SOLVEUR LINEAIRE, ET NON PAS LA SD SOLVEUR CREE PAR LA
!        CMDE ECLATEE NUME_DDL LORS DE LA CONSTITUTION DES MATRICES.
    call jeveuo(raide//'.REFA', 'L', jrefa)
    solveu='&&OP0045.SOLVEUR'
    call cresol(solveu)
    call jeveuo(solveu//'.SLVK', 'L', islvk)
    call jeveuo(solveu//'.SLVI', 'L', islvi)
    nprec=zi(islvi)
    metres=zk24(islvk)
    if ((metres(1:4).ne.'LDLT') .and. (metres(1:10).ne.'MULT_FRONT') .and.&
        (metres(1:5).ne.'MUMPS')) call u2mess('F', 'ALGELINE5_71')
!
    nprec=zi(islvi)
    metres=zk24(islvk)
!
!     ------------------------------------------------------------------
!     ------------  CONSTRUCTION DE LA MATRICE SHIFTEE   ---------------
!     ------------------------------------------------------------------
!
!     --- VERIFICATION DES FREQUENCES MIN ET MAX, PASSAGE EN OMEGA2
    if (typres .eq. 'DYNAMIQUE') then
        fmin = rzero
        fmax = rzero
        if (nnvalp .gt. 0) fmin = zr(lborvp)
        if (nnvalp .gt. 1) fmax = zr(lborvp+1)
        if (lc .and. (fmin.lt.0.d0)) then
            fmin = -fmin
            if (niv .ge. 1) call u2mess('I', 'ALGELINE6_10')
        endif
        omemin = omega2(fmin)
        omemax = omega2(fmax)
    else
        omemin = rzero
        omemax = rzero
        if (nnvalp .gt. 0) omemin = zr(lborvp)
        if (nnvalp .gt. 1) omemax = zr(lborvp+1)
        fmin=omemin
        fmax=omemax
    endif
!
!     --- ARRET SI PAS DE FREQUENCE DANS L'INTERVALLE DONNE  ---
    call getvtx(' ', 'STOP_BANDE_VIDE', 1, iarg, 1,&
                arret, n1)
!
!     ------------------------------------------------------------------
!     ----  DETETECTION DES MODES DE CORPS RIGIDE                 ------
!     ------------------------------------------------------------------
!
    lxrig = 0
    nstoc = 0
    if (modrig .eq. 'MODE_RIGIDE') then
        vecrig = '&&OP0045.MODE.RIGID'
        call uttcpu('CPU.RESO.1', 'DEBUT', ' ')
        call uttcpu('CPU.RESO.4', 'DEBUT', ' ')
        call tldlg2(lraide, nprec, nstoc, vecrig, ' ',&
                    ' ')
        call uttcpu('CPU.RESO.1', 'FIN', ' ')
        call uttcpu('CPU.RESO.4', 'FIN', ' ')
        if (nstoc .ne. 0) call jeveuo(vecrig, 'E', lxrig)
    endif
!      IF (LCPU) THEN
!        CALL SYSTEM_CLOCK(IETFIN)
!        RETFIN=REAL(IETFIN-IETDEB)/REAL(IETRAT)
!        WRITE(IFM,*)'<OP0045> COUT LECTURE PARAMETRES: ',RETFIN
!        CALL SYSTEM_CLOCK(IETDEB,IETRAT,IETMAX)
!      ENDIF
!     ------------------------------------------------------------------
!     ----  CREATION DE LA MATRICE DYNAMIQUE ET DE SA FACTORISEE  ------
!     ------------------------------------------------------------------
    matpsc = '&&OP0045.DYN_FAC_R '
    matopa = '&&OP0045.DYN_FAC_C '
    omeshi=rzero
    sigma=czero
    npivot=0
    lmtpsc= 0
    lmatra=0
!     --- SI AVEC L'OPTION 'BANDE', ON FOURNIT UNE TABLE, CONTROLE ET
!     --- LECTURE DES PARAMETRES DE LA TABLE (AU FORMAT INFO_MODE)
!     --- GAIN DE TEMPS, ON NE RECALCULE PAS TOUT DS LE VPFOPR SUIVANT
    ltabmo=.false.
    effmin=-9999.d0
    effmax=-9999.d0
    if (optiof .eq. 'BANDE') then
        call getvid('CALC_'//typevp, 'TABLE_'//typevp, 1, ibid, 1,&
                    tabmod, l)
        if (l .eq. 1) then
            ltabmo=.true.
            call vptabl(tabmod, typevp, fmin, fmax, precdc,&
                        nfreq, effmin, effmax)
!     --- RECUPERATION DES BORNES EFFECTIVES ET CHANGEMENT DES BORNES
!     --- DE LA BANDE SI NECESSAIRE
            rtest=abs(fmax-effmax)+abs(fmin-effmin)
            if (rtest .gt. eps) then
                valk(1)=tabmod
                call u2mesk('A', 'ALGELINE2_26', 1, valk)
                fmin=effmin
                fmax=effmax
                if (typres .eq. 'DYNAMIQUE') then
                    omemin=omega2(fmin)
                    omemax=omega2(fmax)
                else
                    omemin=fmin
                    omemax=fmax
                endif
            endif
        endif
    endif
!
! --- DETERMINATION D'INFO POUR LE TEST DE STURM ET LES POSITIONS
!     MODALES + CONSTRUCTION DE LA MATRICE DYNAMIQUE/ SA FACTORISEE
!     (STOCKEE DS MATOPA). CAS PARTICULIER DU QEP APPROCHE REEL, POUR
!     LEQUEL ON UTILISE AUSSI UNE MATRICE MATPSC DISTINCTE DE MATOPA.
!     DANS TOUS LES AUTRES CAS, MATPSC=MATOPA.
! --- AVEC QZ, UTILE QUE POUR GENERALISE REEL SYMETRIQUE
!     DANS LES AUTRES CAS (VPFOPC,WPFOPR,WPFOPC) ON SORT DES LE CALCUL
!     DU SHIFT SIGMA
    if (.not.lc) then
!     --- PROBLEME GENERALISE REEL SYMETRIQUE ---
        if (lkr .and. (.not.lns)) then
            call mtdefs(matopa, raide, 'V', 'R')
            call mtdscr(matopa)
            call jeveuo(matopa(1:19)//'.&INT', 'E', lmatra)
!     --- POUR EVITER DE REFAIRE LE TEST DE STURM DE PRETTRAITEMENT AVEC
!     --- L'OPTION 'BANDE'
            if (ltabmo) then
                optiov='BANDEA'
            else
                optiov=optiof
            endif
            call vpfopr(optiov, typres, lmasse, lraide, lmatra,&
                        omemin, omemax, omeshi, nfreq, npiv2,&
                        omecor, precsh, nbrss, nblagr, solveu,&
                        det, idet)
            npivot=npiv2(1)
            if (nfreq .le. 0) then
                if (arret(1:3) .eq. 'OUI') then
                    call utexcp(24, 'MODAL_1')
                else
                    nfreq = 1
                    call rscrsd('G', modes, typcon, nfreq)
                    goto 999
                endif
            endif
            lmtpsc=lmatra
            matpsc=matopa
        else
!     --- PROBLEME GENERALISE COMPLEXE OU REEL NON SYM ---
            call vpfopc(lmasse, lraide, fmin, sigma, matopa,&
                        raide, lqz, solveu)
            if (.not.lqz) call jeveuo(matopa(1:19)//'.&INT', 'L', lmatra)
        endif
!
    else
!
!     --- PROBLEME QUADRATIQUE REEL SYM OU NON SYM---
        if (lkr) then
            call wpfopr(lmasse, lamor, lraide, appr, fmin,&
                        sigma, matopa, matpsc, raide, lqz,&
                        solveu)
            if (.not.lqz) then
                call jeveuo(matopa(1:19)//'.&INT', 'L', lmatra)
                call jeexin(matpsc(1:19)//'.&INT', iexin)
                if (iexin .ne. 0) call jeveuo(matpsc(1:19)//'.&INT', 'L', lmtpsc)
            endif
        else
!     --- PROBLEME QUADRATIQUE COMPLEXE SYM ---
            call wpfopc(lmasse, lamor, lraide, fmin, sigma,&
                        matopa, raide, lqz, solveu)
            if (.not.lqz) call jeveuo(matopa(1:19)//'.&INT', 'L', lmatra)
        endif
    endif
!
!      IF (LCPU) THEN
!        CALL SYSTEM_CLOCK(IETFIN)
!        RETFIN=REAL(IETFIN-IETDEB)/REAL(IETRAT)
!        WRITE(IFM,*)'<OP0045> COUT MATRICE DYNAMIQUE + FACTO: ',RETFIN
!        CALL SYSTEM_CLOCK(IETDEB,IETRAT,IETMAX)
!      ENDIF
!    --- ON BLINDE LES STRUCTURES DE DONNEES DE TYPE MATR_ASSE
!    --- ON NE MANIPULE PAR LA SUITE QUE LEUR DESCRIPTEUR
!    --- MATOPA --> LMATRA ET MATPSC --> LMTPSC
!    --- ON TOUCHE A LEUR .REFA POUR DETRUIRE CORRECTEMENT LES
!    --- EVENTUELLES OCCURENCES EXTERNES
    if (lmatra .eq. 0) matopa=' '
    if (lmtpsc .eq. 0) matpsc=' '
    if (lmatra .ne. 0) then
        call jeexin(matopa(1:19)//'.REFA', iret)
        if (iret .ne. 0) then
            call jeveuo(matopa(1:19)//'.REFA', 'E', krefa)
            zk24(krefa-1+7)=solveu
        endif
    endif
    if ((lmtpsc.ne.0) .and. (lmtpsc.ne.lmatra)) then
        call jeexin(matpsc(1:19)//'.REFA', iret)
        if (iret .ne. 0) then
            call jeveuo(matpsc(1:19)//'.REFA', 'E', krefa)
            zk24(krefa-1+7)=solveu
        endif
    endif
!
!     ------------------------------------------------------------------
!     ----  CORRECTION EVENTUELLE DU NBRE DE MODES DEMANDES NFREQ ------
!     ----  DETERMINATION DE LA DIMENSION DU SOUS ESPACE NBVECT   ------
!     ------------------------------------------------------------------
!
    if (niv .ge. 1) call u2mesi('I', 'ALGELINE6_11', 1, nfreq)
!
!     --- CORRECTION DU NOMBRE DE FREQUENCES DEMANDEES
    if (nfreq .gt. neqact) then
        nfreq = neqact
        if (niv .ge. 1) call u2mesi('I', 'ALGELINE6_12', 1, nfreq)
    endif
!
!     --- DETERMINATION DE NBVECT (DIMENSION DU SOUS ESPACE) ---
    if (.not.lqz) then
        if (niv .ge. 1) call u2mesi('I', 'ALGELINE6_13', 1, nbvect)
        if (nbvec2 .ne. 0) then
            icoef = nbvec2
        else
            if (method .eq. 'JACOBI') then
                icoef = 2
            else if (method.eq.'TRI_DIAG') then
                icoef = 4
            else if (method.eq.'SORENSEN') then
                icoef = 2
            endif
        endif
        if (nbvect .lt. nfreq) then
            if (method .eq. 'JACOBI') then
                nbvect = min(min(7+nfreq,icoef*nfreq),neqact)
            else if (method.eq.'TRI_DIAG') then
                nbvect = min(max(7+nfreq,icoef*nfreq),neqact)
            else if (method.eq.'SORENSEN') then
                nbvect = min(max(3+nfreq,icoef*nfreq),neqact)
            endif
            if (niv .ge. 1) call u2mesi('I', 'ALGELINE6_14', 1, nbvect)
        else
            if (nbvect .gt. neqact) then
                nbvect = neqact
                if (niv .ge. 1) call u2mesi('I', 'ALGELINE6_15', 1, nbvect)
            endif
        endif
    endif
!
!     --- TRAITEMENT SPECIFIQUE A SORENSEN ---
    if ((method.eq.'SORENSEN') .and. (nbvect-nfreq.le.2)) then
        if (nfreq .gt. (neqact+2)) then
!C        DIMINUTION FORCEE DE NFREQ
            nfreq=neqact-2
        endif
!C      AUGMENTATION FORCEE DE NBVECT
        nbvect = nfreq + 2
    endif
!
!     --- TRAITEMENT SPECIFIQUE A QZ ---
!     AVEC QZ ON A PAS D'ESPACE DE PROJECTION, IL FAUT DONC AFFECTER
!     NBVECT EN DUR
    if (lqz) nbvect=neq
!
!     --- CORRECTION DE NBVECT DANS LE CAS QUADRATIQUE
    if (lc) then
        nbvect = 2*nbvect
        nfreq = 2*nfreq
        call u2mess('I', 'ALGELINE2_75')
    endif
!
!
!     ------------------------------------------------------------------
!     --------------  ALLOCATION DES ZONES DE TRAVAIL   ----------------
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!     --------------------MACRO_MODE_MECA PARALLELE (PART II)-----------
!     ------------------------------------------------------------------
!     --- REDIMENSIONNEMENT DES BUFFERS DE COM
!     ------------------------------------------------------------------
    nbvecg=-9999
    nfreqg=-9999
    if (lcomod) then
!       --- ON REMET LE COM WORLD POUR COMMUNIQUER NBVECT/NBFREQ
        call asmpi_comm('SET', mpicow)
        call asmpi_barrier()
!       --- EST-ON LE PROCESSUS MAITRE DU COM LOCAL: RANGL=0 ?
!       --- SI OUI, ON ENVOI LES BONNES VALEURS DE NBVECT/NFREQ
!       --- SUR LE COM GLOBAL MPICOW, SINON ON RENVOI ZERO POUR NE PAS
!       --- COMPTER PLUSIEURS FOIS L'INFO.
        if (rangl .eq. 0) then
            nbvecg=nbvect
            nfreqg=nfreq
        else
            nbvecg=0
            nfreqg=0
        endif
        call asmpi_comm_vect('MPI_SUM', 'I', sci=nbvecg)
        call asmpi_comm_vect('MPI_SUM', 'I', sci=nfreqg)
!         --- ON REMET LE COM LOCAL POUR LES FACTO ET SOLVES A SUIVRE
        call asmpi_barrier()
        call asmpi_comm('SET', mpicou)
    endif
!     ------------------------------------------------------------------
!
!     --- CREATION ET INITIALISATION DES SD
    if (lcomod) then
        mxresf = nfreqg
        iauxr=nbparr*nbvecg
        iauxi=nbpari*nbvecg
        iauxk=nbpark*nbvecg
    else
        mxresf = nfreq
        iauxr=nbparr*nbvect
        iauxi=nbpari*nbvect
        iauxk=nbpark*nbvect
    endif
    call wkvect('&&OP0045.RESU_I', 'V V I', iauxi, lresui)
    call wkvect('&&OP0045.RESU_', 'V V R', iauxr, lresur)
    call wkvect('&&OP0045.RESU_K', 'V V K24', iauxk, lresuk)
    call vecint(iauxi, indf, zi(lresui))
    call vecini(iauxr, undf, zr(lresur))
    call vecink(iauxk, kzero, zk24(lresuk))
!
!     --- CAS GENERALISE REEL ---
    if (lkr .and. (.not.lc) .and. (.not.lns)) then
        if (lcomod) then
            call wkvect('&&OP0045.VECTEUR_PROPRE', 'V V R', neq*nbvecg, lvec)
        else
            call wkvect('&&OP0045.VECTEUR_PROPRE', 'V V R', neq*nbvect, lvec)
        endif
    else
!     --- CAS GENERALISE COMPLEXE OU QUADRATIQUE REEL ET COMPLEXE ---
        if (lcomod) ASSERT(.false.)
        call wkvect('&&OP0045.VECTEUR_PROPRE', 'V V C', neq*nbvect, lvec)
    endif
!
    if (method .eq. 'TRI_DIAG') then
        call wkvect('&&OP0045.MAT.DIAG', 'V V R', nbvect, ldiagr)
        call wkvect('&&OP0045.MAT.SUR.DIAG', 'V V R', nbvect, lsurdr)
        call wkvect('&&OP0045.SIGNES', 'V V R', nbvect, lsign)
        if (.not.lc) then
            call wkvect('&&OP0045.MAT.MOD.REDUITE', 'V V R', nbvect* nbvect, iadz)
        else
            call wkvect('&&OP0045.VECT.LANCZOS', 'V V R', neq*nbvect, iadx)
            call wkvect('&&OP0045.VECTY   ', 'V V R', neq*nbvect, iady)
            call wkvect('&&OP0045.MAT.MOD.REDUITE', 'V V R', 2*nbvect* nbvect, iadz)
            call wkvect('&&OP0045.VECT_DEP.H', 'V V R', neq, iadrh)
            call wkvect('&&OP0045.VECT_DEP.B', 'V V R', neq, iadrb)
        endif
    else if (method .eq. 'JACOBI') then
        call wkvect('&&OP0045.VALPRO', 'V V R', nbvect, lvalpr)
    else if (lqz) then
        qrn = nbvect
        qrlwor=8*qrn
        qrn2=qrn*qrn
        if (typeqz(1:7) .eq. 'QZ_EQUI') then
            call wkvect('&&OP0045.QRLSCALE.WORK', 'V V R', qrn, ilscal)
            call wkvect('&&OP0045.QRRSCALE.WORK', 'V V R', qrn, irscal)
            call wkvect('&&OP0045.QRRCONDE.WORK', 'V V R', qrn, icscal)
            call wkvect('&&OP0045.QRRCONDV.WORK', 'V V R', qrn, ivscal)
            call wkvect('&&OP0045.QRI.WORK', 'V V S', qrn+6, iiscal)
            allocate(bwork(qrn))
        endif
        if (lkr .and. (.not.lc) .and. (.not.lns)) then
            call wkvect('&&OP0045.QZ.VALPRO', 'V V R', qrn, lvalpr)
            call wkvect('&&OP0045.QZ.MATRICEK', 'V V R', qrn2, iqrn)
            call wkvect('&&OP0045.QZ.MATRICEM', 'V V R', qrn2, lqrn)
            call wkvect('&&OP0045.QZ.ALPHAR', 'V V R', qrn, qrar)
            call wkvect('&&OP0045.QZ.ALPHAI', 'V V R', qrn, qrai)
            call wkvect('&&OP0045.QZ.BETA', 'V V R', qrn, qrba)
            call wkvect('&&OP0045.QZ.VL', 'V V R', qrn, qrvl)
            call wkvect('&&OP0045.QZ.WORK', 'V V R', qrlwor, kqrn)
        else
            if (lc) call wkvect('&&OP0045.VECT.AUC', 'V V C', qrn2, lauc)
            call wkvect('&&OP0045.QZ.VALPRO', 'V V C', qrn, lvalpr)
            call wkvect('&&OP0045.QZ.MATRICEK', 'V V C', qrn2, iqrn)
            call wkvect('&&OP0045.QZ.MATRICEM', 'V V C', qrn2, lqrn)
            call wkvect('&&OP0045.QZ.ALPHA', 'V V C', qrn, qrar)
            call wkvect('&&OP0045.QZ.BETA', 'V V C', qrn, qrba)
            call wkvect('&&OP0045.QZ.VL', 'V V C', qrn, qrvl)
            call wkvect('&&OP0045.QZ.WORK', 'V V C', qrlwor, kqrn)
            call wkvect('&&OP0045.QZ.WORKR', 'V V R', qrlwor, kqrnr)
        endif
        call jerazo('&&OP0045.QZ.MATRICEK', qrn2, 1)
        call jerazo('&&OP0045.QZ.MATRICEM', qrn2, 1)
    else if (method .eq. 'SORENSEN') then
        lonwl = 3*nbvect**2+6*nbvect
        call wkvect('&&OP0045.SELECT', 'V V L', nbvect, lselec)
!     --- CAS REEL GENERALISE ---
        if (lkr .and. (.not.lc) .and. (.not.lns)) then
            call wkvect('&&OP0045.RESID', 'V V R', neq, lresid)
            call wkvect('&&OP0045.VECT.WORKD', 'V V R', 3*neq, lworkd)
            call wkvect('&&OP0045.VECT.WORKL', 'V V R', lonwl, lworkl)
            call wkvect('&&OP0045.VECT.WORKV', 'V V R', 3*nbvect, lworkv)
            call wkvect('&&OP0045.VAL.PRO', 'V V R', 2*(nfreq+1), ldsor)
            call wkvect('&&OP0045.VECT.AUX', 'V V R', neq, laux)
!     --- CAS COMPLEXE GENERALISE ---
        else if ((.not.lc).and.(lns.or..not.lkr)) then
            call wkvect('&&OP0045.RESID', 'V V C', neq, lresid)
            call wkvect('&&OP0045.VECT.WORKD', 'V V C', 3*neq, lworkd)
            call wkvect('&&OP0045.VECT.WORKL', 'V V C', lonwl, lworkl)
            call wkvect('&&OP0045.VECT.WORKV', 'V V C', 3*nbvect, lworkv)
            call wkvect('&&OP0045.VAL.PRO', 'V V C', (nfreq+1), ldsor)
            call wkvect('&&OP0045.VECT.AUX', 'V V C', neq, laux)
            call wkvect('&&OP0045.VECT.AUR', 'V V R', nbvect, lworkr)
!     --- CAS REEL QUADRATIQUE APPROCHE REELLE OU IMAGINAIRE ---
        else if ((lkr.and.lc).and.(appr.ne.'C')) then
            call wkvect('&&OP0045.RESID', 'V V R', 2*neq, lresid)
            call wkvect('&&OP0045.VECT.WORKD', 'V V R', 6*neq, lworkd)
            call wkvect('&&OP0045.VECT.AUX', 'V V R', 2*neq, laux)
            call wkvect('&&OP0045.VECT.AUC', 'V V C', 2*neq*(nbvect+1), lauc)
            call wkvect('&&OP0045.VECT.AUR', 'V V R', 2*neq*(nbvect+1), laur)
            call wkvect('&&OP0045.VECT.AUL', 'V V C', neq*(nbvect+1), laul)
            call wkvect('&&OP0045.VAL.PR', 'V V R', nbvect+1, ldiagr)
            call wkvect('&&OP0045.VAL.PI', 'V V R', nbvect+1, lsurdr)
            call wkvect('&&OP0045.VAL.PRO', 'V V R', 2*(nfreq+1), ldsor)
            call wkvect('&&OP0045.VECT.WORKL', 'V V R', lonwl, lworkl)
            call wkvect('&&OP0045.VECT.WORKV', 'V V R', 3*nbvect, lworkv)
!     --- CAS REEL QUADRATIQUE APPROCHE COMPLEXE ---
        else if ((lkr.and.lc).and.(appr.eq.'C')) then
            call wkvect('&&OP0045.RESID', 'V V C', 2*neq, lresid)
            call wkvect('&&OP0045.VECT.WORKD', 'V V C', 6*neq, lworkd)
            call wkvect('&&OP0045.VECT.AUX', 'V V C', 2*neq, laux)
            call wkvect('&&OP0045.VECT.AUC', 'V V C', 2*neq*(nbvect+1), lauc)
            call wkvect('&&OP0045.VECT.AUR', 'V V R', 2*neq*(nbvect+1), laur)
            call wkvect('&&OP0045.VECT.WORKL', 'V V C', lonwl, lworkl)
            call wkvect('&&OP0045.VECT.WORKV', 'V V C', 3*nbvect, lworkv)
            call wkvect('&&OP0045.VAL.PRO', 'V V C', 2*(nfreq+1), ldsor)
!     --- CAS COMPLEXE QUADRATIQUE  ---
        else if ((.not.lkr).and.lc) then
            call wkvect('&&OP0045.RESID', 'V V C', 2*neq, lresid)
            call wkvect('&&OP0045.VECT.WORKD', 'V V C', 6*neq, lworkd)
            call wkvect('&&OP0045.VECT.WORKL', 'V V C', lonwl, lworkl)
            call wkvect('&&OP0045.VECT.WORKV', 'V V C', 3*nbvect, lworkv)
            call wkvect('&&OP0045.VAL.PRO', 'V V C', 2*(nfreq+1), ldsor)
            call wkvect('&&OP0045.VECT.AUX', 'V V C', 2*neq, laux)
            call wkvect('&&OP0045.VECT.AUC', 'V V C', 2*neq*(nbvect+1), lauc)
            call wkvect('&&OP0045.VECT.AUR', 'V V R', 2*neq*(nbvect+1), laur)
        else
! ---- OPTION ILLICITE
            ASSERT(.false.)
        endif
    endif
!
!
! ---- TEST POUR VALIDER LE QUADRATIQUE INFORMATIQUEMENT
! TEST POUR SIMULER LE PB GENERALISE KU=LAMBDA*MU VIA LES CHEMINS
! INFORMATIQUE DU PB QUADRATIQUE. ON POSE C=-M ET M=0
! OBJECTIF: VALIDER LE QUADRATIQUE INFORMATIQUEMENT
! PERIMETRE: UNIQUEMENT EN SYMETRIQUE
!
! ATTENTION: CETTE PROGRAMMATION MODIFIE AMOR ET MASSE
!            ELLE NE PEUT DONC SERVIR QU'A TESTER MODE_ITER_SIMULT
!
!     LTESTQ=.FALSE.
!     IF ((LTESTQ).AND.(LC)) THEN
!       CALL JEVEUO(JEXNUM(AMOR(1:19)//'.VALM',1),'E',JVALMA)
!       CALL JEVEUO(JEXNUM(MASSE(1:19)//'.VALM',1),'E',JVALMM)
!       CALL JEVEUO(NUMEDD(1:14)//'.SMOS.SMHC','L',IHCOL)
!       CALL JEVEUO(NUMEDD(1:14)//'.SMOS.SMDI','L',IADIA)
!       IDEB=1
!       DO 35 J = 1,NEQ
!         IFIN = ZI(IADIA-1+J)
!         DO 34 I = IDEB,IFIN
!           ZR(JVALMA-1+I)=-ZR(JVALMM-1+I)
!           ZR(JVALMM-1+I)=0.D0
!  34     CONTINUE
!         IDEB = IFIN+1
!  35   CONTINUE
!     ENDIF
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!     -------  CALCUL DES VALEURS PROPRES ET VECTEURS PROPRES   --------
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!
!      IF (LCPU) THEN
!        CALL SYSTEM_CLOCK(IETFIN)
!        RETFIN=REAL(IETFIN-IETDEB)/REAL(IETRAT)
!        WRITE(IFM,*)'<OP0045> COUT PRETRAITEMENTS: ',RETFIN
!        CALL SYSTEM_CLOCK(IETDEB,IETRAT,IETMAX)
!      ENDIF
    if (.not.lc) then
!
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!     ---------------------  PROBLEME GENERALISE   ---------------------
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!
        if ((method(1:8).eq.'SORENSEN') .and. lkr .and. (.not.lns)) then
!     ------------------------------------------------------------------
!     -------  SORENSEN PB GENERALISE REEL SYMETRIQUE  --------
!     ------------------------------------------------------------------
            call vpsorn(lmasse, lmatra, neq, nbvect, nfreq,&
                        tolsor, zr(lvec), zr(lresid), zr(lworkd), zr(lworkl),&
                        lonwl, zl(lselec), zr(ldsor), omeshi, zr(laux),&
                        zr(lworkv), zi(lprod), zi(lddl), neqact, maxitr,&
                        ifm, niv, priram, alpha, omecor,&
                        nconv, flage, solveu)
            call rectfr(nconv, nconv, omeshi, npivot, nblagr,&
                        zr(ldsor), nfreq+1, zi(lresui), zr(lresur), mxresf)
            call vpbost(typres, nconv, nconv, omeshi, zr(ldsor),&
                        nfreq+1, vpinf, vpmax, precdc, method,&
                        omecor)
            if (typres .eq. 'DYNAMIQUE') call vpordi(1, 0, nconv, zr( lresur+mxresf), zr(lvec),&
                                                     neq, zi(lresui))
            do 37 imet = 1, nconv
                zi(lresui-1+ mxresf+imet) = izero
                zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+imet))
!           SI OPTION 'PLUS_GRANDE' : CONVERSION EN VALEUR PHYSIQUE
                if (lpg) zr(lresur-1+imet) = +1.d0 / (quapi2 * zr( lresur-1+imet))
                zr(lresur-1+2*mxresf+imet) = rzero
                zk24(lresuk-1+ mxresf+imet) = 'SORENSEN'
37          continue
            if (typres .ne. 'DYNAMIQUE') then
                call vpordo(0, 0, nconv, zr(lresur+mxresf), zr(lvec),&
                            neq)
                do 38 imet = 1, nconv
                    zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+ imet))
                    zi(lresui-1+imet) = imet
38              continue
            endif
            else if ((method(1:8).eq.'SORENSEN').and.(lns.or..not.lkr))&
        then
!     ------------------------------------------------------------------
!     -------  SORENSEN PB GENERALISE COMPLEXE OU REEL NON SYM  --------
!     ------------------------------------------------------------------
            if (lcomod) ASSERT(.false.)
            call vpsorc(lmasse, lmatra, neq, nbvect, nfreq,&
                        tolsor, zc(lvec), zc(lresid), zc(lworkd), zc(lworkl),&
                        lonwl, zl(lselec), zc(ldsor), sigma, zc(laux),&
                        zc(lworkv), zr( lworkr), zi(lprod), zi(lddl), neqact,&
                        maxitr, ifm, niv, priram, alpha,&
                        nconv, flage, solveu)
            npivot = nblagr
            call rectfc(nconv, nconv, sigma, npivot, nblagr,&
                        zc(ldsor), nfreq+1, zi(lresui), zr(lresur), nfreq)
            call vpbosc(typres, nconv, nconv, sigma, zc(ldsor),&
                        nfreq+1, vpinf, vpmax, precdc, method,&
                        omecor)
            do 377 imet = 1, nconv
                zi(lresui-1+ mxresf+imet) = izero
                zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+imet))
                zk24(lresuk-1+ mxresf+imet) = 'SORENSEN'
377          continue
!
        else if (lqz.and.lkr.and.(.not.lns)) then
!     ------------------------------------------------------------------
!     -------  QZ PB GENERALISE REEL SYMETRIQUE  --------
!     ------------------------------------------------------------------
            call vpqzla(typeqz, qrn, iqrn, lqrn, qrar,&
                        qrai, qrba, qrvl, lvec, kqrn,&
                        lvalpr, nconv, omecor, ktyp, kqrnr,&
                        neqact, ilscal, irscal, optiof, omemin,&
                        omemax, omeshi, zi(lprod), nfreq, lmasse,&
                        lraide, lamor, numedd, sigma, icscal,&
                        ivscal, iiscal, bwork, flage)
            call rectfr(nconv, nconv, omeshi, npivot, nblagr,&
                        zr(lvalpr), nfreq, zi(lresui), zr(lresur), mxresf)
            call vpbost(typres, nconv, nconv, omeshi, zr(lvalpr),&
                        nfreq, vpinf, vpmax, precdc, method,&
                        omecor)
            if (typres .eq. 'DYNAMIQUE') call vpordi(1, 0, nconv, zr( lresur+mxresf), zr(lvec),&
                                                     neq, zi(lresui))
            do 125 imet = 1, nconv
                zi(lresui-1+ mxresf+imet) = izero
                zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+imet))
!           SI OPTION 'PLUS_GRANDE' : CONVERSION EN VALEUR PHYSIQUE
                if (lpg) zr(lresur-1+imet) = +1.d0 / (quapi2 * zr( lresur-1+imet))
                zr(lresur-1+2*mxresf+imet) = rzero
                zk24(lresuk-1+ mxresf+imet) = typeqz
125          continue
            if (typres .ne. 'DYNAMIQUE') then
                call vpordo(0, 0, nconv, zr(lresur+mxresf), zr(lvec),&
                            neq)
                do 126 imet = 1, nconv
                    zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+ imet))
                    zi(lresui-1+imet) = imet
126              continue
            endif
!
        else if (lqz.and.((.not.lkr).or.lns)) then
!     ------------------------------------------------------------------
!     -------  QZ PB GENERALISE COMPLEXE OU REEL NON SYM  --------
!     ------------------------------------------------------------------
            if (lcomod) ASSERT(.false.)
            call vpqzla(typeqz, qrn, iqrn, lqrn, qrar,&
                        qrai, qrba, qrvl, lvec, kqrn,&
                        lvalpr, nconv, omecor, ktyp, kqrnr,&
                        neqact, ilscal, irscal, optiof, omemin,&
                        omemax, omeshi, zi(lprod), nfreq, lmasse,&
                        lraide, lamor, numedd, sigma, icscal,&
                        ivscal, iiscal, bwork, flage)
            npivot = nblagr
!
            call rectfc(nconv, nconv, sigma, npivot, nblagr,&
                        zc( lvalpr), nfreq, zi(lresui), zr(lresur), nfreq)
!
            call vpbosc(typres, nconv, nconv, sigma, zc(lvalpr),&
                        nfreq, vpinf, vpmax, precdc, method,&
                        omecor)
!
            do 127 imet = 1, nconv
                zi(lresui-1+ mxresf+imet) = izero
                zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+imet))
                zk24(lresuk-1+ mxresf+imet) = typeqz
127          continue
!
        else if (method(1:6).eq.'JACOBI') then
!     ------------------------------------------------------------------
!     -------  JACOBI PB GENERALISE REEL   --------
!     ------------------------------------------------------------------
            if ((.not.lkr) .or. lns) ASSERT(.false.)
            call sspace(lmtpsc, lmatra, lmasse, neq, nbvect,&
                        nfreq, zi( lprod), itemax, nperm, tol,&
                        toldyn, zr(lvec), zr(lvalpr), nitjac, nitbat,&
                        solveu)
            call rectfr(nfreq, nbvect, omeshi, npivot, nblagr,&
                        zr(lvalpr), nbvect, zi(lresui), zr(lresur), mxresf)
            call vpbost(typres, nfreq, nbvect, omeshi, zr(lvalpr),&
                        nbvect, vpinf, vpmax, precdc, method,&
                        omecor)
            if (typres .eq. 'DYNAMIQUE') call vpordi(1, 0, nfreq, zr( lresur+mxresf), zr(lvec),&
                                                     neq, zi(lresui))
!
            do 30 imet = 1, nfreq
                zi(lresui-1+2*mxresf+imet) = nitbat
                zi(lresui-1+4*mxresf+imet) = nitjac
                zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+imet))
!           SI OPTION 'PLUS_GRANDE' : CONVERSION EN VALEUR PHYSIQUE
                if (lpg) zr(lresur-1+imet) = +1.d0 / (quapi2 * zr( lresur-1+imet))
                zr(lresur-1+2*mxresf+imet) = rzero
                zk24(lresuk-1+ mxresf+imet) = 'BATHE_WILSON'
30          continue
            if (typres .ne. 'DYNAMIQUE') then
                call vpordo(0, 0, nfreq, zr(lresur+mxresf), zr(lvec),&
                            neq)
                do 31 imet = 1, nfreq
                    zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+ imet))
                    zi(lresui-1+imet) = imet
31              continue
            endif
!
        else if (method(1:8).eq.'TRI_DIAG') then
!     ------------------------------------------------------------------
!     -------  LANCZOS PB GENERALISE REEL   --------
!     ------------------------------------------------------------------
            if ((.not.lkr) .or. lns) ASSERT(.false.)
            if (nstoc .ge. nbvect) call u2mess('A', 'ALGELINE2_72')
            if (nstoc .ne. 0) then
                do 26 i = 1, neq * nstoc
                    zr(lvec + i - 1) = zr(lxrig + i -1)
26              continue
            endif
            call vp2ini(lmtpsc, lmasse, lmatra, neq, nbvect,&
                        nborto, prorto, zi(lprod), zi(lddl), zr(ldiagr),&
                        zr(lsurdr), zr(lsign), zr( lvec), prsudg, nstoc,&
                        omeshi, solveu)
            call vp2trd('G', nbvect, zr(ldiagr), zr(lsurdr), zr(lsign),&
                        zr(iadz), nitv, nitqrm)
            call vpreco(nbvect, neq, zr(iadz), zr(lvec))
            call rectfr(nfreq, nbvect, omeshi, npivot, nblagr,&
                        zr(ldiagr), nbvect, zi(lresui), zr(lresur), mxresf)
            call vpbost(typres, nfreq, nbvect, omeshi, zr(ldiagr),&
                        nbvect, vpinf, vpmax, precdc, method,&
                        omecor)
            if (typres .eq. 'DYNAMIQUE') call vpordi(1, 0, nfreq, zr( lresur+mxresf), zr(lvec),&
                                                     neq, zi(lresui))
            do 32 imet = 1, nfreq
                zi(lresui-1+ mxresf+imet) = nitqrm
                zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+imet))
!           SI OPTION 'PLUS_GRANDE' : CONVERSION EN VALEUR PHYSIQUE
                if (lpg) zr(lresur-1+imet) = +1.d0 / (quapi2 * zr( lresur-1+imet))
                zr(lresur-1+2*mxresf+imet) = rzero
                zk24(lresuk-1+ mxresf+imet) = 'LANCZOS'
32          continue
            if (typres .ne. 'DYNAMIQUE') then
                call vpordo(0, 0, nfreq, zr(lresur+mxresf), zr(lvec),&
                            neq)
                do 33 imet = 1, nfreq
                    zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+ imet))
                    zi(lresui-1+imet) = imet
33              continue
            endif
        endif
!
    else
!
        if (lcomod) ASSERT(.false.)
!
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!     ---------------------  PROBLEME QUADRATIQUE   --------------------
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!
        if (method(1:8) .eq. 'TRI_DIAG') then
!     ------------------------------------------------------------------
!     -------  LANCZOS PB QUADRATIQUE   --------
!     ------------------------------------------------------------------
            call wp2ini(appr, lmasse, lamor, lraide, lmatra,&
                        lmtpsc, sigma, zr(iadrh), zr(iadrb), optiof,&
                        prorto, nborto, nbvect, neq, zi( lprod),&
                        zi(lddl), zr(ldiagr), zr(lsurdr), zr(lsign), zr(iadx),&
                        zr(iady), solveu)
            call vp2trd('Q', nbvect, zr(ldiagr), zr(lsurdr), zr(lsign),&
                        zr(iadz), nitv, nitqrm)
            npivot = nblagr
            nfreq = nfreq / 2
            call wp2vec(appr, optiof, nfreq, nbvect, neq,&
                        sigma, zr(iadx), zr(iady), zr(iadz), 2*nbvect,&
                        zr(lsurdr), zr(ldiagr), zc(lvec), mxresf, zi(lresui),&
                        zr(lresur), zi(lprod), omecor)
            do 36 imet = 1, nfreq
                zi(lresui-1+mxresf+imet) = nitqrm
                zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+imet))
                zk24(lresuk-1+mxresf+imet) = 'LANCZOS'
36          continue
!
        else if (lqz) then
!     ------------------------------------------------------------------
!     -------  QZ PB QUADRATIQUE REEL ET COMPLEXE, SYM OU NON  --------
!     ------------------------------------------------------------------
            call vpqzla(typeqz, qrn, iqrn, lqrn, qrar,&
                        qrai, qrba, qrvl, lvec, kqrn,&
                        lvalpr, nconv, omecor, ktyp, kqrnr,&
                        neqact, ilscal, irscal, optiof, omemin,&
                        omemax, omeshi, zi(lprod), nfreq, lmasse,&
                        lraide, lamor, numedd, sigma, icscal,&
                        ivscal, iiscal, bwork, flage)
            nfreq=nfreq/2
            call wp4vec(nfreq, nconv, neq, sigma, zc(lvalpr),&
                        zc(lvec), mxresf, zi(lresui), zr(lresur), zi(lprod),&
                        zc(lauc), omecor)
            do 578 imet = 1, nfreq
                zi(lresui-1+mxresf+imet) = izero
                zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+imet))
                zk24(lresuk-1+mxresf+imet) = typeqz
578          continue
        else if (method(1:8).eq.'SORENSEN') then
            if (lkr) then
                if ((appr.eq.'R') .or. (appr.eq.'I')) then
!     ------------------------------------------------------------------
!     -------  SORENSEN PB QUADRATIQUE REEL  SYM  --------
!     -------  APPROCHE REELLE OU IMAGINAIRE      --------
!     ------------------------------------------------------------------
                    call wpsorn(appr, lmasse, lamor, lmatra, neq,&
                                nbvect, nfreq, tolsor, zc(lvec), zr(lresid),&
                                zr(lworkd), zr(lworkl), lonwl, zl(lselec), zr( ldsor),&
                                zr(lsurdr), zr(ldiagr), sigma, zr(laux), zr(lworkv),&
                                zi(lprod), zi(lddl), neqact, maxitr, ifm,&
                                niv, priram, alpha, nconv, flage,&
                                zr(laur), zc(lauc), zc(laul), solveu)
                    nfreq = nconv / 2
                    call wp3vec(appr, optiof, nfreq, nconv, neq,&
                                sigma, zr(lsurdr), zr(ldiagr), zc(lvec), mxresf,&
                                zi(lresui), zr(lresur), zi(lprod), zc(lauc), omecor)
                else
!     ------------------------------------------------------------------
!     -------  SORENSEN PB QUADRATIQUE REEL,SYM OU NON   --------
!     -------  APPROCHE COMPLEXE                         --------
!     ------------------------------------------------------------------
                    call wpsorc(lmasse, lamor, lmatra, neq, nbvect,&
                                nfreq, tolsor, zc(lvec), zc(lresid), zc(lworkd),&
                                zc(lworkl), lonwl, zl(lselec), zc(ldsor), sigma,&
                                zc(laux), zc(lworkv), zi(lprod), zi(lddl), neqact,&
                                maxitr, ifm, niv, priram, alpha,&
                                nconv, flage, zc(lauc), zr(laur), solveu)
                    nfreq = nconv / 2
                    call wp4vec(nfreq, nconv, neq, sigma, zc(ldsor),&
                                zc( lvec), mxresf, zi(lresui), zr(lresur), zi(lprod),&
                                zc( lauc), omecor)
                endif
                do 378 imet = 1, nfreq
                    zi(lresui-1+mxresf+imet) = izero
                    zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+ imet))
                    zk24(lresuk-1+mxresf+imet) = 'SORENSEN'
378              continue
            else
!     ------------------------------------------------------------------
!     -------  SORENSEN PB QUADRATIQUE COMPLEXE SYM  --------
!     -------  APPROCHE COMPLEXE                     --------
!     ------------------------------------------------------------------
                if (lns) ASSERT(.false.)
                call wpsorc(lmasse, lamor, lmatra, neq, nbvect,&
                            nfreq, tolsor, zc(lvec), zc(lresid), zc(lworkd),&
                            zc( lworkl), lonwl, zl(lselec), zc(ldsor), sigma,&
                            zc(laux), zc(lworkv), zi(lprod), zi(lddl), neqact,&
                            maxitr, ifm, niv, priram, alpha,&
                            nconv, flage, zc(lauc), zr(laur), solveu)
                nfreq = nconv / 2
                call wp5vec(optiof, nfreq, nconv, neq, zc(ldsor),&
                            zc(lvec), mxresf, zi(lresui), zr(lresur), zc(lauc))
                do 379 imet = 1, nfreq
                    zi(lresui-1+mxresf+imet) = izero
                    zr(lresur-1+imet) = freqom(zr(lresur-1+mxresf+ imet))
                    zk24(lresuk-1+mxresf+imet) = 'SORENSEN'
379              continue
            endif
        endif
    endif
! ---- NOMBRE DE MODES CONVERGES
! ---- SI LE SOLVEUR MODAL A BIEN ACHEVE SON TRAVAIL ON FAIT CETTE AFFEC
! ---- TATION SINON ON NE TIENT COMPTE QUE DES NCONV MODES REELLEMENT CV
    if (.not.flage) nconv = nfreq
!
!     ------------------------------------------------------------------
!     -------------------- CORRECTION : OPTION BANDE -------------------
!     ------------------------------------------------------------------
!
!     --- SI OPTION BANDE ON NE GARDE QUE LES FREQUENCES DANS LA BANDE
    mfreq = nconv
    if (optiof .eq. 'BANDE') then
        if (lc .or. lns .or. .not.lkr) ASSERT(.false.)
        do 110 ifreq = mfreq - 1, 0
            if (zr(lresur+mxresf+ifreq) .gt. omemax .or. zr(lresur+ mxresf+ifreq) .lt. omemin) &
            nconv = nconv - 1
110      continue
        if (mfreq .ne. nconv) call u2mess('I', 'ALGELINE2_17')
    endif
!
!
!      IF (LCPU) THEN
!        CALL SYSTEM_CLOCK(IETFIN)
!        RETFIN=REAL(IETFIN-IETDEB)/REAL(IETRAT)
!        WRITE(IFM,*)'<OP0045> COUT SOLVEUR MODAL + POST 1: ',RETFIN
!        CALL SYSTEM_CLOCK(IETDEB,IETRAT,IETMAX)
!      ENDIF
!
!     ------------------------------------------------------------------
!     -------------- CALCUL DES PARAMETRES GENERALISES  ----------------
!     ----------- CALCUL DE LA NORME D'ERREUR SUR LE MODE  -------------
!     ---------------- STOCKAGE DES VECTEURS PROPRES  ------------------
!     ------------------------------------------------------------------
!
!     --- POSITION MODALE NEGATIVE DES MODES INTERDITE
    knega = 'NON'
    nparr = nbparr
    if (typcon .eq. 'MODE_ACOU') nparr = 7
!
!     ------------------------------------------------------------------
!     --------------------MACRO_MODE_MECA PARALLELE (PART III)----------
!     ------------------------------------------------------------------
    if ((.not.lc) .and. lkr .and. (.not.lns)) then
        call vppara(modes, typcon, knega, lraide, lmasse,&
                    lamor, mxresf, neq, nconv, omecor,&
                    zi(lddl), zi(lprod), zr(lvec), cbid, nbpari,&
                    nparr, nbpark, nopara, '    ', zi(lresui),&
                    zr(lresur), zk24(lresuk), ktyp, lcomod, icom1,&
                    icom2, typres, nfreqg)
    else
        if (lcomod) ASSERT(.false.)
        call vppara(modes, typcon, knega, lraide, lmasse,&
                    lamor, mxresf, neq, nconv, omecor,&
                    zi(lddl), zi(lprod), rbid, zc(lvec), nbpari,&
                    nparr, nbpark, nopara, '    ', zi(lresui),&
                    zr(lresur), zk24(lresuk), ktyp, lcomod, ibid,&
                    ibid, k16bid, ibid)
    endif
!      IF (LCPU) THEN
!        CALL SYSTEM_CLOCK(IETFIN)
!        RETFIN=REAL(IETFIN-IETDEB)/REAL(IETRAT)
!        WRITE(IFM,*)'<OP0045> COUT VPPARA HORS COM: ',RETFIN
!        CALL SYSTEM_CLOCK(IETDEB,IETRAT,IETMAX)
!      ENDIF
!
!     --- IMPRESSIONS LIEES A LA METHODE ---
    call vpwecf(' ', typres, nconv, mxresf, zi(lresui),&
                zr(lresur), zk24(lresuk), lamor, ktyp, lns)
    call titre()
!
!     ------------------------------------------------------------------
!     ----------- CONTROLE DE VALIDITE DES MODES CALCULES  -------------
!     ------------------------------------------------------------------
!
    call getvtx('VERI_MODE', 'STOP_ERREUR', 1, iarg, 1,&
                optiov, lmf)
    if (optiov .eq. 'OUI') then
        ctyp = 'E'
    else
        ctyp = 'A'
    endif
!
    call getvr8('VERI_MODE', 'SEUIL', 1, iarg, 1,&
                seuil, lmf)
    call getvtx('VERI_MODE', 'STURM', 1, iarg, 1,&
                optiov, lmf)
    if (optiov .eq. 'NON') then
        optiov = ' '
    else
        optiov = optiof
        if (lc .or. (.not.lkr) .or. lns) then
! --- POUR DEBRANCHER LE TEST DE STURM DANS VPCNTL
            optiov = ' '
            valk(1) = matra
            valk(2) = matrc
            call u2mesk('I', 'ALGELINE2_73', 2, valk)
        endif
    endif
!
    lmat(1) = lraide
    lmat(2) = lmasse
    lmat(3) = lmtpsc
! --- SI ON MANIPULE DEUX MATRICES DYNAMIQUES (MATOPA/MATPSC), ON SE
!     DEBARASSE DE CELLE INUTILE (MATRICE + FACTORISEE EVENTUELLE)
!     ET DE SON EVENTUELLE OCCURENCE EXTERNE (MUMPS)
    if ((lmtpsc.ne.lmatra) .and. (lmatra.ne.0)) call detrsd('MATR_ASSE', matopa)
!
!     ------------------------------------------------------------------
!     --------------------MACRO_MODE_MECA PARALLELE (PART IV)-----------
!     ------------------------------------------------------------------
!     --- EN CAS DE TEST DE STURM LOCAL A CHAQUE SOUS-BANDE, REMISE A
!     --- JOUR DES BORNES VIA LE COM WORLD.
!     --- PUIS ON REMET LE COMCOU POUR NE PAS GENER LES FACTOS EVENTUEL
!     --- LES DE VPCNTL.
!     ------------------------------------------------------------------
    if (lcomod) then
        call asmpi_comm('SET', mpicow)
        call asmpi_barrier()
        call asmpi_comm_vect('MPI_MIN', 'R', scr=omemin)
        call asmpi_comm_vect('MPI_MIN', 'R', scr=vpinf)
        call asmpi_comm_vect('MPI_MAX', 'R', scr=omemax)
        call asmpi_comm_vect('MPI_MAX', 'R', scr=vpmax)
        call asmpi_barrier()
        call asmpi_comm('SET', mpicou)
    endif
    call vpcntl(ctyp, modes, optiov, omemin, omemax,&
                seuil, nconv, zi(lresui), lmat, omecor,&
                precdc, ierx, vpinf, vpmax, zr(lresur),&
                zr(lresur+3*mxresf), zr(lresur+mxresf), typres, nblagr, solveu,&
                nbrss, precsh)
    call getvtx('VERI_MODE', 'STOP_ERREUR', 1, iarg, 1,&
                optiov, lmf)
!
    if ((optiov.eq.'OUI') .and. (ierx.ne.0)) call u2mess('F', 'ALGELINE2_74')
!
    if (flage) call u2mess('F', 'ALGELINE5_75')
999  continue
!
!
!     ------------------------------------------------------------------
!
888  continue
!     --- DESTRUCTION DE LA MATRICE DYNAMIQUE RESTANTE (VRAI MATPSC DIS
!     SOSSIEE DE MATOPA OU MATPSC POINTANT SUR MATOPA D'OU LA RECONSTRUC
!     TION DE NOM CI-DESSOUS
    if (lmtpsc .ne. 0) then
        matpsc=zk24(zi(lmtpsc+1))(1:19)
        call detrsd('MATR_ASSE', matpsc)
    endif
    if (associated(bwork)) deallocate(bwork)
!
!     ------------------------------------------------------------------
!     -----------------------MACRO_MODE_MECA PARALLELE (PART V) -------
!     ------------------------------------------------------------------
!     --- AVANT DE QUITTER L'OP. ON REMET LE COM WORLD (AU CAS OU)
!     --- DESTRUCTION DES SOUS-COMMUNICATEURS EVENTUELLEMENT ASSOCIES A
!     --- UNE OCCURENCE MUMPS (APRES CELLE DE LADITE OCCURENCE)
!     ------------------------------------------------------------------
    if (lcomod) then
        call asmpi_comm('SET', mpicow)
        call asmpi_barrier()
        call asmpi_comm('FREE', mpicou)
    endif
!      IF (LCPU) THEN
!        CALL SYSTEM_CLOCK(IETFIN)
!        RETFIN=REAL(IETFIN-IETDEB)/REAL(IETRAT)
!        WRITE(IFM,*)'<OP0045> COUT POST 3: ',RETFIN
!        CALL SYSTEM_CLOCK(IETDEB,IETRAT,IETMAX)
!      ENDIF
    call jedema()
!
!     FIN DE OP0045
!
end subroutine
