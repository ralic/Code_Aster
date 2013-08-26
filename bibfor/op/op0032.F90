subroutine op0032()
    implicit none
!     ------------------------------------------------------------------
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
!     EIGENVALUE-COUNTING METHODS FOR GEP OR QEP
!     ------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
!
!
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterc/asmpi_split_comm.h"
#include "asterc/getres.h"
#include "asterc/getvc8.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/apm012.h"
#include "asterfort/apm345.h"
#include "asterfort/asmpi_barrier.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/cresol.h"
#include "asterfort/detrsd.h"
#include "asterfort/freqom.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/omega2.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/titre.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/vecint.h"
#include "asterfort/vpddl.h"
#include "asterfort/vpecst.h"
#include "asterfort/vpfopr.h"
#include "asterfort/vrrefe.h"
#include "asterfort/wkvect.h"
    mpi_int :: mpicow, mpicou, mrang, mnbproc
    integer :: rang, nbproc
    integer :: islvk, islvi, jrefa, itest, nmultc, lamor, jlmod, jlmoe, pivot1
    integer :: pivot2, mxddl, nbrss, ierd, ii, ifapm, k, nbmod, nblagr, nbcine
    integer :: neqact, neq, niterc, npivot(2), l, lmasse, lraide, lddl
    integer :: ldynam, nk, nbrow, lprod, iret, nbfreq, krefa, idet(2), jstu
    integer :: vali(4), ifm, niv, nbtetc, nbtet0, nbtet1, typeco
    integer :: nbtet2, nbev0, nbev1, nbev2, miterc, iarg, ibid, k1, k2
    integer :: jkpar, l1, l2, l3, l11, l21, frecou, izero
    real(kind=8) :: omgmin, omgmax, omin, omax, fcorig, omecor, precsh, rayonc
    real(kind=8) :: dimc1, rzero, calpar(2), calpac(3), calpaf(2), rbid, det(2)
    complex(kind=8) :: centrc, zimc1, cbid
    logical :: ltest, lc, ldyna, lflamb, lfirst, lcomod, lcoinf
    character(len=1) :: typep, tpparn(1), tpparr(2), tpparc(3), tpparf(2)
    character(len=1) :: tpparm(2), k1bid
    character(len=3) :: impr
    character(len=8) :: typcon, typmet, typcha, table, kopt1, koptn
    character(len=14) :: matra, matrb, matrc
    character(len=16) :: concep, nomcmd, typmod, typpar, nmparn(1), nmparr(2)
    character(len=16) :: nmparc(3), nmparf(2), nmparm(2)
    character(len=19) :: masse, raide, dynam, solveu, amor, matref
    character(len=24) :: valk(4), metres, k24rc, kbid, k24mod, k24stu, k24moe
    character(len=24) :: k24par
    parameter   ( mxddl=1,miterc=10000,nmultc=2)
!     ------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
    rzero=0.d0
    izero=0
!-----------------------------------------------------------------------
!------------------ INITIALIZATIONS/READING OF THE USER-DATA -----------
!-----------------------------------------------------------------------
!
!     --- OUTPUT CONCEPT ---
    call getres(table, concep, nomcmd)
!
!     ------------------------------------------------------------------
!     ------- INFO_MODE // SEUL OU DS MACRO_MODE_MECA (PART I)  --------
!     ------------------------------------------------------------------
!     --- RECUPERATION ET TEST DE VALIDITE DES PARAMETRES
!     ------------------------------------------------------------------
    call asmpi_comm('GET_WORLD', mpicow)
    call asmpi_comm('GET', mpicou)
    if (mpicow .ne. mpicou) ASSERT(.false.)
    call asmpi_info(mpicow, mrang, mnbproc)
    rang = to_aster_int(mrang)
    nbproc = to_aster_int(mnbproc)
!
!     INFO // DS MACRO_MODE_MECA
    typeco=-999
    call getvis('PARALLELISME_MACRO', 'TYPE_COM', 1, iarg, 1,&
                typeco, l)
    valk(1)='TYPE_COM'
    vali(1)=typeco
    if (l .ne. 1) call u2mesg('F', 'APPELMPI_6', 1, valk, 1,&
                              vali, 0, rbid)
    valk(2)='RANG'
    valk(3)='NBPROC'
    vali(2)=rang
    vali(3)=nbproc
    if ((&
        ((typeco.ne.1).and.(typeco.ne.2).and.(typeco.ne.-999)) .or. (nbproc.lt.1) .or.&
        (rang.lt.0)&
        )) call u2mesg('F', 'APPELMPI_8', 3, valk, 3,&
                       vali, 0, rbid)
    if (((typeco.eq.1).or.(typeco.eq.2)) .and. (nbproc.gt.1)) then
        lcomod=.true.
    else
        lcomod=.false.
    endif
!
!     INFO // SEUL
    typpar='XXXXXXXXXXXXXXXX'
    call getvtx(' ', 'NIVEAU_PARALLELISME', 1, iarg, 1,&
                typpar, l)
    if (l .ne. 1) ASSERT(.false.)
    if ((typpar.ne.'COMPLET') .and. (typpar.ne.'PARTIEL')) ASSERT(.false.)
    if ((typpar.eq.'COMPLET') .and. (nbproc.gt.1) .and. (.not.lcomod)) then
        lcoinf=.true.
        typeco=1
    else
        lcoinf=.false.
    endif
!     ------------------------------------------------------------------
!
!
!     --- READ OF MATRICES, CHECK OF REFERENCES ---
!     --- COMPUTATION OF THE MATRIX DESCRIPTORS ---
    call getvtx(' ', 'TYPE_MODE', 1, iarg, 1,&
                typmod, ibid)
    if (typmod .eq. 'DYNAMIQUE') then
        matra = 'MATR_RIGI'
        matrb = 'MATR_MASS'
        matrc = 'MATR_AMOR'
    else if (typmod .eq. 'MODE_FLAMB') then
        matra = 'MATR_RIGI'
        matrb = 'MATR_RIGI_GEOM'
    else if (typmod .eq. 'GENERAL') then
        matra = 'MATR_A'
        matrb = 'MATR_B'
        matrc = 'MATR_C'
    else if (typmod .eq. 'MODE_COMPLEXE') then
        matra = 'MATR_RIGI'
        matrb = 'MATR_MASS'
        matrc = 'MATR_AMOR'
    endif
    call getvid(' ', matra, 1, iarg, 1,&
                raide, l)
    call getvid(' ', matrb, 1, iarg, 1,&
                masse, l)
    amor=' '
    lamor=0
    if ((typmod.eq.'GENERAL') .or. (typmod.eq.'MODE_COMPLEXE')) then
        call getvid(' ', matrc, 1, iarg, 1,&
                    amor, lamor)
    endif
    if (lamor .eq. 0) then
        lc=.false.
    else
        lc=.true.
    endif
!
    call vrrefe(masse, raide, iret)
    if (iret .ne. 0) then
        valk(1) = raide
        valk(2) = masse
        call u2mesk('F', 'ALGELINE2_58', 2, valk)
    endif
    call mtdscr(masse)
    call jeveuo(masse//'.&INT', 'E', lmasse)
    call mtdscr(raide)
    call jeveuo(raide//'.&INT', 'E', lraide)
!   --- REFERENCE MATRICE TO BE USE AS A PATTERN FOR BUILDING THE ---
!   --- DYNAMIC MATRICES (THE ISSUE IS SYMMETRIC OR NOT)          ---
    matref=raide
    if (zi(lmasse+4) .eq. 0) matref=masse
    if (zi(lraide+4) .eq. 0) matref=raide
    if (lc) then
        call mtdscr(amor)
        call jeveuo(amor//'.&INT', 'E', lamor)
        if (zi(lamor+4) .eq. 0) matref=amor
    endif
!
!     --- READING/TREATEMENT SD LINEAR SOLVER  ---
    call jeveuo(raide//'.REFA', 'L', jrefa)
    solveu='&&OP0032.SOLVEUR'
    call cresol(solveu)
    call jeveuo(solveu//'.SLVK', 'L', islvk)
    call jeveuo(solveu//'.SLVI', 'L', islvi)
    metres=zk24(islvk)
    if ((metres(1:4).ne.'LDLT') .and. (metres(1:10).ne.'MULT_FRONT') .and.&
        (metres(1:5).ne.'MUMPS')) call u2mess('F', 'ALGELINE5_71')
!
!
!     --- TYPE OF EIGENVALUE-COUNTING METHOD ---
    call getvtx('COMPTAGE', 'METHODE', 1, iarg, 1,&
                typmet, ibid)
!
!     --- AUTOMATIC PARAMETRIZATION WITH 'AUTO'                 ---
    if (typmet(1:4) .eq. 'AUTO') then
        if (zi(lmasse+3)*zi(lmasse+4)*zi(lraide+3)*zi(lraide+4) .ne. 1 .or. lc) then
            typmet='APM'
        else
            typmet='STURM'
        endif
        valk(1)=typmet
        call u2mesk('I', 'ALGELINE2_27', 1, valk)
    endif
!     --- IF GENERAL: KIND OF COMPUTATION   ---
    if (typmod(1:7) .eq. 'GENERAL') then
        if (zi(lmasse+3)*zi(lmasse+4)*zi(lraide+3)*zi(lraide+4) .ne. 1 .or. lc) then
            typmod='MODE_COMPLEXE'
        else
            typmod='MODE_FLAMB'
        endif
        valk(1)=typmod
        call u2mesk('I', 'ALGELINE2_31', 1, valk)
    endif
!
!     --- TEMPORARY EXCLUSION RULES                             ---
!     --- + DEFAULT VALUES                                      ---
    if ((typmod(1:13).eq.'MODE_COMPLEXE') .and. (typmet(1:3).ne.'APM')) then
        call u2mess('I', 'ALGELINE4_20')
        typmet='APM'
    endif
    if ((typmod(1:13).ne.'MODE_COMPLEXE') .and. (typmet(1:5).ne.'STURM')) then
        call u2mess('I', 'ALGELINE4_20')
        typmet='STURM'
    endif
!
!     --- KIND OF COMPUTATION : REAL (GEP), DYNAMIC OR BUCKLING    ---
!     --- NBMOD: SIZE OF THE LIST OF MODES (FREQUENCIES OR BUCKLING---
!     --- MODES).
!     --- NAME OF JEVEUX OBJECTS NEEDED FOR BUILDING THE CARTE ---
    k24mod='&&OP0032.LISTE_MODE'
    k24moe='&&OP0032.LISTE_MODE_EFF'
    k24stu='&&OP0032.RESU_STURM'
    ldyna=.false.
    lflamb=.false.
    nbmod=-9999
    if (typmod(1:9) .eq. 'DYNAMIQUE') then
!     --- COUPLE OR LIST OF FREQUENCIES ---
        ldyna=.true.
        call getvr8(' ', 'FREQ', 1, iarg, 0,&
                    rbid, l)
        nbmod=abs(l)
        call wkvect(k24mod, 'V V R', nbmod, jlmod)
        call wkvect(k24moe, 'V V R', nbmod, jlmoe)
        call wkvect(k24stu, 'V V I', nbmod-1, jstu)
        call getvr8(' ', 'FREQ', 1, iarg, nbmod,&
                    zr(jlmod), l)
        if (l .ne. nbmod) ASSERT(.false.)
        do 10 k = 1, nbmod-1
            zi(jstu+k-1)=izero
            zr(jlmoe+k-1)=rzero
            if (zr(jlmod+k) .le. zr(jlmod+k-1)) ASSERT(.false.)
10      continue
        zr(jlmoe+nbmod-1)=rzero
!
    else if (typmod(1:13).eq.'MODE_COMPLEXE') then
!     --- CHARACTERISTIC OF THE COMPLEX SHAPE ---
        call getvtx(' ', 'TYPE_CONTOUR', 1, iarg, 1,&
                    typcon, l1)
        call getvr8(' ', 'RAYON_CONTOUR', 1, iarg, 1,&
                    rayonc, l2)
        call getvc8(' ', 'CENTRE_CONTOUR', 1, iarg, 1,&
                    centrc, l3)
        if ((abs(l1)*abs(l2)*abs(l3)) .ne. 1) ASSERT(.false.)
        calpac(1) = dble(centrc)
        calpac(2) = dimag(centrc)
        calpac(3) = rayonc
        nbmod=2
!
    else if (typmod(1:10).eq.'MODE_FLAMB') then
!     --- COUPLE OR LIST OF BUCKLING MODES ---
        lflamb=.true.
        call getvr8(' ', 'CHAR_CRIT', 1, iarg, 0,&
                    rbid, l)
        if (abs(l) .ge. 2) then
            nbmod=abs(l)
            call wkvect(k24mod, 'V V R', nbmod, jlmod)
            call wkvect(k24moe, 'V V R', nbmod, jlmoe)
            call wkvect(k24stu, 'V V I', nbmod-1, jstu)
            call getvr8(' ', 'CHAR_CRIT', 1, iarg, nbmod,&
                        zr(jlmod), l)
            if (l .ne. nbmod) ASSERT(.false.)
            do 12 k = 1, nbmod-1
                zi(jstu+k-1)=izero
                zr(jlmoe+k-1)=rzero
                if (zr(jlmod+k) .le. zr(jlmod+k-1)) ASSERT(.false.)
12          continue
            zr(jlmoe+nbmod-1)=rzero
        else
!       --- PARAMETRIZATION PB
            ASSERT(.false.)
        endif
!
    else
!     --- BAD VALUE OF TYMOD ---
        ASSERT(.false.)
!
    endif
!
!     --- GET THE PARAMETERS OF THE METHOD                      ---
!     --- INITIALIZATIONS JUST IN CASE                          ---
    fcorig=1.d-2
    precsh=1.d-2
    nbrss=5
    nbtetc=40
    niterc=3
    if (typmet(1:5) .eq. 'STURM') then
        if (typmod(1:9) .eq. 'DYNAMIQUE') then
            call getvr8('COMPTAGE', 'SEUIL_FREQ', 1, iarg, 1,&
                        fcorig, ibid)
            omecor=omega2(fcorig)
        else
            call getvr8('COMPTAGE', 'SEUIL_CHAR_CRIT', 1, iarg, 1,&
                        fcorig, ibid)
            omecor=fcorig
        endif
        call getvr8('COMPTAGE', 'PREC_SHIFT', 1, iarg, 1,&
                    precsh, ibid)
        call getvis('COMPTAGE', 'NMAX_ITER_SHIFT', 1, iarg, 1,&
                    nbrss, ibid)
    else if (typmet(1:3).eq.'APM') then
        call getvis('COMPTAGE', 'NBPOINT_CONTOUR', 1, iarg, 1,&
                    nbtetc, ibid)
        call getvis('COMPTAGE', 'NMAX_ITER_CONTOUR', 1, iarg, 1,&
                    niterc, ibid)
!     --- TEMPORARY, WE UNPLUG THE USE OF ROMBOUT METHOD, IT NEEDS ---
!     --- TO BE MORE RELIABLE                                      ---
        typcha='LDLT'
!        TYPCHA='ROMBOUT'
!        CALL GETVTX('COMPTAGE','POLYNOME_CHARAC',1,IARG,1,TYPCHA,IBID)
    else
        ASSERT(.false.)
    endif
!
!-----------------------------------------------------------------------
!-------------------- EXCLUSION RULES, PARTICULAR CASES ----------------
!-----------------------------------------------------------------------
!
!
!     --- EXCLUSION RULE IF NONSYMETRIC OR COMPLEXE GEP OR QEP  ---
    if ((zi(lmasse+3)*zi(lmasse+4)*zi(lraide+3)*zi(lraide+4).ne.1 .or.lc)) then
        if (typmod(1:13) .ne. 'MODE_COMPLEXE') call u2mess('F', 'ALGELINE4_10')
    endif
!
!     --- CURRENT SCOPE OF USE OF THE OPTION TYPCHA='ROMBOUT'   ---
    if ((typmet(1:3).eq.'APM') .and. (typcha(1:7).eq.'ROMBOUT')) then
        if (lc .or. (zk24(jrefa+9)(1:4).eq.'GENE') .or.&
            (zi(lmasse+3)*zi( lmasse+4)*zi(lraide+3)*zi(lraide+4).ne.1)) then
            call u2mess('F', 'ALGELINE4_17')
        endif
    endif
!
!      --- SCHEMAS PARALLELES
!
!      --- INFO_MODE OU MACRO_MODE_MECA // VALIDES QU'AVEC STURM
    if (lcomod .or. lcoinf) then
!      --- PROBABLEMENT MAUVAISE PROGRAMMATION EN AMONT
        if (typmet(1:5) .ne. 'STURM') ASSERT(.false.)
    endif
!
!      --- INFO_MODE PARALLELE: INCOMPATIBILITES FONCTIONNELLES ET
!      --- DESEQUILIBRAGE DE CHARGE POTENTIEL.
    if (lcoinf) then
        if ((typpar.eq.'PARTIEL') .and. (metres(1:5).ne.'MUMPS') .and. (nbproc.gt.1)) then
            vali(1)=nbproc
            valk(1)=metres
            call u2mesg('F', 'MODAL_14', 1, valk, 1,&
                        vali, 0, rbid)
        endif
        if ((nbproc.lt.(nbmod-1)) .or.&
            ((nbproc.gt.(nbmod-1)).and.( metres(1:5).ne.'MUMPS'))) then
            vali(1)=nbproc
            vali(2)=nbmod
            valk(1)=metres
            call u2mesg('F', 'MODAL_10', 1, valk, 2,&
                        vali, 0, rbid)
        endif
        l1=nbproc/(nbmod-1)
        l2=nbproc-(nbmod-1)*l1
        if ((nbproc.gt.(nbmod-1)) .and. (l2.ne.0)) then
            vali(1)=nbmod-1
            vali(2)=l1
            vali(3)=l1+1
            call u2mesg('I', 'MODAL_11', 0, kbid, 3,&
                        vali, 0, rbid)
        endif
    endif
!
!-----------------------------------------------------------------------
!-------------------------- PRE-TRAITEMENTS ----------------------------
!-----------------------------------------------------------------------
!
!     --- PREPARATION FOR THE COMPUTATION OF THE DYNAMIC MATRIX ---
!     --- IN GEP ONLY DYNAM, IN QEP DYNAM            ---
    if ((typmet(1:5).eq.'STURM') .or. ((typmet(1:3).eq.'APM').and.(typcha(1:4).eq.'LDLT'))) then
        dynam = '&&OP0032.MATR_DYNAM'
        if (typmet(1:5) .eq. 'STURM') then
!     --- IF STURM TEST, DYNAM'TYPE IS THE SAME AS RAIDE'S ONE: ---
!     --- OFTEN REAL                                            ---
            call mtdefs(dynam, raide, 'V', ' ')
        else
!     --- IF APM TEST, DYNAM'TYPE IS ALWAYS COMPLEX.            ---
            call mtdefs(dynam, matref, 'V', 'C')
        endif
        call jeveuo(dynam(1:19)//'.REFA', 'E', krefa)
        zk24(krefa-1+7)=solveu
        call mtdscr(dynam)
        call jeveuo(dynam(1:19)//'.&INT', 'E', ldynam)
    endif
!
!     --- COMPUTATION OF THE LAGRANGE MULTIPLIERS ---
    if (typmet(1:5) .eq. 'STURM') then
        neq = zi(lraide+2)
        call wkvect('&&OP0032.POSITION.DDL', 'V V I', neq*mxddl, lddl)
        call wkvect('&&OP0032.DDL.BLOQ.CINE', 'V V I', neq, lprod)
        call vpddl(raide, masse, neq, nblagr, nbcine,&
                   neqact, zi(lddl), zi(lprod), ierd)
    endif
!
!-----------------------------------------------------------------------
!-----------------------------STURM METHOD -----------------------------
!-----------------------------------------------------------------------
    nbrow=-9999
    if (typmet(1:5) .eq. 'STURM') then
!
        if (nbmod .lt. 2) ASSERT(.false.)
        nbrow=nbmod-1
!
!     ------------------------------------------------------------------
!     ------- INFO_MODE // SEUL OU DS MACRO_MODE_MECA (PART II)  -------
!     ------------------------------------------------------------------
!     --- SI TYPECO=1 OU 2 ON PASSE EN COM LOCAL + DISTRIBUTION DES
!     ---     TESTS DE STURM + ON REVIENT AU COMM_WORLD.
!     ------------------------------------------------------------------
        if (lcomod .or. lcoinf) then
!         --- CALCUL DU VECTEUR DE COULEURS POUR DETERMINER LES SOUS-
!         --- COMMUNICATEURS ASSOCIES A CHAQUE ANALYSE+FACTO. MUMPS.
!         --- VECTEUR COULEUR ZI(JKPAR+I)= FREQ DONNE LE NUMERO DE LA
!         --- FREQUENCE A TRAITER PAR LE PROC DE RANG I.
!         --- PAR DEFAUT, LA PREMIERE FREQUENCE A LE NUMERO 0.
!         --- * AVEC TYPECO=1,LE PROC 0 TRAITE A LA FOIS FREQ0 ET FREQ1,
!         ---   PUIS LES AUTRES FREQS SONT DISTRIBUEES SUR LES AUTRES
!         ---   PROCS. REGLE PARTICULIERE EN CAS DE DESEQUILIBRAGE CF.
!         ---   COMMENTAIRE PLUS LOIN.
!         --- * AVEC TYPECO=2, PROC 0 TRAITE FREQ0, PROC 1 TRAITE FREQ1.
!         --- REGLE 1: ZI(JKPAR+NBPROC-1)=NBROW (IMPORTANT POUR VPFOPR).
!         --- REGLE 2: ON GARDE CONTIGUES LES PROCS DEDIES A UNE FACTO
!         --- MUMPS ET EN CAS DE DESEQUILIBRAGE DE CHARGE ON DONNE 1
!         --- PROC DE PLUS AUX PREMIERES FREQUENCES (SI TYPECO=1) OU A
!         --- LA PREMIERE (SI TYPECO=2).
            k24par='&&OP0032.COULEUR'
            call wkvect(k24par, 'V V I', nbproc, jkpar)
            call vecint(nbproc, -9999, zi(jkpar))
!         --- CAS PARTICULIER: INFO_MODE INITIAL SUR 1 SEULE SOUS-BANDE.
!         --- IL EST EGAL FONCTIONNELLEMENT A L'INFO_MODE FINAL.
            if ((typeco.eq.1) .and. (nbrow.eq.1)) typeco=2
!         --- ULTIME VERIF (DEJA FAIT PAR AILLEURS NORMALEMENT)
            if ((nbproc.lt.nbrow) .or.&
                ( (nbproc.gt.nbrow) .and. (metres( 1:5).ne.'MUMPS') .and. (typeco.eq.1) )) &
            ASSERT(.false.)
            if (typeco .eq. 1) then
                l1=nbproc/nbrow
                l11=l1+1
                l2=nbproc-l1*nbrow
                l21=l2+1
                l3=l11*l2
                do 40 k = 1, l2
                    call vecint(l11, k, zi(jkpar+(k-1)*l11))
40              continue
                do 41 k = l21, nbrow
                    call vecint(l1, k, zi(jkpar+l3+(k-l21)*l1))
41              continue
            else if (typeco.eq.2) then
                if (nbrow .ne. 1) ASSERT(.false.)
                l1=nbproc/2
                l2=nbproc-2*l1
                l11=l1+l2
                call vecint(l11, 0, zi(jkpar))
                call vecint(l1, 1, zi(jkpar+l11))
                if (l11 .ne. l1) then
                    vali(1)=l11
                    vali(2)=l1
                    call u2mesg('I', 'MODAL_13', 0, kbid, 2,&
                                vali, 0, rbid)
                endif
            endif
!         --- ULTIME VERIF VECTEUR COULEUR
            do 42 k = 1, nbproc
                l1=zi(jkpar+k-1)
                if ((l1.lt.0) .or. (l1.gt.nbrow)) ASSERT(.false.)
42          continue
!
!         --- FREQUENCE COURANTE CAD FREQ A TRAITER PAR LE PROC COURANT
            frecou=zi(jkpar+rang)
!         --- ON AFFECTE UN COMMUNICATEUR LOCAL MPICOU POUR NE PAS
!         --- INTERFERER AVEC LA FACTORISATION NUMERIQUE.
!         --- ON REMET LE COMM_WORLD MPICOW AU SEIN DE VPFOPR.
!         --- ON DETRUIT LE MPICOU QU'APRES LA DESTRUCTION DE L'OCCU
!         --- RENCE MUMPS ASSOCIEE.
            call asmpi_split_comm(mpicow, to_mpi_int(frecou), to_mpi_int(0), 'mumps', mpicou)
            if (mpicow .eq. mpicou) ASSERT(.false.)
            call asmpi_barrier()
            call asmpi_comm('SET', mpicou)
            if (typeco .eq. 1) then
!         --- CALCUL // TYPE 1
                kopt1='STURML1P'
                koptn='STURMLNP'
                if (frecou .eq. 1) then
!         --- LE PROC (ET SES AMIS DU MEME SOUS-COMMUNICATEUR) TRAITE LA
!         --- PREMIERE SOUS-BANDE
                    lfirst=.true.
                    k1=1
                    k2=0
                else
!         --- LE PROC (ET SES AMIS DU MEME SOUS-COMMUNICATEUR) SAUTENT
!         --- LA PREMIERE SOUS-BANDE ET TRAITE LA FREQ FRECOU
                    lfirst=.false.
                    k1=frecou
                    k2=k1
                endif
            else if (typeco.eq.2) then
!         --- CALCUL // TYPE 2
                if (frecou .eq. 0) then
!         --- LE PROC (ET SES AMIS DU MEME SOUS-COMMUNICATEUR) TRAITE
!         --- LA PREMIERE FREQUENCE DE LA PREMIERE SOUS-BANDE
                    kopt1='STURML10'
                else if (frecou.eq.1) then
!         --- LE PROC (ET SES AMIS DU MEME SOUS-COMMUNICATEUR) TRAITE
!         --- LA SECONDE FREQUENCE DE LA PREMIERE SOUS-BANDE
                    kopt1='STURML11'
                endif
                koptn='XXXXXXXX'
                lfirst=.true.
                k1=1
                k2=0
            else
                ASSERT(.false.)
            endif
!
        else
!         --- CALCUL SEQ: LE PROC FAIT LES NBROW CALCULS
            k1=2
            k2=nbrow
            lfirst=.true.
            kopt1='STURML1'
            koptn='STURMLN'
        endif
        npivot(1)=-9999
        npivot(2)=-9999
!
!       --- TO PERFORM A LIST OF BANDES ---
!       --- STEP 1: FIRST BANDE         ---
        if (ldyna) then
            omin=omega2(zr(jlmod))
            omax=omega2(zr(jlmod+1))
        else if (lflamb) then
            omin=zr(jlmod)
            omax=zr(jlmod+1)
        else
            ASSERT(.false.)
        endif
        if (lfirst) then
            call vpfopr(kopt1, typmod, lmasse, lraide, ldynam,&
                        omin, omax, rbid, zi(jstu), npivot,&
                        omecor, precsh, nbrss, nblagr, solveu,&
                        det, idet)
!          --- WE STORE THE POSSIBLY CORRECTED FREQUENCY/BUCKLING MODE
            if (ldyna) then
                zr(jlmoe)=freqom(omin)
                zr(jlmoe+1)=freqom(omax)
            else
                zr(jlmoe)=omin
                zr(jlmoe+1)=omax
            endif
        endif
        do 20 k = k1, k2
!        --- STEP K: BANDE NUMBER K
            if (ldyna) then
                omin=omega2(zr(jlmod+k-1))
                omax=omega2(zr(jlmod+k))
            else
                omin=zr(jlmod+k-1)
                omax=zr(jlmod+k)
            endif
            npivot(1)=npivot(2)
            npivot(2)=k
            call vpfopr(koptn, typmod, lmasse, lraide, ldynam,&
                        omin, omax, rbid, zi(jstu+k-1), npivot,&
                        omecor, precsh, nbrss, nblagr, solveu,&
                        det, idet)
            if (ldyna) then
                zr(jlmoe+k)=freqom(omax)
            else
                zr(jlmoe+k)=omax
            endif
20      continue
!
!     ------------------------------------------------------------------
!     ------ INFO_MODE // SEUL OU DS MACRO_MODE_MECA (PART III)  -------
!     ------------------------------------------------------------------
!     --- SI TYPECO=1/2 ON COMMUNIQUE TOUS LES RESULTATS DES CALCULS.
!     ------------------------------------------------------------------
        if (lcomod .or. lcoinf) then
            call asmpi_comm_vect('MPI_SUM', 'I', nbmod-1, ibid, zi(jstu),&
                                 rbid, cbid)
            call asmpi_comm_vect('MPI_SUM', 'R', nbmod, ibid, ibid,&
                                 zr(jlmoe), cbid)
            call jedetr(k24par)
        endif
!
!-----------------------------------------------------------------------
!------------------------ ARGUMENT PRINCIPAL METHOD --------------------
!-----------------------------------------------------------------------
!   --- COMBO OF THE WORK OF H.J.JUNG (HYUNDAI)/O.BERTRAND (PHD INRIA)
    else if (typmet(1:3).eq.'APM') then
!
        nbrow=1
!   --- VALUE TO START SOME SELF-TESTING PROCEDURES: ONLY FOR ---
!   --- DEVELOPPERS AND FOR DEBBUGING PHASE                   ---
        ltest=.false.
        itest=0
!       LTEST=.TRUE.
!
!   --- FOR PRINT IN THE FILE IFAPM THE DISPLAY Z/ARG(PC(Z)) ONLY  ---
!   --- FOR DEBUGGING ISSUES                                       ---
        ifapm=18
        impr='OUI'
        impr='NON'
!
!   --- FOR TEST ISSUE ONLY (SEE APM012/APTEST)---
        if ((ltest) .and. (typcha(1:7).eq.'ROMBOUT')) then
            if (itest .eq. 3) then
                nk=20
            else if (itest.eq.4) then
                nk=10
            else if (itest.eq.1) then
                nk=4
            else if (itest.eq.0) then
                nk=2
            endif
        else
            nk=zi(lmasse+2)
        endif
!   --- STEPS 0/1/2 OF THE APM ALGORITHM IF WE USE ROMBOUT VARIANT ---
        if (typcha(1:7) .eq. 'ROMBOUT') call apm012(nk, k24rc, ltest, itest, rayonc,&
                                                    centrc, lraide, lmasse, solveu)
!
!   --- STEPS 3, 4 AND 5 OF THE APM ALGORITHM
!   --- ITERATION LOOP TO DETERMINE THE STABILIZED NUMBER OF   ---
!   --- EIGENVALUES. TRICKS TO LIMIT THE NUMBER OF COMPUTATION ---
!   --- WITH THE PARAMETERS MITERC AND NMULTC
        nbtet0=min(miterc,max(1,nbtetc/nmultc))
        nbtet1=min(miterc,nbtetc)
        nbtet2=min(miterc,nbtetc*nmultc)
        pivot1=0
        pivot2=-9999
        nbev0=0
        nbev1=0
        nbev2=0
        do 30 ii = 1, niterc
            if (ii .eq. 1) then
                if (impr .eq. 'NON') call apm345(nbtet0, typcon, rayonc, centrc, nk,&
                                                 k24rc, nbev0, ltest, typcha, lraide,&
                                                 lmasse, ldynam, solveu, lamor, lc,&
                                                 impr, ifapm)
                call apm345(nbtet1, typcon, rayonc, centrc, nk,&
                            k24rc, nbev1, ltest, typcha, lraide,&
                            lmasse, ldynam, solveu, lamor, lc,&
                            impr, ifapm)
            endif
            if (impr .eq. 'NON') call apm345(nbtet2, typcon, rayonc, centrc, nk,&
                                             k24rc, nbev2, ltest, typcha, lraide,&
                                             lmasse, ldynam, solveu, lamor, lc,&
                                             impr, ifapm)
!
            write(ifm,4000)nbtet0,nbtet1,nbtet2,nbev0,nbev1,nbev2
!
!
!   --- SHIFT OF THE THREE LEVELS OF DISCRETISATIONS ---
!   --- TO CONTINUE THE HEURISTIC                    ---
            if (((nbev0.ne.nbev1).or.(nbev1.ne.nbev2)) .and. ( impr.eq.'NON')) then
                nbtet0=nbtet1
                nbtet1=nbtet2
                nbtet2=nmultc*nbtet2
!
                nbev0=nbev1
                nbev1=nbev2
!
!   --- ERROR MESSAGES
                if (nbtet2 .gt. miterc) call u2mesi('F', 'ALGELINE4_13', 1, miterc)
                if (ii .eq. niterc) call u2mesi('F', 'ALGELINE4_14', 1, niterc)
!
            else if (impr.eq.'NON') then
!    --- THE HEURISTIC CONVERGES
                pivot2=nbev1
                write(ifm,4010)
                goto 31
            else if (impr.eq.'OUI') then
                write(ifm,4020)
                ASSERT(.false.)
            endif
30      continue
31      continue
        if (typcha(1:7) .eq. 'ROMBOUT') call jedetr(k24rc)
        if (pivot2 .lt. 0) call u2mess('F', 'ALGELINE4_22')
!
    else
!   --- ILLEGAL OPTION ---
        ASSERT(.false.)
    endif
!
!-----------------------------------------------------------------------
!-------------------------- POSTTRAITEMENTS ----------------------------
!-----------------------------------------------------------------------
!
!   --- DESTRUCTION OF THE DYNAMIC MATRIX
    if ((typmet(1:5).eq.'STURM') .or. ((typmet(1:3).eq.'APM').and.(typcha(1:4).eq.'LDLT'))) &
    call detrsd('MATR_ASSE', dynam)
!
!     ------------------------------------------------------------------
!     ------- INFO_MODE // SEUL OU DS MACRO_MODE_MECA (PART IV)  -------
!     ------------------------------------------------------------------
!     --- AVANT DE QUITTER L'OP. ON REMET LE COM WORLD (AU CAS OU)
!     --- DESTRUCTION DES SOUS-COMMUNICATEURS EVENTUELLEMENT ASSOCIES A
!     --- UNE OCCURENCE MUMPS (APRES CELLE DE LADITE OCCURENCE)
!     ------------------------------------------------------------------
    if (lcomod .or. lcoinf) then
        call asmpi_comm('SET', mpicow)
        call asmpi_barrier()
        call asmpi_comm('FREE', mpicou)
    endif
!
!   --- PRINT THE RESULTS TO THE MSG FILE AND SAVE  THE EVALUATED ---
!   --- NUMBER OF FREQUENCIES AS WELL AS THE CALCULATION PARAMS   ---
!   --- TO AN SD_TABLE                                            ---
!
    if (typmet(1:3) .eq. 'APM') then
        typep='C'
        if (typcon(1:6) .eq. 'CERCLE') then
            dimc1=rayonc
            zimc1=centrc
        endif
        call vpecst(ifm, typmod, omgmin, omgmax, pivot1,&
                    pivot2, nbfreq, nblagr, typep, typcon,&
                    dimc1, zimc1)
    endif
!
    call tbcrsd(table, 'G')
    call titre()
!
!   --- BUILDING OF THE DATA STRUCTURE CARTE  ---
    nmparn(1) = 'NB_MODE'
    nmparr(1) = 'FREQ_MIN'
    nmparr(2) = 'FREQ_MAX'
    nmparc(1) = 'CENTRE_R'
    nmparc(2) = 'CENTRE_I'
    nmparc(3) = 'RAYON'
    nmparf(1) = 'CHAR_CRIT_MIN'
    nmparf(2) = 'CHAR_CRIT_MAX'
    nmparm(1) = 'BORNE_MIN_EFFECT'
    nmparm(2) = 'BORNE_MAX_EFFECT'
!
    tpparn(1) = 'I'
    tpparr(1) = 'R'
    tpparr(2) = 'R'
    tpparc(1) = 'R'
    tpparc(2) = 'R'
    tpparc(3) = 'R'
    tpparf(1) = 'R'
    tpparf(2) = 'R'
    tpparm(1) = 'R'
    tpparm(2) = 'R'
!
    call tbajpa(table, 2, nmparr, tpparr)
    call tbajpa(table, 3, nmparc, tpparc)
    call tbajpa(table, 2, nmparf, tpparf)
    call tbajpa(table, 2, nmparm, tpparm)
    call tbajpa(table, 1, nmparn, tpparn)
!
!     --- FIRST ROW (TITLE OF THE COLUMNS) OF THE ASTER TABLE ---
!     --- NUMERICAL VALUES OF THE OTHERS ROWS ---
!
    if (typmod(1:9) .eq. 'DYNAMIQUE') then
        do 50 k = 1, nbrow
            call tbajli(table, 1, nmparn, zi(jstu+k-1), rbid,&
                        cbid, kbid, 0)
            calpar(1)=zr(jlmod+k-1)
            calpar(2)=zr(jlmod+k)
            call tbajli(table, 2, nmparr, ibid, calpar,&
                        cbid, kbid, k)
            calpar(1)=zr(jlmoe+k-1)
            calpar(2)=zr(jlmoe+k)
            call tbajli(table, 2, nmparm, ibid, calpar,&
                        cbid, kbid, k)
50      continue
!
    else if (typmod(1:13).eq.'MODE_COMPLEXE') then
        call tbajli(table, 1, nmparn, nbfreq, rbid,&
                    cbid, kbid, 0)
        call tbajli(table, 3, nmparc, ibid, calpac,&
                    cbid, kbid, 1)
!
    else if (typmod(1:10).eq.'MODE_FLAMB') then
        do 55 k = 1, nbrow
            call tbajli(table, 1, nmparn, zi(jstu+k-1), rbid,&
                        cbid, kbid, 0)
            calpaf(1)=zr(jlmod+k-1)
            calpaf(2)=zr(jlmod+k)
            call tbajli(table, 2, nmparf, ibid, calpaf,&
                        cbid, kbid, k)
            calpaf(1)=zr(jlmoe+k-1)
            calpaf(2)=zr(jlmoe+k)
            call tbajli(table, 2, nmparm, ibid, calpaf,&
                        cbid, kbid, k)
55      continue
!
    else
        ASSERT(.false.)
    endif
!
!  ---- DESTRUCTION OF THE TEMPORARY DATA STRUCTURES ---
    if (typmod .ne. 'MODE_COMPLEXE') then
        call jedetr(k24mod)
        call jedetr(k24moe)
        call jedetr(k24stu)
    endif
    call jedema()
!
!-----------------------------------------------------------------------
!-------------------------- FORTRAN PRINT FORMAT -----------------------
!-----------------------------------------------------------------------
    4000 format('(METHODE APM) POUR LES 3 NIVEAUX DE DISCRETISATION ',&
     &       'SUIVANTS',/,&
     &       ' --- ',i5,' --- ',i5,' --- ',i5,' ---',/,&
     &       ' NOMBRE DE VALEURS PROPRES DETECTEES ',/,&
     &       ' --- ',i5,' --- ',i5,' --- ',i5,' ---')
    4010 format('(METHODE APM) CONVERGENCE DE L''HEURISTIQUE ')
    4020 format('(METHODE APM) ATTENTION CALCUL DE TEST POUR IMPRIMER LA',&
     &       ' COURBE DES NOMBRES DE TOURS ')
!
end subroutine
