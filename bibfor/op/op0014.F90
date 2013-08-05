subroutine op0014()
    implicit none
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     OPERATEUR FACTORISER
!     BUT: - FACTORISE UNE MATRICE ASSEMBLEE EN 2 MATRICES TRIANGULAIRES
!            (SOLVEUR MUMPS,MULT_FRONT,LDLT), OU
!          - DETERMINE UNE MATRICE DE PRECONDITIONNEMENT POUR L'ALGO DU
!            GRADIENT CONJUGUE PRCONDITIONNE (SOLVEUR GCPC)
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/apetsc.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/gcncon.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtcopy.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdsc2.h"
#include "asterfort/mtdscr.h"
#include "asterfort/mtexis.h"
#include "asterfort/pcldlt.h"
#include "asterfort/pcmump.h"
#include "asterfort/titre.h"
#include "asterfort/tldlgg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/uttcpu.h"
#include "asterfort/vrrefe.h"
    character(len=3) :: kstop
    character(len=4) :: klag2
    character(len=24) :: valk(2)
    character(len=8) :: matass, matfac, type, ktypr, ktyps, precon, mixpre
    character(len=12) :: kooc
    character(len=16) :: concep, nomcmd, metres
    character(len=19) :: mass, mfac, solveu, solvbd
    integer :: nprec, iatfac, ibdeb, ibfin, ibid, ier1, ifm, ildeb, ilfin
    integer :: iret, iretgc, isingu, istop, jadia, pcpiv, niremp
    integer :: ldtblo, lfnblo, ndeci, neq, niv, npvneg
    integer :: jslvk, jslvr, jslvi, reacpr
    real(kind=8) :: rbid, fillin, epsmat, eps
    integer :: iarg
!     ------------------------------------------------------------------
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(matfac, concep, nomcmd)
    mfac = matfac
    call getvid('  ', 'MATR_ASSE', 0, iarg, 1,&
                matass, ibid)
    mass = matass
    call dismoi('F', 'METH_RESO', mass, 'MATR_ASSE', ibid,&
                metres, ibid)
!
    if (metres .eq. 'GCPC' .or. metres .eq. 'PETSC') then
        call uttcpu('CPU.RESO.1', 'DEBUT', ' ')
        call uttcpu('CPU.RESO.4', 'DEBUT', ' ')
    endif
!
!
!     CAS DU SOLVEUR  GCPC :
!     ---------------------
    if (metres .eq. 'GCPC') then
!        VERIFICATION : CONCEPT REENTRANT INTERDIT
        call exisd('MATR_ASSE', matfac, iret)
        if (iret .eq. 1) then
            call u2mess('F', 'ALGELINE5_56')
        endif
!        VERIFICATION : MATR_ASSE A VALEURS COMPLEXES INTERDIT
        if (concep(16:16) .eq. 'C') then
            call u2mess('F', 'ALGELINE5_57')
        endif
!
        call getvtx(' ', 'PRE_COND', 0, iarg, 1,&
                    precon, ibid)
!
        if (precon .eq. 'LDLT_INC') then
!          ON ECRIT DANS LA SD SOLVEUR LE TYPE DE PRECONDITIONNEU
            call dismoi('F', 'SOLVEUR', mass, 'MATR_ASSE', ibid,&
                        solveu, ier1)
            call jeveuo(solveu//'.SLVK', 'E', jslvk)
            zk24(jslvk-1+2) = precon
!
            call getvis(' ', 'NIVE_REMPLISSAGE', 0, iarg, 1,&
                        niremp, iret)
            call pcldlt(mfac, mass, niremp, 'G')
        else if (precon.eq.'LDLT_SP') then
!          OBLIGATOIRE POUR AVOIR UN CONCEPT DE SORTIE SD_VERI OK
            if (mass .ne. mfac) call copisd('MATR_ASSE', 'G', mass, mfac)
!          ON EST OBLIGE DE MODIFIER DIRECTEMENT MASS
            mfac=mass
!          CREATION D'UN NOM UNIQUE POUR LA SD SOLVEUR MUMPS
!          SIMPLE PRECISION
            call gcncon('.', solvbd)
!          LECTURE PARAMETRE
            call getvis(' ', 'REAC_PRECOND', 0, iarg, 1,&
                        reacpr, ibid)
            call getvis(' ', 'PCENT_PIVOT', 0, iarg, 1,&
                        pcpiv, ibid)
!
!      --- ON REMPLIT LA SD_SOLVEUR GCPC
            call dismoi('F', 'SOLVEUR', mass, 'MATR_ASSE', ibid,&
                        solveu, ier1)
            call jeveuo(solveu//'.SLVK', 'E', jslvk)
            call jeveuo(solveu//'.SLVR', 'E', jslvr)
            call jeveuo(solveu//'.SLVI', 'E', jslvi)
            zk24(jslvk-1+1) = 'GCPC'
            zk24(jslvk-1+2) = precon
            zk24(jslvk-1+3) = solvbd
            zi(jslvi-1+5) = 0
            zi(jslvi-1+6) = reacpr
            zi(jslvi-1+7) = pcpiv
!
!      --- APPEL A LA CONSTRUCTION DU PRECONDITIONNEUR
            call pcmump(mfac, solveu, iretgc)
            if (iretgc .ne. 0) then
                call u2mess('F', 'ALGELINE5_76')
            endif
        endif
!
        goto 9999
    endif
!
!
    call getvis('  ', 'NPREC', 0, iarg, 1,&
                nprec, ibid)
    call getvtx('  ', 'STOP_SINGULIER', 0, iarg, 1,&
                kstop, ibid)
    if (kstop .eq. 'OUI') then
        istop = 0
    else if (kstop.eq.'NON') then
        istop = 1
    endif
!
!
!
!     CAS DU SOLVEUR MUMPS :
!     ----------------------
    if (metres .eq. 'MUMPS') then
        kooc='AUTO'
        mixpre='NON'
        epsmat=-1.d0
        eps=-1.d0
        if (mass .ne. mfac) call copisd('MATR_ASSE', 'G', mass, mfac)
        call dismoi('F', 'SOLVEUR', mass, 'MATR_ASSE', ibid,&
                    solveu, ier1)
        call getvis(' ', 'PCENT_PIVOT', 1, iarg, 1,&
                    pcpiv, ibid)
        call getvtx(' ', 'TYPE_RESOL', 1, iarg, 1,&
                    ktypr, ibid)
        call getvtx(' ', 'PRETRAITEMENTS', 1, iarg, 1,&
                    ktyps, ibid)
        call getvtx(' ', 'ELIM_LAGR2', 1, iarg, 1,&
                    klag2, ibid)
        call getvtx(' ', 'GESTION_MEMOIRE', 1, iarg, 1,&
                    kooc, ibid)
        call jeveuo(solveu//'.SLVI', 'E', jslvi)
        call jeveuo(solveu//'.SLVK', 'E', jslvk)
        call jeveuo(solveu//'.SLVR', 'E', jslvr)
        zi(jslvi-1+1) =nprec
        zi(jslvi-1+2) =pcpiv
        zi(jslvi-1+3) =istop
        zk24(jslvk-1+2)=ktyps
        zk24(jslvk-1+3)=ktypr
        zk24(jslvk-1+6)=klag2
        zk24(jslvk-1+7)=mixpre
        zk24(jslvk-1+8)='NON'
        zk24(jslvk-1+9)=kooc
        zk24(jslvk-1+10)='XXXX'
        zk24(jslvk-1+11)='XXXX'
        zk24(jslvk-1+12)='XXXX'
        zr(jslvr-1+1) =epsmat
        zr(jslvr-1+2) =eps
        ildeb = 1
        ilfin = 0
    endif
!
!
!     CAS DU SOLVEUR PETSC :
!     ----------------------
    if (metres .eq. 'PETSC') then
!        OBLIGATOIRE POUR AVOIR UN CONCEPT DE SORTIE SD_VERI OK
        if (mass .ne. mfac) call copisd('MATR_ASSE', 'G', mass, mfac)
!        ON EST OBLIGE DE MODIFIER DIRECTEMENT MASS
        mfac=mass
        call dismoi('F', 'SOLVEUR', mass, 'MATR_ASSE', ibid,&
                    solveu, ier1)
        call getvtx(' ', 'PRE_COND', 0, iarg, 1,&
                    precon, ibid)
        call jeveuo(solveu//'.SLVK', 'E', jslvk)
        call jeveuo(solveu//'.SLVR', 'E', jslvr)
        call jeveuo(solveu//'.SLVI', 'E', jslvi)
        zk24(jslvk-1+2) = precon
!
        if (precon .eq. 'LDLT_INC') then
            call getvis(' ', 'NIVE_REMPLISSAGE', 0, iarg, 1,&
                        niremp, ibid)
            call getvr8(' ', 'REMPLISSAGE', 0, iarg, 1,&
                        fillin, ibid)
            zr(jslvr-1+3) = fillin
            zi(jslvi-1+4) = niremp
        else if (precon.eq.'LDLT_SP') then
!          CREATION D'UN NOM UNIQUE POUR LA SD SOLVEUR MUMPS
!          SIMPLE PRECISION
            call gcncon('.', solvbd)
!          LECTURE PARAMETRE
            call getvis(' ', 'REAC_PRECOND', 0, iarg, 1,&
                        reacpr, ibid)
            call getvis(' ', 'PCENT_PIVOT', 0, iarg, 1,&
                        pcpiv, ibid)
            zk24(jslvk-1+3) = solvbd
            zi(jslvi-1+5) = 0
            zi(jslvi-1+6) = reacpr
            zi(jslvi-1+7) = pcpiv
        else
            ASSERT(.false.)
        endif
        call apetsc('DETR_MAT', ' ', mfac, rbid, ' ',&
                    0, ibid, iret)
        call apetsc('PRERES', solveu, mfac, rbid, ' ',&
                    0, ibid, iret)
        iret=0
        goto 9999
    endif
!
!
!     CAS DES SOLVEURS LDLT/MULT_FRONT/MUMPS :
!     -------------------------------------
!
!     --- RECUPERATION DES INDICES DE DEBUT ET FIN DE LA FACTORISATION -
!     - 1) AVEC DDL_XXX
    if (metres .ne. 'MUMPS') then
        ildeb = 1
        ilfin = 0
        call getvis('  ', 'DDL_DEBUT', 0, iarg, 1,&
                    ildeb, ibid)
        call getvis('  ', 'DDL_FIN', 0, iarg, 1,&
                    ilfin, ibid)
!     - 2) AVEC BLOC_XXX
        ibdeb = 1
        ibfin = 0
        call getvis('  ', 'BLOC_DEBUT', 0, iarg, 1,&
                    ibdeb, ldtblo)
        call getvis('  ', 'BLOC_FIN', 0, iarg, 1,&
                    ibfin, lfnblo)
!
!
!     --- EXISTENCE / COMPATIBILITE DES MATRICES ---
        call mtexis(mfac, iret)
        if (iret .ne. 0) then
            call vrrefe(mass, mfac, ier1)
            if (ier1 .ne. 0) then
                valk(1) = matass
                valk(2) = matfac
                call u2mesk('F', 'ALGELINE2_18', 2, valk)
            else if (mfac.ne.mass) then
                if (ildeb .eq. 1 .and. ibdeb .eq. 1) then
                    call mtcopy(mass, mfac, iret)
                    ASSERT(iret.eq.0)
                endif
            endif
        else
            type = ' '
            call mtdefs(mfac, mass, 'GLOBALE', type)
            call mtcopy(mass, mfac, iret)
            ASSERT(iret.eq.0)
        endif
    endif
!
!     --- CHARGEMENT DES DESCRIPTEURS DE LA MATRICE A FACTORISER ---
    call mtdscr(mfac)
    call jeveuo(mfac(1:19)//'.&INT', 'E', iatfac)
    if (iatfac .eq. 0) then
        call u2mesk('F', 'ALGELINE2_19', 1, matfac)
    endif
    call mtdsc2(zk24(zi(iatfac+1)), 'SXDI', 'L', jadia)
!
!     --- NEQ : NOMBRE D'EQUATIONS (ORDRE DE LA MATRICE) ---
    neq = zi(iatfac+2)
!
    if (metres .ne. 'MUMPS') then
!
!     --- VERIFICATION DES ARGUMENTS RELATIF A LA PARTIE A FACTORISER --
!     --- 1) AVEC DDL_XXX
        if (ilfin .lt. ildeb .or. ilfin .gt. neq) ilfin = neq
!
!     --- 2) AVEC BLOC_XXX
        if (ldtblo .ne. 0) then
            if (ibdeb .lt. 1) then
                call u2mess('A', 'ALGELINE2_1')
                ibdeb = 1
            else if (ibdeb.gt.zi(iatfac+13)) then
                call u2mess('F', 'ALGELINE2_20')
            endif
            ildeb = zi(jadia+ibdeb-2) + 1
        endif
        if (lfnblo .ne. 0) then
            if (ibfin .lt. 1) then
                call u2mess('F', 'ALGELINE2_21')
            else if (ibdeb.gt.zi(iatfac+13)) then
                call u2mess('A', 'ALGELINE2_8')
                ibfin = zi(iatfac+13)
            endif
            ilfin = zi(jadia+ibfin-1)
        endif
!
!     --- IMPRESSION SUR LE FICHIER MESSAGE ----------------------------
        if (niv .eq. 2) then
            write(ifm,*)' +++ EXECUTION DE "',nomcmd,'"'
            write(ifm,*)'       NOM DE LA MATRICE ASSEMBLEE  "',matass,'"'
            write(ifm,*)'       NOM DE LA MATRICE FACTORISEE "',matfac,'"'
            if (ildeb .eq. 1 .and. ilfin .eq. neq) then
                write(ifm,*)'     FACTORISATION COMPLETE DEMANDEE'
            else
                write(ifm,*)'     FACTORISATION PARTIELLE DE LA LIGNE',&
     &        ildeb,' A LA LIGNE ',ilfin
            endif
            write(ifm,*)'     NOMBRE TOTAL D''EQUATIONS  ',neq
            write(ifm,*)'     NB. DE CHIFFRES SIGNIF. (NPREC) ',nprec
            write(ifm,*)' +++ -------------------------------------------'
        endif
    endif
!
!
!     ------------------ FACTORISATION EFFECTIVE -------------------
    call tldlgg(istop, iatfac, ildeb, ilfin, nprec,&
                ndeci, isingu, npvneg, iret)
!     --------------------------------------------------------------
!
9999  continue
!
    if (metres .eq. 'GCPC' .or. metres .eq. 'PETSC') then
        call uttcpu('CPU.RESO.1', 'FIN', ' ')
        call uttcpu('CPU.RESO.4', 'FIN', ' ')
    endif
!
    call titre()
!
    call jedema()
end subroutine
