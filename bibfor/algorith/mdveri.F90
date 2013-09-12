subroutine mdveri()
    implicit none
! ----------------------------------------------------------------------
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
!
!     VERIFICATION DE PREMIER NIVEAU
!
!
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: i, nbchoc, nbrede, nbrevi, ibid, jref1, jref2
    real(kind=8) :: r8bid
    character(len=8) :: nomres, method, amogen, k8bid, ouinon, channo
    character(len=8) :: matr1, matr2, basemo
    character(len=24) :: ref1, ref2
    character(len=16) :: typres, nomcmd
    integer :: iarg
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: kf, n1, nagen, nared, nbasfl, nbexc, nm
    integer :: npas, nts
!-----------------------------------------------------------------------
    call getres(nomres, typres, nomcmd)
    call getvtx('SCHEMA_TEMPS', 'SCHEMA', iocc=1, scal=method, nbret=n1)
!
    call getvid(' ', 'MATR_AMOR', scal=amogen, nbret=nagen)
    call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=0, nbret=nared)
    if (nagen .ne. 0 .and. method .eq. 'DEVOGE') then
        call u2mess('E', 'ALGORITH5_67')
    endif
!     IF (NAGEN.EQ.0 .AND. NARED.EQ.0 .AND. METHOD(1:4).EQ.'ITMI') THEN
!        CALL U2MESS('E','ALGORITH5_68')
!     ENDIF
!
    if (method(1:4) .eq. 'ITMI') then
        call getvid('SCHEMA_TEMPS', 'BASE_ELAS_FLUI', iocc=1, nbval=0, nbret=nbasfl)
        if (nbasfl .eq. 0) call u2mess('E', 'ALGORITH5_69')
!
        call getvr8('INCREMENT', 'PAS', iocc=1, nbval=0, nbret=npas)
        if (npas .eq. 0) call u2mess('E', 'ALGORITH5_70')
!
        call getvtx('SCHEMA_TEMPS', 'ETAT_STAT', iocc=1, scal=ouinon, nbret=ibid)
        if (ouinon(1:3) .eq. 'OUI') then
            call getvr8('SCHEMA_TEMPS', 'TS_REG_ETAB', iocc=1, nbval=0, nbret=nts)
            if (nts .eq. 0) call u2mess('E', 'ALGORITH5_71')
        endif
!
        call getvtx('CHOC', 'SOUS_STRUC_1', iocc=1, nbval=0, nbret=n1)
        if (n1 .ne. 0) call u2mess('E', 'ALGORITH5_72')
!
        call getvtx('CHOC', 'NOEUD_2', iocc=1, nbval=0, nbret=n1)
        if (n1 .ne. 0) call u2mess('E', 'ALGORITH5_73')
!
        call getvtx('CHOC', 'SOUS_STRUC_2', iocc=1, nbval=0, nbret=n1)
        if (n1 .ne. 0) call u2mess('E', 'ALGORITH5_74')
    endif
!
    call getfac('RELA_EFFO_DEPL', nbrede)
    if (nbrede .ne. 0) then
        if (method .eq. 'NEWMARK') call u2mesk('E', 'ALGORITH5_75', 1, 'RELA_EFFO_DEPL')
    endif
!
    call getfac('RELA_EFFO_VITE', nbrevi)
    if (nbrevi .ne. 0) then
        if (method .eq. 'NEWMARK') call u2mesk('E', 'ALGORITH5_75', 1, 'RELA_EFFO_VITE')
    endif
!
    call getfac('CHOC', nbchoc)
    if (nbchoc .ne. 0) then
        if (method .eq. 'NEWMARK') call u2mesk('E', 'ALGORITH5_75', 1, 'CHOC')
    endif
!
    call getfac('EXCIT', nbexc)
    kf = 0
    do 20 i = 1, nbexc
        call getvid('EXCIT', 'VECT_ASSE_GENE', iocc=i, nbval=0, nbret=nm)
        if (nm .ne. 0) then
            kf = kf+1
        endif
20  end do
    if (kf .ne. 0 .and. method(1:4) .eq. 'ITMI') then
        call u2mess('E', 'ALGORITH5_78')
    endif
!
!     COHERENCE MATRICES
    call getvid(' ', 'MATR_MASS', scal=matr1, nbret=ibid)
    call getvid(' ', 'MATR_RIGI', scal=matr2, nbret=ibid)
    call jeveuo(matr1//'           .REFA', 'L', jref1)
    call jeveuo(matr2//'           .REFA', 'L', jref2)
    ref1=zk24(jref1)
    ref2=zk24(jref2)
    if (ref1(1:8) .ne. ref2(1:8)) then
        call u2mess('E', 'ALGORITH5_42')
    endif
!
!     COHERENCE SOUS LE MC EXCIT/VECT_ASSE_GENE ET LES MATRICES
    basemo=ref1(1:8)
    do 21 i = 1, nbexc
        call getvid('EXCIT', 'VECT_ASSE_GENE', iocc=i, nbval=ibid, vect=channo,&
                    nbret=nm)
        if (nm .ne. 0) then
            call jeveuo(channo//'           .REFE', 'L', jref1)
            ref1=zk24(jref1)
            if (ref1(1:8) .ne. basemo) then
                call u2mess('E', 'ALGORITH5_42')
            endif
        endif
21  end do
!
!
!
end subroutine
