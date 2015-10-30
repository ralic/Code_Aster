subroutine mdveri()
    implicit none
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/utmess.h"
    integer :: i, ibid, jref1
    character(len=8) :: nomres, method, amogen, channo
    character(len=8) :: matr1, matr2, basemo
    character(len=24) :: ref1, ref2
    character(len=16) :: typres, nomcmd
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: kf, n1, nagen, nared, nbexc, nm
    character(len=24), pointer :: vref2(:) => null()
!-----------------------------------------------------------------------
    call getres(nomres, typres, nomcmd)
    call getvtx('SCHEMA_TEMPS', 'SCHEMA', iocc=1, scal=method, nbret=n1)
!
    call getvid(' ', 'MATR_AMOR', scal=amogen, nbret=nagen)
    call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=0, nbret=nared)

!     IF (NAGEN.EQ.0 .AND. NARED.EQ.0 .AND. METHOD(1:4).EQ.'ITMI') THEN
!        CALL UTMESS('E','ALGORITH5_68')
!     ENDIF
!
!
    call getfac('EXCIT', nbexc)
    kf = 0
    do i = 1, nbexc
        call getvid('EXCIT', 'VECT_ASSE_GENE', iocc=i, nbval=0, nbret=nm)
        if (nm .ne. 0) then
            kf = kf+1
        endif
    end do
!
!     COHERENCE MATRICES
    call getvid(' ', 'MATR_MASS', scal=matr1, nbret=ibid)
    call getvid(' ', 'MATR_RIGI', scal=matr2, nbret=ibid)
    call jeveuo(matr1//'           .REFA', 'L', jref1)
    call jeveuo(matr2//'           .REFA', 'L', vk24=vref2)
    ref1=zk24(jref1)
    ref2=vref2(1)
    if (ref1(1:8) .ne. ref2(1:8)) then
        call utmess('E', 'ALGORITH5_42')
    endif
!
!     COHERENCE SOUS LE MC EXCIT/VECT_ASSE_GENE ET LES MATRICES
    basemo=ref1(1:8)
    do i = 1, nbexc
        call getvid('EXCIT', 'VECT_ASSE_GENE', iocc=i, nbval=ibid, vect=channo,&
                    nbret=nm)
        if (nm .ne. 0) then
            call jeveuo(channo//'           .REFE', 'L', jref1)
            ref1=zk24(jref1)
            if (ref1(1:8) .ne. basemo) then
                call utmess('E', 'ALGORITH5_42')
            endif
        endif
    end do
!
!
!
end subroutine
