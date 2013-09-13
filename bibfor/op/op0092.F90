subroutine op0092()
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
!
!     OPERATEUR :   CALC_CHAR_SEISME
!
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/simono.h"
#include "asterfort/simult.h"
#include "asterfort/titre.h"
#include "asterfort/u2mess.h"
    character(len=8) :: monmot
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: nbd, nbdir, nbv
!-----------------------------------------------------------------------
    call infmaj()
!
    call getvr8(' ', 'DIRECTION', nbval=0, nbret=nbd)
    nbdir = -nbd
    if (nbdir .ne. 3 .and. nbdir .ne. 6) then
        call u2mess('F', 'ALGELINE2_76')
    endif
!
!     SEISME ????
!
    monmot = ' '
    call getvtx(' ', 'MONO_APPUI', scal=monmot, nbret=nbv)
    if (monmot(1:3) .eq. 'OUI') then
!
!        --- SEISME MONO-APPUI ---
        call simono()
    else
!
!        --- SEISME MULT-APPUI ---
        call simult()
    endif
!
    call titre()
!
end subroutine
