subroutine mdlibe(nomres, nbnli)
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: hassan.berro at edf.fr
!
!     Memory clearup of resulting vectors in a DYNA_VIBRA//TRAN/GENE calculation
!     with a non-constant integration step
!     ------------------------------------------------------------------
! in  : nomres : result name (usually &&AD****)
! in  : nbnli  : number of localised non linearities
! ----------------------------------------------------------------------
!
#include "asterfort/jelibe.h"
#include "asterfort/jeexin.h"
!   Input arguments
    character(len=8), intent(in) :: nomres
    integer,          intent(in) :: nbnli
!   Local variables
    integer           :: iret
    character(len=9)  :: bl8pt
    character(len=12) :: bl11pt
!-----------------------------------------------------------------------
    bl11pt = '           .'
    bl8pt  = '        .'

    call jelibe(nomres//bl11pt//'DEPL')
    call jelibe(nomres//bl11pt//'VITE')
    call jelibe(nomres//bl11pt//'ACCE')
    call jelibe(nomres//bl11pt//'ORDR')
    call jelibe(nomres//bl11pt//'DISC')
    call jelibe(nomres//bl11pt//'PTEM')

    if (nbnli .gt. 0 ) then
        call jelibe(nomres//bl8pt//'NL.TYPE')
        call jelibe(nomres//bl8pt//'NL.INTI')
        call jelibe(nomres//bl8pt//'NL.VIND')
        call jeexin(nomres//bl8pt//'NL.VINT', iret)
        if (iret.gt.0) call jelibe(nomres//bl8pt//'NL.VINT')
    end if
!
end subroutine
