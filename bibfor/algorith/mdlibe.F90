subroutine mdlibe(nomres, nbnoli, nbrede, nbrevi)
    implicit none
!
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
! person_in_charge: hassan.berro at edf.fr
!
!     Memory clearup of resulting vectors in a DYNA_VIBRA//TRAN/GENE calculation
!     with a non-constant integration step
!     ------------------------------------------------------------------
! in  : nomres : result name (usually &&AD****)
! in  : nbnoli : number of localised non linearities from the shocks family (CHOC=_F*)
! in  : nbrede : number of force = F(X) relationships
! in  : nbrevi : number of force = F(V) relationships
! ----------------------------------------------------------------------
!
#include "asterfort/jelibe.h"
!   Input arguments
    character(len=8), intent(in) :: nomres
    integer,          intent(in) :: nbnoli
    integer,          intent(in) :: nbrede
    integer,          intent(in) :: nbrevi
!   Local variables
    character(len=12)            :: bl11pt
!-----------------------------------------------------------------------
    bl11pt = '           .'
    call jelibe(nomres//bl11pt//'DEPL')
    call jelibe(nomres//bl11pt//'VITE')
    call jelibe(nomres//bl11pt//'ACCE')
    call jelibe(nomres//bl11pt//'ORDR')
    call jelibe(nomres//bl11pt//'DISC')
    call jelibe(nomres//bl11pt//'PTEM')

    if (nbnoli.gt.0) then
        call jelibe(nomres//bl11pt//'NCHO')
        call jelibe(nomres//bl11pt//'SST')
        call jelibe(nomres//bl11pt//'FCHO')
        call jelibe(nomres//bl11pt//'DLOC')
        call jelibe(nomres//bl11pt//'VCHO')
        call jelibe(nomres//bl11pt//'ICHO')
        call jelibe(nomres//bl11pt//'VINT')
        call jelibe(nomres//bl11pt//'INTI')
    end if

    if (nbrede.gt.0) then
        call jelibe(nomres//bl11pt//'REDC')
        call jelibe(nomres//bl11pt//'REDD')
        call jelibe(nomres//bl11pt//'REDN')
    end if

    if (nbrevi.gt.0) then
        call jelibe(nomres//bl11pt//'REVC')
        call jelibe(nomres//bl11pt//'REVV')
        call jelibe(nomres//bl11pt//'REVN')
    end if
!
end subroutine
