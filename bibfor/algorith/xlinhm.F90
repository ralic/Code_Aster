subroutine xlinhm(elrefp, elref2)
!
    implicit none
!
#   include "asterfort/elref1.h"
    character(len=8) :: elrefp, elref2
!
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
! person_in_charge: samuel.geniaut at edf.fr
!
!          BUT : RECUPERER L'ELEMENT LINEAIRE ASSOCIE A L'ELEMENT
!                QUADRATIQUE HM-XFEM
!
!
! OUT  ELREFP  : NOM DE L'ELEMENT QUADRATIQUE PARENT
! OUT  ELREF2  : NOM DE L'ELEMENT LINEAIRE
!     ------------------------------------------------------------------
!
!     ON RECUPERE L'ELEMENT PARENT PRINCIPAL
    call elref1(elrefp)
!
    if (elrefp .eq. 'QU8') then
        elref2='QU4'
    else if (elrefp.eq.'TR6') then
        elref2='TR3'
    else if (elrefp.eq.'H20') then
        elref2='HE8'
    else if (elrefp.eq.'P15') then
        elref2='PE6'
    else if (elrefp.eq.'P13') then
        elref2='PY5'
    else if (elrefp.eq.'T10') then
        elref2='TE4'
    endif
end subroutine
