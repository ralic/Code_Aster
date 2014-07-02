subroutine modthm(modint)
    implicit none
#include "asterf_types.h"
#include "asterfort/lteatt.h"
#include "asterfort/teattr.h"
    character(len=3), intent(out) :: modint
! =====================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
! =====================================================================
! --- determination du mode d'integration : CLA/RED/LUM
!     pour le type_element courant.
! =====================================================================
    aster_logical :: lprincip
    integer :: iret
    character(len=1) :: d1, d2
    character(len=3) :: mint
! =====================================================================
!
!   -- l'element est-il principal ?
    call teattr('S', 'DIM_TOPO_MODELI', d1, iret)
    call teattr('S', 'DIM_TOPO_MAILLE', d2, iret)
    lprincip=(d1.eq.d2)
!
    modint='CLA'
    if (lprincip) then
        call teattr('C', 'INTTHM', mint, iret)
        if (iret .eq. 0) modint=mint
    endif
! =====================================================================
end subroutine
