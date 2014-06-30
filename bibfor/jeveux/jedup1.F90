subroutine jedup1(o1z, base, o2z)
    implicit none
! ======================================================================
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
! ======================================================================
#include "asterfort/jedupo.h"
#include "asterfort/jeexin.h"
    character(len=*) :: o1z, base, o2z
! ----------------------------------------------------------------------
!     RECOPIE L'OBJET JEVEUX "O1" SUR LA BASE "BASE" SOUS LE NOM "O2"
! ----------------------------------------------------------------------
    integer :: iret
    character(len=24) :: o1, o2
!
! DEB ------------------------------------------------------------------
    o1=o1z
    o2=o2z
    call jeexin(o1, iret)
    if (iret .gt. 0) call jedupo(o1, base, o2, .false._1)
end subroutine
