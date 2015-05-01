subroutine vecinc(n, s, x, inc)
    implicit none
#include "asterfort/assert.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ----------------------------------------------------------------
!     INITIALISATION DU VECTEUR COMPLEXE   X = S
!     IN  S      :  COMPLEXE POUR INITIALISER
!     IN  N      :  DIMENSION DE X SI INC=1 (SINON LA DIMENSION EST N*INC)
!     IN  INC    :  INCREMENT SUR LES INDICES DE X A INITIALISER
!     OUT X      :  VECTEUR COMPLEXE RESULTAT
!     POUR TOUS LES TYPES DE DONNEES VOIR AUSSI VECINI, VECINT, VECINK
!     ET VECINC.
!     ----------------------------------------------------------------
!   Obligatory arguments
    integer,         intent(in)   :: n
    complex(kind=8), intent(in)   :: s
    complex(kind=8)               :: x(*)
!   Optional argument
    integer, optional, intent(in) :: inc
!   ------------------------------------------------------------------
    integer :: i, inc2, ninc
!   ------------------------------------------------------------------
    ASSERT(n .ge. 1)
    inc2 = 1
    if (present(inc)) then
        ASSERT(inc .ge. 1)
        inc2 = inc
    endif
    ninc = n*inc2
    do i = 1, ninc, inc2
        x(i) = s
    end do
!
end subroutine
