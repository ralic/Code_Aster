function lcnrte(d)
    implicit none
!       ----------------------------------------------------------------
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
!       ----------------------------------------------------------------
!       'NORME' D UN DEVIATEUR  AU SENS DU SECOND INVARIANT DUAL
!       DU TENSEUR (3X3) CORRESPONDANT SOUS FORME VECTEUR (6X1)
!       IN  D      :  DEVIATEUR
!                                           T  1/2
!       OUT LCNRTE :  NORME DE    D = (2/3 D D)
!       ----------------------------------------------------------------
#include "asterfort/lcprsc.h"
    integer :: n, nd
    real(kind=8) :: d(6), p, lcnrte, d23
    common /tdim/   n , nd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    data d23        /.66666666666666d0/
!
    call lcprsc(d, d, p)
    lcnrte = sqrt ( d23 * p )
end function
