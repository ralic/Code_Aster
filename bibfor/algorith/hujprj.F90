subroutine hujprj(k, tin, toud, p, q)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
!  ------------------------------------------------------
!  LOI DE HUJEUX: PROJECTION DANS LE PLAN DEVIATEUR K
!  IN  K        :  DIRECTION K=1 A 3
!      TIN( )   :  TENSEUR A PROJETER (NDIM), EGAL A:
!                     - TENSEUR DES CONTRAINTES DE CAUCHY
!                     - TENSEUR DES DEFORMATIONS
!
!  OUT
!      TOUD  :  DEVIATEUR K (NDIM/2)
!      P     :  COMPOSANTE ISOTROPE K
!      Q     :  NORME DEVIATEUR K
!  ------------------------------------------------------
    integer :: ndt, ndi, i, j, k
    real(kind=8) :: d12, dd, deux
    real(kind=8) :: tin(6), tou(3), toud(3), p, q
!
    common /tdim/ ndt  , ndi
!
    data   d12, deux /0.5d0, 2.d0/
!
    j = 1
    do 10 i = 1, ndi
        if (i .ne. k) then
            tou(j) = tin(i)
            j = j+1
        endif
10  continue
!
    tou(3) = tin(ndt+1-k)
!
    dd = d12*( tou(1)-tou(2) )
    toud(1) = dd
    toud(2) = -dd
    toud(3) = tou(3)
!
    p = d12*( tou(1)+tou(2) )
    q = dd**deux + ((tou(3))**deux)/deux
    q = sqrt(q)
!
end subroutine
