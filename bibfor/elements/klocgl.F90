subroutine klocgl(k, r)
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
! TOURNE KSTAB VERS LE REPERE GLOBAL
    implicit none
#include "asterfort/mulmat.h"
#include "asterfort/r8inir.h"
    real(kind=8) :: k(18, 18), r(3, 3), krt(18, 18), tmp(18, 18), kr(18, 18)
    integer :: i, j, kk
!
    call r8inir(324, 0.d0, krt, 1)
    call r8inir(324, 0.d0, kr, 1)
    do 30 kk = 1, 6
        do 20 j = 1, 3
            do 10 i = 1, 3
                kr((kk-1)*3+i,(kk-1)*3+j) = r(i,j)
                krt((kk-1)*3+i,(kk-1)*3+j) = r(j,i)
10          continue
20      continue
30  end do
!
    call mulmat(18, 18, 18, k, kr,&
                tmp)
    call mulmat(18, 18, 18, krt, tmp,&
                k)
!      CALL DCOPY(324,KR,1,K,1)
!
end subroutine
