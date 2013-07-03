subroutine shaksg(k, r)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!--------------------------------------------------------
! ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
!-------------------------------------------------------
! TOURNE KSTAB VERS LE REPERE GLOBAL
    implicit none
#include "asterfort/mulmat.h"
#include "asterfort/r8inir.h"
#include "blas/dcopy.h"
    real(kind=8) :: k(24, 24), r(3, 3), krt(24, 24), tmp(24, 24), kr(24, 24)
    integer :: i, j, kk
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      CALL ZDANUL(KRT,576)
!      CALL ZDANUL(KR,576)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call r8inir(576, 0.d0, krt, 1)
    call r8inir(576, 0.d0, kr, 1)
    do 30 kk = 1, 8
        do 20 j = 1, 3
            do 10 i = 1, 3
!CCCCCC ATTENTION< INVERSE LE 3 NOVEMBRE 2001
                kr((kk-1)*3+i, (kk-1)*3+j) = r(i,j)
                krt((kk-1)*3+i, (kk-1)*3+j) = r(j,i)
10          continue
20      continue
30  end do
    call mulmat(24, 24, 24, k, kr,&
                tmp)
    call mulmat(24, 24, 24, krt, tmp,&
                kr)
!      CALL SHIFTD(KR,K,576)
    call dcopy(576, kr, 1, k, 1)
!
end subroutine
