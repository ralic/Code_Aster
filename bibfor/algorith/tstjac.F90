subroutine tstjac(np1, n, typj, kmod, kmod0)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
! DESCRIPTION : TESTE LA VARIATION DE LA MATRICE DE RAIDEUR
! -----------
!               APPELANT : CALCMD
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterfort/u2mess.h"
    integer :: np1, n, typj
    real(kind=8) :: kmod(np1, *), kmod0(np1, *)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, j
    real(kind=8) :: sup, sup1, val, max, tol
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  ABS
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    typj = 0
    sup = 0.0d0
    sup1 = 0.0d0
    tol = 1.0d-03
    max = 0.0d0
!
    do 10 j = 1, n
        do 11 i = 1, n
            sup1 = abs(kmod0(i,j))
            if (sup1 .gt. sup) sup = sup1
11      continue
10  end do
!
!
    if (sup .eq. 0.d0) then
        call u2mess('I', 'ALGORITH11_1')
        do 20 j = 1, n
            do 21 i = 1, n
                sup1 = abs(kmod(i,j))
                if (sup1 .gt. sup) sup = sup1
21          continue
20      continue
    endif
!
!
    if (sup .eq. 0.d0) call u2mess('F', 'ALGORITH11_2')
!
!
    do 30 j = 1, n
        do 31 i = 1, n
            val = abs(kmod(i,j) - kmod0(i,j)) / sup
            if (val .gt. max) max = val
31      continue
30  end do
!
    if (max .ge. tol) typj = 1
!
! --- FIN DE TSTJAC.
end subroutine
