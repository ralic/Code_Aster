subroutine ptenpo(n, x, mat, ep, itype, iform)
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
!
! --------------------------------------------------------------------------------------------------
!
!     CALCUL ENERGIE DE DEFORMATION POUR
!         - ELEMENT DE POUTRE (POU_D_T, POU_D_E)
!         - ELEMENT DISCRET
!         - ELEMENT BARRE
!
! --------------------------------------------------------------------------------------------------
!
! IN  N      :  DIMENSION DE LA MATRICE MAT
! IN  X      :  VECTEUR DE DEPLACEMENT
! IN  MAT    :  MATRICE DE RAIDEUR
! OUT EP     :  ENERGIE DE DEFORMATION
! IN  ITYPE  :  TYPE DE LA SECTION
! IN  IFORM  :
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
    integer :: n, itype, iform
    real(kind=8) :: x(*), mat(n, n), ep(*)
#include "asterfort/vtmv.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jcft(8), ncft(3), icft(6, 3), na(4), ia(4, 4)
    real(kind=8) :: x2(12), mat2(144)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i, j, kk, l, nn
    real(kind=8) :: r, zero
!
! --------------------------------------------------------------------------------------------------
!
    data jcft/  2 ,  3 ,  5 ,  6 ,  8 ,  9 , 11 , 12 /
    data ncft/  2 ,  6 ,  6 /
    data icft/  1 ,  7 ,  0 ,  0 ,  0 ,  0 , &
                2 ,  4 ,  6 ,  8 , 10 , 12 ,&
                3 ,  4 ,  5 ,  9 , 10 , 11 /
!
!   element droit classique
    data na/  2 ,  2 ,  4 ,  4  /
    data ia/  1 ,  7 ,  0 ,  0 ,&
              4 , 10 ,  0 ,  0 ,&
              2 ,  6 ,  8 , 12 ,&
              3 ,  5 ,  9 , 11  /
!
! --------------------------------------------------------------------------------------------------
!
!
    zero = 0.d0
!
!   energie potentielle globale
    call vtmv(n, x, mat, r)
    ep(1) = r / 2.0d0
    if (iform .eq. 0) goto 900
    if (abs(ep(1)) .lt. 1.d-06) goto 900
! --------------------------------------------------------------------------------------------------
!   Repartition d'energie
    nn = 0
    if (itype .eq. 0 .or. itype .eq. 1 .or. itype .eq. 2) then
!       element droit de section constante ou variable
        nn = 4
        do kk = 1, 8
            if (mat( 4,jcft(kk)).ne.zero .or. mat(10,jcft(kk)).ne.zero) then
!               couplage flexion-torsion
                do l = 1, 3
                    do i = 1, ncft(l)
                        x2(i) = x(icft(i,l))
                        do j = 1, ncft(l)
                            mat2(ncft(l)*(j-1)+i) = mat(icft(i,l), icft(j,l) )
                        enddo
                    enddo
                    call vtmv(ncft(l), x2, mat2, r)
                    ep(1+l) = r / 2.0d0
                enddo
                iform= 101
                goto 900
            endif
        enddo
!       element droit classique
        do l = 1, 4
            do i = 1, na(l)
                x2(i) = x(ia(i,l))
                do j = 1, na(l)
                    mat2(na(l)*(j-1)+i) = mat ( ia(i,l) , ia(j,l) )
                enddo
            enddo
            call vtmv(na(l), x2, mat2, r)
            ep(1+l) = r / 2.0d0
        enddo
!
    else if (itype .eq. 20 .or. itype.eq.21) then
!       element discret type nodal
        nn = n
        do i = 2, n
            do j = 1, i-1
                if (mat(i,j).ne.zero) goto 900
            enddo
        enddo
        do i = 1, n
            ep(1+i) = x(i) * mat(i,i) * x(i) / 2.0d0
        enddo
!
    else if (itype .eq. 22 .or. itype.eq.23) then
!       element discret type nodal
        nn = n
        do i = 1, n
            ep(1+i) = x(i) * mat(i,i) * x(i) / 2.0d0
        enddo
!
    else if (itype .eq. 40 .or. itype.eq.41) then
!       element discret type liaison
        nn = n / 2
        do i = 2, nn
            do j = 1, i-1
                if (mat(i,j).ne.zero .or. mat(i,j+nn).ne.zero .or. mat(i+nn,j+nn).ne.zero) goto 900
            enddo
        enddo
        do i = 1, nn
            ep(1+i) = (x(i)*mat(i,i)*x(i) + 2.0d0*x(i)*mat(i,i+nn)*x(i+nn) + &
                       x(i+nn)*mat(i+nn,i+nn)*x(i+nn))/2.0d0
        enddo
!
    else if (itype .eq. 42 .or. itype.eq.43) then
!       element discret type liaison
        nn = n / 2
        do i = 1, nn
            ep(1+i) = (x(i)*mat(i,i)*x(i) + x(i)*mat(i,i+nn)*x(i+nn) + &
                       x(i+nn)*mat(i+nn,i)*x(i) + x(i+nn)*mat(i+nn,i+nn)*x(i+nn))/2.0d0
        enddo
    endif
!
!   pourcentage
    do i = 2, nn+1
        ep(i) = ep(i)/ep(1)
    enddo
!
900  continue
end subroutine
