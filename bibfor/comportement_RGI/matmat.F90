subroutine matmat(a, b, nl, nc1, nc2,&
                  c)
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
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!=====================================================================
!  C(NL,NC2)=A(NL,NC1)*B(NC1,NC2)
!  LE PREMIER INDICE EST DE LIGNE, LE DEUXIEME DE COLONNE
!=====================================================================
    implicit none
    integer :: nc1
    integer :: nl
    real(kind=8) :: a(nl, *)
    real(kind=8) :: b(nc1, *), xx
    integer :: nc2, i, j, k
    real(kind=8) :: c(nl, *)
    do i = 1, nl
        do j = 1, nc2
            xx= 0.d0
            do k = 1, nc1
                xx = a(i,k)*b(k,j) + xx
            end do
            c(i,j)=xx
        end do
    end do
end subroutine
