subroutine permr8(tab, shift, nbr)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! TOLE CRS_1404
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    include 'asterfort/assert.h'
    integer :: shift, nbr
    real(kind=8) :: tab(nbr)
!
!-----------------------------------------------------------------------
! PERMUTATION CIRCULAIRE DES ELEMENTS D'UN TABLEAU DE REAL*8
!
! IN : TAB   = TABLEAU A PERMUTER
!      SHIFT = INDICE DU TABLEAU PAR LEQUEL ON SOUHAITE DEBUTER
!      NBR   = NOMBRE D'ELEMENTS DU TABLEAU
!
! OUT : TAB = TABLEAU AVEC PERMUTATION
!
!-----------------------------------------------------------------------
!
    integer :: i
    real(kind=8) :: tampon(nbr)
!
    call assert((shift.ge.1).and.(shift.le.nbr))
!
    do 10 i = 1, nbr
        tampon(i) = tab( mod(i+shift-2,nbr) + 1 )
10  end do
!
    do 20 i = 1, nbr
        tab(i) = tampon(i)
20  end do
!
end subroutine
