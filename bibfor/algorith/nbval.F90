subroutine nbval(ck, cm, cmat, ndim, lambda,&
                 nb)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!***********************************************************************
!    B. GUIGON   P. RICHARD                    DATE 06/04/92
!-----------------------------------------------------------------------
!  BUT:  < CALCULER LE NOMBRE DE VALEUR PROPRES INFERIEURES A LAMBDA
    implicit none
!          D'UN  PROBLEME HERMITIEN >
!
!   CALCULER LE NOMBRE DE VALEURS PROPRES INFERIEURES A LAMBDA D'UN
!   PROBLEME HERMITIEN
!                     CK*X= L CM*X
!-----------------------------------------------------------------------
!
! CK       /I/: MATRICE RAIDEUR DU PROBLEME
! CM       /I/: MATRICE MASSE DU PROBLEME
! NDIM     /I/: DIMENSION DES MATRICES
! LAMBDA   /I/: VOIR DESCRIPTION DE LA ROUTINE
! NB       /O/: NOMBRE DE VALEURS PROPRES
!
!-----------------------------------------------------------------------
!
    include 'asterfort/trldc.h'
    integer :: ndim, nb
    complex(kind=8) :: ck(*), cm(*), cmat(*)
    real(kind=8) :: lambda
    integer :: i, ipivo
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!      INITIALISATION DE LA MATRICE K-LAMBDA*M
!
!-----------------------------------------------------------------------
    integer :: idiag
!-----------------------------------------------------------------------
    do 10 i = 1, ndim*(ndim+1)/2
        cmat(i)=ck(i)-lambda*cm(i)
10  end do
!
!    FACTORISATION DE LA MATRICE
!
    call trldc(cmat, ndim, ipivo)
!
!    COMPTAGE DU NOMBRE DE TERME NEGATIF SUR LA DIAGONALE DE 'D'
!    DANS LA DECOMPOSTION LDLT DE LA MATRICE
!
    nb=0
    do 20 i = 1, ndim
        idiag = i*(i-1)/2+1
        if (dble(cmat(idiag)) .lt. 0.d0) nb=nb+1
20  end do
!
end subroutine
