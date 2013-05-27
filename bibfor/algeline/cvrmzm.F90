subroutine cvrmzm(n, a, lda, b, ldb)
    implicit none
    integer :: n, lda, ldb
    real(kind=8) :: a(lda, *)
    complex(kind=8) :: b(ldb, *)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!-----------------------------------------------------------------------
!   COPIE UNE MATRICE REELLE DANS UNE MATRICE COMPLEXE.
!-----------------------------------------------------------------------
! IN  : N    : DIMENSION DES MATRICES A ET B.
!     : A    : MATRICE REELLE DE DIMENSION N.
!     : LDA  : DIMENSION DE A.
!     : LDB  : DIMENSION DE B.
! OUT : B    : MATRICE COMPLEXE D'ORDRE N CONTENANT UNE COPIE DE A.
!-----------------------------------------------------------------------
    integer :: i, j
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (n .lt. 1) then
        write(6,*)  'THE ORDER OF THE MATRIX MUST BE AT '//&
     &               'LEAST 1 WHILE N = %(I1) IS GIVEN.'
        goto 9000
    endif
!
    if (lda .lt. n) then
        write(6,*)  'THE LEADING DIMENSION OF A MUST BE AT '//&
     &               'LEAST AS LARGE AS N WHILE LDA = %(I1) AND N '//&
     &               '= %(I2) ARE GIVEN.'
        goto 9000
    endif
!
    if (ldb .lt. n) then
        write(6,*)  'THE LEADING DIMENSION OF B MUST BE AT '//&
     &               'LEAST AS LARGE AS N WHILE LDB = %(I1) AND N '//&
     &               '= %(I2) ARE GIVEN.'
        goto 9000
    endif
!       --- A EST COPIEE DANS B
    do 10 j = n, 1, -1
        do 10 i = n, 1, -1
            b(i,j) = dcmplx(a(i,j),0.0d0)
10      continue
!
9000  continue
end subroutine
