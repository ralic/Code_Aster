subroutine diaghr(n, a, lda, eval, evec,&
                  ldevec, acopy, rwk, cwk)
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/cvrmzm.h'
    include 'asterfort/diares.h'
    include 'asterfort/diatri.h'
    include 'asterfort/mexthr.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/tridia.h'
    include 'asterfort/zmult.h'
    include 'blas/izamax.h'
    include 'blas/zcopy.h'
    integer :: n, lda, ldevec
    real(kind=8) :: eval(*), rwk(*)
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
!    CALCUL DES VALEURS PROPRES ET DES VECTEURS PROPRES D'UNE MATRICE
!                       COMPLEXE HERMITIENNE.
!-----------------------------------------------------------------------
! IN  : N    : DIMENSION DES MATRICES.
!       A    : MATRICE INITIALE.
! OUT : EVAL : VALEURS PROPRES DE LA MATRICE A.
!       EVEC : VECTEURS PROPRES ASSOCIES AUX VALEURS PROPRES.
! IN  : LDA  : DIMENSION DE LA MATRICE A.
!     :LDEVEC: DIMENSION DE LA MATRICE EVEC.
!      ACOPY : MATRICE DE TRAVAIL.
!      RWK   : VECTEUR DE TRAVAIL.
!      CWK   : VECTEUR DE TRAVAIL.
!-----------------------------------------------------------------------
    complex(kind=8) :: a(lda, *), evec(ldevec, *), acopy(n, *), cwk(*)
    integer :: i, j
    complex(kind=8) :: scale
    real(kind=8) :: dble
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (n .lt. 1) then
!C         WRITE(6,*) 'THE ARGUMENT N = %(I1).  THE '//
!C     &               'ORDER OF THE MATRIX MUST BE AT LEAST 1.'
        call assert(.false.)
    endif
!
    if (lda .lt. n) then
!C          WRITE(6,*) 'THE ARGUMENT LDA = %(I1).  THE '//
!C     &               'LEADING DIMENSION OF THE MATRIX MUST BE AT '//
!C     &               'LEAST EQUAL TO THE ORDER, N = %(I2).'
        call assert(.false.)
    endif
!
    if (ldevec .lt. n) then
!C         WRITE(6,*)  'THE ARGUMENT LDEVEC = %(I1).  THE '//
!C     &               'LEADING DIMENSION OF THE EIGENVECTOR MATRIX '//
!C     &               'MUST BE AT LEAST EQUAL TO THE ORDER, N = '//
!C     &               '%(I2).'
        call assert(.false.)
    endif
!    --- A EST COPIEE DANS ACOPY
    do 10 i = 1, n
        call zcopy(i, a(1, i), 1, acopy(1, i), 1)
10  end do
!
    call mexthr(n, acopy, n)
!
    call r8inir(n*n, 0.0d0, rwk(n+1), 1)
    call r8inir(n, 1.0d0, rwk(n+1), n+1)
!
!   --- REDUCTION EN UNE MATRICE SYMETRIQUE TRIDIAGONALE ---
    call tridia(n, acopy, n, eval, rwk,&
                cwk, cwk(n+1))
!
!   --- CALCUL DES VECTEURS ET DES VALEURS PROPRES ---
    call diatri(n, eval, rwk, .true., rwk(n+1),&
                n)
!
!   --- LES VECTEURS PROPRES SONT STOCKES DANS UNE MATRICE COMPLEXE ---
    call cvrmzm(n, rwk(n+1), n, evec, ldevec)
!
!   --- TRANSFORMATION DES VECTEURS PROPRES ---
    call diares(n, n, acopy, n, cwk,&
                evec, ldevec, cwk(n+1))
!
!   --- NORMALISATION DES VECTEURS PROPRES ---
    do 20 j = 1, n
        scale = evec(izamax(n,evec(1,j),1),j)
        if (dble(scale) .ne. 0.0d0 .or. dimag(scale) .ne. 0.0d0) call zmult(n, 1.0d0/scale,&
                                                                            evec(1, j), 1)
20  end do
!
end subroutine
