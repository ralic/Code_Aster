subroutine vpzhea(n, k, l, a, ia,&
                  intger)
!
!**********************************************************************
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
!**********************************************************************
!     PROCEDURE DIRHES
!     (CF. WILKINSON & REINSCH LINEAR ALGEBRA PP.346-347)
!     ETANT DONNEE LA MATRICE NON SYMETRIQUE A, STOCKEE DANS LE TABLEAU
!     A(N,N), CETTE ROUTINE REDUIT SOUS LA FORME D UNE MATRICE DE
!     HESSENBERG HH, LA SOUS-MATRICE D ORDRE L-K+1 DEBUTANT A L ELEMENT
!     A(K,K) ET FINISSANT A L ELEMENT A(L,L).
!     CETTE REDUCTION EST REALISEE PAR LA METHODE DIRECTE (AN=NH).
!     LA MATRICE HH EST REECRITE SUR LA MATRICE A, AVEC LES DETAILS
!     CONCERNANT LES TRANSFORMATIONS (N), STOCKES DANS LE TRIANGLE
!     RESTANT SOUS LA MATRICE HH ET DANS LES ELEMENTS K A L DU TABLEAU
!     INTGER(N).
!
! --- DECLARATIONS
!
    implicit none
!
! ARGUMENTS
! ---------
#include "asterfort/vpztr1.h"
#include "asterfort/vpztr2.h"
    integer :: n, k, l, ia
    real(kind=8) :: a(ia, n)
    integer :: intger(n)
!
! VARIABLES LOCALES
! -----------------
    real(kind=8) :: x, y
    integer :: i, j, k1, m
!
!**********************************************************************
!                     DEBUT DU CODE EXECUTABLE
!**********************************************************************
!
    k1 = k + 1
    if (k1 .gt. l) goto 9999
    do 140 j = k1, n
        m = j
        x = 0.0d0
        if (j .gt. l) goto 120
        do 20 i = j, l
            if (abs(a(i,j-1)) .gt. abs(x)) then
                x = a(i,j-1)
                m = i
            endif
20      continue
        intger(j) = m
!
! --- INTERVERSION DES COLONNES ET DES RANGEES DE LA MATRICE A
        if (m .ne. j) then
            do 40 i = k, n
                y = a(m,i)
                a(m,i) = a(j,i)
                a(j,i) = y
40          continue
            do 60 i = 1, l
                y = a(i,m)
                a(i,m) = a(i,j)
                a(i,j) = y
60          continue
        endif
        if (x .ne. 0.0d0 .and. j .lt. l) then
            do 100 i = j + 1, l
                a(i,j-1) = a(i,j-1)/x
100          continue
            call vpztr1(l, l-j, ia, a(1, j+1), a(j+1, j-1),&
                        a(1, j), 1.d0)
        endif
120      continue
        call vpztr2(j-k, ia, a(k+1, k), a(k+1, j))
        if (j .lt. l) then
            call vpztr1(l-j, j-k, ia, a(j+1, k), a(k+1, j),&
                        a(j+1, j), -1.d0)
        endif
140  end do
!
9999  continue
end subroutine
