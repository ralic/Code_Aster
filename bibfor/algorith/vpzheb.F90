subroutine vpzheb(kl, l, m, a, ia,&
                  intger, zvps, iz, n)
!
!***********************************************************************
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
!***********************************************************************
!     PROCEDURE DIRBAK
!     (CF. WILKINSON & REINSCH LINEAR ALGEBRA PP.347)
!     ETANT DONNES LES M VECTEURS PROPRES (STOCKES EN COLONNES DANS LE
!     TABLEAU ZVPS(N,M)) DE LA MATRICE DE HESSENBERG HH, CONSTRUITE
!     DANS LA ROUTINE VPZHEA ET LES DETAILS DES TRANSFORMATIONS
!     EFFECTUEES PAR VPZHEA CONSERVES DANS LA PARTIE INFERIEURE DE
!     LA MATRICE HH ET DANS LES ELEMENTS K A L DU TABLEAU INTGER(N),
!     CETTE ROUTINE FORME LES VECTEURS PROPRES DE LA MATRICE A
!     ET LES REECRIT DANS LES VECTEURS PROPRES DONNES EN ENTREE.
!
!
! --- DECLARATIONS
!
    implicit none
!
! ARGUMENTS
! ---------
    include 'asterfort/vpztr3.h'
    integer :: ia, iz, kl, l, m, n
    real(kind=8) :: a(ia, n), zvps(iz, m)
    integer :: intger(n)
!
! VARIABLES LOCALES
! -----------------
    real(kind=8) :: x
    integer :: i, ii, j, k, m1
!
!
!***********************************************************************
!                     DEBUT DU CODE EXECUTABLE
!***********************************************************************
!
! --- INITIALISATION ET VERIFICATION DES DONNEES D ENTREE
!
    k = kl
!
    if (m .le. 0) goto 9999
!
! --- FORMATION DES NOUVEAUX VECTEURS PROPRES
!
    if (k+2 .le. l) then
        do 20 j = 1, m
!
            call vpztr3(l-k, ia, a(k+1, k), zvps(k+1, j))
!
20      continue
    endif
!
    k = k + 1
!
! --- INTERVERSION DE CES VECTEURS SI NECESSAIRE
!
    i = l + 1
    if (k .gt. l) goto 9999
!
    do 80 ii = k, l
        i = i - 1
        m1 = intger(i)
        if (m1 .ne. i) then
            do 60 j = 1, m
                x = zvps(m1,j)
                zvps(m1,j) = zvps(i,j)
                zvps(i,j) = x
60          continue
        endif
80  end do
!
9999  continue
end subroutine
