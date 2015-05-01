subroutine pmppr(amat, na1, na2, ka, bmat,&
                 nb1, nb2, kb, cmat, nc1,&
                 nc2)
!
    implicit none
!
#include "asterfort/assert.h"
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
    integer, intent(in) :: ka
    integer, intent(in) :: kb
    integer, intent(in) :: na1
    integer, intent(in) :: na2
    integer, intent(in) :: nb1
    integer, intent(in) :: nb2
    integer, intent(in) :: nc1
    integer, intent(in) :: nc2
    real(kind=8), intent(in) :: amat(na1, na2)
    real(kind=8), intent(in) :: bmat(nb1, nb2)
    real(kind=8), intent(out) :: cmat(nc1, nc2)
!
! --------------------------------------------------------------------------------------------------
!
! PRODUIT DE DEUX MATRICES STOCKEE PLEINE AVEC PRISE EN COMPTE
! DE TRANSPOSITION PAR L'INTERMEDIAIRE D'INDICATEUR K
!
! --------------------------------------------------------------------------------------------------
!
! AMAT     /I/: PREMIERE MATRICE
! NA1      /I/: NOMBRE DE LIGNE DE LA PREMIERE MATRICE
! NA2      /I/: NOMBRE DE COLONNE DE LA PREMIERE MATRICE
! KB       /I/: INDICATEUR TRANSPOSITION PREMIERE MATRICE
! BMAT     /I/: DEUXIEME MATRICE
! NB1      /I/: NOMBRE DE LIGNE DE LA DEUXIEME MATRICE
! NB2      /I/: NOMBRE DE COLONNE DE LA DEUXIEME MATRICE
! KB       /I/: INDICATEUR TRANSPOSITION DEUXIEME MATRICE
! CMAT     /I/: MATRICE RESULTAT
! NC1      /I/: NOMBRE DE LIGNE DE LA MATRICE RESULTAT
! NC2      /I/: NOMBRE DE COLONNE DE LA MATRICE RESULTAT
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i, j, k
!
! --------------------------------------------------------------------------------------------------
!
    if (ka .eq. 1 .and. kb .eq. 1) then
        if (na2 .ne. nb1) then
            ASSERT(.false.)
        endif
        if (nc1 .ne. na1 .or. nc2 .ne. nb2) then
            ASSERT(.false.)
        endif
        do i = 1, na1
            do j = 1, nb2
                cmat(i,j)=0.d0
                do k = 1, nb1
                    cmat(i,j)=cmat(i,j)+amat(i,k)*bmat(k,j)
                enddo
            enddo
        enddo
    endif
!
    if (ka .eq. -1 .and. kb .eq. 1) then
        if (na1 .ne. nb1) then
            ASSERT(.false.)
        endif
        if (nc1 .ne. na2 .or. nc2 .ne. nb2) then
            ASSERT(.false.)
        endif
        do i = 1, na2
            do j = 1, nb2
                cmat(i,j)=0.d0
                do k = 1, nb1
                    cmat(i,j)=cmat(i,j)+amat(k,i)*bmat(k,j)
                enddo
            enddo
        enddo
    endif
!
    if (ka .eq. 1 .and. kb .eq. -1) then
        if (na2 .ne. nb2) then
            ASSERT(.false.)
        endif
        if (nc1 .ne. na1 .or. nc2 .ne. nb1) then
            ASSERT(.false.)
        endif
        do i = 1, na1
            do j = 1, nb1
                cmat(i,j)=0.d0
                do k = 1, na2
                    cmat(i,j)=cmat(i,j)+amat(i,k)*bmat(j,k)
                enddo
            enddo
        enddo
    endif
!
    if (ka .eq. -1 .and. kb .eq. -1) then
        if (na1 .ne. nb2) then
            ASSERT(.false.)
        endif
        if (nc1 .ne. na2 .or. nc2 .ne. nb1) then
            ASSERT(.false.)
        endif
        do i = 1, na2
            do j = 1, nb1
                cmat(i,j)=0.d0
                do k = 1, nb2
                    cmat(i,j)=cmat(i,j)+amat(k,i)*bmat(j,k)
                enddo
            enddo
        enddo
    endif
!
end subroutine
