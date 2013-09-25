subroutine prmama(iprod, amat, na, na1, na2,&
                  bmat, nb, nb1, nb2, cmat,&
                  nc, nc1, nc2, ier)
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
! DESCRIPTION : PRODUITS DE MATRICES PLEINES RECTANGULAIRES
! -----------                                                  T
!               IPROD = 1 : C = A * B     IPROD = 2 : C = A * B
!
!                                T                         T   T
!               IPROD = 3 : C = A * B     IPROD = 4 : C = A * B
!
!               APPELANTS : CALCMD, PRBRD1, PRBRD2, PRLGMA, PROJMD
!
! IN  : IPROD : INTEGER , SCALAIRE
!               INDICATEUR DU PRODUIT A EFFECTUER
! IN  : AMAT  : REAL*8 , TABLEAU DE DIMENSION (NA,*)
!               MATRICE A
! IN  : NA    : INTEGER , SCALAIRE , PARAMETRE DIMENSIONNANT
! IN  : NA1   : INTEGER , SCALAIRE
!               NOMBRE DE LIGNES DE LA MATRICE A
! IN  : NA2   : INTEGER , SCALAIRE
!               NOMBRE DE COLONNES DE LA MATRICE A
! IN  : BMAT  : REAL*8 , TABLEAU DE DIMENSION (NB,*)
!               MATRICE B
! IN  : NB    : INTEGER , SCALAIRE , PARAMETRE DIMENSIONNANT
! IN  : NB1   : INTEGER , SCALAIRE
!               NOMBRE DE LIGNES DE LA MATRICE B
! IN  : NB2   : INTEGER , SCALAIRE
!               NOMBRE DE COLONNES DE LA MATRICE B
! OUT : CMAT  : REAL*8 , TABLEAU DE DIMENSION (NC,*)
!               MATRICE C
! IN  : NC    : INTEGER , SCALAIRE , PARAMETRE DIMENSIONNANT
! IN  : NC1   : INTEGER , SCALAIRE
!               NOMBRE DE LIGNES DE LA MATRICE C
! IN  : NC2   : INTEGER , SCALAIRE
!               NOMBRE DE COLONNES DE LA MATRICE C
! OUT : IER   : INTEGER , SCALAIRE , CODE RETOUR
!               IER = 0  OK
!               IER = 1  DIMENSIONS DE A ET B INCOMPATIBLES POUR
!                        LE PRODUIT DEMANDE
!               IER = 2  DIMENSIONS DE C INCOMPATIBLES AVEC CELLES DE
!                        A ET B POUR LE PRODUIT DEMANDE
!               IER = 3  LES DEUX ERREURS ONT ETE CONSTATEES
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "blas/ddot.h"
    integer :: iprod, na, na1, na2, nb, nb1, nb2, nc, nc1, nc2, ier
    real(kind=8) :: amat(na, *), bmat(nb, *), cmat(nc, *)
!
! VARIABLES LOCALES
! -----------------
    integer :: i, j, k
    real(kind=8) :: ctemp, zero
!
! FONCTIONS EXTERNES
! ------------------
!     EXTERNAL   DDOT
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    ier = 0
    zero = 0.0d0
    select case (iprod)
!
!---- C = A * B
!
    case (1)
        if (na2 .ne. nb1) ier = 1
        if (nc1 .ne. na1 .or. nc2 .ne. nb2) ier = ier + 2
        if (ier .ne. 0) goto 999
!
        do j = 1, nb2
            do i = 1, na1
                ctemp = zero
                do k = 1, nb1
                    ctemp = ctemp + amat(i,k) * bmat(k,j)
                end do
                cmat(i,j) = ctemp
            end do
        end do
!
!              T
!---- C = A * B
!
    case (2)
        if (na2 .ne. nb2) ier = 1
        if (nc1 .ne. na1 .or. nc2 .ne. nb1) ier = ier + 2
        if (ier .ne. 0) goto 999
!
        do j = 1, nb1
            do i = 1, na1
                ctemp = zero
                do k = 1, nb2
                    ctemp = ctemp + amat(i,k) * bmat(j,k)
                end do
                cmat(i,j) = ctemp
            end do
        end do
!
!          T
!---- C = A * B
!
    case (3)
        if (na1 .ne. nb1) ier = 1
        if (nc1 .ne. na2 .or. nc2 .ne. nb2) ier = ier + 2
        if (ier .ne. 0) goto 999
!
        do j = 1, nb2
            do i = 1, na2
                cmat(i,j) = ddot(nb1,amat(1,i),1,bmat(1,j),1)
            end do
        end do
!
!          T   T
!---- C = A * B
!
    case (4)
        if (na1 .ne. nb2) ier = 1
        if (nc1 .ne. na2 .or. nc2 .ne. nb1) ier = ier + 2
        if (ier .ne. 0) goto 999
!
        do j = 1, nb1
            do i = 1, na2
                ctemp = zero
                do k = 1, nb2
                    ctemp = ctemp + amat(k,i) * bmat(j,k)
                end do
                cmat(i,j) = ctemp
            end do
        end do
!
    end select
999 continue
!
! --- FIN DE PRMAMA.
end subroutine
