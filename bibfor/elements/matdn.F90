subroutine matdn(nb1, xr, intsn, madn, nks1,&
                 nks2)
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
!
! ......................................................................
!     FONCTION :  CALCUL DE
!
!                 MADN ( 3 ,  6 * NB1  + 3 ) = TRANSLATION SEULEMENT
!
!                 AUX POINTS D INTEGRATION NORMALE
!
! ......................................................................
!
!
!
    implicit none
!
#include "asterfort/r8inir.h"
    real(kind=8) :: madn ( 3 , 51 )
    real(kind=8) :: nks1 ( 3 , 51 )
    real(kind=8) :: nks2 ( 3 , 51 )
    real(kind=8) :: xr ( * )
!
    integer :: jn
!
    integer :: intsn
!
    integer :: nb1
    integer :: ii
!
!DEB
!
!---- INITIALISATION
!
    call r8inir(3 * 51, 0.d0, madn, 1)
    call r8inir(3 * 51, 0.d0, nks1, 1)
    call r8inir(3 * 51, 0.d0, nks2, 1)
!
!---- LES ADRESSES DES FONCTIONS DE FORME ET DE LEURS DERIVEES
!     DECALAGE DE 8 NOEUDS DE SERENDIP ET 9 NOEUDS DE LAGRANGE
!
!---------- NOEUDS DE SERENDIP
!
    do 100 jn = 1, nb1
!
!------- PARTIE TRANSLATION
!
        do 400 ii = 1, 3
!
            madn ( ii , ( jn - 1 ) * 6 + ii ) = xr ( 135 + 8 * (&
            intsn - 1 ) + jn )
!
            nks1 ( ii , ( jn - 1 ) * 6 + ii ) = xr ( 207 + 8 * (&
            intsn - 1 ) + jn )
!
            nks2 ( ii , ( jn - 1 ) * 6 + ii ) = xr ( 279 + 8 * (&
            intsn - 1 ) + jn )
!
400      continue
!
100  end do
!
!FIN
!
end subroutine
