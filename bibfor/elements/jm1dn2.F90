subroutine jm1dn2(indn, indc, nb1, nb2, xr,&
                  epais, ksi3s2, intsx, vecnph, jm1,&
                  j1dn2)
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
!
!
! ......................................................................
!     FONCTION :  CALCUL DU PRODUIT
!
!                 J1DN2 ( 9 ,  6 * NB1  + 3 ) =
!
!                 JTILD ( 9 , 9 ) * DNDQSI2  ( 9 ,  6 * NB1  + 3 )
!
!                 POUR LA PARTIE MATERIELLE DE LA RIGIDITE GEOMETRIQUE
!
!                 AUX POINTS D INTEGRATION REDUITE OU NORMALE
!
! ......................................................................
!
!
!
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/promat.h"
#include "asterfort/r8inir.h"
#include "asterfort/valfor.h"
    integer :: jn
!
    integer :: nb1, nb2
!
    integer :: indn, indc
!
    integer :: intsx
    integer :: intsx1, intsx2
!
    integer :: lt1, lt2
    integer :: i1, i2
!
    integer :: l1, l2, l3
    integer :: i3, i4, i5
!
    real(kind=8) :: vi ( 3 )
!
    real(kind=8) :: xr ( * )
!
    real(kind=8) :: epais
!
    real(kind=8) :: ksi3s2
!
    real(kind=8) :: jm1 ( 3 )
    real(kind=8) :: j1dn2 ( 9 , 51 )
!
    real(kind=8) :: tmpi ( 3 )
!
    real(kind=8) :: vecnph ( 9 , 3 )
!
!
!DEB
!
!---- INITIALISATION
!
    call r8inir(9 * 51, 0.d0, j1dn2, 1)
!
!---- LES ADRESSES DES FONCTIONS DE FORME ET DE LEURS DERIVEES
!     SELON INDN ( VOIR ROUTINE BTDFN )
!
    call valfor(indn, lt1, lt2, l1, l2,&
                l3)
!
!---- DECALAGE DE 8 NOEUDS DE SERENDIP
!
    intsx1 = 8 * ( intsx - 1 )
!
!---- DECALAGE DE 9 NOEUDS DE LAGRANGE
!
    intsx2 = 9 * ( intsx - 1 )
!
    i1 = lt1 + intsx1
    i2 = lt2 + intsx1
!
    i3 = l1 + intsx2
    i4 = l2 + intsx2
    i5 = l3 + intsx2
!
    ASSERT((indc.eq.1).or.(indc.eq.0))
!
    if (indc .eq. 1) then
!
!----- CALCUL COMPLET AVEC TERMES ROTATION
!
        do 100 jn = 1, nb2
!
!------- PARTIE ROTATION
!
!------- REMPLISSAGE DE VI ( 3 )
!
            vi ( 1 ) = epais * ksi3s2 * xr ( i4 + jn )
            vi ( 2 ) = epais * ksi3s2 * xr ( i5 + jn )
            vi ( 3 ) = epais * 0.5d0 * xr ( i3 + jn )
!
!------- PRODUIT  JM1 ( 3 , 3 ) * VI ( 3 )
!
            call promat(jm1, 3, 3, 3, vi,&
                        3, 3, 1, tmpi)
!
!------- REMPLISSAGE DE J1DN2 ( 9 , 6 * NB1 + 3 )
!
!        JTILD-1 ( 9 , 9 ) * DNDQSI ( 9 , 6 * NB1 + 3 )
!
!
!
            if (jn .le. nb1) then
!
!
!
!---------- NOEUDS DE SERENDIP
!
!---------- BLOC U      0      TMPZI   -TMPYI
!
                j1dn2( 1 , (jn-1) * 6 + 5 )= tmpi ( 1 )* vecnph ( jn ,&
  3             )
                j1dn2( 2 , (jn-1) * 6 + 5 )= tmpi ( 2 )* vecnph ( jn ,&
  3             )
                j1dn2( 3 , (jn-1) * 6 + 5 )= tmpi ( 3 )* vecnph ( jn ,&
  3             )
!
                j1dn2( 1 , (jn-1) * 6 + 6 )= - tmpi ( 1 )* vecnph (&
                jn , 2 )
                j1dn2( 2 , (jn-1) * 6 + 6 )= - tmpi ( 2 )* vecnph (&
                jn , 2 )
                j1dn2( 3 , (jn-1) * 6 + 6 )= - tmpi ( 3 )* vecnph (&
                jn , 2 )
!
!---------- BLOC V     - TMPZI 0        TMPXI
!
                j1dn2( 4 , (jn-1) * 6 + 4 )= - tmpi ( 1 )* vecnph (&
                jn , 3 )
                j1dn2( 5 , (jn-1) * 6 + 4 )= - tmpi ( 2 )* vecnph (&
                jn , 3 )
                j1dn2( 6 , (jn-1) * 6 + 4 )= - tmpi ( 3 )* vecnph (&
                jn , 3 )
!
                j1dn2( 4 , (jn-1) * 6 + 6 )= tmpi ( 1 )* vecnph ( jn ,&
  1             )
                j1dn2( 5 , (jn-1) * 6 + 6 )= tmpi ( 2 )* vecnph ( jn ,&
  1             )
                j1dn2( 6 , (jn-1) * 6 + 6 )= tmpi ( 3 )* vecnph ( jn ,&
  1             )
!
!---------- BLOC W       TMPY  -TMPXI   0
!
                j1dn2( 7 , (jn-1) * 6 + 4 )= tmpi ( 1 )* vecnph ( jn ,&
  2             )
                j1dn2( 8 , (jn-1) * 6 + 4 )= tmpi ( 2 )* vecnph ( jn ,&
  2             )
                j1dn2( 9 , (jn-1) * 6 + 4 )= tmpi ( 3 )* vecnph ( jn ,&
  2             )
!
                j1dn2( 7 , (jn-1) * 6 + 5 )= - tmpi ( 1 )* vecnph (&
                jn , 1 )
                j1dn2( 8 , (jn-1) * 6 + 5 )= - tmpi ( 2 )* vecnph (&
                jn , 1 )
                j1dn2( 9 , (jn-1) * 6 + 5 )= - tmpi ( 3 )* vecnph (&
                jn , 1 )
!
!
!---------- PARTIE TRANSLATION
!
!---------- REMPLISSAGE DE VI ( 3 )
!
                vi ( 1 ) = xr ( i1 + jn )
                vi ( 2 ) = xr ( i2 + jn )
                vi ( 3 ) = 0.d0
!
!---------- PRODUIT  JM1 ( 3 , 3 ) * VI ( 3 )
!
                call promat(jm1, 3, 3, 3, vi,&
                            3, 3, 1, tmpi)
!
!---------- BLOC U
!
                j1dn2( 1 , (jn-1) * 6 + 1 )= tmpi ( 1 )
                j1dn2( 2 , (jn-1) * 6 + 1 )= tmpi ( 2 )
                j1dn2( 3 , (jn-1) * 6 + 1 )= tmpi ( 3 )
!
!---------- BLOC V
!
                j1dn2( 4 , (jn-1) * 6 + 2 )= tmpi ( 1 )
                j1dn2( 5 , (jn-1) * 6 + 2 )= tmpi ( 2 )
                j1dn2( 6 , (jn-1) * 6 + 2 )= tmpi ( 3 )
!
!---------- BLOC W
!
                j1dn2( 7 , (jn-1) * 6 + 3 )= tmpi ( 1 )
                j1dn2( 8 , (jn-1) * 6 + 3 )= tmpi ( 2 )
                j1dn2( 9 , (jn-1) * 6 + 3 )= tmpi ( 3 )
!
            else
!
!------- SUPERNOEUD
!
!
!
!
!---------- BLOC U      0      TMPZI   -TMPYI
!
                j1dn2( 1 , nb1 * 6 + 2 )= tmpi ( 1 )* vecnph ( jn , 3&
                )
                j1dn2( 2 , nb1 * 6 + 2 )= tmpi ( 2 )* vecnph ( jn , 3&
                )
                j1dn2( 3 , nb1 * 6 + 2 )= tmpi ( 3 )* vecnph ( jn , 3&
                )
!
                j1dn2( 1 , nb1 * 6 + 3 )= - tmpi ( 1 )* vecnph ( jn ,&
  2             )
                j1dn2( 2 , nb1 * 6 + 3 )= - tmpi ( 2 )* vecnph ( jn ,&
  2             )
                j1dn2( 3 , nb1 * 6 + 3 )= - tmpi ( 3 )* vecnph ( jn ,&
  2             )
!
!---------- BLOC V     - TMPZI 0        TMPXI
!
                j1dn2( 4 , nb1 * 6 + 1 )= - tmpi ( 1 )* vecnph ( jn ,&
  3             )
                j1dn2( 5 , nb1 * 6 + 1 )= - tmpi ( 2 )* vecnph ( jn ,&
  3             )
                j1dn2( 6 , nb1 * 6 + 1 )= - tmpi ( 3 )* vecnph ( jn ,&
  3             )
!
                j1dn2( 4 , nb1 * 6 + 3 )= tmpi ( 1 )* vecnph ( jn , 1&
                )
                j1dn2( 5 , nb1 * 6 + 3 )= tmpi ( 2 )* vecnph ( jn , 1&
                )
                j1dn2( 6 , nb1 * 6 + 3 )= tmpi ( 3 )* vecnph ( jn , 1&
                )
!
!---------- BLOC W       TMPY  -TMPXI   0
!
                j1dn2( 7 , nb1 * 6 + 1 )= tmpi ( 1 )* vecnph ( jn , 2&
                )
                j1dn2( 8 , nb1 * 6 + 1 )= tmpi ( 2 )* vecnph ( jn , 2&
                )
                j1dn2( 9 , nb1 * 6 + 1 )= tmpi ( 3 )* vecnph ( jn , 2&
                )
!
                j1dn2( 7 , nb1 * 6 + 2 )= - tmpi ( 1 )* vecnph ( jn ,&
  1             )
                j1dn2( 8 , nb1 * 6 + 2 )= - tmpi ( 2 )* vecnph ( jn ,&
  1             )
                j1dn2( 9 , nb1 * 6 + 2 )= - tmpi ( 3 )* vecnph ( jn ,&
  1             )
!
            endif
!
100      continue
!
!
!
!
    else if (indc .eq. 0) then
!
!
!
!
!----- CALCUL INCOMPLET SANS TERMES ROTATION
!
!
        do 200 jn = 1, nb1
!
!---------- NOEUDS DE SERENDIP
!
!---------- PARTIE TRANSLATION
!
!---------- REMPLISSAGE DE VI ( 3 )
!
            vi ( 1 ) = xr ( i1 + jn )
            vi ( 2 ) = xr ( i2 + jn )
            vi ( 3 ) = 0.d0
!
!---------- PRODUIT  JM1 ( 3 , 3 ) * VI ( 3 )
!
            call promat(jm1, 3, 3, 3, vi,&
                        3, 3, 1, tmpi)
!
!---------- BLOC U
!
            j1dn2( 1 , (jn-1) * 6 + 1 )= tmpi ( 1 )
            j1dn2( 2 , (jn-1) * 6 + 1 )= tmpi ( 2 )
            j1dn2( 3 , (jn-1) * 6 + 1 )= tmpi ( 3 )
!
!---------- BLOC V
!
            j1dn2( 4 , (jn-1) * 6 + 2 )= tmpi ( 1 )
            j1dn2( 5 , (jn-1) * 6 + 2 )= tmpi ( 2 )
            j1dn2( 6 , (jn-1) * 6 + 2 )= tmpi ( 3 )
!
!---------- BLOC W
!
            j1dn2( 7 , (jn-1) * 6 + 3 )= tmpi ( 1 )
            j1dn2( 8 , (jn-1) * 6 + 3 )= tmpi ( 2 )
            j1dn2( 9 , (jn-1) * 6 + 3 )= tmpi ( 3 )
!
200      continue
!
!
!
    endif
!
!
!
!
!FIN
!
end subroutine
