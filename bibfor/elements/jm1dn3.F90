subroutine jm1dn3(nb2, xr, epais, ksi3s2, intsn,&
                  jm1, j1dn3)
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
!                 J1DN3 ( 9 , 3 * NB2 ) =
!
!                 JTILD ( 9 , 9 ) * DNDQSI3  ( 9 , 3 * NB2 )
!
!                 POUR LA PARTIE NON CLASSIQUE D LA RIGIDITE GEOMETRIQUE
!
!                 AUX POINTS D INTEGRATION NORMALE
!
! ......................................................................
!
!
!
    implicit none
!
!
    include 'asterfort/promat.h'
    include 'asterfort/r8inir.h'
    integer :: jn
!
    integer :: nb2
!
    integer :: intsn
    integer :: intsn2
!
    integer :: i3, i4, i5
    integer :: l1, l2, l3
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
    real(kind=8) :: j1dn3 ( 9 , 27 )
!
    real(kind=8) :: tmpi ( 3 )
!
!
!DEB
!
!
!---- INITIALISATION
!
    call r8inir(9 * 27, 0.d0, j1dn3, 1)
!
!---- LES ADRESSES DES FONCTIONS DE FORME ET DE SES DERIVEES
!     SELON IND ( VOIR ROUTINE BTDFN )
!
!
!
!
!------- NOEUDS DE LAGRANGE POUR LA ROTATION
!
!              N ( 2 )              L1  POUR  I3
!            D N ( 2 ) D QSI 1      L2  POUR  I4
!            D N ( 2 ) D QSI 2      L3  POUR  I5
!
!        VOIR ROUTINE BTDFN
!
    l1 = 459
    l2 = 540
    l3 = 621
!
!------- DECALAGE DE 9 NOEUDS DE LAGRANGE
!
    intsn2 = 9 * ( intsn - 1 )
!
    i3 = l1 + intsn2
    i4 = l2 + intsn2
    i5 = l3 + intsn2
!
!
!        MODIFICATION VERIFIEE  DANS INIT080  NB2 AU LIEU DE NB1
!
    do 100 jn = 1, nb2
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
!------- REMPLISSAGE DE J1DN3 ( 9 , NB2 * 3 )
!
!        JTILD-1 ( 9 , 9 ) * DNDQSI ( 9 , 3 * NB2 )
!
!---------- BLOC U
!
        j1dn3 ( 1 ,(jn-1)*3 + 1 ) = tmpi ( 1 )
        j1dn3 ( 2 ,(jn-1)*3 + 1 ) = tmpi ( 2 )
        j1dn3 ( 3 ,(jn-1)*3 + 1 ) = tmpi ( 3 )
!
!---------- BLOC V
!
        j1dn3 ( 4 ,(jn-1)*3 + 2 ) = tmpi ( 1 )
        j1dn3 ( 5 ,(jn-1)*3 + 2 ) = tmpi ( 2 )
        j1dn3 ( 6 ,(jn-1)*3 + 2 ) = tmpi ( 3 )
!
!---------- BLOC W
!
        j1dn3 ( 7 ,(jn-1)*3 + 3 ) = tmpi ( 1 )
        j1dn3 ( 8 ,(jn-1)*3 + 3 ) = tmpi ( 2 )
        j1dn3 ( 9 ,(jn-1)*3 + 3 ) = tmpi ( 3 )
!
100  end do
!
!
!FIN
!
end subroutine
