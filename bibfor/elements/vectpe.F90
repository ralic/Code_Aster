subroutine vectpe(nb1, nb2, vecu, vectn, vecnph,&
                  vecpe)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit none
!
    include 'asterfort/r8inir.h'
    integer :: nb1, nb2
!
    integer :: ii
!
    integer :: in
!
    real(kind=8) :: vectn ( 9 , 3 )
!
    real(kind=8) :: vecnph ( 9 , 3 )
!
    real(kind=8) :: vecu ( 8 , 3 )
!
    real(kind=8) :: vecpe ( 51 )
!
!DEB
!
!---- INITIALISATION
!
    call r8inir(51, 0.d0, vecpe, 1)
!
    do 200 in = 1, nb1
!
!----------- NOEUDS DE SERENDIP
!
        do 210 ii = 1, 3
!
            vecpe ((in-1)*6+ ii )= vecu ( in , ii )
!
            vecpe ((in-1)*6+ ii + 3 )= vecnph ( in , ii ) - vectn&
            ( in , ii )
!
210      continue
!
200  end do
!
!---- SUPERNOEUD
!
    do 220 ii = 1, 3
!
        vecpe ( (nb1)*6+ ii )= vecnph ( nb2, ii ) - vectn ( nb2, ii )
!
220  end do
!
!
!
!
!
!FIN
!
end subroutine
