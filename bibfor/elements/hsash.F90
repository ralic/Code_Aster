subroutine hsash(vectt, dudx, hss1, hss2)
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
    include 'asterfort/matsa.h'
    include 'asterfort/promat.h'
    real(kind=8) :: vectt ( 3 , 3 )
!
    real(kind=8) :: dudx ( 9 )
!
    real(kind=8) :: hss1 ( 2 , 9 )
!
    real(kind=8) :: hss2 ( 2 , 9 )
!
    real(kind=8) :: hsh ( 2 , 6 )
!
    real(kind=8) :: sa1 ( 6 , 9 )
!
    real(kind=8) :: sa2 ( 6 , 9 )
!
!     CONSTRUCTION DE  HSH  ( 2 , 6 ) SHEAR
!
!
!DEB
!
!
!---- MATRICES
!      SA1 ( 6 , 9 ) = ( S ) + 1 / 2 ( A ( DUDX ) ) DEF TOT
!      SA2 ( 6 , 9 ) = ( S ) +       ( A ( DUDX ) ) DEF DIF
!
    call matsa(dudx, sa1, sa2)
!
!     CONSTRUCTION DE  HSH  ( 2 , 6 ) SHEAR ( ROUTINE HFMSS )
!
    hsh (1,1)=2*vectt(1,1)*vectt(3,1)
    hsh (1,2)=2*vectt(1,2)*vectt(3,2)
    hsh (1,3)=2*vectt(1,3)*vectt(3,3)
    hsh (1,4)=vectt(3,2)*vectt(1,1)+vectt(3,1)*vectt(1,2)
    hsh (1,5)=vectt(1,1)*vectt(3,3)+vectt(3,1)*vectt(1,3)
    hsh (1,6)=vectt(3,3)*vectt(1,2)+vectt(1,3)*vectt(3,2)
!
    hsh (2,1)=2*vectt(2,1)*vectt(3,1)
    hsh (2,2)=2*vectt(2,2)*vectt(3,2)
    hsh (2,3)=2*vectt(3,3)*vectt(2,3)
    hsh (2,4)=vectt(2,1)*vectt(3,2)+vectt(3,1)*vectt(2,2)
    hsh (2,5)=vectt(2,1)*vectt(3,3)+vectt(3,1)*vectt(2,3)
    hsh (2,6)=vectt(2,2)*vectt(3,3)+vectt(2,3)*vectt(3,2)
!
!---- MATRICE
    hss1 ( 2 , 9 ) = hsh(2,6) * sa1 ( 6 , 9 )
!
    call promat(hsh, 2, 2, 6, sa1,&
                6, 6, 9, hss1)
!
!---- MATRICE
    hss2 ( 2 , 9 ) = hsh(2,6) * sa2 ( 6 , 9 )
!
    call promat(hsh, 2, 2, 6, sa2,&
                6, 6, 9, hss2)
!
!
!
!FIN
!
end subroutine
