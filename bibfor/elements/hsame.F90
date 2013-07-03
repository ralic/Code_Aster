subroutine hsame(vectt, dudx, hsm1, hsm2)
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
#include "asterfort/matsa.h"
#include "asterfort/promat.h"
    real(kind=8) :: vectt ( 3 , 3 )
!
    real(kind=8) :: dudx ( 9 )
!
    real(kind=8) :: hsm1 ( 3 , 9 )
!
    real(kind=8) :: hsm2 ( 3 , 9 )
!
    real(kind=8) :: hfm ( 3 , 6 )
!
    real(kind=8) :: sa1 ( 6 , 9 )
!
    real(kind=8) :: sa2 ( 6 , 9 )
!
!DEB
!
!
!     CONSTRUCTION DE  HFM  ( 3 , 6 ) MEMBRANE-FLEXION
!                                     ( ROUTINE HFMSS )
!
!
    hfm(1,1)=vectt(1,1)*vectt(1,1)
    hfm(1,2)=vectt(1,2)*vectt(1,2)
    hfm(1,3)=vectt(1,3)*vectt(1,3)
    hfm(1,4)=vectt(1,1)*vectt(1,2)
    hfm(1,5)=vectt(1,1)*vectt(1,3)
    hfm(1,6)=vectt(1,2)*vectt(1,3)
!
    hfm(2,1)=vectt(2,1)*vectt(2,1)
    hfm(2,2)=vectt(2,2)*vectt(2,2)
    hfm(2,3)=vectt(2,3)*vectt(2,3)
    hfm(2,4)=vectt(2,1)*vectt(2,2)
    hfm(2,5)=vectt(2,1)*vectt(2,3)
    hfm(2,6)=vectt(2,2)*vectt(2,3)
!
    hfm(3,1)=2*vectt(1,1)*vectt(2,1)
    hfm(3,2)=2*vectt(1,2)*vectt(2,2)
    hfm(3,3)=2*vectt(1,3)*vectt(2,3)
    hfm(3,4)=vectt(2,1)*vectt(1,2)+vectt(1,1)*vectt(2,2)
    hfm(3,5)=vectt(2,1)*vectt(1,3)+vectt(1,1)*vectt(2,3)
    hfm(3,6)=vectt(2,2)*vectt(1,3)+vectt(1,2)*vectt(2,3)
!
!
!---- MATRICES
!      SA1 ( 6 , 9 ) = ( S ) + 1 / 2 ( A ( DUDX ) ) POUR DEF TOT
!      SA2 ( 6 , 9 ) = ( S ) +       ( A ( DUDX ) ) POUR DEF DIF
!
    call matsa(dudx, sa1, sa2)
!
!---- MATRICE
    hsm1 ( 3 , 9 ) = hfm(3,6) * sa1 ( 6 , 9 )
!
    call promat(hfm, 3, 3, 6, sa1,&
                6, 6, 9, hsm1)
!
!---- MATRICE
    hsm2 ( 3 , 9 ) = hfm(3,6) * sa2 ( 6 , 9 )
!
    call promat(hfm, 3, 3, 6, sa2,&
                6, 6, 9, hsm2)
!
!
!
!FIN
!
end subroutine
