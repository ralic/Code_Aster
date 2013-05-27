subroutine tbajvi(table, nbpara, nompar, vi, livi)
    implicit none
    include 'asterfort/tbajva.h'
    integer :: nbpara, vi, livi(*)
    character(len=*) :: table, nompar
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT:
!       ROUTINE CHAPEAU D'APPEL A TBAJVA POUR NE PASSER QUE DES ENTIERS
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: vr, livr(1)
!
    character(len=8) :: vk, livk(1)
!
    complex(kind=8) :: vc, livc(1)
!
! ----------------------------------------------------------------------
!
    vr = 0.d0
    livr(1) = 0.d0
    vc = dcmplx(0.d0,0.d0)
    livc(1) = dcmplx(0.d0,0.d0)
    vk = ' '
    livk(1) = ' '
!
    call tbajva(table, nbpara, nompar, vi, livi,&
                vr, livr, vc, livc, vk,&
                livk)
!
end subroutine
