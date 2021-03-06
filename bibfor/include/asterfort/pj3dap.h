!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
          interface 
            subroutine pj3dap(ino2,geom2,geom1,tetr4,cobary,itr3,nbtrou,&
     &btdi,btvr,btnb,btlc,btco,l_dmax,dmax,dala,loin,dmin)
              integer :: ino2
              real(kind=8) :: geom2(*)
              real(kind=8) :: geom1(*)
              integer :: tetr4(*)
              real(kind=8) :: cobary(4)
              integer :: itr3
              integer :: nbtrou
              integer :: btdi(*)
              real(kind=8) :: btvr(*)
              integer :: btnb(*)
              integer :: btlc(*)
              integer :: btco(*)
              aster_logical :: l_dmax
              real(kind=8) :: dmax
              real(kind=8) :: dala
              aster_logical :: loin
              real(kind=8) :: dmin
            end subroutine pj3dap
          end interface 
