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
            subroutine pj2dco(mocle,moa1,moa2,nbma1,lima1,nbno2,lino2,  &
     &geom1,geom2,corres,l_dmax,dmax,dala)
              character(len=*) :: mocle
              character(len=8) :: moa1
              character(len=8) :: moa2
              integer :: nbma1
              integer :: lima1(*)
              integer :: nbno2
              integer :: lino2(*)
              character(len=*) :: geom1
              character(len=*) :: geom2
              character(len=16) :: corres
              aster_logical :: l_dmax
              real(kind=8) :: dmax
              real(kind=8) :: dala
            end subroutine pj2dco
          end interface 
