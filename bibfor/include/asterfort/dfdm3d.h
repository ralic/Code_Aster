!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
            subroutine dfdm3d(nno,ipg,ipoids,idfde,coor,jac,dfdx,dfdy,  &
     &dfdz)
              integer, intent(in) :: nno
              integer, intent(in) :: ipg
              integer, intent(in) :: ipoids
              integer, intent(in) :: idfde
              real(kind=8), intent(in) :: coor(*)
              real(kind=8), intent(out) :: jac
              real(kind=8) ,optional, intent(out) :: dfdx(*)
              real(kind=8) ,optional, intent(out) :: dfdy(*)
              real(kind=8) ,optional, intent(out) :: dfdz(*)
            end subroutine dfdm3d
          end interface 
