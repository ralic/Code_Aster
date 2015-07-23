subroutine get_line_points(line, pt1, pt2)
!
      implicit none
!
      real(kind=8), intent(in) :: line(3)
      real(kind=8), intent(out) :: pt1(3)
      real(kind=8), intent(out) :: pt2(3)
!
!-----------------------------------------------------------------------
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
!======================================================================
!
!     
!
! IN  LINE : LIGNE 2D en coordonees projectives
! OUT PT1 : un point sur la ligne en coordonnees projectives
! OUT P22 : autre point en coordonnees projectives
!
      if(line(1).eq.0.d0.and.line(2).eq.0.d0) then
         pt1 = (/ 1.d0, 0.d0, 0.d0 /)
         pt2 = (/ 0.d0, 1.d0, 0.d0 /)
      else
         pt2 = (/ -line(2), line(1), 0.d0 /)
         if(abs(line(1)).lt.abs(line(2))) then
            pt1 = (/ 0.d0, -line(3), line(2)/)
         else
            pt1 = (/ -line(3), 0.d0, line(1)/)
         endif
      endif
!
!
end subroutine
