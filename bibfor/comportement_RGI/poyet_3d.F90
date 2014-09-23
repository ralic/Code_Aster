subroutine poyet_3d(casol,temp,nasol)
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
! ======================================================================
! person_in_charge: etienne grimal at edf.fr
!=====================================================================
!      provient de rsi_3d : 
!     calcul de la concentration en calcium
!=====================================================================
        implicit none
      real(kind=8) :: nasol
      real(kind=8) :: temp
      real(kind=8) :: casol
      real(kind=8) :: x
      real(kind=8) :: y
      real(kind=8) :: z
      real(kind=8) :: w
      w=356.77d0  
      x=-386.74d0
      y=0.0098d0
      z=1.36d0      
      casol=w*dexp(-x*nasol-y*temp-z*temp*nasol)*0.001d0
end subroutine
