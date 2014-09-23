subroutine cash_3d(alf,dcash,khi,csheff,casol,alsol,nasol,cash)
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
!     calcul des vistesses de cash
!=====================================================================
        implicit none
      real(kind=8) :: alf
      real(kind=8) :: dcash
      real(kind=8) :: khi
      real(kind=8) :: trefcash
      real(kind=8) :: csheff
      real(kind=8) :: casol
      real(kind=8) :: alsol
      real(kind=8) :: nasol
      real(kind=8) :: cash
!     attention desormais alf contient cash
!     condition sur alf (si alf est faible on ne peut plus former de cash)
      if (alf.le.cash) then
        dcash=0.d0
      else
 !       les alus fice ds les csh le sont definitvement      
        dcash=max((log10(alf)-log10(cash)),0.d0)*khi*100.
      endif
end subroutine 
