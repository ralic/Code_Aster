subroutine vaft_3d(khi,casol,alsol,ssol,ohsol,kaft,gam1,gam2,daft)
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
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!      provient de rsi_3d : 
!     Calcul de la vitesse de précipitation/dissolution del'AFt
!=====================================================================
        implicit none
      real(kind=8) :: khi
      real(kind=8) ::casol
      real(kind=8) ::alsol,ssol
      real(kind=8) ::ohsol,kaft
      real(kind=8) ::gam1
      real(kind=8) ::gam2
      real(kind=8) ::daft
      real(kind=8) :: actca
      real(kind=8) ::acts
      real(kind=8) ::actal
      real(kind=8) ::actoh
      real(kind=8) ::nom
      real(kind=8) ::denom
 !     calcul des activités ioniques          
      actca=casol*gam2
      acts=ssol*gam2
      actal=alsol*gam1
      actoh=ohsol*gam1
 !     décomposition du calcul: numérateur/dénominateur
      nom=dlog10((actca**6.d0)*(acts**3.d0)*(actal**2.d0)*(actoh**4.d0))
      denom=dlog10(kaft)
      daft=khi*(1.d0-(nom/denom))
end subroutine
