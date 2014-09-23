subroutine b3d_dth(theta,dth,dt80)
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
! person_in_charge: etienne grimal at edf.fr
!=====================================================================
!=====================================================================
!     calcul de lendommagement thermique    
!=====================================================================
        implicit none
!     declarations externes
        real(kind=8) :: theta
        real(kind=8) :: dth
        real(kind=8) :: dt80,dth0,theta_ref,dtheta,dth1,dtheta_k
!     endommagement thermique initial
      dth0=dth
!     parametres de la loi d endommagement thermique
      theta_ref=50.d0
      if((theta_ref.lt.80.d0).and.(dt80.lt.1.).and.(dt80.gt.0.)) then
!      calcul de dtheta_k fonction de dt80 impose
       dtheta_k=-(80.d0-theta_ref)/log(1.d0-dt80)
!      endommagement thermique      
       dtheta=max(theta-theta_ref,0.d0)
       dth1=1.d0-exp(-(dtheta/dtheta_k))
      else
!      pas d'endo thermique
       dth1=0.d0
      end if
!     condition de croissance de l endo thermique
      dth=min(max(dth1,dth0),0.99999d0)
end subroutine

