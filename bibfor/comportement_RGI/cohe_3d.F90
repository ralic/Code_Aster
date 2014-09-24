subroutine cohe_3d(delta1, beta1, cohe1, h1, cohemin1,&
                   epse2, delta2, beta2, cohe2, h2)
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
!       calcul de la cohesion pour Drucker Prager interface3d      
!=====================================================================
    implicit none
    real(kind=8) :: delta1
    real(kind=8) :: beta1
    real(kind=8) :: cohe1
    real(kind=8) :: h1
    real(kind=8) :: cohemin1
    real(kind=8) :: epse2
    real(kind=8) :: delta2
    real(kind=8) :: beta2
    real(kind=8) :: cohe2
    real(kind=8) :: h2
!       loi d ecrouissage lineaire
    epse2=max(epse2,0.d0)
    cohe2=cohe1+H1*epse2
!        if(cohe2.le.cohemin1) then
!        si cohesion quasi nulle on repasse en von mises        
!         cohe2=cohemin1
!         H2=0.d0
!         delta2=0.d0
!         beta2=0.d0
!        else
    h2=h1
    beta2=beta1
    delta2=delta1
end subroutine
