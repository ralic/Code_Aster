subroutine flu_tot1d(e0, e1, eta1, eta2, dt,&
                     s0, s1, eps10, eps11, deps0,&
                     deps1, deps2)
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
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!=====================================================================
!     calcul de l evolution des deformations pour une evolution
!     lineaire de la contrainte entre le debut et la fin du pas
!     en supposant les parametres visco elastiques constants sur le pas
!     de temps
!
!     cf fichier Maple flu_inc1d si parametres varibles sur le pas de te
!     il faudra alors rajouter eps0, veps1 et veps2 en varible internes
!=====================================================================

        implicit none
        real(kind=8) :: e0
        real(kind=8) :: e1
        real(kind=8) :: eta1
        real(kind=8) :: eta2
        real(kind=8) :: dt
        real(kind=8) :: s0
        real(kind=8) :: s1
        real(kind=8) :: eps10
        real(kind=8) :: eps11
        real(kind=8) :: deps0
        real(kind=8) :: deps1
        real(kind=8) :: deps2,t4,t12,t5
!
!     calul de la deformation visco elastique en fin de pas
    if (dt .gt. 0.d0) then
        t4 = exp(-e1 / eta1 * dt)
        t5 = t4 * s0
        t12 = e1 ** 2
        eps11=-(t5 * dt * e1 - t4 * s1 * eta1 + t5 * eta1 - t4 * eps10 *dt&
     & * t12 - dt * s1 * e1 + s1 * eta1 - s0 * eta1) / dt / t12
    else
        eps11=eps10
        end if
        deps1=eps11-eps10
!
!     calcul de l'increment de deformation visqueuse
        deps2 = (s0 * dt + dt * s1) / eta2 / 0.2d1
!
!     calcul de l increment elastique
        deps0= (s1-s0)/e0
end subroutine
