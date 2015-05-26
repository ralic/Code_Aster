subroutine b3d_d66(nu, sn3, d66, e0, prog1,&
                   comp)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!     calcul de la matrice d'endommagement 6*6 en base principale des endommagements
!     hypothese du materiau orthotrope dans les directions principales de fissuration
!     sn=1/(1-d Normal)
!     sp=1/(1-d Poisson)
!===================================================================
    implicit none
#include "asterfort/indice0.h"
!
!     declaration externe
    real(kind=8) :: d66(6, 6), sn3(3)
    real(kind=8) :: nu, e0
    logical :: prog1, comp
!     declarations locales
    integer :: i, j, k, l
    real(kind=8) :: s33(3, 3), t1, t2, t4, t6, t8, t13, t15, t16, t21, t22, t23
    real(kind=8) :: t24, t27, t28, t29, t31, t32, t34, t37, t39, t40, t46, t50, t51
    real(kind=8) :: d1, d2, d3, sdmax, smax
!     limitation l'endo si prog1=.true.
    if (prog1) then
        smax=1.d5
        do i = 1, 3
            sn3(i)=min(sn3(i),smax)
        end do
    end if
!     initialisation de la matrice d endommagement dt66
    do i = 1, 6
        do j = 1, 6
            d66(i,j)=0.d0
        end do
    end do
    if (comp) then
!      endommagement simplifie  (pas de couplage directionnel)
        do i = 1, 3
            do j = 1, 3
                if (i .eq. j) then
                    s33(i,j)=1.d0/sn3(i)
                else
                    s33(i,j)=0.d0
                end if
            end do
        end do
    else
!      cas ou on endommage en suivant la theorie de l homogeneisation
        d1=1.d0-1.d0/sn3(1)
        d2=1.d0-1.d0/sn3(2)
        d3=1.d0-1.d0/sn3(3)
        t1 = nu ** 2
        t2 = 2.d0 * nu
        t4 = t1* d3* d2
        t6 = nu - 1.d0
        t8 = t1 * nu
        t13 = t8 * d1
        t15 = t1 * d1
        t16 = t15 * d2
        t21 = t15 * d3
        t22 = -t8 +3.d0 * t1 -3.d0 * nu + 1.d0 + t8 * d3 * d2 - t4 + t13 * d2 - t16 + 2.d0 * t13 &
              &* d2 * d3 + t13 * d3 - t21
        t23 = 1.d0 / t22
        t24 = -1.d0 + d1
        t27 = nu * d2
        t28 = nu * d3
        t29 = nu - 1.d0 + t28
        t31 = t6 * t23
        t32 = t31 * t24
        t34 = t27 + nu - 1.d0
        t37 = nu * d1
        t39 = -1.d0 + d2
        t40 = t31 * t39
        t46 = nu - 1.d0 + t37
        t50 = -1.d0 + d3
        t51 = t31 * t50
        s33(1,1) = -(-t1 + t2 - 1.d0 + t4) * t6 * t23 * t24
        s33(1,2) = t27 * t29 * t32
        s33(1,3) = t28 * t34 * t32
        s33(2,1) = t37 * t29 * t40
        s33(2,2) = -(-t1 + t2 - 1.d0 + t21) * t6 * t23 * t39
        s33(2,3) = t28 * t46 * t40
        s33(3,1) = t37 * t34 * t51
        s33(3,2) = t27 * t46 * t51
        s33(3,3) = -(-t1 + t2 - 1.d0 + t16) * t6 * t23 * t50
    end if
    do i = 1, 3
        do j = 1, 3
            if (i .eq. j) then
                d66(i,j)=1.d0-s33(i,j)
            else
                d66(i,j)=-s33(i,j)
            end if
        end do
    end do
!     calcul des termes de cisaillement
    do i = 4, 6
        call indice0(i, k, l)
!      endommagement effectif
        sdmax=max(sn3(k),sn3(l))
        d66(i,i)=1.d0-1.d0/sdmax
    end do
end subroutine
