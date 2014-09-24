subroutine b3d_erupt(local, i, l3, e23, r,&
                     beta, epic, fr, gf, e,&
                     dpic)
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
!     calcul de la deformation a rupture pour une loi d'endo
!     une energie et une taille d element donnee
!=====================================================================
    implicit none
#include "asterf_types.h"
!
    aster_logical :: local
    integer :: i
    real(kind=8) :: l3(3)
    real(kind=8) :: e23(3)
    real(kind=8) :: r
    real(kind=8) :: beta
    real(kind=8) :: epic
    real(kind=8) :: fr
    real(kind=8) :: gf
    real(kind=8) :: e
    real(kind=8) :: dpic
    real(kind=8) :: li, t1, t2, t4, t10, t13, sef2
    if (local .or. (i.eq.1)) then
        li=l3(i)
        if (li .eq. 0.) then
            print*,'Il manque la taille ds la direction', i,' ds b3d_erupt'
        end if
        t1 = r ** 2
        t2 = t1 * li
        t4 = gf * e
        t10 = fr * dpic
        t13 = fr * beta
        sef2 = (-0.2D1 * t2 - 0.6D1 * t4 * fr + t2 * fr + 0.2D1 * t2 * beta + 0.6D1 * t4 * t10 - &
               &0.4D1 * t2 * t13) / (-dpic * beta + 0.2D1 * t10 * beta + dpic + t10 + beta - fr -&
               & 0.2D1 * t13 - 0.1D1) / r / li / 0.2D1
        e23(i)=sef2/e
    else
        e23(i)=e23(1)
    end if 
end subroutine
