subroutine b3d_l3(local, t33, n33, vt33, vss33,&
                  l3)
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
!=====================================================================
!      calcul des tailles dans les directions principales vss33
!=====================================================================
    implicit none
#include "asterfort/tail3d.h"
#include "asterf_types.h"
#include "asterf_types.h"
!      declaration externes
    aster_logical :: local
    real(kind=8) :: t33(3, 3), n33(3, 3), vt33(3, 3), vss33(3, 3)
    real(kind=8) :: l3(3)
!      declarations locales
    integer :: i, j, k
    real(kind=8) :: codir33(3, 3), denom
    if (local) then
!       calcul de la taille de l elements finis dans les directions prin
        call tail3d(l3, t33, n33, vss33)
!***********************************************************************
!       ne disposant pas de vt33 ds code ASTER on passe cette
!       correction secondaire
        goto 10
!***********************************************************************
!       correction de la longueur en fonction de l orientation des
!       contraintes par rapport a l element
        do i = 1, 3
            do j = 1, 3
                codir33(i,j)=0.d0
                do k = 1, 3
!            premier indice du codir = celui de la  contrainte seuil ,
!            2nd l un des axes du repere de l element
                    codir33(i,j)=codir33(i,j)+vss33(k,i)*vt33(k,j)
                end do
                codir33(i,j)=abs(codir33(i,j))
            end do
        end do
!       correction des directions pour les propagations de fissure biais
        do i = 1, 3
            denom=max(codir33(i,1),codir33(i,2),codir33(i,3))
            l3(i)=l3(i)*2.d0/denom
!         print*,'ds b3d_l3 l3(',i,')=',l3(i)
        end do
 10     continue
    else
!       on est en formulation sans localisation, l energie ne depend pas
!       de l element, les tailles sont alors unitaires
        do i = 1, 3
            l3(i)=1.d0
        end do
    end if
end subroutine
