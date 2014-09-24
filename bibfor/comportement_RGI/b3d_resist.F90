subroutine b3d_resist(ssr6, rap6, t33, n33, vt33,&
                      local, rt, beta1, epic, fr,&
                      gf, young0, xnu0, reg, src3,&
                      srt3, vrap33, vrap33t)
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
!      calcul des seuils d endo associees a des resistances residuelles
!      les resistance et les seuils sont en base fixe
!=====================================================================
    implicit none
#include "asterfort/b3d_valp33.h"
#include "asterfort/x6x33.h"
#include "asterfort/transpos1.h"
#include "asterfort/b3d_l3.h"
#include "asterfort/x33x6.h"
#include "asterfort/b3d_inv.h"
#include "asterfort/b3d_chrep.h"
#include "asterf_types.h"
        real(kind=8) :: ssr6(6)
        real(kind=8) :: rap6(6)
        real(kind=8) :: t33(3, 3)
        real(kind=8) :: n33(3, 3)
        real(kind=8) :: vt33(3, 3)
        aster_logical :: local
        real(kind=8) :: rt
        real(kind=8) :: beta1
        real(kind=8) :: epic
        real(kind=8) :: fr
        real(kind=8) :: gf
        real(kind=8) :: young0
        real(kind=8) :: xnu0
        real(kind=8) :: reg
        real(kind=8) :: src3(3)
        real(kind=8) :: srt3(3)
        real(kind=8) :: vrap33(3, 3)
        real(kind=8) :: vrap33t(3, 3)
!
!      declaration des varibles locales
    real(kind=8) :: x33(3, 3), y33(3, 3), rap33(3, 3), rap3(3)
    real(kind=8) :: l3(3)
    integer :: i
    real(kind=8) :: codir33(3, 3)
!
!      directions principales des resistances
    call x6x33(Rap6,x33)
    call b3d_valp33(x33, rap3, vrap33)
!      creation de la matrice de passage inverse
    call transpos1(vrap33t,vrap33,3)
!      taille des elements ds les dir prin des resistances
    call b3d_l3(local, t33, n33, vt33, vrap33,&
                l3)
!      calcul des deformations a rupture de la loi d endommagement
!      et des seuil en contrainte effective associes a la resistance
    do i = 1, 3
        call b3d_inv(rap3(i), rt, young0, epic, reg,&
                     beta1, gf, l3(i), fr, xnu0,&
                     ssr6(i))
        srt3(i)=ssr6(i)
        src3(i)=0.d0
!        print*,'ds d3d_resist ss_rap',i,'=',ssr6(i)
    end do
    do i = 4, 6
        ssr6(i)=0.d0
    end do
!      retour des seuils en base fixe
    call x6x33(ssr6,x33)
    call b3d_chrep(y33, x33, vrap33t)
    call x33x6(y33,ssr6)
end subroutine
