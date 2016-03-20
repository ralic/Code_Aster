subroutine lcsegp(elem_dime   , nb_lagr       , indi_lagc     ,&
                  nb_node_mast, elin_mast_coor, elin_mast_code,&
                  nb_node_slav, elin_slav_coor, elin_slav_code,&
                  poidspg     , gauss_coot    , jacobian      ,&
                  vtmp)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/mmnewd.h"
#include "asterfort/mmdonf.h"
#include "asterfort/mmtang.h"
#include "asterfort/mmnorm.h"
#include "asterfort/reerel.h"
#include "asterfort/apdist.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    integer, intent(in) :: elem_dime
    integer, intent(in) :: nb_lagr
    integer, intent(in) :: indi_lagc(10)
    integer, intent(in) :: nb_node_mast
    real(kind=8), intent(in) :: elin_mast_coor(elem_dime,nb_node_mast)
    character(len=8), intent(in) :: elin_mast_code
    integer, intent(in) :: nb_node_slav
    real(kind=8), intent(in) :: elin_slav_coor(elem_dime,nb_node_slav)
    character(len=8), intent(in) :: elin_slav_code
    real(kind=8), intent(in) :: poidspg
    real(kind=8), intent(in) :: gauss_coot(2)
    real(kind=8), intent(in) :: jacobian
    real(kind=8), intent(inout) :: vtmp(55)
!
! --------------------------------------------------------------------------------------------------
!
! Contact (LAC) - Elementary computations
!
! Compute contact vector (slave side)
!
! --------------------------------------------------------------------------------------------------
!
! In  elem_dime        : dimension of elements
! In  nb_lagr          : total number of Lagrangian dof on contact element
! In  indi_lagc        : PREVIOUS node where Lagrangian dof is present (1) or not (0)
! In  nb_node_mast     : number of nodes of for master side from contact element
! In  elin_mast_coor   : coordinates of sub-elements in master element
! In  elin_mast_code   : code of all sub-elements in master element
! In  nb_node_slav     : number of nodes of for slave side from contact element
! In  elin_slav_coor   : coordinates of sub-elements in slave element
! In  elin_slav_code   : code of all sub-elements in slave element
! In  poidspg          : weight at integration point
! In  gauss_coot       : coordiantes of current integration point
! In  jacobian         : jacobian at integration point
! IO  vtmp             : vector
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_node, i_dime, jj, shift, niverr
    real(kind=8) :: tau1(3), tau2(3), dir(3), gauss_coor(3), proj_tole
    real(kind=8) :: vect_pm(3), ksi1, ksi2, sig, pdgr
    real(kind=8) :: dff(2, 9), dist, r_lagr_c
    real(kind=8) :: slav_coor(3,9),mast_coor(3,9)
!
! --------------------------------------------------------------------------------------------------
!
    jj=0
    proj_tole=1.d-9
    gauss_coor(1:3) = 0.d0
    tau1(1:3) = 0.d0
    tau2(1:3) = 0.d0
    vect_pm(1:3) = 0.d0
    dir(1:3) = 0.d0
    shift = 0
    r_lagr_c=real(nb_lagr,kind=8)
    slav_coor(1:3,1:9) = 0.d0
    mast_coor(1:3,1:9) = 0.d0
!
! - Get coordinates of elements
!
    do i_node = 1,nb_node_slav
        do i_dime = 1,elem_dime
            slav_coor(i_dime,i_node) = elin_slav_coor(i_dime,i_node)
        enddo
    enddo
    do i_node = 1,nb_node_mast
        do i_dime = 1,elem_dime
            mast_coor(i_dime,i_node) = elin_mast_coor(i_dime,i_node)
        enddo
    enddo
!
! - Project slave element into real space
!
    call reerel(elin_slav_code, nb_node_slav, elem_dime, elin_slav_coor, gauss_coot,&
                gauss_coor) 
!
! - Compute normal
!    
    call mmdonf(elem_dime, nb_node_slav, elin_slav_code,&
                gauss_coot(1), gauss_coot(2),&
                dff)
    call mmtang(elem_dime, nb_node_slav, slav_coor, dff, tau1,&
                tau2)
    call mmnorm(elem_dime, tau1, tau2, dir)
    pdgr=jacobian*poidspg
!
! - Projection of slave point on master element
   
     call mmnewd(elin_mast_code, nb_node_mast, elem_dime, mast_coor, &
                 gauss_coor, 200,&
                 proj_tole, dir, ksi1, ksi2,&
                 tau1, tau2, niverr)
     if (niverr.eq.1) then
        write(*,*)"mmnewd failed"
        ASSERT(.false.)
     end if
!
! - Compute distance
!
    call apdist(elin_mast_code, mast_coor, nb_node_mast, ksi1, ksi2,&
                gauss_coor, dist, vect_pm)
!
! - Compute _algebrical_ distance
!
    if (elem_dime .eq. 3) then
        sig = vect_pm(1)*dir(1)+vect_pm(2)*dir(2)+vect_pm(3)*dir(3)
    elseif (elem_dime .eq. 2) then
        sig = vect_pm(1)*dir(1)+vect_pm(2)*dir(2)
    else
        ASSERT(.false.)
    end if
    dist=sign(dist,sig)
!
! - Compute vector
!
    do i_node = 1, nb_node_slav 
        shift=shift+indi_lagc(i_node) 
        if (indi_lagc(i_node+1).eq. 1) then
            jj=elem_dime*(i_node-1)+shift+elem_dime+1
               vtmp(jj)=vtmp(jj)-pdgr*dist/(r_lagr_c)
        end if
    end do
!
end subroutine
        
