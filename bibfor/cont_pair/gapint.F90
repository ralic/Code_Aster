subroutine gapint(pair_tole     , elem_dime       ,&
                  elem_slav_code, elem_slav_nbnode, elem_slav_coor,&
                  elem_mast_code, elem_mast_nbnode, elem_mast_coor,&
                  nb_poin_inte  , poin_inte       ,&
                  gap_moy       , inte_weight     )
!
implicit none
!
#include "asterf_types.h" 
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/jedetr.h"
#include "asterfort/wkvect.h"
#include "asterfort/lctria.h"
#include "asterfort/jacsur.h"
#include "asterfort/mmnewd.h"
#include "asterfort/apdist.h"
#include "asterfort/reerel.h"
#include "asterfort/lcptga.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmdonf.h"
#include "asterfort/mmtang.h"
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
    real(kind=8), intent(in) :: pair_tole
    integer, intent(in) :: elem_dime
    character(len=8), intent(in) :: elem_slav_code
    integer, intent(in) :: elem_slav_nbnode
    real(kind=8), intent(in) :: elem_slav_coor(3,elem_slav_nbnode)
    character(len=8), intent(in) :: elem_mast_code
    integer, intent(in) :: elem_mast_nbnode
    real(kind=8), intent(in) :: elem_mast_coor(3,elem_mast_nbnode)
    integer, intent(in) :: nb_poin_inte
    real(kind=8), intent(in) :: poin_inte(elem_dime-1,nb_poin_inte)
    real(kind=8), intent(out) :: gap_moy
    real(kind=8), intent(out) :: inte_weight
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Compute mean square gap and weight of intersection
!
! --------------------------------------------------------------------------------------------------
!
! In  elem_slav_coor   : coordinates of slave element
! In  elem_slav_nbnode : number of nodes of slave element
! In  elem_mast_coor   : coordinates of master element
! In  elem_mast_nbnode : number of nodes of master element
! In  poin_inte        : list (sorted) of intersection points
! In  nb_poin_inte     : number of intersection points
! In  elem_slav_code   : code of slave element
! In  elem_mast_code   : code of master element
! Out inte_weight      : total weight of intersection
! In  pair_tole        : tolerance for pairing
! Out gap moy          : mean square gap
! In  elem_dime        : dimension of elements
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_tria, i_gauss, nb_tria, nb_gauss, niverr, i_node, i_dime
    real(kind=8) :: tria_coor(2,3),gauss_weight(12), gauss_coor(2,12), dist_sign
    real(kind=8) :: jaco_weight, gauss_coou(3), dire_norm(3), dist, gauss_coot(2), vect_pm(3)
    real(kind=8) :: tau1(3), tau2(3), ksi1, ksi2, jacobian, sig, elem_slav_coot(27)
    integer :: tria_node(6,3)
    character(len=8) :: gauss_family
!
! --------------------------------------------------------------------------------------------------
!
    gap_moy     = 0.d0
    inte_weight = 0.d0
!
! - Transform the format of slave element coordinates
!
    do i_node = 1,elem_slav_nbnode
        do i_dime = 1, elem_dime
            elem_slav_coot(elem_dime*(i_node-1)+i_dime) = &
                elem_slav_coor(i_dime, i_node)
        end do
    end do
!
! - Triangulation of convex polygon defined by intersection points
!
    if ((elem_dime-1) .eq. 2) then
        call lctria(nb_poin_inte, nb_tria, tria_node)
    else
        nb_tria = 1
    end if
!
! - Loop on triangles
!
    do i_tria=1, nb_tria
!
! ----- Get coordinates of triangles (parametric slave space)
!
        if ((elem_dime-1) .eq. 2) then
            tria_coor(1,1) = poin_inte(1,tria_node(i_tria,1))
            tria_coor(2,1) = poin_inte(2,tria_node(i_tria,1))
            tria_coor(1,2) = poin_inte(1,tria_node(i_tria,2))
            tria_coor(2,2) = poin_inte(2,tria_node(i_tria,2))
            tria_coor(1,3) = poin_inte(1,tria_node(i_tria,3))
            tria_coor(2,3) = poin_inte(2,tria_node(i_tria,3))
            gauss_family         = 'FPG6'
        else
            tria_coor(1,1) = poin_inte(1,1)
            tria_coor(2,1) = 0.d0
            tria_coor(1,2) = poin_inte(1,2)
            tria_coor(2,2) = 0.d0
            gauss_family         = 'FPG3'
        end if
!
! ----- Get integration scheme
!
        call lcptga(elem_dime, tria_coor , gauss_family,&
                    nb_gauss      , gauss_coor, gauss_weight)           
!
! ----- Loop on Gauss points
!       
        do i_gauss = 1, nb_gauss
!
            jacobian            = 0.d0
            dist               = 0.d0
            dire_norm(1:3)        = 0.d0
            gauss_coou(1:3)       = 0.d0
            gauss_coot(1:2) = 0.d0
!
! --------- Transform the format of Gauss coordinates
!
            gauss_coot(1) = gauss_coor(1, i_gauss)
            if (elem_dime .eq. 3) then
                gauss_coot(2) = gauss_coor(2, i_gauss)
            end if
!
! --------- Transfert Gauss coordinates in real space
!
            call reerel(elem_slav_code, elem_slav_nbnode, elem_dime, elem_slav_coot,&
                        gauss_coot    , gauss_coou)
!
! --------- Compute jacobian
!
            call jacsur(elem_slav_coor, elem_slav_nbnode, elem_slav_code, elem_dime,&
                        gauss_coot(1) , gauss_coot(2)   , jacobian      , dire_norm)
            jaco_weight = gauss_weight(i_gauss)*jacobian
!
! --------- Projection along given direction
!    
            call mmnewd(elem_mast_code, elem_mast_nbnode, elem_dime, elem_mast_coor,&
                        gauss_coou    , 200             , pair_tole, dire_norm     ,&
                        ksi1          , ksi2            , tau1     , tau2          ,&
                        niverr)
            if (niverr.eq.1) then
                ASSERT(.false.)
            end if
!
! --------- Compute distance from point to its orthogonal projection
!
            call apdist(elem_mast_code, elem_mast_coor, elem_mast_nbnode, ksi1, ksi2,&
                        gauss_coou    , dist          , vect_pm)
            if (elem_dime .eq. 3) then
                sig = vect_pm(1)*dire_norm(1)+vect_pm(2)*dire_norm(2)+vect_pm(3)*dire_norm(3)
            elseif (elem_dime .eq. 2) then
                sig = vect_pm(1)*dire_norm(1)+vect_pm(2)*dire_norm(2)
            else
                ASSERT(.false.)
            end if 
            dist_sign = -sign(dist,sig)      
!
! --------- Total integration weight and mean square gap                 
!
            gap_moy     = gap_moy+jaco_weight*dist_sign
            inte_weight = inte_weight+jaco_weight
            
        enddo
    enddo
   
end subroutine                     
