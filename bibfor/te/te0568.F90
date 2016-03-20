subroutine te0568(nomopt, nomte)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/lcelem.h"
#include "asterfort/lcstco.h"
#include "asterfort/lcgeog.h"
#include "asterfort/lcpjit.h"
#include "asterfort/lctria.h"
#include "asterfort/lcptga.h"
#include "asterfort/lctppe.h"
#include "asterfort/lcsees.h"
#include "asterfort/lcsema.h"
#include "asterfort/lcsegp.h"
#include "asterfort/lctrco.h"
#include "asterfort/lcrtma.h"
#include "asterfort/mmmtdb.h"
#include "asterfort/lcsena.h"
#include "asterfort/apdcma.h"
#include "asterfort/apdmae.h"
#include "asterfort/aprtpe.h"
#include "asterfort/aprtpm.h"
#include "asterf_types.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=16), intent(in) :: nomopt
    character(len=16), intent(in) :: nomte
!
! --------------------------------------------------------------------------------------------------
!
! Elementary computation
!
! Option: CHAR_MECA_CONT
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i
    integer :: nb_node_slav, nb_node_mast, nb_lagr, nb_poin_inte, nb_dof, nb_tria, nb_gauss
    integer :: indi_lagc(10)
    integer :: elem_dime
    integer :: jvect
    integer :: i_tria, i_dime, i_elin_mast, i_elin_slav, i_node, i_gauss
    real(kind=8) :: proj_tole, lagrc
    integer :: algo_reso_geom, indi_cont, norm_smooth
    aster_logical :: l_axis, l_elem_frot, loptf, debug, l_upda_jaco       
    real(kind=8) :: norm(3)
    character(len=8) :: elem_slav_code, elem_mast_code
    real(kind=8) :: elem_mast_coor(27),elem_slav_coor(27)
    real(kind=8) :: elin_mast_coor(27)
    integer :: elin_mast_nbsub, elin_mast_sub(8,4), elin_mast_nbnode(8)
    character(len=8) :: elin_mast_code 
    real(kind=8) :: elin_slav_coor(27)
    integer :: elin_slav_nbsub, elin_slav_sub(8,9), elin_slav_nbnode(8)
    character(len=8) :: elin_slav_code
    real(kind=8) :: poin_inte(32), tria_coot(2,3), tria_coor(32), tria_coor_aux(32)
    integer :: tria_node(6,3)
    real(kind=8) :: inte_weight
    real(kind=8) :: gauss_weight(12), gauss_coor(2,12), gauss_coot(2)
    character(len=8) :: elga_fami_slav, elga_fami_mast 
    real(kind=8) :: poidpg, jaco_init, jaco_upda, jacobian
    real(kind=8) :: shape_func(9), shape_dfunc(2, 9)
    real(kind=8) :: vtmp(55)
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    vtmp(1:55)           = 0.d0
    elem_mast_coor(1:27) = 0.d0
    elem_slav_coor(1:27) = 0.d0
    proj_tole            = 1.d-9
    debug                = .false.
    loptf                = nomopt.eq.'RIGI_FROT'
    ASSERT(.not.loptf)
!
! - Get informations about contact element
!
    call lcelem(nomte         , elem_dime     ,&
                l_axis        , l_elem_frot   ,&
                nb_dof        , nb_lagr       , indi_lagc   ,&
                elem_slav_code, elga_fami_slav, nb_node_slav,&
                elem_mast_code, elga_fami_mast, nb_node_mast)
    ASSERT(nb_dof .le. 55)
!
! - Get indicators
!
    call lcstco(algo_reso_geom, indi_cont, l_upda_jaco, lagrc)
!
! - Compute updated geometry
!
    call lcgeog(elem_dime     , nb_lagr       , indi_lagc ,&
                nb_node_slav  , nb_node_mast  , &
                algo_reso_geom, elem_mast_coor, elem_slav_coor,&
                norm_smooth)
!
! - Compute vector
!
    if (indi_cont .eq. 1) then
!
! ----- Cut elements in linearized sub-elements
!
        call apdcma(elem_mast_code, elin_mast_sub, elin_mast_nbnode, elin_mast_nbsub)
        call apdmae(elem_slav_code, elin_slav_sub, elin_slav_nbnode, elin_slav_nbsub)
!
! ----- Loop on linearized slave sub-elements
!
        do i_elin_slav = 1, elin_slav_nbsub
!
! --------- Code for current linearized slave sub-element
!
            if (elin_slav_nbnode(i_elin_slav) .eq. 2 .and. elem_dime .eq. 2) then
                elin_slav_code = 'SE2'
            elseif (elin_slav_nbnode(i_elin_slav) .eq. 3 .and. elem_dime .eq. 2) then
                elin_slav_code = 'SE3'
            elseif (elin_slav_nbnode(i_elin_slav) .eq. 3 .and. elem_dime .eq. 3) then
                elin_slav_code = 'TR3'
            elseif (elin_slav_nbnode(i_elin_slav) .eq. 4 .and. elem_dime .eq. 3) then
                elin_slav_code = 'QU4'
            elseif (elin_slav_nbnode(i_elin_slav) .eq. 6 .and. elem_dime .eq. 3) then
                elin_slav_code = 'TR6'
            elseif (elin_slav_nbnode(i_elin_slav) .eq. 9 .and. elem_dime .eq. 3) then
                elin_slav_code = 'QU9'
            else
                ASSERT(.false.)
            end if
!
! --------- Get coordinates for current linearized slave sub-element
!
            elin_slav_coor(:) = 0.d0
            do i_node = 1, elin_slav_nbnode(i_elin_slav)
                do i_dime = 1, elem_dime
                    elin_slav_coor((i_node-1)*elem_dime+i_dime) = &
                       elem_slav_coor((elin_slav_sub(i_elin_slav,i_node)-1)*elem_dime+i_dime)
                end do
            end do
!
! --------- Loop on linearized master sub-elements
!         
            do i_elin_mast = 1, elin_mast_nbsub
!
! ------------- Code for current linearized master sub-element
!
                if (elin_mast_nbnode(i_elin_mast) .eq. 2 .and. elem_dime .eq. 2) then
                    elin_mast_code = 'SE2'                             
                elseif (elin_mast_nbnode(i_elin_mast) .eq. 3 .and. elem_dime .eq. 3) then
                    elin_mast_code = 'TR3'
                elseif (elin_mast_nbnode(i_elin_mast) .eq. 4 .and. elem_dime .eq. 3) then
                    elin_mast_code = 'QU4'
                else
                    ASSERT(.false.)
                end if
!
! ------------- Get coordinates for current linearized master sub-element
!              
                elin_mast_coor(:) = 0.d0
                do i_node = 1, elin_mast_nbnode(i_elin_mast)
                    do i_dime = 1, elem_dime
                        elin_mast_coor((i_node-1)*elem_dime+i_dime) = &
                            elem_mast_coor((elin_mast_sub(i_elin_mast,i_node)-1)*elem_dime+i_dime)
                    end do     
                end do
!
! ------------- Projection/intersection
!
                call lcpjit(proj_tole                    , elem_dime     ,&
                            elin_mast_nbnode(i_elin_mast), elin_mast_coor, elin_mast_code,&
                            elin_slav_nbnode(i_elin_slav), elin_slav_coor, elin_slav_code,&
                            poin_inte                    , inte_weight   , nb_poin_inte)
                if (debug) then
                    write(*,*) "Intersection - Master: ", 'Mast', i_elin_mast
                    write(*,*) "Intersection - Slave : ", 'Slav', i_elin_slav
                    write(*,*) "Intersection - Poids : ", inte_weight
                    write(*,*) "Intersection - Nb    : ", nb_poin_inte
                    write(*,*) "Intersection - Points: ", poin_inte
                endif
!
                if (inte_weight .gt. proj_tole) then
!
! ----------------- Triangulation of convex polygon defined by intersection points
!
                    if (elem_dime .eq. 3) then
                        call lctria(nb_poin_inte, nb_tria, tria_node)
                    elseif (elem_dime .eq. 2) then
                        nb_tria = 1
                    else
                        ASSERT(.false.)
                    end if
                    if (debug) then
                        write(*,*) "Triangulation: ", nb_poin_inte, nb_tria
                    endif
!
! ----------------- Loop on triangles
!
                    do i_tria = 1, nb_tria
!
! --------------------- Coordinates of current triangle
!
                        tria_coor(1:32) = 0.d0
                        if (elem_dime .eq. 3) then
                            call lctrco(i_tria, tria_node, poin_inte, tria_coor)
                        elseif (elem_dime .eq. 2) then
                            tria_coor(1:32) = poin_inte(1:32)
                        endif
                        if (debug) then
                            write(*,*) "Triangle: ", i_tria, tria_coor
                        endif
                        tria_coor_aux(1:32)=tria_coor(1:32)
!
! --------------------- Change shape of vector
!
                        tria_coot(1:2,1:3) = 0.d0
                        if (elem_dime .eq. 3) then
                            do i_node = 1,3
                                do i_dime = 1,(elem_dime-1)
                                    tria_coot(i_dime, i_node) = &
                                        tria_coor((i_node-1)*(elem_dime-1)+i_dime)
                                end do
                            end do
                        else
                            tria_coot(1,1) = tria_coor(1)
                            tria_coot(2,1) = 0.d0
                            tria_coot(1,2) = tria_coor(2)
                            tria_coot(2,2) = 0.d0
                        end if
!
! --------------------- Get integration points for slave element
!
                        call lcptga(elem_dime, tria_coot , elga_fami_slav,&
                                    nb_gauss , gauss_coor, gauss_weight)
!
! --------------------- Loop on integration points in slave element
!
                        do i_gauss = 1, nb_gauss
!
! ------------------------- Get current integration point
!
                            gauss_coot(1:2) = 0.d0
                            do i_dime = 1, elem_dime-1
                                gauss_coot(i_dime) = gauss_coor(i_dime, i_gauss)
                            end do
                            poidpg = gauss_weight(i_gauss)
!
! ------------------------- Compute geometric quantities for contact (slave side)
!
                            call lctppe('Slave'     , elem_dime     , l_axis        ,&
                                        nb_node_slav, elin_slav_coor, elem_slav_code,&
                                        gauss_coot  , shape_func    , shape_dfunc   ,&
                                        jaco_init   , jaco_upda     , norm)
                            if (l_upda_jaco) then
                                jacobian = jaco_upda
                            else
                                jacobian = jaco_init
                            endif
!
                        end do                                     
                                                                    
! ------- CONTRIBUTIONS ESCLAVES et "GEOMETRIQUES":
!
! --------------------- Projection from para. space of triangle into sub-element para. space
!
                        if (elem_slav_code .ne. elin_slav_code ) then
                            call aprtpe(elem_dime     , tria_coor  , 3,&
                                        elem_slav_code, i_elin_slav)
                        endif
!
! --------------------- Change shape of vector
!
                        tria_coot(1:2,1:3)=0.d0
                        if (elem_dime .eq. 3) then
                            do i_node = 1,3
                                do i_dime = 1,(elem_dime-1)
                                    tria_coot(i_dime, i_node) = &
                                        tria_coor((i_node-1)*(elem_dime-1)+i_dime)
                                end do
                            end do
                        else
                            tria_coot(1,1) = tria_coor(1)
                            tria_coot(2,1) = 0.d0
                            tria_coot(1,2) = tria_coor(2)
                            tria_coot(2,2) = 0.d0
                        end if
!
! --------------------- Get integration points for slave element
!
                        call lcptga(elem_dime, tria_coot , elga_fami_slav,&
                                    nb_gauss , gauss_coor, gauss_weight)
!
! --------------------- Loop on integration points in slave element
!
                        do i_gauss = 1, nb_gauss
!
! ------------------------- Get current integration point
!
                            gauss_coot(1:2) = 0.d0
                            do i_dime = 1, elem_dime-1
                                gauss_coot(i_dime) = gauss_coor(i_dime, i_gauss)
                            end do
                            poidpg = gauss_weight(i_gauss)
!
! ------------------------- Compute geometric quantities for contact (slave side)
!
                            call lctppe('Slave'     , elem_dime     , l_axis        ,&
                                        nb_node_slav, elem_slav_coor, elem_slav_code,&
                                        gauss_coot  , shape_func    , shape_dfunc   ,&
                                        jaco_init   , jaco_upda     , norm)
                            if (l_upda_jaco) then
                                jacobian = jaco_upda
                            else
                                jacobian = jaco_init
                            endif
!
! ------------------------- Compute contact vector - geometric (slave side)
!
                            call lcsees(elem_dime  , nb_node_slav, nb_lagr  ,&
                                        norm_smooth, norm        , indi_lagc, lagrc,&
                                        poidpg     , shape_func  , jacobian ,&
                                        vtmp )
                            call lcsegp(elem_dime   , nb_lagr       , indi_lagc     ,&
                                        nb_node_mast, elem_mast_coor,&
                                        elem_mast_code,&
                                        nb_node_slav, elem_slav_coor,&
                                        elem_slav_code,&
                                        poidpg      , gauss_coot    , jaco_upda,&
                                        vtmp)

                        end do             
!
! --------------------- Projection of triangle in master parametric space
!
                        call lcrtma(elem_dime       , proj_tole,&
                                    tria_coor_aux   , &
                                    elin_slav_nbnode(i_elin_slav), elin_slav_coor, elin_slav_code,&
                                    nb_node_mast                 , elem_mast_coor, elem_mast_code,&
                                    tria_coot)
!
! --------------------- Get integration points for master element
!
                        call lcptga(elem_dime, tria_coot , elga_fami_mast,&
                                    nb_gauss , gauss_coor, gauss_weight)
!
! --------------------- Loop on integration points in master element
!
                        do i_gauss = 1, nb_gauss
!
! ------------------------- Get current integration point
!
                            gauss_coot(1:2) = 0.d0
                            do i_dime = 1, elem_dime-1
                                gauss_coot(i_dime) = gauss_coor(i_dime,i_gauss)
                            end do
                            poidpg = gauss_weight(i_gauss)
!
! ------------------------- Compute geometric quantities for contact (master side)
!
                            call lctppe('Master'    , elem_dime     , l_axis        ,&
                                        nb_node_mast, elem_mast_coor, elem_mast_code,&
                                        gauss_coot  , shape_func    , shape_dfunc   ,&
                                        jaco_init   , jaco_upda     , norm)
                            if (l_upda_jaco) then
                                jacobian = jaco_upda
                            else
                                jacobian = jaco_init
                            endif
!
! ------------------------- Compute contact vector (master side)
!
                            call lcsema(elem_dime  , nb_node_mast, nb_node_slav, nb_lagr,&
                                        norm_smooth, norm        , lagrc   ,&
                                        poidpg     , shape_func  , jacobian,&
                                        vtmp )
                        end do
                    end do
                end if
            end do
        end do
    elseif (indi_cont .eq. 0) then
        call lcsena(elem_dime, nb_lagr, nb_node_slav, indi_lagc, &
                    lagrc    , vtmp)
    else
!
    endif
!
! - Write vector
!
    call jevech('PVECTUR', 'E', jvect)
    do i = 1, nb_dof
        zr(jvect-1+i) = vtmp(i)
    end do
!
    call jedema()
end subroutine
