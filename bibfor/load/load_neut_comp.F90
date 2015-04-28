subroutine load_neut_comp(type_calc, stop_calc , model         , time      , load_name,&
                          load_nume, nb_in_maxi, nb_in_prep    , lpain     , lchin    ,&
                          base     , resu_elem , matr_vect_elem, time_move_, i_load_  )
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/copisd.h"
#include "asterfort/reajre.h"
#include "asterfort/gcnco2.h"
#include "asterfort/corich.h"
#include "asterfort/load_neut_spec.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=4), intent(in) :: type_calc
    character(len=1), intent(in) :: stop_calc
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: time
    character(len=8), intent(in) :: load_name
    integer, intent(in) :: load_nume
    integer, intent(in) :: nb_in_maxi
    integer, intent(in) :: nb_in_prep
    character(len=*), intent(inout) :: lpain(nb_in_maxi)
    character(len=*), intent(inout) :: lchin(nb_in_maxi)
    character(len=19), intent(inout) :: resu_elem
    character(len=19), intent(in) :: matr_vect_elem
    character(len=1), intent(in) :: base
    character(len=24), optional, intent(in) :: time_move_
    integer, optional, intent(in) :: i_load_
!
! --------------------------------------------------------------------------------------------------
!
! Neumann loads computation - Thermic
! 
! Elementary (on one load)
!
! --------------------------------------------------------------------------------------------------
!
! In  type_calc        : type of option to compute
!                        '2MBR' for second member (vector)
!                        'RESI' for residual (vector)
!                        'MRIG' for rigidity (matrix)
!                        'MTAN' for tangent matrix
! In  stop_calc        : CALCUL subroutine comportement
! In  model            : name of the model
! In  time             : time (<CARTE>)
! In  load_name        : name of current load
! In  load_nume        : identification of load type
! In  nb_in_maxi       : maximum number of input fields
! In  nb_in_prep       : number of input fields before specific ones
! IO  lpain            : list of input parameters
! IO  lchin            : list of input fields
! IO  resu_elem        : name of resu_elem
! In  matr_vect_elem   : name of matr_elem or vect_elem
! In  base             : JEVEUX base to create vect_elem
! In  time_move        : modified time (<CARTE>) for THER_NON_LINE_MO
! In  i_load           : index of current load
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_type_neum
    parameter (nb_type_neum=10)
!
    integer :: i_type_neum, nb_in_add, ibid
    character(len=16) :: load_option
    character(len=24) :: load_ligrel  
    integer :: nbout, nbin
    character(len=8) :: lpaout, newnom
!
! --------------------------------------------------------------------------------------------------
!
    do i_type_neum = 1, nb_type_neum
!
! ----- Get information about load
!
        if (present(time_move_)) then
            call load_neut_spec('MOVE'     , type_calc  , model       , time       , load_name  ,&
                                load_nume  , i_type_neum, nb_type_neum, nb_in_maxi , nb_in_prep ,&
                                lchin      , lpain      , nb_in_add   , lpaout     , load_ligrel,&
                                load_option,&
                                time_move_ = time_move_)
        else
            call load_neut_spec('STAT'     , type_calc  , model       , time       , load_name  ,&
                                load_nume  , i_type_neum, nb_type_neum, nb_in_maxi , nb_in_prep ,&
                                lchin      , lpain      , nb_in_add   , lpaout     , load_ligrel,&
                                load_option)
        endif
!
        if (load_option .ne. 'No_Load') then
!
! --------- Generate new RESU_ELEM name
!
            newnom = resu_elem(10:16)
            call gcnco2(newnom)
            resu_elem(10:16) = newnom(2:8)
!
! --------- Attach load to RESU_ELEM
!
            if (present(i_load_)) then
                call corich('E', resu_elem, i_load_, ibid)
            else
                call corich('E', resu_elem, -1, ibid)
            endif
!
! --------- Number of fields
!
            nbin  = nb_in_prep+nb_in_add
            nbout = 1
!
! --------- Computation
!
            call calcul(stop_calc, load_option, load_ligrel, nbin  , lchin,&
                        lpain    , nbout      , resu_elem  , lpaout, base ,&
                        'OUI')
!
! --------- Copying output field
!
            call reajre(matr_vect_elem, resu_elem, base)
        endif
    end do

end subroutine
