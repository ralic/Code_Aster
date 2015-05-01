subroutine load_neum_comp(stop       , i_load    , load_name , load_nume  , load_type,&
                          ligrel_calc, nb_in_maxi, nb_in_prep, lpain      , lchin    ,&
                          base       , resu_elem , vect_elem , iden_direct, name_inputz)
!
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/copisd.h"
#include "asterfort/exisd.h"
#include "asterfort/reajre.h"
#include "asterfort/gcnco2.h"
#include "asterfort/corich.h"
#include "asterfort/load_neum_spec.h"
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=1), intent(in) :: stop
    integer, intent(in) :: i_load
    character(len=8), intent(in) :: load_name
    integer, intent(in) :: load_nume
    character(len=4), intent(in) :: load_type
    character(len=19), intent(in) :: ligrel_calc
    integer, intent(in) :: nb_in_maxi
    integer, intent(in) :: nb_in_prep
    character(len=*), intent(inout) :: lpain(nb_in_maxi)
    character(len=*), intent(inout) :: lchin(nb_in_maxi)
    character(len=19), intent(inout) :: resu_elem
    character(len=19), intent(in) :: vect_elem
    character(len=1), intent(in) :: base
    character(len=*), optional, intent(in) :: iden_direct
    character(len=*), optional, intent(in) :: name_inputz
!
! --------------------------------------------------------------------------------------------------
!
! Neumann loads computation
! 
! Elementary (on one load) - Vector
!
! --------------------------------------------------------------------------------------------------
!
! In  stop           : CALCUL subroutine comportement
! In  i_load         : index of current load
! In  load_name      : name of current load
! In  load_nume      : identification of load type
! In  load_type      : load type to compute - 'Dead', 'Pilo' or 'Suiv'
! In  ligrel_calc    : LIGREL to compute
! In  nb_in_maxi     : maximum number of input fields
! In  nb_in_prep     : number of input fields before specific ones
! IO  lpain          : list of input parameters
! IO  lchin          : list of input fields
! IO  resu_elem      : name of resu_elem
! In  vect_elem      : name of vect_elem
! In  base           : JEVEUX base to create vect_elem
! In  iden_direct    : direct identification of type
! In  name_inputz    : direct name of input field
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_type_neum
    parameter (nb_type_neum=18)
!
    integer :: iexist, i_type_neum, nb_in_add, ibid
    character(len=16) :: load_option
    character(len=24) :: load_ligrel  
    integer :: nbout, nbin
    character(len=8) :: lpaout, newnom
!
! --------------------------------------------------------------------------------------------------
!
    lpaout = 'PVECTUR'
!
    do i_type_neum = 1, nb_type_neum
!
! ----- Get information about load
!
        if (present(iden_direct)) then
            call load_neum_spec(load_name   , load_nume  , load_type  , ligrel_calc, i_type_neum,&
                                nb_type_neum, nb_in_maxi , nb_in_prep , lchin      , lpain      ,&
                                nb_in_add   , load_ligrel, load_option, iden_direct = iden_direct,&
                                name_inputz = name_inputz)
        else
            call load_neum_spec(load_name   , load_nume  , load_type  , ligrel_calc, i_type_neum,&
                                nb_type_neum, nb_in_maxi , nb_in_prep , lchin      , lpain      ,&
                                nb_in_add   , load_ligrel, load_option)
        endif
!
        if (load_option .ne. 'No_Load') then
!
! --------- Generate new RESU_ELEM name
!
            newnom = resu_elem(10:16)
            call gcnco2(newnom)
            resu_elem(10:16) = newnom(2:8)
            call corich('E', resu_elem, i_load, ibid)
!
! --------- Number of fields
!
            nbin  = nb_in_prep+nb_in_add
            nbout = 1
!
! --------- Computation (or not)
!
            if (load_option .eq. 'Copy_Load') then
                call copisd('CHAMP_GD', base, load_ligrel(1:8), resu_elem)
            else
                call calcul(stop , load_option, load_ligrel, nbin  , lchin,&
                            lpain, nbout      , resu_elem  , lpaout, base ,&
                            'OUI')
            endif
!
! --------- Copying output field
!
            call exisd('CHAMP_GD', resu_elem, iexist)
            ASSERT((iexist.gt.0).or.(stop.eq.'C'))
            call reajre(vect_elem, resu_elem, base)
        endif
    end do

end subroutine
