subroutine load_neut_resi(stop_calc , model     , time , load_name, load_nume,&
                          nb_in_maxi, nb_in_prep, lpain, lchin    , base     ,&
                          resu_elem , vect_elem )
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
    character(len=19), intent(in) :: vect_elem
    character(len=1), intent(in) :: base
!
! --------------------------------------------------------------------------------------------------
!
! Neumann loads computation - Thermic
! 
! Elementary (on one load) - Residuals
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of the model
! In  time             : time (<CARTE>)
! In  load_name        : name of current load
! In  load_nume        : identification of load type
! In  nb_in_maxi       : maximum number of input fields
! In  nb_in_prep       : number of input fields before specific ones
! IO  lpain            : list of input parameters
! IO  lchin            : list of input fields
! IO  resu_elem        : name of resu_elem
! In  vect_elem        : name of vect_elem
! In  base             : JEVEUX base to create vect_elem
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_type_neum
    parameter (nb_type_neum=9)
!
    integer :: i_type_neum, nb_in_add, ibid
    character(len=16) :: resi_option
    character(len=24) :: load_ligrel  
    integer :: nbout, nbin
    character(len=8) :: lpaout, newnom
!
! --------------------------------------------------------------------------------------------------
!
    lpaout = 'PRESIDU'
!
    do i_type_neum = 1, nb_type_neum
!
! ----- Get information about load
!
        call load_neut_spec('STAT'      , model       , time       , load_name , load_nume,&
                            i_type_neum , nb_type_neum, nb_in_maxi , nb_in_prep, lchin    ,&
                            lpain       , nb_in_add   , load_ligrel,&
                            resi_option_ = resi_option)
!
        if (resi_option .ne. 'No_Load') then
!
! --------- Generate new RESU_ELEM name
!
            newnom = resu_elem(10:16)
            call gcnco2(newnom)
            resu_elem(10:16) = newnom(2:8)
            call corich('E', resu_elem, -1, ibid)
!
! --------- Number of fields
!
            nbin  = nb_in_prep+nb_in_add
            nbout = 1
!
! --------- Computation
!
            call calcul(stop_calc, resi_option, load_ligrel, nbin  , lchin,&
                        lpain    , nbout      , resu_elem  , lpaout, base ,&
                        'OUI')
!
! --------- Add RESU_ELEM in vect_elem
!
            call reajre(vect_elem, resu_elem, base)
        endif
    end do

end subroutine
