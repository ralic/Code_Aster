subroutine comp_read_mesh(mesh          , keywordfact, iocc        ,&
                          list_elem_affe, l_affe_all , nb_elem_affe)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/reliem.h"
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
    character(len=8), intent(in) :: mesh
    character(len=16), intent(in) :: keywordfact
    integer, intent(in) :: iocc
    character(len=24), intent(in) :: list_elem_affe
    aster_logical, intent(out) :: l_affe_all
    integer, intent(out):: nb_elem_affe
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Get list of elements where comportment is defined
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  keywordfact      : factor keyword to read (COMPORTEMENT)
! In  iocc             : factor keyword index
! In  list_elem_affe   : name of JEVEUX object for list of elements where comportment is defined
! Out l_affe_all       : .true. if all elements were affected
! Out nb_elem_affe     : number of elements where comportment is defined
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8), parameter :: keyw_type(2) = (/'GROUP_MA', 'MAILLE  '/)
    character(len=16), parameter :: keyw_name(2) = (/'GROUP_MA', 'MAILLE  '/)
    integer :: nt
!
! --------------------------------------------------------------------------------------------------
!
    l_affe_all   = .false._1
    nb_elem_affe = 0
!
! - Get list of elements
!
    call getvtx(keywordfact, 'TOUT', iocc = iocc, nbret = nt)
    if (nt .ne. 0) then
        l_affe_all = .true.
    else
        l_affe_all = .false.
        call reliem(' ', mesh     , 'NU_MAILLE', keywordfact   , iocc,&
                    2  , keyw_name, keyw_type  , list_elem_affe, nb_elem_affe)
        if (nb_elem_affe .eq. 0) then
            l_affe_all = .true.
        endif
    endif
!
end subroutine
