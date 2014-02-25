subroutine char_affe_neum(mesh, ndim, keywordfact, iocc, nb_carte,&
                          carte, nb_cmp)
!
    implicit none
!
#include "asterfort/getelem.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/vetyma.h"
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: ndim
    character(len=16), intent(in) :: keywordfact
    integer, intent(in) :: iocc
    integer, intent(in) :: nb_carte
    character(len=19), intent(in) :: carte(nb_carte)
    integer, intent(in) :: nb_cmp(nb_carte)
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Apply Neumann loads in <CARTE> with elements type check
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh         : name of mesh
! In  ndim         : space dimension
! In  keywordfact  : factor keyword to read elements
! In  iocc         : factor keyword index in AFFE_CHAR_MECA
! In  nb_carte     : number of <CARTE> for this Neumann load
! In  carte        : <CARTE> for this Neumann load
! In  nb_cmp       : number of components in the <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: load_type
    character(len=24) :: list_elem
    integer, pointer :: p_list_elem(:) => null()
    integer :: nb_elem
    integer :: codret, i_carte
!
! --------------------------------------------------------------------------------------------------
!
    list_elem = '&&LIST_ELEM'
    load_type = keywordfact
!
! - Elements to apply
!
    call getelem(mesh, keywordfact, iocc, 'A', list_elem, &
                 nb_elem)
    if (nb_elem .ne. 0) then
!
! ----- Check elements
!
        call jeveuo(list_elem, 'L', vi = p_list_elem)
        do i_carte = 1, nb_carte
            if (nb_cmp(i_carte) .ne. 0) then
                call vetyma(mesh, ndim, keywordfact, list_elem, nb_elem,&
                            codret)
            endif
        end do
!
! ----- Apply Neumann loads in <CARTE>
!
        do i_carte = 1, nb_carte
            if (nb_cmp(i_carte) .ne. 0) then
                call nocart(carte(i_carte), 3, nb_cmp(i_carte), mode='NUM', nma=nb_elem,&
                            limanu=p_list_elem)
            endif
        end do
!
    endif
!
    call jedetr(list_elem)
!
end subroutine
