subroutine char_crea_cart(phenom  , load_type, load, mesh, vale_type,&
                          nb_carte, carte)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lisnnl.h"
#include "asterfort/nocart.h"
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
    character(len=*), intent(in) :: phenom
    character(len=16), intent(in) :: load_type
    character(len=8), intent(in) :: load
    character(len=8), intent(in) :: mesh
    character(len=4), intent(in) :: vale_type
    integer, intent(out) :: nb_carte
    character(len=19), intent(out) :: carte(*)
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Creation and initialization to zero of <CARTE> for Neumann loads
!
! --------------------------------------------------------------------------------------------------
!
! In  phenom       : phenomenon (MECANIQUE/THERMIQUE/ACOUSTIQUE)
! In  load_type    : type of load
! In  mesh         : name of mesh
! In  load         : name of load
! In  vale_type    : affected value type (real, complex or function)
! Out nb_carte     : number of <CARTE> for this Neumann load
! Out carte        : <CARTE> for this Neumann load
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cmp(2)
    character(len=8) :: name_cmp(2, 7)
    character(len=13) :: obje_pref
    character(len=8) :: gran_name(2)
    character(len=4) :: cart_type(2)
    integer :: jvalv, i_carte, i_cmp, iret
    aster_logical :: l_init(2)
    character(len=8), pointer :: ncmp(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Prefix of <CARTE> objects
!
    call lisnnl(phenom, load, obje_pref)
!
! - Number of <CARTE> objects - TODO: using lisdef utility
!
    if (load_type.eq.'EFFE_FOND') then
        nb_carte = 2
    else if (load_type.eq.'ONDE_PLANE') then
        nb_carte = 2
    else if (load_type.eq.'ROTATION') then
        nb_carte = 1
    else
        ASSERT(.false.)
    endif
    ASSERT(nb_carte.le.2)
!
! - Name of the <CARTE> - TODO: using lisdef utility
!
    if (load_type .eq. 'EFFE_FOND') then
        carte(1) = obje_pref(1:13)//'.EFOND'
        carte(2) = obje_pref(1:13)//'.PREFF'
    else if (load_type.eq.'ONDE_PLANE') then
        carte(1) = obje_pref(1:13)//'.ONDPL'
        carte(2) = obje_pref(1:13)//'.ONDPR'
    else if (load_type.eq.'ROTATION') then
        carte(1) = obje_pref(1:13)//'.ROTAT'
    else
        ASSERT(.false.)
    endif
!
! - Name of the <GRANDEUR> - TODO: using lisdef utility
!
    if (load_type .eq. 'EFFE_FOND') then
        if (vale_type .eq. 'REEL') then
            gran_name(1) = 'NEUT_R'
            gran_name(2) = 'PRES_R'
        else if (vale_type.eq.'FONC') then
            gran_name(1) = 'NEUT_R'
            gran_name(2) = 'PRES_F'
        else
            ASSERT(.false.)
        endif
    else if (load_type.eq.'ONDE_PLANE') then
        if (vale_type .eq. 'FONC') then
            gran_name(1) = 'NEUT_K8'
            gran_name(2) = 'NEUT_R'
        else
            ASSERT(.false.)
        endif
    else if (load_type.eq.'ROTATION') then
        if (vale_type .eq. 'REEL') then
            gran_name(1) = 'ROTA_R'
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
!
! - Type of the <CARTE> - TODO: using lisdef utility
!
    if (load_type .eq. 'EFFE_FOND') then
        if (vale_type .eq. 'REEL') then
            cart_type(1) = 'R'
            cart_type(2) = 'R'
        else if (vale_type.eq.'FONC') then
            cart_type(1) = 'R'
            cart_type(2) = 'K8'
        else
            ASSERT(.false.)
        endif
    else if (load_type.eq.'ONDE_PLANE') then
        if (vale_type .eq. 'FONC') then
            cart_type(1) = 'K8'
            cart_type(2) = 'R'
        else
            ASSERT(.false.)
        endif
    else if (load_type.eq.'ROTATION') then
        if (vale_type .eq. 'REEL') then
            cart_type(1) = 'R'
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
!
! - Components of the <CARTE> - TODO: using lisdef utility
!
    if (load_type .eq. 'EFFE_FOND') then
        nb_cmp(1) = 1
        nb_cmp(2) = 1
        name_cmp(1,1) = 'X1'
        name_cmp(2,1) = 'PRES'
    else if (load_type.eq.'ONDE_PLANE') then
        nb_cmp(1) = 2
        nb_cmp(2) = 6
        name_cmp(1,1) = 'Z1'
        name_cmp(1,2) = 'Z2'
        name_cmp(2,1) = 'X1'
        name_cmp(2,2) = 'X2'
        name_cmp(2,3) = 'X3'
        name_cmp(2,4) = 'X4'
        name_cmp(2,5) = 'X5'
        name_cmp(2,6) = 'X6'
    else if (load_type.eq.'ROTATION') then
        nb_cmp(1) = 7
        name_cmp(1,1) = 'OME'
        name_cmp(1,2) = 'AR'
        name_cmp(1,3) = 'BR'
        name_cmp(1,4) = 'CR'
        name_cmp(1,5) = 'X'
        name_cmp(1,6) = 'Y'
        name_cmp(1,7) = 'Z'
    else
        ASSERT(.false.)
    endif
!
! - Creation of the <CARTE>
!
    do i_carte = 1, nb_carte
        call exisd('CARTE', carte(i_carte), iret)
        if (iret .eq. 0) then
            call alcart('G', carte(i_carte), mesh, gran_name(i_carte))
            l_init(i_carte) = .true.
        else
            l_init(i_carte) = .false.
        endif
    enddo
!
! - Initialization of the <CARTE>
!
    do i_carte = 1, nb_carte
        if (l_init(i_carte)) then
            call jeveuo(carte(i_carte)//'.NCMP', 'E', vk8=ncmp)
            call jeveuo(carte(i_carte)//'.VALV', 'E', jvalv)
            do i_cmp = 1, nb_cmp(i_carte)
                ncmp(i_cmp) = name_cmp(i_carte,i_cmp)
                if (cart_type(i_carte) .eq. 'R') then
                    zr(jvalv-1+i_cmp) = 0.d0
                else if (cart_type(i_carte) .eq.'K8') then
                    zk8(jvalv-1+i_cmp) = '&FOZERO'
                else
                    ASSERT(.false.)
                endif
            enddo
            call nocart(carte(i_carte), 1, nb_cmp(i_carte))
        endif
    enddo
!
    call jedema()
end subroutine
