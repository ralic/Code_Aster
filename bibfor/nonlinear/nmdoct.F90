subroutine nmdoct(mesh  , list_load       , sdcont_defi     , sdunil_defi , l_cont,&
                  l_unil, ligrel_link_cont, ligrel_link_xfem, sd_iden_rela)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/focste.h"
#include "asterfort/getvid.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/liscad.h"
#include "asterfort/lisccr.h"
#include "asterfort/liscli.h"
#include "asterfort/wkvect.h"
#include "asterfort/xrela_elim.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: list_load
    character(len=24), intent(out) :: sdcont_defi
    character(len=24), intent(out) :: sdunil_defi
    aster_logical, intent(out) :: l_cont
    aster_logical, intent(out) :: l_unil
    character(len=19), intent(out) :: ligrel_link_cont
    character(len=19), intent(out) :: ligrel_link_xfem
    character(len=24), intent(out) :: sd_iden_rela
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear algorithm - Initializations
!
! Get information about CONTACT
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  list_load        : list of loads
! Out sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! Out sdunil_defi      : name of unilateral condition datastructure (from DEFI_CONTACT)
! Out l_cont           : .true. if contact
! Out l_unil           : .true. if unilateral condition
! Out ligrel_link_cont : name of LIGREL for contact
! Out ligrel_link_xfem : name of LIGREL for contact with xfem
! Out sd_iden_rela     : name of object for identity relations between dof
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_info_maxi
    parameter   (nb_info_maxi=99)
    character(len=24) :: list_info_type(nb_info_maxi)
!
    character(len=16) :: keyw
    integer :: nb_load_cont, nb_load_init, nb_load_new, i_load, nb_info_type
    integer :: rel_lin_disc, rel_lin_xfem
    character(len=8) :: load_cont
    integer :: iform, i_neum_lapl
    character(len=8) :: ligrel_link_slav, ligrel_link
    character(len=19) :: list_load_new
    character(len=24) :: lload_info
    character(len=8) :: load_name, load_func, func_const
    real(kind=8) :: coef
    aster_logical :: l_cont_xfem_gg, l_cont_cont, l_cont_xfem, l_cont_disc, l_edge_elim
    character(len=8), pointer :: load_type(:) => null()
    integer, pointer :: v_load_info(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    keyw             = 'CONTACT'
    list_load_new    = '&&NMDOCT.LISCHA'
    sdcont_defi      = '&&NMDOCT.DEFIC'
    sdunil_defi      = '&&NMDOCT.DEFIU'
    ligrel_link_cont = ' '
    ligrel_link_xfem = ' '
    l_cont           = .false.
    l_unil           = .false.
    rel_lin_xfem     = 0
    rel_lin_disc     = 0
    sd_iden_rela     = ' '
!
! - Read previous list of load
!
    lload_info = list_load(1:19)//'.INFC'
    call jeveuo(lload_info, 'L', vi = v_load_info)
    nb_load_init = v_load_info(1)
    nb_load_new  = nb_load_init
!
! - Prepare constant function
!
    func_const = '&&NMDOCT'
    coef = 1.d0
    call focste(func_const, 'TOUTRESU', coef, 'V')
!
! - Get name of datastructure from DEFI_CONTACT
!
    call getvid(' ', keyw, scal=load_cont, nbret=nb_load_cont)
    if (nb_load_cont .le. 0) then   
        goto 999
    endif
!
! - Define datastructure names
!
    sdcont_defi = load_cont(1:8)//'.CONTACT'
    sdunil_defi = load_cont(1:8)//'.UNILATE'
!
! - Contact formulation
!
    iform = cfdisi(sdcont_defi,'FORMULATION')
    if (iform .eq. 4) then
        l_unil = .true.
    else
        l_cont = .true.
    endif
    l_edge_elim    = cfdisl(sdcont_defi,'ELIM_ARETE')
    l_cont_xfem_gg = cfdisl(sdcont_defi,'CONT_XFEM_GG')
    l_cont_disc    = iform.eq.1 
    l_cont_cont    = iform.eq.2 
    l_cont_xfem    = iform.eq.3
!
! - Contact - Continue: get list of elements for slave surface
!
    if (l_cont_cont) then
        ligrel_link_slav = load_cont(1:8)
        nb_load_new  = nb_load_new+1
    endif
!
! - Contact - Continue: prepare list of contact elements
!
    if (l_cont_cont) then
        ligrel_link_cont  = '&&LIGRCF.CHME.LIGRE'
        call wkvect(ligrel_link_cont(1:8)//'.TYPE', 'V V K8', 1, vk8 = load_type)
        load_type(1) = 'ME'
        nb_load_new = nb_load_new+1
    endif
!
! - Contact - XFEM (large sliding): get list of elements for slave surface
!
    if (l_cont_xfem_gg) then
        ligrel_link_slav = load_cont(1:8)
        nb_load_new = nb_load_new+1
    endif
!
! - Contact - XFEM (large sliding): prepare list of contact elements
!
    if (l_cont_xfem_gg) then
        ligrel_link_xfem = '&&LIGRXF.CHME.LIGRE'
        call wkvect(ligrel_link_xfem(1:8)//'.TYPE', 'V V K8', 1, vk8 = load_type)
        load_type(1) = 'ME'
        nb_load_new = nb_load_new+1
    endif
!
! - Contact - Discrete: list of linear relation (QUAD8)
!
    if (l_cont_disc) then
        ligrel_link = load_cont(1:8)
        call jeexin(ligrel_link//'.CHME.LIGRE.LGRF', rel_lin_disc)
        if (rel_lin_disc .ne. 0) then
            nb_load_new = nb_load_new+1
        endif
    endif
!
! -- Contact - XFEM: list of linear relations
!
    if (l_cont_xfem) then
        if (l_edge_elim) then
            call xrela_elim(mesh, sdcont_defi, sd_iden_rela)
        else
            ligrel_link = load_cont(1:8)
            call jeexin(ligrel_link//'.CHME.LIGRE.LGRF', rel_lin_xfem)
            if (rel_lin_xfem .ne. 0) then
                nb_load_new = nb_load_new+1
            endif
        endif
    endif
!
! - Add LIGREL to list of loads
!
    if (nb_load_new .ne. nb_load_init) then
!
! ----- Create new datastructure
!
        call lisccr('MECA', list_load_new, nb_load_new, 'V')
!
! ----- Copy old datastructure in new one
!
        do i_load = 1, nb_load_init
            nb_info_type = nb_info_maxi
            call liscli(list_load, i_load, nb_info_maxi, list_info_type, load_name,&
                        load_func, nb_info_type, i_neum_lapl)
            call liscad('MECA'      , list_load_new , i_load, load_name, load_func, &
                        nb_info_type, list_info_type, i_neum_laplz = i_neum_lapl)
        end do
!
! ----- Contact - Continue
!
        if (l_cont_cont) then
            i_load = nb_load_init + 1
            call liscad('MECA'        ,list_load_new, i_load, ligrel_link_slav, func_const,&
                        info_typez = 'ELEM_TARDIF')
            i_load = nb_load_init + 2
            call liscad('MECA'        ,list_load_new, i_load, ligrel_link_cont, func_const,&
                        info_typez = 'ELEM_TARDIF')
        endif
!
! ----- Contact - Discrete
!
        if (l_cont_disc) then
            if (rel_lin_disc .ne. 0) then
                i_load = nb_load_init + 1
                call liscad('MECA'        ,list_load_new, i_load, ligrel_link, func_const,&
                            info_typez = 'DIRI_CSTE')
            endif
        endif
!
! ----- Contact - XFEM
!
        if (l_cont_xfem) then
            if (l_cont_xfem_gg) then
                i_load = nb_load_init + 1
                call liscad('MECA'        ,list_load_new, i_load, ligrel_link_slav, func_const,&
                            info_typez = 'ELEM_TARDIF')
                i_load = nb_load_init + 2
                call liscad('MECA'        ,list_load_new, i_load, ligrel_link_xfem, func_const,&
                            info_typez = 'ELEM_TARDIF')
            endif
            if (rel_lin_xfem .ne. 0) then
                i_load = nb_load_init + 1
                call liscad('MECA'        ,list_load_new, i_load, ligrel_link, func_const,&
                            info_typez = 'DIRI_CSTE')
            endif
        endif
!
! ----- Copy and clean
!
        call lisccr('MECA', list_load, nb_load_new, 'V')
        call copisd(' ', 'V', list_load_new, list_load)
        call detrsd('LISTE_CHARGES', list_load_new)
!
    endif
!
999 continue
end subroutine

