subroutine comp_meca_mod(mesh       , model       ,&
                         keywordfact, iocc        , rela_comp,&
                         model_dim  , model_mfront)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/comp_read_mesh.h"
#include "asterfort/teattr.h"
#include "asterfort/utmess.h"
#include "asterc/lccree.h"
#include "asterc/lcdiscard.h"
#include "asterc/lctest.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: model
    character(len=8), intent(in) :: mesh
    character(len=16), intent(in) :: keywordfact
    integer, intent(in) :: iocc
    character(len=16), intent(in) :: rela_comp
    integer, intent(out) :: model_dim
    character(len=16), intent(out) :: model_mfront
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Find dimension and type of modelisation for MFront
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  keywordfact      : factor keyword to read (COMPORTEMENT)
! In  iocc             : factor keyword index
! In  rela_comp        : RELATION comportment
! Out model_dim        : dimension of modelisation (2D or 3D)
! Out model_mfront     : type of modelisation MFrront
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_elem_affe, nb_elem, i_elem, elem_nume
    integer :: elem_type_nume, iret
    aster_logical :: l_affe_all, l_mfront_cp
    character(len=24) :: list_elem_affe
    character(len=16) :: elem_type_name, model_type, model_type_save, principal, model_thm
    character(len=16) :: rela_comp_py
    character(len=1) :: d1
    integer, pointer :: v_model_elem(:) => null()
    integer, pointer :: v_elem_affe(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    model_mfront    = ' '
    model_dim       = 0
    model_type_save = ' '
    list_elem_affe  = '&&COMPMECASAVE.LIST'
!
! - Access to model
!
    call jeveuo(model//'.MAILLE', 'L', vi = v_model_elem)
!
! - Get list of elements where comportment is defined
!
    call comp_read_mesh(mesh          , keywordfact, iocc        ,&
                        list_elem_affe, l_affe_all , nb_elem_affe)
    if (l_affe_all) then
        call dismoi('NB_MA_MAILLA', mesh, 'MAILLAGE', repi=nb_elem)
    else
        call jeveuo(list_elem_affe, 'L', vi = v_elem_affe)
        nb_elem = nb_elem_affe
    endif
!
! - Coding comportment
!
    call lccree(1, rela_comp, rela_comp_py)
    call lctest(rela_comp_py, 'MODELISATION', 'C_PLAN', iret)
    l_mfront_cp = iret .ne. 0
!
! - Loop on elements
!
    do i_elem = 1, nb_elem
!
! ----- Current element
!
        if (l_affe_all) then
            elem_nume = i_elem
        else
            elem_nume = v_elem_affe(i_elem)
        endif
!
! ----- Type of finite element
!
        elem_type_nume = v_model_elem(elem_nume)
        if (elem_type_nume .ne. 0) then
            call jenuno(jexnum('&CATA.TE.NOMTE', elem_type_nume), elem_type_name)
!
! --------- Get parameters
!
            call teattr('C', 'TYPMOD'         , model_type, iret, typel=elem_type_name)
            call teattr('C', 'PRINCIPAL'      , principal , iret, typel=elem_type_name)
            call teattr('C', 'THM'            , model_thm , iret, typel=elem_type_name)
            call teattr('C', 'DIM_TOPO_MODELI', d1        , iret, typel=elem_type_name)
            read(d1,'(I1)') model_dim
            if (principal .eq. 'OUI') then
                if (model_type_save .eq. ' ') then
                    model_type_save = model_type
                endif
                if (model_type_save .ne. model_type) then
                    call utmess('F','COMPOR4_13', si=iocc, nk=2, &
                                valk=[model_type_save, model_type])
                endif
                if ( model_type .eq. 'COMP3D' ) then
                    model_mfront = '_Tridimensional'
                elseif ( model_type .eq. 'C_PLAN' ) then
! Deborst algorithm
                    if (l_mfront_cp) then
                        model_mfront = '_PlaneStress'
                    else
                        model_mfront = '_Axisymmetrical'
                        model_dim    = 2
                    endif
                elseif ( model_type .eq. 'D_PLAN' ) then
                    model_mfront = '_PlaneStrain'
                elseif ( model_type .eq. 'AXIS' ) then
                    model_mfront = '_Axisymmetrical'
                elseif ( model_type .eq. 'COMP1D' ) then
! Deborst algorithm
                    model_mfront = '_Axisymmetrical'
                    model_dim    = 2
                elseif ( model_thm .eq. 'OUI' ) then
                    model_mfront = '_Tridimensional'
                else
                    call utmess('F','COMPOR4_14')
                endif
            endif
        endif
    end do
!
    call lcdiscard(rela_comp_py)
!
end subroutine
