subroutine model_check(model, l_veri_elem)
!
    implicit none
!
#include "asterfort/calcul.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/modexi.h"
#include "asterfort/utmess.h"
#include "asterfort/taxis.h"
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
!
    character(len=8), intent(in) :: model
    logical(kind=1), optional, intent(in) :: l_veri_elem
!
! --------------------------------------------------------------------------------------------------
!
! Checking model
!
! --------------------------------------------------------------------------------------------------
!
! In  model        : name of the model
! In  l_veri_elem  : .true. if check jacobian (element quality)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_dim_geom, nb_dim_geom2, nb_dim_geom3
    character(len=16) :: repk
    integer :: i_disc_2d, i_disc_3d
    character(len=8) :: mesh
    logical(kind=1) :: l_axis
    integer :: nb_mesh_elem
    character(len=19) :: ligrel_model
    character(len=24) :: model_maille
    integer, pointer  :: p_model_maille(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call dismoi('NOM_MAILLA'  , model, 'MODELE'  , repk = mesh)
    call dismoi('NB_MA_MAILLA', mesh , 'MAILLAGE', repi = nb_mesh_elem)
    call dismoi('AXIS'        , model, 'MODELE'  , repk = repk)
    l_axis = repk.eq.'OUI'
    call dismoi('DIM_GEOM'    , model, 'MODELE'  , repi = nb_dim_geom)
    
    ligrel_model = model//'.MODELE'
!
! - Check topological(kind=1) dimensions
!
    if (nb_dim_geom .gt. 3) then
        nb_dim_geom2 = 0
        call utmess('A', 'MODELE1_14')
    else
        nb_dim_geom2=3
        nb_dim_geom3=3
        call dismoi('Z_CST', mesh, 'MAILLAGE', repk=repk)
        if (repk .eq. 'OUI') then
            nb_dim_geom2=2
            call dismoi('Z_ZERO', mesh, 'MAILLAGE', repk=repk)
            if (repk .eq. 'OUI') then
                nb_dim_geom3=2
            endif
        endif
!
        if ((nb_dim_geom.eq.3) .and. (nb_dim_geom2.eq.2)) then
!
! --------- Correct: shells elements with Z=Constant
!
        else if ((nb_dim_geom.eq.2) .and. (nb_dim_geom2.eq.3)) then
!
! --------- Warning: 2D model with 3D mesh
!
            call utmess('A', 'MODELE1_53')
        elseif ((nb_dim_geom.eq.2) .and. &
                (nb_dim_geom2.eq.2).and. &
                (nb_dim_geom3.eq.3)) then
!
! --------- Something strange: 2D with Z=Constant but Z<>0
!
            call utmess('A', 'MODELE1_58')
        endif
    endif
!
! - DISCRET elements: only 2D OR 3D
!
    call modexi(model, 'DIS_', i_disc_3d)
    call modexi(model, '2D_DIS_', i_disc_2d)
    if (nb_dim_geom2 .eq. 2 .and. i_disc_3d .eq. 1 .and. i_disc_2d .eq. 1) then
        call utmess('F', 'MODELE1_54')
    endif
!
! - Check if X>0 for axis elements
!
    if (l_axis) then
        model_maille = model//'.MAILLE'
        call jeveuo(model_maille, 'L', vi = p_model_maille)
        call taxis(mesh, p_model_maille, nb_mesh_elem)
    endif
!
! - ON VERIFIE QUE LA GEOMETRIE DES MAILLES N'EST PAS TROP CHAHUTEE
!
    if (present(l_veri_elem)) then
        if (l_veri_elem) then
            call calcul('C', 'VERI_JACOBIEN', ligrel_model, 1, mesh//'.COORDO',&
                        'PGEOMER', 1, '&&OP0018.CODRET', 'PCODRET', 'V',&
                        'OUI')
        endif
    endif
end subroutine
