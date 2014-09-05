subroutine nmext1(mesh          , field    , field_disc   , field_type, field_s  ,&
                  nb_elem       , nb_node  , nb_poin      , nb_spoi   , nb_cmp   ,&
                  type_extr_elem, type_extr, type_extr_cmp, list_node , list_elem,&
                  list_poin     , list_spoi, list_cmp     , work_node , work_poin,&
                  work_elem)
!
implicit none
!
#include "asterfort/nmext2.h"
#include "asterfort/nmext3.h"
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
! aslint: disable=W1504
!
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nb_node
    integer, intent(in) :: nb_elem
    integer, intent(in) :: nb_poin
    integer, intent(in) :: nb_spoi
    integer, intent(in) :: nb_cmp
    character(len=19), intent(in) :: field
    character(len=24), intent(in) :: field_type
    character(len=24), intent(in) :: field_s
    character(len=4), intent(in) :: field_disc
    character(len=24), intent(in) :: list_node
    character(len=24), intent(in) :: list_elem
    character(len=24), intent(in) :: list_poin
    character(len=24), intent(in) :: list_spoi
    character(len=24), intent(in) :: list_cmp
    character(len=8), intent(in) :: type_extr
    character(len=8), intent(in) :: type_extr_elem
    character(len=8), intent(in) :: type_extr_cmp
    character(len=19), intent(in) :: work_poin
    character(len=19), intent(in) :: work_node
    character(len=19), intent(in) :: work_elem
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Field extraction datastructure
!
! Extract values and store them in working vectors
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  field            : name of field
! In  field_type       : type of field (name in results datastructure)
! In  field_disc       : localization of field (discretization: NOEU or ELGA)
! In  field_s          : name of reduced field (CHAM_ELEM_S)
! In  nb_node          : number of nodes
! In  nb_elem          : number of elements
! In  nb_poin          : number of points (Gauss)
! In  nb_spoi          : number of subpoints
! In  nb_cmp           : number of components
! In  work_node        : working vector to save node values
! In  work_elem        : working vector to save element values
! In  work_poin        : working vector to save point (Gauss) values
! In  list_node        : name of object contains list of nodes
! In  list_elem        : name of object contains list of elements
! In  list_poin        : name of object contains list of points (Gauss)
! In  list_spoi        : name of object contains list of subpoints
! In  list_cmp         : name of object contains list of components
! In  type_extr        : type of extraction
! In  type_extr_elem   : type of extraction by element
! In  type_extr_cmp    : type of extraction for components
!
! --------------------------------------------------------------------------------------------------
!
!
! - For nodal values
!
    if (field_disc .eq. 'NOEU') then
        call nmext2(mesh         , field    , nb_cmp  , nb_node  , type_extr,&
                    type_extr_cmp, list_node, list_cmp, work_node)
    endif
!
! - For point (Gauss) values)
!
    if (field_disc .eq. 'ELGA') then
        call nmext3(mesh         , field    , field_type, field_s       , nb_cmp   ,&
                    nb_elem      , nb_poin  , nb_spoi   , type_extr_elem, type_extr,&
                    type_extr_cmp, list_elem, list_poin , list_spoi     , list_cmp ,&
                    work_poin    , work_elem)
    endif
!
end subroutine
