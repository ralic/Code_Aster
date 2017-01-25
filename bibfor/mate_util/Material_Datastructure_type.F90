module Material_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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

!
! --------------------------------------------------------------------------------------------------
!
! Material
!
! Define types for datastructures
!
! --------------------------------------------------------------------------------------------------
!

! - Type: components for a state variable
    type Mat_DS_VarcComp
        character(len=8)      :: phys_para_cmp
        character(len=8)      :: varc_cmp
    end type Mat_DS_VarcComp

! - Type: external state variable
    type Mat_DS_VarcAffe
        integer               :: indx_cata
        real(kind=8)          :: vale_refe
        character(len=16)     :: vale_phys_para
        character(len=8)      :: evol
        character(len=8)      :: type_affe
        character(len=16)     :: evol_prol_l
        character(len=16)     :: evol_prol_r
        character(len=8)      :: evol_func
        aster_logical         :: l_affe_tout
        integer, pointer      :: v_elem(:)
        integer               :: nb_elem
    end type Mat_DS_VarcAffe


! - Type: external state variable
    type Mat_DS_VarcCata
        character(len=8)               :: name
        character(len=8)               :: type_phys_para
        integer                        :: nb_cmp
        character(len=16)              :: field_type_def
        type(Mat_DS_VarcComp), pointer :: list_cmp(:)
    end type Mat_DS_VarcCata

! - Type: list of external state variables affected
    type Mat_DS_VarcListAffe
        type(Mat_DS_VarcAffe), pointer :: list_affe_varc(:)
        integer                        :: nb_affe_varc
        integer                        :: nb_varc_acti
        integer                        :: nb_varc_cmp
    end type Mat_DS_VarcListAffe

! - Type: catalog of external state variables
    type Mat_DS_VarcListCata
        type(Mat_DS_VarcCata), pointer :: list_cata_varc(:)
        integer                        :: nb_varc
    end type Mat_DS_VarcListCata
!
end module
