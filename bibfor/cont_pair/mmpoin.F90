subroutine mmpoin(mesh, ds_contact, newgeo, sdappa)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmex.h"
#include "asterfort/cfnumm.h"
#include "asterfort/infdbg.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mcomce.h"
#include "asterfort/mmelin.h"
#include "asterfort/mmgaus.h"
#include "asterfort/mminfi.h"
#include "asterfort/mmnpoi.h"
#include "asterfort/mmnumn.h"
#include "asterfort/mmvalp.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: newgeo
    character(len=19), intent(in) :: sdappa
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue method - Fill pairing datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  mesh             : name of mesh
! In  ds_contact       : datastructure for contact management
! In  newgeo           : name of field for geometry update from initial coordinates of nodes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=24) :: sdappa_poin, sdappa_infp, sdappa_noms
    real(kind=8), pointer :: v_sdappa_poin(:) => null()
    integer, pointer :: v_sdappa_infp(:) => null()
    character(len=16), pointer :: v_sdappa_noms(:) => null()
    integer :: i_node_escl, i_poin, i_poin_elem, i_zone, i_elem_slav
    integer :: nb_elem_slav, nb_poin_elem, elem_slav_nbnode, nt_poin
    integer :: elem_slav_indx, elem_slav_nume, node_slav_nume
    integer :: jdecme
    integer :: type_inte
    real(kind=8) :: poin_coor(3), elem_slav_coor(27)
    real(kind=8) :: ksi1, ksi2
    character(len=8) :: elem_slav_type, elem_slav_name
    character(len=16) :: poin_name
    integer :: model_ndim, nb_cont_zone
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ......... PREPARATION DE L''APPARIEMENT'
    endif
!
! - Access to pairing datastructure
!
    sdappa_poin = sdappa(1:19)//'.POIN'
    sdappa_infp = sdappa(1:19)//'.INFP'
    sdappa_noms = sdappa(1:19)//'.NOMS'
    call jeveuo(sdappa_poin, 'E', vr   = v_sdappa_poin)
    call jeveuo(sdappa_infp, 'E', vi   = v_sdappa_infp)
    call jeveuo(sdappa_noms, 'E', vk16 = v_sdappa_noms)
!
! - Get parameters
!
    model_ndim   = cfdisi(ds_contact%sdcont_defi, 'NDIM' )
    nb_cont_zone = cfdisi(ds_contact%sdcont_defi, 'NZOCO')
!
! - Loop on contact zones
!
    i_poin  = 1
    nt_poin = 0
    do i_zone = 1, nb_cont_zone
!
! ----- Get parameters on current zone
!
        nb_elem_slav = mminfi(ds_contact%sdcont_defi, 'NBMAE' , i_zone)
        jdecme       = mminfi(ds_contact%sdcont_defi, 'JDECME', i_zone)
        type_inte    = mminfi(ds_contact%sdcont_defi,'INTEGRATION',i_zone )
!
! ----- Loop on slave elements
!
        do i_elem_slav = 1, nb_elem_slav
!
! --------- Get current slave element
!
            elem_slav_indx = jdecme + i_elem_slav
            call cfnumm(ds_contact%sdcont_defi, elem_slav_indx, elem_slav_nume)
            call jenuno(jexnum(mesh//'.NOMMAI', elem_slav_nume), elem_slav_name)
!
! --------- Get coordinates of slave element
!
            call mcomce(mesh, newgeo, elem_slav_nume, elem_slav_coor, elem_slav_type,&
                        elem_slav_nbnode)
!
! --------- Number of contact (integration) points on current slave element
!
            call mmelin(mesh, elem_slav_nume, type_inte, nb_poin_elem)
!
! --------- Loop on contact (integration) points
!
            do i_poin_elem = 1, nb_poin_elem
!
! ------------- Get current contact point
!
                call mmnumn(mesh            , type_inte  , elem_slav_nume,&
                            elem_slav_nbnode, i_poin_elem, node_slav_nume)
!
! ------------- Parameters of current integration point
!
                call mmgaus(elem_slav_type, type_inte, i_poin_elem, ksi1, ksi2)
!
! ------------- Coordinates of contact point
!
                call mmvalp(model_ndim, elem_slav_type, elem_slav_nbnode, 3        ,&
                            ksi1      , ksi2          , elem_slav_coor  , poin_coor)
                v_sdappa_poin(3*(i_poin-1)+1) = poin_coor(1)
                v_sdappa_poin(3*(i_poin-1)+2) = poin_coor(2)
                v_sdappa_poin(3*(i_poin-1)+3) = poin_coor(3)
!
! ------------- Node is excluded ?
!
                if (node_slav_nume .gt. 0) then
                    call cfmmex(ds_contact%sdcont_defi, 'CONT', i_zone, node_slav_nume, i_node_escl)
                    v_sdappa_infp(i_poin) = i_node_escl
                endif
!
! ------------- Name of point
!
                call mmnpoi(mesh, elem_slav_name, node_slav_nume, i_poin_elem, poin_name)
                v_sdappa_noms(i_poin) = poin_name
!
! ------------- Next point
!
                i_poin  = i_poin + 1
                nt_poin = nt_poin + 1
            end do
        end do
    end do
!
    ASSERT(nt_poin.eq.cfdisi(ds_contact%sdcont_defi, 'NTPT'))
!
end subroutine
