subroutine apcinv(mesh, sdappa, i_zone)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cncinv.h"
#include "asterfort/cnvois.h"
#include "asterfort/codent.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
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
!
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: sdappa
    integer, intent(in) :: i_zone
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Create objects for inverse connectivity
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sdappa           : name of pairing datastructure
! In  i_zone           : index of contact zone
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: knuzo
    character(len=24) :: cnivma, cnives
    integer :: nb_elem_mast, nb_elem_slav
    character(len=24) :: sdappa_mast, sdappa_slav 
    character(len=24) :: sdappa_slne, sdappa_mane
    integer :: mast_indx_maxi , slav_indx_maxi, mast_indx_mini, slav_indx_mini
    integer, pointer :: v_sdappa_mast(:) => null()
    integer, pointer :: v_sdappa_slav(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!
! - Generate name of objects
!
    ASSERT(i_zone .le. 9)
    call codent(i_zone, 'G', knuzo)
    sdappa_mane = sdappa(1:19)//'.MAN'//knuzo(1:1)
    sdappa_slne = sdappa(1:19)//'.ESN'//knuzo(1:1)
    sdappa_mast = sdappa(1:19)//'.MAS'//knuzo(1:1)
    sdappa_slav = sdappa(1:19)//'.ESC'//knuzo(1:1)
!
! - Access to objects
!
    call jelira(sdappa_mast, 'LONMAX', nb_elem_mast)
    call jelira(sdappa_slav, 'LONMAX', nb_elem_slav) 
    call jeveuo(sdappa_mast, 'L', vi = v_sdappa_mast)
    call jeveuo(sdappa_slav, 'L', vi = v_sdappa_slav)
!
! - Get parameters
!
    mast_indx_maxi = maxval(v_sdappa_mast)
    slav_indx_maxi = maxval(v_sdappa_slav)
    mast_indx_mini = minval(v_sdappa_mast)
    slav_indx_mini = minval(v_sdappa_slav)
!
! - Create inverse connectivities
!
    cnivma = '&&aplcpg_cnivma'
    cnives = '&&aplcpg_cnives'
    call cncinv(mesh, v_sdappa_slav, nb_elem_slav, 'V', cnives)
    call cncinv(mesh, v_sdappa_mast, nb_elem_mast, 'V', cnivma)
!
! - Create neighbouring objects
!
    call jedetr(sdappa_slne)
    call jedetr(sdappa_mane) 
    call cnvois(mesh  , v_sdappa_slav, nb_elem_slav, slav_indx_mini, slav_indx_maxi,&
                cnives, sdappa_slne)
    call cnvois(mesh  , v_sdappa_mast, nb_elem_mast, mast_indx_mini, mast_indx_maxi,&
                cnivma, sdappa_mane)
!
! - Cleaning
!
    call jedetr(cnivma)
    call jedetr(cnives)
!
end subroutine        
