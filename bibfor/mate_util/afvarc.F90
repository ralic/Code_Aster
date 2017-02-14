subroutine afvarc(chmate, mesh, model)
!
use Material_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/afvarc_read.h"
#include "asterfort/afvarc_obje_crea.h"
#include "asterfort/afvarc_obje_affe.h"
#include "asterfort/afvarc_shrink.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=8), intent(in) :: chmate
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
!
! --------------------------------------------------------------------------------------------------
!
! Material - External state variables (VARC)
!
! For AFFE_MATERIAU/AFFE_VARC
!
! --------------------------------------------------------------------------------------------------
!
! In  chmate           : name of material field (CHAM_MATER)
! In  mesh             : name of mesh
! In  model            : name of model
!
! --------------------------------------------------------------------------------------------------
!
    type(Mat_DS_VarcListCata) :: varc_cata
    type(Mat_DS_VarcListAffe) :: varc_affe
    integer :: nb_affe_varc
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Read data
!
    call afvarc_read(varc_cata, varc_affe)
    nb_affe_varc = varc_affe%nb_affe_varc
!
    if (nb_affe_varc .ne. 0) then
! ----- Create objects
        call afvarc_obje_crea('G', chmate, mesh, varc_cata, varc_affe)
! ----- Affect values in objects
        call afvarc_obje_affe(chmate, mesh, model, varc_cata, varc_affe)
! ----- Shrink number of components to save memory
        call afvarc_shrink(chmate, varc_affe)
    endif
!
    call jedema()
end subroutine
