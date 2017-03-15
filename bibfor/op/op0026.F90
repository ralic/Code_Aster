subroutine op0026()
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/catabl.h"
#include "asterfort/infmaj.h"
#include "asterfort/utmess.h"
#include "asterfort/jemarq.h"
#include "asterfort/diinst.h"
#include "asterfort/nmchai.h"
#include "asterfort/jedema.h"
#include "asterfort/calcGetData.h"
#include "asterfort/calcGetDataMeca.h"
#include "asterfort/calcPrepDataMeca.h"
#include "asterfort/calcCalcMeca.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODifY
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
! ALONG WITH THIS PROGRAM; if NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!

!
! --------------------------------------------------------------------------------------------------
!
!  O P E R A T E U R    C A L C U L
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: zsolal = 17
    integer, parameter :: zvalin = 28
    character(len=19) :: hval_incr(zvalin), hval_algo(zsolal)
    integer, parameter :: nb_obje_maxi = 9
    character(len=16) :: obje_name(nb_obje_maxi)
    character(len=24) :: obje_sdname(nb_obje_maxi)
    integer :: nb_option, nume_inst
    integer :: long, nume_harm
    integer :: nb_obje
    real(kind=8) :: time_prev, time_curr
    character(len=8) :: table_new, table_old
    type(NL_DS_Constitutive) :: ds_constitutive
    character(len=16) :: list_option(4), phenom
    character(len=19) :: list_load
    character(len=19) :: list_inst
    character(len=24) :: model, mate, cara_elem
    character(len=24) :: varc_refe
    character(len=19) :: disp_prev, disp_cumu_inst, vari_prev, sigm_prev
    character(len=19) :: merigi, vediri, vefint, veforc, vevarc_prev, vevarc_curr
    aster_logical :: l_elem_nonl
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
! - Get commons data
!
    call calcGetData(table_new, table_old  ,&
                     nb_option, list_option,&
                     nume_inst, list_inst  ,&
                     phenom)
!
! - Get data
!
    if (phenom .eq. 'MECANIQUE') then
        call calcGetDataMeca(list_load      , model         , mate  , cara_elem,&
                             disp_prev      , disp_cumu_inst     , vari_prev, sigm_prev   ,&
                             ds_constitutive, l_elem_nonl, nume_harm)
    elseif (phenom .eq. 'THERMIQUE') then
        !call calcGetDataTher()
    else
        ASSERT(.false.)
    endif
!
! - Get current and previous times
!
    time_prev = diinst(list_inst,nume_inst-1)
    time_curr = diinst(list_inst,nume_inst)
!    partps(1) = time_prev
!    partps(2) = time_curr
!
! - Check lengths
!
    call nmchai('VALINC', 'LONMAX', long)
    ASSERT(long.eq.zvalin)
    call nmchai('SOLALG', 'LONMAX', long)
    ASSERT(long.eq.zsolal)
!
! - Prepare data
!
    if (phenom .eq. 'MECANIQUE') then
        call calcPrepDataMeca(model          , mate          , cara_elem,& 
                              disp_prev      , disp_cumu_inst, vari_prev, sigm_prev,&
                              time_prev      , time_curr     ,&
                              ds_constitutive, varc_refe     ,&
                              hval_incr      , hval_algo     ,&
                              merigi         , vediri        , vefint, veforc,&
                              vevarc_prev    , vevarc_curr   )
    elseif (phenom .eq. 'THERMIQUE') then
        !call calcPrepDataTher()
    else
        ASSERT(.false.)
    endif
!
! - Compute 
!
    if (phenom .eq. 'MECANIQUE') then
        call calcCalcMeca(nb_option   , list_option    , &
                          list_load   , model          , mate       , cara_elem,& 
                          l_elem_nonl , ds_constitutive, varc_refe  ,&
                          hval_incr   , hval_algo      ,&
                          merigi      , vediri         , vefint     , veforc,&
                          vevarc_prev , vevarc_curr    , nume_harm  ,&
                          nb_obje_maxi, obje_name      , obje_sdname, nb_obje)
    elseif (phenom .eq. 'THERMIQUE') then
        !call calcCalcTher()
    else
        ASSERT(.false.)
    endif
!
! - Table management
!
    call catabl(table_new, table_old, time_curr, nume_inst, nb_obje,&
                obje_name, obje_sdname)
!
    call jedema()
!
end subroutine
