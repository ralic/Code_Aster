subroutine nmvarc_prep(type_comp, model    , cara_elem, mate     , varc_refe,&
                       compor   , exis_temp, mxchin   , nbin     , lpain    ,&
                       lchin    , mxchout  , nbout    , lpaout   , lchout   ,&
                       sigm_prev, vari_prev, varc_prev, varc_curr)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/alchml.h"
#include "asterfort/detrsd.h"
#include "asterfort/exixfe.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/nmvcex.h"
#include "asterfort/xajcin.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=1), intent(in) :: type_comp
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: varc_refe
    character(len=24), intent(in) :: cara_elem
    character(len=24), intent(in) :: compor
    aster_logical, intent(in) :: exis_temp
    integer, intent(in) :: mxchin
    character(len=8), intent(inout) :: lpain(mxchin)
    character(len=19), intent(inout) :: lchin(mxchin)
    integer, intent(out) :: nbin
    integer, intent(in) :: mxchout
    character(len=8), intent(inout) :: lpaout(mxchout)
    character(len=19), intent(inout) :: lchout(mxchout)
    integer, intent(out) :: nbout
    character(len=19), intent(in) :: sigm_prev
    character(len=19), intent(in) :: vari_prev
    character(len=19), intent(in) :: varc_prev
    character(len=19), intent(in) :: varc_curr
!
! --------------------------------------------------------------------------------------------------
!
! Nonlinear mechanics (algorithm)
!
! Command variables - Fields preparation
!
! --------------------------------------------------------------------------------------------------
!
! In  type_comp      : type of computation
!                      '-' - Previous step
!                      '+' - Current step
! In  model          : name of model
! In  mate           : name of material characteristics (field)
! In  cara_elem      : name of elementary characteristics (field)
! In  varc_refe      : name of reference command variables vector
! In  compor         : name of comportment definition (field)
! In  exis_temp      : .true. if temperature variable command exists
! In  mxchin         : maximum number of input fields
! IO  lpain          : list of input parameters
! IO  lchin          : list of input fields
! IO  nbin           : number of input fields
! In  mxchout        : maximum number of output fields
! IO  lpaout         : list of output parameters
! IO  lchout         : list of output fields
! IO  nbout          : number of output fields
! In  sigm_prev      : stress at previous step
! In  vari_prev      : internal variables at previous step
! In  varc_prev      : command variables at previous step
! In  varc_curr      : command variables at current step
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: lxfem
    integer :: iret
    character(len=19) :: chsith
    character(len=19) :: vrcmoi, vrcplu, time_curr, time_prev
    character(len=24) :: chgeom, chcara(18), chvref, ligrmo
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    call exixfe(model, iret)
    lxfem  = iret.ne.0
    chvref = '&&NMVCPR.VREF'
    ligrmo = model(1:8)//'.MODELE'
    chsith = '&&NMVCPR.CHSITH'
!
! - Init fields
!
    call inical(mxchin, lpain, lchin, mxchout, lpaout,&
                lchout)
!
! - Get command variables
!
    call nmvcex('TOUT', varc_refe, chvref)
    call nmvcex('INST', varc_curr, time_curr)
    call nmvcex('TOUT', varc_curr, vrcplu)
    call nmvcex('TOUT', varc_prev, vrcmoi)
    call nmvcex('INST', varc_prev, time_prev)
!
! - Geometry field
!
    call megeom(model, chgeom)
!
! - Elementary characteristics
!
    call mecara(cara_elem, chcara)
!
! - Input fields
!
    lpain(1)  = 'PVARCRR'
    lchin(1)  = chvref(1:19)
    lpain(2)  = 'PGEOMER'
    lchin(2)  = chgeom(1:19)
    lpain(3)  = 'PMATERC'
    lchin(3)  = mate(1:19)
    lpain(4)  = 'PCACOQU'
    lchin(4)  = chcara(7)(1:19)
    lpain(5)  = 'PCAGNPO'
    lchin(5)  = chcara(6)(1:19)
    lpain(6)  = 'PCADISM'
    lchin(6)  = chcara(3)(1:19)
    lpain(7)  = 'PCAORIE'
    lchin(7)  = chcara(1)(1:19)
    lpain(8)  = 'PCAGNBA'
    lchin(8)  = chcara(11)(1:19)
    lpain(9)  = 'PCAARPO'
    lchin(9)  = chcara(9)(1:19)
    lpain(10) = 'PCAMASS'
    lchin(10) = chcara(12)(1:19)
    lpain(11) = 'PCAGEPO'
    lchin(11) = chcara(5)(1:19)
    lpain(12) = 'PCONTMR'
    lchin(12) = sigm_prev
    lpain(13) = 'PVARIPR'
    lchin(13) = vari_prev
    lpain(14) = 'PCOMPOR'
    lchin(14) = compor(1:19)
    lpain(15) = 'PNBSP_I'
    lchin(15) = chcara(1) (1:8)//'.CANBSP'
    lpain(16) = 'PFIBRES'
    lchin(16) = chcara(1) (1:8)//'.CAFIBR'
!
! - Computation of elementary vectors - Previous
!
    if (type_comp.eq.'-') then
        lpain(17) = 'PTEMPSR'
        lchin(17) = time_prev
        lpain(18) = 'PVARCPR'
        lchin(18) = vrcmoi
        nbin = 18
    endif
!
! - Computation of elementary vectors - Current
!
    if (type_comp.eq.'+') then
        lpain(17) = 'PTEMPSR'
        lchin(17) = time_curr
        lpain(18) = 'PVARCPR'
        lchin(18) = vrcplu
        lpain(19) = 'PVARCMR'
        lchin(19) = vrcmoi
        nbin = 19
    endif
!
! - XFEM input fields
!
    if (lxfem .and. exis_temp) then
        call xajcin(model, 'CHAR_MECA_TEMP_R', mxchin, lchin, lpain,&
                    nbin) 
    endif
!
! - Output fields
!
    lpaout(1) = 'PVECTUR'
    nbout = 1
!
! - XFEM output fields
!
    if (lxfem .and. exis_temp) then
        call detrsd('CHAM_ELEM', chsith)
        call alchml(ligrmo, 'SIEF_ELGA', 'PCONTRR', 'V', chsith,&
                    iret, ' ')
        lpaout(2) = 'PCONTRT'
        lchout(2) = chsith
        nbout = nbout+1
    endif
!
    call jedema()
end subroutine
