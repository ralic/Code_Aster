subroutine merimp(model, cara_elem, mate, varc_refe, compor,&
                  carcri, acti_func, iterat, sddyna, hval_incr,&
                  hval_algo, caco3d, mxchin, nbin, lpain,&
                  lchin)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cesvar.h"
#include "asterfort/copisd.h"
#include "asterfort/exisd.h"
#include "asterfort/exixfe.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecact.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/ndynkk.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmvcex.h"
#include "asterfort/xajcin.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer, intent(in) :: iterat
    character(len=*), intent(in) :: mate
    character(len=19), intent(in) :: sddyna
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: cara_elem
    character(len=24), intent(in) :: compor
    character(len=24), intent(in) :: varc_refe
    integer, intent(in) :: acti_func(*)
    character(len=24), intent(in) :: carcri
    character(len=24), intent(in) :: caco3d
    character(len=19), intent(in) :: hval_incr(*)
    character(len=19), intent(in) :: hval_algo(*)
    integer, intent(in) :: mxchin
    character(len=8), intent(inout) :: lpain(mxchin)
    character(len=19), intent(inout) :: lchin(mxchin)
    integer, intent(out) :: nbin
!
! --------------------------------------------------------------------------------------------------
!
! Nonlinear mechanics (algorithm)
!
! Computation of rigidity matrix and internal forces - Input fields
!
! --------------------------------------------------------------------------------------------------
!
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret
    aster_logical :: lxfem
    character(len=24) :: chgeom, chcara(18), chiter
    character(len=19) :: stadyn, depent, vitent
    character(len=16) :: option
    character(len=19) :: disp_prev, sigm_prev, vari_prev, varc_prev, stru_prev
    character(len=19) :: disp_curr, sigm_curr, vari_curr, varc_curr, stru_curr
    character(len=19) :: time_prev, vrcmoi
    character(len=19) :: time_curr, vrcplu
    character(len=24) :: vrcref
    character(len=24) :: vari_iter, stru_iter
    character(len=19) :: depkm1, vitkm1, acckm1
    character(len=19) :: vite_curr, acce_curr, vite_prev, acce_prev
    character(len=19) :: romkm1, romk
    character(len=24) :: ligrmo
    character(len=19) :: disp_iter, disp_cumu_inst
    aster_logical :: ldyna
    integer :: ifm, niv
    real(kind=8) :: iter
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! - Initializations
!
    ligrmo = model(1:8)//'.MODELE'
    option = 'FULL_MECA'
    chiter = '&&MERIMO.CH_ITERAT'
    vari_iter = '&&MERIMO.VARMOJ'
    stru_iter = '&&MERIMO.STRMOJ'
    nbin = 0
    ASSERT(mate(9:18).eq.'.MATE_CODE')
!
! - Active functionnalities
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lxfem = isfonc(acti_func, 'XFEM')
!
! - Get fields from hat-variables - Begin of time step
!
    call nmchex(hval_incr, 'VALINC', 'DEPMOI', disp_prev)
    call nmchex(hval_incr, 'VALINC', 'VITMOI', vite_prev)
    call nmchex(hval_incr, 'VALINC', 'ACCMOI', acce_prev)
    call nmchex(hval_incr, 'VALINC', 'SIGMOI', sigm_prev)
    call nmchex(hval_incr, 'VALINC', 'VARMOI', vari_prev)
    call nmchex(hval_incr, 'VALINC', 'COMMOI', varc_prev)
    call nmchex(hval_incr, 'VALINC', 'STRMOI', stru_prev)
!
! - Get fields from hat-variables - End of time step
!
    call nmchex(hval_incr, 'VALINC', 'DEPPLU', disp_curr)
    call nmchex(hval_incr, 'VALINC', 'VITPLU', vite_curr)
    call nmchex(hval_incr, 'VALINC', 'ACCPLU', acce_curr)
    call nmchex(hval_incr, 'VALINC', 'SIGPLU', sigm_curr)
    call nmchex(hval_incr, 'VALINC', 'VARPLU', vari_curr)
    call nmchex(hval_incr, 'VALINC', 'COMPLU', varc_curr)
    call nmchex(hval_incr, 'VALINC', 'STRPLU', stru_curr)
!
    call nmchex(hval_incr, 'VALINC', 'DEPKM1', depkm1)
    call nmchex(hval_incr, 'VALINC', 'VITKM1', vitkm1)
    call nmchex(hval_incr, 'VALINC', 'ACCKM1', acckm1)
    call nmchex(hval_incr, 'VALINC', 'ROMKM1', romkm1)
    call nmchex(hval_incr, 'VALINC', 'ROMK  ', romk)
!
    call nmchex(hval_algo, 'SOLALG', 'DEPDEL', disp_cumu_inst)
    call nmchex(hval_algo, 'SOLALG', 'DDEPLA', disp_iter)
!
! - Dynamic fields
!
    if (ldyna) then
        call ndynkk(sddyna, 'DEPENT', depent)
        call ndynkk(sddyna, 'VITENT', vitent)
        call ndynkk(sddyna, 'STADYN', stadyn)
    endif
!
! - Get command variables
!
    call nmvcex('TOUT', varc_prev, vrcmoi)
    call nmvcex('INST', varc_prev, time_prev)
    call nmvcex('TOUT', varc_curr, vrcplu)
    call nmvcex('INST', varc_curr, time_curr)
    call nmvcex('TOUT', varc_refe, vrcref)
!
! - Get internal variables from previous iteration
!
    call exisd('CHAMP_GD', vari_curr(1:19), iret)
    if (iret .ne. 0) then
        call copisd('CHAMP_GD', 'V', vari_curr(1:19), vari_iter(1:19))
    else
        call copisd('CHAMP_GD', 'V', vari_prev(1:19), vari_iter(1:19))
    endif
!
! - Get structural variables from previous iteration
!
    call exisd('CHAMP_GD', stru_curr(1:19), iret)
    if (iret .ne. 0) then
        call copisd('CHAMP_GD', 'V', stru_curr(1:19), stru_iter(1:19))
    else
        call copisd('CHAMP_GD', 'V', stru_prev(1:19), stru_iter(1:19))
    endif
!
! - Extend elementary field for internal variables
!
    call exisd('CHAM_ELEM_S', compor, iret)
    if (iret .eq. 0) call cesvar(cara_elem, compor, ligrmo, compor)
    call copisd('CHAM_ELEM_S', 'V', compor, vari_curr)
    call copisd('CHAM_ELEM_S', 'V', compor, sigm_curr)
!
! - Geometry field
!
    call megeom(model, chgeom)
!
! - Elementary characteristics
!
    call mecara(cara_elem, chcara)
!
! - Field for iteration number
!
    iter = iterat
    call mecact('V', chiter, 'MODELE', ligrmo, 'NEUT_R',&
                ncmp=1, nomcmp='X1', sr=iter)
!
! - Input fields
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom(1:19)
    lpain(2) = 'PMATERC'
    lchin(2) = mate
    lpain(3) = 'PCONTMR'
    lchin(3) = sigm_prev(1:19)
    lpain(4) = 'PVARIMR'
    lchin(4) = vari_prev(1:19)
    lpain(5) = 'PCOMPOR'
    lchin(5) = compor(1:19)
    lpain(6) = 'PDEPLMR'
    lchin(6) = disp_prev(1:19)
    lpain(7) = 'PDEPLPR'
    lchin(7) = disp_cumu_inst(1:19)
    lpain(8) = 'PCACABL'
    lchin(8) = chcara(10)(1:19)
    lpain(9) = 'PINSTMR'
    lchin(9) = time_prev(1:19)
    lpain(10) = 'PINSTPR'
    lchin(10) = time_curr(1:19)
    lpain(11) = 'PCARCRI'
    lchin(11) = carcri(1:19)
    lpain(12) = 'PCAGNPO'
    lchin(12) = chcara(6)(1:19)
    lpain(13) = 'PCAORIE'
    lchin(13) = chcara(1)(1:19)
    lpain(14) = 'PCADISK'
    lchin(14) = chcara(2)(1:19)
    lpain(15) = 'PCACOQU'
    lchin(15) = chcara(7)(1:19)
    lpain(16) = 'PITERAT'
    lchin(16) = chiter(1:19)
    lpain(17) = 'PDDEPLA'
    lchin(17) = disp_iter(1:19)
    lpain(18) = 'PDEPKM1'
    lchin(18) = depkm1(1:19)
    lpain(19) = 'PVITKM1'
    lchin(19) = vitkm1(1:19)
    lpain(20) = 'PACCKM1'
    lchin(20) = acckm1(1:19)
    lpain(21) = 'PROMKM1'
    lchin(21) = romkm1(1:19)
    lpain(22) = 'PROMK'
    lchin(22) = romk(1:19)
    lpain(23) = 'PVARIMP'
    lchin(23) = vari_iter(1:19)
    lpain(24) = 'PCAGNBA'
    lchin(24) = chcara(11)(1:19)
    lpain(25) = 'PCAMASS'
    lchin(25) = chcara(12)(1:19)
    lpain(26) = 'PCAGEPO'
    lchin(26) = chcara(5)(1:19)
    lpain(27) = 'PVARCMR'
    lchin(27) = vrcmoi(1:19)
    lpain(28) = 'PVARCPR'
    lchin(28) = vrcplu(1:19)
    lpain(29) = 'PNBSP_I'
    lchin(29) = chcara(16)(1:19)
    lpain(30) = 'PFIBRES'
    lchin(30) = chcara(17)(1:19)
    lpain(31) = 'PCINFDI'
    lchin(31) = chcara(15)(1:19)
    lpain(32) = 'PVARCRR'
    lchin(32) = vrcref(1:19)
    lpain(33) = 'PCACO3D'
    lchin(33) = caco3d(1:19)
    lpain(34) = 'PCAARPO'
    lchin(34) = chcara(9)(1:19)
    lpain(35) = 'PSTRXMR'
    lchin(35) = stru_prev(1:19)
    lpain(36) = 'PSTRXMP'
    lchin(36) = stru_iter(1:19)
    nbin = 36
!
! - XFEM fields
!
    if (lxfem) then
        call xajcin(model, option, mxchin, lchin, lpain,&
                    nbin)
    endif
!
! - Dynamic
!
    if (ldyna) then
        nbin = nbin + 1
        lpain(nbin) = 'PDEPENT'
        lchin(nbin) = depent(1:19)
        nbin = nbin + 1
        lpain(nbin) = 'PVITENT'
        lchin(nbin) = vitent(1:19)
        nbin = nbin + 1
        lpain(nbin) = 'PSTADYN'
        lchin(nbin) = stadyn(1:19)
        nbin = nbin + 1
        lpain(nbin) = 'PVITPLU'
        lchin(nbin) = vite_curr(1:19)
        nbin = nbin + 1
        lpain(nbin) = 'PACCPLU'
        lchin(nbin) = acce_curr(1:19)
        nbin = nbin + 1
        lpain(nbin) = 'PVITMOI'
        lchin(nbin) = vite_prev(1:19)
        nbin = nbin + 1
        lpain(nbin) = 'PACCMOI'
        lchin(nbin) = acce_prev(1:19)
    endif
!
    call jedema()
end subroutine
