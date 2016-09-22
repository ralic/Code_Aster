subroutine nmrest_ecro(model_, mate_, ds_constitutive, hval_incr)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/calcul.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmvcex.h"
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
! person_in_charge: sofiane.hendili at edf.fr
!
    character(len=*), intent(in) :: model_
    character(len=*), intent(in) :: mate_
    type(NL_DS_Constitutive), intent(in) :: ds_constitutive
    character(len=19), intent(in) :: hval_incr(*)
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Post-treatment 
!
! Annealing
!
! --------------------------------------------------------------------------------------------------
!
! In  model          : name of model
! In  mate           : name of material characteristics (field)
! In  ds_constitutive  : datastructure for constitutive laws management
! In  hval_incr      : hat-variable for incremental values
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nbin = 7
    integer, parameter :: nbout = 1
    character(len=8)   :: lpaout(nbout), lpain(nbin)
    character(len=19)  :: lchout(nbout), lchin(nbin)
!
    character(len=8)  :: model
    character(len=19) :: mate
    character(len=16) :: option
    character(len=19) :: ligrmo
    character(len=1)  :: base
    character(len=19) :: vari_curr, varc_prev, varc_curr, vari_curr_modi
    character(len=19) :: vrcplu, vrcmoi, time_curr
!
! --------------------------------------------------------------------------------------------------
!
    option = 'REST_ECRO'
    base   = 'V'
    model  = model_
    mate   = mate_
    ligrmo = model(1:8)//'.MODELE'
!
! - Get fields from hat-variables - Begin of time step
!
    call nmchex(hval_incr, 'VALINC', 'VARPLU', vari_curr)
    call nmchex(hval_incr, 'VALINC', 'COMMOI', varc_prev)
    call nmchex(hval_incr, 'VALINC', 'COMPLU', varc_curr)
    call nmvcex('TOUT', varc_prev, vrcmoi)
    call nmvcex('TOUT', varc_curr, vrcplu)
    call nmvcex('INST', varc_curr, time_curr)
!
    vari_curr_modi = '&&VARI_TMP'
    call copisd('CHAM_ELEM_S', 'V', ds_constitutive%compor, vari_curr_modi)
!
! - Input fields
!
    lpain(1) = 'PMATERC'
    lchin(1) = mate
    lpain(2) = 'PCOMPOR'
    lchin(2) = ds_constitutive%compor(1:19)
    lpain(3) = 'PVARIMR'
    lchin(3) = vari_curr
    lpain(4) = 'PVARCMR'
    lchin(4) = vrcmoi
    lpain(5) = 'PVARCPR'
    lchin(5) = vrcplu
    lpain(6) = 'PTEMPSR'
    lchin(6) = time_curr
    lpain(7) = 'PCARCRI'
    lchin(7) = ds_constitutive%carcri(1:19)
!
! - Output field
!
    lpaout(1) = 'PVARIPR'
    lchout(1) = vari_curr_modi
!
! - Computation
!
    call calcul('S'  , option, ligrmo, nbin  , lchin,&
                lpain, nbout , lchout, lpaout, base ,&
                'OUI')
!
    call copisd('CHAMP_GD', 'V', vari_curr_modi, vari_curr)
    call detrsd('CHAMP', vari_curr_modi)
!
end subroutine
