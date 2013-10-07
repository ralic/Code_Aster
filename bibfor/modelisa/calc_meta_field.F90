subroutine calc_meta_field(ligrmo, chmate, tempe, compor, phasin, &
                           meta_out)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
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
    character(len=19), intent(in) :: meta_out
    character(len=24), intent(in) :: ligrmo
    character(len=19), intent(in) :: compor
    character(len=24), intent(in) :: phasin
    character(len=24), intent(in) :: chmate
    character(len=24), intent(in) :: tempe
!
! --------------------------------------------------------------------------------------------------
!
! CALC_META
!
! Compute initial metallurgical field in meta_out
!
! --------------------------------------------------------------------------------------------------
!
! In  ligrmo   : LIGREL for model
! In  chmate   : field of coded material
! In  tempe    : field of current temperature
! In  compor   : field of behavior
! In  phasin   : field of phase
! In  meta_out : metallurgical field
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=4)
    character(len=8)  :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchin(nbin)
!
    character(len=1) :: base
    character(len=16) :: option
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initialization
!
    option = 'META_INIT_ELNO'
    base = 'V'
!
! - Fields in
!
    lpain(1) = 'PMATERC'
    lchin(1) = chmate(1:19)
    lpain(2) = 'PCOMPOR'
    lchin(2) = compor(1:19)
    lpain(3) = 'PTEMPER'
    lchin(3) = tempe(1:19)
    lpain(4) = 'PPHASIN'
    lchin(4) = phasin(1:19)
!
! - Fields out
!
    lpaout(1) = 'PPHASNOU'
!
! - Computation
!
    call calcul('S', option, ligrmo, nbin, lchin,&
                lpain, nbout, meta_out, lpaout, base,&
                'OUI')
!
    call jedema()
end subroutine
