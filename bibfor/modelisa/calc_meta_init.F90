subroutine calc_meta_init(sd_temp, temp_nume, ligrmo, compor, phasin,&
                          chmate)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calc_meta_field.h"
#include "asterfort/copisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/utmess.h"
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
    character(len=8), intent(in) :: sd_temp
    integer, intent(in) :: temp_nume
    character(len=24), intent(in) :: ligrmo
    character(len=24), intent(in) :: compor
    character(len=24), intent(in) :: phasin
    character(len=24), intent(in) :: chmate
!
! --------------------------------------------------------------------------------------------------
!
! CALC_META
!
! Compute META_INIT_ELNO and save it in datastructure
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
    character(len=19) :: meta_out
    integer :: iret, iainst
    character(len=8) :: k8_dummy
    character(len=24) :: tempe, sd_field
    real(kind=8) :: time
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Prepare metallurgical field
!
    meta_out = '&&SMEVOL.PHAS_META1'
    call copisd('CHAM_ELEM_S', 'V', compor, meta_out)
!
! - Temperature field
!
    call rsexch('F', sd_temp, 'TEMP', temp_nume, tempe,&
                iret)
!
! - Current time
!
    call rsadpa(sd_temp, 'L', 1, 'INST', temp_nume,&
                0, sjv=iainst, styp=k8_dummy)
    time = zr(iainst)
!
! - Computation of metallurgical field
!
    call calc_meta_field(ligrmo, chmate, tempe, compor, phasin,&
                         meta_out)
!
! - Save in result datastructure
!
    call rsexch(' ', sd_temp, 'META_ELNO', temp_nume, sd_field,&
                iret)
    call copisd('CHAMP_GD', 'G', meta_out, sd_field)
    call rsnoch(sd_temp, 'META_ELNO', temp_nume)
!
! - Print
!
    call utmess('I', 'ARCHIVAGE_6', sk='META_ELNO', si=temp_nume, sr=time)
!
    call jedema()
end subroutine
