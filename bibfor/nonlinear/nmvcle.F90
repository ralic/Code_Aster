subroutine nmvcle(modelz, matez, cara_elemz, time, varcz)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/detrsd.h"
#include "asterfort/mecact.h"
#include "asterfort/vrcins.h"
#include "asterfort/wkvect.h"
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
    character(len=*), intent(in) :: modelz
    character(len=*), intent(in) :: matez
    character(len=*), intent(in) :: cara_elemz
    real(kind=8), intent(in) :: time
    character(len=*), intent(in) :: varcz
!
! --------------------------------------------------------------------------------------------------
!
! Nonlinear mechanics (algorithm)
!
! Command variables - Read
!
! --------------------------------------------------------------------------------------------------
!
! In  model          : name of model
! In  mate           : name of material characteristics (field)
! In  cara_elem      : name of elementary characteristics (field)
! In  time           : time
! In  varc           : command variable field
!
! --------------------------------------------------------------------------------------------------
!
    character(len=2) :: codret
    character(len=8) :: model, mate, cara_elem
    character(len=14) :: varc
    character(len=24) :: varc_list
    character(len=24) :: varc_time
!
! --------------------------------------------------------------------------------------------------
!
    varc          = varcz
    model         = modelz
    cara_elem     = cara_elemz
    mate          = matez
!
! - Old object deleted
!
    call detrsd('VARI_COM', varc)
!
! - List of command variables
!
    varc_list = varc//'.TOUT'
!
! - Construct command variables fields
!
    call vrcins(model, mate, cara_elem, time, varc_list,&
                codret)
!
! - Construct current time <CARTE>
!
    varc_time = varc//'.INST'
    call mecact('V', varc_time, 'MODELE', model(1:8)//'.MODELE', 'INST_R',&
                ncmp=1, nomcmp='INST', sr=time)
!
end subroutine
