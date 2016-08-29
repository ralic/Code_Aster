subroutine rs_get_model(result_, nume, model, codret)
!
implicit none
!
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterfort/getvid.h"
#include "asterfort/rsadpa.h"
#include "asterfort/utmess.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: result_
    integer, intent(in) :: nume
    character(len=*), intent(out) :: model
    integer, intent(out) :: codret
!
! --------------------------------------------------------------------------------------------------
!
! Results datastructure - Utility
!
! Get model at index stored in results datastructure or from command file
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of results datastructure
! In  nume             : index to find in results datastructure
! Out model            : name of model
! Out codret           : return code
!                        -1 - No model found
!                         1 - Model from command file
!                         2 - Model from results datastructure
!                         3 - Model from results datastructure and command file (the same)
!                         4 - Model from command file is different from results datastructure
!                        
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: result, model_resu, model_comm
    integer :: nocc, jv_para
!
! --------------------------------------------------------------------------------------------------
!
    result = result_
    model  = ' '
    nocc   = 0
    codret = -1
!
! - Get from command file
!
    model_comm = ' '
    if (getexm(' ','MODELE') .eq. 1) then
        call getvid(' ', 'MODELE', scal=model_comm, nbret=nocc)
    else
        model_comm = ' '
        nocc       = 0
    endif
!
! - Get from results datastructure
!
    model_resu = ' '
    call rsadpa(result, 'L', 1, 'MODELE', nume,&
                0, sjv=jv_para)
    model_resu = zk8(jv_para)
!
! - Select model
!
    if (model_resu .eq. ' ') then
        if (nocc .eq. 0) then
            model  = ' '
            codret = -1
        else
            model  = model_comm
            codret = 1
        endif
    else
        if (nocc .eq. 0) then
            model  = model_resu
            codret = 2
        else if (model_resu .eq. model_comm) then
            model  = model_comm
            codret = 3
        else
            model  = model_comm
            codret = 4
        endif
    endif
!
end subroutine
