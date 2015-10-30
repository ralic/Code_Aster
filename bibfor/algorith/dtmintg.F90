subroutine dtmintg(sd_dtm_, sd_int_, buffdtm, buffint)
    implicit none
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
! person_in_charge: hassan.berro at edf.fr
!
! dtmintg : Integrate from t_i to t_i+1 the differential equations of motion
!           using the integration method specified in the sd_int.
! 
#include "jeveux.h"
#include "blas/dcopy.h"
#include "asterfort/assert.h"
#include "asterfort/dtmget.h"
#include "asterfort/intadapt1.h"
#include "asterfort/intadapt2.h"
#include "asterfort/intdevo.h"
#include "asterfort/inteuler.h"
#include "asterfort/inteuler2.h"
#include "asterfort/intitmi.h"
#include "asterfort/intnewm.h"
#include "asterfort/intruku32.h"
#include "asterfort/intruku54.h"
!
!   -0.1- Input/output arguments
    character(len=*) , intent(in) :: sd_dtm_
    character(len=*) , intent(in) :: sd_int_
    integer, pointer              :: buffdtm(:)
    integer, pointer              :: buffint(:)
!
!   -0.2- Local variables
    integer               :: nbnoli, method
    character(len=8)      :: sd_dtm, sd_int
    real(kind=8), pointer :: chosav1(:) => null()
    real(kind=8), pointer :: chosav2(:) => null()
!
!   0 - Initializations
    sd_dtm = sd_dtm_
    sd_int = sd_int_
!
    call dtmget(sd_dtm, _SCHEMA_I, iscal=method, buffer=buffdtm)
    call dtmget(sd_dtm, _NB_NONLI, iscal=nbnoli, buffer=buffdtm)
    if (nbnoli.gt.0) then
        call dtmget(sd_dtm, _NL_SAVES, vr=chosav2, buffer=buffdtm)
        call dtmget(sd_dtm, _NL_SAVE1, vr=chosav1, buffer=buffdtm)
        call dcopy(size(chosav2), chosav2, 1, chosav1, 1)
    end if

    select case (method)
!
        case(_SCH_EULER)
            call inteuler(sd_dtm, sd_int, buffdtm, buffint)
!
        case(_SCH_DEVOGE)
            call intdevo(sd_dtm, sd_int, buffdtm, buffint)
!
        case(_SCH_NEWMARK)
            call intnewm(sd_dtm, sd_int, buffdtm, buffint)
!
        case(_SCH_RUNGE_KUTTA_32)
            call intruku32(sd_dtm, sd_int, buffdtm, buffint)
!
        case(_SCH_RUNGE_KUTTA_54)
            call intruku54(sd_dtm, sd_int, buffdtm, buffint)
!
        case(_SCH_ADAPT_ORDRE1)
            call intadapt1(sd_dtm, sd_int, buffdtm, buffint)
!
        case(_SCH_ADAPT_ORDRE2)
            call intadapt2(sd_dtm, sd_int, buffdtm, buffint)
!
        case(_SCH_ITMI)
            call intitmi(sd_dtm, sd_int, buffdtm, buffint)
!
        case default
            ASSERT(.false.)
!
    end select

end subroutine