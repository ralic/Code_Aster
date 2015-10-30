subroutine comdtm()
    implicit none
! ----------------------------------------------------------------------
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
!  Transient calculations on a reduced basis : DYNA_VIBRA (//TRAN//GENE)
! ----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/dtmcalc.h"
#include "asterfort/dtminfo.h"
#include "asterfort/dtminit.h"
#include "asterfort/dtmprep.h"
#include "asterfort/dtmget.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jemarq.h"
#include "asterfort/utimsd.h"
!
    character(len=8) :: sd_dtm, sd_int
!
    call jemarq()
!
    sd_dtm = '&&DTM&&&'

!   Verifies and reads all input data about the transient calculation in generalized
!   coordinates, with possible punctual nonlinearities. All information is saved in 
!   the temoporary work data structure : sd_dtm
    call dtmprep(sd_dtm)

!   Writes down some user information about the time integration parameters before
!   actually proceding with the calculations
    call dtminfo(sd_dtm)
    ! call utimsd(6, 2, .false._1, .true._1, sd_dtm, 1, 'V')

!   Initializes the calculation, by reading the initial state and calculating 
!   an initial acceleration if needed
    sd_int = '&&INTEGR'
    call dtminit(sd_dtm, sd_int)

!   Allocates memory, and loops over the time steps and integrates the dynamic
!   equations of motions as required. Archiving is accomplished within this routine
    call dtmcalc(sd_dtm, sd_int)
!   call dtmget(sd_dtm,'CALC_SD',kscal=nomres)
!   call utimsd(6, 2, .false._1, .true._1, nomres, 1, 'G')
!
    call jedema()
end subroutine
