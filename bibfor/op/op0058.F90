subroutine op0058()
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     OPERATEUR POST_GENE_PHYS
! ----------------------------------------------------------------------
! person_in_charge: hassan.berro at edf.fr
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/pgpcal.h"
#include "asterfort/pgpcrt.h"
#include "asterfort/pgpext.h"
#include "asterfort/pgppre.h"
#include "asterfort/utimsd.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!     ------------------------------------------------------------------
    character(len=8) :: sd_pgp
!     ------------------------------------------------------------------
    call jemarq()
    call infmaj()

    sd_pgp = '&&OP0058'

!   Verifies all input data, searches for node or elements number and field components
!   Extracts a reduced modal basis corresponding to the user's request 
    call pgppre(sd_pgp)
!   call utimsd(6, 2, .false._1, .true._1, sd_pgp, 1, 'V')


!   Creates a table (observation) which is given as the ouptut of the command
    call pgpcrt(sd_pgp)


!   Calculates the physical fields of interest according to the user's input, saves
!   the results in a sd_pgp data structure
    call pgpcal(sd_pgp)


!   Adds extra contributions to the results : static correction, multiply pinned systems
!   and entraining acceleration.
    call pgpext(sd_pgp)


    call jedema()
end subroutine
