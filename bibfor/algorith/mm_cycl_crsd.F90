subroutine mm_cycl_crsd(sd_cont_defi,sd_cont_solv)
!
    implicit     none
!
#include "asterfort/cfdisi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: sd_cont_defi
    character(len=24), intent(in) :: sd_cont_solv
!
! --------------------------------------------------------------------------------------------------
!
! Contact (continue method)
!
! Creating data structures for cycling detection and treatment
!
! --------------------------------------------------------------------------------------------------
!
!
! In  sd_cont_solv : data structure for contact solving
! In  sd_cont_defi : data structure from contact definition 
!
! --------------------------------------------------------------------------------------------------
!
    integer :: point_number
    character(len=24) :: sd_cycl_lis, sd_cycl_nbr, sd_cycl_typ, sd_cycl_gli
    integer :: jcylis, jcynbr, jcytyp, jcygli
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    point_number = cfdisi(sd_cont_defi,'NTPC' )
!
! - Name of cycling objects
!
    sd_cycl_lis = sd_cont_solv(1:14)//'.CYCLIS'
    sd_cycl_nbr = sd_cont_solv(1:14)//'.CYCNBR'
    sd_cycl_typ = sd_cont_solv(1:14)//'.CYCTYP'
    sd_cycl_gli = sd_cont_solv(1:14)//'.CYCGLI'
!
! - Creating cycling objects
! 
    call wkvect(sd_cycl_lis, 'V V I'  , 4*point_number, jcylis)
    call wkvect(sd_cycl_nbr, 'V V I'  , 4*point_number, jcynbr)
    call wkvect(sd_cycl_typ, 'V V I'  , 4*point_number, jcytyp)
    call wkvect(sd_cycl_gli, 'V V R'  , 12*point_number, jcygli)
!
    call jedema()
end subroutine
