subroutine mm_cycl_init(sd_cont_defi, sd_cont_solv, cycl_type)
!
    implicit     none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
    integer, intent(in) :: cycl_type
!
! --------------------------------------------------------------------------------------------------
!
! Contact (continue method)
!
! Initialization of data structures for cycling detection and treatment
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_cont_solv : data structure for contact solving
! In  sd_cont_defi : data structure from contact definition 
! In  cycl_type    : type of cycling to erase 
!                     0 - for erasing for all cycles
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sd_cycl_lis, sd_cycl_nbr, sd_cycl_typ, sd_cycl_gli
    integer :: jcylis, jcynbr, jcytyp, jcygli
    integer :: point_number, point_index
    integer :: cycl_index, i
    logical :: cont_disc, cont_xfem
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Formulation of contact
!
    cont_disc = cfdisl(sd_cont_defi,'FORMUL_DISCRETE')
    cont_xfem = cfdisl(sd_cont_defi,'FORMUL_XFEM')
    if (cont_disc .or. cont_xfem) goto 99
!
! - Access to cycling objects
!
    sd_cycl_lis = sd_cont_solv(1:14)//'.CYCLIS'
    sd_cycl_nbr = sd_cont_solv(1:14)//'.CYCNBR'
    sd_cycl_typ = sd_cont_solv(1:14)//'.CYCTYP'
    sd_cycl_gli = sd_cont_solv(1:14)//'.CYCGLI'
    call jeveuo(sd_cycl_lis, 'E', jcylis)
    call jeveuo(sd_cycl_nbr, 'E', jcynbr)
    call jeveuo(sd_cycl_typ, 'E', jcytyp)
    call jeveuo(sd_cycl_gli, 'E', jcygli)
!
! - Initializations
!
    point_number = cfdisi(sd_cont_defi,'NTPC' )
    do point_index = 1, point_number
        if (cycl_type.le.0) then
            do cycl_index = 1, 4
                zi(jcylis-1+4*(point_index-1)+cycl_index) = 0
                zi(jcynbr-1+4*(point_index-1)+cycl_index) = 0
                zi(jcytyp-1+4*(point_index-1)+cycl_index) = 0
            end do
            do i = 1, 12
                zr(jcygli-1+12*(point_index-1)+i) = 0.d0
            end do
        endif
        if (cycl_type.eq.3) then
            zi(jcylis-1+4*(point_index-1)+3) = 1
        endif
    end do
!
99  continue
!
    call jedema()
end subroutine
