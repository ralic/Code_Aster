subroutine vrcinp(ind, instam, instap)
!
use calcul_module, only : ca_iactif_, ca_jvcnom_, ca_nbcvrc_ , ca_jvcfon_, ca_jvcval_
!
implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveut.h"
#include "asterfort/wkvect.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    integer, intent(in) :: ind
    real(kind=8), intent(in) :: instam
    real(kind=8), intent(in) :: instap
!
! --------------------------------------------------------------------------------------------------
!
! Material - External state variables (VARC)
!
! Preparation (for calcul_module) - SIMU_POINT_MAT
!
! --------------------------------------------------------------------------------------------------
!
! In  ind              : what to do
!                         0 => ca_iactif_=0 (end)
!                         1 => ca_iactif_=2 initializations (begin)
!                         2 => interpolation
! In  instam           : time at beginning of step time
! In  instap           : time at end of step time
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbocc, ier, n1, iocc
    character(len=16), parameter :: keywf = 'AFFE_VARC'
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    if (ind .eq. 1) then
!
        ca_iactif_ = 2
        call getfac(keywf, nbocc)
        ca_nbcvrc_ = nbocc
        ASSERT(ca_nbcvrc_.le.100)

        if (ca_nbcvrc_ .ne. 0) then
! --------- Create objects
            call wkvect('&&OP0033.TVCNOM', 'V V K8', ca_nbcvrc_, ca_jvcnom_)
            call wkvect('&&OP0033.TVCFON', 'V V K8', ca_nbcvrc_, ca_jvcfon_)
            call wkvect('&&OP0033.TVCVAL', 'V V R', 3*ca_nbcvrc_, ca_jvcval_)

! --------- Get address of objects
            call jeveut('&&OP0033.TVCNOM', 'E', ca_jvcnom_)
            call jeveut('&&OP0033.TVCFON', 'E', ca_jvcfon_)
            call jeveut('&&OP0033.TVCVAL', 'E', ca_jvcval_)

! --------- Fill objects
            do iocc = 1, nbocc
                call getvtx(keywf, 'NOM_VARC' , iocc=iocc, scal=zk8(ca_jvcnom_-1+iocc),&
                            nbret=n1)
                call getvid(keywf, 'VALE_FONC', iocc=iocc, scal=zk8(ca_jvcfon_-1+iocc),&
                            nbret=n1)
                call getvr8(keywf, 'VALE_REF' , iocc=iocc, scal=zr(ca_jvcval_-1+3*(iocc-1)+3),&
                            nbret=n1)
            end do
        endif

    else if (ind.eq.0) then
        ca_iactif_=0

    else if (ind.eq.2) then
        ca_iactif_=2
        do iocc = 1, ca_nbcvrc_
            call fointe('F', zk8(ca_jvcfon_-1+iocc), 1, ['INST'], [instam],&
                        zr( ca_jvcval_-1+3*(iocc-1)+1), ier)
            call fointe('F', zk8(ca_jvcfon_-1+iocc), 1, ['INST'], [instap],&
                        zr( ca_jvcval_-1+3*(iocc-1)+2), ier)
        end do
    endif
!
    call jedema()
end subroutine
