subroutine tresu_read_refe(mclfac, iocc, tbref)
    implicit none
    character(len=*), intent(in) :: mclfac
    integer, intent(in) :: iocc
    character(len=16), intent(out) :: tbref(2)
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
! utilisé par TEST_RESU, TEST_TABLE
!
! in  : mclfac : mot cle facteur
! in  : iocc   : numero d'occurrence
! out : tbref  : (1) = reference
!                (2) = legende
!
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lxnoac.h"
!
    integer :: n0, n2
    character(len=16) :: legend, refer
!
    call jemarq()
!
    call getvtx(mclfac, 'LEGENDE', iocc=iocc, nbval=0, nbret=n0)
    call getvtx(mclfac, 'REFERENCE', iocc=iocc, nbval=0, nbret=n2)
!
    legend='XXXX'
    if (n0 .lt. 0) then
        call getvtx(mclfac, 'LEGENDE', iocc=iocc, scal=legend, nbret=n0)
        call lxnoac(legend, legend)
    endif
!
    refer='NON_REGRESSION'
    if (n2 .lt. 0) then
        call getvtx(mclfac, 'REFERENCE', iocc=iocc, scal=refer, nbret=n2)
    endif
!
    tbref(1)=refer
    tbref(2)=legend
!
    call jedema()
!
end subroutine
