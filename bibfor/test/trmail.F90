subroutine trmail(ific, nocc)
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/getvid.h"
#include "asterfort/tresu_tole.h"
#include "asterfort/tresu_read_refe.h"
#include "asterfort/tresu_mail.h"
#include "asterfort/utmess.h"
    integer, intent(in) :: ific
    integer, intent(in) :: nocc
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     COMMANDE:  TEST_RESU
!                MOT CLE FACTEUR "MAILLAGE"
! ----------------------------------------------------------------------
    integer :: iocc, refi, refir, n1, n2, n2r, iret
    real(kind=8) :: epsi, epsir
    character(len=3) :: ssigne
    character(len=8) :: crit
    character(len=16) :: tbtxt(2), tbref(2)
    character(len=8) :: nommai
    aster_logical :: lref
!     ------------------------------------------------------------------
!
    do 100 iocc = 1, nocc
        call getvid('MAILLAGE', 'MAILLAGE', iocc=iocc, scal=nommai, nbret=n1)
        call getvtx('MAILLAGE', 'VALE_ABS', iocc=iocc, scal=ssigne, nbret=n1)
        call tresu_tole(epsi, mcf='MAILLAGE', iocc=iocc)
        call getvtx('MAILLAGE', 'CRITERE', iocc=iocc, scal=crit, nbret=n1)
!
        call tresu_read_refe('MAILLAGE', iocc, tbtxt)
        lref=.false.
        call getvr8('MAILLAGE', 'PRECISION', iocc=iocc, scal=epsir, nbret=iret)
        if (iret .ne. 0) then
            lref=.true.
            tbref(1)=tbtxt(1)
            tbref(2)=tbtxt(2)
            tbtxt(1)='NON_REGRESSION'
        endif
!
        write (ific,*) '---- MAILLAGE '
        write (ific,*) '     ',nommai
!
        call getvis('MAILLAGE', 'VALE_CALC_I', iocc=iocc, scal=refi, nbret=n2)
        if (n2 .eq. 1) then
            call tresu_mail(nommai, tbtxt, refi, iocc,&
                           epsi, crit, .true._1, ssigne)
            if (lref) then
                call getvis('MAILLAGE', 'VALE_REFE_I', iocc=iocc, scal=refir, nbret=n2r)
                ASSERT(n2.eq.n2r)
                call tresu_mail(nommai, tbref, refir, iocc,&
                               epsir, crit, .false._1, ssigne)
            endif
        endif
        write (ific,*)' '
100 end do
!
!
end subroutine
