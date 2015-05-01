subroutine refdcp(resin, resout)
!
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedup1.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
!
    character(len=8) :: resin, resout
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
! person_in_charge: hassan.berro at edf.fr
! ----------------------------------------------------------------------
!
!   COPIER LE CONTENU DE REFERENCES DYNAMIQUE DE resin DANS resout
!
    integer :: ir1, ir2
    character(len=16) :: refd, indi
    character(len=1) :: jvb
!
    call jemarq()
!
    refd = '           .REFD'
    indi = '           .INDI'
    jvb = 'G'
!
    if (resin .ne. resout) then
        call jeexin(resin //refd, ir1)
        call jeexin(resout//refd, ir2)
!
        if (ir1 .gt. 0 .and. ir2 .gt. 0) then
            call jedetr(resout//refd)
            call jedetr(resout//indi)
        endif
!
        if (ir1 .gt. 0) then
            if (resout(1:2) .eq. '&&') jvb = 'V'
            call jedup1(resin//refd, jvb, resout//refd)
            call jedup1(resin//indi, jvb, resout//indi)
        endif
!
!       For debugging purposes only...
!       call utimsd(8, 1, .false._1, .true._1, resout//refd,1, 'G')
!       call utimsd(8, 1, .false._1, .true._1, resout//indi,1, 'G')
    endif
!
    call jedema()
end subroutine
