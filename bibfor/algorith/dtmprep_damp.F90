subroutine dtmprep_damp(sd_dtm_)
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
! dtmprep_damp : Creation of a pseudo damping matrix from a list of damping coefficients
!                given by the user under the keywords LIST_AMOR and AMOR_MODAL in the 
!                command DYNA_TRAN_MODAL
!
!                The only parameter added/modified in the sd_dtm is : AMOR_DIA
!
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/dtmget.h"
#include "asterfort/dtminivec.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/r8inir.h"
#include "asterfort/rsadpa.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
!   -0.1- Input/output arguments
    character(len=*)          , intent(in) :: sd_dtm_
!
!   -0.2- Local variables
    integer           :: n1, n2, n, nbamor, nbmode
    integer           :: vali(3), iamog, iam, idiff
    integer           :: im, lamre
    real(kind=8)      :: rundef
    character(len=8)  :: sd_dtm, basemo, listam
    character(len=24) :: typeba, valk(1)
    real(kind=8), pointer :: puls(:) => null()
    real(kind=8), pointer :: amogen(:) => null()
!
!   0 - Initializations
    sd_dtm = sd_dtm_
    rundef = r8vide()
!
!   1 - Retrieval of some necessary information
    call dtmget(sd_dtm, _NB_MODES, iscal=nbmode)
    call dtmget(sd_dtm, _BASE_MOD, kscal=basemo)
    call dtmget(sd_dtm, _TYP_BASE, kscal=typeba)
!
    call dtmget(sd_dtm, _OMEGA, vr=puls)
!
    call dtminivec(sd_dtm, _AMOR_DIA, nbmode, vr=amogen)
!
!   2 - Command keywords
    call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=0, nbret=n1)
    call getvid('AMOR_MODAL', 'LIST_AMOR', iocc=1, nbval=0, nbret=n2)
!
!   3 - Filling up the diagonal damping matrix given the user entries
    if ((n1.ne.0) .or. (n2.ne.0)) then
        if (n1 .ne. 0) then
            nbamor = -n1
        else
            call getvid('AMOR_MODAL', 'LIST_AMOR', iocc=1, scal=listam)
            call jelira(listam//'           .VALE', 'LONMAX', nbamor)
        endif
        if (nbamor .gt. nbmode) then
!
            vali (1) = nbmode
            vali (2) = nbamor
            vali (3) = nbmode
            valk (1) = 'PREMIERS COEFFICIENTS'
            call utmess('A', 'ALGORITH16_18', sk=valk(1), ni=3, vali=vali)
            if (n1 .ne. 0) then
                call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=nbmode,&
                            vect=amogen)
            else
                call jeveuo(listam//'           .VALE', 'L', iamog)
                do iam = 1, nbmode
                    amogen(iam) = zr(iamog+iam-1)
                enddo
            endif
        else if (nbamor.lt.nbmode) then
!
            if (n1 .ne. 0) then
                call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=nbamor,&
                            vect=amogen, nbret=n)
            else
                call jeveuo(listam//'           .VALE', 'L', iamog)
                do iam = 1, nbamor
                    amogen(iam) = zr(iamog+iam-1)
                enddo
            endif
            idiff = nbmode - nbamor
            vali (1) = idiff
            vali (2) = nbmode
            vali (3) = idiff
            call utmess('I', 'ALGORITH16_19', ni=3, vali=vali)
            do iam = nbamor + 1, nbmode
                amogen(iam) = amogen(nbamor)
            enddo
        else if (nbamor.eq.nbmode) then
!
            if (n1 .ne. 0) then
                call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=nbamor,&
                            vect=amogen, nbret=n)
            else
                call jeveuo(listam//'           .VALE', 'L', iamog)
                do iam = 1, nbamor
                    amogen(iam) = zr(iamog+iam-1)
                enddo
            endif
        endif
    else
        if (typeba(1:9) .eq. 'MODE_MECA') then
            call rsadpa(basemo, 'L', 1, 'AMOR_REDUIT', 1,&
                        0, sjv=lamre, istop=0)
            if (zr(lamre).ne.rundef) then
                do im = 1, nbmode
                    call rsadpa(basemo, 'L', 1, 'AMOR_REDUIT', im,&
                                0, sjv=lamre)
                    amogen(im) = zr(lamre)
                end do
            else 
                call r8inir(nbmode, 0.d0, amogen, 1)
            end if
        else 
            call r8inir(nbmode, 0.d0, amogen, 1)
        end if
    endif
!
    do im = 1, nbmode
        amogen(im) = 2.0d0*puls(im)*amogen(im)
    enddo
!
end subroutine