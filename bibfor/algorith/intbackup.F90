subroutine intbackup(sd_int_, sd_int_backup_)
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
! Extract the value of a parameter in the temporary data structure for an
! integration scheme in linear dynamics (DYNA_VIBRA)
!
!  sd_int_        [Obl]: Name of the integration data structure to be backed up [K8]
!  sd_int_backup_ [Obl]: Backup name of the integration data structure [K8]
!
! Examples : call intbackup('&&INTEGR','&&INTBAK')
!            call intbackup('&&INTEGR','&&INTBAK')
!
! ----------------------------------------------------------------------
! person_in_charge: hassan.berro at edf.fr    
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/intget.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeexin.h"

!
!   ====================================================================
!   = 0 =   Variable declarations and initialization
!   ====================================================================
!
!   -0.1- Input/output arguments
    character(len=*)          , intent(in) :: sd_int_
    character(len=*)          , intent(in) :: sd_int_backup_
!
!   -0.2- Local variables
!   --- For strings copying
    character(len=8) :: sd_int
    character(len=8) :: sd_int_bu

!   --- For general usage
    integer           :: ip, iocc, level, iret1, iret2
    character(len=6)  :: k_iocc
    character(len=24) :: savejv1, savejv2
!
#include "intinc.h"
!
    savejv1 = '                        '
    savejv2 = '                        '

!   Copying the input strings, in order to allow in-command truncated input
    sd_int = sd_int_
    sd_int_bu = sd_int_backup_

!   ====================================================================
!   = 1 = Loop over the integration sd parameters
!   ====================================================================

    call intget(sd_int, IND_ARCH, iscal=level)
    savejv1(1:8) = sd_int
    savejv2(1:8) = sd_int_bu
    do ip = 1, _INT_NBPAR
        savejv1(16:24)='.'//params(ip)
        savejv2(16:24)='.'//params(ip)
        if (parind(ip).gt.0) then
            do iocc = 1, level
                call codent(iocc, 'G', k_iocc)
                savejv1(9:15) = '.'//k_iocc(1:6)
                savejv2(9:15) = '.'//k_iocc(1:6)
                call jeexin(savejv1, iret1)
                if (iret1.gt.0) then
                    call jeexin(savejv2, iret2)
                    if (iret2.gt.0) call jedetr(savejv2)
                    call jedupo(savejv1, 'V', savejv2, .false._1)
                end if        
            end do
        else
            savejv1(9:15) = '       '
            savejv2(9:15) = '       '
            call jeexin(savejv1, iret1)
            if (iret1.gt.0) then
                call jeexin(savejv2, iret2)
                if (iret2.gt.0) call jedetr(savejv2)
                call jedupo(savejv1, 'V', savejv2, .false._1)
            end if
        end if
    end do


end subroutine