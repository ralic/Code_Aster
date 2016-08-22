subroutine nlbuff(sd_nl, addrs, level)
    use iso_c_binding, only: c_loc, c_ptr, c_f_pointer
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! Save in the internal buffer the addresses of param objects for quick access
!
!  sd_nl [Obl]: Name of the nl data structure requested [K24]
! ----------------------------------------------------------------------
! person_in_charge: hassan.berro at edf.fr    
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/crevec.h"
#include "asterfort/nlget.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveut.h"
#include "asterfort/jgetptc.h"
#include "asterfort/wkvect.h"

!   ====================================================================
!   = 0 =   Variable declarations and initialization
!   ====================================================================
!
!   -0.1- Input/output arguments
    character(len=*) , intent(in)  :: sd_nl
    integer, pointer , intent(out) :: addrs(:)
    integer, optional, intent(in)  :: level
!
!   -0.2- Local variables
!   --- For strings copying
    character(len=8) :: sd_nl_

!   --- For general usage
    integer           :: ip, iret, addr, long, jbuff
    integer           :: ilev, lvl, dec
    character(len=6)  :: k_iocc
    character(len=24) :: savename
    type(c_ptr) :: pc
!
#include "nlinc.h"
!
!   Copying the input strings, in order to allow in-command truncated input
    sd_nl_ = sd_nl
    nullify(addrs)
!
!   Variable lvl defines the maximum buffer level in the case of per occurence items
    lvl = 1 
    if (present(level)) lvl = level
!
!   ====================================================================
!   = 1 = Validation of the input arguments, distinguishing global vars
!   ====================================================================
    call jeexin(sd_nl_//'.BUFFER.        ',iret)
    if (iret.gt.0) then
        call jelibe(sd_nl_//'.BUFFER.        ')
        call jedetr(sd_nl_//'.BUFFER.        ')
    end if
    call crevec(sd_nl_//'.BUFFER.        ','V V I',2*lvl*_NL_NBPAR,jbuff)

    call jgetptc(jbuff, pc, vi=zi(1))
    call c_f_pointer(pc, addrs, [2*lvl*_NL_NBPAR])

    do ip = 1, 2*lvl*_NL_NBPAR
        addrs(ip) = 0
    end do
    do ilev = 1, lvl
        dec = (ilev-1)*2*_NL_NBPAR
        do ip = 1, _NL_NBPAR
            savename = '                        '
            savename(1:8) = sd_nl_
            if(parind(ip).gt.0) then
                call codent(ilev, 'G', k_iocc)
                savename(9:15) = '.'//k_iocc(1:6)
            else if (ilev.gt.1) then
                goto 10
            end if
            savename(16:24)='.'//params(ip)
            call jeexin(savename, iret)
            if (iret.gt.0) then
                call jelibe(savename)
                call jeveut(savename, 'E', addr)
                call jelira(savename, 'LONMAX', long)
                addrs(dec+ip) = addr
                addrs(dec+_NL_NBPAR+ip) = long            
            else if (abs(parind(ip)).eq.1) then
                call crevec(savename, 'V V '//partyp(ip),1, addr)
                addrs(dec+ip) = addr
                addrs(dec+_NL_NBPAR+ip) = 1            
            end if
10          continue            
        end do
    end do

end subroutine