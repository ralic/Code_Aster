subroutine intsav(sd_int_, ip, lonvec, iocc, kscal,&
                  iscal, rscal, cscal, kvect, ivect,&
                  rvect, cvect, buffer)
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
! Save a parameter in the temporary data structure for integration schemes
! used in linear dynamics (DYNA_VIBRA)
!
!  sd_int_ [Obl]: Name of the integration data structure to be saved into [K24]
!  ip      [Obl]: Index of the parameter to be saved [I]
!  lonvec  [Obl]: Length of the Vector to be saved, = 1 for scalars [I]
!  iocc    [Opt]: Index of the occurence, by default = 1 [I]kscal_
!  kscal   [Opt]: Value to be saved in the case of a character parameter [K24]
!  iscal   [Opt]: Value to be saved in the case of an integer parameter [I]
!  rscal   [Opt]: Value to be saved in the case of a float parameter [R8]
!  cscal   [Opt]: Value to be saved in the case of a complex parameter [C8]
!  kvect   [Opt]: Vector to be saved in the case of character parameters [K24]
!  ivect   [Opt]: Vector to be saved in the case of integer parameters [I]
!  rvect   [Opt]: Vector to be saved in the case of float parameters [R8]
!  cvect   [Opt]: Vector to be saved in the case of complex parameters [C8]
!
! 1 - First, we verify that the parameter name is valid
! 2 - Second, we save the given value/vector in the correct work vector  
!     according to the sd_int_ data structure's map
!
! Examples : call intsav('&&OP0074','SCHEME',1, kscal=method)
!            call intsav('&&OP0074','MASS_DIA',1, rvect=(1,1,1))
! ----------------------------------------------------------------------
! person_in_charge: hassan.berro at edf.fr    
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "intdef.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/dcopy.h"
#include "blas/zcopy.h"
!   ====================================================================
!   = 0 =   Variable declarations and initialization
!   ====================================================================
!
!   -0.1- Input/output arguments
    character(len=*)          , intent(in) :: sd_int_
    integer                   , intent(in) :: ip
    integer                   , intent(in) :: lonvec
    integer,          optional, intent(in) :: iocc
    character(len=*), optional, intent(in) :: kscal
    integer,          optional, intent(in) :: iscal
    real(kind=8),     optional, intent(in) :: rscal
    complex(kind=8),  optional, intent(in) :: cscal   
    character(len=*), optional, intent(in) :: kvect(lonvec)
    integer,          optional, intent(in) :: ivect(lonvec)
    real(kind=8),     optional, intent(in) :: rvect(lonvec)
    complex(kind=8),  optional, intent(in) :: cvect(lonvec)
    integer, pointer, optional, intent(in) :: buffer(:)
!
!   -0.2- Local variables
!   --- For strings copying
    character(len=8) :: sd_int
    character(len=24):: kscal_
    character(len=24):: savejv
    character(len=24), pointer :: kvect_(:) => null()


!   --- For general usage
    aster_logical     :: input_test 
    integer           :: i, jvect, jscal, iret
    integer           :: dec, level, lvec, addr
    character(len=6)  :: k_iocc
!
#include "intinc.h"
!
    savejv = '                        '

!   Copying the input strings, in order to allow in-command truncated input
    sd_int = sd_int_

    if (present(kscal)) kscal_ = kscal
    if (present(kvect)) then
        AS_ALLOCATE(vk24=kvect_, size=lonvec)
        do i = 1, lonvec
            kvect_(i)=kvect(i)
        end do
    end if
!
!   ====================================================================
!   = 1 = Validation of the input arguments, distinguishing global vars
!   ====================================================================

    input_test = UN_PARMI4(kscal, iscal, rscal, cscal) .or. &
                 UN_PARMI4(kvect, ivect, rvect, cvect)

    ASSERT(input_test)

    if (lonvec.gt.1) then
        input_test = UN_PARMI4(kvect, ivect, rvect, cvect)
        ASSERT(input_test)
    end if

!   The parameter to be saved was not found in the predefined list
    if (ip.gt._INT_NBPAR) then 
        ASSERT(.false.)
    end if

    if (present(buffer)) then

        dec = 0
        if (present(iocc)) then 
            level = size(buffer)/(2*_INT_NBPAR)
            if (iocc.le.level) then
                dec = (iocc-1)*2*_INT_NBPAR
            else
                goto 20
            endif
        end if

        addr = buffer(dec+ip)
        lvec = buffer(dec+_INT_NBPAR+ip)

        if ((addr.ne.0).and.(lonvec.eq.lvec)) then
            if (present(iscal)) then
                zi(addr) = iscal
            elseif (present(rscal)) then
                zr(addr) = rscal
            elseif (present(cscal)) then
                zc(addr) = cscal
            elseif (present(kscal)) then
                if (partyp(ip).eq.'K8 ') zk8(addr)  = kscal
                if (partyp(ip).eq.'K16') zk16(addr) = kscal
                if (partyp(ip).eq.'K24') zk24(addr) = kscal
            elseif (present(ivect)) then
                do i = 1, lvec
                    zi(addr+i-1) = ivect(i)
                end do
            elseif (present(rvect)) then
                call dcopy(lvec, rvect, 1, zr(addr), 1)
            elseif (present(cvect)) then
                call zcopy(lvec, cvect, 1, zc(addr), 1)
            elseif (present(kvect)) then
                if (partyp(ip).eq.'K8 ') then
                    do i = 1, lvec
                        zk8(addr+i-1) = kvect(i)
                    end do
                elseif (partyp(ip).eq.'K16') then
                    do i = 1, lvec
                        zk16(addr+i-1) = kvect(i)
                    end do
                elseif (partyp(ip).eq.'K24') then
                    do i = 1, lvec
                        zk24(addr+i-1) = kvect(i)
                    end do
                endif
            endif
            goto 99
        end if
    end if

20  continue

    savejv(1:8) = sd_int
    if (present(iocc)) then 
!       The parameter to be saved is global but an occurence index was given
        ASSERT(parind(ip).gt.0)
        call codent(iocc, 'G', k_iocc)
        savejv(9:15) = '.'//k_iocc(1:6)
    endif
    savejv(16:24)='.'//params(ip)

!   ====================================================================
!   = 2 = Saving data
!   ====================================================================

!   --- Vectors
    if (abs(parind(ip)).eq.2) then 
!
!       The parameter to be saved is a vector but no vector input was found
        input_test = UN_PARMI4(kvect, ivect, rvect, cvect)
        ASSERT(input_test)
        ASSERT(lonvec.ge.1)
!
        call jeexin(savejv, iret)
        if (iret.gt.0) then 
            call jeveuo(savejv, 'E', jvect)
        else 
            call wkvect(savejv, 'V V '//partyp(ip), lonvec, jvect)
        endif
!
        if (partyp(ip).eq.'K8') then
            do i = 1, lonvec
                zk8(jvect+i-1) = kvect_(i)(1:8)
            end do
        elseif (partyp(ip).eq.'K16') then
            do i = 1, lonvec
                zk16(jvect+i-1) = kvect_(i)(1:16)
            end do
        elseif (partyp(ip).eq.'K24') then
            do i = 1, lonvec
                zk24(jvect+i-1) = kvect_(i)
            end do
        else if (partyp(ip).eq.'R') then
            call dcopy(lonvec, rvect, 1, zr(jvect), 1)
        else if (partyp(ip).eq.'C') then
            call zcopy(lonvec, cvect, 1, zc(jvect), 1)            
        else if (partyp(ip).eq.'I') then
            do i = 1, lonvec
                zi(jvect+i-1) = ivect(i)
            end do
        end if
!
!   --- Scalars
    else if (abs(parind(ip)).eq.1) then
!
!       The parameter to be saved is a scalar but no scalar input was found
        input_test = UN_PARMI4(kscal, iscal, rscal, cscal)
        ASSERT(input_test)
!
        call jeexin(savejv, iret)
        if (iret.gt.0) then 
            call jeveuo(savejv, 'E', jscal)
        else 
            call wkvect(savejv, 'V V '//partyp(ip), 1, jscal)
        endif
!
        if (partyp(ip).eq.'K8 ') then 
            zk8(jscal) = kscal_(1:8)
        else if (partyp(ip).eq.'K16') then 
            zk16(jscal) = kscal_(1:16)
        else if (partyp(ip).eq.'K24') then 
            zk24(jscal) = kscal_
        elseif (partyp(ip).eq.'R') then 
            zr(jscal) = rscal
        elseif (partyp(ip).eq.'C') then 
            zc(jscal) = cscal
        elseif (partyp(ip).eq.'I')  then 
            zi(jscal) = iscal
        end if
!
    end if
!
    if (present(kvect)) then
        AS_DEALLOCATE(vk24=kvect_)
    end if

99  continue
!
end subroutine