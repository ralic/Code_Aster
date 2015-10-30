subroutine intinivec(sd_int_, ip, lonvec, iocc, vi,&
                     vr, vc, vk8, vk16, vk24, address)
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
! Extract the value of a parameter in the temporary data structure for the
! command DYNA_TRAN_MODAL
!
!  sd_int_ [Obl]: Name of the int data structure requested [K24]
!  ip      [Obl]: Index of the parameter relating to the vector to be created [I]
!  lonvec  [Obl]: Length of the vector to be created [I]
!  iocc    [Opt]: Index of the occurence in the case of a non global vector [I]
!  vi      [Opt]: Pointer to the created integer vector     [I]
!  vr      [Opt]: Pointer to the created real vector        [R]
!  vc      [Opt]: Pointer to the created complex vector     [C]
!  vk8     [Opt]: Pointer to the created K8 strings vector  [K8]
!  vk16    [Opt]: Pointer to the created K16 strings vector [K16]
!  vk24    [Opt]: Pointer to the created K24 strings vector [K24]
!
! Examples : call intinivec('&&OP0074', IND_ARCH, vi=indarch)
!
! ----------------------------------------------------------------------
! person_in_charge: hassan.berro at edf.fr
!  
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "intdef.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
!
!   ====================================================================
!   = 0 =   Variable declarations and initialization
!   ====================================================================
!
!   -0.1- Input/output arguments
    character(len=*)                    , intent(in) :: sd_int_
    integer                             , intent(in) :: ip
    integer,                              intent(in) :: lonvec
    integer,                    optional, intent(in) :: iocc
    integer          , pointer, optional, intent(out) :: vi(:)
    real(kind=8)     , pointer, optional, intent(out) :: vr(:)
    complex(kind=8)  , pointer, optional, intent(out) :: vc(:)
    character(len=8) , pointer, optional, intent(out) :: vk8(:)
    character(len=16), pointer, optional, intent(out) :: vk16(:)
    character(len=24), pointer, optional, intent(out) :: vk24(:)
    integer                   , optional, intent(out) :: address
!
!   -0.2- Local variables
!   --- For strings copying
    character(len=8) :: sd_int

!   --- For general usage
    integer           :: iret, add, i
    character(len=6)  :: k_iocc
    character(len=24) :: savename
!
#include "intinc.h"
!
    savename = '                        '
    add = 0

!   Copying the input strings, in order to allow in-command truncated input
    sd_int = sd_int_

!   The parameter to be saved was not found in the predefined list
    if (ip.gt._INT_NBPAR) then 
        ASSERT(.false.)
    end if

    savename(1:8) = sd_int
    if (present(iocc)) then 
!       The parameter to be extracted is global but an occurence index was given
        ASSERT(parind(ip).gt.0)
        call codent(iocc, 'G', k_iocc)
        savename(9:15) = '.'//k_iocc(1:6)
    else
        ASSERT(parind(ip).lt.0)
    end if
    savename(16:24)='.'//params(ip)

    call jeexin(savename, iret)
    if (iret.ne.0) then
        ! write (*,*) "Careful, using intinivec with a pre-existing vector"
        ! write (*,*) "The original <", params(ip) ,"> vector is deleted"
        call jelibe(savename)       
        call jedetr(savename)
    end if

    if (partyp(ip).eq.'I') then
        if (present(vi)) then
            call wkvect(savename, 'V V I', lonvec, vi=vi)
            do i = 1, lonvec
                vi(i) = 0
            end do
        else 
            call wkvect(savename, 'V V I', lonvec, add)
            do i = 1, lonvec
                zi(add+i-1) = 0
            end do            
        end if
    else if (partyp(ip).eq.'R') then
        if (present(vr)) then
            call wkvect(savename, 'V V R', lonvec, vr=vr)
            do i = 1, lonvec
                vr(i) = 0.d0
            end do            
        else 
            call wkvect(savename, 'V V R', lonvec, add)
            do i = 1, lonvec
                zr(add+i-1) = 0.d0
            end do  
        end if
    else if (partyp(ip).eq.'C') then
        if (present(vc)) then
            call wkvect(savename, 'V V C', lonvec, vc=vc)
            do i = 1, lonvec
                vc(i) = dcmplx(0.d0, 0.d0)
            end do
        else 
            call wkvect(savename, 'V V C', lonvec, add)
            do i = 1, lonvec
                zc(add+i-1) = dcmplx(0.d0, 0.d0)
            end do 
        end if
    else if (partyp(ip).eq.'K8') then
        if (present(vk8)) then
            call wkvect(savename, 'V V K8', lonvec, vk8=vk8)
            do i = 1, lonvec
                vk8(i) = ' '
            end do
        else 
            call wkvect(savename, 'V V K8', lonvec, add)
            do i = 1, lonvec
                zk8(add+i-1) = ' '
            end do
        end if
    else if (partyp(ip).eq.'K16') then
        if (present(vk16)) then
            call wkvect(savename, 'V V K16', lonvec, vk16=vk16)
            do i = 1, lonvec
                vk16(i) = ' '
            end do
        else 
            call wkvect(savename, 'V V K16', lonvec, add)
            do i = 1, lonvec
                zk16(add+i-1) = ' '
            end do
        end if
    else if (partyp(ip).eq.'K24') then
        if (present(vk24)) then
            call wkvect(savename, 'V V K24', lonvec, vk24=vk24)
            do i = 1, lonvec
                vk24(i) = ' '
            end do
        else 
            call wkvect(savename, 'V V K24', lonvec, add)
            do i = 1, lonvec
                zk24(add+i-1) = ' '
            end do
        end if
    end if
    if (present(address)) then
        ASSERT(add.gt.0)
        address = add
    end if

end subroutine