subroutine cazouu(keywf, nb_cont_zone, keyw_)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getmjm.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/utmess.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=16), intent(in) :: keywf
    integer, intent(in) :: nb_cont_zone
    character(len=*), intent(in) :: keyw_
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Check if keyword is the same for all contact zones
!
! --------------------------------------------------------------------------------------------------
!
! In  keywf            : factor keyword to read
! In  nb_cont_zone     : number of zones of contact
! In  keyw             : keyword
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_keyw
    parameter   (nb_keyw=99)
    character(len=3) :: keyw_type(nb_keyw)
    character(len=16) :: keyw_name(nb_keyw)
!
    character(len=1) :: s_keyw_cata_type
    character(len=8) :: keyw_cata_type
    character(len=16) :: keyw
    aster_logical :: l_error
    integer :: i_zone, noc, nb_keyw_cata, i_keyw_cata, n
    real(kind=8) :: vale_r
    integer :: vale_i
    character(len=16) :: vale_k
    real(kind=8) :: vale_refe_r
    integer :: vale_refe_i
    character(len=16) :: vale_refe_k
!
! --------------------------------------------------------------------------------------------------
!
    l_error = .false.
    i_zone  = 1
    keyw    = keyw_
!
! - Get list of keywords in catalog
!
    call getmjm(keywf       , i_zone, 1, keyw_name, keyw_type,&
                nb_keyw_cata)
    nb_keyw_cata = abs(nb_keyw_cata)
    ASSERT(nb_keyw_cata.lt.nb_keyw)
!
    if (nb_keyw_cata .ne. 0) then
!
! ----- Loop on contact zones
!
        do i_zone = 1, nb_cont_zone
            call getmjm(keywf, i_zone, nb_keyw_cata, keyw_name, keyw_type,&
                        n)
!
! --------- Loop on keywords in catalog
!
            do i_keyw_cata = 1, nb_keyw_cata
                if (keyw_name(i_keyw_cata) .eq. keyw) then
!
! ----------------- Read value (depends on type)
!
                    keyw_cata_type = keyw_type(i_keyw_cata)
                    vale_i         = 0
                    vale_r         = 0.d0
                    vale_k         = ' '
                    if (keyw_cata_type(1:1) .eq. 'I') then
                        s_keyw_cata_type = 'I'
                        call getvis(keywf, keyw, iocc=i_zone, scal=vale_i, nbret=noc)
                    else if (keyw_cata_type(1:2).eq.'TX') then
                        s_keyw_cata_type = 'T'
                        call getvtx(keywf, keyw, iocc=i_zone, scal=vale_k, nbret=noc)
                    else if (keyw_cata_type(1:2).eq.'R8') then
                        s_keyw_cata_type = 'R'
                        call getvr8(keywf, keyw, iocc=i_zone, scal=vale_r, nbret=noc)
                    else
                        ASSERT(.false.)
                    endif
                    if (noc .ne. 0) then
                        if (i_zone .eq. 1) then
                            vale_refe_i = vale_i
                            vale_refe_r = vale_r
                            vale_refe_k = vale_k
                        else
                            if (s_keyw_cata_type .eq. 'I') then
                                if (vale_i .ne. vale_refe_i) then
                                    l_error = .true.
                                    goto 99
                                endif
                            else if (s_keyw_cata_type.eq.'R') then
                                if (abs(vale_r - vale_refe_r).le.r8prem()) then
                                    l_error = .true.
                                    goto 99
                                endif
                            else if (s_keyw_cata_type.eq.'T') then
                                if (vale_k .ne. vale_refe_k) then
                                    l_error = .true.
                                    goto 99
                                endif
                            else
                                ASSERT(.false.)
                            endif
                        endif
                    endif
                endif
            end do
        end do
    endif
!
 99 continue
!
    if (l_error) then
        call utmess('F', 'CONTACT3_4', sk=keyw)
    endif
!
end subroutine
