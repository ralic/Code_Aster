subroutine char_read_tran(keywordfact , iocc      , ndim,&
                          l_tran_     , tran_     ,&
                          l_cent_     , cent_     ,&
                          l_angl_naut_, angl_naut_)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getexm.h"
#include "asterc/r8dgrd.h"
#include "asterfort/assert.h"
#include "asterfort/getvr8.h"
#include "asterfort/utmess.h"
!
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
!
    character(len=16), intent(in) :: keywordfact
    integer, intent(in) :: iocc
    integer, intent(in) :: ndim
    aster_logical, optional, intent(out) :: l_tran_
    real(kind=8), optional, intent(out) :: tran_(3)
    aster_logical, optional, intent(out) :: l_cent_
    real(kind=8), optional, intent(out) :: cent_(3)
    aster_logical, optional, intent(out) :: l_angl_naut_
    real(kind=8), optional, intent(out) :: angl_naut_(3)
!
! --------------------------------------------------------------------------------------------------
!
! AFFE_CHAR_MECA
!
! Read transformation
!
! --------------------------------------------------------------------------------------------------
!
! In  keywordfact      : factor keyword to read
! In  iocc             : factor keyword index in AFFE_CHAR_MECA
! In  ndim             : space dimension
! Out l_tran           : .true. if TRAN defined (translation)
! Out tran             : vector defining translation
! Out l_cent           : .true. if center defined (rotation)
! Out cent             : vector defining center
! Out l_angl_naut      : .true. if angl defined (rotation)
! Out angl_naut        : angle defining rotation
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nangmx, i
    integer :: ntran, ncent, nangl, vali(2)
    aster_logical :: l_tran, l_cent, l_angl_naut
    real(kind=8) :: tran(3), cent(3), angl_naut(3) 
!
! --------------------------------------------------------------------------------------------------
!
    l_tran         = .false.
    l_angl_naut    = .false.
    l_cent         = .false.
    tran(1:3)      = 0.d0
    cent(1:3)      = 0.d0
    angl_naut(1:3) = 0.d0
    if (ndim .eq. 3) then
        nangmx = 3
    else
        nangmx = 1
    endif
!
! - Translation
!
    if (getexm(keywordfact,'TRAN') .eq. 1) then
        call getvr8(keywordfact, 'TRAN', iocc=iocc, nbval=0, nbret=ntran)
        ntran = -ntran
        if (ntran .ne. 0) then
            l_tran = .true.
            if (ntran .ne. ndim) then
                vali(1) = ndim
                vali(2) = ndim
                call utmess('F', 'CHARGES2_42', sk='TRAN', ni=2, vali=vali)
            endif
            call getvr8(keywordfact, 'TRAN', iocc=iocc, nbval=ntran, vect=tran)
        endif
    endif
!
! - Rotation
!
    if (getexm(keywordfact,'CENTRE') .eq. 1) then
        call getvr8(keywordfact, 'CENTRE', iocc=iocc, nbval=0, nbret=ncent)
        ncent = -ncent
        if (ncent .ne. 0) then
            l_cent = .true.
            if (ncent .ne. ndim) then
                vali(1) = ndim
                vali(2) = ndim
                call utmess('F', 'CHARGES2_42', sk='CENTRE', ni=2, vali=vali)
            endif
            call getvr8(keywordfact, 'CENTRE', iocc=iocc, nbval=ncent, vect=cent)
        endif
    endif
!
    if (getexm(keywordfact,'ANGL_NAUT') .eq. 1) then
        call getvr8(keywordfact, 'ANGL_NAUT', iocc=iocc, nbval=0, nbret=nangl)
        nangl = -nangl
        if (nangl .ne. 0) then
            l_angl_naut = .true.
            if (nangl .ne. nangmx) then
                vali(1) = nangmx
                vali(2) = ndim
                call utmess('F', 'CHARGES2_42', sk='ANGL_NAUT', ni=2, vali=vali)
            endif
            call getvr8(keywordfact, 'ANGL_NAUT', iocc=iocc, nbval=nangmx, vect=angl_naut)
            do i = 1, 3
                angl_naut(i) = angl_naut(i)*r8dgrd()
            end do
        endif
    endif
!
! - Copy output
!
    if (present(l_tran_)) then
        l_tran_    = l_tran
        tran_(1:3) = tran(1:3)
    endif
    if (present(l_cent_)) then
        l_cent_    = l_cent
        cent_(1:3) = cent(1:3)
    endif
    if (present(l_angl_naut_)) then
        l_angl_naut_    = l_angl_naut
        angl_naut_(1:3) = angl_naut(1:3)
    endif
!
end subroutine
