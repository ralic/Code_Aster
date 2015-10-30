subroutine dtmforeq(sd_dtm_, sd_int_, index, buffdtm, buffint)
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
! dtmforeq : Calculate the equilibrium forces for a given step specified by the argument
!           "index".
!
#include "jeveux.h"
#include "asterfort/dtmget.h"
#include "asterfort/intget.h"
#include "asterfort/intbuff.h"
#include "asterfort/intinivec.h"
#include "asterfort/pmavec.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"

!
!   -0.1- Input/output arguments
    character(len=*) , intent(in) :: sd_dtm_
    character(len=*) , intent(in) :: sd_int_
    integer          , intent(in) :: index
    integer, pointer , intent(in) :: buffdtm(:)
    integer, pointer              :: buffint(:)
!
!   -0.2- Local variables
    aster_logical    :: mdiag, kdiag, cdiag
    integer          :: nbmode, i, iret
    character(len=8) :: sd_dtm, sd_int
!
    real(kind=8)    , pointer :: depl(:)   => null()
    real(kind=8)    , pointer :: vite(:)   => null()
    real(kind=8)    , pointer :: acce(:)   => null()
    real(kind=8)    , pointer :: fext(:)   => null()
    real(kind=8)    , pointer :: masgen(:)   => null()
    real(kind=8)    , pointer :: riggen(:)   => null()
    real(kind=8)    , pointer :: amogen(:)   => null()
!
!   0 - Initializations
    sd_dtm = sd_dtm_
    sd_int = sd_int_
!
    call dtmget(sd_dtm, _NB_MODES, iscal=nbmode, buffer=buffdtm)

    mdiag = .false.
    call intget(sd_int, MASS_FUL, iocc=1, lonvec=iret, buffer=buffint)
    if (iret.gt.0) then
        call intget(sd_int, MASS_FUL, iocc=1, vr=masgen, buffer=buffint)
    else
        call intget(sd_int, MASS_DIA, iocc=1, vr=masgen, buffer=buffint)
        mdiag = .true.
    end if

    kdiag = .false.
    call intget(sd_int, RIGI_FUL, iocc=1, lonvec=iret, buffer=buffint)
    if (iret.gt.0) then
        call intget(sd_int, RIGI_FUL, iocc=1, vr=riggen, buffer=buffint)
    else
        call intget(sd_int, RIGI_DIA, iocc=1, vr=riggen, buffer=buffint)
        kdiag = .true.
    end if

    cdiag = .false.
    call intget(sd_int, AMOR_FUL, iocc=1, lonvec=iret, buffer=buffint)
    if (iret.gt.0) then
        call intget(sd_int, AMOR_FUL, iocc=1, vr=amogen, buffer=buffint)
    else
        call intget(sd_int, AMOR_DIA, iocc=1, vr=amogen, buffer=buffint)
        cdiag = .true.
    end if

    call intget(sd_int, DEPL, iocc=index, vr=depl, buffer=buffint)
    call intget(sd_int, VITE, iocc=index, vr=vite, buffer=buffint)
    call intget(sd_int, ACCE, iocc=index, vr=acce, buffer=buffint)

    call intget(sd_int, FORCE_EX, iocc=index, lonvec=iret)
    if (iret.eq.0) then
        call intinivec(sd_int, FORCE_EX, nbmode, iocc=index, vr=fext)
    else 
        call intget(sd_int, FORCE_EX, iocc=index, vr=fext, buffer=buffint)
    end if

!   --- M is diagonal
    if (mdiag) then
        if (kdiag) then
            if (cdiag) then
                do i = 1, nbmode
                    fext(i) = masgen(i)*acce(i) + masgen(i)*amogen(i)*vite(i) &
                                                + riggen(i)*depl(i)
                end do
            else
!               --- Calculate C[nxn] * V[n*1]
                call pmavec('ZERO', nbmode, amogen, vite, fext)
                do i = 1, nbmode
                    fext(i) = masgen(i)*acce(i) + fext(i) &
                                                + riggen(i)*depl(i)
                end do
            endif
        else
!           --- Calculate the term M[nxn] * A[nx1] + C[nxn] * V[nx1]
            if (cdiag) then
                do i = 1, nbmode
                    fext(i) = masgen(i)*acce(i) + masgen(i)*amogen(i)*vite(i)
                end do
            else
                do i = 1, nbmode
                    fext(i) = masgen(i)*acce(i)
                end do
                call pmavec('CUMU', nbmode, amogen, vite, fext)
            end if

!           --- Add up the term K[nxn] * X[n*1]
            call pmavec('CUMU', nbmode, riggen, depl, fext)
        end if

!   --- M is not diagonal
    else
!       --- Calculate the term C[nxn] * V[nx1] = Zeta[nxn] * M [nxn] * V [nx1]
        if (cdiag) then
            call pmavec('ZERO', nbmode, masgen, vite, fext)
            do i = 1, nbmode
                fext(i) = amogen(i)*fext(i)
            end do    
        else
            call pmavec('ZERO', nbmode, amogen, vite, fext)
        endif

!       --- Add up the term M[nxn] * A[nx1]
        call pmavec('CUMU', nbmode, masgen, acce, fext)

!       --- Add up the term K[nxn] * X[nx1]
        if (kdiag) then
            do i = 1, nbmode
                fext(i) = fext(i) + riggen(i)*depl(i)
            end do     
        else
            call pmavec('CUMU', nbmode, riggen, depl, fext)
        end if
    endif

end subroutine
