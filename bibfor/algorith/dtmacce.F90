subroutine dtmacce(sd_dtm_, sd_int_, index, buffdtm, buffint, calcf)
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
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE imPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: hassan.berro at edf.fr
!
! dtmacce : Calculates the acceleration from the equilibrium of the equation of motion.
!           The results are saved in the integration data structure.
!
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dtmforc.h"
#include "asterfort/dtmget.h"
#include "asterfort/intget.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/pmavec.h"
#include "asterfort/resoud.h"
#include "asterfort/rrlds.h"
#include "asterfort/wkvect.h"
#include "asterfort/uttcpu.h"
#include "asterfort/uttcpr.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
!   -0.1- Input/output arguments
    character(len=*),        intent(in) :: sd_dtm_
    character(len=*),        intent(in) :: sd_int_
    integer         ,        intent(in) :: index
    integer, pointer,        intent(in) :: buffdtm(:)
    integer, pointer,        intent(in) :: buffint(:)
    aster_logical, optional, intent(in) :: calcf
!
!   -0.2- Local variables
    aster_logical     :: mdiag, kdiag, cdiag, instrum, calcforc
    integer           :: index_m, nbmode, iret, iret2
    integer           :: i
    real(kind=8)      :: tps(7), omega2
    complex(kind=8)   :: cbid
!
    character(len=8)  :: sd_dtm, sd_int
    character(len=24) :: solver

!
    integer         , pointer :: matdesc(:) => null()
!
    real(kind=8)    , pointer :: depl(:)    => null()
    real(kind=8)    , pointer :: vite(:)    => null()
    real(kind=8)    , pointer :: acce(:)    => null()
    real(kind=8)    , pointer :: fext(:)    => null()
    real(kind=8)    , pointer :: work(:)    => null()
    real(kind=8)    , pointer :: masgen(:)  => null()
    real(kind=8)    , pointer :: riggen(:)  => null()
    real(kind=8)    , pointer :: amogen(:)  => null()
    real(kind=8)    , pointer :: masfact(:)  => null()
!
!   0 - Initializations
    sd_dtm = sd_dtm_
    sd_int = sd_int_
    cbid = dcmplx(0.d0, 0.d0)

    calcforc = .true.
    if (present(calcf)) calcforc = calcf

    instrum = .false.
    if (instrum) then
        call uttcpu('CPU.DTMACE', 'INIT', ' ')
        call uttcpu('CPU.DTMACE', 'DEBUT', ' ')
    end if

!    call uttcpu('CPU.DTMACE', 'INIT', ' ')
!    call uttcpu('CPU.DTMACE', 'DEBUT', ' ')
!    do i=1,10000
!        call dtmget(sd_dtm, _NB_MODES, iscal=nbmode, buffer=buffdtm)
!    end do
!    call uttcpu('CPU.DTMACE', 'FIN', ' ')
!    call uttcpr('CPU.DTMACE', 7, tps)
!    write(*,*) "10000 calls to NB_MODES with buffer requires (s): ", tps(7)
!
!    call uttcpu('CPU.DTMACE', 'DEBUT', ' ')
!    do i=1,10000
!        call dtmget(sd_dtm, _NB_MODES, iscal=nbmode)
!    end do
!    call uttcpu('CPU.DTMACE', 'FIN', ' ')
!    call uttcpr('CPU.DTMACE', 7, tps)
!    write(*,*) "10000 calls to NB_MODES without buffer requires (s): ", tps(7)

!
!   1 - Retrieval of the necessary information
    call dtmget(sd_dtm, _NB_MODES, iscal=nbmode, buffer=buffdtm)
    call dtmget(sd_dtm, _MAT_DESC, vi=matdesc, buffer=buffdtm)
!
!   --- Test whether the matrices change with the integration steps
    if (index.eq.1) then
        index_m = 1
        goto 10
    end if

    index_m = -1
    if (matdesc(1).ne.0) then
        do i = 0 , index-1
            call intget(sd_int, AMOR_FUL, iocc=index-i, lonvec=iret, buffer=buffint)
            call intget(sd_int, AMOR_DIA, iocc=index-i, lonvec=iret2, buffer=buffint)
            if ((iret+iret2).ne.0) then
                index_m = index-i
                goto 10
            end if
        end do
    else
        do i = 0 , index-1
            call intget(sd_int, MASS_FUL, iocc=index-i, lonvec=iret, buffer=buffint)
            call intget(sd_int, MASS_DIA, iocc=index-i, lonvec=iret2, buffer=buffint)
            if ((iret+iret2).ne.0) then
                index_m = index-i
                goto 10
            end if
        end do
    end if
!
!   --- Can not find the index of the matrices, something is wrong...
    ASSERT(index_m.gt.0)
10  continue

    mdiag = .false.
    if (matdesc(1).eq.0) then
        call intget(sd_int, MASS_FUL, iocc=index_m, lonvec=iret, buffer=buffint)
        if (iret.gt.0) then
            call intget(sd_int, MASS_FUL, iocc=index_m, vr=masgen, buffer=buffint)
        else
            call intget(sd_int, MASS_DIA, iocc=index_m, vr=masgen, buffer=buffint)
            mdiag = .true.
        end if
    endif

    kdiag = .false.
    if (matdesc(2).eq.0) then
        call intget(sd_int, RIGI_FUL, iocc=index_m, lonvec=iret, buffer=buffint)
        if (iret.gt.0) then
            call intget(sd_int, RIGI_FUL, iocc=index_m, vr=riggen, buffer=buffint)
        else
            call intget(sd_int, RIGI_DIA, iocc=index_m, vr=riggen, buffer=buffint)
            kdiag = .true.
        end if
    end if

    cdiag = .false.
    if (matdesc(3).eq.0) then
        call intget(sd_int, AMOR_FUL, iocc=index_m, lonvec=iret, buffer=buffint)
        if (iret.gt.0) then
            call intget(sd_int, AMOR_FUL, iocc=index_m, vr=amogen, buffer=buffint)
        else
            call intget(sd_int, AMOR_DIA, iocc=index_m, vr=amogen, buffer=buffint)
            cdiag = .true.
        end if
    end if
!
    call intget(sd_int, DEPL, iocc=index, vr=depl, buffer=buffint)
    call intget(sd_int, VITE, iocc=index, vr=vite, buffer=buffint)
    call intget(sd_int, ACCE, iocc=index, vr=acce, buffer=buffint)
!
!   --- Calculate external forces if needed
    if (calcforc) call dtmforc(sd_dtm, sd_int, index, buffdtm, buffint)
    call intget(sd_int, FORCE_EX, iocc=index, vr=fext, buffer=buffint)

    call dtmget(sd_dtm, _ACC_WORK, vr=work, buffer=buffdtm)
!
!   --- M is diagonal
    if (mdiag) then
        if (kdiag) then
            if (cdiag) then
                do i = 1, nbmode
                    omega2  = riggen(i)/masgen(i)
                    acce(i) = fext(i)/masgen(i) -    omega2*depl(i) &
                                                - amogen(i)*vite(i)
                end do
            else
!               --- Calculate C[nxn] * V[n*1]
                call pmavec('ZERO', nbmode, amogen, vite, work)
                do i = 1, nbmode
                    acce(i) = fext(i) - riggen(i)*depl(i) - work(i)
                    acce(i) = acce(i) / masgen(i)
                end do
            endif
        else
!           --- Calculate the term C[nxn] * V[nx1] = Zeta[nxn] * M [nxn] * V [nx1]
            if (cdiag) then
                do i = 1, nbmode
                    work(i) = amogen(i)*masgen(i)*vite(i)
                end do
            else
                call pmavec('ZERO', nbmode, amogen, vite, work)
            end if

!           --- Add up the term K[nxn] * X[n*1] + C[nxn] * V[n*1]
            call pmavec('CUMU', nbmode, riggen, depl, work)
            do i = 1, nbmode
                acce(i) = fext(i) - work(i)
                acce(i) = acce(i) / masgen(i)
            end do
        end if
!   --- M is not diagonal
    else
!       --- Calculate the term C[nxn] * V[nx1] = Zeta[nxn] * M [nxn] * V [nx1]
        if (cdiag) then
            if (matdesc(1).ne.0) then
                call mrmult('ZERO', matdesc(1), vite, work, 1, .false._1) 
            else
                call pmavec('ZERO', nbmode, masgen, vite, work)
            end if
            do i = 1, nbmode
                work(i) = amogen(i)*work(i)
            end do
        else if (matdesc(3).ne.0) then
            call mrmult('ZERO', matdesc(3), vite, work, 1, .false._1)        
        else
            call pmavec('ZERO', nbmode, amogen, vite, work)
        endif
!       --- Add up the term K[nxn] * X[nx1]
        if (kdiag) then
            do i = 1, nbmode
                work(i) = work(i) + riggen(i)*depl(i)
            end do
        else if (matdesc(2).ne.0) then
            call mrmult('CUMU', matdesc(2), depl, work, 1, .false._1)        
        else
            call pmavec('CUMU', nbmode, riggen, depl, work)
        end if

!       --- Calculate the term B = F - KX - CV (saved under acce)
        do i = 1, nbmode
            acce(i) = fext(i) - work(i)
        end do

!       --- Resolve the linear equation : A.X = B (A: MASS, X: ACCE, B: F-KX-CV)
        if (matdesc(1).ne.0) then
            call dtmget(sd_dtm, _SOLVER, savejv=solver)
            call resoud(zk24(zi(matdesc(1)+1))(1:19), ' ', solver, ' ', 1,&
                                                      ' ', ' ', ' ', acce, [cbid],&
                                                      ' ', .true._1, 0, iret)
        else
            call intget(sd_int, MASS_FAC, iocc=index_m, vr=masfact, buffer=buffint)
            call rrlds(masfact, nbmode, nbmode, acce, 1)
        end if
    endif

    if (instrum) then
        call uttcpu('CPU.DTMACE', 'FIN', ' ')
        call uttcpr('CPU.DTMACE', 7, tps)
        write(*,*) "ELPSD DTMACE", tps(7)
    end if

end subroutine