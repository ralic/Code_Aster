subroutine dtmforc(sd_dtm_, sd_int_, index, buffdtm, buffint, nlaccnt)
    implicit none
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
! person_in_charge: hassan.berro at edf.fr
!
! dtmforc : Calculate the forces at the current step, specified by the argument
!           "index".
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dtmcase_coder.h"
#include "asterfort/dtmforc_ants.h"
#include "asterfort/dtmforc_choc.h"
#include "asterfort/dtmforc_flam.h"
#include "asterfort/dtmforc_decr.h"
#include "asterfort/dtmforc_dvis.h"
#include "asterfort/dtmforc_rede.h"
#include "asterfort/dtmforc_revi.h"
#include "asterfort/dtmforc_rotf.h"
#include "asterfort/dtmget.h"
#include "asterfort/dtmsav.h"
#include "asterfort/intget.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/intbuff.h"
#include "asterfort/intinivec.h"
#include "asterfort/mdfedy.h"
#include "asterfort/mdfext.h"
#include "asterfort/nlget.h"
#include "asterfort/pmavec.h"
#include "asterfort/pmtvec.h"
#include "asterfort/uttcpu.h"
#include "asterfort/uttcpr.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"

!
!   -0.1- Input/output arguments
    character(len=*) , intent(in) :: sd_dtm_
    character(len=*) , intent(in) :: sd_int_
    integer          , intent(in) :: index
    integer, pointer , intent(in) :: buffdtm(:)
    integer, pointer              :: buffint(:)
    integer, optional, intent(in) :: nlaccnt

!
!   -0.2- Local variables
    aster_logical    :: instrum
    integer          :: nbmode, ntotex, nbnli, ind
    integer          :: iret, i, nlcase, nlacc, nl_type, inl
    real(kind=8)     :: temps, dt, r8b, tps(7)
    character(len=7) :: casek7
    character(len=8) :: sd_dtm, sd_int, sd_nl
!
    integer         , pointer :: idescf(:)  => null()
    integer         , pointer :: liad(:)    => null()
    integer         , pointer :: inumor(:)  => null()
    real(kind=8)    , pointer :: depl0(:)   => null()
    real(kind=8)    , pointer :: vite0(:)   => null()
    real(kind=8)    , pointer :: acce0(:)   => null()
    real(kind=8)    , pointer :: fext0(:)   => null()
    real(kind=8)    , pointer :: depl(:)    => null()
    real(kind=8)    , pointer :: vite(:)    => null()
    real(kind=8)    , pointer :: acce(:)    => null()
    real(kind=8)    , pointer :: fext(:)    => null()
    real(kind=8)    , pointer :: phi(:)     => null()
    real(kind=8)    , pointer :: fext_nl(:) => null()
    real(kind=8)    , pointer :: fext_tgt(:)=> null()
    real(kind=8)    , pointer :: fadd_nl(:) => null()
    real(kind=8)    , pointer :: coefm(:)   => null()
    character(len=8), pointer :: nomfon(:)  => null()
    integer, pointer          :: buffnl(:)  => null()

!
!   0 - Initializations
    sd_dtm = sd_dtm_
    sd_int = sd_int_
!
    nlacc = 0
    if (present(nlaccnt)) nlacc=nlaccnt
!
    instrum = .false.
    if (instrum) then
        call uttcpu('CPU.DTMFORC', 'INIT', ' ')
        call uttcpu('CPU.DTMFORC', 'DEBUT', ' ')
    end if

!
    call dtmget(sd_dtm, _NB_MODES, iscal=nbmode, buffer=buffdtm)
    call dtmget(sd_dtm, _NB_EXC_T, iscal=ntotex, buffer=buffdtm)
    call dtmget(sd_dtm, _NB_NONLI, iscal=nbnli , buffer=buffdtm)
!
    call intget(sd_int, FORCE_EX, iocc=index, lonvec=iret, buffer=buffint)
    if (iret.eq.0) then
        call intinivec(sd_int, FORCE_EX, nbmode, iocc=index, vr=fext)
        nullify(buffint)
        call intbuff(sd_int, buffint, level=index)
    else
        call intget(sd_int, FORCE_EX, iocc=index, vr=fext, buffer=buffint)
    endif
!
    call dtmget(sd_dtm, _NL_CASE, iscal=nlcase, buffer=buffdtm)

    if (nlcase.eq.0) then
!       --- No implicit treatment of chocs or free state (vol), simply use the
!           integration displacements, velocities, and accelerations
        call intget(sd_int, DEPL , iocc=index, vr=depl, buffer=buffint)
        call intget(sd_int, VITE , iocc=index, vr=vite, buffer=buffint)
!       --- The acceleration at the preceding step is required
        call intget(sd_int, ACCE , iocc=1    , vr=acce, buffer=buffint)
    else
!       --- Implicit treatment of chocs, project these vectors back to the original
!           basis by calculating : [Phi] x DEPL, [Phi] x VITE, [Phi] x ACCE
        call intget(sd_int, DEPL , iocc=index, vr=depl0, buffer=buffint)
        call intget(sd_int, VITE , iocc=index, vr=vite0, buffer=buffint)
        call intget(sd_int, ACCE , iocc=1    , vr=acce0, buffer=buffint)
        call dtmcase_coder (nlcase, casek7)
        call jeveuo(sd_dtm // '.PRJ_BAS.' //casek7, 'E', vr=phi)
        call dtmget(sd_dtm, _IMP_DEPL, vr=depl, buffer=buffdtm)
        call dtmget(sd_dtm, _IMP_VITE, vr=vite, buffer=buffdtm)
        call dtmget(sd_dtm, _IMP_ACCE, vr=acce, buffer=buffdtm)
        call pmavec('ZERO', nbmode, phi, depl0, depl)
        call pmavec('ZERO', nbmode, phi, vite0, vite)
        call pmavec('ZERO', nbmode, phi, acce0, acce)
        nullify(fext)
        call dtmget(sd_dtm, _IMP_FEXT, vr=fext, buffer=buffdtm)
        call intget(sd_int, FORCE_EX, iocc=index, vr=fext0, buffer=buffint)
    end if

    call vecini(nbmode, 0.d0, fext)

    call intget(sd_int, INDEX, iocc=index, iscal=ind  , buffer=buffint)
    call intget(sd_int, TIME , iocc=index, rscal=temps, buffer=buffint)
    call intget(sd_int, STEP , iocc=index, rscal=dt   , buffer=buffint)
!
    if (ntotex .ne. 0) then
        call dtmget(sd_dtm, _DESC_FRC,vi=idescf, buffer=buffdtm)
        call dtmget(sd_dtm, _FUNC_NAM,vk8=nomfon, buffer=buffdtm)
        call dtmget(sd_dtm, _COEF_MLT,vr=coefm, buffer=buffdtm)
        call dtmget(sd_dtm, _ADRES_VC,vi=liad, buffer=buffdtm)
        call dtmget(sd_dtm, _N_ORD_VC,vi=inumor, buffer=buffdtm)
        call mdfext(temps, r8b, nbmode, ntotex, idescf,&
                    nomfon, coefm, liad, inumor, 1,&
                    fext)
    end if
!
    if (nbnli.ne.0) then
        call dtmget(sd_dtm, _SD_NONL  , kscal=sd_nl, buffer=buffdtm)
        call dtmget(sd_dtm, _NL_BUFFER, vi=buffnl  , buffer=buffdtm)

        call nlget (sd_nl,  _F_TOT_WK , vr=fext_nl , buffer=buffnl)
        call nlget (sd_nl,  _F_TAN_WK , vr=fext_tgt, buffer=buffnl)

        do inl = 1, nbnli
            call nlget(sd_nl, _NL_TYPE, iocc=inl, iscal=nl_type, buffer=buffnl)

            select case (nl_type)
!
                case(NL_CHOC)
                    call dtmforc_choc(inl, sd_dtm, sd_nl, buffdtm, buffnl,&
                                      temps, depl, vite, fext_nl, fext_tgt)

!                   --- Special case of choc nonlinearities that can be
!                       implicitely treated
                    if ((nlcase.eq.0).or.(nlacc.eq.1)) then
                        do i=1, nbmode
                            fext(i) = fext(i) + fext_nl(i)
                        end do
                    else
                        do i=1, nbmode
                            fext(i) = fext(i) + fext_tgt(i)
                        end do
                    end if
    
                case(NL_BUCKLING)
                    call dtmforc_flam(inl, sd_dtm, sd_nl, buffdtm, buffnl,&
                                      temps, depl, vite, fext)

                case(NL_ANTI_SISMIC)
                    call dtmforc_ants(inl, sd_dtm, sd_nl, buffdtm, buffnl,&
                                      temps, depl, vite, fext)

                case(NL_DIS_VISC)
                    call dtmforc_dvis(inl, sd_dtm, sd_nl, buffdtm, buffnl,&
                                      temps, dt, depl, vite, fext)

                case(NL_DIS_ECRO_TRAC)
                    call dtmforc_decr(inl, sd_dtm, sd_nl, buffdtm, buffnl,&
                                      temps, dt, depl, vite, fext)

                case(NL_CRACKED_ROTOR)
                    call dtmforc_rotf(inl,sd_dtm, sd_nl, buffdtm, buffnl,&
                                      temps, depl, fext)
    ! !
    !             case(NL_LUBRICATION)
    !                 call dtmprep_noli_pali(sd_dtm, sd_nl, icomp)
    ! 
                case(NL_FX_RELATIONSHIP)
                    call dtmforc_rede(inl, sd_dtm, sd_nl, buffdtm, buffnl,&
                                      depl, fext)
    ! ! 
                case(NL_FV_RELATIONSHIP)
                    call dtmforc_revi(inl, sd_dtm, sd_nl, buffdtm, buffnl,&
                                      vite, fext)
    !
                case default
                    ASSERT(.false.)
    !
            end select
        end do
!
        if (nlcase.ne.0) then
!           --- Implicit treatment of chocs, project the force to the new basis
!               by calculating : [Phi]_t x FORC
            call pmtvec('ZERO', nbmode, phi, fext, fext0)
            if (nlacc.eq.0) then
                call dtmget(sd_dtm, _F_NL_ADD, vr =fadd_nl, buffer=buffdtm)
                do i=1, nbmode
                    fext0(i) = fext0(i) + fadd_nl(i)
                end do
            end if
        end if
    end if

    if (instrum) then
        call uttcpu('CPU.DTMFORC', 'FIN', ' ')
        call uttcpr('CPU.DTMFORC', 7, tps)
        write(*,*) "ELPSD DTMFORC : ", tps(7)
    end if

end subroutine
