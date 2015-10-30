subroutine dtmforc(sd_dtm_, sd_int_, index, buffdtm, buffint, nlaccnt)
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
! dtmforc : Calculate the forces at the current step, specified by the argument
!           "index".
!
#include "jeveux.h"
#include "asterfort/dtmcase_coder.h"
#include "asterfort/dtmget.h"
#include "asterfort/dtmsav.h"
#include "asterfort/intget.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/intbuff.h"
#include "asterfort/intinivec.h"
#include "asterfort/mdfcho.h"
#include "asterfort/mdfedy.h"
#include "asterfort/mdfext.h"
#include "asterfort/mdfred.h"
#include "asterfort/mdfrev.h"
#include "asterfort/mdrfis.h"
#include "asterfort/pmavec.h"
#include "asterfort/pmtvec.h"
#include "asterfort/r8inir.h"
#include "asterfort/uttcpu.h"
#include "asterfort/uttcpr.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/vecini.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"

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
    aster_logical    :: prdeff, instrum
    integer          :: nbmode, ntotex, nbnli, nbrfis, nbpal
    integer          :: nbrede, nbrevi, ind, nbconv, nbmxcv
    integer          :: iret, i, nlcase, nlacc
    real(kind=8)     :: dt, temps, r8b, vrotat, angini
    real(kind=8)     :: conv, dtedyos, tps(7)
    character(len=7) :: casek7
    character(len=8) :: sd_dtm, sd_int, monmot, foncp, nomres
!
    integer         , pointer :: idescf(:)  => null()
    integer         , pointer :: liad(:)    => null()
    integer         , pointer :: inumor(:)  => null()
    integer         , pointer :: nbsconv(:) => null()   
    integer         , pointer :: rangcho(:) => null()
    integer         , pointer :: saredi(:)  => null()
    integer         , pointer :: sarevi(:)  => null()
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
    real(kind=8)    , pointer :: deplcho(:) => null()
    real(kind=8)    , pointer :: paracho(:) => null()
    real(kind=8)    , pointer :: psidel(:)  => null()
    real(kind=8)    , pointer :: saucho(:)  => null()
    real(kind=8)    , pointer :: fsauv(:)   => null()
    real(kind=8)    , pointer :: dplred(:)  => null()
    real(kind=8)    , pointer :: saured(:)  => null()
    real(kind=8)    , pointer :: dplrev(:)  => null()
    real(kind=8)    , pointer :: saurev(:)  => null()
    character(len=8), pointer :: nomfon(:)  => null()
    character(len=8), pointer :: noeucho(:) => null()
    character(len=8), pointer :: inticho(:) => null()
    character(len=8), pointer :: nofdep(:)  => null()
    character(len=8), pointer :: nofvit(:)  => null()
    character(len=8), pointer :: nofacc(:)  => null()
    character(len=8), pointer :: fk(:)      => null()
    character(len=8), pointer :: dfk(:)     => null()
    character(len=8), pointer :: typal(:)   => null()
    character(len=8), pointer :: finpal(:)  => null()
    character(len=8), pointer :: cnpal(:)   => null()
    character(len=8), pointer :: fonred(:)  => null()
    character(len=8), pointer :: fonrev(:)  => null()
    character(len=8), target  :: blanc(1)
    real(kind=8)    , target  :: zeroarr(1)

!
!   0 - Initializations
    sd_dtm = sd_dtm_
    sd_int = sd_int_

    blanc(1) = ' '
    zeroarr(1) = 0.d0
    nofdep  => blanc
    nofvit  => blanc
    nofacc  => blanc
    psidel  => zeroarr
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
    call dtmget(sd_dtm, _NB_NONLI, iscal=nbnli, buffer=buffdtm)
    call dtmget(sd_dtm, _NB_R_FIS, iscal=nbrfis, buffer=buffdtm)
    call dtmget(sd_dtm, _NB_PALIE, iscal=nbpal, buffer=buffdtm)
    call dtmget(sd_dtm, _FX_NUMB , iscal=nbrede, buffer=buffdtm)
    call dtmget(sd_dtm, _FV_NUMB , iscal=nbrevi, buffer=buffdtm)
!
    call intget(sd_int, STEP    , iocc=index, rscal=dt, buffer=buffint)
!
    call intget(sd_int, FORCE_EX, iocc=index, lonvec=iret, buffer=buffint)
    if (iret.eq.0) then
        call intinivec(sd_int, FORCE_EX, nbmode, iocc=index, vr=fext)
        nullify(buffint)
        call intbuff(sd_int, buffint, level=index)
    else
        call intget(sd_int, FORCE_EX, iocc=index, vr=fext, buffer=buffint)
    endif

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

    call r8inir(nbmode, 0.d0, fext, 1)

    call intget(sd_int, INDEX, iocc=index, iscal=ind, buffer=buffint)
    call intget(sd_int, TIME , iocc=index, rscal=temps, buffer=buffint)

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

    if (nbnli.ne.0) then
        call dtmget(sd_dtm, _CHO_DEPL,vr =deplcho, buffer=buffdtm)
        call dtmget(sd_dtm, _CHO_PARA,vr =paracho, buffer=buffdtm)

        if (nbrfis .eq. 0) then

            call dtmget(sd_dtm, _F_TOT_WK,vr =fext_nl, buffer=buffdtm)
            call dtmget(sd_dtm, _F_TAN_WK,vr =fext_tgt, buffer=buffdtm)
            call vecini(nbmode,0.d0,fext_nl)
            call vecini(nbmode,0.d0,fext_tgt)
            
            call dtmget(sd_dtm, _CALC_SD ,kscal=nomres, buffer=buffdtm)
            call dtmget(sd_dtm, _CHO_RANK,vi =rangcho, buffer=buffdtm)
            call dtmget(sd_dtm, _CHO_NOEU,vk8=noeucho, buffer=buffdtm)
            call dtmget(sd_dtm, _CHO_NAME,vk8=inticho, buffer=buffdtm)
            call dtmget(sd_dtm, _NL_SAVES,vr =saucho, buffer=buffdtm)

            call jeexin(nomres//'           .FDEP', iret)
            if (iret.gt.0) then
                call jeveuo(nomres//'           .FDEP','L',vk8=nofdep)
                call jeveuo(nomres//'           .FVIT','L',vk8=nofvit)
                call jeveuo(nomres//'           .FACC','L',vk8=nofacc)
            end if

            call dtmget(sd_dtm, _PSI_DELT, lonvec=iret, buffer=buffdtm)
            if (iret.gt.0) call dtmget(sd_dtm, _PSI_DELT, vr=psidel, buffer=buffdtm)

            call dtmget(sd_dtm, _MULTI_AP, kscal=monmot, buffer=buffdtm)

            call mdfcho(nbmode, depl, vite, acce, fext_nl,&
                        nbnli, rangcho, deplcho, paracho, noeucho,&
                        saucho, [temps,dt], nofdep, nofvit, nofacc,&
                        ntotex, psidel, monmot(1:8), fext_tgt)

            if ((nlcase.eq.0).or.(nlacc.eq.1)) then
                do i=1, nbmode
                    fext(i) = fext(i) + fext_nl(i)
                end do
            else
                do i=1, nbmode
                    fext(i) = fext(i) + fext_tgt(i)
                end do
            end if
        else
            call dtmget(sd_dtm, _ROTR_FK ,vk8=fk, buffer=buffdtm)
            call dtmget(sd_dtm, _ROTR_DFK,vk8=dfk, buffer=buffdtm)
            call dtmget(sd_dtm, _V_ROT   ,rscal=vrotat, buffer=buffdtm)
            call dtmget(sd_dtm, _ANGL_INI,rscal=angini, buffer=buffdtm)
            call dtmget(sd_dtm, _ANGL_FON,kscal=foncp, buffer=buffdtm)

            call mdrfis(nbmode, depl, fext, nbnli, nbrfis,&
                        deplcho, fk, dfk, paracho, angini,&
                        vrotat, foncp, temps)
        endif

    end if


    conv = 1.d0
    if (nbpal .ne. 0) then

        call dtmget(sd_dtm, _DT_EDYOS,rscal=dtedyos, buffer=buffdtm)
        call dtmget(sd_dtm, _V_ROT   ,rscal=vrotat, buffer=buffdtm)

        call dtmget(sd_dtm, _PAL_TYP ,vk8=typal, buffer=buffdtm)
        call dtmget(sd_dtm, _PAL_FIN ,vk8=finpal, buffer=buffdtm)
        call dtmget(sd_dtm, _PAL_CN  ,vk8=cnpal, buffer=buffdtm)
        call dtmget(sd_dtm, _PAL_FSAV,vr=fsauv, buffer=buffdtm)

        prdeff = .true.
        call mdfedy(nbpal, nbmode, ind, dt, dtedyos,&
                    temps, vrotat, deplcho, depl, vite,&
                    fext, typal, finpal, cnpal, prdeff,&
                    conv, fsauv)

        call dtmget(sd_dtm, _PAL_NBCV, vi=nbsconv, buffer=buffdtm)
        nbconv = nbsconv(1)
        nbmxcv = nbsconv(2)
        if ((conv.le.0.d0) .and. (nbconv.gt.nbmxcv)) then
            call utmess('F', 'EDYOS_46')
        else if ((conv.le.0.d0) .and. (nbconv.le.nbmxcv)) then
            nbconv = nbconv + 1
        endif
        call dtmsav(sd_dtm, _PAL_NBCV,2,ivect=[nbconv,nbmxcv], buffer=buffdtm)

    endif

    if (nbrede .ne. 0) then
        call dtmget(sd_dtm, _FX_DEPLR,vr=dplred, buffer=buffdtm)
        call dtmget(sd_dtm, _FX_FONCT,vk8=fonred, buffer=buffdtm)
        call dtmget(sd_dtm, _FX_SREDR,vr=saured, buffer=buffdtm)
        call dtmget(sd_dtm, _FX_SREDI,vi=saredi, buffer=buffdtm)

        call mdfred(nbmode, depl, fext, nbrede, dplred,&
                    fonred, saured, saredi)
    endif

    if (nbrevi .ne. 0) then
        call dtmget(sd_dtm, _FV_DEPLR, vr=dplrev, buffer=buffdtm)
        call dtmget(sd_dtm, _FV_FONCT, vk8=fonrev, buffer=buffdtm)
        call dtmget(sd_dtm, _FV_SREVR, vr=saurev, buffer=buffdtm)
        call dtmget(sd_dtm, _FV_SREVI, vi=sarevi, buffer=buffdtm)

        call mdfrev(nbmode, vite, fext, nbrevi, dplrev,&
                    fonrev, saurev, sarevi)
    endif

    if (nlcase.ne.0) then
!       --- Implicit treatment of chocs, project the force to the new basis
!           by calculating : [Phi]_t x FORC
        call pmtvec('ZERO', nbmode, phi, fext, fext0)
        if (nlacc.eq.0) then
            call dtmget(sd_dtm, _F_NL_ADD, vr =fadd_nl, buffer=buffdtm)
            do i=1, nbmode
                fext0(i) = fext0(i) + fadd_nl(i)            
            end do
        end if
    end if

    if (instrum) then
        call uttcpu('CPU.DTMFORC', 'FIN', ' ')
        call uttcpr('CPU.DTMFORC', 7, tps)
        write(*,*) "ELPSD DTMFORC : ", tps(7)
    end if

end subroutine
