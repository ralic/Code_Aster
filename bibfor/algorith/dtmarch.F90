subroutine dtmarch(sd_dtm_, sd_int_, buffdtm, buffint)
    use iso_c_binding, only: c_loc, c_ptr, c_f_pointer
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
! dtmarch : Archive the current step.
!
#include "jeveux.h"
#include "blas/dcopy.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/dtmallo.h"
#include "asterfort/dtmbuff.h"
#include "asterfort/dtmcase_coder.h"
#include "asterfort/dtmget.h"
#include "asterfort/dtmsav.h"
#include "asterfort/intbuff.h"
#include "asterfort/intget.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jgetptc.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdarch.h"
#include "asterfort/mdtr74grd.h"
#include "asterfort/pmavec.h"
!
!   -0.1- Input/output arguments
    character(len=*), intent(in) :: sd_dtm_
    character(len=*), intent(in) :: sd_int_
    integer, pointer             :: buffdtm(:)
    integer, pointer             :: buffint(:)
!
!   -0.2- Local variables
    integer           :: ipas, nbmode, nbsauv, nbnoli, nbscho
    integer           :: nbrede, nbrevi, ndec, dimschor, nbvari
    integer           :: ind, ic, ii, iret, nlcase
    integer           :: index, iarch_sd
    real(kind=8)      :: t, dt
    character(len=4)  :: intk
    character(len=7)  :: casek7
    character(len=8)  :: sd_dtm, sd_int
    character(len=24) :: nomres
    type(c_ptr) :: pc

    integer         , pointer :: allocs(:)   => null()
    integer         , pointer :: isto(:)     => null()
    integer         , pointer :: saredi(:)   => null()
    integer         , pointer :: sarevi(:)   => null()
    integer         , pointer :: iorsto(:)   => null()
    integer         , pointer :: ichost(:)   => null()
    integer         , pointer :: iredst(:)   => null()
    integer         , pointer :: irevst(:)   => null()
    real(kind=8)    , pointer :: depgen(:)   => null()
    real(kind=8)    , pointer :: vitgen(:)   => null()
    real(kind=8)    , pointer :: accgen(:)   => null()
    real(kind=8)    , pointer :: depl0(:)    => null()
    real(kind=8)    , pointer :: vite0(:)    => null()
    real(kind=8)    , pointer :: acce0(:)    => null()   
    real(kind=8)    , pointer :: phi(:)      => null()
    real(kind=8)    , pointer :: saucho_v(:) => null()
    real(kind=8)    , pointer :: saured(:)   => null()
    real(kind=8)    , pointer :: saurev(:)   => null()
    real(kind=8)    , pointer :: temsto(:)   => null()
    real(kind=8)    , pointer :: passto(:)   => null()
    real(kind=8)    , pointer :: depsto(:)   => null()
    real(kind=8)    , pointer :: vitsto(:)   => null()
    real(kind=8)    , pointer :: accsto(:)   => null()
    real(kind=8)    , pointer :: fchost(:)   => null()
    real(kind=8)    , pointer :: dchost(:)   => null()
    real(kind=8)    , pointer :: vchost(:)   => null()
    real(kind=8)    , pointer :: vint(:)     => null()
    real(kind=8)    , pointer :: dredst(:)   => null()
    real(kind=8)    , pointer :: drevst(:)   => null()
!
#define saucho(ic,m) saucho_v((m-1)*nbnoli+ic) 

!   0 - Initializations
    sd_dtm = sd_dtm_
    sd_int = sd_int_

    call dtmget(sd_dtm, _ARCH_STO, vi=isto     , buffer=buffdtm)
    call intget(sd_int,  IND_ARCH, iscal=index , buffer=buffint)
    call dtmget(sd_dtm, _NB_MODES, iscal=nbmode, buffer=buffdtm)

    call dtmget(sd_dtm, _IARCH_SD, iscal=iarch_sd)

    if (iarch_sd.gt.0) then
        call codent(iarch_sd, 'D0', intk)
        nomres = '&&AD'//intk
        call jelira(nomres(1:8)//'           .ORDR','LONMAX', nbsauv)
        if (isto(1).ge.(nbsauv)) then
            iarch_sd = iarch_sd + 1
            call dtmsav(sd_dtm, _IARCH_SD, 1, iscal=iarch_sd)
            call dtmallo(sd_dtm)
            call codent(iarch_sd, 'D0', intk)
            nomres = '&&AD'//intk
        end if
    else
        call dtmget(sd_dtm, _CALC_SD, kscal=nomres, buffer=buffdtm)
        call dtmget(sd_dtm, _ARCH_NB, iscal=nbsauv, buffer=buffdtm)
        if (isto(1).ge.(nbsauv)) then
            write(*,*) 'trying to exceed the number of saved initially estimated'
            write(*,*) 'estimated = ', nbsauv
            write(*,*) 'isto(1) = ', isto(1)
            ASSERT(.false.)         
        end if
    end if

    call intget(sd_int, STEP , iocc=index, rscal=dt,   buffer=buffint)
    call intget(sd_int, INDEX, iocc=index, iscal=ipas, buffer=buffint)
    call intget(sd_int, TIME , iocc=index, rscal=t,    buffer=buffint)

    call dtmget(sd_dtm, _NL_CASE, iscal=nlcase, buffer=buffdtm)

    call intget(sd_int, DEPL, iocc=index, lonvec=iret, buffer=buffint)
    if (iret.ne.0) then
        if (nlcase.eq.0) then
            call intget(sd_int, DEPL ,iocc=index, vr=depgen, buffer=buffint)
            call intget(sd_int, VITE ,iocc=index, vr=vitgen, buffer=buffint)
            call intget(sd_int, ACCE ,iocc=index, vr=accgen, buffer=buffint)
        else
!           --- Implicit treatment of chocs, project the acceleration to the 
!               previous basis : [Phi] x ACCE
!               Same treatment is done for displacement and velocity
!
            call dtmcase_coder (nlcase, casek7)
            call jeveuo(sd_dtm // '.PRJ_BAS.' //casek7, 'E', vr=phi)
            call intget(sd_int, DEPL , iocc=index , vr=depl0, buffer=buffint)
            call intget(sd_int, VITE , iocc=index , vr=vite0, buffer=buffint)
            call intget(sd_int, ACCE , iocc=index , vr=acce0, buffer=buffint)
            call dtmget(sd_dtm, _IMP_DEPL, vr=depgen, buffer=buffdtm)
            call dtmget(sd_dtm, _IMP_VITE, vr=vitgen, buffer=buffdtm)
            call dtmget(sd_dtm, _IMP_ACCE, vr=accgen, buffer=buffdtm)           
            call pmavec('ZERO', nbmode, phi, depl0, depgen)
            call pmavec('ZERO', nbmode, phi, vite0, vitgen)
            call pmavec('ZERO', nbmode, phi, acce0, accgen)
        end if
    else
        ASSERT(.false.)
    end if
!
!
    call dtmget(sd_dtm, _IND_ALOC, vi=allocs,buffer=buffdtm)
!
!   [jordr, jdisc, jptem, jdepl , jvite, jacce,  
!    jfcho, jdcho, jvcho, jadcho, jredc, jredd,
!    jrevc, jrevv                                 ]

    call jgetptc(allocs(1)+isto(1), pc, vi=zi(1))
    call c_f_pointer(pc, iorsto, [1])

    call jgetptc(allocs(2)+isto(1), pc, vr=zr(1))
    call c_f_pointer(pc, temsto, [1])

    call jgetptc(allocs(3)+isto(1), pc, vr=zr(1))
    call c_f_pointer(pc, passto, [1])

    ind = nbmode * isto(1)
    call jgetptc(allocs(4)+ind, pc, vr=zr(1))
    call c_f_pointer(pc, depsto, [nbmode])
    call jgetptc(allocs(5)+ind, pc, vr=zr(1))
    call c_f_pointer(pc, vitsto, [nbmode])
    call jgetptc(allocs(6)+ind, pc, vr=zr(1))
    call c_f_pointer(pc, accsto, [nbmode])
!
!   Obligatory information (time, displacement, velocity, etc.)
    iorsto(1) = ipas
    temsto(1) = t
    passto(1) = dt
    call dcopy(nbmode, depgen, 1, depsto, 1)
    call dcopy(nbmode, vitgen, 1, vitsto, 1)
    call dcopy(nbmode, accgen, 1, accsto, 1)

!
!   Optional information (nonlinearities)
    call dtmget(sd_dtm, _NB_NONLI,iscal=nbnoli,buffer=buffdtm)
    call dtmget(sd_dtm, _FX_NUMB,iscal=nbrede,buffer=buffdtm)
    call dtmget(sd_dtm, _FV_NUMB,iscal=nbrevi,buffer=buffdtm)

    if (nbnoli .ne. 0) then

        call dtmget(sd_dtm, _NL_SAVES,vr=saucho_v,buffer=buffdtm)
!

        ndec = nbnoli*isto(1)
        call jgetptc(allocs(7)+ndec*3, pc, vr=zr(1))
        call c_f_pointer(pc, fchost, [nbnoli*3])

        call jgetptc(allocs(9)+ndec*3, pc, vr=zr(1))
        call c_f_pointer(pc, vchost, [nbnoli*3])

        call jgetptc(allocs(10)+ndec, pc, vi=zi(1))
        call c_f_pointer(pc, ichost, [nbnoli])
!
        call jeveuo(nomres(1:19)//'.DLOC', 'E', vr=dchost)
        call jeveuo(nomres(1:19)//'.VINT', 'E', vr=vint)
!
        nbscho = nbsauv*3*nbnoli
        dimschor= mdtr74grd('SCHOR')
        nbvari = mdtr74grd('MAXVINT')
        ind = nbnoli*isto(1)*nbvari
        do ic = 1, nbnoli
            isto(2)                = isto(2) + 1
            fchost((ic-1)*3+1)     = saucho(ic,1)
            dchost(isto(2))        = saucho(ic,4)
            vchost((ic-1)*3+1)      = saucho(ic,7)
            dchost(nbscho+isto(2)) = saucho(ic,10)

            isto(2)                = isto(2) + 1
            fchost((ic-1)*3+2)      = saucho(ic,2)
            dchost(isto(2))        = saucho(ic,5)
            vchost((ic-1)*3+2)      = saucho(ic,8)
            dchost(nbscho+isto(2)) = saucho(ic,11)

            isto(2)                = isto(2) + 1
            fchost((ic-1)*3+3)      = saucho(ic,3)
            dchost(isto(2))        = saucho(ic,6)
            vchost((ic-1)*3+3)      = saucho(ic,9)
            dchost(nbscho+isto(2)) = saucho(ic,12)
            ichost(ic)             = nint(saucho(ic,13))
!           variables internes
!           V(nbnoli,vint,nbsauv)
            do ii= 1, nbvari
                vint(ic+(ii-1)*nbnoli+ind) = saucho(ic,dimschor+ii)
            enddo
        enddo
    endif
!
!   cas des relations effort-déplacement
    if (nbrede .ne. 0) then
        call dtmget(sd_dtm, _FX_SREDR,vr=saured,buffer=buffdtm)
        call dtmget(sd_dtm, _FX_SREDI,vi=saredi,buffer=buffdtm)

        call jgetptc(allocs(11)+isto(3), pc, vi=zi(1))
        call c_f_pointer(pc, iredst, [nbrede])
        call jgetptc(allocs(12)+isto(3), pc, vr=zr(1))
        call c_f_pointer(pc, dredst, [nbrede])

        do ii = 1, nbrede
            isto(3) = isto(3) + 1
            iredst(ii) = saredi(ii)
            dredst(ii) = saured(ii)
        enddo
    endif
!
!   cas des relations effort-vitesse
    if (nbrevi .ne. 0) then
        call dtmget(sd_dtm, _FV_SREVR,vr=saurev,buffer=buffdtm)
        call dtmget(sd_dtm, _FV_SREVI,vi=sarevi,buffer=buffdtm)

        call jgetptc(allocs(13)+isto(4), pc, vi=zi(1))
        call c_f_pointer(pc, irevst, [nbrevi])
        call jgetptc(allocs(14)+isto(4), pc, vr=zr(1))
        call c_f_pointer(pc, drevst, [nbrevi])

        do ii = 1, nbrevi
            isto(4) = isto(4) + 1
            irevst(ii) = sarevi(ii)
            drevst(ii) = saurev(ii)
        enddo
    endif
!
    isto(1) = isto(1) + 1
!
end subroutine