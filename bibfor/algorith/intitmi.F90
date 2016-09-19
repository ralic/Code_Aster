subroutine intitmi(sd_dtm_, sd_int_, buffdtm, buffint)
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
! intnewm : Integrate from t_i to t_i+1 the differential equations of motion
!           using an integral method.
! 
#include "jeveux.h"
#include "blas/dcopy.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/dtmacce.h"
#include "asterfort/dtmforc.h"
#include "asterfort/dtmget.h"
#include "asterfort/intbuff.h"
#include "asterfort/intget.h"
#include "asterfort/intinivec.h"
#include "asterfort/intsav.h"
#include "asterfort/nlget.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
!   -0.1- Input/output arguments
    character(len=*) , intent(in) :: sd_dtm_
    character(len=*) , intent(in) :: sd_int_
    integer, pointer              :: buffdtm(:)
    integer, pointer              :: buffint(:)
!
!   -0.2- Local variables
    aster_logical     :: mdiag, kdiag
    integer           :: i, nbequ, ind1, iret, nbvint
    integer           :: nbnoli, upmat
    real(kind=8)      :: t1, dt, coeff, epsi, dtold
    real(kind=8)      :: ksi0
    complex(kind=8)   :: s0, sr0, z0, za1, za2
    complex(kind=8)   :: zin
    character(len=8)  :: sd_dtm, sd_int, sd_nl
!
    integer         , pointer :: buffnl(:)   => null()

    real(kind=8)    , pointer :: depl1(:)    => null()
    real(kind=8)    , pointer :: vite1(:)    => null()
    real(kind=8)    , pointer :: acce1(:)    => null()
    real(kind=8)    , pointer :: fext1(:)    => null()
    real(kind=8)    , pointer :: depl2(:)    => null()
    real(kind=8)    , pointer :: vite2(:)    => null()
    real(kind=8)    , pointer :: acce2(:)    => null()
    real(kind=8)    , pointer :: fext2(:)    => null()

    real(kind=8)    , pointer :: mgen(:)    => null()
    real(kind=8)    , pointer :: kgen(:)    => null()
    real(kind=8)    , pointer :: agen(:)    => null()

    real(kind=8)    , pointer :: par(:)      => null()
    real(kind=8)    , pointer :: nlsav0(:)  => null()
    real(kind=8)    , pointer :: nlsav1(:)  => null()

    real(kind=8)    , pointer :: omegas(:)  => null()
    real(kind=8)    , pointer :: s0_r(:)    => null()
    real(kind=8)    , pointer :: za1_ri(:)  => null()
    real(kind=8)    , pointer :: za2_ri(:)  => null()
    real(kind=8)    , pointer :: trans_vd(:)=> null()
!
#define nbnlsav par(1)
#define nbsavnl nint(nbnlsav)

#define omega(k) omegas(k)
#define omega_mod(k) omegas(nbequ+k)

#define za1_r(k) za1_ri(k)
#define za1_i(k) za1_ri(nbequ+k)
#define za2_r(k) za2_ri(k)
#define za2_i(k) za2_ri(nbequ+k)

#define za1(k) dcmplx(za1_r(k),za1_i(k))
#define za2(k) dcmplx(za2_r(k),za2_i(k))
#define za3(k) dcmplx(1.d0-za2_r(k),-za2_i(k))

#define trans_v(j,k) trans_vd((j-1)*nbequ+k)
#define trans_d(j,k) trans_vd(2*nbequ+(j-1)*nbequ+k)
!
!   0 - Initializations
    sd_dtm = sd_dtm_
    sd_int = sd_int_
    epsi   = r8prem()
    upmat  = 0
!
!   1 - Retrieval of the system's state at instant t_i (index=1)
    call intget(sd_int, TIME    , iocc=1, rscal=t1  , buffer=buffint)
    call intget(sd_int, INDEX   , iocc=1, iscal=ind1, buffer=buffint)
    call intget(sd_int, STEP    , iocc=1, rscal=dt  , buffer=buffint)

    call intget(sd_int, DEPL    , iocc=1, vr=depl1, lonvec=nbequ, buffer=buffint)
    call intget(sd_int, VITE    , iocc=1, vr=vite1, buffer=buffint)
    call intget(sd_int, ACCE    , iocc=1, vr=acce1, buffer=buffint)
    call intget(sd_int, FORCE_EX, iocc=1, vr=fext1, buffer=buffint)

!   2 - Detection of the initial call to the ITMI algorithm
!       DEPL/2 does not exist in the buffer
    call intget(sd_int, DEPL, iocc=2, lonvec=iret, buffer=buffint)
    if (iret.eq.0) then
!
!       2.1 - Algorithm initialization : 
!       --- Parameters
        call intinivec(sd_int, PARAMS, 1, vr=par)
!
!       --- [>--- omega ---<,>--- omega_mod ---<]
        call intinivec(sd_int, WORK1, 2*nbequ, vr=omegas)
        call intinivec(sd_int, WORK2, nbequ, vr=s0_r)

!       --- za* [>---real part ---<,>---imag part ---<]
        call intinivec(sd_int, WORK3, 2*nbequ, vr=za1_ri) 
        call intinivec(sd_int, WORK4, 2*nbequ, vr=za2_ri) 

!       --- trans [>--- velocity ---<,>--- displacement ---<]
        call intinivec(sd_int, WORK5, 4*nbequ, vr=trans_vd) 

!       --- Allocate work vectors for NL_SAVES
        call dtmget(sd_dtm, _NB_NONLI, iscal=nbnoli)
        if (nbnoli.gt.0) then
            call dtmget(sd_dtm, _SD_NONL , kscal=sd_nl)
            call nlget(sd_nl, _INTERNAL_VARS, lonvec=nbvint)
            nbnlsav = nbvint *1.d0
            call intinivec(sd_int, WORK6, nbsavnl, vr=nlsav0)
        else
            nbnlsav = 0.d0
        endif


!       - Check that the mass and stiffness matrices are both diagonal
        mdiag = .false.
        call intget(sd_int, MASS_FUL, iocc=1, lonvec=iret, buffer=buffint)
        if (iret.gt.0) then
            call intget(sd_int, MASS_FUL, iocc=1, vr=mgen,buffer=buffint)
        else
            call intget(sd_int, MASS_DIA, iocc=1, vr=mgen,buffer=buffint)
            mdiag = .true.
        end if
!
        kdiag = .false.
        call intget(sd_int, RIGI_FUL, iocc=1, lonvec=iret, buffer=buffint)
        if (iret.gt.0) then
            call intget(sd_int, RIGI_FUL, iocc=1, vr=kgen,buffer=buffint)
        else
            call intget(sd_int, RIGI_DIA, iocc=1, vr=kgen,buffer=buffint)
            kdiag = .true.
        end if
        
        if (.not.(mdiag.and.kdiag)) then
            call utmess('F', 'DYNAMIQUE_83')
        end if
!
        call intget(sd_int, AMOR_FUL, iocc=1, lonvec=iret, buffer=buffint)
        if (iret.gt.0) then
            call utmess('A', 'DYNAMIQUE_29')
        end if
        call intget(sd_int, AMOR_DIA, iocc=1, vr=agen,buffer=buffint)

        do i = 1, nbequ
            omega(i) = sqrt(kgen(i)/mgen(i))
            ksi0 = agen(i)/(2.d0*omega(i))

            if (abs(ksi0) .gt. 1.d0) then
                omega_mod(i) = 1.d-20
            else
                omega_mod(i) = omega(i) * sqrt(1.0d0-ksi0*ksi0)
            end if
            s0_r(i)  = -ksi0*omega(i)
            s0       = dcmplx(s0_r(i),omega_mod(i))
            sr0      = s0*dt
            z0       = exp(sr0)
            za1      = (z0-1.0d0)/(s0*mgen(i))
            za2      = (1.0d0/sr0) - (1.0d0/(z0-1.0d0))

            za1_r(i) = dreal(za1)
            za1_i(i) = dimag(za1)
            za2_r(i) = dreal(za2)
            za2_i(i) = dimag(za2)

            trans_v(1,i) = dimag(s0*z0)/omega_mod(i)
            trans_v(2,i) = dimag(omega(i)*omega(i)*dconjg(z0))/omega_mod(i)
            trans_d(1,i) = dimag(z0)/omega_mod(i)
            trans_d(2,i) = dimag(s0*dconjg(z0))/omega_mod(i)
        end do

!       --- Allocate DEPL/VITE/ACCE/2 (t_i+1)
        call intinivec(sd_int, DEPL    , nbequ, iocc=2, vr=depl2)
        call intinivec(sd_int, VITE    , nbequ, iocc=2, vr=vite2)
        call intinivec(sd_int, ACCE    , nbequ, iocc=2, vr=acce2)
        call intinivec(sd_int, FORCE_EX, nbequ, iocc=2, vr=fext2)
!
        dtold = dt
!
        nullify(buffint)
        call intbuff(sd_int, buffint, level=2)

    else
!       --- Algorithm is already initialized, retrieval of all parameters and operators
        call intget(sd_int, PARAMS, vr=par)
        call intget(sd_int, WORK1, vr=omegas  , buffer=buffint)
        call intget(sd_int, WORK2, vr=s0_r    , buffer=buffint)
        call intget(sd_int, WORK3, vr=za1_ri  , buffer=buffint) 
        call intget(sd_int, WORK4, vr=za2_ri  , buffer=buffint) 
        call intget(sd_int, WORK5, vr=trans_vd, buffer=buffint) 
        if (nbsavnl.gt.0) call intget(sd_int, WORK6, vr=nlsav0, buffer=buffint)

!       --- Retrieval of already allocated DEPL/VITE/ACCE/2 (t_i+1)
        call intget(sd_int, DEPL    , iocc=2, vr=depl2, buffer=buffint)
        call intget(sd_int, VITE    , iocc=2, vr=vite2, buffer=buffint)
        call intget(sd_int, ACCE    , iocc=2, vr=acce2, buffer=buffint)
        call intget(sd_int, FORCE_EX, iocc=2, vr=fext2, buffer=buffint)
!
        call intget(sd_int, STEP    , iocc=2, rscal=dtold , buffer=buffint)
    endif

    if (nbsavnl.gt.0) then
        call dtmget(sd_dtm, _SD_NONL  , kscal=sd_nl, buffer=buffdtm)
        call dtmget(sd_dtm, _NL_BUFFER, vi=buffnl, buffer=buffdtm)
        call nlget (sd_nl , _INTERNAL_VARS, vr=nlsav1, buffer=buffnl)
        call dcopy(nbsavnl, nlsav1, 1, nlsav0, 1)
    end if

    coeff = dt/dtold

    call intget(sd_int, MAT_UPDT, iscal=upmat, buffer=buffint)
    if (upmat.eq.1) then
        call intget(sd_int, MASS_DIA, iocc=1, vr=mgen,buffer=buffint)
        call intget(sd_int, RIGI_DIA, iocc=1, vr=kgen,buffer=buffint)

        call intget(sd_int, AMOR_FUL, iocc=1, lonvec=iret, buffer=buffint)
        if (iret.gt.0) then
            call utmess('A', 'DYNAMIQUE_29')
        end if
        call intget(sd_int, AMOR_DIA, iocc=1, vr=agen,buffer=buffint)

        do i = 1, nbequ
            omega(i) = sqrt(kgen(i)/mgen(i))
            ksi0 = agen(i)/(2.d0*omega(i))

            if (abs(ksi0) .gt. 1.d0) then
                omega_mod(i) = 1.d-20
            else
                omega_mod(i) = omega(i) * sqrt(1.0d0-ksi0*ksi0)
            end if
            s0_r(i)  = -ksi0*omega(i)
            s0       = dcmplx(s0_r(i),omega_mod(i))
            sr0      = s0*dt
            z0       = exp(sr0)
            za1      = (z0-1.0d0)/(s0*mgen(i))
            za2      = (1.0d0/sr0) - (1.0d0/(z0-1.0d0))

            za1_r(i) = dreal(za1)
            za1_i(i) = dimag(za1)
            za2_r(i) = dreal(za2)
            za2_i(i) = dimag(za2)

            trans_v(1,i) = dimag(s0*z0)/omega_mod(i)
            trans_v(2,i) = dimag(omega(i)*omega(i)*dconjg(z0))/omega_mod(i)
            trans_d(1,i) = dimag(z0)/omega_mod(i)
            trans_d(2,i) = dimag(s0*dconjg(z0))/omega_mod(i)
        end do
    end if

!   3 - Updating the operators in the event of a change in dt
    if ((upmat.eq.1) .or. (abs(coeff-1.d0).ge.epsi)) then
        do i = 1, nbequ
            s0       = dcmplx(s0_r(i),omega_mod(i))
            sr0      = s0*dt
            z0       = exp(sr0)
            za2      = (1.0d0/sr0) - (1.0d0/(z0-1.0d0))

            za2_r(i) = dreal(za2)
            za2_i(i) = dimag(za2)

            trans_v(1,i) = dimag(s0*z0)/omega_mod(i)
            trans_v(2,i) = dimag(omega(i)*omega(i)*dconjg(z0))/omega_mod(i)
            trans_d(1,i) = dimag(z0)/omega_mod(i)
            trans_d(2,i) = dimag(s0*dconjg(z0))/omega_mod(i)
        end do
    end if


!   4 - Estimating DEPL/VITE/2 (t_i+1) using a simple Euler's integration scheme
!       and then calculating the force at (t_i+1) with this estimation
    do i = 1, nbequ
        vite2(i) = vite1(i) + ( dt * acce1(i) )
        depl2(i) = depl1(i) + ( dt * vite2(i) )        
    enddo
    call intsav(sd_int, TIME , 1, iocc=2, rscal=t1+dt, buffer=buffint)
    call intsav(sd_int, STEP , 1, iocc=2, rscal=dt, buffer=buffint)
    call intsav(sd_int, INDEX, 1, iocc=2, iscal=ind1+1, buffer=buffint)
    
    call dtmforc(sd_dtm, sd_int, 2, buffdtm, buffint)
    if (nbsavnl.gt.0) call dcopy(nbsavnl, nlsav0, 1, nlsav1, 1)

    if (upmat.eq.1) then
        ! do i = 1, nbequ
        !     fext1(i) = 0.5*(fext1(i) + fext2(i))
        ! enddo
        call intsav(sd_int, MAT_UPDT, 1, iscal=0, buffer=buffint)
        call dcopy(nbequ, fext2, 1, fext1, 1)
    end if

    
    do i = 1, nbequ
        zin = za1(i) * ( za2(i)*fext2(i) + za3(i)*fext1(i) )
        vite2(i) = trans_v(1,i)*vite1(i) + trans_v(2,i)*depl1(i) + &
                   s0_r(i)*dimag(zin)/omega_mod(i) + dreal(zin)
        depl2(i) = trans_d(1,i)*vite1(i) + trans_d(2,i)*depl1(i) + &
                   dimag(zin)/omega_mod(i)
    end do


!   5 - Correcting the force and determining the ACCE/2 (t_i+1)
    call dtmacce(sd_dtm, sd_int, 2, buffdtm, buffint)

!   6 - Preparing the algorithm for the next step, copy index 2 in 1
    call dcopy(nbequ, depl2, 1, depl1, 1)
    call dcopy(nbequ, vite2, 1, vite1, 1)
    call dcopy(nbequ, acce2, 1, acce1, 1)
    call dcopy(nbequ, fext2, 1, fext1, 1)
    call intsav(sd_int, STEP , 1, iocc=1, rscal=dt, buffer=buffint)
    call intsav(sd_int, TIME , 1, iocc=1, rscal=t1+dt, buffer=buffint)
    call intsav(sd_int, INDEX, 1, iocc=1, iscal=ind1+1, buffer=buffint)

!   7 - Set the archiving index to 2
    call intsav(sd_int, IND_ARCH, 1, iscal=2, buffer=buffint)

end subroutine
