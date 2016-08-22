subroutine intruku32(sd_dtm_, sd_int_, buffdtm, buffint)
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
! intruku32 : Integrate from t_i to t_i+1 the differential equations of motion
!             using a Runge-Kutta type order 3/2 scheme.
! 
#include "jeveux.h"
#include "blas/dcopy.h"
#include "asterc/r8prem.h"
#include "asterc/r8maem.h"
#include "asterfort/dtmacce.h"
#include "asterfort/dtmget.h"
#include "asterfort/dtmsav.h"
#include "asterfort/getvr8.h"
#include "asterfort/intbuff.h"
#include "asterfort/intget.h"
#include "asterfort/intinivec.h"
#include "asterfort/intsav.h"
#include "asterfort/mdtr74grd.h"
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
    integer           :: i, nbequ, ind1, iret1, iret2
    integer           :: ee, ss, j_kde, j_kvi, nbvint
    integer           :: nbnoli, iret, iret3
    real(kind=8)      :: t1, dt, t2, dt2, coeff
    real(kind=8)      :: errd, errde, errv, errvi, errt
    real(kind=8)      :: seuil1, seuil2, skd, skv, epsi
    real(kind=8)      :: ddep, dvit, errt0
    character(len=8)  :: sd_dtm, sd_int, sd_nl

    real(kind=8)    , pointer :: depl1(:)    => null()
    real(kind=8)    , pointer :: vite1(:)    => null()
    real(kind=8)    , pointer :: acce1(:)    => null()
    real(kind=8)    , pointer :: fext1(:)    => null()
    real(kind=8)    , pointer :: depl2(:)    => null()
    real(kind=8)    , pointer :: vite2(:)    => null()
    real(kind=8)    , pointer :: acce2(:)    => null()
    real(kind=8)    , pointer :: fext2(:)    => null()

    real(kind=8)    , pointer :: nlsav0(:)    => null()
    real(kind=8)    , pointer :: nlsav1(:)    => null()

    real(kind=8)    , pointer :: par(:)    => null()
    integer, pointer          :: buffnl(:)   => null()


! Algorithm parameters saved in a linear vector, easily accessible using the defines
#define cdp(i) par(i)
#define adp(i,j) par(4+(i-1)*3+j)
#define edp(i) par(4+9+i)
#define tol par(4+9+4+1)
#define alpha par(4+9+4+2)
#define dtmin par(4+9+4+3)
#define dtmax par(4+9+4+4)
#define deltadt par(4+9+4+5)
#define nbnlsav par(4+9+4+6)
#define nbsavnl nint(par(4+9+4+6))
! Work vectors accessible using indices (ee,i) where :
! <ee> : RK level (1-6)
! <i>  : equation index (1-nbequ)
#define kde(ee,i) zr(j_kde+(ee-1)*nbequ+i-1)
#define kvi(ee,i) zr(j_kvi+(ee-1)*nbequ+i-1)

!
!   0 - Initializations
    sd_dtm = sd_dtm_
    sd_int = sd_int_
    epsi = 100.d0*r8prem()
    errt0 = r8maem()

!   1 - Retrieval of the system's state at instant t_i (index=1)
    call intget(sd_int, TIME , iocc=1, rscal=t1  , buffer=buffint)
    call intget(sd_int, INDEX, iocc=1, iscal=ind1, buffer=buffint)
    call intget(sd_int, STEP , iocc=1, rscal=dt  , buffer=buffint)
    dt2 = dt

    call intget(sd_int, DEPL    , iocc=1, vr=depl1  , lonvec=nbequ, buffer=buffint)
    call intget(sd_int, VITE    , iocc=1, vr=vite1, buffer=buffint)
    call intget(sd_int, ACCE    , iocc=1, vr=acce1, buffer=buffint)
    call intget(sd_int, FORCE_EX, iocc=1, vr=fext1, buffer=buffint)

!   2 - Detection of the initial call to the Runge-Kutta 5/4 algorithm
!       DEPL/2 does not exist in the buffer
    call intget(sd_int, DEPL, iocc=2, lonvec=iret, buffer=buffint)
    if (iret.eq.0) then
!
!       2.1 - Algorithm initialization
!
        call intinivec(sd_int, PARAMS, 23, vr=par)

        call getvr8('SCHEMA_TEMPS', 'TOLERANCE', iocc=1, scal=tol)
        call getvr8('SCHEMA_TEMPS', 'PAS_MINI' , iocc=1, scal=dtmin, nbret=iret1)
        call getvr8('SCHEMA_TEMPS', 'PAS_MAXI' , iocc=1, scal=dtmax, nbret=iret2)
        call getvr8('SCHEMA_TEMPS', 'ALPHA'    , iocc=1, scal=alpha, nbret=iret3)

        if (iret1.ne.1) dtmin = 1.d-10* dt
        if (iret2.ne.1) dtmax = 1.d10 * dt
        if (iret3.ne.1) alpha = 0.d0

!       deltadt gives the ratio between dtmin and dtmax, it is considered as an
!       indicator for whether we should adapt or no the time step
        deltadt = abs(dtmax/dtmin - 1.d0)

!       Bogacki-Shampine coefficients
!       Note, these are actually saved in PARAMS, a #define is given in the beginning
!       of this subroutine to relate cdp and adp to the PARAMS vector (pointed to by par)
        cdp(1)=0.00d0
        cdp(2)=0.50d0
        cdp(3)=0.75d0
        cdp(4)=1.d0
!
        adp(1,1)=0.50d0
        adp(2,1)=0.00d0
        adp(2,2)=0.75d0
!
        adp(3,1)=2.d0/9.d0
        adp(3,2)=1.d0/3.d0
        adp(3,3)=4.d0/9.d0
!
        edp(1)=  5.0d0/72.0d0
        edp(2)= -1.d0/12.0d0
        edp(3)= -1.d0/9.d0
        edp(4)=  1.d0/8.d0

!       --- Allocate work vectors kde and kdv
        call intinivec(sd_int, WORK1, 3*nbequ, address=j_kde)
        call intinivec(sd_int, WORK2, 3*nbequ, address=j_kvi)

!       --- Allocate work vectors for NL_SAVES
        call dtmget(sd_dtm, _NB_NONLI , iscal=nbnoli)
        if (nbnoli.gt.0) then
            call dtmget(sd_dtm, _SD_NONL , kscal=sd_nl)
            call nlget(sd_nl, _INTERNAL_VARS, lonvec=nbvint)
            nbnlsav = nbvint *1.d0
            call intinivec(sd_int, WORK3, nbsavnl, vr=nlsav0)
        else
            nbnlsav = 0.d0
        endif

!       --- Allocate vectors DEPL/VITE/ACCE/2 (t_i+1)
        call intinivec(sd_int, DEPL    , nbequ, iocc=2, vr=depl2)
        call intinivec(sd_int, VITE    , nbequ, iocc=2, vr=vite2)
        call intinivec(sd_int, ACCE    , nbequ, iocc=2, vr=acce2)
        call intinivec(sd_int, FORCE_EX, nbequ, iocc=2, vr=fext2)

        nullify(buffint)
        call intbuff(sd_int, buffint,level=2)
    else
!       --- Algorithm is already initialized, just retrieve DEPL/VITE/ACCE/2
!           Note that these are made identical to index 1 (dcopy before exit)
        call intget(sd_int, DEPL    , iocc=2, vr=depl2, buffer=buffint)
        call intget(sd_int, VITE    , iocc=2, vr=vite2, buffer=buffint)
        call intget(sd_int, ACCE    , iocc=2, vr=acce2, buffer=buffint)
        call intget(sd_int, FORCE_EX, iocc=2, vr=fext2, buffer=buffint)

!       --- Retrieve work vectors kde and kdv
        call intget(sd_int, WORK1, address=j_kde  , buffer=buffint)
        call intget(sd_int, WORK2, address=j_kvi  , buffer=buffint)

!       --- Retrieve the algorithm parameters
        call intget(sd_int, PARAMS, vr=par)

!       --- Retrieve nl parameters save container
        if (nbsavnl.gt.0) call intget(sd_int, WORK3, vr=nlsav0, buffer=buffint)

    end if
    if (nbsavnl.gt.0) then
        call dtmget(sd_dtm, _SD_NONL  , kscal=sd_nl, buffer=buffdtm)
        call dtmget(sd_dtm, _NL_BUFFER, vi=buffnl, buffer=buffdtm)
        call nlget (sd_nl , _INTERNAL_VARS, vr=nlsav1, buffer=buffnl)
        call dcopy(nbsavnl, nlsav1, 1, nlsav0, 1)
    end if
    call intsav(sd_int, INDEX, 1, iocc=2, iscal=ind1+1, buffer=buffint)

10  continue
    call dcopy(nbequ, depl1, 1, depl2, 1)
    call dcopy(nbequ, vite1, 1, vite2, 1)
    call dcopy(nbequ, acce1, 1, acce2, 1)
    if (nbsavnl.gt.0) then
        call dcopy(nbsavnl, nlsav0, 1, nlsav1, 1)
    end if

!   3 - Loop over all levels (1-3)
    do ee = 1, 3

        t2  = t1 + dt*cdp(ee+1)
        dt2 = dt * (cdp(ee+1)-cdp(ee))

        call intsav(sd_int, TIME , 1, iocc=2, rscal=t2, buffer=buffint)
        call intsav(sd_int, STEP , 1, iocc=2, rscal=dt2, buffer=buffint)

        call dcopy(nbequ, vite2, 1, kde(ee,1), 1)
        call dcopy(nbequ, acce2, 1, kvi(ee,1), 1)

        do i = 1, nbequ
            depl2(i) = depl1(i)
            vite2(i) = vite1(i)
            do ss = 1, ee
                depl2(i) = depl2(i) + dt*adp(ee,ss)*kde(ss,i)
                vite2(i) = vite2(i) + dt*adp(ee,ss)*kvi(ss,i)
            enddo
        enddo
!
        t2  = t1 + dt*cdp(ee+1)
        call intsav(sd_int, TIME , 1, iocc=2, rscal=t2, buffer=buffint)
        call dtmacce(sd_dtm, sd_int, 2, buffdtm, buffint)
!
    enddo
    dt2 = dt

!   4 - If an adaptative scheme is requested (dtmax != dtmin)
    if (deltadt.ge.epsi) then

    !   4.1 - Estimation of the global errors in displacement and velocity
        errd = 0.d0
        errv = 0.d0
        do i = 1, nbequ
            errde = 0.d0
            errvi = 0.d0
            do ee = 1, 3
                errde = errde + edp(ee)*kde(ee,i)
                errvi = errvi + edp(ee)*kvi(ee,i)
            enddo

    !       --- The last estimation is not saved in kde and kvi, but is present in VITE/ACCE/2
            errde = errde + edp(4)*vite2(i)
            errvi = errvi + edp(4)*acce2(i)

            ddep = abs(depl2(i)-depl1(i))
            dvit = abs(vite2(i)-vite1(i))
    !       --- Use the relative tolerance for the local error
            skd  = max(tol*abs(depl1(i)+alpha),tol*(ddep+alpha),epsi)
            errd = errd + (errde/skd)**2

            skv  = max(tol*abs(vite1(i)+alpha),tol*(dvit+alpha),epsi)
            errv = errv + (errvi/skv)**2
        enddo

    !   4.2 - Calculation of a total error, and timestep adaptation
        errt = max( dt*sqrt((errd+errv)/(2*nbequ)), epsi )

        seuil1 = (0.9d0/5.0d0)**(4.d0)
        seuil2 = (0.9d0/0.2d0)**(4.d0)

!       --- Refining the time step is not helping, the system is probably badly 
!           initialized (not in dynamic equilibrium) and the integration can proceed
!           as is toward an equilibrium state
        if ((errt*1.01d0).gt.errt0) then
            errt = 0.99d0
        end if

        if (errt .lt. seuil1) then
            coeff = 5.0d0
        else if (errt .gt. seuil2) then
            coeff = 0.2d0
        else
            coeff = 0.9d0*(errt)**(-1.d0/4.d0)
        endif
        dt2 = min(dtmax, dt*coeff)
!       
        if ((errt.ge.1.d0) .and. (dt2.ge.dtmin)) then
            dt = dt2
            errt0 = errt
            goto 10
        else if (dt2.lt.dtmin) then
            call utmess('F', 'ALGORITH5_23')
        end if
    endif


!   5 - Preparing the algorithm for the next step, copy index 2 in 1
    call dcopy(nbequ, depl2, 1, depl1, 1)
    call dcopy(nbequ, vite2, 1, vite1, 1)
    call dcopy(nbequ, acce2, 1, acce1, 1)
    call dcopy(nbequ, fext2, 1, fext1, 1)
    call intsav(sd_int, STEP , 1, iocc=1, rscal=dt2, buffer=buffint)
    call intsav(sd_int, TIME , 1, iocc=1, rscal=t1+dt, buffer=buffint)
    call intsav(sd_int, INDEX, 1, iocc=1, iscal=ind1+1, buffer=buffint)

    call intsav(sd_int, STEP , 1, iocc=2, rscal=dt, buffer=buffint)
    call intsav(sd_int, TIME , 1, iocc=2, rscal=t1+dt, buffer=buffint)
    call intsav(sd_int, INDEX, 1, iocc=2, iscal=ind1+1, buffer=buffint)

!   6 - Set the archiving index to 2
    call intsav(sd_int, IND_ARCH, 1, iscal=2, buffer=buffint)

end subroutine