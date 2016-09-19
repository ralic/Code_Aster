subroutine intadapt1(sd_dtm_, sd_int_, buffdtm, buffint)
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
! intadapt1 : Integrate from t_i to t_i+1 the differential equations of motion
!             using the ADAPT order-1 formulation.
! 
#include "jeveux.h"
#include "blas/dcopy.h"
#include "asterc/r8prem.h"
#include "asterfort/dtmacce.h"
#include "asterfort/dtmget.h"
#include "asterfort/frqapp.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/intbuff.h"
#include "asterfort/intinivec.h"
#include "asterfort/intget.h"
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
    integer           :: i, nbequ, ind1, iret1, iret2
    integer           :: npper, nrmax, nr, nbnoli, nbvint
    real(kind=8)      :: t1, dt, dt1, dt2
    real(kind=8)      :: coeff, err, epsi, pas1, norm
    real(kind=8)      :: cpmin, freq
    character(len=8)  :: sd_dtm, sd_int, sd_nl, vvar

    real(kind=8)    , pointer :: depl1(:)    => null()
    real(kind=8)    , pointer :: vite1(:)    => null()
    real(kind=8)    , pointer :: acce1(:)    => null()
    real(kind=8)    , pointer :: fext1(:)    => null()
    real(kind=8)    , pointer :: depl2(:)    => null()
    real(kind=8)    , pointer :: vite2(:)    => null()
    real(kind=8)    , pointer :: acce2(:)    => null()
    real(kind=8)    , pointer :: fext2(:)    => null()

    real(kind=8)    , pointer :: par(:)      => null()
    real(kind=8)    , pointer :: vmin(:)     => null()

    real(kind=8)    , pointer :: nlsav0(:)  => null()
    real(kind=8)    , pointer :: nlsav1(:)  => null()

    integer, pointer          :: buffnl(:)   => null()


! Algorithm parameters saved in a linear vector, easily accessible using the defines
#define crit_vmin par(1)
#define npper_r par(2)
#define nrmax_r par(3)
#define cmult par(4)
#define cdivi par(5)
#define dtmin par(6)
#define dtmax par(7)
#define deltadt par(8)
#define stabstep par(9)
#define nbnlsav par(10)
#define nbsavnl nint(par(10))
!
!   0 - Initializations
    sd_dtm = sd_dtm_
    sd_int = sd_int_
    epsi = 100.d0*r8prem()

!   1 - Retrieval of the system's state at instant t_i (index=1)
    call intget(sd_int, TIME , iocc=1, rscal=t1  , buffer=buffint)
    call intget(sd_int, INDEX, iocc=1, iscal=ind1, buffer=buffint)
    call intget(sd_int, STEP , iocc=1, rscal=dt  , buffer=buffint)
    dt1 = dt
    dt2 = dt

    call intget(sd_int, DEPL    , iocc=1, vr=depl1  , lonvec=nbequ, buffer=buffint)
    call intget(sd_int, VITE    , iocc=1, vr=vite1, buffer=buffint)
    call intget(sd_int, ACCE    , iocc=1, vr=acce1, buffer=buffint)
    call intget(sd_int, FORCE_EX, iocc=1, vr=fext1, buffer=buffint)

!   2 - Detection of the initial call to the ADAPT_ORDR1 algorithm
!       DEPL/2 does not exist in the buffer
    call intget(sd_int, DEPL, iocc=2, lonvec=iret1, buffer=buffint)
    if (iret1.eq.0) then
!
!       2.1 - Algorithm initialization
!
        call intinivec(sd_int, PARAMS, 10, vr=par)

        call getvtx('SCHEMA_TEMPS', 'VITE_MIN', iocc=1, scal=vvar)
!       VITE_MIN = 'NORM' <=> crit_vmin = 1.d0 ; VITE_MIN = 'MAXI' <=> crit_vmin = 2.d0
        crit_vmin = 1.d0
        if (vvar(1:4).eq.'MAXI') crit_vmin = 2.d0

        call getvis('SCHEMA_TEMPS', 'NB_POIN_PERIODE', iocc=1, scal=npper)
        call getvis('SCHEMA_TEMPS', 'NMAX_ITER_PAS'  , iocc=1, scal=nrmax)
        npper_r = 1.d0*npper
        nrmax_r = 1.d0*nrmax

        call getvr8('SCHEMA_TEMPS', 'COEF_MULT_PAS'  , iocc=1, scal=cmult)
        call getvr8('SCHEMA_TEMPS', 'COEF_DIVI_PAS'  , iocc=1, scal=cdivi)
    
        call getvr8('SCHEMA_TEMPS', 'PAS_MAXI', iocc=1, scal=dtmax, nbret=iret1)
        if (iret1.ne.1) dtmax = 1.d6*dt
        call getvr8('SCHEMA_TEMPS', 'PAS_MINI', iocc=1, scal=dtmin, nbret=iret2)
        if (iret2.ne.1) then
            call getvr8('SCHEMA_TEMPS', 'PAS_LIMI_RELA', iocc=1, scal=cpmin)
            dtmin = cpmin*dt
        end if

!       deltadt gives the ratio between dtmin and dtmax, it is considered as an
!       indicator for whether we should adapt or no the time step
        deltadt = abs(dtmax/dtmin - 1.d0)

        stabstep = 1.d0

!       --- Allocate and initialize work vector vmin
        call intinivec(sd_int, WORK1, nbequ, vr=vmin)
        do i = 1, nbequ
            vmin(i) = 1.d-15
        end do
        
!       --- Allocate work vectors for NL_SAVES
        call dtmget(sd_dtm, _NB_NONLI , iscal=nbnoli)
        if (nbnoli.gt.0) then
            call dtmget(sd_dtm, _SD_NONL , kscal=sd_nl)
            call nlget(sd_nl, _INTERNAL_VARS, lonvec=nbvint)
            nbnlsav = nbvint *1.d0
            call intinivec(sd_int, WORK2, nbsavnl, vr=nlsav0)
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
        call intget(sd_int, DEPL, iocc=2, vr=depl2, buffer=buffint)
        call intget(sd_int, VITE, iocc=2, vr=vite2, buffer=buffint)
        call intget(sd_int, ACCE, iocc=2, vr=acce2, buffer=buffint)

!       --- Retrieve algorithm parameters
        call intget(sd_int, PARAMS, vr=par)
!       --- Retrieve work vector vmin
        call intget(sd_int, WORK1, vr=vmin, buffer=buffint) 
!       --- Retrieve choc parameters save container
        if (nbsavnl.gt.0) call intget(sd_int, WORK2, vr=nlsav0, buffer=buffint)
    end if
    if (nbsavnl.gt.0) then
        call dtmget(sd_dtm, _SD_NONL  , kscal=sd_nl, buffer=buffdtm)
        call dtmget(sd_dtm, _NL_BUFFER, vi=buffnl, buffer=buffdtm)
        call nlget (sd_nl , _INTERNAL_VARS, vr=nlsav1, buffer=buffnl)
        call dcopy(nbsavnl, nlsav1, 1, nlsav0, 1)
    end if
    call intsav(sd_int, INDEX, 1, iocc=2, iscal=ind1+1, buffer=buffint)

    nr = 1
!
10  continue
!
    if (nbsavnl.gt.0) call dcopy(nbsavnl, nlsav0, 1, nlsav1, 1)

!   3 - Calculate the system's state at index <2>

    pas1 = 0.5d0*(dt+dt1)

    if (dt1.le.1.d-13) pas1= dt1

    do i = 1, nbequ
        vite2(i) = vite1(i) + acce1(i) * pas1
        depl2(i) = depl1(i) + vite2(i) * dt1
    enddo

    call intsav(sd_int, TIME , 1, iocc=2, rscal=t1+dt1, buffer=buffint)
    call intsav(sd_int, STEP , 1, iocc=2, rscal=dt1   , buffer=buffint)

    call dtmacce(sd_dtm, sd_int, 2, buffdtm, buffint)


!   4 - If an adaptative scheme is requested (dtmax != dtmin)
    if (deltadt.ge.epsi) then

!       4.1 - Estimation of the error
        call frqapp(dt2, nbequ, depl1, depl2, acce1,&
                    acce2, vmin, freq)

        err = max(npper_r*freq*dt2, epsi)

!       4.2 - Set the error to zero for the 5 first steps (faster initialization)
        if (ind1.lt.5) err = 0.d0

        coeff = 1.d0
        if (err .ge. 1.d0) then
            coeff = 1.d0/cdivi
        else if (stabstep.gt.4.5d0) then
            if (err .lt. 0.75d0) then 
                coeff = cmult
                stabstep = 1.d0
            end if               
        else 
            stabstep = stabstep + 1.d0
        end if

!       4.3 - Determine the time step for the next iteration or integration step  
        dt2 = min(dtmax, dt1*coeff)

        if ((err.ge.1.d0) .and. (dt2.ge.dtmin) .and. (nr.lt.nint(nrmax_r))) then
            dt1 = dt2
            nr = nr + 1
            goto 10
        else if (dt2.lt.dtmin) then
            call utmess('F', 'ALGORITH5_23')
        else if (nr.eq.nint(nrmax_r)) then
            call utmess('A', 'DYNAMIQUE_18', si=nr, nr=2, valr=[t1, dt1])
        end if

!       4.4 - Caculate Vmin for the next step
        if (abs(crit_vmin - 1.d0) .le. epsi) then
!           --- NORM
            norm = 0.d0
            do i = 1, nbequ
                norm = norm + vite2(i)**2
            enddo
            norm = sqrt(norm)*0.01d0
            do i = 1, nbequ
                vmin(i) = norm
            enddo
        else
!           --- MAXI
            do i = 1, nbequ
                vmin(i) = max(abs(vite2(i)*0.01d0),vmin(i))
            enddo
        endif

    endif

    call intget(sd_int, FORCE_EX, iocc=2, vr=fext2, buffer=buffint)

!   5 - Preparing the algorithm for the next step, copy index 2 in 1
    call dcopy(nbequ, depl2, 1, depl1, 1)
    call dcopy(nbequ, vite2, 1, vite1, 1)
    call dcopy(nbequ, acce2, 1, acce1, 1)
    call dcopy(nbequ, fext2, 1, fext1, 1)
    call intsav(sd_int, STEP , 1, iocc=1, rscal=dt2, buffer=buffint)
    call intsav(sd_int, TIME , 1, iocc=1, rscal=t1+dt1, buffer=buffint)
    
    call intsav(sd_int, INDEX, 1, iocc=1, iscal=ind1+1, buffer=buffint)
    call intsav(sd_int, INDEX, 1, iocc=2, iscal=ind1+1, buffer=buffint)

!   6 - Set the archiving index to 2
    call intsav(sd_int, IND_ARCH, 1, iscal=2, buffer=buffint)

end subroutine
