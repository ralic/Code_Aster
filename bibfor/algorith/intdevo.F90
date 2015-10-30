subroutine intdevo(sd_dtm_, sd_int_, buffdtm, buffint)
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
! intdevo : Integrate from t_i to t_i+1 the differential equations of motion
!           using the Devogelaero-Fu integration method.
#include "jeveux.h"
#include "blas/dcopy.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/dtmforc.h"
#include "asterfort/dtmget.h"
#include "asterfort/getvr8.h"
#include "asterfort/intbuff.h"
#include "asterfort/intdevo_oper.h"
#include "asterfort/intget.h"
#include "asterfort/intinivec.h"
#include "asterfort/intsav.h"
#include "asterfort/mdtr74grd.h"
#include "asterfort/pmavec.h"
#include "asterfort/rrlds.h"
#include "asterfort/trlds.h"
#include "asterfort/utimsd.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
!
!   -0.1- Input/output arguments
    character(len=*) , intent(in) :: sd_dtm_
    character(len=*) , intent(in) :: sd_int_
    integer, pointer              :: buffdtm(:)
    integer, pointer              :: buffint(:)
!
!   -0.2- Local variables
    integer           :: i, j, nbequ, ind1, iret
    integer           :: jw6, ind, iret1, iret2, nr
    integer           :: nbnoli, upmat, iret3
    real(kind=8)      :: t1, dt, dt1, dt2, dt3
    real(kind=8)      :: dt4, dt5, dt6, mdiag_r
    real(kind=8)      :: kdiag_r, cdiag_r, dtnew, pas0, pas1
    real(kind=8)      :: epsi, errt, c0, c1, alpha1
    real(kind=8)      :: alpha2, beta, depmag, tol_dim, coeff
    real(kind=8)      :: seuil1, seuil2, dtold, errdep, coeff2
    character(len=8)  :: sd_dtm, sd_int

    real(kind=8)    , pointer :: depl1(:)    => null()
    real(kind=8)    , pointer :: vite1(:)    => null()
    real(kind=8)    , pointer :: acce1(:)    => null()
    real(kind=8)    , pointer :: fext1(:)    => null()
    real(kind=8)    , pointer :: depl2(:)    => null()
    real(kind=8)    , pointer :: vite2(:)    => null()
    real(kind=8)    , pointer :: acce2(:)    => null()
    real(kind=8)    , pointer :: fext2(:)    => null()
    real(kind=8)    , pointer :: depl3(:)    => null()
    real(kind=8)    , pointer :: vite3(:)    => null()
    real(kind=8)    , pointer :: acce3(:)    => null()
    real(kind=8)    , pointer :: fext3(:)    => null()
    real(kind=8)    , pointer :: depl4(:)    => null()
    real(kind=8)    , pointer :: vite4(:)    => null()
    real(kind=8)    , pointer :: acce4(:)    => null()
    real(kind=8)    , pointer :: fext4(:)    => null()

    real(kind=8)    , pointer :: par(:)      => null()
    real(kind=8)    , pointer :: mgen(:)     => null()
    real(kind=8)    , pointer :: kgen(:)     => null()   
    real(kind=8)    , pointer :: agen(:)     => null()
    real(kind=8)    , pointer :: mgenf(:)    => null()
    
    real(kind=8)    , pointer :: chosav0(:)  => null()
    real(kind=8)    , pointer :: chosav1(:)  => null()

    real(kind=8)    , pointer :: invm_c(:)   => null()
    real(kind=8)    , pointer :: invm_k(:)   => null()
    real(kind=8)    , pointer :: op_h0(:)    => null()
    real(kind=8)    , pointer :: op_h1(:)    => null()
    real(kind=8)    , pointer :: op_h2(:)    => null()
    real(kind=8)    , pointer :: x1(:)       => null()

#define mdiag (nint(par(1)).eq.1)
#define kdiag (nint(par(2)).eq.1)
#define cdiag (nint(par(3)).eq.1)
#define tol par(4)
#define alpha par(5)
#define dtmin par(6)
#define dtmax par(7)
#define deltadt par(8)
#define nbnlsav par(9)
#define nbsavnl nint(par(9))

#define im_c(row,col) invm_c((row-1)*nbequ+col)
#define im_k(row,col) invm_k((row-1)*nbequ+col)
#define h0(row,col) op_h0((row-1)*nbequ+col)
#define h1(row,col) op_h1((row-1)*nbequ+col)
#define h2(row,col) op_h2((row-1)*nbequ+col)
#define k(row,col) kgen((row-1)*nbequ+col)
#define c(row,col) agen((row-1)*nbequ+col) 

#define oldpas(m) zr(jw6+m-1)
#define depl2e(m) zr(jw6+2+m-1)
#define depl1p(m) zr(jw6+2+nbequ+m-1)
#define vite1p(m) zr(jw6+2+2*nbequ+m-1)
#define fext1p(m) zr(jw6+2+3*nbequ+m-1)
#define depl3e(m) zr(jw6+2+4*nbequ+m-1)
!
!   0 - Initializations
    sd_dtm = sd_dtm_
    sd_int = sd_int_
    epsi   = r8prem()
!
!   The indices have been chosen as follows
!
!         n-1/2      n      n+1/2     n+1
!!   x------|--------|--------|--------|--------> time
!         <2>      <1>      <3>      <4>
!          |        :        :        :
!          |<-dt/2->:<-dt/2->:<-dt/2->|
!                   :                 :
!                   |<------dt------->|
!

!   1 - Retrieval of the system's state at instant t_i (index=1)
    call intget(sd_int, TIME , iocc=1, rscal=t1  , buffer=buffint)
    call intget(sd_int, INDEX, iocc=1, iscal=ind1, buffer=buffint)
    call intget(sd_int, STEP , iocc=1, rscal=dt  , buffer=buffint)

    call intget(sd_int, DEPL    , iocc=1, vr=depl1, lonvec=nbequ, buffer=buffint)
    call intget(sd_int, VITE    , iocc=1, vr=vite1, buffer=buffint)
    call intget(sd_int, ACCE    , iocc=1, vr=acce1, buffer=buffint)
    call intget(sd_int, FORCE_EX, iocc=1, vr=fext1, buffer=buffint)

    dtold = dt
    dt1   = dt / 2.d0

!   3 - Detection of the initial call to the Devogelaero-Fu algorithm
!       DEPL/2 does not exist in the buffer
    call intget(sd_int, DEPL, iocc=2, lonvec=iret, buffer=buffint)
    if (iret.eq.0) then
!
!       3.1 - Algorithm initialization, DEPL/1 and VITE/1 are known
!
!       --- Allocate vectors DEPL/VITE/ACCE/2 (t_i-1/2)
        call intinivec(sd_int, DEPL    , nbequ, iocc=2, vr=depl2)
        call intinivec(sd_int, VITE    , nbequ, iocc=2, vr=vite2)
        call intinivec(sd_int, ACCE    , nbequ, iocc=2, vr=acce2)
        call intinivec(sd_int, FORCE_EX, nbequ, iocc=2, vr=fext2)
        call intsav(sd_int, STEP, 1, iocc=2, rscal=dt1, buffer=buffint)

!       --- Copy the external force 1 (t_i) in 2 (t_i-1/2)
        call dcopy(nbequ, fext1, 1, fext2, 1)
!
!       --- Detection of the matrix types (full/diagonal) for M, K and C
        mdiag_r = 0.d0
        call intget(sd_int, MASS_FUL, iocc=1, lonvec=iret1, buffer=buffint)
        if (iret1.gt.0) then
            call intget(sd_int, MASS_FUL, iocc=1, vr=mgen,buffer=buffint)
        else
            call intget(sd_int, MASS_DIA, iocc=1, vr=mgen,buffer=buffint)
            mdiag_r = 1.d0
        end if
!
        kdiag_r = 0.d0
        call intget(sd_int, RIGI_FUL, iocc=1, lonvec=iret1, buffer=buffint)
        if (iret1.gt.0) then
            call intget(sd_int, RIGI_FUL, iocc=1, vr=kgen,buffer=buffint)
        else
            call intget(sd_int, RIGI_DIA, iocc=1, vr=kgen,buffer=buffint)
            kdiag_r = 1.d0
        end if
!
        cdiag_r = 0.d0
        call intget(sd_int, AMOR_FUL, iocc=1, lonvec=iret1, buffer=buffint)
        if (iret1.gt.0) then
            call intget(sd_int, AMOR_FUL, iocc=1, vr=agen,buffer=buffint)
        else
            call intget(sd_int, AMOR_DIA, iocc=1, vr=agen,buffer=buffint)
            cdiag_r = 1.d0
        end if

        call intinivec(sd_int, PARAMS, 9, vr=par)
        par(1) = mdiag_r
        par(2) = kdiag_r
        par(3) = cdiag_r

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

!
!       invm_c = (M^-1) x C
!       invm_k = (M^-1) x K
!       H0     = 4I - dt x (M^-1) x C
!       H1     = 4I + dt x (M^-1) x C
!       H2     = 6I + dt x (M^-1) x C

!       --- Memory allocation for operators invm_c, h0, h1, and h2
        if (cdiag) then
!           --- Operator invm_c is diagonal
            call intinivec(sd_int, WORK1, nbequ, vr=invm_c)
!           --- h0 is only needed for initialization
            AS_ALLOCATE(vr=op_h0, size=nbequ)
!           --- Operators h1, h2 are diagonal           
            call intinivec(sd_int, WORK3, nbequ, vr=op_h1)
            call intinivec(sd_int, WORK4, nbequ, vr=op_h2)            
        else
!           --- Operator invm_c is full           
            call intinivec(sd_int, WORK1, nbequ*nbequ, vr=invm_c)
!           --- h0 is only needed for initialization
            AS_ALLOCATE(vr=op_h0, size=nbequ*nbequ)
!           --- Operators h1, h2 are full           
            call intinivec(sd_int, WORK3, nbequ*nbequ, vr=op_h1)
            call intinivec(sd_int, WORK4, nbequ*nbequ, vr=op_h2)            
        endif

!       --- Memory allocation for the operator invm_k
        if (mdiag.and.kdiag) then
!           --- Operator invm_k is diagonal = omega2 diagonal terms
            call intinivec(sd_int, WORK2, nbequ, vr=invm_k)
        else
!           --- Operator invm_k is full
            call intinivec(sd_int, WORK2, nbequ*nbequ, vr=invm_k)         
        endif

!       --- Memory allocation for work vector x1
        call intinivec(sd_int, WORK5, nbequ, vr=x1)
        call intinivec(sd_int, WORK6, 3+5*nbequ, address=jw6)

!       --- Allocate work vectors for NL_SAVES
        call dtmget(sd_dtm, _NB_NONLI , iscal=nbnoli)
        if (nbnoli.gt.0) then
            nbnlsav = (nbnoli*(mdtr74grd('SCHOR')+mdtr74grd('MAXVINT')))*1.d0
            call intinivec(sd_int, WORK7, nbsavnl, vr=chosav0)
        else
            nbnlsav = 0.d0
        endif

        oldpas(1) = dt
        oldpas(2) = dt
        call dcopy(nbequ, depl1, 1, depl1p(1), 1)
        call dcopy(nbequ, vite1, 1, vite1p(1), 1)
        call dcopy(nbequ, fext1, 1, fext1p(1), 1)

        if (.not.(mdiag)) then
            AS_ALLOCATE(vr=mgenf, size=nbequ*nbequ)
            call dcopy(nbequ*nbequ, mgen, 1, mgenf, 1)
            call trlds(mgenf, nbequ, nbequ, iret2)
            call intdevo_oper(nbequ, par, mgenf, kgen, agen, &
                              dt, invm_c, op_h1, op_h2, invm_k)
            AS_DEALLOCATE(vr=mgenf)          
        else
            call intdevo_oper(nbequ, par, mgen, kgen, agen, &
                              dt, invm_c, op_h1, op_h2, invm_k)
        end if
!       --------------------------------------------------------------------------------
!       --- Calculation of the operator h0
        if (cdiag) then
            do i = 1, nbequ
                op_h0(i) = 4.d0 - dt*invm_c(i)
            end do            
        else
            if (mdiag) then
                do i = 1, nbequ
                    h0(i,i)  = 4.d0 - dt*im_c(i,i)
                    do j = i+1, nbequ
                        h0(i,j)  = -dt*im_c(i,j)
                        h0(j,i)  = -dt*im_c(j,i)
                    end do
                end do            
            else
                do i = 1, nbequ
                    h0(i,i)  = 4.d0 - dt*im_c(i,i)
                    do j = i+1, nbequ
                        h0(i,j)  = -dt*im_c(i,j)
                        h0(j,i)  = -dt*im_c(j,i)
                    end do
                end do 
            end if
            call trlds(op_h0, nbequ, nbequ, iret2)
        end if
!       --------------------------------------------------------------------------------
!       --- Calculation of the pre-initial state (t_(i-1/2))
!       --------------------------------------------------------------------------------
        if (mdiag) then
            if (kdiag) then
                do i = 1, nbequ
                    fext1(i) = (fext1(i)/mgen(i)) - invm_k(i)*depl1(i)
                end do
            else
                call pmavec('ZERO', nbequ, invm_k, depl1, x1)
                do i = 1, nbequ
                    fext1(i) = (fext1(i)/mgen(i)) - x1(i)
                end do               
            endif
        else
            call rrlds(mgen, nbequ, nbequ, fext1, 1)
            call pmavec('ZERO', nbequ, invm_k, depl1, x1)
            do i = 1, nbequ
                fext1(i) = fext1(i) - x1(i)
            end do            
        end if
!
        dt6 = dt1/4.d0
!
        if (cdiag) then
            do i = 1, nbequ
                depl2(i) = depl1(i) - dt1*vite1(i) + dt6*(fext1(i)-invm_c(i)*vite1(i))
            end do
        else          
            call pmavec('ZERO', nbequ, invm_c, vite1, depl2)
            do i = 1, nbequ
                depl2(i) = depl1(i) - dt1*vite1(i) + dt6*(fext1(i)-depl2(i))
            end do
        endif
!
        if (mdiag) then
            if (kdiag) then
                do i = 1, nbequ
                    fext2(i) = (fext2(i)/mgen(i)) - invm_k(i)*depl2(i)
                end do
            else
                call pmavec('ZERO', nbequ, invm_k, depl2, x1)
                do i = 1, nbequ
                    fext2(i) = (fext2(i)/mgen(i)) - x1(i)
                end do               
            endif
        else
            call rrlds(mgen, nbequ, nbequ, fext2, 1)
            call pmavec('ZERO', nbequ, invm_k, depl2, x1)
            do i = 1, nbequ
                fext2(i) = fext2(i) - x1(i)
            end do            
        end if
!
        if (cdiag) then
            do i = 1, nbequ
                vite2(i) = (op_h1(i)*vite1(i) - dt * (fext1(i)+fext2(i)))/op_h0(i)
            end do
        else          
            call pmavec('ZERO', nbequ, op_h1, vite1, vite2)
            do i = 1, nbequ
                vite2(i) = vite2(i) - dt * (fext1(i)+fext2(i))
            end do
            call rrlds(op_h0, nbequ, nbequ, vite2, 1)
        endif

        AS_DEALLOCATE(vr=op_h0)

!       --- Variable time-step, initialize depl2e by extrapolating the initial state
        if ((deltadt.ge.epsi)) then
            do i = 1,nbequ
                depl2e(i) = (2.d0*depl2(i)-depl1(i)) + (dt/2.d0)*(2.d0*vite2(i)-vite1(i)) + &
                            (((dt/2.d0)**2.d0)/24.d0)       * &
                            (13.d0*fext1(i))
            end do
        end if


!       --- Allocate vectors DEPL/VITE/ACCE/3 (t_i+1/2)
        call intinivec(sd_int, DEPL    , nbequ, iocc=3, vr=depl3)
        call intinivec(sd_int, VITE    , nbequ, iocc=3, vr=vite3)
        call intinivec(sd_int, ACCE    , nbequ, iocc=3, vr=acce3)
        call intinivec(sd_int, FORCE_EX, nbequ, iocc=3, vr=fext3)

!       --- Allocate vectors DEPL/VITE/ACCE/4 (t_i+1)
        call intinivec(sd_int, DEPL    , nbequ, iocc=4, vr=depl4)
        call intinivec(sd_int, VITE    , nbequ, iocc=4, vr=vite4)
        call intinivec(sd_int, ACCE    , nbequ, iocc=4, vr=acce4)
        call intinivec(sd_int, FORCE_EX, nbequ, iocc=4, vr=fext4)

        nullify(buffint)
        call intbuff(sd_int, buffint, level=4)

        ! write(*,*) "*******************************************************"
        ! write(*,*) "INITIALIZATION OF DEVOGELAERO ALGO"
        ! write(*,*) "*******************************************************"
        ! call utimsd(6, 2, .false._1, .true._1, sd_int, 1, 'V')

    else
!       --- Algorithm is already initialized, just retrieve DEPL/VITE/ACCE/FEXT 2/3/4
        call intget(sd_int, DEPL    , iocc=2, vr=depl2 , buffer=buffint)
        call intget(sd_int, VITE    , iocc=2, vr=vite2 , buffer=buffint)
        call intget(sd_int, ACCE    , iocc=2, vr=acce2 , buffer=buffint)
        call intget(sd_int, FORCE_EX, iocc=2, vr=fext2 , buffer=buffint)
        call intget(sd_int, DEPL    , iocc=3, vr=depl3 , buffer=buffint)
        call intget(sd_int, VITE    , iocc=3, vr=vite3 , buffer=buffint)
        call intget(sd_int, ACCE    , iocc=3, vr=acce3 , buffer=buffint)
        call intget(sd_int, FORCE_EX, iocc=3, vr=fext3 , buffer=buffint)
        call intget(sd_int, DEPL    , iocc=4, vr=depl4 , buffer=buffint)
        call intget(sd_int, VITE    , iocc=4, vr=vite4 , buffer=buffint)
        call intget(sd_int, ACCE    , iocc=4, vr=acce4 , buffer=buffint)
        call intget(sd_int, FORCE_EX, iocc=4, vr=fext4 , buffer=buffint)
!       --- Parameters and work vectors containing the necessary operators
        call intget(sd_int, PARAMS  ,  vr=par     , buffer=buffint)
        call intget(sd_int, WORK1   ,  vr=invm_c  , buffer=buffint)
        call intget(sd_int, WORK2   ,  vr=invm_k  , buffer=buffint)
        call intget(sd_int, WORK3   ,  vr=op_h1   , buffer=buffint)
        call intget(sd_int, WORK4   ,  vr=op_h2   , buffer=buffint)
        call intget(sd_int, WORK5   ,  vr=x1      , buffer=buffint)
!       --- Work vector for a variable-step DEVOGE algorithm
        call intget(sd_int, WORK6   ,  address=jw6, buffer=buffint)
!       --- Retrieve choc parameters save container
        if (nbsavnl.gt.0) call intget(sd_int, WORK7, vr=chosav0, buffer=buffint)

        if (mdiag) then
            call intget(sd_int, MASS_DIA, vr=mgen, buffer=buffint)
        else
            call intget(sd_int, MASS_FUL, vr=mgen, buffer=buffint)
        endif
        call intget(sd_int, STEP, iocc=4, rscal=dtold , buffer=buffint)
    end if
    if (nbsavnl.gt.0) then
        call dtmget(sd_dtm, _NL_SAVES, vr=chosav1, buffer=buffdtm)
        call dcopy(nbsavnl, chosav1, 1, chosav0, 1)
    end if

    call intget(sd_int, MAT_UPDT, iscal=upmat, buffer=buffint)
    if (upmat.eq.1) then
        mdiag_r = 0.d0
        call intget(sd_int, MASS_FUL, iocc=1, lonvec=iret1, buffer=buffint)
        if (iret1.gt.0) then
            call intget(sd_int, MASS_FUL, iocc=1, vr=mgen,buffer=buffint)
        else
            call intget(sd_int, MASS_DIA, iocc=1, vr=mgen,buffer=buffint)
            mdiag_r = 1.d0
        end if
!
        kdiag_r = 0.d0
        call intget(sd_int, RIGI_FUL, iocc=1, lonvec=iret1, buffer=buffint)
        if (iret1.gt.0) then
            call intget(sd_int, RIGI_FUL, iocc=1, vr=kgen,buffer=buffint)
        else
            call intget(sd_int, RIGI_DIA, iocc=1, vr=kgen,buffer=buffint)
            kdiag_r = 1.d0
        end if
!
        cdiag_r = 0.d0
        call intget(sd_int, AMOR_FUL, iocc=1, lonvec=iret1, buffer=buffint)
        if (iret1.gt.0) then
            call intget(sd_int, AMOR_FUL, iocc=1, vr=agen,buffer=buffint)
        else
            call intget(sd_int, AMOR_DIA, iocc=1, vr=agen,buffer=buffint)
            cdiag_r = 1.d0
        end if

        par(1) = mdiag_r
        par(2) = kdiag_r
        par(3) = cdiag_r

        nullify(op_h1)
        nullify(op_h2)
        nullify(invm_c)
        nullify(invm_k)       
        if (cdiag) then
            call intinivec(sd_int, WORK1, nbequ, vr=invm_c)
            call intinivec(sd_int, WORK3, nbequ, vr=op_h1)
            call intinivec(sd_int, WORK4, nbequ, vr=op_h2)  
        else
            call intinivec(sd_int, WORK1, nbequ*nbequ, vr=invm_c)
            call intinivec(sd_int, WORK3, nbequ*nbequ, vr=op_h1)
            call intinivec(sd_int, WORK4, nbequ*nbequ, vr=op_h2)  
        endif

        if (mdiag.and.kdiag) then
            call intinivec(sd_int, WORK2, nbequ, vr=invm_k)
        else
            call intinivec(sd_int, WORK2, nbequ*nbequ, vr=invm_k)         
        endif

        if (.not.(mdiag)) then
            AS_ALLOCATE(vr=mgenf, size=nbequ*nbequ)
            call dcopy(nbequ*nbequ, mgen, 1, mgenf, 1)
            call trlds(mgenf, nbequ, nbequ, iret2)
            call intdevo_oper(nbequ, par, mgenf, kgen, agen, &
                              dt, invm_c, op_h1, op_h2, invm_k)
            AS_DEALLOCATE(vr=mgenf)          
        else
            call intdevo_oper(nbequ, par, mgen, kgen, agen, &
                              dt, invm_c, op_h1, op_h2, invm_k)
        end if

        nullify(buffint)
        call intbuff(sd_int, buffint, level=4)

        dtold = dt
        call intsav(sd_int, MAT_UPDT, 1, iscal=0, buffer=buffint)

        ! write(*,*) "*******************************************************"
        ! write(*,*) "CHANGE IN STATE WITH MATRIX UPDATE FOR DEVOGELAERO ALGO"
        ! write(*,*) "*******************************************************"
        ! call utimsd(6, 2, .false._1, .true._1, sd_int, 1, 'V')
    end if

    nr = 0
    dtnew = dt

10  continue
!
    if (nbsavnl.gt.0) call dcopy(nbsavnl, chosav0, 1, chosav1, 1)

!   2 - Definition of algorithm parameters
    dt1 = dt / 2.d0
    dt2 = dt / 4.d0
    dt3 = dt / 6.d0
    dt4 = dt * dt / 24.d0
    dt5 = dt * dt / 6.d0
    dt6 = dt * dt / 8.d0
    coeff = dt/dtold

!   3- Updating the operators H1 and H2 in the event of a change in dt
    if (abs(coeff-1.d0).ge.epsi) then
!       --- Update H1, H2
!           H1 = 4I + dt x (M^-1) x C
!           H2 = 6I + dt x (M^-1) x C
        if (cdiag) then
!           --- H1, H2 are diagonal
            do i = 1, nbequ
                op_h1(i) = (op_h1(i) - 4.d0)*coeff + 4.d0
                op_h2(i) = (op_h2(i) - 6.d0)*coeff + 6.d0
            end do            
        else
!           --- H1, H2 are full
            do i = 1, nbequ
                h1(i,i)  = (h1(i,i) - 4.d0)*coeff + 4.d0
                h2(i,i)  = (h2(i,i) - 6.d0)*coeff + 6.d0
                do j = i+1, nbequ
                    h1(i,j)  =  h1(i,j)*coeff
                    h2(i,j)  =  h2(i,j)*coeff
                    h1(j,i)  =  h1(j,i)*coeff
                    h2(j,i)  =  h2(j,i)*coeff
                end do
            end do
        end if
!       --- Variable time-step : interpolate DEPL, VITE, FORCE_EX /2 (t_i-1/2)
        if ((nr.gt.0)) then
            do i = 1,nbequ
                fext2(i) = fext1(i)+coeff*(fext2(i)-fext1(i))
                depl2(i) = depl1(i)+coeff*(depl2(i)-depl1(i))
                vite2(i) = vite1(i)+coeff*(vite2(i)-vite1(i))
            end do
        end if
    end if


!   4 - Calculation of DEPL/3 (t_i+1/2)
    do i = 1, nbequ
        x1(i) = 4.d0 * vite1(i) - vite2(i)
    end do
    if (cdiag) then
        do i = 1, nbequ
            depl3(i) = invm_c(i) * x1(i)
        end do
    else 
        call pmavec('ZERO', nbequ, invm_c, x1, depl3)
    end if
    do i = 1, nbequ
        depl3(i) = depl1(i) + dt1*vite1(i) + dt4 * (4.d0 * fext1(i) - fext2(i) - depl3(i))
        vite3(i) = (depl3(i) - depl1(i)) / dt1
    end do

    call intsav(sd_int, INDEX, 1, iocc=3, iscal=ind1+1, buffer=buffint)
    call intsav(sd_int, TIME , 1, iocc=3, rscal=t1+dt1, buffer=buffint)
    call intsav(sd_int, STEP , 1, iocc=3, rscal=dt1, buffer=buffint)

    call dtmforc(sd_dtm, sd_int, 3, buffdtm, buffint)
    if (mdiag) then
        if (kdiag) then
            do i = 1, nbequ
                fext3(i) = (fext3(i)/mgen(i)) - invm_k(i)*depl3(i)
            end do
        else
            call pmavec('ZERO', nbequ, invm_k, depl3, x1)
            do i = 1, nbequ
                fext3(i) = (fext3(i)/mgen(i)) - x1(i)
            end do               
        endif
    else
        call rrlds(mgen, nbequ, nbequ, fext3, 1)
        call pmavec('ZERO', nbequ, invm_k, depl3, x1)
        do i = 1, nbequ
            fext3(i) = fext3(i) - x1(i)
        end do
    end if

!   5 - Calculation of VITE/3(t_i+1/2) and DEPL/4 (t_i+1)
    if (cdiag) then
        do i = 1, nbequ
            vite3(i) = invm_c(i) * vite1(i)
        end do
    else
        call pmavec('ZERO', nbequ, invm_c, vite1, vite3)
    endif
    do i = 1, nbequ
        vite3(i) = 4.d0 * (vite1(i) + dt2*(fext1(i) + fext3(i) - vite3(i)) )
    end do

    if (cdiag) then
        do i = 1, nbequ
            vite3(i) = vite3(i)/op_h1(i)
        end do
    else
        call rrlds(op_h1, nbequ, nbequ, vite3, 1)
    endif

    do i = 1, nbequ
        x1(i) = vite1(i) + 2.d0 * vite3(i)
    end do
    if (cdiag) then
        do i = 1, nbequ
            depl4(i) = invm_c(i) * x1(i)
        end do
    else 
        call pmavec('ZERO', nbequ, invm_c, x1, depl4)
    end if

    do i = 1, nbequ
        depl4(i) = depl1(i) + dt*vite1(i) + dt5 * ( fext1(i) + 2.d0*fext3(i) - depl4(i))
        vite4(i) = (depl4(i) - depl3(i)) / dt1
    end do
!
!
    call intsav(sd_int, INDEX, 1, iocc=4, iscal=ind1+1, buffer=buffint)
    call intsav(sd_int, TIME , 1, iocc=4, rscal=t1+dt, buffer=buffint)
    call intsav(sd_int, STEP , 1, iocc=4, rscal=dt1, buffer=buffint)

    call dtmforc(sd_dtm, sd_int, 4, buffdtm, buffint)

    if (mdiag) then
        if (kdiag) then
            do i = 1, nbequ
                fext4(i) = (fext4(i)/mgen(i)) - invm_k(i)*depl4(i)
            end do
        else
            call pmavec('ZERO', nbequ, invm_k, depl4, x1)
            do i = 1, nbequ
                fext4(i) = (fext4(i)/mgen(i)) - x1(i)
            end do               
        endif
    else
        call rrlds(mgen, nbequ, nbequ, fext4, 1)
        call pmavec('ZERO', nbequ, invm_k, depl4, x1)
        do i = 1, nbequ
            fext4(i) = fext4(i) - x1(i)
        end do
    end if

!   6 - Calculation of VITE4 and ACCE4 (t_i+1)
    do i = 1, nbequ
        x1(i) = 4.d0 * vite3(i) + vite1(i)
    end do
    if (cdiag) then
        do i = 1, nbequ
            vite4(i) = invm_c(i) * x1(i)
        end do
    else 
        call pmavec('ZERO', nbequ, invm_c, x1, vite4)
    end if

    do i = 1, nbequ
        vite4(i) =  6.d0 *(vite1(i) + dt3* (fext4(i)+4.d0*fext3(i)+fext1(i)-vite4(i)) )
    end do

    if (cdiag) then
        do i = 1, nbequ
            vite4(i) = vite4(i)/op_h2(i)
        end do
    else
        call rrlds(op_h2, nbequ, nbequ, vite4, 1)
    endif

    if (cdiag) then
        do i = 1, nbequ
            acce4(i) = invm_c(i) * vite4(i)
        end do
    else 
        call pmavec('ZERO', nbequ, invm_c, vite4, acce4)
    end if

    do i = 1, nbequ
        acce4(i) = fext4(i) - acce4(i)
    enddo

!   7 - Calculate the error and refine the timestep if needed
    if ((deltadt.ge.epsi)) then
!       --- 7.1 - First determine which case correspond to the current
!                 integration step, attribute an index from 0-3
        ind = 0
        if  ((abs(dt-oldpas(2)       ).le.epsi) .and. & 
             (abs(oldpas(2)-oldpas(1)).le.epsi)) then 
            ind = 0
            ! Constant time-step case
        else if ((abs(dt-oldpas(2)       ).gt.epsi) .and. &
                 (abs(oldpas(2)-oldpas(1)).le.epsi)) then 
            ind = 1
            ! Just after the first change in time-step
        else if ((abs(dt-oldpas(2)       ).le.epsi) .and. &
                 (abs(oldpas(1)-oldpas(1)).gt.epsi)) then 
            ind = 2
            ! Second integration after a change in time-step
        else if ((abs(dt-oldpas(2)       ).gt.epsi) .and. &
                 (abs(oldpas(2)-oldpas(1)).gt.epsi)) then
            ! 2 consecutive changes in time-step
            ind = 3
        end if
!       --- 7.2 - Calculate an estimate on DEPL/3 (t_i+1/2) based on
!                 either the system's state at t_i+1 or t_i
        if (ind.eq.0) then 
            do i = 1, nbequ
                depl3e(i) = depl4(i) - dt1*vite4(i) + &
                            ((dt1**2.d0)/24.d0)     * &
                            (7.d0*fext4(i) + 6.d0*fext3(i) - fext1(i))
            end do
        else 
            do i = 1, nbequ
                depl3e(i) = depl1(i) + dt1*vite1(i) + &
                            ((dt1**2.d0)/24.d0)     * &
                            (7.d0*fext1(i) + 6.d0*fext3(i) - fext4(i))
            end do
        end if

!       --- 7.3 - In case more than one iteration was detected in the
!                 current step, calculate a new estimation of the 
!                 system state at t_i-1/2 (DEPL/2)
        pas0 = oldpas(1)/2.d0
        pas1 = oldpas(2)/2.d0
        if (nr .eq. 1) then
            do i = 1,nbequ
                depl2e(i) = depl1p(i) + pas1*vite1p(i) + &
                            ((pas1**2.d0)/24.d0)       * &
                            (7.d0*fext1p(i) + 6.d0*fext2(i) - fext1(i))
            end do
        end if
!       --- 7.4 - Calculate the error on displacement, per unit half-step (dt1 = dt/2)
        errt = 0.d0
        if (ind.eq.0) then
            beta = (45.d0 * dt1) / 4.d0
            do i = 1, nbequ
                errt = errt + (( (depl3e(i)-depl3(i)) - &
                                 (depl2e(i)-depl2(i))  ) / beta)**2.d0
            end do
        else if (ind.eq.1) then
            c1 = dt1/pas1
            alpha1 = (c1**3.d0) * (2.d0 + c1) / 3.d0
            beta  = 5.d0 * pas1 * (12.d0 + 7.d0*c1 - (c1**2.d0)) / (8.d0 * c1)
            do i = 1, nbequ
                errt = errt + ((       (depl3e(i)-depl3(i)) - &
                                alpha1*(depl2e(i)-depl2(i))   ) / beta)**2.d0
            end do
        else if  (ind.eq.2) then
            c0 = pas1/pas0
            alpha2 = (1.d0 + (2.d0/c0)) / 3.d0
            beta = 5.d0 * pas0 * (1.d0 + 5.d0*c0 + 3.d0 * (c0**2.d0)) / (4.d0 * c0)
            do i = 1, nbequ
                errt = errt + ((alpha2*(depl3e(i)-depl3(i)) - &
                                       (depl2e(i)-depl2(i))    ) / beta)**2.d0
            end do
        else
            c0 = pas1/pas0
            c1 = dt1/pas1
            alpha1 = (c1**3.d0) * (2.d0 + c1) / 3.d0
            alpha2 = (1.d0 + (2.d0/c0)) / 3.d0
            beta = 5.d0 * dt1 * ( (c0**2) * (12.d0 + 7.d0*c1 - (c1**2)  ) + &
                                   c0 * (20.d0 + 12.d0*c1 - 2.d0*(c1**2)) + &
                                   2.d0*c1 + 4.d0 ) / &
                                  (24.d0 * (c0**2.d0)*(c1**2.d0))
            do i = 1, nbequ
                errt = errt + (( alpha2*(depl3e(i)-depl3(i)) - &
                                 alpha1*(depl2e(i)-depl2(i))    ) / beta)**2.d0
            end do
        end if
!       --- The average global error is given as (in displacement units) :
        errdep  = sqrt(errt)*dt1/nbequ
        tol_dim = 0.d0
        depmag  = 0.d0
        do i = 1, nbequ
            depmag = depmag + max(abs(depl3(i)),abs(vite3(i)*dt))**2.d0
        end do
        depmag = sqrt(depmag)/nbequ
        tol_dim = tol * (depmag+alpha)
        if (tol_dim.le.epsi) then
            errt = (0.9d0)**4.d0
        else 
            errt = errdep/tol_dim
        end if
!       --- Time-step adaptation, as a function of the estimated commited relative error
        seuil1 = (0.9d0/1.5d0)**(4.d0)
        seuil2 = (0.9d0/0.5d0)**(4.d0)
        if (errt .lt. seuil1) then
            coeff2 = 1.5d0
        else if (errt .gt. seuil2) then
            coeff2 = 0.5d0
        else
            coeff2 = 0.9d0*(errt)**(-1.d0/4.d0)
        endif
        nr = nr + 1
        dtnew = max(dtmin, coeff2*dt)
        dtnew = min(dtnew, dtmax)
        coeff2 = dtnew/dt
        if (errt.lt.1.d0) then
            if (abs(coeff2-1.d0).lt.(0.15d0)) then
                coeff2 = 1.d0
                dtnew = dt
            end if
        end if
        if ((errt.gt.1.d0).and.(nr.lt.25)) then
            if (coeff2.eq.1.d0) then
                ASSERT(.false.)
            end if
            dtold = dt
            dt = dtnew
            goto 10
        else if (nr.eq.100) then
            ASSERT(.false.)
        end if

        call dcopy(nbequ, depl1, 1, depl1p(1), 1)
        call dcopy(nbequ, vite1, 1, vite1p(1), 1)
        call dcopy(nbequ, fext1, 1, fext1p(1), 1)
        call dcopy(nbequ, depl3e(1), 1, depl2e(1), 1)

        oldpas(1) = oldpas(2)
        oldpas(2) = dt

    end if

!   8 - Preparing the algorithm for the next step, copy index 3 in 2 and 4 in 1
    call dcopy(nbequ, depl3, 1, depl2, 1)
    call dcopy(nbequ, vite3, 1, vite2, 1)
    call dcopy(nbequ, acce3, 1, acce2, 1)
    call dcopy(nbequ, fext3, 1, fext2, 1)
    call dcopy(nbequ, depl4, 1, depl1, 1)
    call dcopy(nbequ, vite4, 1, vite1, 1)
    call dcopy(nbequ, acce4, 1, acce1, 1)
    call dcopy(nbequ, fext4, 1, fext1, 1)
    call intsav(sd_int, STEP , 1, iocc=1, rscal=dtnew, buffer=buffint)
    call intsav(sd_int, TIME , 1, iocc=1, rscal=t1+dt, buffer=buffint)
    call intsav(sd_int, INDEX, 1, iocc=1, iscal=ind1+1, buffer=buffint)

    call intsav(sd_int, TIME , 1, iocc=4, rscal=t1+dt, buffer=buffint)
    call intsav(sd_int, STEP , 1, iocc=4, rscal=dt, buffer=buffint)
    call intsav(sd_int, INDEX, 1, iocc=4, iscal=ind1+1, buffer=buffint)

!   8 - Set the archiving index to 4
    call intsav(sd_int, IND_ARCH, 1, iscal=4, buffer=buffint)

end subroutine