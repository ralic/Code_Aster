subroutine inteuler2(sd_dtm_, sd_int_, buffdtm, buffint)
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
! inteuler : Integrate from t_i to t_i+1 the differential equations of motion
!            using EULER integration method.
! 
#include "jeveux.h"
#include "blas/dcopy.h"
#include "asterfort/dtmacce.h"
#include "asterfort/intbuff.h"
#include "asterfort/intget.h"
#include "asterfort/intinivec.h"
#include "asterfort/intsav.h"
!
!   -0.1- Input/output arguments
    character(len=*) , intent(in) :: sd_dtm_
    character(len=*) , intent(in) :: sd_int_
    integer, pointer              :: buffdtm(:)
    integer, pointer              :: buffint(:)
!
!   -0.2- Local variables
    integer           :: i, nbequ, ind1, iret
    real(kind=8)      :: t1, dt
    character(len=8)  :: sd_dtm, sd_int

    real(kind=8) , pointer :: depl1(:)    => null()
    real(kind=8) , pointer :: vite1(:)    => null()
    real(kind=8) , pointer :: acce1(:)    => null()
    real(kind=8) , pointer :: fext1(:)    => null()

    real(kind=8) , pointer :: depl2(:)    => null()
    real(kind=8) , pointer :: vite2(:)    => null()
    real(kind=8) , pointer :: acce2(:)    => null()
    real(kind=8) , pointer :: fext2(:)    => null()

!
!   0 - Initializations
    sd_dtm = sd_dtm_
    sd_int = sd_int_

!   1 - Retrieval of the system's state at instant t_i (index=1)
    call intget(sd_int, TIME , iocc=1, rscal=t1  , buffer=buffint)
    call intget(sd_int, INDEX, iocc=1, iscal=ind1, buffer=buffint)
    call intget(sd_int, STEP , iocc=1, rscal=dt  , buffer=buffint)

    call intget(sd_int, DEPL    , iocc=1, vr=depl1, lonvec=nbequ, buffer=buffint)
    call intget(sd_int, VITE    , iocc=1, vr=vite1, buffer=buffint)
    call intget(sd_int, ACCE    , iocc=1, vr=acce1, buffer=buffint)
    call intget(sd_int, FORCE_EX, iocc=1, vr=fext1, buffer=buffint)

!   2 - Detection of the initial call to the Newmark algorithm
!       DEPL/2 does not exist in the buffer
    call intget(sd_int, DEPL, iocc=2, lonvec=iret, buffer=buffint)
    if (iret.eq.0) then
        call intinivec(sd_int, DEPL    , nbequ, iocc=2, vr=depl2)
        call intinivec(sd_int, VITE    , nbequ, iocc=2, vr=vite2)
        call intinivec(sd_int, ACCE    , nbequ, iocc=2, vr=acce2)
        call intinivec(sd_int, FORCE_EX, nbequ, iocc=2, vr=fext2)

        nullify(buffint)
        call intbuff(sd_int, buffint, level=2)

    else 
        call intget(sd_int, VITE    , iocc=2, vr=vite2, buffer=buffint)
        call intget(sd_int, ACCE    , iocc=2, vr=acce2, buffer=buffint)
        call intget(sd_int, FORCE_EX, iocc=2, vr=fext2, buffer=buffint)
    end if

!   2 - Calculation of the system's state at instant t_(i+1) (saved under index=1)
    do i = 1, nbequ
        vite2(i) = vite1(i) + ( dt * acce1(i) )
        depl2(i) = depl1(i) + ( dt * vite2(i) )
    enddo

!   3 - Calculation of the acceleration from DE of motion at instant t_(i+1)
    call intsav(sd_int, TIME , 1, iocc=2, rscal=t1+dt, buffer=buffint)
    call intsav(sd_int, STEP , 1, iocc=2, rscal=dt, buffer=buffint)
    call intsav(sd_int, INDEX, 1, iocc=2, iscal=ind1+1, buffer=buffint)
    call dtmacce(sd_dtm, sd_int, 2, buffdtm, buffint)

    call intsav(sd_int, TIME , 1, iocc=1, rscal=t1+dt, buffer=buffint)
    call intsav(sd_int, STEP , 1, iocc=1, rscal=dt, buffer=buffint)
    call intsav(sd_int, INDEX, 1, iocc=1, iscal=ind1+1, buffer=buffint)
    call dcopy(nbequ, depl2, 1, depl1, 1)
    call dcopy(nbequ, vite2, 1, vite1, 1)
    call dcopy(nbequ, acce2, 1, acce1, 1)
    call dcopy(nbequ, fext2, 1, fext1, 1)

!   4 - Set the archiving index to 2
    call intsav(sd_int, IND_ARCH, 1, iscal=2, buffer=buffint)

end subroutine