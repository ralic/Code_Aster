!> This module manages the global values stored during the execution.
!
module superv_module
!
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
!
! person_in_charge: mathieu.courtois@edf.fr

! dummy argument because of ifdef
! aslint: disable=W0104

    use calcul_module, only: calcul_init
    implicit none
    private

#include "asterf_types.h"
#include "threading_interfaces.h"
#include "asterc/gtopti.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/assert.h"
#include "asterfort/check_aster_allocate.h"
#include "asterfort/foint0.h"
#include "asterfort/jermxd.h"
#include "asterfort/utgtme.h"
#include "asterfort/utmess.h"
#include "asterfort/utptme.h"

    logical :: first = .true.
    integer :: initMaxThreads = 0

    public :: superv_before, superv_after
    public :: asthread_getmax, asthread_setnum, asthread_blasset, asthread_getnum

contains

!>  Initialize the values or reinitialize them between before executing an operator
!
!>  @todo Remove treatments from execop.
    subroutine superv_before()
        mpi_int :: world, current
        integer :: maxThreads, iret
        real(kind=8) :: rval(7), v0
        character(len=8) :: k8tab(7)

!   Check MPI communicators: must be equal between operators
        call asmpi_comm('GET_WORLD', world)
        call asmpi_comm('GET', current)
        ASSERT( world == current )
!   OpenMP variables
        if ( first ) then
            first = .false.
            call gtopti('numthreads', maxThreads, iret)
            initMaxThreads = maxThreads
        endif
        call asthread_setnum( initMaxThreads )
        call asthread_blasset( initMaxThreads )
!   Memory allocation
!       Adjust Jeveux parameters
        k8tab(1) = 'LIMIT_JV'
        k8tab(2) = 'MEM_TOTA'
        k8tab(3) = 'VMSIZE'
        k8tab(4) = 'CMAX_JV'
        k8tab(5) = 'RLQ_MEM'
        k8tab(6) = 'COUR_JV'
        k8tab(7) = 'VMPEAK'
        call utgtme(7, k8tab, rval, iret)
        if ( rval(3) - rval(6) .lt. rval(5) ) then
!           the remaining memory decreased: adjust it
            call utptme('RLQ_MEM ', rval(3) - rval(6), iret)
        endif
        if (rval(2) - rval(5) .ge. 0) then
            v0 = rval(1)
            if ((rval(2) - rval(5)) .gt. v0) then
!               reduce memory limit
                call jermxd((rval(2) - rval(5)) * 1024 * 1024, iret)
                if (iret .eq. 0) then
                    k8tab(1) = 'RLQ_MEM'
                    k8tab(2) = 'LIMIT_JV'
                    call utgtme(2, k8tab, rval, iret)
                    if (abs(rval(2) - v0) .gt. v0 * 0.1d0) then
                       call utmess('I', 'JEVEUX1_73', nr=2, valr=rval)
                    endif
                endif
            endif
        endif
!       Reinit calcul mark in case of exception
        call calcul_init()
!       Reinitialize counter for as_[de]allocate
        call check_aster_allocate(init=0)
!       Reset commons used for function interpolation
        call foint0()
    end subroutine superv_before


!>  Initialize the values or reinitialize them between before executing an operator
    subroutine superv_after()

!   Memory allocation
!       Check for not deallocated vectors
        call check_aster_allocate()
    end subroutine superv_after

!>  Return the current maximum number of available threads
!
!>  @return current number of threads
    function asthread_getmax()
        implicit none
        integer :: asthread_getmax
#ifdef _USE_OPENMP
        asthread_getmax = omp_get_max_threads()
#else
        asthread_getmax = 1
#endif
    end function asthread_getmax


!>  Set the maximum number of threads for OpenMP and Blas
!
!>  @param[in] nbThreads new maximum number of threads
    subroutine asthread_setnum( nbThreads )
        implicit none
        integer, intent(in) :: nbThreads
#ifdef _USE_OPENMP
        call omp_set_num_threads( nbThreads )
#endif
    end subroutine asthread_setnum


!>  Set the maximum number of threads for Blas functions
!
!>  @param[in] nbThreads new maximum number of threads for Blas
    subroutine asthread_blasset( nbThreads )
        implicit none
        integer, intent(in) :: nbThreads
#ifdef _USE_OPENMP
# ifdef _USE_OPENBLAS
        call openblas_set_num_threads( nbThreads )
# endif
# ifdef _USE_MKL
        call mkl_set_num_threads( nbThreads )
# endif
#endif
    end subroutine asthread_blasset


!>  Return the current thread id
!
!>  @return the current thread id
    function asthread_getnum()
        implicit none
        integer :: asthread_getnum
#ifdef _USE_OPENMP
        asthread_getnum = omp_get_thread_num()
#else
        asthread_getnum = 0
#endif
    end function asthread_getnum

end module superv_module
