#ifndef THREADING_INTERFACES_H
#define THREADING_INTERFACES_H
! personne_in_charge: mathieu.courtois@edf.fr
!
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
!
interface
#ifdef _USE_OPENMP
    subroutine omp_set_num_threads(a)
        integer, intent(in) :: a
    end subroutine

    function omp_get_max_threads()
        integer :: omp_get_max_threads
    end function

    function omp_get_thread_num()
        integer :: omp_get_thread_num
    end function
#endif

#ifdef _USE_OPENBLAS
    subroutine openblas_set_num_threads(a)
        integer, intent(in) :: a
    end subroutine
#endif

#ifdef _USE_MKL
    subroutine mkl_set_num_threads(a)
        integer, intent(in) :: a
    end subroutine
#endif

end interface
#endif
