subroutine entete()
!           CONFIGURATION MANAGEMENT OF EDF VERSION
! ==================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D              WWW.CODE-ASTER.ORG
!
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
! MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS
! PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE
! LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,
! BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ==================================================================
! person_in_charge: mathieu.courtois at edf.fr
    implicit none
! ----------------------------------------------------------------------
!     ECRITURE DE L'ENTETE
! ----------------------------------------------------------------------
    include 'asterc/lihdfv.h'
    include 'asterc/limedv.h'
    include 'asterc/liscov.h'
    include 'asterc/mlnbpr.h'
    include 'asterc/prhead.h'
    include 'asterfort/mpiexe.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mess.h'
    integer :: vali(3)
!
#ifdef _USE_MPI
    include 'mpif.h'
    integer :: rang, nbproc, mpicow, ibid
#endif
#ifdef _USE_OPENMP
    integer :: nbth
#endif
! ----------------------------------------------------------------------
! --- INFORMATIONS GLOBALES
    call prhead(1)
! --- CONFIGURATION MPI
#ifdef _USE_MPI
    call mpiexe('MPI_COMM_WORLD', mpicow, ibid, ibid, ibid)
    call mpiexe('MPI_ERRHANDLER_SET', mpicow, ibid, ibid, ibid)
    call mpiexe('MPI_RANG_SIZE', mpicow, ibid, rang, nbproc)
    vali(1) = rang
    vali(2) = nbproc
    call u2mesi('I', 'SUPERVIS2_11', 2, vali)
#else
    call u2mess('I', 'SUPERVIS2_12')
#endif
! --- CONFIGURATION OPENMP
#ifdef _USE_OPENMP
    nbth = mlnbpr()
    call u2mesi('I', 'SUPERVIS2_13', 1, nbth)
#endif
! --- LIBRARIES HDF5 ET MED
#ifndef _DISABLE_HDF5
    call lihdfv(vali(1), vali(2), vali(3))
    call u2mesi('I', 'SUPERVIS2_14', 3, vali)
#else
    call u2mess('I', 'SUPERVIS2_15')
#endif
#ifndef _DISABLE_MED
    call limedv(vali(1), vali(2), vali(3))
    call u2mesi('I', 'SUPERVIS2_16', 3, vali)
#else
    call u2mess('I', 'SUPERVIS2_17')
#endif
! --- LIBRARIES SOLVEURS
#ifdef _HAVE_MUMPS
!     STOCKER A L'INSTALLATION MUMPS_VERSION DE smumps_c.h
    call u2mess('I', 'SUPERVIS2_18')
#else
    call u2mess('I', 'SUPERVIS2_19')
#endif
#ifndef _DISABLE_SCOTCH
    call liscov(vali(1), vali(2), vali(3))
    call u2mesi('I', 'SUPERVIS2_20', 3, vali)
#else
    call u2mess('I', 'SUPERVIS2_21')
#endif
!     SAUT DE LIGNE
    call u2mess('I', 'VIDE_1')
end subroutine
