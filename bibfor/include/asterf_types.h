#ifndef ASTERF_TYPES_H
#define ASTERF_TYPES_H
!
!   COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
!   THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
!   IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
!   THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
!   (AT YOUR OPTION) ANY LATER VERSION.
!
!   THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
!   WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
!   MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
!   GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
!   YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
!   ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!      1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
! Definition of types used by aster
!
#include "asterf.h"
!
#define aster_int_kind ASTER_INT_SIZE
#define aster_int integer(kind=aster_int_kind)
#define to_aster_int(a) int(a, ASTER_INT_SIZE)
!
#ifdef _DISABLE_MED
#   define MED_INT_SIZE 4
#endif
#define med_int_kind MED_INT_SIZE
#define med_int integer(kind=med_int_kind)
#define to_med_int(a) int(a, MED_INT_SIZE)
!
#ifndef _USE_MPI
#   define MPI_INT_SIZE 4
#endif
#define mpi_int_kind MPI_INT_SIZE
#define mpi_int integer(kind=mpi_int_kind)
#define mpi_bool logical(kind=mpi_int_kind)
#define to_mpi_int(a) int(a, MPI_INT_SIZE)
!
#ifdef _DISABLE_MUMPS
#   define MUMPS_INT_SIZE 4
#endif
#define mumps_int_kind MUMPS_INT_SIZE
#define mumps_int integer(kind=mumps_int_kind)
#define to_mumps_int(a) int(a, MUMPS_INT_SIZE)
!
#define blas_int_kind BLAS_INT_SIZE
#define blas_int integer(kind=blas_int_kind)
#define to_blas_int(a) int(a, BLAS_INT_SIZE)
!
#define to_petsc_int(a) int(a, 4)
!
#endif
