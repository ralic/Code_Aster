#ifndef ASTERF_PETSC_H
#define ASTERF_PETSC_H
#ifdef _HAVE_PETSC
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
!----------------------------------------------------------------
!    include necessaire a la gestion des instances PETSC
!----------------------------------------------------------------
!
#include <petscversion.h>
! Gestion des versions de PETSc
#if PETSC_VERSION_MAJOR == 3 && PETSC_VERSION_MINOR <= 2
#   define ASTER_PETSC_VERSION_LEQ_32
#endif
#if PETSC_VERSION_MAJOR == 3 && PETSC_VERSION_MINOR <= 3
#   define ASTER_PETSC_VERSION_LEQ_33
#endif
#if PETSC_VERSION_MAJOR == 3 && PETSC_VERSION_MINOR <= 4
#   define ASTER_PETSC_VERSION_LEQ_34
#endif
#if PETSC_VERSION_MAJOR == 3 && PETSC_VERSION_MINOR <= 5
#   define ASTER_PETSC_VERSION_LEQ_35
#endif
! Inclusion des interfaces Fortran de PETSc définies
! dans la librairie  
#ifdef ASTER_PETSC_VERSION_LEQ_35
#include <finclude/petscsys.h>
#include <finclude/petscvec.h>
#include <finclude/petscvec.h90>
#include <finclude/petscmat.h>
#include <finclude/petscmat.h90>
#include <finclude/petscpc.h>
#include <finclude/petscpc.h90>
#include <finclude/petscksp.h>
#include <finclude/petscksp.h90>
#else
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscvec.h90>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscmat.h90>
#include <petsc/finclude/petscpc.h>
#include <petsc/finclude/petscpc.h90>
#include <petsc/finclude/petscksp.h>
#include <petsc/finclude/petscksp.h90>
!
#endif
!
#endif
#endif
