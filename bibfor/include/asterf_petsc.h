#ifndef ASTERF_PETSC_H
#define ASTERF_PETSC_H
#ifdef _HAVE_PETSC
!
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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

! Inclusion des interfaces Fortran de PETSc définies
! dans la librairie
#if PETSC_VERSION_LT(3,6,0)
#include <finclude/petscsys.h>
#include <finclude/petscvec.h>
#include <finclude/petscvec.h90>
#include <finclude/petscmat.h>
#include <finclude/petscmat.h90>
#include <finclude/petscpc.h>
#include <finclude/petscpc.h90>
#include <finclude/petscksp.h>
#include <finclude/petscksp.h90>
#include <finclude/petscviewer.h>
#include <finclude/petscviewer.h90>

#else
#ifndef PETSC_USE_FORTRAN_MODULES
#define PETSC_USE_FORTRAN_MODULES
#endif
#include <petsc/finclude/petscsysdef.h>
#include <petsc/finclude/petscvecdef.h>
#include <petsc/finclude/petscmatdef.h>
#include <petsc/finclude/petscpcdef.h>
#include <petsc/finclude/petsckspdef.h>
#include <petsc/finclude/petscviewerdef.h>
use petscsys
use petscvec
use petscmat
use petscpc
use petscksp
!
#endif
!
#endif
#endif
