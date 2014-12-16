#ifndef ASTERF_PETSC_H
#define ASTERF_PETSC_H
# ifdef _HAVE_PETSC
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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

# include "finclude/petsc.h"
!
! Gestion des versions de PETSc
#if PETSC_VERSION_MAJOR == 3 && PETSC_VERSION_MINOR <= 2
#   define ASTER_PETSC_VERSION_LEQ_32
#endif
#if PETSC_VERSION_MAJOR == 3 && PETSC_VERSION_MINOR <= 4
#   define ASTER_PETSC_VERSION_LEQ_34
#endif
!
# include "petsc_interfaces.h"


!----------------------------------------------------------------
!   spetsc : common pour les instances PETSC

! On ne peut pas creer directement un tableau de pointer,
! il faut passer par un artifice (type derive) :
type p_int4
sequence
    integer(kind=4), pointer :: pi4(:)
end type

integer, parameter :: nmxins=5
character(len=19)  :: nomats(nmxins), nosols(nmxins)
character(len=14)  :: nonus(nmxins)
Mat :: ap(nmxins)
KSP :: kp(nmxins)
Vec :: b, x

! Les variables suivantes sont utilisees par les preconditionneurs multigrille
integer(kind=4) :: tblocs(nmxins),fictifs(nmxins)
type(p_int4), target :: new_ieqs(nmxins), old_ieqs(nmxins)
common /spetsc/ap, kp, b, x, tblocs,fictifs, new_ieqs, old_ieqs, &
       nomats, nosols, nonus
!
!----------------------------------------------------------------
!   ldltsp : common pour le preconditionneur simple precision ldlt_sp
character(len=19) :: spsomu, spmat, spsolv
Vec :: xlocal, xglobal
VecScatter :: xscatt
common /ldltsp/xlocal, xscatt, xglobal, spsomu, spmat, spsolv
!----------------------------------------------------------------
#endif
#endif
