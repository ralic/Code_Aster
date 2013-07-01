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
#   include "finclude/petscsys.h"
#   include "finclude/petscvec.h"
#   include "finclude/petscmat.h"
#   include "finclude/petscksp.h"
#   include "finclude/petscpc.h"
#   include "finclude/petscdmda.h"
#   include "finclude/petscis.h"
#   include "finclude/petscviewer.h"

!----------------------------------------------------------------
!   spetsc : common pour les instances PETSC
    integer      nmxins
    parameter   (nmxins=5)
    character*19 nomats(nmxins),nosols(nmxins)
    character*14  nonus(nmxins)
    Mat    ap(nmxins)
    KSP    kp(nmxins)
    Vec    b,x
    common /spetsc/ ap,kp,b,x,nomats,nosols,nonus

!----------------------------------------------------------------
!   ldltsp : common pour le preconditionneur simple precision ldlt_sp
    character(len=19) :: spsomu, spmat, spsolv
    Vec :: xlocal, xglobal
    VecScatter :: xscatt
    common /ldltsp/xlocal,xscatt,xglobal,spsomu,spmat,spsolv
!----------------------------------------------------------------
