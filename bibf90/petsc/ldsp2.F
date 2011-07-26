      SUBROUTINE LDSP2(pc,x,y,ierr)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF LDSP2 PETSC  DATE 26/07/2011   AUTEUR DESOZA T.DESOZA 
! ================================================================== 
! COPYRIGHT (C) 1991 - 2011  EDF R&D              WWW.CODE-ASTER.ORG 
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

#ifdef _HAVE_PETSC

!================================================================
#include "include/finclude/petsc.h"
#include "include/finclude/petscvec.h"
#include "include/finclude/petscsys.h"
#include "include/finclude/petscpc.h"
#include "include/finclude/petscksp.h"
!================================================================
      COMPLEX*16   CBID
      INTEGER      IRET
!     COMMUN POUR LE PRECONDITIONNEUR SIMPLE PRECISION LDLT_SP
      CHARACTER*19 SPSOMU,SPMAT,SPSOLV
      Vec          xlocal,xglobal
      VecScatter   xscatt
      COMMON /LDLTSP/xlocal,xscatt,xglobal,SPSOMU,SPMAT,SPSOLV
!----------------------------------------------------------------
!     Variables PETSc
      PetscInt ierr
      PC  pc
      Vec x,y
      PetscScalar XX(1)
      PetscOffset xidx
!----------------------------------------------------------------

! --  COPIE DU VECTEUR D'ENTREE, CAR ERREUR S'IL EST TRANSFORME
      call VecCopy(x,xlocal, ierr)
      CALL ASSERT(ierr.EQ.0)
      
! --  RECUPERATION DES VALEURS DU VECTEUR SUR LES DIFFERENTS PROCS
      call VecScatterBegin(xscatt,xlocal,xglobal,INSERT_VALUES,
     &                                             SCATTER_FORWARD,ierr)
      CALL ASSERT(ierr.EQ.0)
      call VecScatterEnd(xscatt,xlocal,xglobal,INSERT_VALUES,
     &                                             SCATTER_FORWARD,ierr)

      CALL ASSERT(ierr.EQ.0)
      call VecGetArray(xglobal,XX,xidx,ierr)
      CALL ASSERT(ierr.EQ.0)

! --  APPEL A LA ROUTINE DE PRECONDITIONNEMENT (DESCENTE/REMONTEE)
      CALL AMUMPH('RESOUD',SPSOMU,SPMAT,XX(xidx+1),CBID,' ',1,IRET,
     &            .TRUE.)

! --  ENVOI DES VALEURS DU VECTEUR SUR LES DIFFERENTS PROCS
      call VecRestoreArray(xglobal,XX,xidx,ierr)
      CALL ASSERT(ierr.EQ.0)
      call VecScatterBegin(xscatt,xglobal,y,INSERT_VALUES,
     &                                             SCATTER_REVERSE,ierr)
      CALL ASSERT(ierr.EQ.0)
      call VecScatterEnd(xscatt,xglobal,y,INSERT_VALUES,
     &                                             SCATTER_REVERSE,ierr)
      CALL ASSERT(ierr.EQ.0)

      ierr=IRET

#else

!     DECLARATION BIDON POUR ASSURER LA COMPILATION
      INTEGER pc,x,y,ierr

#endif

      END

