      SUBROUTINE LDSP2(pc,x,y,ierr)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF LDSP2 PETSC  DATE 19/07/2010   AUTEUR TARDIEU N.TARDIEU 
! ================================================================== 
! COPYRIGHT (C) 1991 - 2010  EDF R&D              WWW.CODE-ASTER.ORG 
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
      CHARACTER*24 SOLVBD
      CHARACTER*19 NOMAT,NOSOLV
      COMPLEX*16   CBID
      INTEGER      IRET
      COMMON       /LDLTSP/xtemp,SOLVBD,NOMAT,NOSOLV
!=================================================================
!  Variables PETSc
      PetscInt ierr
      PC  pc
      Vec x,y,xtemp
      PetscScalar XX(1)
      PetscOffset xidx
!=================================================================

C -- COPIE DU VECTEUR D'ENTREE, CAR ERREUR SI IL EST TRANSFORME

      call VecCopy(x,xtemp, ierr)
      CALL ASSERT(ierr.EQ.0)

      call VecGetArray(xtemp,XX,xidx,ierr)
      CALL ASSERT(ierr.EQ.0)

C -- APPEL A LA ROUTINE LE PRECONDITIONNEMENT (DESCENTE/REMONTEE)
      CALL AMUMPH('RESOUD',SOLVBD,NOMAT,XX(xidx+1),CBID,' ',1,IRET)

      call VecRestoreArray(xtemp,XX,xidx,ierr)
      CALL ASSERT(ierr.EQ.0)
      call VecCopy(xtemp,y,ierr)
      CALL ASSERT(ierr.EQ.0)

      ierr=IRET

#else

C      DECLARATION BIDON POUR ASSURER LA COMPILATION
      INTEGER pc,x,y,ierr

#endif

      END

