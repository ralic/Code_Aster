      SUBROUTINE LDSP1(pc,ierr)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF LDSP1 PETSC  DATE 18/06/2012   AUTEUR COURTOIS M.COURTOIS 
! ================================================================== 
! COPYRIGHT (C) 1991 - 2012  EDF R&D              WWW.CODE-ASTER.ORG 
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
! RESPONSABLE DESOZA T.DESOZA
!----------------------------------------------------------------
!
!  PRECONDITIONNEUR ISSU D'UNE FACTORISATION SIMPLE PRECISION
!
!----------------------------------------------------------------
      INCLUDE 'jeveux.h'
#ifdef _HAVE_PETSC

!================================================================
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
#include "finclude/petscksp.h"
#include "finclude/petscpc.h"
!================================================================
!----------------------------------------------------------------
!     VARIABLES LOCALES
      INTEGER      JREFA,IRET
!     COMMUN POUR LE PRECONDITIONNEUR SIMPLE PRECISION LDLT_SP
      CHARACTER*19 SPSOMU,SPMAT,SPSOLV
      Vec          xlocal,xglobal
      VecScatter   xscatt
      COMMON /LDLTSP/xlocal,xscatt,xglobal,SPSOMU,SPMAT,SPSOLV
!----------------------------------------------------------------
!     Variables PETSc
      PetscInt ierr
      PC  pc
!----------------------------------------------------------------

      CALL JEMARQ()

! --  LA MATRICE A ETE TRAITEE PAR PETSC DONC ELLE EST CONSIDEREE
! --  COMME FACTORISEE, SI ON LA LAISSE COMME CA MUMPH VA LA
! --  CONSIDERER COMME TELLE ET NE FERA PAS DE FACTO SP
      CALL JEVEUO(SPMAT//'.REFA','E',JREFA)
      ZK24(JREFA-1+8) = ' '

! --  APPEL A LA ROUTINE DE FACTO SP POUR LE PRECONDITIONNEMENT
      CALL PCMUMP(SPMAT,SPSOLV,IRET  )

      ierr = IRET

      CALL JEDEMA()

#else

!      DECLARATION BIDON POUR ASSURER LA COMPILATION
       INTEGER pc,ierr

#endif

      END
