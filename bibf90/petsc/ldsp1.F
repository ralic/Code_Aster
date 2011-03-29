      SUBROUTINE LDSP1(pc,ierr)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF LDSP1 PETSC  DATE 29/03/2011   AUTEUR BOITEAU O.BOITEAU 
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
      CHARACTER*19 SOLVBD
      CHARACTER*19 NOMAT,NOSOLV
      INTEGER      JREFA,IRET
      COMMON /LDLTSP/xlocal,xscatt,xglobal,SOLVBD,NOMAT,NOSOLV
!================================================================
! --- DECLARATIONS NORMALISEES JEVEUX ----------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
!================================================================
!  Variables PETSc
      PetscInt ierr
      PC  pc
      Vec xlocal,xglobal
      VecScatter   xscatt
!================================================================

      CALL JEMARQ()

! -- LA MATRICE A ETE TRAITEE PAR PETSC DONC ELLE EST CONSIDEREE
! -- COMME FACTORISEE, SI ON LA LAISSE COMME CA MUMPH VA LA
! -- CONSIDERER COMME TELLE ET NE FERA PAS DE FACTO SP
      CALL JEVEUO(NOMAT//'.REFA','E',JREFA)
      ZK24(JREFA-1+8)=' '

! --  APPEL A LA ROUTINE DE FACTO SP POUR LE PRECONDITIONNEMENT
      CALL PCMUMP(NOMAT,NOSOLV,IRET  )

      ierr = IRET

      CALL JEDEMA()

#else

!      DECLARATION BIDON POUR ASSURER LA COMPILATION
       INTEGER pc,ierr

#endif

      END
