      SUBROUTINE MPICM0(RANG,NBPROC)
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF MPICM0 UTILITAI  DATE 16/02/2010   AUTEUR PELLET J.PELLET 
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  SUR-COUCHE MPI
!
! ARGUMENTS D'APPELS
! OUT RANG   : RANG DU PROCESSEUR
! OUT NBPROC : NOMBRE DE PROCESSEURS
!----------------------------------------------------------------------
! RESPONSABLE PELLET J.PELLET
      IMPLICIT NONE
C DECLARATION PARAMETRES D'APPELS
      INTEGER      RANG,NBPROC

#ifdef _USE_MPI
      INCLUDE 'mpif.h'
      INTEGER*4    RANG4,IERMPI,NBPRO4

        CALL MPI_COMM_RANK(MPI_COMM_WORLD,RANG4,IERMPI)
        RANG=RANG4

        CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NBPRO4,IERMPI)
        NBPROC=NBPRO4

#else
C======================================================================
        RANG=0
        NBPROC=1

#endif
      END
