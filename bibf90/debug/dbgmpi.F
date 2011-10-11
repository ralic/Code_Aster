      SUBROUTINE DBGMPI(ICO)
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF DBGMPI DEBUG  DATE 11/10/2011   AUTEUR DESOZA T.DESOZA 
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  utilitaire de debug pour MPI :
!      verifier qu'a un certain moment du programme, tous les
!      processeurs sont bien en coherence.
!
! exemple d'utilisation :
!     dans le superviseur (execop.f), on peut verifier le compteur
!     de commandes.
!     S'il n'est pas OK, c'est que les processeurs ne sont plus
!     synchrones
!
! ARGUMENTS D'APPELS
!   IN ico   : I8  : compteur entier
!
! s'arrete en erreur fatale si ICO n'est pas le meme sur tous
! les processeurs.
!----------------------------------------------------------------------
! RESPONSABLE PELLET J.PELLET
! CORPS DU PROGRAMME
      IMPLICIT NONE
! DECLARATION PARAMETRES D'APPELS
      INTEGER ICO
#ifdef _USE_MPI
      INCLUDE 'mpif.h'

! DECLARATION VARIABLES LOCALES
      INTEGER      RANG,NBPROC,LOISEM,ICOIN,ICOUT
      INTEGER*4    IERMPI,LR8,LINT,LC8
      LOGICAL      FIRST
      SAVE         FIRST,LR8,LINT,LC8
      DATA         FIRST /.TRUE./

! ---------------------------------------------------------------------

      IF (FIRST) THEN
!       -- POUR LA GESTION DES ERREURS :
        CALL MPI_ERRHANDLER_SET(MPI_COMM_WORLD,MPI_ERRORS_RETURN,IERMPI)
        CALL MPIERR(IERMPI)
        IF (LOISEM().EQ.8) THEN
          LINT=MPI_INTEGER8
        ELSE
          LINT=MPI_INTEGER
        ENDIF
        LR8 = MPI_DOUBLE_PRECISION
        LC8 = MPI_DOUBLE_COMPLEX
        FIRST= .FALSE.
      ENDIF

!     --  FILTRE POUR EVITER DU TRAVAIL SUPPLEMENTAIRE SI NBPROC=1
      CALL MPICM0(RANG  ,NBPROC)
      IF (NBPROC.EQ.1) GOTO 999

      ICOIN=ICO

!     -- POUR CAPTER LES ALL_REDUCE / MPI_SUM :
      CALL MPI_ALLREDUCE(ICOIN,ICOUT,1,LINT,MPI_SUM,
     &                                      MPI_COMM_WORLD,IERMPI)
      CALL MPIERR(IERMPI)
      ICOUT=ICOUT/NBPROC
      CALL ASSERT(ICOUT.EQ.ICOIN)

!     -- POUR CAPTER LES ALL_REDUCE / MPI_MAX :
      CALL MPI_ALLREDUCE(ICOIN,ICOUT,1,LINT,MPI_MAX,
     &                                      MPI_COMM_WORLD,IERMPI)
      CALL MPIERR(IERMPI)
      CALL ASSERT(ICOUT.EQ.ICOIN)

  999 CONTINUE
#endif
      END
