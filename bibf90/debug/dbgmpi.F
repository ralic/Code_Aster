      SUBROUTINE DBGMPI(ICO)
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF DBGMPI DEBUG  DATE 26/05/2010   AUTEUR PELLET J.PELLET 
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
!    - FONCTION REALISEE:  utilitaire de debug pour MPI :
!      verifier qu'a un certain moment du programme, tous les
!      processeurs sont bien en coherence.
!
! exemple d'utilisation :
!     dans le superviseur (execop.f), on peut verifier le compteur
!     de commandes.
!     S'il n'est pas OK, c'est que le mpicm3 du superviseur s'apparie
!     a un autre MPI_ALL_REDUCE('INTEGER','MPI_SUM'/'MPI_MAX')
!
! ARGUMENTS D'APPELS
!   IN ico   : I8  : compteur entier
!
! s'arrete en erreur fatale si ico n'est pas le meme sur tous
! les processeurs.
!----------------------------------------------------------------------
! RESPONSABLE PELLET J.PELLET
! CORPS DU PROGRAMME
      IMPLICIT NONE
C DECLARATION PARAMETRES D'APPELS
      INTEGER ICO

C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
      INTEGER*4          ZI4
      COMMON  / I4VAJE / ZI4(1)
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNUM
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

#ifdef _USE_MPI
      INCLUDE 'mpif.h'

C DECLARATION VARIABLES LOCALES
      INTEGER      NBPROC,RANG,LOISEM,ICO2,ICO3
      INTEGER*4    RANG4,IERMPI,LR8,LINT,NBPRO4,N4,LC8
      LOGICAL      FIRST
      SAVE         LR8,LINT,FIRST,LC8
      DATA         FIRST /.TRUE./

C ---------------------------------------------------------------------

      IF (FIRST) THEN
        CALL MPI_ERRHANDLER_SET(MPI_COMM_WORLD,MPI_ERRORS_RETURN,IERMPI)
        CALL FETMER(IERMPI)
        IF (LOISEM().EQ.8) THEN
          LINT=MPI_INTEGER8
        ELSE
          LINT=MPI_INTEGER
        ENDIF
        LR8 = MPI_DOUBLE_PRECISION
        LC8 = MPI_DOUBLE_COMPLEX
        FIRST= .FALSE.
      ENDIF

C     --  FILTRE POUR EVITER DU TRAVAIL SUPPLEMENTAIRE SI NBPROC=1
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NBPRO4,IERMPI)
      CALL FETMER(IERMPI)
      IF (NBPRO4.EQ.1) GOTO 999


      ICO2=ICO
      N4=1

C     -- POUR CAPTER LES ALL_REDUCE / MPI_SUM :
      CALL MPI_ALLREDUCE(ICO2,ICO3,N4,LINT,MPI_SUM,
     &                      MPI_COMM_WORLD,IERMPI)
      ICO3=ICO3/NBPRO4
      CALL ASSERT(ICO3.EQ.ICO2)
      CALL FETMER(IERMPI)

C     -- POUR CAPTER LES ALL_REDUCE / MPI_MAX :
      CALL MPI_ALLREDUCE(ICO2,ICO3,N4,LINT,MPI_MAX,
     &                      MPI_COMM_WORLD,IERMPI)
      CALL ASSERT(ICO3.EQ.ICO2)
      CALL FETMER(IERMPI)


  999 CONTINUE


#else
C======================================================================
C     -- RIEN A FAIRE !
#endif
      END
