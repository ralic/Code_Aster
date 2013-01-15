      SUBROUTINE MPIALR()
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF MPIALR UTILITAI  DATE 14/01/2013   AUTEUR COURTOIS M.COURTOIS 
! ================================================================== 
! COPYRIGHT (C) 1991 - 2013  EDF R&D              WWW.CODE-ASTER.ORG 
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
! RESPONSABLE COURTOIS M.COURTOIS
      IMPLICIT NONE
!-----------------------------------------------------------------------
!     FONCTION REALISEE : MPI CHECK ALARM
!       EN FIN D'EXECUTION, LE PROCESSEUR #0 DONNE A L'UTILISATEUR
!       LA LISTE DES ALARMES QUI ONT ETE EMISES PAR PROCESSEUR.
!-----------------------------------------------------------------------
#ifdef _USE_MPI
!
      INCLUDE 'mpif.h'
      INCLUDE 'aster_constant.h'
!
      LOGICAL   GTSTAT
      INTEGER*4 RANK, NBPRO4, IERMPI, IVAL, MPST(MPI_STATUS_SIZE)
      INTEGER   I, NP1, VALI(2)
      LOGICAL   VU
!
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, RANK, IERMPI)
      CALL MPIERR(IERMPI)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, NBPRO4, IERMPI)
      CALL MPIERR(IERMPI)
      NP1 = NBPRO4 - 1
!
      IF ( .NOT. GTSTAT(ST_OK) ) THEN
        IF ( RANK .EQ. 0 ) THEN
          CALL U2MESS('I', 'CATAMESS_88')
        ENDIF
        GOTO 9999
      ENDIF
!
!     SUR LES PROCESSEURS AUTRES QUE #0
!
      IF (RANK .NE. 0) THEN
!       RECUPERER LA LISTE DES ALARMES
        I = 0
        CALL GTALRM(I)
!       CHAQUE PROCESSEUR ENVOIE LA LISTE DES ALARMES EMISES AU PROC #0
        IVAL = I
        CALL MPI_SEND(IVAL, 1, MPI_INTEGER4, 0, ST_TAG_ALR,
     &                 MPI_COMM_WORLD, IERMPI)
        CALL MPIERR(IERMPI)
!
!     SUR LE PROCESSEUR #0
!
      ELSE
!       DEMANDE LA LISTE DES ALARMES A CHAQUE PROCESSEUR
        VU = .FALSE.
        DO 10 I=1, NP1
          CALL MPI_RECV(IVAL, 1, MPI_INTEGER4, I, ST_TAG_ALR,
     &                   MPI_COMM_WORLD, MPST, IERMPI)
          CALL MPIERR(IERMPI)
          IF ( IVAL .NE. 0) THEN
            VU = .TRUE.
            VALI(1) = I
            VALI(2) = IVAL
            IF ( IVAL .EQ. 1 ) THEN
              CALL U2MESI('A+', 'APPELMPI_1', 2, VALI)
            ELSE
              CALL U2MESI('A+', 'APPELMPI_2', 2, VALI)
            ENDIF
          ENDIF
 10     CONTINUE
        IF ( VU ) THEN
          CALL U2MESS('A', 'VIDE_1')
        ENDIF
!
      ENDIF
 9999 CONTINUE
#endif
      END
