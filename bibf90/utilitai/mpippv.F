      SUBROUTINE MPIPPV(OPTMPI,TYPSCA,NBV,VI,VI4,
     &                  VR,NUDEST,NUMESS)
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF MPIPPV UTILITAI  DATE 26/02/2013   AUTEUR BOITEAU O.BOITEAU 
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
!----------------------------------------------------------------------
!  FONCTION REALISEE : SUR-COUCHE MPI
!
!  COMMUNICATION MPI POINT A POINT D'UN VECTEUR FORTRAN
!                --- -       -          -
!
! ARGUMENTS D'APPELS
! IN OPTMPI :
!      /'MPI_SEND' == ENVOYER UN MESSAGE MPI
!      /'MPI_RECV' == RECEVOIR UN MESSAGE MPI
!
! IN TYPSCA : /'I' /'I4' /'R'
! IN NBV    : LONGUEUR DU VECTEUR VI, VR
! IN VI(*)  : VECTEUR D'ENTIERS A ECHANGER (SI TYPSCA='I')
! IN VI4(*) : VECTEUR D'ENTIERS A ECHANGER (SI TYPSCA='I4')
! IN VR(*)  : VECTEUR DE REELS A ECHANGER  (SI TYPSCA='R')
! IN NUDEST : NUMERO DU PROCESSEUR D'ORIGINE OU DESTINATAIRE
! IN NUMESS : NUMERO MPI DU MESSAGE
!----------------------------------------------------------------------
! RESPONSABLE SELLENET N.SELLENET
      IMPLICIT NONE
! DECLARATION PARAMETRES D'APPELS
      INCLUDE 'jeveux.h'
      CHARACTER*(*) OPTMPI,TYPSCA
      INTEGER       NBV,VI(*)
      INTEGER*4     VI4(*)
      REAL*8        VR(*)
      INTEGER       NUDEST,NUMESS
!
#ifdef _USE_MPI
      INCLUDE 'mpif.h'
! DECLARATION VARIABLES LOCALES
      CHARACTER*2  TYPSC1
      INTEGER      LOISEM,IRET
      INTEGER*4    IERMPI,LR8,LINT,LINT4,NBV4,NBPRO4,NUDES4,NUMES4
      INTEGER*4    COMCOU,MPICOU
! ---------------------------------------------------------------------
      CALL JEMARQ()
!---- COMMUNICATEUR MPI DE TRAVAIL
      MPICOU=COMCOU(1)
! --- COMPTEUR
      CALL UTTCPU('CPU.CMPI.1','DEBUT',' ')
!
!     -- INITIALISATIONS :
!     --------------------
      IF (LOISEM().EQ.8) THEN
        LINT=MPI_INTEGER8
      ELSE
        LINT=MPI_INTEGER
      ENDIF
      LINT4=MPI_INTEGER4
      LR8 = MPI_DOUBLE_PRECISION
!
!     -- S'IL N'Y A QU'UN SEUL PROC, IL N'Y A RIEN A FAIRE :
      CALL MPI_COMM_SIZE(MPICOU,NBPRO4,IERMPI)
      CALL MPIERR(IERMPI)
      IF (NBPRO4.EQ.1) GOTO 9999
!
!     -- SCALAIRE :
!     -------------
      TYPSC1=TYPSCA
      NBV4=NBV
      NUDES4=NUDEST
      NUMES4=NUMESS
!
      IF (OPTMPI.EQ.'MPI_SEND') THEN
!     ---------------------------------
        IF (TYPSC1.EQ.'R') THEN
          CALL MPI_SEND(VR,NBV4,LR8,NUDES4,NUMES4,
     &                  MPICOU,IERMPI)
        ELSEIF (TYPSC1.EQ.'I') THEN
          CALL MPI_SEND(VI,NBV4,LINT,NUDES4,NUMES4,
     &                  MPICOU,IERMPI)
        ELSEIF (TYPSC1.EQ.'I4') THEN
          CALL MPI_SEND(VI4,NBV4,LINT4,NUDES4,NUMES4,
     &                  MPICOU,IERMPI)
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        CALL MPIERR(IERMPI)
      ELSE IF (OPTMPI.EQ.'MPI_RECV') THEN
!     ---------------------------------
        IF (TYPSC1.EQ.'R ') THEN
          CALL MPI_RECV(VR,NBV4,LR8,NUDES4,NUMES4,
     &                  MPICOU,MPI_STATUS_IGNORE,
     &                  IERMPI)
        ELSEIF (TYPSC1.EQ.'I ') THEN
          CALL MPI_RECV(VI,NBV4,LINT,NUDES4,NUMES4,
     &                  MPICOU,MPI_STATUS_IGNORE,
     &                  IERMPI)
        ELSEIF (TYPSC1.EQ.'I4') THEN
          CALL MPI_RECV(VI4,NBV4,LINT4,NUDES4,NUMES4,
     &                  MPICOU,MPI_STATUS_IGNORE,
     &                  IERMPI)
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        CALL MPIERR(IERMPI)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
!
 9999 CONTINUE
! --- COMPTEUR
      CALL UTTCPU('CPU.CMPI.1','FIN',' ')
      CALL JEDEMA()
#endif
      END
