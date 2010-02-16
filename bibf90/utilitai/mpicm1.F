      SUBROUTINE MPICM1(OPTMPI,TYPSCA,NBV,VI,VR)
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!&           CONFIGURATION MANAGEMENT OF EDF VERSION                   
!& MODIF MPICM1 UTILITAI  DATE 16/02/2010   AUTEUR PELLET J.PELLET 
! ==================================================================
! COPYRIGHT (C) 1991 - 2009  EDF R&D              WWW.CODE-ASTER.ORG
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
! BUT : SUR-COUCHE POUR MPI :
! FAIRE UN ECHANGE BCAST/REDUCE/ALL_REDUCE SUR UN VECTEUR FORTRAN
!
! ARGUMENTS D'APPELS
! IN OPTMPI   :
!      /'MPI_MAX'  == 'ALLREDUCE + MAX'
!      /'MPI_MIN'  == 'ALLREDUCE + MIN'
!      /'MPI_SUM'  == 'ALLREDUCE + SUM'
!
!      /'REDUCE'   == 'REDUCE + SUM' : TOUS -> 0
!      /'BCAST'    == 'BCAST'        : 0    -> TOUS
!
! IN   TYPSCA: /'I' /'R'
! IN   NBV   : LONGUEUR DU VECTEUR VI, VR
! IN   VI(*) : VECTEUR D'ENTIERS A ECHANGER (SI TYPSC1='I')
! IN   VR(*) : VECTEUR DE REELS A ECHANGER (SI TYPSC1='R')
!----------------------------------------------------------------------
! RESPONSABLE

      IMPLICIT NONE


C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC,CBID
      COMMON /CVARJE/ZC(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------


      CHARACTER*(*) OPTMPI,TYPSCA
      CHARACTER*1 TYPSC1
      INTEGER      NBV,VI(*),VI2(1000)
      REAL*8   VR(*),VR2(1000)
      INTEGER      IBID ,LOISEM,K,JTRAV
      INTEGER*4    IERMPI,LR8,LINT,LMPI,NBV4,LC8,LOPMPI,NBPRO4
      LOGICAL      FIRST
      SAVE         LR8,LINT,FIRST
      DATA         FIRST /.TRUE./

#ifdef _USE_MPI
      INCLUDE 'mpif.h'

C ---------------------------------------------------------------------

C     -- S'IL N'Y A QU'UN SEUL PROC, IL N'Y A RIEN A FAIRE :
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NBPRO4,IERMPI)
      IF (NBPRO4.EQ.1) GOTO 9999



      TYPSC1=TYPSCA
      CALL ASSERT(TYPSC1.EQ.'R'.OR.TYPSC1.EQ.'I')
      NBV4=NBV


C     -- INITIALISATIONS :
C     --------------------
      IF (FIRST) THEN
C       -- POUR LA GESTION DES ERREURS :
        CALL MPI_ERRHANDLER_SET(MPI_COMM_WORLD,MPI_ERRORS_RETURN,IERMPI)
        IF (LOISEM().EQ.8) THEN
          LINT=MPI_INTEGER8
        ELSE
          LINT=MPI_INTEGER
        ENDIF
        LR8 = MPI_DOUBLE_PRECISION
        FIRST= .FALSE.
      ENDIF



C     -- CHOIX OPERATION MPI  :
C     ---------------------------
      IF (OPTMPI.EQ.'MPI_MAX') THEN
        LOPMPI=MPI_MAX
      ELSE IF (OPTMPI.EQ.'MPI_MIN') THEN
        LOPMPI=MPI_MIN
      ELSE
        LOPMPI=MPI_SUM
      ENDIF



C     -- SI REDUCE OU ALLREDUCE, IL FAUT UN 2EME BUFFER
C        - SI NBV <= 1000 : ON UTILISE UN TABLEAU STATIQUE
C        - SINON ON ALLOUE UN TABLEAU JEVEUX
C     ------------------------------------------------------
      IF (OPTMPI.NE.'BCAST') THEN
        CALL ASSERT(NBV.GT.0)

        IF (NBV.LE.1000) THEN
          IF (TYPSC1.EQ.'R') THEN
            DO 1, K=1,NBV
              VR2(K)=VR(K)
 1          CONTINUE
          ELSE IF (TYPSC1.EQ.'I') THEN
            DO 2, K=1,NBV
              VI2(K)=VI(K)
 2          CONTINUE
          ENDIF

        ELSE
          IF (TYPSC1.EQ.'R') THEN
            CALL WKVECT('&&MPICM1.TRAV','V V R',NBV,JTRAV)
            DO 3,K=1,NBV
              ZR(JTRAV-1+K)=VR(K)
 3          CONTINUE
          ELSEIF (TYPSC1.EQ.'I') THEN
            CALL WKVECT('&&MPICM1.TRAV','V V I',NBV,JTRAV)
            DO 4,K=1,NBV
              ZI(JTRAV-1+K)=VI(K)
 4          CONTINUE
          ENDIF
        ENDIF
      ENDIF



      IF (OPTMPI.EQ.'BCAST') THEN
C     ---------------------------------
        IF (TYPSC1.EQ.'R') THEN
          CALL MPI_BCAST(VR,NBV4,LR8,0,MPI_COMM_WORLD,IERMPI)
        ELSE IF(TYPSC1.EQ.'I') THEN
          CALL MPI_BCAST(VI,NBV4,LINT,0,MPI_COMM_WORLD,IERMPI)
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF


      ELSE IF (OPTMPI.EQ.'REDUCE') THEN
C     ---------------------------------
        IF (TYPSC1.EQ.'R') THEN
          IF (NBV.LE.1000) THEN
            CALL MPI_REDUCE(VR2,VR,NBV4,LR8,LOPMPI,
     &                    0,MPI_COMM_WORLD,IERMPI)
          ELSE
            CALL MPI_REDUCE(ZR(JTRAV),VR,NBV4,LR8,LOPMPI,
     &                    0,MPI_COMM_WORLD,IERMPI)
          ENDIF

        ELSE IF(TYPSC1.EQ.'I') THEN
          IF (NBV.LE.1000) THEN
            CALL MPI_REDUCE(VI2,VI,NBV4,LINT,LOPMPI,
     &                    0,MPI_COMM_WORLD,IERMPI)
          ELSE
            CALL MPI_REDUCE(ZI(JTRAV),VI,NBV4,LINT,LOPMPI,
     &                    0,MPI_COMM_WORLD,IERMPI)
          ENDIF

        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF


      ELSE IF (OPTMPI(1:4).EQ.'MPI_') THEN
C     ---------------------------------
        IF (TYPSC1.EQ.'R') THEN
          IF (NBV.LE.1000) THEN
            CALL MPI_ALLREDUCE(VR2,VR,NBV4,LR8,LOPMPI,
     &                      MPI_COMM_WORLD,IERMPI)
          ELSE
            CALL MPI_ALLREDUCE(ZR(JTRAV),VR,NBV4,LR8,LOPMPI,
     &                      MPI_COMM_WORLD,IERMPI)
          ENDIF

        ELSEIF (TYPSC1.EQ.'I') THEN
          IF (NBV.LE.1000) THEN
            CALL MPI_ALLREDUCE(VI2,VI,NBV4,LINT,LOPMPI,
     &                      MPI_COMM_WORLD,IERMPI)
          ELSE
            CALL MPI_ALLREDUCE(ZI(JTRAV),VI,NBV4,LINT,LOPMPI,
     &                      MPI_COMM_WORLD,IERMPI)

          ENDIF

        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF


      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      IF (OPTMPI.NE.'BCAST'.AND.NBV.GT.1000)
     &     CALL JEDETR('&&MPICM1.TRAV')


 9999 CONTINUE
#endif
      END
