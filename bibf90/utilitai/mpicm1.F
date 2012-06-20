      SUBROUTINE MPICM1(OPTMPI,TYPSCA,NBV,VI,VR)
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF MPICM1 UTILITAI  DATE 18/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
!----------------------------------------------------------------------
!  FONCTION REALISEE : SUR-COUCHE MPI
!
!  FAIRE UN ECHANGE BCAST/REDUCE/ALL_REDUCE SUR UN VECTEUR FORTRAN
!  (OU BIEN FAIRE UNE SIMPLE "BARRIERE" POUR SYNCHRONISER LES PROCS)
!
! ARGUMENTS D'APPELS
! IN OPTMPI :
!      /'MPI_MAX'  == 'ALLREDUCE + MAX'
!      /'MPI_MIN'  == 'ALLREDUCE + MIN'
!      /'MPI_SUM'  == 'ALLREDUCE + SUM'
!
!      /'BARRIER'  == 'BARRIER'
!      /'REDUCE'   == 'REDUCE + SUM' : TOUS -> 0
!      /'BCAST'    == 'BCAST'        : 0    -> TOUS
!
! IN   TYPSCA : /'I' /'R'  (SI OPTMPI /= 'BARRIER')
! IN   NBV    : LONGUEUR DU VECTEUR VI, VR  (SI OPTMPI /= 'BARRIER')
! IN   VI(*)  : VECTEUR D'ENTIERS A ECHANGER (SI TYPSCA='I')
! IN   VR(*)  : VECTEUR DE REELS A ECHANGER  (SI TYPSCA='R')
!----------------------------------------------------------------------
! RESPONSABLE PELLET J.PELLET
      IMPLICIT NONE
! DECLARATION PARAMETRES D'APPELS
      INCLUDE 'jeveux.h'
      CHARACTER*(*) OPTMPI,TYPSCA
      INTEGER       NBV,VI(*)
      REAL*8            VR(*)

#ifdef _USE_MPI
      INCLUDE 'mpif.h'
! DECLARATION VARIABLES LOCALES
      CHARACTER*1  TYPSC1
      INTEGER      VI2(1000)
      REAL*8       VR2(1000)
      INTEGER      LOISEM,K,JTRAV
      INTEGER*4    IERMPI,LR8,LINT,NBV4,LOPMPI,NBPRO4
      LOGICAL      FIRST
      SAVE         FIRST,LR8,LINT
      DATA         FIRST /.TRUE./
! ---------------------------------------------------------------------
      CALL JEMARQ()
! --- COMPTEUR
      CALL UTTCPU('CPU.CMPI.1','DEBUT',' ')

!     -- INITIALISATIONS :
!     --------------------
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
        FIRST= .FALSE.
      ENDIF

!     -- S'IL N'Y A QU'UN SEUL PROC, IL N'Y A RIEN A FAIRE :
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NBPRO4,IERMPI)
      CALL MPIERR(IERMPI)
      IF (NBPRO4.EQ.1) GOTO 9999



      IF (OPTMPI.EQ.'BARRIER') THEN
!     ---------------------------------
        CALL MPI_BARRIER(MPI_COMM_WORLD,IERMPI)
        CALL MPIERR(IERMPI)
        GOTO 9999
      ENDIF



!     -- SCALAIRE :
!     -------------
      TYPSC1=TYPSCA
      CALL ASSERT(TYPSC1.EQ.'R'.OR.TYPSC1.EQ.'I')
      NBV4=NBV

!     -- CHOIX OPERATION MPI  :
!     ---------------------------
      IF (OPTMPI.EQ.'MPI_MAX') THEN
        LOPMPI=MPI_MAX
      ELSE IF (OPTMPI.EQ.'MPI_MIN') THEN
        LOPMPI=MPI_MIN
      ELSE
        LOPMPI=MPI_SUM
      ENDIF

!     -- SI REDUCE OU ALLREDUCE, IL FAUT UN 2EME BUFFER
!        - SI NBV <= 1000 : ON UTILISE UN TABLEAU STATIQUE
!        - SINON ON ALLOUE UN TABLEAU JEVEUX
!     ------------------------------------------------------
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
!     ---------------------------------
        IF (TYPSC1.EQ.'R') THEN
          CALL MPI_BCAST(VR,NBV4,LR8 ,0,MPI_COMM_WORLD,IERMPI)
        ELSE IF(TYPSC1.EQ.'I') THEN
          CALL MPI_BCAST(VI,NBV4,LINT,0,MPI_COMM_WORLD,IERMPI)
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        CALL MPIERR(IERMPI)


      ELSE IF (OPTMPI.EQ.'REDUCE') THEN
!     ---------------------------------
        IF (TYPSC1.EQ.'R') THEN
          IF (NBV.LE.1000) THEN
            CALL MPI_REDUCE(VR2,      VR,NBV4,LR8,LOPMPI,
     &                      0,MPI_COMM_WORLD,IERMPI)
          ELSE
            CALL MPI_REDUCE(ZR(JTRAV),VR,NBV4,LR8,LOPMPI,
     &                      0,MPI_COMM_WORLD,IERMPI)
          ENDIF

        ELSE IF(TYPSC1.EQ.'I') THEN
          IF (NBV.LE.1000) THEN
            CALL MPI_REDUCE(VI2      ,VI,NBV4,LINT,LOPMPI,
     &                      0,MPI_COMM_WORLD,IERMPI)
          ELSE
            CALL MPI_REDUCE(ZI(JTRAV),VI,NBV4,LINT,LOPMPI,
     &                      0,MPI_COMM_WORLD,IERMPI)
          ENDIF
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        CALL MPIERR(IERMPI)


      ELSE IF (OPTMPI(1:4).EQ.'MPI_') THEN
!     ---------------------------------
        IF (TYPSC1.EQ.'R') THEN
          IF (NBV.LE.1000) THEN
            CALL MPI_ALLREDUCE(VR2,      VR,NBV4,LR8,LOPMPI,
     &                         MPI_COMM_WORLD,IERMPI)
          ELSE
            CALL MPI_ALLREDUCE(ZR(JTRAV),VR,NBV4,LR8,LOPMPI,
     &                         MPI_COMM_WORLD,IERMPI)
          ENDIF
        ELSEIF (TYPSC1.EQ.'I') THEN
          IF (NBV.LE.1000) THEN
            CALL MPI_ALLREDUCE(VI2,      VI,NBV4,LINT,LOPMPI,
     &                         MPI_COMM_WORLD,IERMPI)
          ELSE
            CALL MPI_ALLREDUCE(ZI(JTRAV),VI,NBV4,LINT,LOPMPI,
     &                         MPI_COMM_WORLD,IERMPI)
          ENDIF
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        CALL MPIERR(IERMPI)

      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      IF (OPTMPI.NE.'BCAST'.AND.NBV.GT.1000) THEN
        CALL JEDETR('&&MPICM1.TRAV')
      ENDIF

 9999 CONTINUE
! --- COMPTEUR
      CALL UTTCPU('CPU.CMPI.1','FIN',' ')
      CALL JEDEMA()
#endif
      END
