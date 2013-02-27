      SUBROUTINE MPICM1(OPTMPI,TYPSCA,NBV,BCRANK,VI,VR,VC)
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF MPICM1 UTILITAI  DATE 26/02/2013   AUTEUR BOITEAU O.BOITEAU 
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
!  FAIRE UN ECHANGE BCAST/REDUCE/ALL_REDUCE SUR UN VECTEUR FORTRAN
!  (OU BIEN FAIRE UNE SIMPLE "BARRIERE" POUR SYNCHRONISER LES PROCS)
!
! ARGUMENTS D'APPELS
! IN OPTMPI :
!      /'MPI_MAX'  == 'ALLREDUCE + MAX' (SEULEMENT 'R'/'I')
!      /'MPI_MIN'  == 'ALLREDUCE + MIN' (SEULEMENT 'R'/'I')
!      /'MPI_SUM'  == 'ALLREDUCE + SUM'
!
!      /'BARRIER'  == 'BARRIER'
!      /'REDUCE'   == 'REDUCE + SUM' : TOUS -> 0
!      /'BCAST'    == 'BCAST'            : PROC DE RANG=BCRANK -> TOUS
!      /'BCASTP'   == 'BCAST'PAR PAQUETS : PROC DE RANG=BCRANK -> TOUS
!
! IN   TYPSCA : /'I' /'R' /'C'  (SI OPTMPI /= 'BARRIER')
! IN   NBV    : LONGUEUR DU VECTEUR V*  (SI OPTMPI /= 'BARRIER')
! IN   BCRANK : RANG DU PROCESSUS MPI D'OU EMANE LE BCAST 
!                                        (SI OPTMPI='BCAST'/'BCASTP')
! IN   VI(*)  : VECTEUR D'ENTIERS A ECHANGER    (SI TYPSCA='I')
! IN   VR(*)  : VECTEUR DE REELS A ECHANGER     (SI TYPSCA='R')
! IN   VC(*)  : VECTEUR DE COMPLEXES A ECHANGER (SI TYPSCA='C')
!----------------------------------------------------------------------
! RESPONSABLE PELLET J.PELLET
      IMPLICIT NONE
! DECLARATION PARAMETRES D'APPELS
      INCLUDE 'jeveux.h'
      CHARACTER*(*) OPTMPI,TYPSCA
      INTEGER       VI(*),NBV,BCRANK
      REAL*8        VR(*)
      COMPLEX*16    VC(*)

#ifdef _USE_MPI
      INCLUDE 'mpif.h'
! DECLARATION VARIABLES LOCALES
      CHARACTER*1   TYPSC1
      INTEGER       VI2(1000)
      REAL*8        VR2(1000)
      COMPLEX*16    VC2(1000)
      INTEGER       LOISEM,K,JTRAV,IRET,SIZBMPI,NBCAST,IMAIN,IREST
      INTEGER*4     IERMPI,LR8,LINT,NBV4,LOPMPI,NBPRO4,COMCOU,MPICOU,
     &              LC8
! ---------------------------------------------------------------------
      CALL JEMARQ()
! --- COMMUNICATEUR MPI DE TRAVAIL
      MPICOU=COMCOU(1)
! --- COMPTEUR
      CALL UTTCPU('CPU.CMPI.1','DEBUT',' ')


!     -- INITIALISATIONS :
!     --------------------
!     TAILLE MAX DES PAQUETS SI OPTMPI='BCASTP'
      SIZBMPI=1D+6
      IF (LOISEM().EQ.8) THEN
        LINT=MPI_INTEGER8
      ELSE
        LINT=MPI_INTEGER
      ENDIF
      LR8 = MPI_DOUBLE_PRECISION
      LC8 = MPI_DOUBLE_COMPLEX

!     -- S'IL N'Y A QU'UN SEUL PROC, IL N'Y A RIEN A FAIRE :
      CALL MPI_COMM_SIZE(MPICOU,NBPRO4,IERMPI)
      CALL MPIERR(IERMPI)
      IF (NBPRO4.EQ.1) GOTO 9999

!     -- VERIFICATION RENDEZ-VOUS
      IRET=1
      CALL MPICHK(NBPRO4,IRET)
      IF (IRET .NE. 0) THEN
        CALL U2MESK('I', 'APPELMPI_83', 1, OPTMPI)
        GOTO 9999
      ENDIF

      IF (OPTMPI.EQ.'BARRIER') THEN
!     ---------------------------------
        CALL MPI_BARRIER(MPICOU,IERMPI)
        CALL MPIERR(IERMPI)
        GOTO 9999
      ENDIF

!     -- SCALAIRE :
!     -------------
      TYPSC1=TYPSCA
      CALL ASSERT(TYPSC1.EQ.'R'.OR.TYPSC1.EQ.'I'.OR.TYPSC1.EQ.'C')
      NBV4=NBV

!     -- CHOIX OPERATION MPI  :
!     ---------------------------
      IF (OPTMPI.EQ.'MPI_MAX') THEN
        LOPMPI=MPI_MAX
        IF (TYPSC1.EQ.'C') CALL ASSERT(.FALSE.)
      ELSE IF (OPTMPI.EQ.'MPI_MIN') THEN
        LOPMPI=MPI_MIN
        IF (TYPSC1.EQ.'C') CALL ASSERT(.FALSE.)
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
            DO 1 K=1,NBV
              VR2(K)=VR(K)
 1          CONTINUE
          ELSE IF (TYPSC1.EQ.'I') THEN
            DO 2 K=1,NBV
              VI2(K)=VI(K)
 2          CONTINUE
          ELSE IF (TYPSC1.EQ.'C') THEN
            DO 3 K=1,NBV
              VC2(K)=VC(K)
 3          CONTINUE
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        ELSE
          IF (TYPSC1.EQ.'R') THEN
            CALL WKVECT('&&MPICM1.TRAV','V V R',NBV,JTRAV)
            DO 6 K=1,NBV
              ZR(JTRAV-1+K)=VR(K)
 6          CONTINUE
          ELSEIF (TYPSC1.EQ.'I') THEN
            CALL WKVECT('&&MPICM1.TRAV','V V I',NBV,JTRAV)
            DO 7 K=1,NBV
              ZI(JTRAV-1+K)=VI(K)
 7          CONTINUE
          ELSEIF (TYPSC1.EQ.'C') THEN
            CALL WKVECT('&&MPICM1.TRAV','V V C',NBV,JTRAV)
            DO 8 K=1,NBV
              ZC(JTRAV-1+K)=VC(K)
 8          CONTINUE
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        ENDIF
      ENDIF


      IF (OPTMPI.EQ.'BCAST') THEN
!     ---------------------------------
        IF (TYPSC1.EQ.'R') THEN
          CALL MPI_BCAST(VR,NBV4,LR8 ,BCRANK,MPICOU,IERMPI)
        ELSE IF(TYPSC1.EQ.'I') THEN
          CALL MPI_BCAST(VI,NBV4,LINT,BCRANK,MPICOU,IERMPI)
        ELSE IF(TYPSC1.EQ.'C') THEN
          CALL MPI_BCAST(VC,NBV4,LC8,BCRANK,MPICOU,IERMPI)
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        CALL MPIERR(IERMPI)

      ELSE IF (OPTMPI.EQ.'BCASTP') THEN
!     ---------------------------------
C       --- ON COMMUNIQUE EN SOUS-PAQUETS DE TAILLE =< SIZBMPI 
C       --- POUR EVITER LES PBS DE CONTENTIONS MEMOIRE ET LES LIMITES
C       --- DE REPRESENTATIONS DES ENTIERS DS LA BIBLI MPI.
        NBCAST=NBV/SIZBMPI
        IMAIN=NBCAST*SIZBMPI
        IREST=NBV-IMAIN
C       IF (NBCAST.GT.0)
C     &    WRITE(6,*)'<MPICM1 + BCASTP> TYPSC1/NBV/NBCAST/SIZBMPI: ',
C     &    TYPSC1,NBV,NBCAST,SIZBMPI
        IF (TYPSC1.EQ.'R') THEN
          NBV4=SIZBMPI
          DO 11 K=1,NBCAST
            CALL MPI_BCAST(VR(1+(K-1)*SIZBMPI),NBV4,LR8,BCRANK,MPICOU,
     &                     IERMPI)
 11       CONTINUE
          NBV4=IREST
          IF (IREST.NE.0)
     &      CALL MPI_BCAST(VR(1+IMAIN),NBV4,LR8,BCRANK,MPICOU,IERMPI)

        ELSE IF(TYPSC1.EQ.'I') THEN
          NBV4=SIZBMPI
          DO 12 K=1,NBCAST
            CALL MPI_BCAST(VI(1+(K-1)*SIZBMPI),NBV4,LINT,BCRANK,MPICOU,
     &                     IERMPI)
 12       CONTINUE
          NBV4=IREST
          IF (IREST.NE.0)
     &      CALL MPI_BCAST(VI(1+IMAIN),NBV4,LINT,BCRANK,MPICOU,IERMPI)

        ELSE IF(TYPSC1.EQ.'C') THEN
          NBV4=SIZBMPI
          DO 13 K=1,NBCAST
            CALL MPI_BCAST(VC(1+(K-1)*SIZBMPI),NBV4,LC8,BCRANK,MPICOU,
     &                     IERMPI)
 13       CONTINUE
          NBV4=IREST
          IF (IREST.NE.0)
     &      CALL MPI_BCAST(VC(1+IMAIN),NBV4,LC8,BCRANK,MPICOU,IERMPI)

        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        CALL MPIERR(IERMPI)

      ELSE IF (OPTMPI.EQ.'REDUCE') THEN
!     ---------------------------------
        IF (TYPSC1.EQ.'R') THEN
          IF (NBV.LE.1000) THEN
            CALL MPI_REDUCE(VR2,      VR,NBV4,LR8,LOPMPI,
     &                      0,MPICOU,IERMPI)
          ELSE
            CALL MPI_REDUCE(ZR(JTRAV),VR,NBV4,LR8,LOPMPI,
     &                      0,MPICOU,IERMPI)
          ENDIF

        ELSE IF(TYPSC1.EQ.'I') THEN
          IF (NBV.LE.1000) THEN
            CALL MPI_REDUCE(VI2      ,VI,NBV4,LINT,LOPMPI,
     &                      0,MPICOU,IERMPI)
          ELSE
            CALL MPI_REDUCE(ZI(JTRAV),VI,NBV4,LINT,LOPMPI,
     &                      0,MPICOU,IERMPI)
          ENDIF
        ELSE IF(TYPSC1.EQ.'C') THEN
          IF (NBV.LE.1000) THEN
            CALL MPI_REDUCE(VC2      ,VC,NBV4,LC8,LOPMPI,
     &                      0,MPICOU,IERMPI)
          ELSE
            CALL MPI_REDUCE(ZC(JTRAV),VC,NBV4,LC8,LOPMPI,
     &                      0,MPICOU,IERMPI)
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
     &                         MPICOU,IERMPI)
          ELSE
            CALL MPI_ALLREDUCE(ZR(JTRAV),VR,NBV4,LR8,LOPMPI,
     &                         MPICOU,IERMPI)
          ENDIF
        ELSEIF (TYPSC1.EQ.'I') THEN
          IF (NBV.LE.1000) THEN
            CALL MPI_ALLREDUCE(VI2,      VI,NBV4,LINT,LOPMPI,
     &                         MPICOU,IERMPI)
          ELSE
            CALL MPI_ALLREDUCE(ZI(JTRAV),VI,NBV4,LINT,LOPMPI,
     &                         MPICOU,IERMPI)
          ENDIF
        ELSEIF (TYPSC1.EQ.'C') THEN
          IF (NBV.LE.1000) THEN
            CALL MPI_ALLREDUCE(VC2,      VC,NBV4,LC8,LOPMPI,
     &                         MPICOU,IERMPI)
          ELSE
            CALL MPI_ALLREDUCE(ZC(JTRAV),VC,NBV4,LC8,LOPMPI,
     &                         MPICOU,IERMPI)
          ENDIF
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        CALL MPIERR(IERMPI)

      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      IF (OPTMPI.NE.'BCAST'.AND.NBV.GT.1000)
     &  CALL JEDETR('&&MPICM1.TRAV')

 9999 CONTINUE
! --- COMPTEUR
      CALL UTTCPU('CPU.CMPI.1','FIN',' ')
      CALL JEDEMA()
#endif
      END
