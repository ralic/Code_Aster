      SUBROUTINE AMUMPP(OPTION,NBSOL ,KXMPS ,LDIST ,TYPE  ,IMPR  ,
     &                  IFMUMP,ELI2LG,RSOLU ,CSOLU ,VCINE ,PREPOS,
     &                  LPRECO,LMIXP)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF AMUMPP MUMPS  DATE 20/06/2012   AUTEUR BOITEAU O.BOITEAU 
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
C-----------------------------------------------------------------------
C BUT : ROUTINE DE PRE/POST-TRAITEMENT DE LA SOLUTION ET DU
C       SECOND MEMBRE POUR AMUMPS/C/D/Z
C
C IN  OPTION :   IN   : OPTION D'UTILISATION.
C IN  NBSOL  :   IN   : NBRE DE SYSTEMES A RESOUDRE
C IN  KXMPS  :   IN   : INDICE DE L'INSTANCE MUMPS DANS DMPS
C IN  LDIST  :  LOG   : LOGICAL MUMPS DISTRIBUE OR NOT
C IN  TYPE   :   K1   : TYPE DU POINTEUR R OU C
C IN  IMPR   :  K14   : FLAG POUR IMPRESSION MATRICE
C IN  IFMUMP :   IN   : UNITE LOGIQUE POUR IMPRESSION FICHIER
C IN  ELI2LG :  LOG   : LOGICAL POUR NE LAISSER QU'1 LAGRANGE ACTIF
C I/O RSOLU  :    R   : EN ENTREE : SECONDS MEMBRES REELS
C                     : EN SORTIE : SOLUTIONS
C I/O CSOLU  :    C   : EN ENTREE : SECONDS MEMBRES COMPLEXES
C                     : EN SORTIE : SOLUTIONS
C IN  VCINE  :  K19   : NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
C IN  PREPOS :  LOG   : SI .TRUE. ON FAIT LES PRE ET POSTTRAITEMENTS DE
C           MISE A L'ECHELLE DU RHS ET DE LA SOLUTION (MRCONL) ET DE LA
C           PRISE EN COMPTE DES AFFE_CHAR_CINE (CSMBGG).
C           SI .FALSE. ON NE LES FAIT PAS (PAR EXEMPLE EN MODAL).
C IN  LPRECO :  LOG   : MUMPS EST-IL UTILISE COMME PRECONDITIONNEUR ?
C IN  LMIXP  :  LOG   : MUMPS EST-IL UTILISE COMME SOLVEUR DIRECT SIMPLE
C                       PRECISION (DANS UN PROCESSUS NON LINEAIRE) ?
C-----------------------------------------------------------------------
! RESPONSABLE BOITEAU O.BOITEAU

      INTEGER      OPTION,NBSOL,KXMPS,IFMUMP
      LOGICAL      LDIST,ELI2LG,PREPOS,LPRECO,LMIXP
      CHARACTER*1  TYPE
      CHARACTER*14 IMPR
      CHARACTER*19 VCINE
      REAL*8       RSOLU(*)
      COMPLEX*16   CSOLU(*)

#ifdef _HAVE_MUMPS

C============================================================
      INCLUDE 'mpif.h'
      INCLUDE 'smumps_struc.h'
      INCLUDE 'cmumps_struc.h'
      INCLUDE 'dmumps_struc.h'
      INCLUDE 'zmumps_struc.h'
      INCLUDE 'jeveux.h'
C============================================================
      INTEGER    NICNTL
      PARAMETER (NICNTL=26)
      INTEGER    NMXINS
      PARAMETER (NMXINS=5)
      TYPE (SMUMPS_STRUC) , TARGET  :: SMPS(NMXINS)
      TYPE (CMUMPS_STRUC) , TARGET  :: CMPS(NMXINS)
      TYPE (SMUMPS_STRUC) , POINTER :: SMPSK
      TYPE (CMUMPS_STRUC) , POINTER :: CMPSK
      TYPE (DMUMPS_STRUC) , TARGET  :: DMPS(NMXINS)
      TYPE (ZMUMPS_STRUC) , TARGET  :: ZMPS(NMXINS)
      TYPE (DMUMPS_STRUC) , POINTER :: DMPSK
      TYPE (ZMUMPS_STRUC) , POINTER :: ZMPSK
      INTEGER      N,NNBSOL,RANG,LMAT,I,IERD,IDVALC,IBID,
     &             JDELG,K,IFM,NIV,JDLG2,JJ
      INTEGER*4    N4
      CHARACTER*1  ROUCS(NMXINS),ROUC,PRECS(NMXINS)
      CHARACTER*4  ETAMS(NMXINS),ETAM
      CHARACTER*8  KBID
      CHARACTER*14 NONUS(NMXINS),NONU
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS),NOMAT,NOSOLV
      CHARACTER*24 VCIVAL
      LOGICAL      LTYPR,LAPX
      REAL*8       RBID,RR4MAX,RAUX,R4MAEM,R4MIEM,R8MAEM,R8MIEM,
     &             RMIN,RMAX,RTEST,VALR(2)
      COMPLEX*16   CBID,CAUX
      COMMON /MUMPSS/ SMPS
      COMMON /MUMPSC/ CMPS
      COMMON /MUMPSD/ DMPS
      COMMON /MUMPSZ/ ZMPS
      COMMON /MUMPSH/ NONUS,NOMATS,NOSOLS,ETAMS,ROUCS,PRECS
      
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)

C     ------------------------------------------------
C     INITS
C     ------------------------------------------------
      LAPX=LPRECO.OR.LMIXP
      RR4MAX=R4MAEM()
      IF (TYPE.EQ.'S') THEN
        SMPSK=>SMPS(KXMPS)
        RANG=SMPSK%MYID
        N=SMPSK%N
        SMPSK%NRHS=NBSOL
        SMPSK%LRHS=N
        LTYPR=.TRUE.
        RMAX=R4MAEM()*0.5
        RMIN=R4MIEM()*2.0
      ELSE IF (TYPE.EQ.'C') THEN
        CMPSK=>CMPS(KXMPS)
        RANG=CMPSK%MYID
        N=CMPSK%N
        CMPSK%NRHS=NBSOL
        CMPSK%LRHS=N
        LTYPR=.FALSE.
        RMAX=R4MAEM()*0.5
        RMIN=R4MIEM()*2.0
      ELSE IF (TYPE.EQ.'D') THEN
        DMPSK=>DMPS(KXMPS)
        RANG=DMPSK%MYID
        N=DMPSK%N
        DMPSK%NRHS=NBSOL
        DMPSK%LRHS=N
        LTYPR=.TRUE.
        RMAX=R8MAEM()*0.5
        RMIN=R8MIEM()*2.0
      ELSE IF (TYPE.EQ.'Z') THEN
        ZMPSK=>ZMPS(KXMPS)
        RANG=ZMPSK%MYID
        N=ZMPSK%N
        ZMPSK%NRHS=NBSOL
        ZMPSK%LRHS=N
        LTYPR=.FALSE.
        RMAX=R8MAEM()*0.5
        RMIN=R8MIEM()*2.0
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
      NNBSOL=N*NBSOL
      N4=NNBSOL
      NOMAT=NOMATS(KXMPS)
      NOSOLV=NOSOLS(KXMPS)
      NONU=NONUS(KXMPS)
      ETAM=ETAMS(KXMPS)
      
      VCIVAL=VCINE//'.VALE'
      CALL MTDSCR(NOMAT)
      CALL JEVEUO(NOMAT//'.&INT','E',LMAT)


      IF (OPTION.EQ.0) THEN

C       ------------------------------------------------
C        PRETRAITEMENTS ASTER DU/DES SECONDS MEMBRES :
C       ------------------------------------------------

        IF (((RANG.EQ.0).AND.(.NOT.LDIST)).OR.(LDIST)) THEN
          IF (RANG.EQ.0) THEN
            IF (TYPE.EQ.'S') THEN
              ALLOCATE(SMPSK%RHS(NNBSOL))
            ELSE IF (TYPE.EQ.'C') THEN
              ALLOCATE(CMPSK%RHS(NNBSOL))
            ELSE IF (TYPE.EQ.'D') THEN
              ALLOCATE(DMPSK%RHS(NNBSOL))
            ELSE IF (TYPE.EQ.'Z') THEN
              ALLOCATE(ZMPSK%RHS(NNBSOL))
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF

C         --- PRISE EN COMPTE DU CONDITIONNEMENT DES LAGRANGES :
C         ON NE LE FAIT PAS SI MUMPS EST UTILISE COMME 
C         PRECONDITIONNEUR (DEJA FAIT DANS RESGRA.F)
          IF ((RANG.EQ.0).AND.(.NOT.LPRECO).AND.PREPOS) THEN
             IF (LTYPR) THEN
               CALL MRCONL('MULT',LMAT,N,'R',RSOLU,NBSOL)
             ELSE
               CALL MCCONL('MULT',LMAT,N,'C',CSOLU,NBSOL)
             ENDIF
          ENDIF

C         --- PRISE EN COMPTE DES CHARGES CINEMATIQUES :
          CALL JEEXIN(VCIVAL,IERD)
          IF ((VCINE.NE.' ').AND.(IERD.NE.0).AND.PREPOS) THEN
            IF (LDIST) THEN
C             -- POUR NE PAS CUMULER LE CHARGEMENT INITIAL:
              IF (RANG.NE.0) THEN
                DO I=1,NNBSOL
                  IF (LTYPR) THEN
                    RSOLU(I)=0.D0
                  ELSE
                    CSOLU(I)=DCMPLX(0.D0,0.D0)
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
            CALL JEVEUO(VCIVAL,'L',IDVALC)
            CALL JELIRA(VCIVAL,'TYPE',IBID,ROUC)
            IF (LTYPR) THEN
              CALL ASSERT(ROUC.EQ.'R')
              DO I=1,NBSOL
                CALL CSMBGG(LMAT,RSOLU(N*(I-1)+1),ZR(IDVALC),
     &                    CBID,CBID,'R')
              ENDDO
            ELSE
              CALL ASSERT(ROUC.EQ.'C')
              DO I=1,NBSOL
                CALL CSMBGG(LMAT,RBID,RBID,CSOLU(N*(I-1)+1),
     &                    ZC(IDVALC),'C')
              ENDDO
            ENDIF
          ENDIF

C         -- REDUCE DU SECOND MEMBRE AU PROC MAITRE
          IF (LDIST.AND.PREPOS) THEN
            IF (LTYPR) THEN
              CALL MPICM1('REDUCE','R',NNBSOL,IBID,RSOLU)
            ELSE
              CALL MPICM1('REDUCE','R',2*NNBSOL,IBID,CSOLU)
            ENDIF
          ENDIF

          
C         -- COPIE DE SOLU DANS %RHS:
          IF (RANG.EQ.0) THEN
            IF (TYPE.EQ.'S') THEN
              DO I=1,NNBSOL
                RAUX=RSOLU(I)
                RTEST=ABS(RAUX)
                IF (RTEST.LT.RMIN) THEN
                  RAUX=0.D0
                ELSE IF (RTEST.GT.RMAX) THEN
                  IF (LAPX) THEN
                    RAUX=RMAX
                  ELSE
                    VALR(1)=RTEST
                    VALR(2)=RMAX
                    CALL U2MESG('F','FACTOR_79',0,KBID,1,I,2,VALR)
                  ENDIF
                ENDIF
                SMPSK%RHS(I)=RAUX
              ENDDO
            ELSE IF (TYPE.EQ.'C') THEN
              DO I=1,NNBSOL
                CAUX=CSOLU(I)
                RTEST=ABS(CAUX)
                IF (RTEST.LT.RMIN) THEN
                  CAUX=DCMPLX(0.D0,0.D0)
                ELSE IF (RTEST.GT.RMAX) THEN
                  IF (LAPX) THEN
                    CAUX=DCMPLX(RMAX,0.D0)
                  ELSE
                    VALR(1)=RTEST
                    VALR(2)=RMAX
                    CALL U2MESG('F','FACTOR_79',0,KBID,1,I,2,VALR)
                  ENDIF
                ENDIF
                CMPSK%RHS(I)=CAUX
              ENDDO
            ELSE IF (TYPE.EQ.'D') THEN
              DO I=1,NNBSOL
                RAUX=RSOLU(I)
                RTEST=ABS(RAUX)
                IF (RTEST.LT.RMIN) THEN
                  RAUX=0.D0
                ELSE IF (RTEST.GT.RMAX) THEN
                  IF (LAPX) THEN
                    RAUX=RMAX
                  ELSE
                    VALR(1)=RTEST
                    VALR(2)=RMAX
                    CALL U2MESG('F','FACTOR_79',0,KBID,1,I,2,VALR)
                  ENDIF
                ENDIF
                DMPSK%RHS(I)=RAUX
              ENDDO
            ELSE IF (TYPE.EQ.'Z') THEN
              DO I=1,NNBSOL
                CAUX=CSOLU(I)
                RTEST=ABS(CAUX)
                IF (RTEST.LT.RMIN) THEN
                  CAUX=DCMPLX(0.D0,0.D0)
                ELSE IF (RTEST.GT.RMAX) THEN
                  IF (LAPX) THEN
                    CAUX=DCMPLX(RMAX,0.D0)
                  ELSE
                    VALR(1)=RTEST
                    VALR(2)=RMAX
                    CALL U2MESG('F','FACTOR_79',0,KBID,1,I,2,VALR)
                  ENDIF
                ENDIF
                ZMPSK%RHS(I)=CAUX
              ENDDO
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF

C         -- IMPRESSION DU/DES SECONDS MEMBRES (SI DEMANDE) :
          IF (IMPR(1:3).EQ.'OUI') THEN
            IF (RANG.EQ.0) THEN
              IF (TYPE.EQ.'S') THEN
                DO K=1,NNBSOL
                  WRITE(IFMUMP,*) K,SMPSK%RHS(K)
                ENDDO
              ELSE IF (TYPE.EQ.'C') THEN
                DO K=1,NNBSOL
                  WRITE(IFMUMP,*) K,CMPSK%RHS(K)
                ENDDO
              ELSE IF (TYPE.EQ.'D') THEN
                DO K=1,NNBSOL
                  WRITE(IFMUMP,*) K,DMPSK%RHS(K)
                ENDDO
              ELSE IF (TYPE.EQ.'Z') THEN
                DO K=1,NNBSOL
                  WRITE(IFMUMP,*) K,ZMPSK%RHS(K)
                ENDDO
              ELSE
                CALL ASSERT(.FALSE.)
              ENDIF
              WRITE(IFMUMP,*) 'MUMPS FIN RHS'
            ENDIF
            IF (IMPR(1:11).EQ.'OUI_NOSOLVE')
     &        CALL U2MESI('F','FACTOR_71',1,IFMUMP)
          ENDIF
        ENDIF


      ELSE IF (OPTION.EQ.2) THEN

        
C       ------------------------------------------------
C        POST-TRAITEMENTS ASTER DE LA SOLUTION :
C       ------------------------------------------------
        IF (RANG.EQ.0) THEN
          IF (TYPE.EQ.'S') THEN
            DO I=1,NNBSOL
              RSOLU(I)=SMPSK%RHS(I)
            ENDDO
            DEALLOCATE(SMPSK%RHS)
          ELSE IF (TYPE.EQ.'C') THEN
            DO I=1,NNBSOL
              CSOLU(I)=CMPSK%RHS(I)
            ENDDO
            DEALLOCATE(CMPSK%RHS)
          ELSE IF (TYPE.EQ.'D') THEN
            CALL DCOPY(N4,DMPSK%RHS,1,RSOLU,1)
            DEALLOCATE(DMPSK%RHS)
          ELSE IF (TYPE.EQ.'Z') THEN
            CALL ZCOPY(N4,ZMPSK%RHS,1,CSOLU,1)
            DEALLOCATE(ZMPSK%RHS)
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF

          IF (ELI2LG) THEN
C           -- PRISE EN COMPTE DES LAGRANGES "2" :
C           -- EN SORTIE DE RESOLUTION AVEC ELIM_LAGR2 ON A :
C           -- LAGR1 = LAGR1 + LAGR2, ON DOIT RECTIFIER CELA :
C           -- LAGR1 = LAGR1/2 PUIS LAGR2 = LAGR1
C           -- VALIDE QUE SUR PROC 0, MAIS C'EST OK CAR ON
C           -- BROADCAST LA SOLUTION APRES
            CALL JEVEUO(NONU//'.NUME.DELG','L',JDELG)
            CALL NUDLG2(NONU)            
            CALL JEVEUO(NONU//'.NUME.DLG2','L',JDLG2)
            IF (LTYPR) THEN
              DO I=1,NBSOL
                DO K=1,N
                  IF (ZI(JDELG-1+K).EQ.-1) THEN
                    RSOLU((I-1)*N+K)= 0.5D0 * RSOLU((I-1)*N+K)
                    JJ = ZI(JDLG2-1+K)
                    RSOLU((I-1)*N+JJ) = RSOLU((I-1)*N+K)
                  ENDIF
                ENDDO
              ENDDO
            ELSE
              DO I=1,NBSOL
                DO K=1,N
                  IF (ZI(JDELG-1+K).EQ.-1) THEN
                    CSOLU((I-1)*N+K) = 0.5D0*CSOLU((I-1)*N+K)
                    JJ = ZI(JDLG2-1+K)
                    CSOLU((I-1)*N+JJ) = CSOLU((I-1)*N+K)
                  ENDIF
                ENDDO
              ENDDO
            ENDIF
          ENDIF

C         --- PRISE EN COMPTE DU CONDITIONNEMENT DES LAGRANGES :
C         ON NE LE FAIT PAS SI MUMPS EST UTILISE COMME 
C         PRECONDITIONNEUR (DEJA FAIT DANS RESGRA.F)
          IF (.NOT.LPRECO.AND.PREPOS) THEN
            IF (LTYPR) THEN
              CALL MRCONL('MULT',LMAT,N,'R',RSOLU,NBSOL)
            ELSE
              CALL MCCONL('MULT',LMAT,N,'C',CSOLU,NBSOL)
            ENDIF
          ENDIF
        ENDIF

C       -- BROADCAST DE SOLU A TOUS LES PROC
        IF (LTYPR) THEN
          CALL MPICM1('BCAST','R',NNBSOL,IBID,RSOLU)
        ELSE
C         -- EST-CE AUSSI SIMPLE POUR LES COMPLEXES ?
          CALL MPICM1('BCAST','R',2*NNBSOL,IBID,CSOLU)
        ENDIF
        
C       -- IMPRESSION DU/DES SOLUTIONS (SI DEMANDE) :
        IF (IMPR(1:9).EQ.'OUI_SOLVE') THEN
          IF (RANG.EQ.0) THEN
            IF (LTYPR) THEN
              DO K=1,NNBSOL
                WRITE(IFMUMP,*) K,RSOLU(K)
              ENDDO
            ELSE
              DO K=1,NNBSOL
                WRITE(IFMUMP,*) K,CSOLU(K)
              ENDDO
            ENDIF
            WRITE(IFMUMP,*) 'MUMPS FIN SOLUTION'
          ENDIF
        ENDIF
      ELSE
C       ------------------------------------------------
C        MAUVAISE OPTION
C       ------------------------------------------------
        CALL ASSERT(.FALSE.)
      ENDIF
      CALL JEDEMA()
#endif
      END
