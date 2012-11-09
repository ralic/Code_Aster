      SUBROUTINE DYLEMA (BASENO, NBMAT, NOMAT, RAIDE, MASSE, AMOR, IMPE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C
C
C ======================================================================
C ----------------------------------------------------------------------
C
C DYNAMIQUE: LECTURE DES MATRICES ASSEMBLEES EN ENTREE DE DYNA_LINE_HARM
C **         **          *        *
C
C ----------------------------------------------------------------------
C
C      IN BASENO : BASE DU NOM DES STRUCTURES
C      OUT NBMAT : NOMBRE DE MATRICES 2, 3 OU 4
C      OUT NOMAT : TABLEAU DES NOMS UTILISATEUR DES MATRICES
C      OUT RAIDE : NOM DE LA MATRICE DE RAIDEUR
C      OUT MASSE : NOM DE LA MATRICE DE MASSE
C      OUT AMOR  : NOM DE LA MATRICE D AMORTISSEMENT
C      OUT IMPE  : NOM DE LA MATRICE D IMPEDANCE
C
C ----------------------------------------------------------------------
C
C
      IMPLICIT NONE
C
C 0.1. ==> ARGUMENTS
C
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      INTEGER NBMAT
      CHARACTER*8   BASENO
      CHARACTER*19  MASSE, RAIDE, AMOR, IMPE
      CHARACTER*24  NOMAT(*)
C
C 0.2. ==> COMMUNS
C
C
C 0.3. ==> VARIABLES LOCALES
C
      INTEGER IBID
      REAL*8 R8BID
      CHARACTER*8 K8BID
      CHARACTER*16 K16BID, NOMCMD

      CHARACTER*1 BL
      INTEGER N1, N2, LAMOR, LIMPE
      INTEGER IAMOG, JAMOG, JAMO2, IATMAT, IATMAR, IATMAM
      INTEGER N, NBMODE, NBMOD2, NEQ, NBBLOC, LGBLOC, IDIFF
      INTEGER NBAMOR, I2, NTERM
      INTEGER IAM, IBLOC, I

      INTEGER       LMAT(4)
      INTEGER VALI(3)

      REAL*8 ACRIT

      CHARACTER*1   KTYP
      CHARACTER*8   LISTAM
      CHARACTER*16  TYPOBJ
      CHARACTER*19  MATIR,MATIM,MATIA,AMORT
      CHARACTER*24  VALER,VALEM,VALEA
      CHARACTER*24 VALK(2)
      LOGICAL       CPX
      INTEGER      IARG

C ----------------------------------------------------------------------
C

C===============
C 1. PREALABLES
C===============
C
      CALL JEMARQ()
C
C====
C 1.1  ==> INITIALISATIONS DIVERSES
C====
      BL = ' '
      CPX=.FALSE.

C====
C 1.2 ==> DONNEES,  RECUPERATION D OPERANDES
C====

      CALL GETRES(K8BID,K16BID,NOMCMD)

C
C  1.2.1    --- NOM DES MATRICES
      CALL GETVID(BL,'MATR_MASS'    ,0,IARG,1,MASSE,N1)
      CALL GETVID(BL,'MATR_RIGI'    ,0,IARG,1,RAIDE,N1)
      CALL GETVID(BL,'MATR_AMOR'    ,0,IARG,1,AMOR ,LAMOR)
      CALL GETVID(BL,'MATR_IMPE_PHI',0,IARG,1,IMPE ,LIMPE)

C
C===============
C  2. RECUPERATION DES DESCRIPTEURS DES MATRICES M, K , C
C===============
      CALL MTDSCR(RAIDE)
      NOMAT(1)=RAIDE(1:19)//'.&INT'
      CALL JEVEUO(NOMAT(1),'L',LMAT(1))
C ON TESTE SI LA MATRICE DE RAIDEUR EST COMPLEXE
      CALL JELIRA(RAIDE//'.VALM','TYPE',IBID,KTYP)
      IF (KTYP.EQ.'C') THEN
        CPX=.TRUE.
      ENDIF
      CALL MTDSCR(MASSE)
      NOMAT(2)=MASSE(1:19)//'.&INT'
      CALL JEVEUO(NOMAT(2),'L',LMAT(2))
C
      NBMAT = 2
      IF (LAMOR.NE.0) THEN
         NBMAT = NBMAT + 1
         CALL MTDSCR(AMOR)
         NOMAT(NBMAT)=AMOR(1:19)//'.&INT'
         CALL JEVEUO(NOMAT(NBMAT),'L',LMAT(NBMAT))
      ENDIF
      NEQ = ZI(LMAT(1)+2)
C
C===============
C  3. RECUPERATION DE L AMORTISSEMENT
C===============
C
      CALL GETVR8('AMOR_MODAL','AMOR_REDUIT',1,IARG,0,R8BID,N1)
      CALL GETVID('AMOR_MODAL','LIST_AMOR',1,IARG,0,K8BID,N2)
      IF (N1.NE.0.OR.N2.NE.0) THEN
         CALL GETTCO(RAIDE,TYPOBJ)
         IF (TYPOBJ(1:14).NE.'MATR_ASSE_GENE') THEN
            VALK (1) = TYPOBJ
            CALL U2MESG('F','ALGORITH15_95',1,VALK,0,0,0,0.D0)
         ENDIF
         NBMODE = NEQ
         NBMOD2 = NEQ*(NEQ+1)/2
         NBMAT = NBMAT+1
         AMORT = BASENO//'.AMORT_MATR'
         CALL MTDEFS(AMORT,MASSE,'V','R')
         CALL MTDSCR(AMORT)
         NOMAT(NBMAT)=AMORT(1:19)//'.&INT'
         CALL JEVEUO(NOMAT(NBMAT),'L',LMAT(NBMAT))
C
         NBBLOC=1
         LGBLOC=ZI(LMAT(NBMAT)+14)
C
         IF (N1.NE.0) THEN
            NBAMOR = -N1
         ELSE
            CALL GETVID('AMOR_MODAL','LIST_AMOR',1,IARG,1,LISTAM,N)
            CALL JELIRA(LISTAM//'           .VALE',
     &                                   'LONMAX',NBAMOR,K8BID)


         ENDIF
         IF (NBAMOR.GT.NBMODE) THEN
C
            VALI (1) = NBMODE
            VALI (2) = NBAMOR
            VALI (3) = NBMODE
            CALL U2MESG('A','ALGORITH15_96',0,' ',3,VALI,0,0.D0)
            CALL WKVECT(BASENO//'.AMORTI','V V R8',NBMODE,JAMOG)
            IF (N1.NE.0) THEN
             CALL GETVR8('AMOR_MODAL','AMOR_REDUIT',1,IARG,NBMODE,
     &                   ZR(JAMOG),N)
            ELSE
             CALL JEVEUO(LISTAM//'           .VALE','L',IAMOG)
             DO 201 IAM = 1,NBMODE
                ZR(JAMOG+IAM-1) = ZR(IAMOG+IAM-1)
 201         CONTINUE
            ENDIF
         ELSEIF (NBAMOR.LT.NBMODE) THEN
C
            CALL WKVECT(BASENO//'.AMORTI','V V R8',NBAMOR,JAMOG)
            IF (N1.NE.0) THEN
               CALL GETVR8('AMOR_MODAL','AMOR_REDUIT',1,IARG,NBAMOR,
     &                     ZR(JAMOG),N)
            ELSE
               CALL JEVEUO(LISTAM//'           .VALE','L',IAMOG)
               DO 210 IAM = 1,NBAMOR
                ZR(JAMOG+IAM-1) = ZR(IAMOG+IAM-1)
 210           CONTINUE
            ENDIF
            IDIFF = NBMODE - NBAMOR
            VALI (1) = IDIFF
            VALI (2) = NBMODE
            VALI (3) = IDIFF
            CALL U2MESG('I','ALGORITH15_97',0,' ',3,VALI,0,0.D0)
            CALL WKVECT(BASENO//'.AMORTI2','V V R8',NBMODE,JAMO2)
            DO 20 IAM = 1,NBAMOR
               ZR(JAMO2+IAM-1) = ZR(JAMOG+IAM-1)
 20         CONTINUE
            DO 22 IAM = NBAMOR+1,NBMODE
               ZR(JAMO2+IAM-1) = ZR(JAMOG+NBAMOR-1)
 22         CONTINUE
            JAMOG = JAMO2
         ELSEIF (NBAMOR.EQ.NBMODE) THEN
            CALL WKVECT(BASENO//'.AMORTI','V V R8',NBAMOR,JAMOG)
            IF (N1.NE.0) THEN
               CALL GETVR8('AMOR_MODAL','AMOR_REDUIT',1,IARG,NBAMOR,
     &                     ZR(JAMOG),N)
            ELSE
               CALL JEVEUO(LISTAM//'           .VALE','L',IAMOG)
               DO 220 IAM = 1,NBAMOR
                ZR(JAMOG+IAM-1) = ZR(IAMOG+IAM-1)
 220           CONTINUE
            ENDIF
         ENDIF
C
         DO 230 IBLOC=1,NBBLOC
C
            MATIR=ZK24(ZI(LMAT(1)+1))(1:19)
            VALER=MATIR//'.VALM'
            CALL JEVEUO( JEXNUM(VALER,IBLOC),'L',IATMAR)
            CALL JELIRA(JEXNUM(VALER,IBLOC),'LONMAX',NTERM,K8BID)
C
            MATIM=ZK24(ZI(LMAT(2)+1))(1:19)
            VALEM=MATIM//'.VALM'
            CALL JEVEUO( JEXNUM(VALEM,IBLOC),'L',IATMAM)
            MATIA=ZK24(ZI(LMAT(NBMAT)+1))(1:19)
            VALEA=MATIA//'.VALM'
            CALL JEVEUO( JEXNUM(VALEA,IBLOC),'E',IATMAT)
            DO 14 I=1,NBMODE
              IF (LGBLOC.EQ.NBMODE) THEN
               IF (CPX) THEN
                 ACRIT = 2.D0*SQRT(ABS(ZC(IATMAR-1+I)*ZR(IATMAM-1+I)))
               ELSE
                 ACRIT = 2.D0*SQRT(ABS(ZR(IATMAR-1+I)*ZR(IATMAM-1+I)))
               ENDIF
               ZR(IATMAT-1+I) = ZR(JAMOG-1+I)*ACRIT
              ELSEIF (LGBLOC.EQ.NBMOD2) THEN
               I2 = I*(I+1)/2
               IF (CPX) THEN
                 ACRIT = 2.D0*SQRT(ABS(ZC(IATMAR-1+I2)*
     &                   ZR(IATMAM-1+I2)))
               ELSE
                 ACRIT = 2.D0*SQRT(ABS(ZR(IATMAR-1+I2)*
     &                   ZR(IATMAM-1+I2)))
               ENDIF
               ZR(IATMAT-1+I2) = ZR(JAMOG-1+I)*ACRIT
              ENDIF
 14         CONTINUE
 230     CONTINUE
C
      ENDIF
C
C===============
C  4. RECUPERATION DE L "IMPEDANCE"
C===============

      IF (LIMPE.NE.0) THEN
         NBMAT = NBMAT + 1
         CALL MTDSCR(IMPE)
         NOMAT(NBMAT)=IMPE(1:19)//'.&INT'
         CALL JEVEUO(NOMAT(NBMAT),'L',LMAT(NBMAT))
      ENDIF

C FIN ------------------------------------------------------------------
      CALL JEDEMA()

      END
