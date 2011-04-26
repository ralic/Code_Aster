      SUBROUTINE IRMAD1 ( IFI,VERSIO,NBNO,PRNO,NUEQ,NEC,DG,NCMPMX,
     &            ITYPE,NSTAT,CHAMNO,NOMCMP,NOMSYM,NUMNOE)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER       IFI,NBNO,PRNO(*),NUEQ(*),NEC,DG(*),NCMPMX,NUMNOE(*)
      INTEGER       VERSIO, ITYPE, NSTAT
      CHARACTER*(*) NOMCMP(*),NOMSYM,CHAMNO(*)
C--------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C        ECRITURE D'UN CHAM_NO SUR FICHIER UNIVERSEL, DATASET TYPE 252
C        A VALEURS REELLES OU COMPLEXES
C--------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C     ------------------------------------------------------------------
      CHARACTER*4  NOMGDS(50), NOMCHS(50)
      CHARACTER*19 CHAMN
      INTEGER      NBCHS
      INTEGER      NBCMPS(50), IPCMPS(50,50), IMPRE
      LOGICAL      LTABL(50), AFAIRE, EXISDG
C
C  --- INITIALISATIONS ----
C
      CALL JEMARQ()
      DO 10 I = 1 , NCMPMX
         LTABL(I) = .FALSE.
 10   CONTINUE
C
      NBCMPT = 0
      DO 100 INNO = 1,NBNO
         INO = NUMNOE(INNO)
         DO 110 IEC=1,NEC
            DG(IEC)=PRNO((INO-1)*(NEC+2)+2+IEC)
 110     CONTINUE
         NCMP = PRNO((INO-1)* (NEC+2)+2)
         IF (NCMP.EQ.0) GOTO 100
         ICOMPT = 0
         DO 112 ICMP = 1,NCMPMX
           IF (EXISDG(DG,ICMP)) ICOMPT = ICOMPT + 1
 112     CONTINUE
         NBCMPT = MAX( NBCMPT , ICOMPT )
 100  CONTINUE
      NROW = NBCMPT
      NCOL = NBNO * NSTAT
      NDIM = NCOL * NROW
C
C --- ALLOCATION DES TABLEAUX DE TRAVAIL ---
C
      CALL ASSERT((ITYPE.EQ.1).OR.(ITYPE.EQ.2))
      IF ( ITYPE .EQ. 1 ) THEN
         CALL WKVECT('&&IRMAD1.VAL','V V R',NDIM,IRVAL)
      ELSEIF ( ITYPE .EQ. 2 ) THEN
         CALL WKVECT('&&IRMAD1.VAL','V V C',NDIM,IRVAL)
      ENDIF
C
C ---- RECHERCHE DES GRANDEURS SUPERTAB -----
C
      CALL IRGAGS(NCMPMX,NOMCMP,NOMSYM,NBCHS,NOMCHS,NBCMPS,NOMGDS,
     &               IPCMPS)
      DO 777 ICHS=1,50
      DO 778 IST=1,50
        IPCMPS(ICHS,IST)=-1
 778  CONTINUE
 777  CONTINUE

C ---- BOUCLE SUR LES DIVERSES GRANDEURS SUPERTAB ----
      IMPRE = 0
      DO 20 ICHS = 1 , NBCHS
         IF ( ICHS .GT. 1 ) THEN
            AFAIRE = .FALSE.
            DO 22 ICP = 1 , NBCMPS(ICHS)
               AFAIRE = (AFAIRE.OR.LTABL(IPCMPS(ICHS,ICP)))
 22         CONTINUE
            IF( .NOT. AFAIRE ) GOTO 20
         ENDIF
         IMPRE = IMPRE + 1
         DO 30 IST = 1 , NSTAT
           CHAMN = CHAMNO(IST)
           CALL JEVEUO(CHAMN//'.VALE','L',IAVALE)
           DO 40 INNO = 1,NBNO
             INO = NUMNOE(INNO)
             DO 42 IEC = 1,NEC
                DG(IEC) = PRNO((INO-1)*(NEC+2)+2+IEC)
 42          CONTINUE
             IVAL = PRNO((INO-1)* (NEC+2)+1)
             NCMP = PRNO((INO-1)* (NEC+2)+2)
             IF ( NCMP .EQ. 0 ) GOTO 40
             ICOMPT = 0
             DO 44 ICMP = 1,NCMPMX
               IF ( EXISDG(DG,ICMP) ) THEN
                 IF( ICHS .EQ. 1 ) LTABL(ICMP)= .TRUE.
                 ICOMPT = ICOMPT + 1
                 K1 = NUEQ(IVAL-1+ICOMPT)
                 DO 46 ICMS = 1 , NBCMPS(ICHS)
                   ICMSUP = IPCMPS(ICHS,ICMS)
                   IF (ICMP.EQ.ICMSUP) THEN
                     K2 = ICMS + (INNO-1)*NBCMPT + (IST-1)*NBCMPT*NBNO
                     IF ( ITYPE .EQ. 1 ) THEN
                       ZR(IRVAL-1+K2) = ZR(IAVALE-1+K1)
                     ELSE
                       ZC(IRVAL-1+K2) = ZC(IAVALE-1+K1)
                     ENDIF
                     GOTO 44
                   ENDIF
 46              CONTINUE
               ENDIF
 44          CONTINUE
 40        CONTINUE
 30      CONTINUE
 20   CONTINUE
C
      CALL ASSERT ( IMPRE .LE. 1 )
C
      IF ( VERSIO .EQ. 5  .AND.  IMPRE .EQ. 1 ) THEN
         IMAT = 147
         IF ( ITYPE .EQ. 1 ) THEN
            MTYP = 4
         ELSE
            MTYP = 6
         ENDIF
         MFOR = 3
         MKEY = 2
         WRITE (IFI,'(A)') '    -1'
         WRITE (IFI,'(A)') '   252'
         WRITE (IFI,'(I10)') IMAT
         WRITE (IFI,'(5I10)') MTYP, MFOR, NROW, NCOL, MKEY
         IF ( ITYPE .EQ. 1 ) THEN
            WRITE (IFI ,'(1P,4D20.12)') ( ZR(IRVAL+I) , I=0,NDIM-1 )
         ELSE
            WRITE (IFI ,'(1P,2(2D20.12))') ( ZC(IRVAL+I) , I=0,NDIM-1 )
         ENDIF
         MFOR = 3
         WRITE (IFI,'(A)') '    -1'
      ENDIF
C
      CALL JEDETR('&&IRMAD1.VAL')
      CALL JEDEMA()
      END
