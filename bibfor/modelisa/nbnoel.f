      SUBROUTINE NBNOEL(CHARZ,NOMAZ,MOTCLE,NGR,CALCMA,INDICE,NBMA,NBNO,
     &                  NBNOQU)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 11/12/2001   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE

      INTEGER NGR,INDICE,NBMA,NBNO,NBNOQU
      CHARACTER*8 CHARZ,NOMAZ,MOTCLE,CALCMA(*)

C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : POINCO/PAMAN2
C ----------------------------------------------------------------------

C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------

      CHARACTER*32 JEXNUM,JEXNOM
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)

C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------

      INTEGER IATYMA,ITYP,NUTYP,JGRO,NBMAIL,NUMAIL
      INTEGER NOEUSO,NOEUMI,N1,II1,II2,NBNOMI
      INTEGER IRET,NEL,IGREL,NBGREL,ITYPEL,IALIEL
      INTEGER IBID,IER,II3,NUMAI2
      CHARACTER*1 K1BID
      CHARACTER*8 NOMTM,NOMA,NOMOB,CHAR
      CHARACTER*16 NOMTE
      CHARACTER*19 NOLIG
      CHARACTER*24 GRMAMA,MAILMA

      CALL JEMARQ()

      CHAR = CHARZ
      NOMA = NOMAZ
      MAILMA = NOMA//'.NOMMAI'
      GRMAMA = NOMA//'.GROUPEMA'
      NBNO = 0
      NBNOQU = 0
      CALL JEVEUO(NOMA//'.TYPMAIL','L',IATYMA)

      IF (MOTCLE.EQ.'GROUP_MA') THEN
        NBMA = 0
        DO 50 II1 = 1,NGR
          CALL JEVEUO(JEXNOM(GRMAMA,CALCMA(II1)),'L',JGRO)
          CALL JELIRA(JEXNOM(GRMAMA,CALCMA(II1)),'LONMAX',NBMAIL,K1BID)
          NBMA = NBMA + NBMAIL
          DO 40 II2 = 1,NBMAIL
            NUMAIL = ZI(JGRO-1+II2)
            ITYP = IATYMA - 1 + NUMAIL
            NUTYP = ZI(ITYP)
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYP),NOMTM)
            CALL JELIRA(JEXNUM(NOMA//'.CONNEX',NUMAIL),'LONMAX',N1,
     &                  K1BID)
            IF (INDICE.EQ.1) THEN
              NOEUMI = 0
              NOEUSO = N1
            ELSE IF (NOMTM(1:5).EQ.'QUAD8' .OR.
     &               NOMTM(1:5).EQ.'TRIA6') THEN
              NOEUMI = NBNOMI(NOMTM(1:5))
              NOEUSO = N1 - NOEUMI
            ELSE IF (NOMTM(1:5).EQ.'QUAD9' .OR.
     &               NOMTM(1:5).EQ.'TRIA7') THEN
              CALL DISMOI('F','NOM_MODELE',CHAR(1:8),'CHARGE',IBID,
     &                    NOMOB,IER)
              NOLIG = NOMOB(1:8)//'.MODELE'
              CALL JEEXIN(NOLIG//'.LIEL',IRET)
              IF (IRET.EQ.0) GO TO 30
              CALL JELIRA(NOLIG//'.LIEL','NUTIOC',NBGREL,K1BID)
              DO 20,IGREL = 1,NBGREL
                CALL JEVEUO(JEXNUM(NOLIG//'.LIEL',IGREL),'L',IALIEL)
                CALL JELIRA(JEXNUM(NOLIG//'.LIEL',IGREL),'LONMAX',NEL,
     &                      K1BID)
                ITYPEL = ZI(IALIEL-1+NEL)
                CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
                IF (NOMTE.EQ.'MEC3QU9H' .OR. NOMTE.EQ.'MEC3TR7H') THEN
                  DO 10 II3 = 1,NEL - 1
                    NUMAI2 = ZI(IALIEL-1+II3)
                    IF (NUMAI2.EQ.NUMAIL) THEN
                      NOEUMI = NBNOMI(NOMTM(1:5)) - 1
                      NOEUSO = N1 - NOEUMI - 1
                      GO TO 30
                    END IF
   10             CONTINUE
                ELSEIF (NOMTE.EQ.'MECA_FACE9') THEN
                  NOEUMI = 0
                  NOEUSO = N1                
                END IF
   20         CONTINUE
   30         CONTINUE
            ELSE
              NOEUMI = 0
              NOEUSO = N1
            END IF
            NBNO = NBNO + NOEUSO
            NBNOQU = NBNOQU + NOEUMI
   40     CONTINUE
   50   CONTINUE
      ELSE IF (MOTCLE.EQ.'MAILLE') THEN
        NBMAIL = NBMA
        DO 90 II1 = 1,NBMAIL
          CALL JENONU(JEXNOM(MAILMA,CALCMA(II1)),NUMAIL)
          CALL JELIRA(JEXNUM(NOMA//'.CONNEX',NUMAIL),'LONMAX',N1,K1BID)
          ITYP = IATYMA - 1 + NUMAIL
          NUTYP = ZI(ITYP)
          CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYP),NOMTM)
          IF (INDICE.EQ.1) THEN
            NOEUMI = 0
            NOEUSO = N1
          ELSE IF (NOMTM(1:5).EQ.'QUAD8' .OR.
     &             NOMTM(1:5).EQ.'TRIA6') THEN
            NOEUMI = NBNOMI(NOMTM(1:5))
            NOEUSO = N1 - NOEUMI
          ELSE IF (NOMTM(1:5).EQ.'QUAD9' .OR.
     &             NOMTM(1:5).EQ.'TRIA7') THEN
            CALL DISMOI('F','NOM_MODELE',CHAR(1:8),'CHARGE',IBID,NOMOB,
     &                  IER)
            NOLIG = NOMOB(1:8)//'.MODELE'
            CALL JEEXIN(NOLIG//'.LIEL',IRET)
            IF (IRET.EQ.0) GO TO 80
            CALL JELIRA(NOLIG//'.LIEL','NUTIOC',NBGREL,K1BID)
            DO 70,IGREL = 1,NBGREL
              CALL JEVEUO(JEXNUM(NOLIG//'.LIEL',IGREL),'L',IALIEL)
              CALL JELIRA(JEXNUM(NOLIG//'.LIEL',IGREL),'LONMAX',NEL,
     &                    K1BID)
              ITYPEL = ZI(IALIEL-1+NEL)
              CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
              IF (NOMTE.EQ.'MEC3QU9H' .OR. NOMTE.EQ.'MEC3TR7H') THEN
                DO 60 II3 = 1,NEL - 1
                  NUMAI2 = ZI(IALIEL-1+II3)
                  IF (NUMAI2.EQ.NUMAIL) THEN
                    NOEUMI = NBNOMI(NOMTM(1:5)) - 1
                    NOEUSO = N1 - NOEUMI - 1
                    GO TO 80
                  END IF
   60           CONTINUE
              ELSEIF (NOMTE.EQ.'MECA_FACE9') THEN
                NOEUMI = 0
                NOEUSO = N1                
              END IF
   70       CONTINUE
   80       CONTINUE
          ELSE
            NOEUMI = 0
            NOEUSO = N1
          END IF
          NBNO = NBNO + NOEUSO
          NBNOQU = NBNOQU + NOEUMI
   90   CONTINUE
      END IF

      CALL JEDEMA()
      END
