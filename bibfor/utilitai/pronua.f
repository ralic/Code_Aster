      SUBROUTINE PRONUA(METHOD,NUAG1,NUAG2)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
      CHARACTER*(*) METHOD,NUAG1,NUAG2

C  BUT : PROJETER LES VALEURS DU NUAGE NUAG1 SUR LES POINTS
C        DU NUAGE NUAG2 SELON LA METHODE METHOD
C
C IN  METHOD   : METHODE D'INTERPOLATION: 'NUAGE_DEG_0' OU 'NUAGE_DEG_1'
C IN  NUAG1 (JXIN)    : SD NUAGE A PROJETER
C IN  NUAG2 (JXVAR)   : SD NUAGE A EVALUER

C VARIABLES LOCALES :
      INTEGER INUAI1,INUAI2,INUAX1,INUAX2,IADREF,IACORR
      INTEGER INUAV1,INUAV2
      INTEGER INDIIS,IRET,INUAL1,INUAL2,IP2,IC2,IP1,IC1
      INTEGER NX1,NX2,NP1,NP2,GD1,GD2,NC1,NC2,IBID,IERD
      INTEGER I1,I2,II2,I
      REAL*8 VAL2R
      CHARACTER*19 NUA1,NUA2
      CHARACTER*24 VALK(2)
      CHARACTER*8 NOGD
      CHARACTER*3 TYSCA
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL,LDREF
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNUM
      CHARACTER*80 ZK80
C
C DEB-------------------------------------------------------------------
      CALL JEMARQ()
      NUA1 = NUAG1
      NUA2 = NUAG2
      CALL JEVEUO(NUA1//'.NUAI','L',INUAI1)
      CALL JEVEUO(NUA2//'.NUAI','L',INUAI2)
      CALL JEVEUO(NUA1//'.NUAX','L',INUAX1)
      CALL JEVEUO(NUA1//'.NUAV','L',INUAV1)
      CALL JEVEUO(NUA2//'.NUAX','L',INUAX2)
      CALL JEVEUO(NUA2//'.NUAV','E',INUAV2)

      NX1 = ZI(INUAI1-1+2)
      NX2 = ZI(INUAI2-1+2)
      IF (NX1.NE.NX2) THEN
        VALK(1) = NUA1
        VALK(2) = NUA2
        CALL U2MESK('F','UTILITAI3_89', 2 ,VALK)
      ENDIF
      NP1 = ZI(INUAI1-1+1)
      NP2 = ZI(INUAI2-1+1)
      GD1 = ZI(INUAI1-1+4)
      GD2 = ZI(INUAI2-1+4)
      IF (GD1.NE.GD2) THEN
         VALK(1) = NUA1
         VALK(2) = NUA2
         CALL U2MESK('F','UTILITAI3_90', 2 ,VALK)
      ENDIF
      CALL JENUNO(JEXNUM('&CATA.GD.NOMGD',GD1),NOGD)
      CALL DISMOI('F','TYPE_SCA',NOGD,'GRANDEUR',IBID,TYSCA,IERD)

      NC1 = ZI(INUAI1-1+3)
      NC2 = ZI(INUAI2-1+3)


C     -- L'OBJET '&&PRONUA.DREF' DONNE LA DISTANCE**2 DE REFERENCE
C        A UTILISER POUR CHAQUE POINT DE NUAG2 :
C        -----------------------------------------------------
      CALL WKVECT('&&PRONUA.DREF','V V R',NP2,IADREF)


C     -- L'OBJET '&&PRONUA.CORRESP' ETABLIT LA CORRESPONDANCE
C        ENTRE LES NUMEROS DE CMPS DE NUAG2 ET CEUX DE NUAG1 :
C        -----------------------------------------------------
      CALL WKVECT('&&PRONUA.CORRESP','V V I',NC2,IACORR)
      DO 1,I2= 1,NC2
        II2=ZI(INUAI2-1+5+I2)
        I1=INDIIS(ZI(INUAI1-1+6),II2,1,NC1)
        IF (I1.EQ.0) THEN
          CALL U2MESK('F','UTILITAI3_91',1,NUA1)
        ELSE
          ZI(IACORR-1+I2) = I1
        END IF
    1 CONTINUE


C     SI LES OBJETS .NUAL N'EXISTENT PAS, ON LES CREE :
C     -------------------------------------------------
      CALL JEEXIN(NUA1//'.NUAL',IRET)
      IF (IRET.EQ.0) THEN
        CALL WKVECT(NUA1//'.NUAL','V V L',NC1*NP1,INUAL1)
        DO 50,I=1,NC1*NP1
          ZL(INUAL1-1+I)=.TRUE.
 50     CONTINUE
      ELSE
        CALL JEVEUO(NUA1//'.NUAL','L',INUAL1)
      END IF

      CALL JEEXIN(NUA2//'.NUAL',IRET)
      IF (IRET.EQ.0) THEN
        CALL WKVECT(NUA2//'.NUAL','V V L',NC2*NP2,INUAL2)
        DO 51,I=1,NC2*NP2
          ZL(INUAL2-1+I)=.TRUE.
 51     CONTINUE
      ELSE
        CALL JEVEUO(NUA2//'.NUAL','L',INUAL2)
      END IF


C     SI TOUS LES POINTS DE NUAG1 PORTENT LES MEMES CMPS
C     ON POURRA NE CALCULER L'OBJET .DREF QU'UNE SEULE FOIS
C     -------------------------------------------------
      LDREF=.TRUE.
      DO 71,IP1=2,NP1
        DO 72,IC1=1,NC1
          IF(  ZL(INUAL1-1+(IP1-1)*NC1+IC1).NEQV.
     &         ZL(INUAL1-1+(IP1-2)*NC1+IC1)) THEN
             LDREF=.FALSE.
             GO TO 73
          END IF
 72     CONTINUE
 71   CONTINUE
 73   CONTINUE

C     BOUCLE SUR LES CMPS DE NUAG2 :
C     ------------------------------
      DO 20,IC2 = 1,NC2
        IC1 = ZI(IACORR-1+IC2)

C       CALCUL EVENTUEL DES DISTANCES DE REFERENCE :
C       --------------------------------------------
        IF ((IC2.EQ.1).OR.(.NOT.LDREF)) CALL NUADRF(NUA1,NUA2,
     &          IC1,IC2,ZR(IADREF))

C       BOUCLE SUR LES POINTS DU NUAGE NUAG2 :
C       --------------------------------------

        IF (TYSCA.EQ.'R') THEN
C       ----------------------
          DO 10,IP2 = 1,NP2
            IF (ZL(INUAL2-1+ (IP2-1)*NC2+IC2)) THEN
              CALL NUAINR(METHOD,NP1,NX1,NC1,IC1,ZR(INUAX1),
     &        ZL(INUAL1), ZR(INUAV1), ZR(INUAX2-1+ (IP2-1)*NX2+1),
     &                  ZR(IADREF-1+IP2),VAL2R)
              ZR(INUAV2-1+ (IP2-1)*NC2+IC2) = VAL2R
            ELSE
              ZR(INUAV2-1+ (IP2-1)*NC2+IC2) = 0.D0
            END IF
   10     CONTINUE

        ELSE
          CALL U2MESS('F','UTILITAI3_93')
        END IF

   20 CONTINUE


C     MENAGE :
C     --------
      CALL JEDETR('&&PRONUA.CORRESP')
      CALL JEDETR('&&PRONUA.DREF')
      CALL JEDEMA()
      END
