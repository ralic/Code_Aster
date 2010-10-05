      SUBROUTINE BARYCH(CH1Z,CH2Z,R1,R2,CHZ,BASE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 04/10/2010   AUTEUR PELLET J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     CH1Z,CH2Z, CHZ
      CHARACTER*19      CH1, CH2,  CH
      CHARACTER*1                        BASE
      REAL*8                    R1,R2
C ----------------------------------------------------------------------
C     BUT :   FAIRE LA COMBINAISON LINERAIRE DE 2 CHAMP :
C             CH = R1*CH1+ R2*CH2 (CHAMP = CHAM_NO OU CHAM_ELEM)
C
C IN:  CH1    : NOM DU 1ER CHAMP
C      CH2    : NOM DU 2EM CHAMP
C      R1,R2  : COEFFICIENTS MULTIPLICATEURS.
C      BASE   : 'G' OU 'V' (GLOBALE OU VOLATILE)
C      CH     : NOM DU CHAMP RESULTAT.
C
C OUT: CH EST REMPLI.
C ----------------------------------------------------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      CHARACTER*5  VALE
      CHARACTER*4  DOCU,SCAL
      CHARACTER*1 K1BID
      CALL JEMARQ()
      CH1=CH1Z
      CH2=CH2Z
      CH=CHZ

      CALL COPISD('CHAMP_GD',BASE,CH1,CH)

      CALL JEEXIN(CH//'.DESC',IBID)
      IF (IBID.GT.0) THEN
        CALL JELIRA(CH//'.DESC','DOCU',IBID,DOCU)
      ELSE
        CALL JELIRA(CH//'.CELD','DOCU',IBID,DOCU)
      END IF


      IF ((DOCU(1:4).NE.'CHNO').AND.(DOCU(1:4).NE.'CHML')) THEN
        CALL ASSERT(.FALSE.)
      ELSE IF ( DOCU(1:4).EQ.'CHNO') THEN
C     -----------------------------------
        VALE='.VALE'
        CALL JELIRA(CH1//VALE,'LONMAX',LON1,K1BID)
        CALL JELIRA(CH1//VALE,'TYPE',IBID,SCAL)
        CALL VRREFE(CH1,CH2,IER)
        IF ( IER .EQ. 0 ) THEN
C
C ----- RECOPIE BRUTALE DES .VALE
          CALL JEVEUO(CH//VALE,'E',IACH)
          CALL JEVEUO(CH1//VALE,'L',IACH1)
          CALL JEVEUO(CH2//VALE,'L',IACH2)
          IF (SCAL(1:1).EQ.'R') THEN
            DO 1,I = 1,LON1
               ZR(IACH-1+I) = R1*ZR(IACH1-1+I) + R2*ZR(IACH2-1+I)
    1       CONTINUE
          ELSE IF (SCAL(1:1).EQ.'C') THEN
            DO 2,I = 1,LON1
               ZC(IACH-1+I) = R1*ZC(IACH1-1+I) + R2*ZC(IACH2-1+I)
    2       CONTINUE
          END IF
        ELSE
          CALL VTCOPY(CH2,CH)
          CALL JEVEUO(CH//VALE,'E',IACH)
          CALL JEVEUO(CH1//VALE,'L',IACH1)
          IF (SCAL(1:1).EQ.'R') THEN
            DO 3,I = 1,LON1
               ZR(IACH-1+I) = R1*ZR(IACH1-1+I) + R2*ZR(IACH-1+I)
    3       CONTINUE
          ELSE IF (SCAL(1:1).EQ.'C') THEN
            DO 4,I = 1,LON1
               ZC(IACH-1+I) = R1*ZC(IACH1-1+I) + R2*ZC(IACH-1+I)
    4       CONTINUE
          END IF
          CALL JEVEUO(CH2//'.REFE','L',JREFE)
        ENDIF


      ELSE IF ( DOCU(1:4).EQ.'CHML') THEN
C     -----------------------------------
        CALL VRREFE(CH1,CH2,IER)
        IF (IER.EQ.0) THEN
          VALE='.CELV'
          CALL JELIRA(CH1//VALE,'TYPE',IBID,SCAL)
          CALL JELIRA(CH1//VALE,'LONMAX',LON1,K1BID)
          CALL JELIRA(CH2//VALE,'LONMAX',LON2,K1BID)
          CALL JELIRA(CH//VALE,'LONMAX',LONG,K1BID)
          CALL ASSERT((LON1.EQ.LON2).AND.(LON1.EQ.LONG))

          CALL JEVEUO(CH//VALE,'E',IACH)
          CALL JEVEUO(CH1//VALE,'L',IACH1)
          CALL JEVEUO(CH2//VALE,'L',IACH2)
          IF (SCAL(1:1).EQ.'R') THEN
            DO 5,I = 1,LON1
               ZR(IACH-1+I) = R1*ZR(IACH1-1+I) + R2*ZR(IACH2-1+I)
    5       CONTINUE
          ELSE IF (SCAL(1:1).EQ.'C') THEN
            DO 6,I = 1,LON1
             ZC(IACH-1+I) = R1*ZC(IACH1-1+I) + R2*ZC(IACH2-1+I)
    6       CONTINUE
          END IF
        ELSE
          CALL U2MESS('F','CALCULEL_27')
        ENDIF
      END IF


 9999 CONTINUE
      CALL JEDEMA()
      END
