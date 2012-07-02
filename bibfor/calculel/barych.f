      SUBROUTINE BARYCH(CH1Z,CH2Z,R1,R2,CHZ,BASE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C ======================================================================
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
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
C
C
      CHARACTER*5  VALE
      CHARACTER*4  DOCU,SCAL
      CHARACTER*1 K1BID
C-----------------------------------------------------------------------
      INTEGER I ,IACH ,IACH1 ,IACH2 ,IBID ,IER ,JREFE 
      INTEGER LON1 ,LON2 ,LONG 
C-----------------------------------------------------------------------
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


      CALL JEDEMA()
      END
