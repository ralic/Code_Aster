      SUBROUTINE ACEVOR(NBOCC,NLM,NLG,NLN,NLJ,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NBOCC,NLM,NLG,NLN,NLJ,IER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
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
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     VERIFICATION DES MOTS CLES POUR LES ORIENTATIONS
C ----------------------------------------------------------------------
C IN  : NBOCC  : NOMBRE D'OCCURENCE
C OUT : NLM    : NOMBRE TOTAL DE MAILLE
C OUT : NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
C OUT : NLN    :
C OUT : NLJ    :
C ----------------------------------------------------------------------
      PARAMETER ( NBCAR = 100 , NBVAL = 1000 , NCO = 4 )
      REAL*8       R8B, VAL(NBVAL)
      CHARACTER*6  KIOC
      CHARACTER*8  K8B, CAR(NBCAR), NOMU, TOU, CARORI(NCO)
      CHARACTER*16 CMD, CONCEP
      CHARACTER*24 VALK(2)
      DATA CARORI  /'VECT_Y   ','VECT_X_Y ','ANGL_NAUT','ANGL_VRIL'/
C     ------------------------------------------------------------------
C
      CALL GETRES(NOMU,CONCEP,CMD)
      NLM = 0
      NLG = 0
      NLN = 0
      NLJ = 0
C
      DO 10 IOC = 1,NBOCC
         CALL CODENT(IOC,'G',KIOC)
         CALL GETVID('ORIENTATION','GROUP_MA' ,IOC,1,0   ,K8B ,NG)
         CALL GETVID('ORIENTATION','MAILLE'   ,IOC,1,0   ,K8B ,NM)
         CALL GETVID('ORIENTATION','GROUP_NO' ,IOC,1,0   ,K8B ,NJ)
         CALL GETVID('ORIENTATION','NOEUD'    ,IOC,1,0   ,K8B ,NN)
         CALL GETVTX('ORIENTATION','CARA'    ,IOC,1,0   ,K8B ,NC)
         CALL GETVTX('ORIENTATION','CARA'    ,IOC,1,NBCAR,CAR,NCAR)
         CALL GETVR8('ORIENTATION','VALE'     ,IOC,1,0   ,R8B ,NV)
         CALL GETVR8('ORIENTATION','VALE'     ,IOC,1,NBVAL,VAL,NVAL)
C
C -- IOC = 1
         IF (IOC.EQ.1) THEN
            IF (NV.EQ.0) THEN
               CALL U2MESS('E','MODELISA_57')
               IER = IER + 1
            ENDIF
            IF (NC.EQ.0) THEN
               CALL U2MESS('E','MODELISA_58')
               IER = IER + 1
            ENDIF
         ENDIF
C
C -- CARA
         IF (NCAR.GT.0) THEN
C-DEL       NCARAC = NCAR
            IF (NVAL.EQ.0) THEN
               CALL U2MESK('E','MODELISA_59',1,KIOC)
               IER = IER + 1
            ENDIF
            K = 0
            DO 20 J = 1 , NCO
               IF (CAR(1).EQ.CARORI(J)) K = J
 20         CONTINUE
         ENDIF
C
C -- VALE
         IF (NVAL.GT.0) THEN
            IF ((K.EQ.1.AND.NVAL.NE.3) .OR. (K.EQ.2.AND.NVAL.NE.6) .OR.
     &          (K.EQ.3.AND.NVAL.NE.3) .OR. (K.EQ.4.AND.NVAL.NE.1) )THEN
                VALK(1) = KIOC
                VALK(2) = CARORI(K)
                CALL U2MESK('E','MODELISA_60', 2 ,VALK)
               IER = IER + 1
            ENDIF
         ENDIF
C
         NSOM = NG + NM + NJ + NN
         IF (NSOM.EQ.NG .OR. NSOM.EQ.NM .OR.
     &       NSOM.EQ.NJ .OR. NSOM.EQ.NN) THEN
            NLM = MAX(NLM,-NM)
            NLG = MAX(NLG,-NG)
            NLN = MAX(NLN,-NN)
            NLJ = MAX(NLJ,-NJ)
         ENDIF
 10   CONTINUE
C
      END
