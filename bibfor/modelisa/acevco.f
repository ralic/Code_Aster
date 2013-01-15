      SUBROUTINE ACEVCO(NBOCC,NLM,NLG,IER)
      IMPLICIT NONE
      INTEGER           NBOCC,NLM,NLG,IER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 14/01/2013   AUTEUR FLEJOU J-L.FLEJOU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                          AFFE_CARA_ELEM
C
C        VERIFICATION DES MOTS CLES POUR L'ELEMENT COQUE
C
C ----------------------------------------------------------------------
C  IN
C     NBOCC  : NOMBRE D'OCCURENCE
C  OUT
C     NLM    : NOMBRE TOTAL DE MAILLE
C     NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
C  IN/OUT
C     IER    : CUMUL DES ERREURS
C ----------------------------------------------------------------------
      INTEGER        IARG, IOC, NCO, NE, NEF, NEX, NEXF, NG, NIN
      INTEGER        NK, NM, NSOM
      REAL*8         R8B
      CHARACTER*8    K8B, NOMU
      CHARACTER*16   CONCEP, CMD
C-----------------------------------------------------------------------
      CALL GETRES(NOMU,CONCEP,CMD)
C
      NLM = 0
      NLG = 0
      DO 10 IOC = 1,NBOCC
         CALL GETVTX('COQUE','GROUP_MA'       ,IOC,IARG,0,K8B,NG  )
         CALL GETVTX('COQUE','MAILLE'         ,IOC,IARG,0,K8B,NM  )
         CALL GETVR8('COQUE','EPAIS'          ,IOC,IARG,0,R8B,NE  )
         CALL GETVID('COQUE','EPAIS_FO'       ,IOC,IARG,0,K8B,NEF )
         CALL GETVR8('COQUE','A_CIS'          ,IOC,IARG,0,R8B,NK  )
         CALL GETVR8('COQUE','EXCENTREMENT'   ,IOC,IARG,0,R8B,NEX )
         CALL GETVID('COQUE','EXCENTREMENT_FO',IOC,IARG,0,K8B,NEXF)
         CALL GETVTX('COQUE','INER_ROTA'      ,IOC,IARG,0,K8B,NIN )
         CALL GETVTX('COQUE','MODI_METRIQUE'  ,IOC,IARG,0,K8B,NCO )
C
         IF (IOC.EQ.1 .AND. ABS(NE+NEF).NE.1) THEN
            CALL U2MESS('E','MODELISA_53')
            IER = IER + 1
         ENDIF
C
         IF ( (NEX+NEXF).NE.0 .AND. NIN.NE.0) THEN
            CALL GETVTX('COQUE','INER_ROTA',IOC,IARG,1,K8B,NIN)
            IF ( K8B .EQ. 'NON' ) THEN
               CALL U2MESS('E','MODELISA_54')
               IER = IER + 1
            ENDIF
         ENDIF
C
         NSOM = NG + NM
         IF (NSOM.EQ.NG .OR. NSOM.EQ.NM) THEN
            NLM = MAX(NLM,-NM)
            NLG = MAX(NLG,-NG)
         ENDIF
 10   CONTINUE
C
      END
