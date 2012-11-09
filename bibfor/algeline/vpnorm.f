      SUBROUTINE VPNORM(NORM,PARA,LMATR,NEQ,NBMODE,DDLEXC,VECPRO,RESUFR,
     &                  LMASIN, XMASTR, ISIGN, NUMDDL, COEF, LBASM )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*(*)     NORM,PARA
      INTEGER           NBMODE,NEQ,LMATR,DDLEXC(*)
      REAL*8            VECPRO(NEQ,*),RESUFR(NBMODE,*),XMASTR,COEF(*)
      LOGICAL           LMASIN,LBASM
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C     NORMALISATION DE VECTEURS ET DE GRANDEURS MODALES
C     ------------------------------------------------------------------
C IN  NORM   : TYPE DE NORMALISATION
C          = 'AVEC_CMP'
C          = 'MASS_GENE'
C          = 'RIGI_GENE'
C          = 'EUCL', 'EUCL_TRAN', ...
C IN  PARA   : ON REPERCUTE LA NORMALISATION SUR LES PARAMETRES MODAUX
C          = 'OUI' DANS CE CAS ILS DOIVENT DEJA AVOIR ETE CALCULES
C          = 'NON' ON NE NORMALISE QUE LES VECTEURS PROPRES
C IN  LMATR   : DESCRIPTEUR D'UNE MATRICE
C IN  NEQ    : NOMBRE D'EQUATIONS
C IN  NBMODE : NOMBRE DE MODES
C IN  DDLEXC : TABLEAU DES DDL EXCLUS
C          = 0 SI EXCLUS
C          = 1 SI NON EXCLUS
C VAR VECPRO : TABLEAU DES VECTEURS PROPRES
C VAR RESUFR : TABLEAU DES GRANDEURS MODALES RANGEES SELON
C        'FREQ'            , 'OMEGA2'          , 'AMOR_REDUIT'     ,
C        'MASS_GENE'       , 'RIGI_GENE'       , 'AMOR_GENE'       ,
C        'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,
C        'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,
C        'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ'
C IN  LMASIN : CALCUL DES MASSES MODALES UNITAIRES
C IN  XMASTR : MASSE DE LA STRUCTURE
C IN  LBASM  : LOGICAL = TRUE SI BASE MODALE,
C                        FALSE SI MODE_MECA,MODE_FLAMB,...
C OUT COEF   : COEFFICIENTS
C     ------------------------------------------------------------------
C
      CHARACTER*24 VALK
C
      REAL*8  XMN, XX1, XX2, XX3, XNORM
C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER IE ,IM ,INDG ,ISIGN ,LPO1 ,LPO2 ,NUMDDL

C-----------------------------------------------------------------------
      CALL JEMARQ()
      IF ( NORM.EQ.'AVEC_CMP' .OR. NORM(1:4) .EQ.'EUCL' ) THEN
C
C     --- NORMALISATION SUR LES DDL NON EXCLUS
C
         DO 2 IM = 1,NBMODE
            XNORM = 0.0D0
            IF (NORM(1:4).EQ.'EUCL') THEN
               DO 4 IE = 1,NEQ
                  XX1 = VECPRO(IE,IM) * DDLEXC(IE)
                  XNORM = XNORM + XX1*XX1
 4             CONTINUE
               XNORM = SQRT(XNORM)
            ELSE
               DO 6 IE = 1,NEQ
                  XX1 = VECPRO(IE,IM)*DDLEXC(IE)
                  IF (ABS(XNORM) .LT. ABS(XX1)) THEN
                    XNORM = XX1
                  ENDIF
 6             CONTINUE
            ENDIF
            XX1 = 1.0D0 / XNORM
            COEF(IM) = XX1
            DO 8 IE = 1,NEQ
               VECPRO(IE,IM) = VECPRO(IE,IM) * XX1
 8          CONTINUE
            IF (PARA.EQ.'OUI') THEN
              XX2 = XX1 * XX1
              RESUFR(IM,4)  = RESUFR(IM,4)  * XX2
              RESUFR(IM,5)  = RESUFR(IM,5)  * XX2
C-PROV        RESUFR(IM,6)  = RESUFR(IM,6)  * XX2
              RESUFR(IM,10) = RESUFR(IM,10) * XNORM
              RESUFR(IM,11) = RESUFR(IM,11) * XNORM
              RESUFR(IM,12) = RESUFR(IM,12) * XNORM
            ELSE
              XX2 = XX1 * XX1
              RESUFR(IM,4)  = RESUFR(IM,4)  * XX2
              RESUFR(IM,5)  = RESUFR(IM,5)  * XX2
C-PROV        RESUFR(IM,6)  = RESUFR(IM,6)  * XX2
            ENDIF
 2       CONTINUE
C
      ELSE IF ( NORM.EQ.'MASS_GENE' .OR. NORM.EQ.'RIGI_GENE') THEN
C
C     --- ON NORMALISE LA MASSE OU LA RAIDEUR GENERALISEE A 1 ---
C
         INDG = 4
         IF (NORM.EQ.'RIGI_GENE') INDG=5
         IF (PARA.EQ.'OUI') THEN
            DO 10 IM = 1,NBMODE
               XMN = RESUFR(IM,INDG)
               XX1 = 1.0D0 / XMN
               XX2 = SQRT(XMN)
               XX3 = 1.0D0 / XX2
               RESUFR(IM,4)  = RESUFR(IM,4)  * XX1
               RESUFR(IM,5)  = RESUFR(IM,5)  * XX1
               RESUFR(IM,10) = RESUFR(IM,10) * XX2
               RESUFR(IM,11) = RESUFR(IM,11) * XX2
               RESUFR(IM,12) = RESUFR(IM,12) * XX2
               COEF(IM) = XX3
               DO 12 IE = 1,NEQ
                  VECPRO(IE,IM) = VECPRO(IE,IM) * XX3
 12            CONTINUE
 10         CONTINUE
         ELSE
            CALL WKVECT('&&VPNORM.POI1','V V R',NEQ,LPO1)
            CALL WKVECT('&&VPNORM.POI2','V V R',NEQ,LPO2)
            DO 20 IM = 1,NBMODE
               DO 22 IE = 1,NEQ
                  ZR(LPO1+IE-1) = VECPRO(IE,IM)
 22            CONTINUE
               CALL MRMULT('ZERO',LMATR,ZR(LPO1),ZR(LPO2),1,.TRUE.)
               XMN = 0.0D0
               DO 24 IE = 1,NEQ
                  XMN = XMN + ( ZR(LPO1+IE-1) * ZR(LPO2+IE-1) )
 24            CONTINUE
               XX1 = 1.0D0 / SQRT(XMN)
               COEF(IM) = XX1
               DO 26 IE = 1,NEQ
                  VECPRO(IE,IM) = VECPRO(IE,IM) * XX1
 26            CONTINUE
 20         CONTINUE
            CALL JEDETR('&&VPNORM.POI2')
            CALL JEDETR('&&VPNORM.POI1')
         ENDIF
C
      ELSE
C
         VALK = NORM
         CALL U2MESG('F', 'ALGELINE4_77',1,VALK,0,0,0,0.D0)
C
      ENDIF
      IF ( LMASIN ) THEN
         DO 30 IM = 1,NBMODE
            RESUFR(IM,13) = RESUFR(IM,7) / XMASTR
            RESUFR(IM,14) = RESUFR(IM,8) / XMASTR
            RESUFR(IM,15) = RESUFR(IM,9) / XMASTR
 30      CONTINUE
      ENDIF
C
      IF ( ISIGN .EQ. 0 ) THEN
      ELSEIF ( ISIGN .EQ. 1 ) THEN
         DO 100 IM = 1,NBMODE
            XX1 = VECPRO(NUMDDL,IM)
            IF ( XX1 .LT. 0.0D0 ) THEN
               COEF(IM) = -COEF(IM)
               DO 102 IE = 1,NEQ
                  VECPRO(IE,IM) = -VECPRO(IE,IM)
 102           CONTINUE
               IF (PARA.EQ.'OUI') THEN
                  RESUFR(IM,10) = -RESUFR(IM,10)
                  RESUFR(IM,11) = -RESUFR(IM,11)
                  RESUFR(IM,12) = -RESUFR(IM,12)
               ENDIF
            ENDIF
 100     CONTINUE
      ELSEIF ( ISIGN .EQ. -1 ) THEN
         DO 110 IM = 1,NBMODE
            XX1 = VECPRO(NUMDDL,IM)
            IF ( XX1 .GT. 0.0D0 ) THEN
               COEF(IM) = -COEF(IM)
               DO 112 IE = 1,NEQ
                  VECPRO(IE,IM) = -VECPRO(IE,IM)
 112           CONTINUE
               IF (PARA.EQ.'OUI') THEN
                  RESUFR(IM,10) = -RESUFR(IM,10)
                  RESUFR(IM,11) = -RESUFR(IM,11)
                  RESUFR(IM,12) = -RESUFR(IM,12)
               ENDIF
            ENDIF
 110     CONTINUE
      ENDIF
C
      CALL JEDEMA()
      END
