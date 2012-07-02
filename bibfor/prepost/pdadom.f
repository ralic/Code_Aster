      SUBROUTINE PDADOM ( XM0, XM2, XM4, DOM )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      REAL*8  XM0,XM2,XM4,DOM
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C
C     CALCUL DOMMAGE EN FREQUENTIEL
C     ----------------------------------------------------------------
C
C
      INTEGER ICODWO,ICODRE(6)
      INTEGER  ICODBA,ICODHS
      CHARACTER*8  NOMMAT,CARA
      CHARACTER*8  METHOD,MECOMP,NOMPAR
      CHARACTER*8  NOMRES(6),KCORRE, KBID
      CHARACTER*16 PHENO,PHENOM
      REAL*8       DELTA,RVKE,ALPHA,PI,SALT,X,VAL(6),RE
      REAL*8       VALMIN,VALMAX,PAS,XIREG,RUNDF,NRUPT
      INTEGER      IBASK,IFONC,IHOSIN,IAWHO2,NBVAL
      LOGICAL ENDUR
      INTEGER      IARG
C
C     ----------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IAPICS ,IPOINT ,NBPAR ,NBPOIN 
      REAL*8 ERFCAM ,R8PI ,R8VIDE ,RBID ,X1 ,X2 ,XNPOIN 
      REAL*8 XP ,Y ,Y1 ,YD1 ,YD2 ,YPIC1 ,YPIC2 

C-----------------------------------------------------------------------
      IFONC  = 0
      IBASK  = 0
      IHOSIN = 0
      PI = R8PI()
      RUNDF = R8VIDE()
      CALL GETVTX(' ','DOMMAGE',1,IARG,1,METHOD,NBVAL)
      CALL GETVID(' ','MATER',1,IARG,1,NOMMAT,NBVAL)
      PHENO = 'FATIGUE'
      CALL RCCOME (NOMMAT,PHENO,PHENOM,ICODRE(1))
      IF(ICODRE(1).EQ.1) CALL U2MESS('F','FATIGUE1_24')
      CARA = 'WOHLER'
      CALL RCPARE(NOMMAT,PHENO,CARA,ICODWO)
      CARA = 'A_BASQUI'
      CALL RCPARE(NOMMAT,PHENO,CARA,ICODBA)
      CARA = 'A0'
      CALL RCPARE(NOMMAT,PHENO,CARA,ICODHS)
      IF(ICODWO.EQ.0) THEN
        IFONC = 1
      ELSEIF(ICODBA.EQ.0) THEN
        IBASK = 1
      ELSEIF(ICODHS.EQ.0) THEN
        IHOSIN = 1
      ELSE
        CALL U2MESS('F','FATIGUE1_34')
      ENDIF
C
C----  DEFINITION DES BORNES INTEGRATION
C
      CALL GETVTX(' ','COMPTAGE',1,IARG,1,MECOMP,NBVAL)
      IF (MECOMP.EQ.'PIC     '.AND.XM4.EQ.RUNDF) THEN
        CALL U2MESS('F','FATIGUE1_35')
      ENDIF
      IF(IHOSIN.NE.0) THEN
        NOMRES(6) = 'SL'
        NBPAR     = 0
        NOMPAR    = ' '
        CALL RCVALE(NOMMAT,'FATIGUE',NBPAR,NOMPAR,RBID,1,NOMRES(6),
     &              VAL(6),ICODRE(6),2)
        VALMIN = VAL(6)
        VALMAX = 10*SQRT(XM0)
      ELSE
        VALMIN = 0.D0
        VALMAX = 10*SQRT(XM0)
      ENDIF
      PAS  = (VALMAX-VALMIN)/300.D0
      IF(PAS.EQ.0.0D0) THEN
        CALL U2MESS('F','FATIGUE1_36')
      ENDIF
      XNPOIN = (VALMAX-VALMIN)/PAS
      NBPOIN = INT(XNPOIN) + 1
C
C------- CALCUL DES POINTS INTEGRATION
C
      IF(XM2.EQ.0.D0) THEN
          CALL U2MESS('F','FATIGUE1_37')
      ENDIF
      IF(MECOMP.EQ.'PIC'.AND.XM4.EQ.0.D0) THEN
          CALL U2MESS('F','FATIGUE1_38')
      ENDIF
      CALL WKVECT('&&PDADOM.DISPICS','V V R8',2*NBPOIN,IAPICS)
      IF(MECOMP.EQ.'PIC     ')  XIREG = SQRT( XM2*XM2/XM0/XM4)
      DO  305  IPOINT = 1,NBPOIN
        X1 = VALMIN + (IPOINT-1)*PAS
        IF (MECOMP.EQ.'PIC     ') THEN
           ALPHA = XIREG*X1/((SQRT(1.D0-XIREG*XIREG))*(SQRT(XM0)))
           ALPHA = (-1.D0/SQRT(2.D0))*ALPHA
           Y1=SQRT(1-XIREG*XIREG)
     &          *EXP(-X1*X1/(2.D0*XM0*(1.D0-XIREG*XIREG)))
           XP=SQRT(PI/2.D0)*ERFCAM(ALPHA)
           Y1=Y1+((XIREG*X1/SQRT(XM0))*EXP(-X1*X1/(2.D0*XM0)))*(XP)
           Y1=(SQRT(XM4)/(SQRT(XM2)*SQRT(XM0)))*Y1
           Y1=(1.D0/(2.D0*PI))*(1.D0/SQRT(2.D0*PI))*Y1
        ELSEIF (MECOMP.EQ.'NIVEAU  ') THEN
           Y1 = (1.D0/(2.D0*PI))*SQRT(XM2/(XM0*XM0*XM0))
           Y1 = Y1 *X1*EXP(-X1*X1/(2.D0*XM0))
        ENDIF
        ZR(IAPICS-1+IPOINT) = X1
        ZR(IAPICS-1+NBPOIN+IPOINT) = Y1
  305 CONTINUE
C
C---------CORRECTION ELASTO-PLASTIQUE
C
      CALL GETVTX(' ','CORR_KE',1,IARG,0,KCORRE,NBVAL)
      IF(NBVAL.NE.0) THEN
        CALL GETVTX(' ','CORR_KE',1,IARG,1,KCORRE,NBVAL)
        CALL GETVID(' ','MATER',1,IARG,1,NOMMAT,NBVAL)
        IF(KCORRE.EQ.'RCCM') THEN
          NOMRES(1) = 'N_KE'
          NOMRES(2) = 'M_KE'
          NOMRES(3) = 'SM'
          NBPAR = 0
          NOMPAR = ' '
          CALL RCVALE(NOMMAT,'RCCM',NBPAR,NOMPAR,RBID,3,NOMRES(1),
     &                VAL(1),ICODRE(1),2)
          DO 304 IPOINT = 1,NBPOIN
            DELTA = ZR(IAPICS+IPOINT-1)
            IF(DELTA.LE.3.D0*VAL(3)) THEN
              RVKE = 1.D0
            ELSEIF(DELTA.GT.3.D0*VAL(3).AND.DELTA.LT.
     &        3.D0*VAL(2)*VAL(3)) THEN
              RVKE = 1.D0 + ((1-VAL(1))/(VAL(1)*(VAL(2)-1)))*
     &                      ((DELTA/(3.D0*VAL(3)))-1.D0)
            ELSEIF(DELTA.GE.3*VAL(2)*VAL(3)) THEN
              RVKE = 1.D0/VAL(1)
            ENDIF
            ZR(IAPICS+IPOINT-1) = RVKE * ZR(IAPICS+IPOINT-1)
 304      CONTINUE
        ENDIF
      ENDIF
C
C ----- INTERPOLATION
C
      IF (METHOD.EQ.'WOHLER') THEN
C
C --- INTERPOLATION SUR LA COURBE DE WOHLER ---
C
        CALL WKVECT('&&PDADOM.WOHLER2','V V R8',NBPOIN,IAWHO2)
        IF(IFONC.NE.0) THEN
           NOMRES(1) = 'WOHLER'
           NBPAR     = 1
           PHENO     = 'FATIGUE'
           NOMPAR    = 'SIGM'
           DO  307  IPOINT = 1,NBPOIN
             DELTA = ZR(IAPICS-1+IPOINT)
             CALL LIMEND( NOMMAT,DELTA,'WOHLER',KBID,ENDUR)
             IF (ENDUR) THEN
                ZR(IAWHO2+IPOINT-1) = 0.D0
             ELSE
               CALL RCVALE(NOMMAT,PHENO,NBPAR,NOMPAR,DELTA,1,NOMRES(1),
     &                                           NRUPT,ICODRE(1),2)
               ZR(IAWHO2+IPOINT-1)  = 1.D0 / NRUPT
             ENDIF
  307     CONTINUE
        ELSEIF(IBASK.NE.0) THEN
          NOMPAR = ' '
          NBPAR  = 0
          NOMRES(1) = 'A_BASQUI'
          NOMRES(2) = 'BETA_BAS'
          CALL RCVALE(NOMMAT,'FATIGUE',NBPAR,NOMPAR,RBID,2,NOMRES,VAL,
     &                                              ICODRE,2)
          DO  308  IPOINT = 1,NBPOIN
            ZR(IAWHO2+IPOINT-1)  = VAL(1)*ZR(IAPICS+IPOINT-1)**VAL(2)
  308     CONTINUE
        ELSEIF(IHOSIN.NE.0) THEN
          NOMRES(1) = 'E_REFE'
          NOMRES(2) = 'A0'
          NOMRES(3) = 'A1'
          NOMRES(4) = 'A2'
          NOMRES(5) = 'A3'
          NOMRES(6) = 'SL'
          NBPAR     = 0
          NOMPAR    = ' '
          CALL RCVALE(NOMMAT,'FATIGUE',NBPAR,NOMPAR,RBID,6,NOMRES,VAL,
     &                                                   ICODRE,2)
          NOMRES(1) = 'E'
          CALL RCVALE(NOMMAT,'ELAS',NBPAR,NOMPAR,RBID,1,NOMRES,RE,
     &                                                ICODRE,2)
          DO  309  IPOINT = 1,NBPOIN
            SALT  = (VAL(1)/RE)*ZR(IAPICS+IPOINT-1)
            IF (SALT.GE.VAL(6)) THEN
              X  = LOG10 (SALT)
              Y = VAL(2) + VAL(3)*X + VAL(4)*(X**2) + VAL(5)*(X**3)
              ZR(IAWHO2+IPOINT-1) = 1.D0 / (10.D0**Y)
            ELSE
              ZR(IAWHO2+IPOINT-1) = 0.D0
            ENDIF
  309     CONTINUE
        ENDIF
      ENDIF
C
C -- CALCUL INTEGRALE
C
      DOM = 0.D0
      DO 310 IPOINT = 2,NBPOIN
        X2 = ZR(IAPICS-1+IPOINT)
        X1 = ZR(IAPICS-1+IPOINT-1)
        YD2 = ZR(IAWHO2-1+IPOINT)
        YD1 = ZR(IAWHO2-1+IPOINT-1)
        YPIC2 = ZR(IAPICS-1+NBPOIN+IPOINT)
        YPIC1 = ZR(IAPICS-1+NBPOIN+IPOINT-1)
        DOM = DOM + (YD2*YPIC2+YD1*YPIC1)* (X2-X1)/2.D0
  310 CONTINUE
C
      CALL JEDETR('&&PDADOM.DISPICS')
      CALL JEDETR('&&PDADOM.WOHLER2')
C
      END
