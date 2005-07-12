      SUBROUTINE UTITES (LABEL1,LABEL2,TYPE,REFI,REFR,REFC,
     +                          VALI,VALR,VALC,EPSI,CRIT,IFIC,SSIGNE)
      IMPLICIT   NONE
      INTEGER                          VALI,REFI,        IFIC
      CHARACTER*(*)      LABEL1,LABEL2,TYPE,        CRIT,     SSIGNE
      REAL*8                          VALR,REFR,EPSI
      COMPLEX*16                          VALC,REFC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/07/2005   AUTEUR CIBHHPD L.SALMONA 
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
C  IN  : K12 : LABEL1 : UN LABEL SUR LA PREMIERE LIGNE (6,17)
C  IN  : K17 : LABEL2 : UN LABEL SUR LA DEUXIEME LIGNE (1,17)
C  IN  : K1  : TYPE   : TYPE DE VALEUR A TESTER 'R', OU 'C'
C  IN  : R8  : REFR   : VALEUR REELLE DE REFERENCE
C  IN  : C16 : REFC   : VALEUR COMPLEXE DE REFERENCE
C  IN  : R8  : VALR   : VALEUR REELLE A TESTER ( ASTER )
C  IN  : C16 : VALC   : VALEUR COMPLEXE A TESTER ( ASTER )
C  IN  : K8  : CRIT   : PRECISION 'RELATIVE' OU 'ABSOLUE'
C  IN  : R8  : EPSI   : PRECISION ESPEREE
C  IN  : I   : IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
C  OUT :                IMPRESSION SUR LISTING
C ----------------------------------------------------------------------
      CHARACTER*4  TESTOK, RELA
      CHARACTER*12 K12
      CHARACTER*17 K17
      REAL*8       ERR, ZERO, CENT, EPSR
      COMPLEX*16   ZEROC
      LOGICAL      LOK
C     ------------------------------------------------------------------
C
      K12    = LABEL1
      K17    = LABEL2
      RELA   = CRIT
      CENT   = 100.0D0
      ZERO   = 0.D0
      ZEROC  = DCMPLX(0.D0,0.D0)
      TESTOK = 'NOOK '
C
      IF ( TYPE(1:1) .EQ. 'R' ) THEN
         IF ( SSIGNE .EQ. 'OUI' ) VALR = ABS(VALR) 
         IF ( RELA .EQ. 'RELA' ) THEN
            LOK = ( ABS(VALR-REFR) .LE. EPSI * ABS(REFR) )
            IF ( REFR .NE. ZERO ) THEN
               ERR = (VALR - REFR) / REFR *CENT
            ELSE
               ERR = 999.999999D0
            ENDIF
            IF ( LOK ) TESTOK = ' OK '
            IF (ABS(ERR).GT.1.0D-03 .AND. ABS(ERR).LT.1.0D+03) THEN
               IF (ERR.LT.-99.9D0) THEN
                  WRITE(IFIC,1001) TESTOK, K12, ERR, VALR
               ELSE
                  WRITE(IFIC,1000) TESTOK, K12, ERR, VALR
               ENDIF
            ELSE
               WRITE(IFIC,1002) TESTOK, K12, ERR, VALR
            ENDIF
            EPSR = EPSI*CENT
            IF (ABS(EPSR).GT.1.0D-03 .AND. ABS(EPSR).LT.1.0D+03 ) THEN
               WRITE(IFIC,1010) K17, EPSR, REFR
            ELSE
               WRITE(IFIC,1012) K17, EPSR, REFR
            ENDIF
         ELSE
            LOK = ( ABS(VALR - REFR) .LE. EPSI )
            ERR =       VALR - REFR
            IF ( LOK ) TESTOK = ' OK '
            IF (ABS(ERR).GT.1.0D-03 .AND. ABS(ERR).LT.1.0D+03) THEN
               IF (ERR.LT.-99.9D0) THEN
                  WRITE(IFIC,1005) TESTOK, K12, ERR, VALR
               ELSE
                  WRITE(IFIC,1004) TESTOK, K12, ERR, VALR
               ENDIF
            ELSE
               WRITE(IFIC,1006) TESTOK, K12, ERR, VALR
            ENDIF
            IF (ABS(EPSI).GT.1.0D-03 .AND. ABS(EPSI).LT.1.0D+03) THEN
               WRITE(IFIC,1014) K17, EPSI, REFR
            ELSE
               WRITE(IFIC,1016) K17, EPSI, REFR
            ENDIF
         ENDIF
C
      ELSEIF ( TYPE(1:1) .EQ. 'I' ) THEN
         IF ( SSIGNE .EQ. 'OUI' ) VALI = ABS(VALI) 
         ERR = DBLE( VALI - REFI )
         IF ( RELA .EQ. 'RELA' ) THEN
            LOK = ( ABS( ERR ) .LE. EPSI*ABS(REFI) )
            IF ( REFI .NE. 0 ) THEN
               ERR = ( VALI - REFI ) / REFI *CENT
            ELSE
               ERR = 999.999999D0
            ENDIF
            IF ( LOK ) TESTOK = ' OK '
            IF (ABS(ERR).GT.1.0D-03 .AND. ABS(ERR).LT.1.0D+03) THEN
               IF (ERR.LT.-99.9D0) THEN
                  WRITE(IFIC,1101) TESTOK, K12, ERR, VALI
               ELSE
                  WRITE(IFIC,1100) TESTOK, K12, ERR, VALI
               ENDIF
            ELSE
               WRITE(IFIC,1102) TESTOK, K12, ERR, VALI
            ENDIF
            EPSR = EPSI*CENT
            IF (ABS(EPSR).GT.1.0D-03 .AND. ABS(EPSR).LT.1.0D+03) THEN
               WRITE(IFIC,1110) K17, EPSR, REFI
            ELSE
               WRITE(IFIC,1112) K17, EPSR, REFI
            ENDIF
         ELSE
            LOK = ( ABS( ERR ) .LE. EPSI )
            IF ( LOK ) TESTOK = ' OK '
            IF (ABS(ERR).GT.1.0D-03 .AND. ABS(ERR).LT.1.0D+03) THEN
               IF (ERR.LT.-99.9D0) THEN
                  WRITE(IFIC,1105) TESTOK, K12, ERR, VALI
               ELSE
                  WRITE(IFIC,1104) TESTOK, K12, ERR, VALI
               ENDIF
            ELSE
               WRITE(IFIC,1106) TESTOK, K12, ERR, VALI
            ENDIF
            IF (ABS(EPSI).GT.1.0D-03 .AND. ABS(EPSI).LT.1.0D+03) THEN
               WRITE(IFIC,1114) K17, EPSI, REFI
            ELSE
               WRITE(IFIC,1116) K17, EPSI, REFI
            ENDIF
         ENDIF
C
      ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
         IF ( RELA .EQ. 'RELA' ) THEN
            LOK = ( ABS(VALC-REFC) .LE. EPSI * ABS(REFC))
            IF ( REFC .NE. ZEROC ) THEN
               ERR = ABS(VALC - REFC) /  ABS(REFC) *CENT
            ELSE
               ERR = 999.999999D0
            ENDIF
            IF ( LOK ) TESTOK = ' OK '
            IF (ABS(ERR).GT.1.0D-03 .AND. ABS(ERR).LT.1.0D+03) THEN
               IF (ERR.LT.-99.9D0) THEN
                  WRITE(IFIC,1201) TESTOK, K12, ERR, VALC
               ELSE
                  WRITE(IFIC,1200) TESTOK, K12, ERR, VALC
               ENDIF
            ELSE
               WRITE(IFIC,1202) TESTOK, K12, ERR, VALC
            ENDIF
            EPSR = EPSI*CENT
            IF (ABS(EPSR).GT.1.0D-03 .AND. ABS(EPSR).LT.1.0D+03) THEN
               WRITE(IFIC,1210) K17, EPSR, REFC
            ELSE
               WRITE(IFIC,1212) K17, EPSR, REFC
            ENDIF
         ELSE
            LOK = ( ABS(VALC - REFC) .LE. EPSI )
            ERR =   ABS(VALC - REFC)
            IF ( LOK ) TESTOK = ' OK '
            IF (ABS(ERR).GT.1.0D-03 .AND. ABS(ERR).LT.1.0D+03) THEN
               IF (ERR.LT.-99.9D0) THEN
                  WRITE(IFIC,1205) TESTOK, K12, ERR, VALC
               ELSE
                  WRITE(IFIC,1204) TESTOK, K12, ERR, VALC
               ENDIF
            ELSE
               WRITE(IFIC,1206) TESTOK, K12, ERR, VALC
            ENDIF
            IF (ABS(EPSI).GT.1.0D-03 .AND. ABS(EPSI).LT.1.0D+03) THEN
               WRITE(IFIC,1214) K17, EPSI, REFC
            ELSE
               WRITE(IFIC,1216) K17, EPSI, REFC
            ENDIF
         ENDIF
      ENDIF
C
C     IF (TESTOK.EQ.'NOOK') CALL ABORT()


1000  FORMAT(A,1X,A,1X,'RELA',3X,   F7.3,' % VALE:',1P,D20.13)
1001  FORMAT(A,1X,A,1X,'RELA',3X,   F7.2,' % VALE:',1P,D20.13)
1002  FORMAT(A,1X,A,1X,'RELA',1X, 1PE9.2,' % VALE:',1P,D20.13)
1004  FORMAT(A,1X,A,1X,'ABSO',3X,   F7.3,'   VALE:',1P,D20.13)
1005  FORMAT(A,1X,A,1X,'ABSO',3X,   F7.2,'   VALE:',1P,D20.13)
1006  FORMAT(A,1X,A,1X,'ABSO',1X, 1PE9.2,'   VALE:',1P,D20.13)
1010  FORMAT(A     ,1X,'TOLE',3X,   F7.3,' % REFE:',1P,D20.13)
1012  FORMAT(A     ,1X,'TOLE',1X, 1PE9.2,' % REFE:',1P,D20.13)
1014  FORMAT(A     ,1X,'TOLE',3X,   F7.3,'   REFE:',1P,D20.13)
1016  FORMAT(A     ,1X,'TOLE',1X, 1PE9.2,'   REFE:',1P,D20.13)
C
1100  FORMAT(A,1X,A,1X,'RELA',3X,   F7.3,' % VALE:',I9)
1101  FORMAT(A,1X,A,1X,'RELA',3X,   F7.2,' % VALE:',I9)
1102  FORMAT(A,1X,A,1X,'RELA',1X, 1PE9.2,' % VALE:',I9)
1104  FORMAT(A,1X,A,1X,'ABSO',3X,   F7.3,'   VALE:',I9)
1105  FORMAT(A,1X,A,1X,'ABSO',3X,   F7.2,'   VALE:',I9)
1106  FORMAT(A,1X,A,1X,'ABSO',1X, 1PE9.2,'   VALE:',I9)
1110  FORMAT(A     ,1X,'TOLE',3X,   F7.3,' % REFE:',I9)
1112  FORMAT(A     ,1X,'TOLE',1X, 1PE9.2,' % REFE:',I9)
1114  FORMAT(A     ,1X,'TOLE',3X,   F7.3,'   REFE:',I9)
1116  FORMAT(A     ,1X,'TOLE',1X, 1PE9.2,'   REFE:',I9)
C
1200  FORMAT(A,1X,A,1X,'RELA',3X,   F7.3,' % VALE:',1P,D20.13,1X,D20.13)
1201  FORMAT(A,1X,A,1X,'RELA',3X,   F7.2,' % VALE:',1P,D20.13,1X,D20.13)
1202  FORMAT(A,1X,A,1X,'RELA',1X, 1PE9.2,' % VALE:',1P,D20.13,1X,D20.13)
1204  FORMAT(A,1X,A,1X,'ABSO',3X,   F7.3,'   VALE:',1P,D20.13,1X,D20.13)
1205  FORMAT(A,1X,A,1X,'ABSO',3X,   F7.2,'   VALE:',1P,D20.13,1X,D20.13)
1206  FORMAT(A,1X,A,1X,'ABSO',1X, 1PE9.2,'   VALE:',1P,D20.13,1X,D20.13)
1210  FORMAT(     A,1X,'TOLE',3X,   F7.3,' % REFE:',1P,D20.13,1X,D20.13)
1212  FORMAT(     A,1X,'TOLE',1X, 1PE9.2,' % REFE:',1P,D20.13,1X,D20.13)
1214  FORMAT(     A,1X,'TOLE',3X,   F7.3,'   REFE:',1P,D20.13,1X,D20.13)
1216  FORMAT(     A,1X,'TOLE',1X, 1PE9.2,'   REFE:',1P,D20.13,1X,D20.13)
C
      END
