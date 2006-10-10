      SUBROUTINE UTITES (LABEL1,LABEL2,TYPE,NBREF,REFI,REFR,REFC,
     +                          VALI,VALR,VALC,EPSI,CRIT,IFIC,SSIGNE)
      IMPLICIT   NONE
      INTEGER                          VALI,NBREF,REFI(NBREF),     IFIC
      CHARACTER*(*)      LABEL1,LABEL2,TYPE,        CRIT,     SSIGNE
      REAL*8                          VALR,REFR(NBREF),EPSI
      COMPLEX*16                          VALC,REFC(NBREF)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 10/10/2006   AUTEUR REZETTE C.REZETTE 
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
C  IN  : I   : NBREF  : NOMBRE DE VALEURS DE REFERENCE (=1 SOUVENT)
C  IN  : R8  : REFR   : VALEUR(S) REELLE(S) DE REFERENCE
C  IN  : C16 : REFC   : VALEUR(S) COMPLEXE(S) DE REFERENCE
C  IN  : R8  : VALR   : VALEUR REELLE A TESTER ( ASTER )
C  IN  : C16 : VALC   : VALEUR COMPLEXE A TESTER ( ASTER )
C  IN  : K8  : CRIT   : PRECISION 'RELATIVE' OU 'ABSOLUE'
C  IN  : R8  : EPSI   : PRECISION ESPEREE
C  IN  : I   : IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
C  OUT :                IMPRESSION SUR LISTING
C ----------------------------------------------------------------------
      INTEGER I, IMIN, MINVI, TMPI
      CHARACTER*4  TESTOK, RELA
      CHARACTER*12 K12
      CHARACTER*17 K17
      REAL*8       ERR, ZERO, CENT, EPSR, TMPR, MINVR, MINVC, TMPC
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
         IF ( SSIGNE .EQ. 'OUI' )  VALR = ABS(VALR)
         MINVR=ABS(VALR-REFR(1))
         IMIN=1
         DO 10 I=1,NBREF-1
           TMPR=ABS(VALR-REFR(I+1))
           IF(TMPR.LT.MINVR)THEN
             TMPR=MINVR
             IMIN=I+1
           ENDIF
 10      CONTINUE
         IF ( RELA .EQ. 'RELA' ) THEN
            LOK = ( ABS(VALR-REFR(IMIN)) .LE. EPSI * ABS(REFR(IMIN)) )
            IF ( REFR(IMIN) .NE. ZERO ) THEN
               ERR = (VALR - REFR(IMIN)) / REFR(IMIN) *CENT
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
               WRITE(IFIC,1010) K17, EPSR, REFR(IMIN)
            ELSE
               WRITE(IFIC,1012) K17, EPSR, REFR(IMIN)
            ENDIF
         ELSE
            LOK = ( ABS(VALR - REFR(IMIN)) .LE. EPSI )
            ERR =       VALR - REFR(IMIN)
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
               WRITE(IFIC,1014) K17, EPSI, REFR(IMIN)
            ELSE
               WRITE(IFIC,1016) K17, EPSI, REFR(IMIN)
            ENDIF
         ENDIF
C
      ELSEIF ( TYPE(1:1) .EQ. 'I' ) THEN
         MINVI=ABS(VALI-REFI(1))
         IMIN=1
         DO 20 I=1,NBREF-1
           TMPI=ABS(VALI-REFI(I+1))
           IF(TMPI.LT.MINVI)THEN
             TMPI=MINVI
             IMIN=I+1
           ENDIF
 20      CONTINUE
         IF ( SSIGNE .EQ. 'OUI' ) VALI = ABS(VALI) 
         ERR = DBLE( VALI - REFI(IMIN) )
         IF ( RELA .EQ. 'RELA' ) THEN
            LOK = ( ABS( ERR ) .LE. EPSI*ABS(REFI(IMIN)) )
            IF ( REFI(IMIN) .NE. 0 ) THEN
               ERR = ( VALI - REFI(IMIN) ) / REFI(IMIN) *CENT
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
               WRITE(IFIC,1110) K17, EPSR, REFI(IMIN)
            ELSE
               WRITE(IFIC,1112) K17, EPSR, REFI(IMIN)
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
               WRITE(IFIC,1114) K17, EPSI, REFI(IMIN)
            ELSE
               WRITE(IFIC,1116) K17, EPSI, REFI(IMIN)
            ENDIF
         ENDIF
C
      ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
         MINVC=ABS(VALC-REFC(1))
         IMIN=1
         DO 30 I=1,NBREF-1
           TMPC=ABS(VALC-REFC(I+1))
           IF(TMPC.LT.MINVC)THEN
             TMPC=MINVC
             IMIN=I+1
           ENDIF
 30      CONTINUE
         IF ( RELA .EQ. 'RELA' ) THEN
            LOK = ( ABS(VALC-REFC(IMIN)) .LE. EPSI * ABS(REFC(IMIN)))
            IF ( REFC(IMIN) .NE. ZEROC ) THEN
               ERR = ABS(VALC - REFC(IMIN)) /  ABS(REFC(IMIN)) *CENT
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
               WRITE(IFIC,1210) K17, EPSR, REFC(IMIN)
            ELSE
               WRITE(IFIC,1212) K17, EPSR, REFC(IMIN)
            ENDIF
         ELSE
            LOK = ( ABS(VALC - REFC(IMIN)) .LE. EPSI )
            ERR =   ABS(VALC - REFC(IMIN))
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
               WRITE(IFIC,1214) K17, EPSI, REFC(IMIN)
            ELSE
               WRITE(IFIC,1216) K17, EPSI, REFC(IMIN)
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
