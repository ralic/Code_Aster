      SUBROUTINE USUVUS ( PUUSUR, VUSUR, NBINST, TEMPS, ISUPP,
     &                            NBPT, FN, VG, IRET )
      IMPLICIT REAL *8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C-----------------------------------------------------------------------
C     CALCULE LE VOLUME USE
C
C IN  : PUUSUR : PUISSANCE USURE
C OUT : VUSUR  : VOLUME USE
C IN  : NBINST : NOMBRE D'INSTANTS
C IN  : TEMPS  : LES INSTANTS
C VAR : ISUPP  : = 1, CALCULE LE VOLUME USE MOBILE
C                = 2, CALCULE LE VOLUME USE OBSTACLE
C                NE CALCULE PAS LE VOLUME USE OBSTACLE, ISUPP = 0
C-----------------------------------------------------------------------
      REAL*8       VUSUR(*), TEMPS(*), PARA(7), FN(*), VG(*)
      CHARACTER*8  K8B
      CHARACTER*24 LOI, MATE
C
      IFIRES = IUNIFI('RESULTAT')
C
      CALL GETVTX(' ','LOI_USURE',1,1,1,LOI,N1)
      IRET = 0
C
C **********************************************************************
C                 M O D E L E     A R C H A R D
C **********************************************************************
C
      IF ( LOI(1:7) .EQ. 'ARCHARD' ) THEN
         IF ( ISUPP .EQ. 1 ) THEN
            WRITE(IFIRES,1000)
            CALL GETVR8('MOBILE','COEF_USURE',1,1,1,XK,N1)
            IF ( N1 .EQ. 0 ) THEN
               CALL GETVTX(' ','MATER_USURE',1,1,1,MATE,N2)
               CALL USUBAN( MATE, ISUPP, PARA, IRET )
               XK = PARA(1)
            ENDIF
            WRITE(IFIRES,2100)
         ELSEIF ( ISUPP .EQ. 2 ) THEN
            CALL GETVR8('OBSTACLE','COEF_USURE',1,1,1,XK,N1)
            IF ( N1 .EQ. 0 ) THEN
               CALL GETVTX(' ','USURE_OBST',1,1,1,K8B,N2)
               IF ( K8B(1:3) .EQ. 'OUI' ) THEN
                  CALL GETVTX(' ','MATER_USURE',1,1,1,MATE,N3)
                  CALL USUBAN ( MATE, ISUPP, PARA, IRET )
                  XK = PARA(1)
               ELSE
                  ISUPP = 0
                  GOTO 9999
               ENDIF
            ENDIF
            WRITE(IFIRES,2200)
         ENDIF
         WRITE(IFIRES,2010) XK
         DO 10 I = 1,NBINST
            VUSUR(I) = XK * PUUSUR * TEMPS(I)
 10      CONTINUE
C
C **********************************************************************
C                 M O D E L E     K W U _ E P R I
C **********************************************************************
C
      ELSEIF ( LOI(1:8) .EQ. 'KWU_EPRI' ) THEN
         IF ( ISUPP .EQ. 1 ) THEN
            WRITE(IFIRES,1010)
            CALL GETVR8('MOBILE','COEF_USURE',1,1,1,PARA(1),N1)
            CALL GETVR8('MOBILE','COEF_FNOR' ,1,1,1,PARA(2),N2)
            CALL GETVR8('MOBILE','COEF_VTAN' ,1,1,1,PARA(3),N3)
            CALL GETVR8('MOBILE','COEF_K'    ,1,1,1,PARA(4),N4)
            CALL GETVR8('MOBILE','COEF_C'    ,1,1,1,PARA(5),N5)
            IF ( N4 .EQ. 0 ) PARA(4) =  5.D0
            IF ( N5 .EQ. 0 ) PARA(5) = 10.D0
            CALL GETVTX(' ','MATER_USURE',1,1,1,MATE,N6)
            IF ( N6 .NE. 0 ) THEN
               CALL USUBAN( MATE, ISUPP, PARA, IRET )
            ENDIF
            WRITE(IFIRES,2100)
         ELSEIF ( ISUPP .EQ. 2 ) THEN
            CALL GETVR8('OBSTACLE','COEF_USURE',1,1,1,PARA(1),N1)
            CALL GETVR8('OBSTACLE','COEF_FNOR' ,1,1,1,PARA(2),N2)
            CALL GETVR8('OBSTACLE','COEF_VTAN' ,1,1,1,PARA(3),N3)
            CALL GETVR8('OBSTACLE','COEF_K'    ,1,1,1,PARA(4),N4)
            CALL GETVR8('OBSTACLE','COEF_C'    ,1,1,1,PARA(5),N5)
            IF ( N4 .EQ. 0 ) PARA(4) =  5.D0
            IF ( N5 .EQ. 0 ) PARA(5) = 10.D0
            CALL GETVTX(' ','MATER_USURE',1,1,1,MATE,N6)
            IF ( N6 .NE. 0 ) THEN
               CALL GETVTX(' ','USURE_OBST',1,1,1,K8B,N2)
               IF ( K8B(1:3) .EQ. 'OUI' ) THEN
                  CALL USUBAN( MATE, ISUPP, PARA, IRET )
               ELSE
                  ISUPP = 0
                  GOTO 9999
               ENDIF
            ENDIF
            NN = N1 + N2 + N3 + N4 + N5
            IF ( NN .EQ. 0 ) THEN
               ISUPP = 0
               GOTO 9999
            ENDIF
            WRITE(IFIRES,2200)
         ENDIF
         WRITE(IFIRES,2010) PARA(1)
         WRITE(IFIRES,2050) PARA(3)
         WRITE(IFIRES,2060) PARA(2)
         WRITE(IFIRES,2070) PARA(4)
         WRITE(IFIRES,2080) PARA(5)
         CALL USUKWU ( NBPT, FN, VG, PARA, W, IRET )
         IF ( IRET .EQ. 10 ) THEN
            CALL U2MESS('F','PREPOST4_85')
         ENDIF
         DO 20 I = 1,NBINST
            VUSUR(I) = PARA(1) * W * PUUSUR * TEMPS(I)
 20      CONTINUE
C
C **********************************************************************
C                 M O D E L E     E D F _ M Z
C **********************************************************************
C
      ELSEIF ( LOI(1:6) .EQ. 'EDF_MZ' ) THEN
         IF ( ISUPP .EQ. 1 ) THEN
           WRITE(IFIRES,1020)
           CALL GETVR8('MOBILE','COEF_S'     ,1,1,1,XS,N1)
           CALL GETVR8('MOBILE','COEF_B'     ,1,1,1,XB,N2)
           CALL GETVR8('MOBILE','COEF_N'     ,1,1,1,XN,N3)
           CALL GETVR8('MOBILE','COEF_USURE' ,1,1,1,XA,N4)
           IF ( N1 .EQ. 0 ) XS = 1.14D-16
           IF ( N2 .EQ. 0 ) XB = 1.2D0
           IF ( N3 .EQ. 0 ) XN = 2.44D-08
           IF ( N4 .EQ. 0 ) XA = 1.D-13
           CALL GETVTX(' ','MATER_USURE',1,1,1,MATE,N5)
           IF ( N5 .NE. 0 ) THEN
              CALL USUBAN ( MATE, ISUPP, PARA, IRET )
              XS = PARA(1)
              XB = PARA(2)
              XN = PARA(3)
              XA = PARA(4)
           ENDIF
           WRITE(IFIRES,2100)
         ELSEIF ( ISUPP .EQ. 2 ) THEN
           CALL GETVR8('OBSTACLE','COEF_S'     ,1,1,1,XS,N1)
           CALL GETVR8('OBSTACLE','COEF_B'     ,1,1,1,XB,N2)
           CALL GETVR8('OBSTACLE','COEF_N'     ,1,1,1,XN,N3)
           CALL GETVR8('OBSTACLE','COEF_USURE' ,1,1,1,XA,N4)
           IF ( N1 .EQ. 0 ) XS = 1.14D-16
           IF ( N2 .EQ. 0 ) XB = 1.2D0
           IF ( N3 .EQ. 0 ) XN = 2.44D-08
           IF ( N4 .EQ. 0 ) XA = 1.D-13
           CALL GETVTX(' ','MATER_USURE',1,1,1,MATE,N5)
           IF ( N5 .NE. 0 ) THEN
              CALL GETVTX(' ','USURE_OBST',1,1,1,K8B,N6)
              IF ( K8B(1:3) .EQ. 'OUI' ) THEN
                 CALL USUBAN ( MATE, ISUPP, PARA, IRET )
                 XS = PARA(2)
                 XB = PARA(3)
                 XN = PARA(4)
                 XA = PARA(1)
              ELSE
                 ISUPP = 0
                 GOTO 9999
              ENDIF
           ENDIF
           CALL GETFAC('OBSTACLE',N6)
           NN = N1 + N2 + N3 + N4 + N5 + N6
           IF ( NN .EQ. 0 ) THEN
              ISUPP = 0
              GOTO 9999
           ENDIF
           WRITE(IFIRES,2200)
         ENDIF
         WRITE(IFIRES,2010) XA
         WRITE(IFIRES,2020) XS
         WRITE(IFIRES,2030) XB
         WRITE(IFIRES,2040) XN
         V0 = XA * ( PUUSUR ** XB )
         XD = XS / V0
         IF ( XD .GT. 1.D0 ) THEN
            IRET = 10
            CALL U2MESS('I','PREPOST4_86')
            CALL U2MESS('I','PREPOST4_87')
            GOTO 9999
         ENDIF
         X1 = ( 1.D0 - XD ) / XN
         DO 30 I = 1,NBINST
            T = TEMPS(I)
            VUSUR(I) = V0 * ( XD*T + X1*( 1.D0 - EXP(-XN*T) ) )
 30      CONTINUE
C
      ENDIF
C
 1000 FORMAT (/,'******* MODELE ARCHARD *******')
 1010 FORMAT (/,'******* MODELE KWU_EPRI *******')
 1020 FORMAT (/,'******* MODELE EDF_MZ *******')
 2100 FORMAT (/,'===> COEFFICIENT(S) UTILISE(S) POUR LE MOBILE :')
 2200 FORMAT (/,'===> COEFFICIENT(S) UTILISE(S) POUR L''OBSTACLE :')
 2010 FORMAT (1P,4X,'       COEFFICIENT D''USURE : ',E12.5)
 2020 FORMAT (1P,4X,'                     SEUIL : ',E12.5)
 2030 FORMAT (1P,4X,'                  EXPOSANT : ',E12.5)
 2040 FORMAT (1P,4X,'    TAUX DE RALENTISSEMENT : ',E12.5)
 2050 FORMAT (1P,4X,' COEFFICIENT DE GLISSEMENT : ',E12.5)
 2060 FORMAT (1P,4X,'      COEFFICIENT D''IMPACT : ',E12.5)
 2070 FORMAT (1P,4X,'               CONSTANTE K : ',E12.5)
 2080 FORMAT (1P,4X,'               CONSTANTE C : ',E12.5)
C
 9999 CONTINUE
      END
