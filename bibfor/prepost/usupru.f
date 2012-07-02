      SUBROUTINE USUPRU ( VUSURT, VUSURO, NBINST, PRUST )
      IMPLICIT NONE
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
C     CALCULE LA PROFONDEUR D'USURE
C
C IN  : VUSURT : VOLUME USE TUBE A CHAQUE INSTANT
C IN  : VUSURO : VOLUME USE OBSTACLE A CHAQUE INSTANT
C IN  : NBINST : NOMBRE D'INSTANTS
C OUT : PRUST  : PROFONDEUR D'USURE DU TUBE POUR CHAQUE INSTANT
C-----------------------------------------------------------------------
      INCLUDE 'jeveux.h'
      REAL*8       LSUP, VUSURT(*), VUSURO(*), PRUST(*), PARA(7)
      CHARACTER*4  CRIT
      CHARACTER*24 TYPE, TYP1, TYP2
      INTEGER      IARG
C
C-----------------------------------------------------------------------
      INTEGER I ,IRE1 ,IRE2 ,IRET ,N1 ,N2 ,N3 
      INTEGER N4 ,N5 ,NBINST 
      REAL*8 AIMP ,ANGL ,CST1 ,CST2 ,DE ,DEPI ,DES3 
      REAL*8 DES5 ,DF ,EPSI ,R8DEPI ,R8DGRD ,RAPP ,RAYO 
      REAL*8 RAYT ,RESU ,UN ,UNS3 ,UNS5 ,V1 ,V2 
      REAL*8 VULIM ,X1 ,X11 ,X2 ,XLA ,ZERO 
C-----------------------------------------------------------------------
      ZERO = 0.D0
      UN   = 1.D0
      DE   = 2.D0
      UNS3 = UN / 3.D0
      DES3 = DE / 3.D0
      UNS5 = UN / 5.D0
      DES5 = DE / 5.D0
      DEPI = R8DEPI()
      CRIT = 'RELA'
      EPSI = 1.D-06
      PARA(1) = ZERO
      PARA(2) = ZERO
      PARA(3) = ZERO
      PARA(4) = ZERO
      PARA(5) = ZERO
      PARA(6) = ZERO
      PARA(7) = ZERO
C
      CALL GETVTX(' ','CONTACT',1,IARG,1,TYPE,N1)
C
C     --- TUBE - BARRE ANTI VIBRATOIRE ---
      IF ( TYPE(1:8) .EQ. 'TUBE_BAV' ) THEN
         CALL GETVR8(' ','RAYON_MOBILE' ,1,IARG,1,RAYT,N1)
         CALL GETVR8(' ','LARGEUR_OBST' ,1,IARG,1,LSUP,N2)
         CALL GETVR8(' ','ANGL_INCLI'   ,1,IARG,1,ANGL,N3)
         CALL GETVR8(' ','ANGL_IMPACT'  ,1,IARG,1,AIMP,N4)
         IF ( N4 .NE. 0 ) THEN
            RAPP = COS ( AIMP * R8DGRD() )
         ELSE
            RAPP = UN
         ENDIF
         IF ( N3 .EQ. 0 ) THEN
            CST1 = ( UN / ( DE * RAYT ) ) ** UNS3
            CST2 = 3.D0 / ( 4.D0 * LSUP )
            DO 10 I = 1,NBINST
               V1 = VUSURT(I)*RAPP + VUSURO(I)*RAPP
               V2 = VUSURT(I)*RAPP / V1
               PRUST(I) = V2 * CST1 * ( ( CST2 * V1 ) ** DES3 )
 10         CONTINUE
         ELSE
            ANGL = ANGL * R8DGRD()
            XLA  = LSUP * ANGL
            CST1 = ( UN / ( DE * RAYT ) ) ** UNS5
            CST2 = 15.D0 * ANGL / 8.D0
            PARA(1) = RAYT
            PARA(2) = LSUP
            PARA(3) = ANGL
            X1 = XLA
            X2 = RAYT
            DO 12 I = 1,NBINST
               V1 = VUSURT(I)*RAPP + VUSURO(I)*RAPP
               V2 = VUSURT(I)*RAPP / V1
               PRUST(I) = V2 * CST1 * ( ( CST2 * V1 ) ** DES5 )
               IF ( PRUST(I) .GT. XLA ) THEN
                  PARA(4) = VUSURT(I)*RAPP
                  PARA(5) = VUSURO(I)*RAPP
                  IF ( PRUST(I) .GE. X2 ) THEN
                   CALL U2MESS('A','PREPOST4_83')
                     PRUST(I) = 9999.D0
                     GOTO 12
                  ENDIF
                  CALL USUNEW ( TYPE,PARA,CRIT,EPSI,X1,X2,RESU,IRET)
                  IF ( IRET .EQ. 0 ) THEN
                     PRUST(I) = RESU
                     X1 = RESU
                  ELSE
                     PRUST(I) = 9999.D0
                  ENDIF
               ENDIF
 12         CONTINUE
         ENDIF
C
C     --- TUBE - TROU CIRCULAIRE ---
      ELSEIF ( TYPE(1:12) .EQ. 'TUBE_ALESAGE' ) THEN
         CALL GETVR8(' ','RAYON_MOBILE'    ,1,IARG,1,RAYT,N1)
         CALL GETVR8(' ','RAYON_OBST'      ,1,IARG,1,RAYO,N2)
         CALL GETVR8(' ','LARGEUR_OBST'    ,1,IARG,1,LSUP,N3)
         CALL GETVR8(' ','ANGL_INCLI'      ,1,IARG,1,ANGL,N4)
         IF ( N4 .NE. 0 ) ANGL = ANGL * R8DGRD()
         PARA(1) = RAYT
         PARA(2) = RAYO
         PARA(3) = LSUP
         PARA(4) = ANGL
         IF ( N2 .EQ. 0 ) THEN
            DO 20 I = 1,NBINST
               PRUST(I) = VUSURT(I) / ( DEPI * LSUP * RAYT )
 20         CONTINUE
         ELSE
           X1 = ZERO
           X2 = RAYT
           IF ( N4 .EQ. 0 ) THEN
             DO 22 I = 1,NBINST
               PARA(5) = VUSURT(I)
               PARA(6) = VUSURO(I)
               CALL USUBIS ( TYPE,PARA,CRIT,EPSI,X1,X2,RESU,IRET)
               IF ( IRET .EQ. 0 ) THEN
                 IF ( RESU .GE. X2 ) THEN
                   CALL U2MESS('A','PREPOST4_83')
                    PRUST(I) = 9999.D0
                    GOTO 22
                 ENDIF
                 PRUST(I) = RESU
                 X1 = RESU
               ELSE
                 PRUST(I) = 9999.D0
               ENDIF
 22          CONTINUE
           ELSE
C            --- CAS 3 OU D < L * THETA ---
             TYP1 = 'TUBE_ALESAG_3A'
C            --- CAS 3 OU D > L * THETA ---
             TYP2 = 'TUBE_ALESAG_3B'
C
             XLA  = LSUP * ANGL
             X1 = ZERO
             X2 = DE * RAYO
             DO 24 I = 1,NBINST
                PARA(5) = VUSURT(I)
                PARA(6) = VUSURO(I)
                CALL USUBIS ( TYP1,PARA,CRIT,EPSI,X1,X2,RESU,IRE1)
                IF ( IRE1 .EQ. 0 ) THEN
                  IF ( RESU .GT. XLA ) THEN
                    CALL USUBIS ( TYP2,PARA,CRIT,EPSI,X1,X2,RESU,IRE2)
                    IF ( IRE2 .EQ. 0 ) THEN
                      PRUST(I) = RESU
                      X1 = RESU
                    ELSE
                      PRUST(I) = 9999.D0
                    ENDIF
                  ELSE
                    PRUST(I) = RESU
                    X1 = RESU
                  ENDIF
                ELSE
                  PRUST(I) = 9999.D0
                ENDIF
 24          CONTINUE
           ENDIF
         ENDIF
C
C     --- TUBE - TROU QUADRIFOLIE OU TRIFOLIE ---
      ELSEIF ( TYPE(1:11) .EQ. 'TUBE_4_ENCO' .OR.
     &         TYPE(1:11) .EQ. 'TUBE_3_ENCO' ) THEN
         CALL GETVR8(' ','RAYON_MOBILE'    ,1,IARG,1,PARA(1),N1)
         CALL GETVR8(' ','RAYON_OBST'      ,1,IARG,1,PARA(2),N2)
         CALL GETVR8(' ','LARGEUR_OBST'    ,1,IARG,1,PARA(3),N3)
         CALL GETVR8(' ','ANGL_INCLI'      ,1,IARG,1,PARA(4),N4)
         CALL GETVR8(' ','ANGL_ISTHME'     ,1,IARG,1,PARA(7),N5)
         IF ( N4 .NE. 0 ) PARA(4) = PARA(4) * R8DGRD()
         PARA(7) = PARA(7) * R8DGRD()
         X1 = ZERO
         X2 = PARA(1)
         IF ( N4 .EQ. 0 ) THEN
            DO 30 I = 1,NBINST
               IF ( TYPE(1:11) .EQ. 'TUBE_4_ENCO' ) THEN
                 PARA(5) = VUSURT(I) / DE
                 PARA(6) = VUSURO(I) / DE
               ELSE
                 PARA(5) = VUSURT(I)
                 PARA(6) = VUSURO(I)
               ENDIF
               CALL USUBIS ( TYPE,PARA,CRIT,EPSI,X1,X2,RESU,IRET)
               IF ( IRET .EQ. 0 ) THEN
                 IF ( RESU .GE. X2 ) THEN
                   CALL U2MESS('A','PREPOST4_83')
                    PRUST(I) = 9999.D0
                    GOTO 30
                 ENDIF
                 PRUST(I) = RESU
                 X1 = RESU
               ELSE
                 PRUST(I) = 9999.D0
               ENDIF
 30         CONTINUE
         ELSE
C           --- CAS 2 OU D < L * THETA ---
            TYP1 = 'TUBE_ENCO_2A'
C           --- CAS 2 OU D > L * THETA ---
            TYP2 = 'TUBE_ENCO_2B'
C
            XLA  = PARA(3) * PARA(4)
            X1 = ZERO
            X2 = DE * PARA(2)
            DO 32 I = 1,NBINST
               IF ( TYPE(1:11) .EQ. 'TUBE_4_ENCO' ) THEN
                 PARA(5) = VUSURT(I) / DE
                 PARA(6) = VUSURO(I) / DE
               ELSE
                 PARA(5) = VUSURT(I)
                 PARA(6) = VUSURO(I)
               ENDIF
               CALL USUBIS ( TYP1,PARA,CRIT,EPSI,X1,X2,RESU,IRE1)
               IF ( IRE1 .EQ. 0 ) THEN
                 IF ( RESU .GT. XLA ) THEN
                   CALL USUBIS ( TYP2,PARA,CRIT,EPSI,X1,X2,RESU,IRE2)
                   IF ( IRE2 .EQ. 0 ) THEN
                     PRUST(I) = RESU
                     X1 = RESU
                   ELSE
                     PRUST(I) = 9999.D0
                   ENDIF
                 ELSE
                   PRUST(I) = RESU
                   X1 = RESU
                 ENDIF
               ELSE
                 PRUST(I) = 9999.D0
               ENDIF
 32         CONTINUE
         ENDIF
C
C     --- TUBE - TUBE ---
      ELSEIF ( TYPE(1:9) .EQ. 'TUBE_TUBE' ) THEN
         CALL GETVR8(' ','RAYON_MOBILE'    ,1,IARG,1,RAYT,N1)
         CALL GETVR8(' ','ANGL_INCLI'      ,1,IARG,1,ANGL,N2)
         CST1 = ( UN / ( DE * RAYT ) ) ** UNS5
         CST2 = 15.D0 * ANGL * R8DGRD() / 8.D0
         DO 40 I = 1,NBINST
            PRUST(I) = CST1 * ( ( CST2 * VUSURT(I) ) ** DES5 )
 40      CONTINUE
C
C     --- GRAPPE - ALESAGE ---
      ELSEIF ( TYPE(1:14) .EQ. 'GRAPPE_ALESAGE' ) THEN
         CALL GETVR8(' ','RAYON_MOBILE'    ,1,IARG,1,PARA(1),N1)
         CALL GETVR8(' ','RAYON_OBST'      ,1,IARG,1,PARA(2),N2)
         X11 = ZERO
         X2 = PARA(2)
         DO 50 I = 1,NBINST
            PRUST(I) = 9999.D0
            PARA(5) = VUSURT(I)
            CALL USUBIS ( TYPE,PARA,CRIT,EPSI,X11,X2,RESU,IRET)
            IF ( IRET .EQ. 0 ) THEN
               IF ( RESU .GE. X2 ) THEN
                  CALL U2MESS('A','PREPOST4_83')
                  GOTO 50
               ENDIF
               PRUST(I) = RESU
               X11 = RESU
            ENDIF
 50      CONTINUE
C
C     --- GRAPPE - 1 ENCOCHE ---
C     --- GRAPPE - 2 ENCOCHE ---
      ELSEIF ( TYPE(1:13) .EQ. 'GRAPPE_1_ENCO' .OR.
     &         TYPE(1:13) .EQ. 'GRAPPE_2_ENCO' ) THEN
         IF ( TYPE(1:13) .EQ. 'GRAPPE_2_ENCO' ) THEN
            PARA(1) = -48.89D+03 / 11.D0
            PARA(2) = 106.03D0   / 11.D0
            PARA(3) =  -0.88D-03 / 11.D0
         ELSE
            PARA(1) = -0.5D0 * 48.89D+03 / 11.D0
            PARA(2) = 0.5D0 * 106.03D0   / 11.D0
            PARA(3) =  -0.5D0 * 0.88D-03 / 11.D0
         ENDIF
         X11 = ZERO
         X2  = 0.00144D0
         PARA(5) = ZERO
         CALL USUFON ( TYPE,PARA,X2,VULIM,DF)
         DO 60 I = 1,NBINST
            PRUST(I) = 9999.D0
            PARA(5) = VUSURT(I)
            IF ( VUSURT(I) .GT. VULIM ) GOTO 62
            CALL USUNEW ( TYPE,PARA,CRIT,EPSI,X11,X2,RESU,IRET)
            IF ( IRET .EQ. 0 ) THEN
               IF ( RESU .GE. X2 ) THEN
                  CALL U2MESS('A','PREPOST4_83')
                  GOTO 62
               ENDIF
               PRUST(I) = RESU
               X11 = RESU
            ENDIF
 62         CONTINUE
 60      CONTINUE
C
      ENDIF
C
      END
