      SUBROUTINE USOBE2 ( DIMOBS, VOLOBS, OBSUSE, RCRAY, RCARTE, SECT,
     &                    ARETE, ARETE2, NS, OBCONT, EPAIS, NOMT19 )
      IMPLICIT   NONE
      INTEGER             DIMOBS, NS
      REAL*8              VOLOBS(*), OBSUSE(*), RCRAY, RCARTE, SECT(*),
     &                    ARETE, ARETE2, EPAIS
      CHARACTER*8         OBCONT
      CHARACTER*19        NOMT19
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 11/10/2000   AUTEUR CIBHHLV L.VIVAN 
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
C     CALCULE LES VOLUMES USES D'OBSTACLE (GUIDAGE ENCO_2)
C ----------------------------------------------------------------------
C     ---- DEBUT DES COMMUNS JEVEUX ------------------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ---- FIN DES COMMUNS JEVEUX --------------------------------------
      INTEGER  USURE(10,5), NBUSUR, I, J, L, IPAS(10,3), IFM, NIV
      INTEGER  N, NC, NCO, IRAYO, ITHET, IREFE, NSS2
      REAL*8   OBS(20,3),  PAS(10,3), PARUSU(10,5), THETA
      REAL*8   ANSINI, ANSFIN, ANGARE, ANGDEB, ANGMAX, ANGFIN
      REAL*8   VOLUME, PROFON, A1, A2, B1, B2, RHO
      REAL*8   EPSI, XPOUR, ANGVA, DIF1, DIF2, TYPE
      REAL*8   PI, R8PI, RAD, R8DGRD, TABR(4)
      COMPLEX*16   C16B
      CHARACTER*4  T2
      CHARACTER*8  K8B, TABK(2)
      CHARACTER*16 NOPARA(7)
C
      DATA NOPARA / 'LIEU'    , 'TRACE'   , 'TYPE'    , 'ANGL_DEBUT', 
     +              'ANGL_FIN', 'ANGL_MAX', 'PROF_MAX' /
C-----------------------------------------------------------------------
C
      CALL INFNIV ( IFM, NIV )
C
      PROFON = 0.D0
      PI = R8PI( )
      RAD = R8DGRD( )
      NSS2 = NS / 2
C
      DO 110 I = 1 , NS
         DO 112 J = 1 , 5
            PARUSU(I,J) = 0.D0
            USURE(I,J) = 0.D0
 112     CONTINUE
         DO 114 J = 1 , 3
            IPAS(I,J) = 0
             PAS(I,J) = 0.D0
 114     CONTINUE
         OBS(I,1) = SECT(I)
         OBS(I,2) = SECT(I+1)
         OBS(I,3) = VOLOBS(I)
 110  CONTINUE
C
      CALL JEVEUO ( OBCONT//'           .VALR', 'L', IRAYO )
      CALL JEVEUO ( OBCONT//'           .VALT', 'L', ITHET )
      CALL JEVEUO ( OBCONT//'           .REFE', 'L', IREFE )
      CALL JELIRA ( OBCONT//'           .VALT', 'LONMAX', NCO, K8B )
C
      J = 0
      N = 1
 10   CONTINUE
         I = 0
 15      CONTINUE
            IF ( OBS(N,3) .NE. 0.D0 ) THEN
               IF ( N .EQ. NS ) THEN
                  N = N + 1
                  I = I + 1
                  GOTO 20
               ENDIF
               N = N+1
               I = I+1
               IF (I.GE.3) GOTO 20
               GOTO 15
            ENDIF
            IF ( I .EQ. 0 ) THEN
               IF ( N .EQ. NS )  GOTO 25
               N = N+1
               GOTO 10
            ENDIF
 20      CONTINUE
         J = J + 1
         USURE(J,1) = J
         USURE(J,2) = I
         USURE(J,3) = N - I
         USURE(J,4) = N - 1
         IF ( N .EQ. (NS+1) )  GOTO 25
         GOTO 10        
 25   CONTINUE
      NBUSUR = J
C
C     DETERMINATION DES TYPES D'USURE :     1 : LUNULE
C     -------------------------------       2 : L+V  1E ENCOCHE
C                                           3 : V    1E ENCOCHE
C                                           4 : L+V  2E ENCOCHE
C                                           5 : V    2E ENCOCHE
C
      DO 120 I = 1,NBUSUR
         IF ((USURE(I,3).EQ.1).OR.(USURE(I,4).EQ.NS)) THEN
            IF (USURE(I,2).EQ.1) THEN
               USURE(I,5) = 3
            ELSE
               USURE(I,5) = 2
            ENDIF
         ELSE
            IF ( (USURE(I,4).EQ.NSS2    ) .OR.
     &           (USURE(I,3).EQ.(NSS2+1)) ) THEN
               IF ( USURE(I,2) .EQ. 1 ) THEN
                  USURE(I,5) = 5
               ELSE
                  USURE(I,5) = 4
               ENDIF
            ELSE
               USURE(I,5) = 1
            ENDIF
         ENDIF
 120  CONTINUE
C
C     CALCUL DES PARAMETRES DES USURES :
C     --------------------------------
C
      DO 130 I = 1 , NBUSUR
         VOLUME = 0.D0
C
C        CAS LUNULE :
C        ----------
         IF ( USURE(I,5) .EQ.1 ) THEN
            DO 132 J = USURE(I,3) , USURE(I,4)
               VOLUME = VOLUME + OBS(J,3)
 132        CONTINUE
            ANSINI = OBS(USURE(I,3),1)
            ANSFIN = OBS(USURE(I,4),2)
            CALL LUNULE ( RCRAY, RCARTE, ANGDEB, ANGFIN, ANGMAX, ANSINI,
     &                    ANSFIN, PROFON, VOLUME, EPAIS )
            TYPE = 1.D0
C
C        CAS L+V  1E ENCOCHE :
C        -------------------
         ELSEIF ( USURE(I,5) .EQ. 2 ) THEN
            DO 134 J = USURE(I,3), USURE(I,4)
               VOLUME = VOLUME + OBS(J,3)
 134        CONTINUE
            IF (USURE(I,3).EQ.1) THEN
               ANGARE = ARETE
            ELSE
               ANGARE = 360.D0 - ARETE
            ENDIF
            ANGVA = 11.D0
            CALL VEOBST ( ARETE, RCARTE, ANGDEB, ANGFIN, ANGVA, ANGARE,
     &                    ANGMAX, PROFON, VOLUME, EPAIS )
            TYPE = 2.D0
C
C        CAS V  1E ENCOCHE :
C        -----------------
         ELSEIF ( USURE(I,5) .EQ. 3 ) THEN
            VOLUME = OBS(USURE(I,3),3)
            IF ( USURE(I,3) .EQ. 1 ) THEN
               ANGARE = ARETE
            ELSE
               ANGARE = 360.D0 - ARETE
            ENDIF
            ANGVA = 28.D0
            CALL VEOBST ( ARETE, RCARTE, ANGDEB, ANGFIN, ANGVA, ANGARE,
     &                    ANGMAX, PROFON, VOLUME, EPAIS )
            TYPE = 2.D0
C
C        CAS L+V  2E ENCOCHE :
C        -------------------
        ELSEIF ( USURE(I,5) .EQ. 4 ) THEN
            DO 136 J = USURE(I,3) , USURE(I,4)
               VOLUME = VOLUME + OBS(J,3)
 136        CONTINUE
            IF ( USURE(I,4) .EQ. NSS2 ) THEN
               ANGARE = ARETE2
            ELSE
               ANGARE = 360.D0 - ARETE2
            ENDIF
            ANGVA = 11.D0
            CALL VEOBST ( ARETE, RCARTE, ANGDEB, ANGFIN, ANGVA, ANGARE,
     &                    ANGMAX, PROFON, VOLUME, EPAIS )
            TYPE = 3.D0
C
C        CAS V  2E ENCOCHE :
C        -----------------
        ELSEIF ( USURE(I,5) .EQ. 5 ) THEN
            VOLUME = OBS(USURE(I,3),3)
            IF ( USURE(I,4) .EQ. NSS2 ) THEN
               ANGARE = ARETE2
            ELSE
               ANGARE = 360.D0 - ARETE2
            ENDIF
            ANGVA = 28.D0
            CALL VEOBST ( ARETE, RCARTE, ANGDEB, ANGFIN, ANGVA, ANGARE,
     &                    ANGMAX, PROFON, VOLUME, EPAIS )
            TYPE = 3.D0
         ENDIF
         PARUSU(I,1) = ANGDEB
         PARUSU(I,2) = ANGMAX
         PARUSU(I,3) = ANGFIN
         PARUSU(I,4) = PROFON
         PARUSU(I,5) = TYPE
 130  CONTINUE
C
C     PAS DE DESCRIPTION DANS LES ZONES USEES :
C     --------------------------------------- 
C
      CALL USPAS2 ( NBUSUR, PARUSU, IPAS, PAS )
C
      TABK(1) = 'OBST'
      TABK(2) = 'TYPE'
      IF ( NIV .GE. 2 ) WRITE(IFM,1000)
      L = 0
      DO 140 I = 1 , NBUSUR
         IF ( USURE(I,5) .EQ. 1 ) THEN
            T2 = '   L'
         ELSEIF ( USURE(I,5) .EQ. 2 ) THEN
            T2 = 'LV_1'
         ELSEIF ( USURE(I,5) .EQ. 3 ) THEN
            T2 = ' V_1'
         ELSEIF ( USURE(I,5) .EQ. 4 ) THEN
            T2 = 'LV_2'
         ELSEIF ( USURE(I,5) .EQ. 5 ) THEN
            T2 = ' V_2'
         ELSE
            T2 = '????'
         ENDIF
         L = L + 1
         TABK(2) = T2
         TABR(1) = PARUSU(I,1)
         TABR(2) = PARUSU(I,3)
         TABR(3) = PARUSU(I,2)
         TABR(4) = PARUSU(I,4)
         CALL TBAJLI ( NOMT19, 7, NOPARA, L, TABR, C16B, TABK, 0 )
         IF ( NIV .GE. 2 ) WRITE(IFM,1010) T2, PARUSU(I,1), 
     +                     PARUSU(I,3), PARUSU(I,2), PARUSU(I,4)
 140  CONTINUE
C
C     TRACE DES USURES :
C     ------------------  
C
      L = 0
      THETA = 0.D0
      NC = 0
      IF ( NBUSUR .EQ. 0 ) THEN
         NC = NC + 1
         IF ( NC .GT. NCO ) GOTO 90
         L = L + 1
         OBSUSE(2*L-1) = ZR(ITHET+NC-1) / RAD
         OBSUSE(2*L  ) = ZR(IRAYO+NC-1)
      ENDIF
      IF ( NBUSUR .NE. 0 ) THEN
 30      CONTINUE
            NC = NC + 1
            IF ( NC .GT. NCO ) GOTO 90
            THETA = ZR(ITHET+NC-1) / RAD
            IF ( THETA .GE. PARUSU(1,1) )  GOTO 50
            L = L + 1
            OBSUSE(2*L-1) = ZR(ITHET+NC-1) / RAD
            OBSUSE(2*L  ) = ZR(IRAYO+NC-1)
         GOTO 30
      ENDIF     
 50   CONTINUE
C
      EPSI = 1.D-10
      IF ( ABS(PROFON) .GT. EPSI )  XPOUR = 0.9D0*PROFON
C
      DO 150 I = 1 , NBUSUR
C
         IF ( PARUSU(I,5) .EQ. 1.D0 ) THEN
            A1 = PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,1) )
            B1 = RCARTE - A1*PARUSU(I,1)
            DO 152 J = 1 , IPAS(I,1)
               RHO = A1*THETA + B1
               IF ( ABS(PROFON) .GT. EPSI ) THEN
                  IF ( (RHO-RCARTE) .GE. XPOUR )  RHO = OBSUSE(2*L)
               ENDIF
               DIF1 = 3.D-3 / SIN(THETA*PI/180.D0) - RHO
               L = L + 1
               IF ( DIF1 .GE. 0.D0 ) THEN
                  OBSUSE(2*L-1) = THETA
                  OBSUSE(2*L)   = RHO + DIF1
               ELSE
                  OBSUSE(2*L-1) = THETA
                  OBSUSE(2*L)   = RHO
               ENDIF
               THETA = THETA + PAS(I,1)
 152        CONTINUE  
C
            A2 = PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,3) )
            B2 = RCARTE - A2*PARUSU(I,3)
            DO 154 J = 1 , IPAS(I,2)
               RHO = A2*THETA + B2
               IF ( ABS(PROFON) .GT. EPSI ) THEN
                  IF ( (RHO-RCARTE) .GE. XPOUR ) RHO = OBSUSE(2*L)
               ENDIF
               DIF2 = -3.D-3 / SIN(THETA*PI/180.D0) - RHO
               L = L + 1
               IF ( DIF2 .GE. 0.D0 ) THEN
                  OBSUSE(2*L-1) = THETA
                  OBSUSE(2*L)   = RHO + DIF2
               ELSE
                  OBSUSE(2*L-1) = THETA
                  OBSUSE(2*L)   = RHO
               ENDIF
               THETA = THETA + PAS(I,2)
 154        CONTINUE
C
         ELSEIF ( PARUSU(I,5) .EQ. 2.D0 ) THEN
           IF ( PARUSU(I,1) .LT. 90.D0 ) THEN
               THETA = PARUSU(I,1)
               DO 156 J = 1 , IPAS(I,3)
                  RHO = RCARTE + PARUSU(I,4) / (ARETE-PARUSU(I,3))
     &                                       * (THETA-PARUSU(I,3))
                  L = L + 1
                  OBSUSE(2*L-1) = THETA
                  OBSUSE(2*L)   = RHO
                  THETA = THETA + PAS(I,3)
 156           CONTINUE
            ENDIF
            IF ( PARUSU(I,3) .GT. 270.D0 ) THEN
               THETA = PARUSU(I,1)
               DO 158 J = 1 , IPAS(I,3)
                 RHO = RCARTE + PARUSU(I,4) / (360.D0-ARETE-PARUSU(I,1))
     &                                      * (THETA-PARUSU(I,1))
                  L = L + 1
                  OBSUSE(2*L-1) = THETA
                  OBSUSE(2*L)   = RHO
                  THETA = THETA + PAS(I,3)
 158           CONTINUE
            ENDIF
C
         ELSEIF ( PARUSU(I,5) .EQ. 3.D0 ) THEN
            IF ( PARUSU(I,3) .LT. 180.D0 ) THEN
               THETA = PARUSU(I,1)
               DO 160 J = 1 , IPAS(I,3)
                  RHO = RCARTE + PARUSU(I,4) / (ARETE2-PARUSU(I,1))
     &                                       * (THETA-PARUSU(I,1))
                  L = L + 1
                  OBSUSE(2*L-1) = THETA
                  OBSUSE(2*L)   = RHO
                  THETA = THETA + PAS(I,3)
 160           CONTINUE
            ELSEIF (PARUSU(I,1).GT.180.D0) THEN
               THETA = PARUSU(I,1)
               DO 162 J = 1 , IPAS(I,3)
                RHO = RCARTE + PARUSU(I,4) / (360.D0-ARETE2-PARUSU(I,3))
     &                                     * (THETA-PARUSU(I,3))
                  L = L + 1
                  OBSUSE(2*L-1) = THETA
                  OBSUSE(2*L)   = RHO
                  THETA = THETA + PAS(I,3)
 162           CONTINUE
            ENDIF
         ENDIF
C
         IF ( I .NE. NBUSUR ) THEN
 60         CONTINUE
               NC = NC + 1
               IF ( NC .GT. NCO )  GOTO 90
               THETA = ZR(ITHET+NC-1) / RAD
               IF ( THETA .GE. PARUSU(I+1,1) )  GOTO 70
               IF ( THETA .GT. PARUSU(I,3) ) THEN
                  L = L + 1
                  OBSUSE(2*L-1) = ZR(ITHET+NC-1) / RAD
                  OBSUSE(2*L)   = ZR(IRAYO+NC-1)
               ENDIF
            GOTO 60
 70         CONTINUE
         ENDIF
 150  CONTINUE
C        
 80   CONTINUE
         NC = NC + 1
         IF ( NC .GT. NCO )  GOTO 90
         THETA = ZR(ITHET+NC-1) / RAD
         IF ( THETA .GT. 360.D0 )  GOTO 90
         IF ( THETA.GT.PARUSU(NBUSUR,3) .OR. NBUSUR.EQ.0 ) THEN
            L = L + 1
            OBSUSE(2*L-1) = ZR(ITHET+NC-1) / RAD
            OBSUSE(2*L)   = ZR(IRAYO+NC-1)
         ENDIF
      GOTO 80
C
 90   CONTINUE
      DIMOBS = L
C
 1000 FORMAT('==> IMPRESSION DE PARAMETRES "OBST" PAR SECTEUR USE:',/,
     +       ' TYPE     ANGL_DEBUT      ANGL_FIN',
     +       '      ANGLE_MAX      PROFONDEUR')
 1010 FORMAT(1P,1X,A4,3X,E12.5,3X,E12.5,3X,E12.5,3X,E12.5)
C
      END
