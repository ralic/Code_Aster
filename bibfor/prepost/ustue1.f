      SUBROUTINE USTUE1 ( DIMTUB, VOLTUB, TUBUSE, RCRAY, RCARTE, SECT,
     &                    ARETE, NS, EPAIS, ECRAY, NOMCMD, NOMT19 )
      IMPLICIT   NONE
      INTEGER             DIMTUB, NS
      REAL*8              VOLTUB(*), TUBUSE(*), RCRAY, RCARTE, SECT(*),
     &                    ARETE, EPAIS, ECRAY
      CHARACTER*16        NOMCMD
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
C     CALCULE LES VOLUMES USES DE TUBE (GUIDAGE ENCO_1)
C ----------------------------------------------------------------------
      INTEGER  USURE(10,5), NBUSUR, I, J, L, N, IPAS(10,2), IFM, NIV
      REAL*8   TUBE(20,3),  PAS(10,2), PARUSU(10,4), PASNU, THETA
      REAL*8   ANSINI, ANSFIN, ANGARE, ANGDEB, ANGMAX, ANGFIN
      REAL*8   VOLUME, PROFON, A1, A2, B1, B2, RHO
      REAL*8   EPSI, XPOUR, ANGVA, TABR(4)
      COMPLEX*16   C16B
      CHARACTER*4  T2
      CHARACTER*8  TABK(2)
      CHARACTER*16 NOPARA(7)
C
      DATA NOPARA / 'LIEU'    , 'TRACE'   , 'TYPE'    , 'ANGL_DEBUT', 
     +              'ANGL_FIN', 'ANGL_MAX', 'PROF_MAX' /
C-----------------------------------------------------------------------
C
      CALL INFNIV ( IFM, NIV )
C
C     PAS DE DESCRIPTION POUR LES ZONES NON-USEES :
C     -------------------------------------------
      PASNU = 5.D-1
C
      PROFON = 0.D0
C
      DO 110 I = 1 , NS
         DO 112 J = 1 , 4
            PARUSU(I,J) = 0.D0
 112     CONTINUE
         DO 114 J = 1 , 2
            IPAS(I,J) = 0
             PAS(I,J) = 0.D0
 114     CONTINUE
         DO 116 J = 1 , 5
            USURE(I,J) = 0.D0
 116     CONTINUE
         TUBE(I,1) = SECT(I)
         TUBE(I,2) = SECT(I+1)
         TUBE(I,3) = VOLTUB(I)
 110  CONTINUE
C
      J = 0
      N = 1
 10   CONTINUE
         I = 0
 15      CONTINUE
            IF ( TUBE(N,3) .NE. 0.D0 ) THEN
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
C     -------------------------------       2 : L+V
C                                           3 : V
C
      DO 120 I = 1,NBUSUR
         IF ((USURE(I,3).EQ.1).OR.(USURE(I,4).EQ.NS)) THEN
            IF (USURE(I,2).EQ.1) THEN
               USURE(I,5)=3
            ELSE
               USURE(I,5)=2
            ENDIF
         ELSE
            USURE(I,5)=1
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
               VOLUME = VOLUME + TUBE(J,3)
 132        CONTINUE
            ANSINI = TUBE(USURE(I,3),1)
            ANSFIN = TUBE(USURE(I,4),2)
            CALL LUNULE ( RCRAY, RCARTE, ANGDEB, ANGFIN, ANGMAX, ANSINI,
     &                    ANSFIN, PROFON, VOLUME, EPAIS )
C
C        CAS LUNULE + V :
C        --------------
         ELSEIF ( USURE(I,5) .EQ. 2 ) THEN
            DO 134 J = USURE(I,3), USURE(I,4)
               VOLUME = VOLUME + TUBE(J,3)
 134        CONTINUE
            IF (USURE(I,3).EQ.1) THEN
               ANGARE = ARETE
            ELSE
               ANGARE = 360.D0 - ARETE
            ENDIF
            ANGVA = 11.D0
            CALL VETUBE ( RCRAY, RCARTE, ANGDEB, ANGFIN, ANGARE, ANGMAX,
     &                    ANGVA, PROFON, VOLUME, EPAIS )
C
C        CAS V :
C        -----
         ELSEIF ( USURE(I,5) .EQ. 3 ) THEN
            VOLUME = TUBE(USURE(I,3),3)
            IF ( USURE(I,3) .EQ. 1 ) THEN
               ANGARE = ARETE
            ELSE
               ANGARE = 360.D0 - ARETE
            ENDIF
            ANGVA = 28.D0
            CALL VETUBE ( RCRAY, RCARTE, ANGDEB, ANGFIN, ANGARE, ANGMAX,
     &                    ANGVA, PROFON, VOLUME, EPAIS )
         ENDIF
         PARUSU(I,1) = ANGDEB
         PARUSU(I,2) = ANGMAX
         PARUSU(I,3) = ANGFIN
         PARUSU(I,4) = PROFON
C
C        VERIFICATION DU NON PERCEMENT :
C        -----------------------------
C
         IF ( PROFON .GE. ECRAY ) THEN
            CALL UTDEBM('A',NOMCMD,'******* PERCEMENT TUBE *******')
            CALL UTIMPR('L','LA PROFONDEUR ', 1, PROFON )
            CALL UTIMPR('S',' EST SUPERIEURE A L''EPAISSEUR ', 1, ECRAY)
            CALL UTFINM
         ENDIF 
 130  CONTINUE
C
C     PAS DE DESCRIPTION DANS LES ZONES USEES :
C     --------------------------------------- 
C
      CALL USPAS1 ( NBUSUR, PARUSU, IPAS, PAS )
C
      TABK(1) = 'TUBE'
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
      IF ( NBUSUR .EQ. 0 ) THEN
         L = L + 1
         TUBUSE(2*L-1) = THETA
         TUBUSE(2*L  ) = RCRAY
      ENDIF
      IF (NBUSUR.NE.0) THEN
30       CONTINUE
            L = L + 1
            TUBUSE(2*L-1) = THETA
            TUBUSE(2*L  ) = RCRAY
            THETA = THETA + PASNU
            IF ( THETA .GE. PARUSU(1,1) ) GOTO 50
         GOTO 30
      ENDIF     
50    CONTINUE
C
      EPSI = 1.D-10
      IF ( ABS(PROFON) .GT. EPSI )  XPOUR = 0.9D0*PROFON
C
      DO 150 I = 1 , NBUSUR
C
         A1 = -PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,1) )
         B1 = RCRAY - A1*PARUSU(I,1)
         DO 152 J = 1 , IPAS(I,1)
            RHO = A1*THETA + B1
            IF ( ABS(PROFON) .GT. EPSI ) THEN
               IF ( (RCRAY-RHO) .GE. XPOUR )  RHO = TUBUSE(2*L)
            ENDIF
            L = L + 1
            TUBUSE(2*L-1) = THETA
            TUBUSE(2*L  ) = RHO
            THETA = THETA + PAS(I,1)
 152     CONTINUE  
C         
         A2 = -PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,3) )
         B2 = RCRAY - A2*PARUSU(I,3)
         DO 154 J = 1 , IPAS(I,2)
            RHO = A2*THETA + B2
            IF ( ABS(PROFON) .GT. EPSI ) THEN
               IF ( (RCRAY-RHO) .GE. XPOUR ) RHO = TUBUSE(2*L)
            ENDIF
            L = L + 1
            TUBUSE(2*L-1) = THETA
            TUBUSE(2*L  ) = RHO
            THETA = THETA + PAS(I,2)
 154     CONTINUE
C
         IF ( I .NE. NBUSUR ) THEN
60          CONTINUE
               THETA = THETA + PASNU
               IF ( THETA .GE. PARUSU(I+1,1) )  GOTO 70
               L = L + 1
               TUBUSE(2*L-1) = THETA
               TUBUSE(2*L  ) = RCRAY
               GOTO 60
70          CONTINUE
         ENDIF
 150  CONTINUE
C
80    CONTINUE
         THETA = THETA + PASNU
         IF ( THETA .GE. 360.D0 )  GOTO 90
         L = L + 1
         TUBUSE(2*L-1) = THETA
         TUBUSE(2*L  ) = RCRAY
      GOTO 80
90    CONTINUE
C
      L = L + 1
      TUBUSE(2*L-1) = 360.D0
      TUBUSE(2*L  ) = RCRAY
      DIMTUB = L
C
 1000 FORMAT('==> IMPRESSION DE PARAMETRES "TUBE" PAR SECTEUR USE:',/,
     +       ' TYPE     ANGL_DEBUT      ANGL_FIN',
     +       '      ANGLE_MAX      PROFONDEUR')
 1010 FORMAT(1P,1X,A4,3X,E12.5,3X,E12.5,3X,E12.5,3X,E12.5)
C
      END
