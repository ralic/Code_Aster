      SUBROUTINE PROJAX( VECPG, NBVEC, NBORDR, PROAXE, IFLAG,
     &                   RMIMA, RAXE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 10/01/2005   AUTEUR F1BHHAJ J.ANGLES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE F1BHHAJ J.ANGLES
      IMPLICIT      NONE
      INTEGER       NBVEC, NBORDR, IFLAG(NBVEC)
      REAL*8        VECPG(2*NBVEC*NBORDR), RMIMA(4*NBVEC)
      REAL*8        RAXE(NBVEC*NBORDR)
      CHARACTER*16  PROAXE
C ----------------------------------------------------------------------
C BUT: PROJETER SUR UN OU DEUX AXES LES POINTS REPRESANTANT LE
C      CISAILLEMENT TAU DANS LE PLAN u, v.
C ----------------------------------------------------------------------
C ARGUMENTS:
C VECPG     IN   R  : VECTEUR DE TRAVAIL CONTENANT LES
C                     COMPOSANTES u ET v DU VECTEUR TAU (CISAILLEMENT),
C                     POUR TOUS LES VECTEURS NORMAUX (n) ET TOUS LES
C                     NUMEROS D'ORDRE.
C NBVEC     IN   I  : NOMBRE DE VECTEURS NORMAUX.
C NBORDR    IN   I  : NOMBRE DE NUMERO D'ORDRE STOCKE DANS LA
C                     STRUCTURE DE DONNEES RESULTAT.
C PROAXE    IN   K16: TYPE DE PROJECTION (UN OU DEUX AXES).
C IFLAG     IN   I  : VECTEUR DE DRAPEAUX INDIQUANT :
C                      - IFLAG(i) = 0 --> CAS GENERAL
C                      - IFLAG(i) = 1 --> CAS OU LES POINTS DANS LE
C                                         PLAN DE CISAILLEMENT SONT
C                                         ALIGNES VERTICALEMENT.
C                      - IFLAG(i) = 2 --> CAS OU LES POINTS DANS LE
C                                         PLAN DE CISAILLEMENT SONT
C                                         ALIGNES HORIZONTALEMENT.
C                      - IFLAG(i) = 3 --> CAS OU LES POINTS DANS LE
C                                         PLAN DE CISAILLEMENT SONT
C                                         CONTENUS DANS UN CADRE DE
C                                         COTES INFERIEURS A EPSILO.
C RMIMA     IN   R  : VECTEUR CONTENANT LES COORDONNEES DES POINTS
C                     EXTREMES DU CADRE (UMIN, UMAX, VMIN, VMAX).
C                     POUR TOUS LES VECTEURS NORMAUX.
C RAXE      OUT  R  : VECTEUR CONTENANT L'AMPLITUDE DES POINTS
C                     PROJETES.
C
C-----------------------------------------------------------------------
C---- COMMUNS NORMALISES  JEVEUX
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNOM,JEXNUM,JEXATR
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ------------------------------------------------------------------
      INTEGER      I, N1, IVECT, IORDR, JSEC1, JSEC2, JSEC3, JSEC4
      INTEGER      INDSEC, NBPTS1, NBPTS2, NBPTS3, NBPTS4, NPTSEC(4)
C
      REAL*8       UMIN, UMAX, VMIN, VMAX, U0, V0, DIAMIN
      REAL*8       CSTAI, CSTBI, CSTAS, CSTBS, UAXE2, VAXE2
      REAL*8       UI, VI, DISTS(4), AI, BI, AS, BS, RPAXI, RPAXS
      REAL*8       UP, VP, VAL
C
      CHARACTER*4  AXEINI
C
C-----------------------------------------------------------------------
C234567                                                              012

      CALL JEMARQ()
C
      CALL WKVECT('&&PROJAX.SECT1', 'V V R', NBORDR*2, JSEC1)
      CALL WKVECT('&&PROJAX.SECT2', 'V V R', NBORDR*2, JSEC2)
      CALL WKVECT('&&PROJAX.SECT3', 'V V R', NBORDR*2, JSEC3)
      CALL WKVECT('&&PROJAX.SECT4', 'V V R', NBORDR*2, JSEC4)

      N1 = 0

      DO 10 IVECT=1, NBVEC
         CALL JERAZO('&&PROJAX.SECT1', NBORDR*2, 1)
         CALL JERAZO('&&PROJAX.SECT2', NBORDR*2, 1)
         CALL JERAZO('&&PROJAX.SECT3', NBORDR*2, 1)
         CALL JERAZO('&&PROJAX.SECT4', NBORDR*2, 1)

         UMIN = RMIMA(4*(IVECT-1) + 1)
         UMAX = RMIMA(4*(IVECT-1) + 2)
         VMIN = RMIMA(4*(IVECT-1) + 3)
         VMAX = RMIMA(4*(IVECT-1) + 4)
         U0 = UMIN + (UMAX - UMIN)/2.0D0
         V0 = VMIN + (VMAX - VMIN)/2.0D0

         IF ((UMAX-UMIN) .GE. (VMAX-VMIN)) THEN
            DIAMIN = UMAX - UMIN
         ELSE
            DIAMIN = VMAX - VMIN
         ENDIF

C 1. DETERMINATION DES POINTS SITUES DANS LES 4 SECTEURS

         NBPTS1 = 0
         NBPTS2 = 0
         NBPTS3 = 0
         NBPTS4 = 0

         DO 20 IORDR=1, NBORDR
            N1 = N1 + 1
            UI = VECPG(2*N1-1)
            VI = VECPG(2*N1)

            DISTS(1) = SQRT((UMIN - UI)**2  + (VMAX - VI)**2)
            DISTS(2) = SQRT((UMAX - UI)**2  + (VMAX - VI)**2)
            DISTS(3) = SQRT((UMAX - UI)**2  + (VMIN - VI)**2)
            DISTS(4) = SQRT((UMIN - UI)**2  + (VMIN - VI)**2)

            INDSEC = 0
            DO 30 I=1, 4
               IF ((DISTS(I) .GT. DIAMIN) .AND. (INDSEC .EQ. 0)) THEN
                  IF (UI .GE. U0) THEN
                     IF (VI .GE. V0) THEN
                        ZR(JSEC2 + NBPTS2*2) = UI
                        ZR(JSEC2 + NBPTS2*2 +1) = VI
                        NBPTS2 = NBPTS2 + 1
                        INDSEC = 1
                     ELSE
                        ZR(JSEC3 + NBPTS3*2) = UI
                        ZR(JSEC3 + NBPTS3*2 +1) = VI
                        NBPTS3 = NBPTS3 + 1
                        INDSEC = 1
                     ENDIF
                  ELSE
                     IF (VI .GE. V0) THEN
                        ZR(JSEC1 + NBPTS1*2) = UI
                        ZR(JSEC1 + NBPTS1*2 +1) = VI
                        NBPTS1 = NBPTS1 + 1
                        INDSEC = 1
                     ELSE
                        ZR(JSEC4 + NBPTS4*2) = UI
                        ZR(JSEC4 + NBPTS4*2 +1) = VI
                        NBPTS4 = NBPTS4 + 1
                        INDSEC = 1
                     ENDIF
                  ENDIF
               ENDIF
 30         CONTINUE
 20      CONTINUE

C 2. CHOIX DE L'AXE INITIAL (SI UN 2EME AXE EST DEMANDE,
C                            IL SERA DEDUIT DE L'AXE INITIAL.)
C
C      A ----------------------------- B
C        | Sect.1             Sect.2 |    PAR DEFINITION L'AXE 1 EST LE
C        |                           |    SEGMENT DB.
C        |                           |
C        |                           |    PAR DEFINITION L'AXE 2 EST LE
C        |                           |    SEGMENT AC.
C        | Sect.4             Sect.3 |
C      D ----------------------------- C


         IF (IFLAG(IVECT) .EQ. 0) THEN
            N1 = N1 - NBORDR

            IF ( ((NBPTS1 .EQ. 0) .AND. (NBPTS3 .EQ. 0) .AND.
     &            (NBPTS2 .GT. 0) .AND. (NBPTS4 .GT. 0)) .OR.
     &           ((NBPTS1 .EQ. 0) .AND. (NBPTS3 .GT. 0) .AND.
     &            (NBPTS2 .GT. 0) .AND. (NBPTS4 .GT. 0)) .OR.
     &           ((NBPTS1 .GT. 0) .AND. (NBPTS3 .EQ. 0) .AND.
     &            (NBPTS2 .GT. 0) .AND. (NBPTS4 .GT. 0)) ) THEN
               AXEINI = 'AXE1'
            ELSEIF ( ((NBPTS1 .GT. 0) .AND. (NBPTS3 .GT. 0) .AND.
     &                (NBPTS2 .EQ. 0) .AND. (NBPTS4 .EQ. 0)) .OR.
     &               ((NBPTS1 .GT. 0) .AND. (NBPTS3 .GT. 0) .AND.
     &                (NBPTS2 .EQ. 0) .AND. (NBPTS4 .GT. 0)) .OR.
     &               ((NBPTS1 .GT. 0) .AND. (NBPTS3 .GT. 0) .AND.
     &                (NBPTS2 .GT. 0) .AND. (NBPTS4 .EQ. 0)) ) THEN
               AXEINI = 'AXE2'
            ELSEIF ( ((NBPTS1 .EQ. 0) .AND. (NBPTS3 .GT. 0) .AND.
     &                (NBPTS2 .EQ. 0) .AND. (NBPTS4 .GT. 0)) .OR.
     &               ((NBPTS1 .EQ. 0) .AND. (NBPTS3 .GT. 0) .AND.
     &                (NBPTS2 .GT. 0) .AND. (NBPTS4 .EQ. 0)) .OR.
     &               ((NBPTS1 .GT. 0) .AND. (NBPTS3 .EQ. 0) .AND.
     &                (NBPTS2 .EQ. 0) .AND. (NBPTS4 .GT. 0)) .OR.
     &               ((NBPTS1 .GT. 0) .AND. (NBPTS3 .EQ. 0) .AND.
     &                (NBPTS2 .GT. 0) .AND. (NBPTS4 .EQ. 0)) .OR.
     &               ((NBPTS1 .GT. 0) .AND. (NBPTS3 .GT. 0) .AND.
     &                (NBPTS2 .GT. 0) .AND. (NBPTS4 .GT. 0)) ) THEN
               NPTSEC(1)=NBPTS1
               NPTSEC(2)=NBPTS2
               NPTSEC(3)=NBPTS3
               NPTSEC(4)=NBPTS4
               CALL RAXINI(ZR(JSEC1), ZR(JSEC2), ZR(JSEC3), ZR(JSEC4),
     &                     NPTSEC, NBORDR, UMIN, UMAX, VMIN, VMAX,
     &                     AXEINI)
            ELSE
               CALL UTMESS('F', 'PROJAX.1', 'PRESENCE DE POINT(S) '//
     &                     'QUE DANS UN SECTEUR.')
            ENDIF

C 3. CALCUL DES CONSTANTES NECESSAIRES A LA PROJECTION SUR UN OU DEUX
C    AXES

            IF ( AXEINI .EQ. 'AXE1' ) THEN
               CSTAI = UMAX-UMIN
               CSTBI = VMAX-VMIN
               AI = (VMAX-VMIN)/(UMAX-UMIN)
               BI = (UMAX*VMIN - UMIN*VMAX)/(UMAX-UMIN)
            ELSEIF ( AXEINI .EQ. 'AXE2' ) THEN
               CSTAI = -(UMAX-UMIN)
               CSTBI = VMAX-VMIN
               AI = -(VMAX-VMIN)/(UMAX-UMIN)
               BI = (UMAX*VMAX - UMIN*VMIN)/(UMAX-UMIN)
            ENDIF

            IF ( PROAXE .EQ. 'DEUX_AXES' ) THEN
               UAXE2 = U0 + 1.0D0
               VAXE2 = V0 - (CSTAI/CSTBI)*(UAXE2-U0)
               CSTAS = UAXE2 - U0
               CSTBS = VAXE2 - V0
               AS = (VAXE2 - V0)/(UAXE2 - U0)
               BS = (UAXE2*V0 - U0*VAXE2)/(UAXE2 - U0)
            ENDIF

C 4. PROJECTION SUR UN AXE OU DEUX AXES

            DO 40 IORDR=1, NBORDR
               N1 = N1 + 1

               UI = VECPG(2*N1 - 1)
               VI = VECPG(2*N1)

C 4.1 PROJECTION SUR L'AXE INITIAL

               CALL PROAX0(UI,VI, CSTAI,CSTBI, AI,BI, U0,V0, RPAXI)

C 4.2 PROJECTION SUR LE SECOND AXE SI CELA EST DEMANDE

               IF ( PROAXE .EQ. 'DEUX_AXES' ) THEN
                  CALL PROAX0(UI,VI, CSTAS,CSTBS, AS,BS, U0,V0, RPAXS)
               ELSE
                  RPAXS = 0.0D0
               ENDIF

C 4.3 CALCUL DU MODULE ET ATTRIBUTION DU SIGNE

               IF (RPAXI .LT. 0.0D0) THEN
                  RAXE(N1) = -SQRT(RPAXI**2 + RPAXS**2)
               ELSE
                  RAXE(N1) = SQRT(RPAXI**2 + RPAXS**2)
               ENDIF
 40         CONTINUE

C LES POINTS SONT ALIGNES VERTICALEMENT
         ELSEIF ( IFLAG(IVECT) .EQ. 1 ) THEN
            N1 = N1 - NBORDR

            DO 50 IORDR=1, NBORDR
               N1 = N1 + 1

               UI = VECPG(2*N1 - 1)
               VI = VECPG(2*N1)
               UP = UI
               VP = VI
               VAL = SQRT((UP-U0)**2 + (VP-V0)**2)
               IF (VP .LT. V0) THEN
                  VAL = -VAL
               ENDIF
               RAXE(N1) = VAL
 50         CONTINUE

C LES POINTS SONT ALIGNES HORIZONTALEMENT
         ELSEIF ( IFLAG(IVECT) .EQ. 2 ) THEN
            N1 = N1 - NBORDR

            DO 60 IORDR=1, NBORDR
               N1 = N1 + 1

               UI = VECPG(2*N1 - 1)
               VI = VECPG(2*N1)
               UP = UI
               VP = VI
               VAL = SQRT((UP-U0)**2 + (VP-V0)**2)
               IF (UP .LT. U0) THEN
                  VAL = -VAL
               ENDIF
               RAXE(N1) = VAL
 60         CONTINUE

C LES POINTS SONT DANS UN CADRE DONT LES COTES SONT INFERIEURS A EPSILO
         ELSEIF ( IFLAG(IVECT) .EQ. 3 ) THEN
            N1 = N1 - NBORDR

            DO 70 IORDR=1, NBORDR
               N1 = N1 + 1

               UI = VECPG(2*N1 - 1)
               VI = VECPG(2*N1)
               UP = UI
               VP = VI
               VAL = SQRT((UP-U0)**2 + (VP-V0)**2)
               IF (UP .LT. U0) THEN
                  VAL = -VAL
               ENDIF
               RAXE(N1) = VAL
 70         CONTINUE

         ENDIF

 10   CONTINUE

      CALL JEDETR('&&PROJAX.SECT1')
      CALL JEDETR('&&PROJAX.SECT2')
      CALL JEDETR('&&PROJAX.SECT3')
      CALL JEDETR('&&PROJAX.SECT4')
C
      CALL JEDEMA()
      END
