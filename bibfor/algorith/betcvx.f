        SUBROUTINE BETCVX (IMAT,NMAT,MATER,SIG,VIND,VINF,ELGEOM,
     &                     NVI,TOLER,NSEUIL)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
C       ----------------------------------------------------------------
C       BETON_DOUBLE_DP: CONVEXE ELASTO PLASTIQUE POUR (MATER,SIG,P1,P2)
C            AVEC UN SEUIL EN COMPRESSION ET UN SEUIL EN TRACTION
C            SEUILC = FCOMP      = (SIGEQ   + A  SIGH)/B - FC
C            SEUILT = FTRAC      = (SIGEQ   + C  SIGH)/D - FT
C                AVEC SIGEQ      = SQRT(3/2(D) (D)) (CONTR EQUIVALENTE)
C                     D          = SIG - 1/3 TR(SIG) I
C                     SIGH       = 1/3 TR(SIG)    (CONTR HYDROSTATIQUE)
C       ----------------------------------------------------------------
C       NSEUIL = 1  --> CRITERE  EN COMPRESSION ACTIVE
C       NSEUIL = 2  --> CRITERE  EN TRACTION ACTIVE
C       NSEUIL = 3  --> CRITERES EN COMPRESSION ET EN TRACTION ACTIVE
C       NSEUIL = 11 --> PROJECTION AU SOMMET DU CONE DE COMPRESSION
C       NSEUIL = 22 --> PROJECTION AU SOMMET DU CONE DE TRACTION
C       NSEUIL = 33 --> PROJECTION AU SOMMET DES CONES DE COMPRESSION
C                       ET TRACTION
C       ----------------------------------------------------------------
C       IN  SIG    :  CONTRAINTE
C       IN  VIND   :  VARIABLES INTERNES = ( PC PT THETA ) A T
C       IN  VINF   :  VARIABLES INTERNES = ( PC PT THETA ) A T+DT
C       IN  IMAT   :  ADRESSE DU MATERIAU CODE
C       IN  NMAT   :  DIMENSION MATER
C       IN  MATER  :  COEFFICIENTS MATERIAU A TEMP
C       IN  ELGEOM :  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES AUX
C                     LOIS DE COMPORTEMENT
C       IN  TOLER  :  TOLERANCE DE CONVERGENCE LOCALE
C       VAR NSEUIL :  SEUIL ELASTIQUE PRECEDENT / NOUVEAU SEUIL CALCULE
C       ----------------------------------------------------------------
        INTEGER         IMAT, NVI , NMAT , NSEUIL
        REAL*8          PC, PT ,   SIG(6) , DEV(6) , VIND(*), VINF(*)
        REAL*8          MATER(NMAT,2),TOLER , ELGEOM(*)
        REAL*8          FCP , FTP, FC , FT , BETA
C        REAL*8          ALPHA
        REAL*8          RAC2 , UN , DEUX , TROIS
        REAL*8          KE , FCOMP , FTRAC
        REAL*8          A, B, C, D
        REAL*8          SIGEQ , SIGH, P, DFCDLC, DFTDLT, KUC, KUT
        REAL*8          LASTS, D13, DLAMBC, DLAMBT, EPSI, ZERO
        PARAMETER       ( ZERO =  0.D0   )
C       ---------------------------------------------------------------
        INTEGER         NDT  , NDI
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
C
        DATA   D13      /.33333333333333D0 /
        DATA   UN       / 1.D0 /
        DATA   DEUX     / 2.D0 /
        DATA   TROIS    / 3.D0 /
        DATA   EPSI     / 1.D-6 /
        RAC2  = SQRT (DEUX)
C
C ---   SEUIL PRECEDENT
C
        LASTS = NSEUIL
C
C ---   DEFORMATIONS PLASTIQUES PRECEDENTES
C
        IF(LASTS.EQ.0) THEN
           PC  = VIND(1)
           PT  = VIND(2)
        ELSE
           PC  = VINF(1)
           PT  = VINF(2)
        ENDIF
C
C ---   CARACTERISTIQUES MATERIAU
C
        FCP    = MATER(1,2)
        FTP    = MATER(2,2)
        BETA   = MATER(3,2)
C
C        ALPHA = FTP / FCP
        A = RAC2 * (BETA - UN) / (DEUX * BETA - UN)
        B = RAC2 / TROIS * BETA / (DEUX * BETA - UN)
        C = RAC2
        D = DEUX * RAC2 / TROIS
C
C ---   CONTRAINTE EQUIVALENTE
C
        CALL LCDEVI ( SIG , DEV )
        CALL LCPRSC ( DEV , DEV , P)
        SIGEQ = SQRT (1.5D0 * P)
C
C ---   CONTRAINTE HYDROSTATIQUE
C
        CALL LCHYDR ( SIG , SIGH )
C
C ---   ECROUISSAGE EN TRACTION ET EN COMPRESSION
C
        CALL BETFPP ( MATER, NMAT, ELGEOM, PC, PT, 3, FC, FT,
     &                DFCDLC, DFTDLT, KUC, KUT, KE)
C
C
C -     SEUIL EN COMPRESSION
C
        FCOMP = (RAC2 * D13 * SIGEQ + A * SIGH) / B - FC
C
C -     SEUIL EN TRACTION
C
        FTRAC = (RAC2 * D13 * SIGEQ + C * SIGH) / D - FT
C
C -     VERIFICATION ET CALCUL DU CAS DE PLASTICITE
C -     FCOMP > 0  -->  NSEUIL = 1  (CRITERE COMPRESSION ACTIVE)
C -     FTRAC > 0  -->  NSEUIL = 2  (CRITERE TRACTION ACTIVE)
C -     FTRAC > 0  ET FTRAC > 0
C -                -->  NSEUIL = 3  (DEUX CRITERES ACTIVES)
C
        NSEUIL = -1
        IF(FCOMP.GT.(FCP*EPSI)) NSEUIL = 1
        IF(FTRAC.GT.(FTP*EPSI)) NSEUIL = 2
        IF(FCOMP.GT.(FCP*EPSI).AND.FTRAC.GT.(FTP*EPSI)) NSEUIL = 3
C
        DLAMBC = VINF(1) - VIND(1)
        DLAMBT = VINF(2) - VIND(2)
C
        IF(LASTS.GT.0) THEN
           IF(LASTS.EQ.1.AND.DLAMBC.LT.ZERO) THEN
             IF(FTRAC.LE.ZERO) THEN
               CALL UTMESS('A','BETCVX','BETON_DOUBLE_DP: INCREMENT '
     &            //'DE DEFORMATION PLASTIQUE EN TRACTION NEGATIF'
     &            //' --> REDECOUPAGE AUTO DU PAS DE TEMPS ')
               NSEUIL = 4
               GOTO 9999
             ELSE
               NSEUIL = 2
               GOTO 9999
             ENDIF
           ENDIF
           IF(LASTS.EQ.2.AND.DLAMBT.LT.ZERO) THEN
             IF(FCOMP.LE.ZERO) THEN
               CALL UTMESS('A','BETCVX','BETON_DOUBLE_DP: INCREMENT '
     &            //'DE DEFORMATION PLASTIQUE EN COMPRESSION NEGATIF'
     &            //' --> REDECOUPAGE AUTO DU PAS DE TEMPS ')
               NSEUIL = 4
               GOTO 9999
             ELSE
               NSEUIL = 1
               GOTO 9999
             ENDIF
           ENDIF
           IF(LASTS.EQ.3.AND.DLAMBC.LT.ZERO) THEN
              NSEUIL = 2
              GOTO 9999
           ENDIF
           IF(LASTS.EQ.3.AND.DLAMBT.LT.ZERO) THEN
              NSEUIL = 1
              GOTO 9999
           ENDIF
           IF(LASTS.EQ.22.AND.NSEUIL.GT.0) THEN
C               CALL UTMESS('A','BETCVX','BETON_DOUBLE_DP: SOLUTION '
C     &            //'NON VALIDE AVEC PROJECTION AU SOMMET DU CONE DE '
C     &            //'TRACTION')
               NSEUIL = 33
               GOTO 9999
           ENDIF
           IF(LASTS.EQ.22.AND.DLAMBT.LT.ZERO) THEN
C               CALL UTMESS('A','BETCVX','BETON_DOUBLE_DP: INCREMENT '
C     &            //'DE DEFORMATION PLASTIQUE NEGATIF LORS DE LA '
C     &            //'PROJECTION AU SOMMET DU CONE DE TRACTION')
               NSEUIL = 33
               GOTO 9999
           ENDIF
           IF(LASTS.EQ.11.AND.NSEUIL.GT.0) THEN
C               CALL UTMESS('A','BETCVX','BETON_DOUBLE_DP: SOLUTION '
C     &            //'NON VALIDE AVEC PROJECTION AU SOMMET DU CONE DE '
C     &            //'COMPRESSION')
               NSEUIL = 44
               GOTO 9999
           ENDIF
           IF(LASTS.EQ.11.AND.DLAMBC.LT.ZERO) THEN
C               CALL UTMESS('A','BETCVX','BETON_DOUBLE_DP: INCREMENT '
C     &            //'DE DEFORMATION PLASTIQUE NEGATIF LORS DE LA '
C     &            //'PROJECTION AU SOMMET DU CONE DE COMPRESSION')
               NSEUIL = 44
               GOTO 9999
           ENDIF
           IF(LASTS.EQ.33.AND.NSEUIL.GT.0) THEN
C               CALL UTMESS('A','BETCVX','BETON_DOUBLE_DP: SOLUTION '
C     &            //'NON VALIDE AVEC PROJECTION AU SOMMET DES CONES '
C     &            //'DE COMPRESSION ET DE TRACTION')
               NSEUIL = 11
               GOTO 9999
           ENDIF
           IF(LASTS.EQ.33.AND.DLAMBC.LT.ZERO) THEN
C               CALL UTMESS('A','BETCVX','BETON_DOUBLE_DP: INCREMENT '
C     &            //'DE DEFORMATION PLASTIQUE NEGATIF EN COMPRESSION '
C     &            //'LORS DE LA PROJECTION AU SOMMET DES CONES DE '
C     &            //'COMPRESSION ET DE TRACTION')
               NSEUIL = 11
               GOTO 9999
           ENDIF
           IF(LASTS.EQ.33.AND.DLAMBT.LT.ZERO) THEN
C               CALL UTMESS('A','BETCVX','BETON_DOUBLE_DP: INCREMENT '
C     &            //'DE DEFORMATION PLASTIQUE NEGATIF EN TRACTION '
C     &            //'LORS DE LA PROJECTION AU SOMMET DES CONES DE '
C     &            //'COMPRESSION ET DE TRACTION')
               NSEUIL = 11
               GOTO 9999
           ENDIF
           IF(LASTS.EQ.4.AND.NSEUIL.LT.0) THEN
C               CALL UTMESS('A','BETCVX','BETON_DOUBLE_DP: PROBLEME DE '
C     &            //'CONVERGENCE SANS PROJECTION AU SOMMET DES CONES')
               NSEUIL = 4
               GOTO 9999
           ENDIF
           IF(LASTS.EQ.44.AND.NSEUIL.LT.0) THEN
C               CALL UTMESS('A','BETCVX','BETON_DOUBLE_DP: PROBLEME DE '
C     &            //'CONVERGENCE AVEC PROJECTION AU SOMMET DES CONES')
               NSEUIL = 44
               GOTO 9999
           ENDIF
           IF(NSEUIL.EQ.LASTS) THEN
              IF (NSEUIL.EQ.2) THEN
                 NSEUIL = 1
              ELSEIF (NSEUIL.EQ.3) THEN
                 NSEUIL = 2
              ELSEIF (NSEUIL.EQ.1) THEN
                 NSEUIL = 2
              ELSE
                 NSEUIL = 4
                 GOTO 9999
              ENDIF
           ENDIF
        ELSE
C
C ---   A LA PREMIERE RESOLUTION, ON REPREND LES MEMES CRITERES QU'AU
C ---   PAS DE CALCUL PRECEDENT
C
           IF(NSEUIL.EQ.3) THEN
              IF(VIND(NVI).GT.EPSI) THEN
                 NSEUIL = INT(VIND(NVI) + 0.5D0)
                 IF(NSEUIL.EQ.22) NSEUIL = 2
                 IF(NSEUIL.EQ.11) NSEUIL = 1
                 IF(NSEUIL.EQ.33) NSEUIL = 3
              ENDIF
           ENDIF
        ENDIF
C
 9999   CONTINUE
C
        END
