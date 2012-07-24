      SUBROUTINE NMCB1D(E0,LABORD,SIGM,VARM,EPSM,
     &                  DEPS,ESOUT,SIGP,VARP,CRIT,
     &                  OPTION)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 23/07/2012   AUTEUR FLEJOU J-L.FLEJOU 
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
C ----------------------------------------------------------------------
C
C              LOI DE LABORDERIE EN 1D
C
C ----------------------------------------------------------------------
      IMPLICIT       NONE
      CHARACTER*(*)  OPTION
      REAL*8         E0,SIGM,EPSM,DEPS,ESOUT,SIGP
      REAL*8         LABORD(*),VARM(*),VARP(*),CRIT(*)
C
C ----------------------------------------------------------------------
C  IN :
C     E0       : MODULE D'YOUNG INITIAL
C     LABORD   : LES 9 COEFFICIENTS DE LA LOI, DANS CET ORDRE
C                    Y01,Y02,A1,A2,B1,B2,BETA1,BETA2,SIGF1
C     SIGM     : CONTRAINTE A L'INSTANT MOINS
C     VARM     : VARIABLES INTERNES A L'INSTANT MOINS
C     EPSM     : DEFORMATION TOTALE A L'INSTANT MOINS
C     DEPS     : INCREMENT DE DEFORMATION TOTALE
C     CRIT     : CRITERES DE CONVERGENGE
C     OPTION   : FULL_MECA,      MISE A JOUR DE MAT VI SIG
C                RAPH_MECA       MISE A JOUR DE     VI SIG
C                RIGI_MECA_TANG, MISE A JOUR DE MAT
C
C  OUT :
C     ESOUT    : MODULE SECANT OU TANGENT
C     SIGP     : CONTRAINTE A L'INSTANT PLUS
C     VARP     : VARIABLES INTERNES A L'INSTANT PLUS
C
C ----------------------------------------------------------------------
      LOGICAL     RIGI,RESI
      REAL*8      Y01,Y02,A1,A2,B1,B2,BETA1,BETA2,SIGF1,ESEC
      REAL*8      D2,D1,YLIM1,YLIM2,SIG,D1M,D2M,Y1,Y2,Z1,Z2
      REAL*8      EPS,X1,X2,EPSF,UN,R8PREM
      INTEGER     CAS,CCAS
      PARAMETER  (UN=1.0D+0)
C ----------------------------------------------------------------------
C
C     RIGI_MECA_TANG ->       DSIDEP       -->  RIGI
C     FULL_MECA      ->  SIG  DSIDEP  VIP  -->  RIGI  RESI
C     RAPH_MECA      ->  SIG          VIP  -->        RESI
      RIGI   = (OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL')
      RESI   = (OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL')
      RIGI=.TRUE.
C
C --- DEFORMATION MECANIQUE TOTALE
      EPS = EPSM + DEPS
C
C --- VARIABLES INTERNES
      D1M   = VARM(1)
      D2M   = VARM(2)
      YLIM1 = VARM(3)
      YLIM2 = VARM(4)
C
C --- CARACTERISTIQUES MATERIAUX
      Y01   =  ABS( LABORD(1) )
      Y02   =  ABS( LABORD(2) )
      A1    =  ABS( LABORD(3) )
      A2    =  ABS( LABORD(4) )
      B1    =  ABS( LABORD(5) )
      B2    =  ABS( LABORD(6) )
      BETA1 =  ABS( LABORD(7) )
      BETA2 = -ABS( LABORD(8) )
      SIGF1 =  ABS( LABORD(9) )
C
C --- AU DEBUT DU PAS DE TEMPS, ON SOUSTRAIT 1 POUR QUE L'ENDOMMAGEMENT
C     N'EVOLUE PLUS A CAUSE DU TEST DE DEPASSEMENT DE SEUIL
      Y1 = YLIM1 - UN
      Y2 = YLIM2 - UN

C     ON PREND LE MAX POUR EVITER QUE Z1 ET Z2 SOIT "NAN"
      Z1=MAX(YLIM1 - UN , Y01)
      Z2=MAX(YLIM2 - UN , Y02)

C --- ON DUPLIQUE L'ENDOMMAGEMENT :
      D1=D1M
      D2=D2M

C --- BORNES DES DEFORMATIONS INITIALES
      X1 = (BETA1*D1/(UN-D1)+BETA2*D2/(UN-D2))/E0
      X2 = (BETA2*D2-SIGF1)/(UN-D2)/E0
C --- BOUCLE TANT QUE L'ON CHANGE DE CAS
10    CONTINUE
C        TRAITEMENT EN FONCTION DE LA DEFORMATION
         IF      ( X1.LE.EPS ) THEN
C           TRACTION
            CAS  = 1
            EPSF = EPS - BETA2*D2/(UN-D2)/E0
            CALL NMCB13(EPSF,SIG,ESEC,E0,D1M,D1,BETA1,A1,B1,Y1,Y01,Z1,
     &                  CRIT)
         ELSE IF ( EPS.LE.X2 ) THEN
C           COMPRESSION AU DELA DE LA FERMETURE DES FISSURES
            CAS = 3
            CALL NMCB13(EPS ,SIG,ESEC,E0,D2M,D2,BETA2,A2,B2,Y2,Y02,Z2,
     &                  CRIT)
         ELSE
C           FAIBLE COMPRESSION
            CAS = 2
            CALL NMCB2(EPS,BETA1,E0,D1M,SIGF1,BETA2,D2M,SIG)
         END IF
C        BORNES DES DEFORMATIONS FINALES
         X1 = (BETA1*D1/(UN-D1)+BETA2*D2/(UN-D2))/E0
         X2 = (BETA2*D2-SIGF1)/(UN-D2)/E0
         IF      ( X1.LE.EPS ) THEN
            CCAS = 1
         ELSE IF ( EPS.LE.X2 ) THEN
            CCAS = 3
         ELSE
            CCAS = 2
         END IF
      IF ( CCAS.NE.CAS ) GOTO 10
C --- FIN BOUCLE

      IF (  RIGI ) THEN
C ------ MODULE TANGENT NUMERIQUE + 10% D'INITIAL
         IF ( ABS(DEPS).GT.R8PREM() ) THEN
            ESOUT = MIN( (SIG-SIGM)/DEPS+E0*0.10D+0 , E0 )
         ELSE
            IF ( ABS(VARM(5)).GT.R8PREM() ) THEN
               ESOUT = VARM(5)
            ELSE
               ESOUT = E0
            ENDIF
         ENDIF
      ENDIF
C
      IF ( RESI ) THEN
C ------ CONTRAINTE
         SIGP = SIG
C ------ VARIABLES INTERNES REACTUALISEES
         IF ( Y1 .GT. VARM(3) ) THEN
            VARP(1) = D1
            VARP(3) = Y1
         ELSE
            VARP(1) = VARM(1)
            VARP(3) = VARM(3)
         ENDIF
         IF ( Y2 .GT. VARM(4) ) THEN
            VARP(2) = D2
            VARP(4) = Y2
         ELSE
            VARP(2) = VARM(2)
            VARP(4) = VARM(4)
         ENDIF
         VARP(5) = ESOUT
      ENDIF
      END
