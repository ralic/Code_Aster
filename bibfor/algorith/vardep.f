      SUBROUTINE VARDEP(NBNL,DEP,DEP0,TCONF2,TCONF1,IVAR,DT0,
     &                  TOLN,TOLC,TOLV)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/05/2000   AUTEUR KXBADNG T.KESTENS 
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
C DESCRIPTION : TEST SUR LA VARIATION DU DEPLACEMENT PHYSIQUE
C -----------   ENTRE LES INSTANTS N ET N+1
C
C               APPELANT : MDITM2
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER    NBNL
      REAL*8     DEP(3,*), DEP0(3,*), TCONF2(4,*), TCONF1(4,*)
      INTEGER    IVAR
      REAL*8     DT0, TOLN, TOLC, TOLV
C
C VARIABLES LOCALES
C -----------------
      INTEGER    IC
      REAL*8     DNORM, DNORM0, TEMP, TOLCH, TOLE, ZERO
C
C FONCTIONS INTRINSEQUES
C ----------------------
C     INTRINSIC  ABS
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      IVAR  = 0
      TOLCH = 10.0D0 * TOLN
      ZERO  = 0.0D0
C
      IF ( DT0.EQ.ZERO ) GO TO 999
C
C---- BOUCLE SUR LES NON-LINEARITES
C
      DO 10 IC = 1, NBNL
C
C------- CHOIX DE LA TOLERANCE EN FONCTION DE LA POSITION DU NOEUD DE
C------- CHOC PAR RAPPORT A LA BUTEE
C
         DNORM0 = TCONF1(4,IC)
         IF ( ABS(DNORM0).LT.TOLCH ) DNORM0 = ZERO
         DNORM  = TCONF2(4,IC)
         IF ( ABS(DNORM ).LT.TOLCH ) DNORM  = ZERO
C
C....... CHOC A L'INSTANT N+1
         IF ( DNORM.LT.ZERO ) THEN
C.......... CHOC OU CONTACT EXACT A L'INSTANT N
            IF ( DNORM0.LE.ZERO ) THEN
               TOLE = TOLC
C.......... VOL A L'INSTANT N
            ELSE
               TOLE = TOLV
            ENDIF
C
C....... CONTACT EXACT A L'INSTANT N+1
         ELSE IF ( DNORM.EQ.ZERO ) THEN
C.......... CHOC A L'INSTANT N
            IF ( DNORM0.LT.ZERO ) THEN
               TOLE = TOLC
C.......... CONTACT EXACT A L'INSTANT N
            ELSE IF ( DNORM0.EQ.ZERO ) THEN
               TOLE = TOLC
C.......... VOL A L'INSTANT N
            ELSE
               TOLE = TOLV
            ENDIF
C
C....... VOL A L'INSTANT N+1
         ELSE
C.......... CHOC A L'INSTANT N
            IF ( DNORM0.LT.ZERO ) THEN
               TOLE = TOLC
C.......... CONTACT EXACT OU VOL A L'INSTANT N
            ELSE
               TOLE = TOLV
            ENDIF
         ENDIF
C
C------- TEST SUR LA PREMIERE COMPOSANTE DU DEPLACEMENT (REPERE LOCAL)
C------- POUR LA BUTEE IC
C
         IF ( DEP(1,IC).NE.ZERO ) THEN
            TEMP = ABS(DEP(1,IC) - DEP0(1,IC))
            IF ( TEMP.GT.TOLE ) THEN
               IVAR = 1
               GO TO 999
            ENDIF
         ENDIF
C
C------- TEST SUR LA DEUXIEME COMPOSANTE DU DEPLACEMENT (REPERE LOCAL)
C------- POUR LA BUTEE IC
C
         IF ( DEP(2,IC).NE.ZERO ) THEN
            TEMP = ABS(DEP(2,IC) - DEP0(2,IC))
            IF ( TEMP.GT.TOLE ) THEN
               IVAR = 1
               GO TO 999
            ENDIF
         ENDIF
C
C------- TEST SUR LA TROISIEME COMPOSANTE DU DEPLACEMENT (REPERE LOCAL)
C------- POUR LA BUTEE IC
C
         IF ( DEP(3,IC).NE.ZERO ) THEN
            TEMP = ABS(DEP(3,IC) - DEP0(3,IC))
            IF ( TEMP.GT.TOLE ) THEN
               IVAR = 1
               GO TO 999
            ENDIF
         ENDIF
C
  10  CONTINUE
C
 999  CONTINUE
C
C --- FIN DE VARDEP.
      END
