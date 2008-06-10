      SUBROUTINE ANTE3D(NBSOM,ITETRA,XBAR,KSI1,KSI2,KSI3)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 27/06/2001   AUTEUR DURAND C.DURAND 
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
C  DESCRIPTION : DETERMINATION DE L'ANTECEDENT DANS L'ELEMENT DE
C  -----------   REFERENCE D'UN POINT APPARTENANT A UN ELEMENT REEL
C                LES ELEMENTS CONSIDERES SONT DES ELEMENTS 3D TETRAEDRES
C                OU PYRAMIDES OU PENTAEDRES OU HEXAEDRES
C
C                APPELANT : RECI3D
C
C  IN     : NBSOM  : INTEGER , SCALAIRE
C                    NOMBRE DE SOMMETS DE L'ELEMENT REEL
C  IN     : ITETRA : INTEGER , SCALAIRE
C                    INDICATEUR DU SOUS-DOMAINE TETRAEDRE AUQUEL
C                    APPARTIENT LE POINT DE L'ELEMENT REEL
C                    SI ELEMENT REEL TETRAEDRE : ITETRA = 1
C                    SI ELEMENT REEL PYRAMIDE  : ITETRA = 1 OU 2
C                    SI ELEMENT REEL PENTAEDRE : ITETRA = 1 OU 2 OU 3
C                    SI ELEMENT REEL HEXAEDRE  : ITETRA = 1 OU 2 OU 3 OU
C                                                         4 OU 5 OU 6
C  IN     : XBAR   : REAL*8 , VECTEUR DE DIMENSION 4
C                    COORDONNEES BARYCENTRIQUES DU POINT DE L'ELEMENT
C                    REEL (BARYCENTRE DES SOMMETS DU SOUS-DOMAINE
C                    TETRAEDRE AUQUEL IL APPARTIENT)
C  OUT    : KSI1   : REAL*8 , SCALAIRE
C                    PREMIERE COORDONNEE DU POINT ANTECEDENT DANS LE
C                    REPERE ASSOCIE A L'ELEMENT DE REFERENCE
C  OUT    : KSI2   : REAL*8 , SCALAIRE
C                    DEUXIEME COORDONNEE DU POINT ANTECEDENT DANS LE
C                    REPERE ASSOCIE A L'ELEMENT DE REFERENCE
C  OUT    : KSI3   : REAL*8 , SCALAIRE
C                    TROISIEME COORDONNEE DU POINT ANTECEDENT DANS LE
C                    REPERE ASSOCIE A L'ELEMENT DE REFERENCE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER       NBSOM, ITETRA
      REAL*8        XBAR(*), KSI1, KSI2, KSI3
C
C VARIABLES LOCALES
C -----------------
      INTEGER       I, ISOM(4), J
C
      REAL*8        XTET(4), YTET(4), ZTET(4),
     &              XPYR(5), YPYR(5), ZPYR(5),
     &              XPEN(6), YPEN(6), ZPEN(6),
     &              XHEX(8), YHEX(8), ZHEX(8)
C
      DATA          XTET /  0.0D0 ,  0.0D0 ,  0.0D0 ,  1.0D0 /
      DATA          YTET /  1.0D0 ,  0.0D0 ,  0.0D0 ,  0.0D0 /
      DATA          ZTET /  0.0D0 ,  1.0D0 ,  0.0D0 ,  0.0D0 /
C
      DATA          XPYR /  1.0D0 ,  0.0D0 , -1.0D0 ,  0.0D0 ,  0.0D0 /
      DATA          YPYR /  0.0D0 ,  1.0D0 ,  0.0D0 , -1.0D0 ,  0.0D0 /
      DATA          ZPYR /  0.0D0 ,  0.0D0 ,  0.0D0 ,  0.0D0 ,  1.0D0 /
C
      DATA          XPEN / -1.0D0 , -1.0D0 , -1.0D0 ,  1.0D0 ,  1.0D0 ,
     &                      1.0D0 /
      DATA          YPEN /  1.0D0 ,  0.0D0 ,  0.0D0 ,  1.0D0 ,  0.0D0 ,
     &                      0.0D0 /
      DATA          ZPEN /  0.0D0 ,  1.0D0 ,  0.0D0 ,  0.0D0 ,  1.0D0 ,
     &                      0.0D0 /
C
      DATA          XHEX / -1.0D0 ,  1.0D0 ,  1.0D0 , -1.0D0 , -1.0D0 ,
     &                      1.0D0 ,  1.0D0 , -1.0D0 /
      DATA          YHEX / -1.0D0 , -1.0D0 ,  1.0D0 ,  1.0D0 , -1.0D0 ,
     &                     -1.0D0 ,  1.0D0 ,  1.0D0 /
      DATA          ZHEX / -1.0D0 , -1.0D0 , -1.0D0 , -1.0D0 ,  1.0D0 ,
     &                      1.0D0 ,  1.0D0 ,  1.0D0 /
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      KSI1 = 0.0D0
      KSI2 = 0.0D0
      KSI3 = 0.0D0
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   CAS D'UN ELEMENT REEL TETRAEDRE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      IF ( NBSOM.EQ.4 ) THEN
C
C 1.1    CALCUL DES COORDONNEES DE L'ANTECEDENT
C ---    DANS L'ELEMENT DE REFERENCE
C
         DO 10 I = 1, 4
            KSI1 = KSI1 + XBAR(I) * XTET(I)
            KSI2 = KSI2 + XBAR(I) * YTET(I)
            KSI3 = KSI3 + XBAR(I) * ZTET(I)
  10     CONTINUE
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   CAS D'UN ELEMENT REEL PYRAMIDE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      ELSE IF ( NBSOM.EQ.5 ) THEN
C
C 2.1    AFFECTATION DES NUMEROS DES SOMMETS
C ---
         IF ( ITETRA.EQ.1 ) THEN
            ISOM(1) = 1
            ISOM(2) = 2
            ISOM(3) = 3
            ISOM(4) = 5
         ELSE IF ( ITETRA.EQ.2 ) THEN
            ISOM(1) = 3
            ISOM(2) = 4
            ISOM(3) = 1
            ISOM(4) = 5
         ELSE
            ISOM(1) = 1
            ISOM(2) = 2
            ISOM(3) = 3
            ISOM(4) = 4
         ENDIF
C
C 2.2    CALCUL DES COORDONNEES DE L'ANTECEDENT
C ---    DANS L'ELEMENT DE REFERENCE
C
         DO 20 I = 1, 4
            J = ISOM(I)
            KSI1 = KSI1 + XBAR(I) * XPYR(J)
            KSI2 = KSI2 + XBAR(I) * YPYR(J)
            KSI3 = KSI3 + XBAR(I) * ZPYR(J)
  20     CONTINUE
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 3   CAS D'UN ELEMENT REEL PENTAEDRE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      ELSE IF ( NBSOM.EQ.6 ) THEN
C
C 3.1    AFFECTATION DES NUMEROS DES SOMMETS
C ---
         IF ( ITETRA.EQ.1 ) THEN
            ISOM(1) = 2
            ISOM(2) = 3
            ISOM(3) = 4
            ISOM(4) = 5
         ELSE IF ( ITETRA.EQ.2 ) THEN
            ISOM(1) = 3
            ISOM(2) = 4
            ISOM(3) = 5
            ISOM(4) = 6
         ELSE IF ( ITETRA.EQ.3 ) THEN
            ISOM(1) = 1
            ISOM(2) = 2
            ISOM(3) = 3
            ISOM(4) = 4
         ELSE IF ( ITETRA.EQ.4 ) THEN
            ISOM(1) = 4
            ISOM(2) = 1
            ISOM(3) = 2
            ISOM(4) = 5
         ELSE IF ( ITETRA.EQ.5 ) THEN
            ISOM(1) = 5
            ISOM(2) = 2
            ISOM(3) = 3
            ISOM(4) = 6
         ELSE IF ( ITETRA.EQ.6 ) THEN
            ISOM(1) = 3
            ISOM(2) = 1
            ISOM(3) = 4
            ISOM(4) = 6
         ENDIF
C
C 3.2    CALCUL DES COORDONNEES DE L'ANTECEDENT
C ---    DANS L'ELEMENT DE REFERENCE
C
         DO 30 I = 1, 4
            J = ISOM(I)
            KSI1 = KSI1 + XBAR(I) * XPEN(J)
            KSI2 = KSI2 + XBAR(I) * YPEN(J)
            KSI3 = KSI3 + XBAR(I) * ZPEN(J)
  30     CONTINUE
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 4   CAS D'UN ELEMENT REEL HEXAEDRE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      ELSE
C
C 4.1    AFFECTATION DES NUMEROS DES SOMMETS
C ---
         IF ( ITETRA.EQ.1 ) THEN
            ISOM(1) = 6
            ISOM(2) = 3
            ISOM(3) = 8
            ISOM(4) = 1
         ELSE IF ( ITETRA.EQ.2 ) THEN
            ISOM(1) = 1
            ISOM(2) = 3
            ISOM(3) = 8
            ISOM(4) = 4
         ELSE IF ( ITETRA.EQ.3 ) THEN
            ISOM(1) = 6
            ISOM(2) = 8
            ISOM(3) = 1
            ISOM(4) = 5
         ELSE IF ( ITETRA.EQ.4 ) THEN
            ISOM(1) = 1
            ISOM(2) = 3
            ISOM(3) = 6
            ISOM(4) = 2
         ELSE IF ( ITETRA.EQ.5 ) THEN
            ISOM(1) = 6
            ISOM(2) = 8
            ISOM(3) = 3
            ISOM(4) = 7
         ELSE IF ( ITETRA.EQ.6 ) THEN
            ISOM(1) = 1
            ISOM(2) = 2
            ISOM(3) = 3
            ISOM(4) = 4
         ELSE IF ( ITETRA.EQ.7 ) THEN
            ISOM(1) = 3
            ISOM(2) = 4
            ISOM(3) = 8
            ISOM(4) = 7
         ELSE IF ( ITETRA.EQ.8 ) THEN
            ISOM(1) = 6
            ISOM(2) = 7
            ISOM(3) = 8
            ISOM(4) = 5
         ELSE IF ( ITETRA.EQ.9 ) THEN
            ISOM(1) = 6
            ISOM(2) = 5
            ISOM(3) = 1
            ISOM(4) = 2
         ELSE IF ( ITETRA.EQ.10 ) THEN
            ISOM(1) = 6
            ISOM(2) = 2
            ISOM(3) = 3
            ISOM(4) = 7
         ELSE IF ( ITETRA.EQ.11 ) THEN
            ISOM(1) = 1
            ISOM(2) = 5
            ISOM(3) = 8
            ISOM(4) = 4
         ENDIF
C
C 4.2    CALCUL DES COORDONNEES DE L'ANTECEDENT
C ---    DANS L'ELEMENT DE REFERENCE
C
         DO 40 I = 1, 4
            J = ISOM(I)
            KSI1 = KSI1 + XBAR(I) * XHEX(J)
            KSI2 = KSI2 + XBAR(I) * YHEX(J)
            KSI3 = KSI3 + XBAR(I) * ZHEX(J)
  40     CONTINUE
C
      ENDIF
C
C --- FIN DE ANTE3D.
      END
