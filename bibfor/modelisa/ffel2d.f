      FUNCTION FFEL2D(NBCNX,INO,KSI1,KSI2)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 02/09/98   AUTEUR CIBHHLV L.VIVAN 
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
C  DESCRIPTION : CALCUL DE LA VALEUR PRISE PAR L'UNE DES FONCTIONS DE
C  -----------   FORME D'UN ELEMENT 2D DE TYPE
C                  - TRIANGLE LINEAIRE A 3 NOEUDS
C                  - TRIANGLE QUADRATIQUE A 6 NOEUDS
C                  - QUADRANGLE BI-LINEAIRE A 4 NOEUDS
C                  - QUADRANGLE QUADRATIQUE INCOMPLET A 8 NOEUDS
C                  - QUADRANGLE QUADRATIQUE COMPLET A 9 NOEUDS
C
C                APPELANT : RECI2D
C
C  IN     : NBCNX  : INTEGER , SCALAIRE
C                    NOMBRE DE NOEUDS DE L'ELEMENT : 3 OU 6 POUR UN
C                    TRIANGLE, 4 OU 8 OU 9 POUR UN QUADRANGLE
C  IN     : INO    : INTEGER , SCALAIRE
C                    INDICE DU NOEUD DE L'ELEMENT DE REFERENCE AUQUEL
C                    EST ASSOCIEE LA FONCTION
C  IN     : KSI1   : REAL*8 , SCALAIRE
C                    PREMIERE COORDONNEE DU POINT OU LA FONCTION EST
C                    CALCULEE, DANS LE REPERE ASSOCIE A L'ELEMENT DE
C                    REFERENCE
C  IN     : KSI2   : REAL*8 , SCALAIRE
C                    SECONDE COORDONNEE DU POINT OU LA FONCTION EST
C                    CALCULEE, DANS LE REPERE ASSOCIE A L'ELEMENT DE
C                    REFERENCE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
      REAL*8        FFEL2D
C
C ARGUMENTS
C ---------
      INTEGER       NBCNX, INO
      REAL*8        KSI1, KSI2
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      IF ( (INO.LT.1) .OR. (INO.GT.NBCNX) )
     &   CALL UTMESS('F','FFEL2D','INDICE DE NOEUD INVALIDE')
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   FONCTIONS DE FORME D'UN ELEMENT TRIANGLE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C 1.1 TRIANGLE LINEAIRE A 3 NOEUDS
C ---
      IF ( NBCNX.EQ.3 ) THEN
C
         IF ( INO.EQ.1 ) THEN
            FFEL2D = 0.5D0 * ( 1.0D0 + KSI2 )
         ELSE IF ( INO.EQ.2 ) THEN
            FFEL2D = -0.5D0 * ( KSI1 + KSI2 )
         ELSE
            FFEL2D = 0.5D0 * ( 1.0D0 + KSI1 )
         ENDIF
C
C 1.2 TRIANGLE QUADRATIQUE A 6 NOEUDS
C ---
      ELSE IF ( NBCNX.EQ.6 ) THEN
C
         IF ( INO.EQ.1 ) THEN
            FFEL2D = 0.5D0 * ( 1.0D0 + KSI2 ) * KSI2
         ELSE IF ( INO.EQ.2 ) THEN
            FFEL2D = 0.5D0 * ( KSI1 + KSI2 ) * ( KSI1 + KSI2 + 1.0D0 )
         ELSE IF ( INO.EQ.3 ) THEN
            FFEL2D = 0.5D0 * ( 1.0D0 + KSI1 ) * KSI1
         ELSE IF ( INO.EQ.4 ) THEN
            FFEL2D = -1.0D0 * ( 1.0D0 + KSI2 ) * ( KSI1 + KSI2 )
         ELSE IF ( INO.EQ.5 ) THEN
            FFEL2D = -1.0D0 * ( 1.0D0 + KSI1 ) * ( KSI1 + KSI2 )
         ELSE
            FFEL2D = ( 1.0D0 + KSI1 ) * ( 1.0D0 + KSI2 )
         ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   FONCTIONS DE FORME D'UN ELEMENT QUADRANGLE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C 2.1 QUADRANGLE BI-LINEAIRE A 4 NOEUDS
C ---
      ELSE IF ( NBCNX.EQ.4 ) THEN
C
         IF ( INO.EQ.1 ) THEN
            FFEL2D = 0.25D0 * ( 1.0D0 + KSI2 ) * ( 1.0D0 - KSI1 )
         ELSE IF ( INO.EQ.2 ) THEN
            FFEL2D = 0.25D0 * ( 1.0D0 - KSI2 ) * ( 1.0D0 - KSI1 )
         ELSE IF ( INO.EQ.3 ) THEN
            FFEL2D = 0.25D0 * ( 1.0D0 + KSI1 ) * ( 1.0D0 - KSI2 )
         ELSE
            FFEL2D = 0.25D0 * ( 1.0D0 + KSI1 ) * ( 1.0D0 + KSI2 )
         ENDIF
C
C 2.2 QUADRANGLE QUADRATIQUE INCOMPLET A 8 NOEUDS
C ---
      ELSE IF ( NBCNX.EQ.8 ) THEN
C
         IF ( INO.EQ.1 ) THEN
            FFEL2D = 0.25D0 * ( 1.0D0 + KSI2 ) * ( 1.0D0 - KSI1 )
     &                      * ( KSI2 - KSI1 - 1.0D0 )
         ELSE IF ( INO.EQ.2 ) THEN
            FFEL2D = -0.25D0 * ( 1.0D0 - KSI2 ) * ( 1.0D0 - KSI1 )
     &                       * ( KSI2 + KSI1 + 1.0D0 )
         ELSE IF ( INO.EQ.3 ) THEN
            FFEL2D = -0.25D0 * ( 1.0D0 - KSI2 ) * ( 1.0D0 + KSI1 )
     &                       * ( KSI2 - KSI1 + 1.0D0 )
         ELSE IF ( INO.EQ.4 ) THEN
            FFEL2D = 0.25D0 * ( 1.0D0 + KSI2 ) * ( 1.0D0 + KSI1 )
     &                      * ( KSI2 + KSI1 - 1.0D0 )
         ELSE IF ( INO.EQ.5 ) THEN
            FFEL2D = 0.5D0 * ( 1.0D0 - KSI1 ) * ( 1.0D0 - KSI2 )
     &                     * ( 1.0D0 + KSI2 )
         ELSE IF ( INO.EQ.6 ) THEN
            FFEL2D = 0.5D0 * ( 1.0D0 - KSI2 ) * ( 1.0D0 - KSI1 )
     &                     * ( 1.0D0 + KSI1 )
         ELSE IF ( INO.EQ.7 ) THEN
            FFEL2D = 0.5D0 * ( 1.0D0 + KSI1 ) * ( 1.0D0 - KSI2 )
     &                     * ( 1.0D0 + KSI2 )
         ELSE
            FFEL2D = 0.5D0 * ( 1.0D0 + KSI2 ) * ( 1.0D0 - KSI1 )
     &                     * ( 1.0D0 + KSI1 )
         ENDIF
C
C 2.3 QUADRANGLE QUADRATIQUE COMPLET A 9 NOEUDS
C ---
      ELSE IF ( NBCNX.EQ.9 ) THEN
C
         IF ( INO.EQ.1 ) THEN
            FFEL2D = 0.25D0 * KSI1 * KSI2 * ( KSI2 + 1.0D0 )
     &                                    * ( KSI1 - 1.0D0 )
         ELSE IF ( INO.EQ.2 ) THEN
            FFEL2D = 0.25D0 * KSI1 * KSI2 * ( KSI2 - 1.0D0 )
     &                                    * ( KSI1 - 1.0D0 )
         ELSE IF ( INO.EQ.3 ) THEN
            FFEL2D = 0.25D0 * KSI1 * KSI2 * ( KSI2 - 1.0D0 )
     &                                    * ( KSI1 + 1.0D0 )
         ELSE IF ( INO.EQ.4 ) THEN
            FFEL2D = 0.25D0 * KSI1 * KSI2 * ( KSI2 + 1.0D0 )
     &                                    * ( KSI1 + 1.0D0 )
         ELSE IF ( INO.EQ.5 ) THEN
            FFEL2D = -0.5D0 * KSI1 * ( KSI2 - 1.0D0 ) * ( KSI1 - 1.0D0 )
     &                             * ( KSI2 + 1.0D0 )
         ELSE IF ( INO.EQ.6 ) THEN
            FFEL2D = -0.5D0 * KSI2 * ( KSI2 - 1.0D0 ) * ( KSI1 + 1.0D0 )
     &                             * ( KSI1 - 1.0D0 )
         ELSE IF ( INO.EQ.7 ) THEN
            FFEL2D = -0.5D0 * KSI1 * ( KSI2 - 1.0D0 ) * ( KSI1 + 1.0D0 )
     &                             * ( KSI2 + 1.0D0 )
         ELSE IF ( INO.EQ.8 ) THEN
            FFEL2D = -0.5D0 * KSI2 * ( KSI2 + 1.0D0 ) * ( KSI1 + 1.0D0 )
     &                             * ( KSI1 - 1.0D0 )
         ELSE
            FFEL2D = ( KSI2 + 1.0D0 ) * ( KSI1 + 1.0D0 )
     &             * ( KSI1 - 1.0D0 ) * ( KSI2 - 1.0D0 )
         ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 3   SORTIE EN ERREUR SI NOMBRE DE NOEUDS INVALIDE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      ELSE
C
         CALL UTMESS('F','FFEL2D','NOMBRE DE NOEUDS INVALIDE')
C
      ENDIF
C
C --- FIN DE FFEL2D.
      END
