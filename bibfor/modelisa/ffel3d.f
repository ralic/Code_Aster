      FUNCTION FFEL3D(NBCNX,INO,X,Y,Z)
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
C  -----------   FORME D'UN ELEMENT 3D DE TYPE
C                  - TETRAEDRE A 4 NOEUDS OU 10 NOEUDS
C                  - PYRAMIDE  A 5 NOEUDS OU 13 NOEUDS
C                  - PENTAEDRE A 6 NOEUDS OU 15 NOEUDS
C                  - HEXAEDRE  A 8 NOEUDS OU 20 NOEUDS OU 27 NOEUDS
C
C                APPELANT : RECI3D
C
C  IN     : NBCNX  : INTEGER , SCALAIRE
C                    NOMBRE DE NOEUDS DE L'ELEMENT
C  IN     : INO    : INTEGER , SCALAIRE
C                    INDICE DU NOEUD DE L'ELEMENT DE REFERENCE AUQUEL
C                    EST ASSOCIEE LA FONCTION
C  IN     : X,Y,Z  : REAL*8 , SCALAIRES
C                    COORDONNEES DU POINT OU LA FONCTION EST CALCULEE,
C                    DANS LE REPERE ASSOCIE A L'ELEMENT DE REFERENCE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
      REAL*8        FFEL3D
C
C ARGUMENTS
C ---------
      INTEGER       NBCNX, INO
      REAL*8        X, Y, Z
C
C VARIABLES LOCALES
C -----------------
      REAL*8        EPSG
C
      REAL*8        R8PREM
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      IF ( (INO.LT.1) .OR. (INO.GT.NBCNX) )
     &   CALL UTMESS('F','FFEL3D','INDICE DE NOEUD INVALIDE')
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   FONCTIONS DE FORME D'UN ELEMENT TETRAEDRE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C 1.1 TETRAEDRE A 4 NOEUDS
C ---
      IF ( NBCNX.EQ.4 ) THEN
C
         IF ( INO.EQ.1 ) THEN
            FFEL3D = Y
         ELSE IF ( INO.EQ.2 ) THEN
            FFEL3D = Z
         ELSE IF ( INO.EQ.3 ) THEN
            FFEL3D = 1.0D0 - X - Y - Z
         ELSE
            FFEL3D = X
         ENDIF
C
C 1.2 TETRAEDRE A 10 NOEUDS
C ---
      ELSE IF ( NBCNX.EQ.10 ) THEN
C
         IF ( INO.EQ.1 ) THEN
            FFEL3D = Y * ( 2.0D0 * Y - 1.0D0 )
         ELSE IF ( INO.EQ.2 ) THEN
            FFEL3D = Z * ( 2.0D0 * Z - 1.0D0 )
         ELSE IF ( INO.EQ.3 ) THEN
            FFEL3D = ( 1.0D0 - X - Y - Z )
     &               * ( 1.0D0 - 2.0D0 * X - 2.0D0 * Y - 2.0D0 * Z )
         ELSE IF ( INO.EQ.4 ) THEN
            FFEL3D = X * ( 2.0D0 * X - 1.0D0 )
         ELSE IF ( INO.EQ.5 ) THEN
            FFEL3D = 4.0D0 * Y * Z
         ELSE IF ( INO.EQ.6 ) THEN
            FFEL3D = 4.0D0 * Z * ( 1.0D0 - X - Y - Z )
         ELSE IF ( INO.EQ.7 ) THEN
            FFEL3D = 4.0D0 * Y * ( 1.0D0 - X - Y - Z )
         ELSE IF ( INO.EQ.8 ) THEN
            FFEL3D = 4.0D0 * X * Y
         ELSE IF ( INO.EQ.9 ) THEN
            FFEL3D = 4.0D0 * X * Z
         ELSE
            FFEL3D = 4.0D0 * X * ( 1.0D0 - X - Y - Z )
         ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   FONCTIONS DE FORME D'UN ELEMENT PYRAMIDE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C 2.1 PYRAMIDE A 5 NOEUDS
C ---
      ELSE IF ( NBCNX.EQ.5 ) THEN
C
         EPSG = 1.0D+08 * R8PREM()
         IF ( (DBLE(ABS(1.0D0-Z)).LT.EPSG) .AND. (INO.NE.5) )
     &      CALL UTMESS('F','FFEL3D','ELEMENT PYRAMIDE A 5 NOEUDS .'//
     &      'LA FONCTION A ESTIMER N''EST PAS CELLE ASSOCIEE AU '//
     &      'SOMMET DE LA PYRAMIDE ET LE POINT D''ESTIMATION EST '//
     &      'PRATIQUEMENT CONFONDU AVEC LE SOMMET => RISQUE DE '//
     &      'DIVISION PAR ZERO')
C
         IF ( INO.EQ.1 ) THEN
            FFEL3D = 0.25D0 * ( -X+Y+Z-1.0D0 ) * ( -X-Y+Z-1.0D0 )
     &                      / ( 1.0D0-Z )
         ELSE IF ( INO.EQ.2 ) THEN
            FFEL3D = 0.25D0 * ( -X-Y+Z-1.0D0 ) * (  X-Y+Z-1.0D0 )
     &                      / ( 1.0D0-Z )
         ELSE IF ( INO.EQ.3 ) THEN
            FFEL3D = 0.25D0 * (  X-Y+Z-1.0D0 ) * (  X+Y+Z-1.0D0 )
     &                      / ( 1.0D0-Z )
         ELSE IF ( INO.EQ.4 ) THEN
            FFEL3D = 0.25D0 * (  X+Y+Z-1.0D0 ) * ( -X+Y+Z-1.0D0 )
     &                      / ( 1.0D0-Z )
         ELSE
            FFEL3D = Z
         ENDIF
C
C 2.2 PYRAMIDE A 13 NOEUDS
C ---
      ELSE IF ( NBCNX.EQ.13 ) THEN
C
         EPSG = 1.0D+08 * R8PREM()
         IF ( (DBLE(ABS(1.0D0-Z)).LT.EPSG) .AND. (INO.NE.5) )
     &      CALL UTMESS('F','FFEL3D','ELEMENT PYRAMIDE A 13 NOEUDS .'//
     &      'LA FONCTION A ESTIMER N''EST PAS CELLE ASSOCIEE AU '//
     &      'SOMMET DE LA PYRAMIDE ET LE POINT D''ESTIMATION EST '//
     &      'PRATIQUEMENT CONFONDU AVEC LE SOMMET => RISQUE DE '//
     &      'DIVISION PAR ZERO')
C
         IF ( INO.EQ.1 ) THEN
            FFEL3D = 0.5D0 * ( -X+Y+Z-1.0D0 ) * ( -X-Y+Z-1.0D0 )
     &                     * (  X-0.5D0 ) / ( 1.0D0-Z )
         ELSE IF ( INO.EQ.2 ) THEN
            FFEL3D = 0.5D0 * ( -X-Y+Z-1.0D0 ) * (  X-Y+Z-1.0D0 )
     &                     * (  Y-0.5D0 ) / ( 1.0D0-Z )
         ELSE IF ( INO.EQ.3 ) THEN
            FFEL3D = 0.5D0 * (  X-Y+Z-1.0D0 ) * (  X+Y+Z-1.0D0 )
     &                     * ( -X-0.5D0 ) / ( 1.0D0-Z )
         ELSE IF ( INO.EQ.4 ) THEN
            FFEL3D = 0.5D0 * (  X+Y+Z-1.0D0 ) * ( -X+Y+Z-1.0D0 )
     &                     * ( -Y-0.5D0 ) / ( 1.0D0-Z )
         ELSE IF ( INO.EQ.5 ) THEN
            FFEL3D = 2.0D0 * Z * (  Z-0.5D0 )
         ELSE IF ( INO.EQ.6 ) THEN
            FFEL3D = -0.5D0 * ( -X+Y+Z-1.0D0 ) * ( -X-Y+Z-1.0D0 )
     &                      * (  X-Y+Z-1.0D0 ) / ( 1.0D0-Z )
         ELSE IF ( INO.EQ.7 ) THEN
            FFEL3D = -0.5D0 * ( -X-Y+Z-1.0D0 ) * (  X-Y+Z-1.0D0 )
     &                      * (  X+Y+Z-1.0D0 ) / ( 1.0D0-Z )
         ELSE IF ( INO.EQ.8 ) THEN
            FFEL3D = -0.5D0 * (  X-Y+Z-1.0D0 ) * (  X+Y+Z-1.0D0 )
     &                      * ( -X+Y+Z-1.0D0 ) / ( 1.0D0-Z )
         ELSE IF ( INO.EQ.9 ) THEN
            FFEL3D = -0.5D0 * (  X+Y+Z-1.0D0 ) * ( -X+Y+Z-1.0D0 )
     &                      * ( -X-Y+Z-1.0D0 ) / ( 1.0D0-Z )
         ELSE IF ( INO.EQ.10 ) THEN
            FFEL3D = Z * ( -X+Y+Z-1.0D0 ) * ( -X-Y+Z-1.0D0 )
     &                 / ( 1.0D0-Z )
         ELSE IF ( INO.EQ.11 ) THEN
            FFEL3D = Z * ( -X-Y+Z-1.0D0 ) * (  X-Y+Z-1.0D0 )
     &                 / ( 1.0D0-Z )
         ELSE IF ( INO.EQ.12 ) THEN
            FFEL3D = Z * (  X-Y+Z-1.0D0 ) * (  X+Y+Z-1.0D0 )
     &                 / ( 1.0D0-Z )
         ELSE
            FFEL3D = Z * (  X+Y+Z-1.0D0 ) * ( -X+Y+Z-1.0D0 )
     &                 / ( 1.0D0-Z )
         ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 3   FONCTIONS DE FORME D'UN ELEMENT PENTAEDRE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C 3.1 PENTAEDRE A 6 NOEUDS
C ---
      ELSE IF ( NBCNX.EQ.6 ) THEN
C
         IF ( INO.EQ.1 ) THEN
            FFEL3D = 0.5D0 * Y * ( 1.0D0 - X )
         ELSE IF ( INO.EQ.2 ) THEN
            FFEL3D = 0.5D0 * Z * ( 1.0D0 - X )
         ELSE IF ( INO.EQ.3 ) THEN
            FFEL3D = 0.5D0 * ( 1.0D0 - Y - Z ) * ( 1.0D0 - X )
         ELSE IF ( INO.EQ.4 ) THEN
            FFEL3D = 0.5D0 * Y * ( 1.0D0 + X )
         ELSE IF ( INO.EQ.5 ) THEN
            FFEL3D = 0.5D0 * Z * ( 1.0D0 + X )
         ELSE
            FFEL3D = 0.5D0 * ( 1.0D0 - Y - Z ) * ( 1.0D0 + X )
         ENDIF
C
C 3.2 PENTAEDRE A 15 NOEUDS
C ---
      ELSE IF ( NBCNX.EQ.15 ) THEN
C
         IF ( INO.EQ.1 ) THEN
            FFEL3D = 0.5D0 * Y * ( 1.0D0 - X ) * ( 2.0D0*Y -2.0D0 -X )
         ELSE IF ( INO.EQ.2 ) THEN
            FFEL3D = 0.5D0 * Z * ( 1.0D0 - X ) * ( 2.0D0*Z -2.0D0 -X )
         ELSE IF ( INO.EQ.3 ) THEN
            FFEL3D = -0.5D0 * ( 1.0D0 - X ) * ( 1.0D0 - Y - Z )
     &                      * (  X + 2.0D0*Y + 2.0D0*Z )
         ELSE IF ( INO.EQ.4 ) THEN
            FFEL3D = 0.5D0 * Y * ( 1.0D0 + X ) * ( 2.0D0*Y -2.0D0 +X )
         ELSE IF ( INO.EQ.5 ) THEN
            FFEL3D = 0.5D0 * Z * ( 1.0D0 + X ) * ( 2.0D0*Z -2.0D0 +X )
         ELSE IF ( INO.EQ.6 ) THEN
            FFEL3D = -0.5D0 * ( 1.0D0 + X ) * ( 1.0D0 - Y - Z )
     &                      * ( -X + 2.0D0*Y + 2.0D0*Z )
         ELSE IF ( INO.EQ.7 ) THEN
            FFEL3D = 2.0D0 * Y * Z * ( 1.0D0 - X )
         ELSE IF ( INO.EQ.8 ) THEN
            FFEL3D = 2.0D0 * Z * ( 1.0D0 - Y - Z ) * ( 1.0D0 - X )
         ELSE IF ( INO.EQ.9 ) THEN
            FFEL3D = 2.0D0 * Y * ( 1.0D0 - Y - Z ) * ( 1.0D0 - X )
         ELSE IF ( INO.EQ.10 ) THEN
            FFEL3D = Y * ( 1.0D0 - X ) * ( 1.0D0 + X )
         ELSE IF ( INO.EQ.11 ) THEN
            FFEL3D = Z * ( 1.0D0 - X ) * ( 1.0D0 + X )
         ELSE IF ( INO.EQ.12 ) THEN
            FFEL3D = ( 1.0D0 - Y - Z ) * ( 1.0D0 - X ) * ( 1.0D0 + X )
         ELSE IF ( INO.EQ.13 ) THEN
            FFEL3D = 2.0D0 * Y * Z * ( 1.0D0 + X )
         ELSE IF ( INO.EQ.14 ) THEN
            FFEL3D = 2.0D0 * Z * ( 1.0D0 - Y - Z ) * ( 1.0D0 + X )
         ELSE
            FFEL3D = 2.0D0 * Y * ( 1.0D0 - Y - Z ) * ( 1.0D0 + X )
         ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 4   FONCTIONS DE FORME D'UN ELEMENT HEXAEDRE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C 4.1 HEXAEDRE A 8 NOEUDS
C ---
      ELSE IF ( NBCNX.EQ.8 ) THEN
C
         IF ( INO.EQ.1 ) THEN
            FFEL3D = 0.125D0 * ( 1.0D0-X ) * ( 1.0D0-Y ) * ( 1.0D0-Z )
         ELSE IF ( INO.EQ.2 ) THEN
            FFEL3D = 0.125D0 * ( 1.0D0+X ) * ( 1.0D0-Y ) * ( 1.0D0-Z )
         ELSE IF ( INO.EQ.3 ) THEN
            FFEL3D = 0.125D0 * ( 1.0D0+X ) * ( 1.0D0+Y ) * ( 1.0D0-Z )
         ELSE IF ( INO.EQ.4 ) THEN
            FFEL3D = 0.125D0 * ( 1.0D0-X ) * ( 1.0D0+Y ) * ( 1.0D0-Z )
         ELSE IF ( INO.EQ.5 ) THEN
            FFEL3D = 0.125D0 * ( 1.0D0-X ) * ( 1.0D0-Y ) * ( 1.0D0+Z )
         ELSE IF ( INO.EQ.6 ) THEN
            FFEL3D = 0.125D0 * ( 1.0D0+X ) * ( 1.0D0-Y ) * ( 1.0D0+Z )
         ELSE IF ( INO.EQ.7 ) THEN
            FFEL3D = 0.125D0 * ( 1.0D0+X ) * ( 1.0D0+Y ) * ( 1.0D0+Z )
         ELSE
            FFEL3D = 0.125D0 * ( 1.0D0-X ) * ( 1.0D0+Y ) * ( 1.0D0+Z )
         ENDIF
C
C 4.2 HEXAEDRE A 20 NOEUDS
C ---
      ELSE IF ( NBCNX.EQ.20 ) THEN
C
         IF ( INO.EQ.1 ) THEN
            FFEL3D = 0.125D0 * ( 1.0D0-X ) * ( 1.0D0-Y ) * ( 1.0D0-Z )
     &                       * ( -2.0D0 - X - Y - Z )
         ELSE IF ( INO.EQ.2 ) THEN
            FFEL3D = 0.125D0 * ( 1.0D0+X ) * ( 1.0D0-Y ) * ( 1.0D0-Z )
     &                       * ( -2.0D0 + X - Y - Z )
         ELSE IF ( INO.EQ.3 ) THEN
            FFEL3D = 0.125D0 * ( 1.0D0+X ) * ( 1.0D0+Y ) * ( 1.0D0-Z )
     &                       * ( -2.0D0 + X + Y - Z )
         ELSE IF ( INO.EQ.4 ) THEN
            FFEL3D = 0.125D0 * ( 1.0D0-X ) * ( 1.0D0+Y ) * ( 1.0D0-Z )
     &                       * ( -2.0D0 - X + Y - Z )
         ELSE IF ( INO.EQ.5 ) THEN
            FFEL3D = 0.125D0 * ( 1.0D0-X ) * ( 1.0D0-Y ) * ( 1.0D0+Z )
     &                       * ( -2.0D0 - X - Y + Z )
         ELSE IF ( INO.EQ.6 ) THEN
            FFEL3D = 0.125D0 * ( 1.0D0+X ) * ( 1.0D0-Y ) * ( 1.0D0+Z )
     &                       * ( -2.0D0 + X - Y + Z )
         ELSE IF ( INO.EQ.7 ) THEN
            FFEL3D = 0.125D0 * ( 1.0D0+X ) * ( 1.0D0+Y ) * ( 1.0D0+Z )
     &                       * ( -2.0D0 + X + Y + Z )
         ELSE IF ( INO.EQ.8 ) THEN
            FFEL3D = 0.125D0 * ( 1.0D0-X ) * ( 1.0D0+Y ) * ( 1.0D0+Z )
     &                       * ( -2.0D0 - X + Y + Z )
         ELSE IF ( INO.EQ.9 ) THEN
            FFEL3D = 0.25D0 * ( 1.0D0-X ) * ( 1.0D0+X )
     &                      * ( 1.0D0-Y ) * ( 1.0D0-Z )
         ELSE IF ( INO.EQ.10 ) THEN
            FFEL3D = 0.25D0 * ( 1.0D0-Y ) * ( 1.0D0+Y )
     &                      * ( 1.0D0+X ) * ( 1.0D0-Z )
         ELSE IF ( INO.EQ.11 ) THEN
            FFEL3D = 0.25D0 * ( 1.0D0-X ) * ( 1.0D0+X )
     &                      * ( 1.0D0+Y ) * ( 1.0D0-Z )
         ELSE IF ( INO.EQ.12 ) THEN
            FFEL3D = 0.25D0 * ( 1.0D0-Y ) * ( 1.0D0+Y )
     &                      * ( 1.0D0-X ) * ( 1.0D0-Z )
         ELSE IF ( INO.EQ.13 ) THEN
            FFEL3D = 0.25D0 * ( 1.0D0-Z ) * ( 1.0D0+Z )
     &                      * ( 1.0D0-X ) * ( 1.0D0-Y )
         ELSE IF ( INO.EQ.14 ) THEN
            FFEL3D = 0.25D0 * ( 1.0D0-Z ) * ( 1.0D0+Z )
     &                      * ( 1.0D0+X ) * ( 1.0D0-Y )
         ELSE IF ( INO.EQ.15 ) THEN
            FFEL3D = 0.25D0 * ( 1.0D0-Z ) * ( 1.0D0+Z )
     &                      * ( 1.0D0+X ) * ( 1.0D0+Y )
         ELSE IF ( INO.EQ.16 ) THEN
            FFEL3D = 0.25D0 * ( 1.0D0-Z ) * ( 1.0D0+Z )
     &                      * ( 1.0D0-X ) * ( 1.0D0+Y )
         ELSE IF ( INO.EQ.17 ) THEN
            FFEL3D = 0.25D0 * ( 1.0D0-X ) * ( 1.0D0+X )
     &                      * ( 1.0D0-Y ) * ( 1.0D0+Z )
         ELSE IF ( INO.EQ.18 ) THEN
            FFEL3D = 0.25D0 * ( 1.0D0-Y ) * ( 1.0D0+Y )
     &                      * ( 1.0D0+X ) * ( 1.0D0+Z )
         ELSE IF ( INO.EQ.19 ) THEN
            FFEL3D = 0.25D0 * ( 1.0D0-X ) * ( 1.0D0+X )
     &                      * ( 1.0D0+Y ) * ( 1.0D0+Z )
         ELSE
            FFEL3D = 0.25D0 * ( 1.0D0-Y ) * ( 1.0D0+Y )
     &                      * ( 1.0D0-X ) * ( 1.0D0+Z )
         ENDIF
C
C 4.3 HEXAEDRE A 27 NOEUDS
C ---
      ELSE IF ( NBCNX.EQ.27 ) THEN
C
         IF ( INO.EQ.1 ) THEN
            FFEL3D = 0.125D0 * X * ( X - 1.0D0 ) * Y * ( Y - 1.0D0 )
     &                       * Z * ( Z - 1.0D0 )
         ELSE IF ( INO.EQ.2 ) THEN
            FFEL3D = 0.125D0 * X * ( X + 1.0D0 ) * Y * ( Y - 1.0D0 )
     &                       * Z * ( Z - 1.0D0 )
         ELSE IF ( INO.EQ.3 ) THEN
            FFEL3D = 0.125D0 * X * ( X + 1.0D0 ) * Y * ( Y + 1.0D0 )
     &                       * Z * ( Z - 1.0D0 )
         ELSE IF ( INO.EQ.4 ) THEN
            FFEL3D = 0.125D0 * X * ( X - 1.0D0 ) * Y * ( Y + 1.0D0 )
     &                       * Z * ( Z - 1.0D0 )
         ELSE IF ( INO.EQ.5 ) THEN
            FFEL3D = 0.125D0 * X * ( X - 1.0D0 ) * Y * ( Y - 1.0D0 )
     &                       * Z * ( Z + 1.0D0 )
         ELSE IF ( INO.EQ.6 ) THEN
            FFEL3D = 0.125D0 * X * ( X + 1.0D0 ) * Y * ( Y - 1.0D0 )
     &                       * Z * ( Z + 1.0D0 )
         ELSE IF ( INO.EQ.7 ) THEN
            FFEL3D = 0.125D0 * X * ( X + 1.0D0 ) * Y * ( Y + 1.0D0 )
     &                       * Z * ( Z + 1.0D0 )
         ELSE IF ( INO.EQ.8 ) THEN
            FFEL3D = 0.125D0 * X * ( X - 1.0D0 ) * Y * ( Y + 1.0D0 )
     &                       * Z * ( Z + 1.0D0 )
         ELSE IF ( INO.EQ.9 ) THEN
            FFEL3D = 0.25D0 * ( 1.0D0 - X ) * ( 1.0D0 + X )
     &                      * Y * ( Y - 1.0D0 ) * Z * ( Z - 1.0D0 )
         ELSE IF ( INO.EQ.10 ) THEN
            FFEL3D = 0.25D0 * X * ( X + 1.0D0 ) * ( 1.0D0 - Y )
     &                      * ( 1.0D0 + Y ) * Z * ( Z - 1.0D0 )
         ELSE IF ( INO.EQ.11 ) THEN
            FFEL3D = 0.25D0 * ( 1.0D0 - X ) * ( 1.0D0 + X )
     &                      * Y * ( Y + 1.0D0 ) * Z * ( Z - 1.0D0 )
         ELSE IF ( INO.EQ.12 ) THEN
            FFEL3D = 0.25D0 * X * ( X - 1.0D0 ) * ( 1.0D0 - Y )
     &                      * ( 1.0D0 + Y ) * Z * ( Z - 1.0D0 )
         ELSE IF ( INO.EQ.13 ) THEN
            FFEL3D = 0.25D0 * X * ( X - 1.0D0 ) * Y * ( Y - 1.0D0 )
     &                      * ( 1.0D0 - Z ) * ( 1.0D0 + Z )
         ELSE IF ( INO.EQ.14 ) THEN
            FFEL3D = 0.25D0 * X * ( X + 1.0D0 ) * Y * ( Y - 1.0D0 )
     &                      * ( 1.0D0 - Z ) * ( 1.0D0 + Z )
         ELSE IF ( INO.EQ.15 ) THEN
            FFEL3D = 0.25D0 * X * ( X + 1.0D0 ) * Y * ( Y + 1.0D0 )
     &                      * ( 1.0D0 - Z ) * ( 1.0D0 + Z )
         ELSE IF ( INO.EQ.16 ) THEN
            FFEL3D = 0.25D0 * X * ( X - 1.0D0 ) * Y * ( Y + 1.0D0 )
     &                      * ( 1.0D0 - Z ) * ( 1.0D0 + Z )
         ELSE IF ( INO.EQ.17 ) THEN
            FFEL3D = 0.25D0 * ( 1.0D0 - X ) * ( 1.0D0 + X )
     &                      * Y * ( Y - 1.0D0 ) * Z * ( Z + 1.0D0 )
         ELSE IF ( INO.EQ.18 ) THEN
            FFEL3D = 0.25D0 * X * ( X + 1.0D0 ) * ( 1.0D0 - Y )
     &                      * ( 1.0D0 + Y ) * Z * ( Z + 1.0D0 )
         ELSE IF ( INO.EQ.19 ) THEN
            FFEL3D = 0.25D0 * ( 1.0D0 - X ) * ( 1.0D0 + X )
     &                      * Y * ( Y + 1.0D0 ) * Z * ( Z + 1.0D0 )
         ELSE IF ( INO.EQ.20 ) THEN
            FFEL3D = 0.25D0 * X * ( X - 1.0D0 ) * ( 1.0D0 - Y )
     &                      * ( 1.0D0 + Y ) * Z * ( Z + 1.0D0 )
         ELSE IF ( INO.EQ.21 ) THEN
            FFEL3D = 0.5D0 * ( 1.0D0 - X ) * ( 1.0D0 + X )
     &                     * ( 1.0D0 - Y ) * ( 1.0D0 + Y )
     &                     * Z * ( Z - 1.0D0 )
         ELSE IF ( INO.EQ.22 ) THEN
            FFEL3D = 0.5D0 * ( 1.0D0 - X ) * ( 1.0D0 + X )
     &                     * Y * ( Y - 1.0D0 )
     &                     * ( 1.0D0 - Z ) * ( 1.0D0 + Z )
         ELSE IF ( INO.EQ.23 ) THEN
            FFEL3D = 0.5D0 * X * ( X + 1.0D0 )
     &                     * ( 1.0D0 - Y ) * ( 1.0D0 + Y )
     &                     * ( 1.0D0 - Z ) * ( 1.0D0 + Z )
         ELSE IF ( INO.EQ.24 ) THEN
            FFEL3D = 0.5D0 * ( 1.0D0 - X ) * ( 1.0D0 + X )
     &                     * Y * ( Y + 1.0D0 )
     &                     * ( 1.0D0 - Z ) * ( 1.0D0 + Z )
         ELSE IF ( INO.EQ.25 ) THEN
            FFEL3D = 0.5D0 * X * ( X - 1.0D0 )
     &                     * ( 1.0D0 - Y ) * ( 1.0D0 + Y )
     &                     * ( 1.0D0 - Z ) * ( 1.0D0 + Z )
         ELSE IF ( INO.EQ.26 ) THEN
            FFEL3D = 0.5D0 * ( 1.0D0 - X ) * ( 1.0D0 + X )
     &                     * ( 1.0D0 - Y ) * ( 1.0D0 + Y )
     &                     * Z * ( Z + 1.0D0 )
         ELSE
            FFEL3D = ( 1.0D0 - X ) * ( 1.0D0 + X )
     &             * ( 1.0D0 - Y ) * ( 1.0D0 + Y )
     &             * ( 1.0D0 - Z ) * ( 1.0D0 + Z )
         ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 5   SORTIE EN ERREUR SI NOMBRE DE NOEUDS INVALIDE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      ELSE
C
         CALL UTMESS('F','FFEL3D','NOMBRE DE NOEUDS INVALIDE')
C
      ENDIF
C
C --- FIN DE FFEL3D.
      END
