      SUBROUTINE ROGLLO ( NB1 , NB2 , VRG , BLAM , CTOR , KNN )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/07/2001   AUTEUR ADBHHPM P.MASSIN 
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
C
      IMPLICIT NONE
C
C ......................................................................
C     FONCTION  :  ROTATION DES BLOCS 3 3 DE LA MATRICE DE RIGIDITE 
C                  DU REPERE GLOBAL AU REPERE LOCAL
C                  COQUE_3D
C
C           (                                  )         (    U    )
C           ( ( M_TRANSLATION ) (  COUPLAGE )  )         (    V    )
C           (                                  )         (    W    )
C ( VRG ) = (                                  )   ( U )=(         )
C       I   (                                  )         ( THETA_X )
C           ( (   COUPLAGE    ) ( M_ROTATION ) )         ( THETA_Y )
C           (                                  )         ( THETA_Z )
C                                                I                   I
C   ON TOURNE  ( M_ROTATION ) SEULEMENT
C
C ......................................................................
C
      INTEGER IN 
      INTEGER II , JJ
      INTEGER I  , J 
      INTEGER NB1 , NB2
      INTEGER IRIG
C
C---- DECLARATIONS RIGIDITE GEOMETRIQUE
C
      REAL * 8 VRG  ( 2601 )
      REAL * 8 BLAM ( 9 , 3 , 3 )
      REAL * 8 RIGRL    ( 3 , 3 )
      REAL * 8 RIGRG ( 3 , 3 )
      REAL * 8 BID33    ( 3 , 3 )
      REAL * 8 R8PREM
      REAL * 8 CTOR , KNN , XMIN
C
      REAL * 8 BARL ( 3 , 3 )
C
C DEB
C
C---- A CHAQUE ITERATION
C
      XMIN = 1.D0 / R8PREM ( ) 
C
C---- EN CHAQUE NOEUD
C
      DO 401 IN = 1 , NB2
C
C------- ON RECUPERE BARLAMBDA
C
            DO 411 JJ = 1 , 3 
               DO 422 II = 1 , 3 
C
                  BARL ( II , JJ ) = BLAM ( IN , II , JJ )
C
 422           CONTINUE
 411        CONTINUE
C
C-------    ON CONSTRUIT RIGRG
C
               IF ( IN . LE . NB1 ) THEN
C
C--------------    NOEUDS DE SERENDIP
                   DO 431 JJ = 1 , 3
                      DO 441 II = 1 , 3
                          J    = 6 * ( IN - 1 ) + JJ + 3
                          I    = 6 * ( IN - 1 ) + II + 3
                          IRIG = ( 6 * NB1 + 3 ) * ( J - 1 ) + I
                          RIGRG ( II , JJ ) = VRG ( IRIG )
 441                  CONTINUE
 431               CONTINUE
C
            ELSE
C
C--------------    SUPERNOEUD
                   DO 451 JJ = 1 , 3
                      DO 461 II = 1 , 3
                          J    = 6 * NB1        + JJ
                          I    = 6 * NB1        + II
                          IRIG = ( 6 * NB1 + 3 ) * ( J - 1 ) + I
                          RIGRG ( II , JJ ) = VRG ( IRIG )
 461                  CONTINUE
 451               CONTINUE
C
            ENDIF
C
C-------    ROTATION DE RIGRG : LOCALES --> GLOBALES 
C
C           RIGRL =  ( LAMBDA0 )   * SIGMT * ( LAMBDA0 ) T
C
C
            CALL  BTKB ( 3 , 3 , 3 , RIGRG , BARL  , BID33 , RIGRL )
C
C-------    ON COMPARE LES DEUX PREMIERS TERMES DIAGONAUX DE RIGRL
C
            IF ( ABS(RIGRL ( 1 , 1 )) .LT. XMIN ) THEN
               XMIN = ABS(RIGRL ( 1 , 1 ))
            ENDIF
            IF ( ABS(RIGRL ( 2 , 2 )) .LT. XMIN ) THEN
               XMIN = ABS(RIGRL ( 2 , 2 ))
            ENDIF
C
 401     CONTINUE
C
            KNN = CTOR * XMIN
C
C FIN
C
      END
