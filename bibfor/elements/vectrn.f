      SUBROUTINE VECTRN ( NB2 , VECTPT , VECTN  , VECTHE , 
     &                                   VECNPH , BLAM )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 27/01/99   AUTEUR D6BHHMA M.ALMIKDAD 
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
      INTEGER NB2 
C
      INTEGER II , JJ
C
      INTEGER IN
C
      REAL * 8 VECTN  ( 9 , 3 ) 
C
      REAL * 8 VECTHE ( 9 , 3 ) 
C
      REAL * 8 VECNPH ( 9 , 3 ) 
C
      REAL * 8 VECNI ( 3 ) , VECNPI ( 3 )
C
      REAL * 8 THETA ( 3 ) , LAMBDA ( 3 , 3 ) 
      REAL * 8               LAMBD0 ( 3 , 3 )
      REAL * 8               BARL   ( 3 , 3 )
C
      REAL * 8 BLAM  ( 9 , 3 , 3 )
C
      REAL * 8 VECTPT ( 9 , 2 , 3 )
C
CDEB
C
C---- INITIALISATIONS
C
      CALL R8INIR ( 9 * 3     , 0.D0 , VECNPH , 1 )
C
      CALL R8INIR ( 9 * 3 * 3 , 0.D0 , BLAM   , 1 )
C
C---- EN CHAQUE NOEUD
C
      DO 100 IN = 1 , NB2 
C
C---- COMPOSANTE PAR COMPOSANTE
C
         DO 110 II = 1 , 3
C
C---------- NORMALE INITIALE 
C
            VECNI ( II ) = VECTN  ( IN , II )
C
C---------- VECTEUR DE ROTATION
C
            THETA ( II ) = VECTHE ( IN , II )
C
C---------- TERMES DE LA LAMBD0
C
            LAMBD0 ( II , 1 ) = VECTPT ( IN , 1 , II )
            LAMBD0 ( II , 2 ) = VECTPT ( IN , 2 , II )
            LAMBD0 ( II , 3 ) = VECTN  ( IN ,     II )
C
 110     CONTINUE
C
C------- TRANSFORMEE DE LA NORMALE PAR GRANDE ROTATION
C
C------- MATRICE DE ROTATION 
C
         CALL MAROTA ( THETA , LAMBDA )
C
C------- TRANSFORMEE DE LA NORMALE
C
         CALL PROMAT ( LAMBDA , 3 , 3 , 3 , 
     &                 VECNI  , 3 , 3 , 1 ,  
     &                 VECNPI )
C
C------- MATRICE DE ROTATION TOTALE
C
         CALL PROMAT ( LAMBDA , 3 , 3 , 3 ,
     &                 LAMBD0 , 3 , 3 , 3 ,
     &                 BARL   )
C
C
         DO 130 JJ = 1 , 3
C
C---------- EN CHAQUE NOEUD
C
            VECNPH ( IN , JJ ) = VECNPI ( JJ )
C
            DO 140 II = 1 , 3
C
               BLAM ( IN , II , JJ ) = BARL ( II , JJ ) 
C
 140        CONTINUE
C
 130     CONTINUE
C
 100  CONTINUE
C
CFIN
C
      END
