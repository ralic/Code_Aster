      SUBROUTINE MATRN ( NB1 , NB2 , XR       , KSI3S2 , EPAIS , INTSN ,
     &                   VECTN , 
     &                   MATN )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/12/98   AUTEUR D6BHHMA M.ALMIKDAD 
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
C
C ......................................................................
C     FONCTION :  CALCUL DE
C
C                 MATN ( 3 ,  6 * NB1  + 3 ) =
C
C                 AUX POINTS D INTEGRATION NORMALE
C
C ......................................................................
C
C
C
      IMPLICIT NONE
C
      REAL * 8 MATN ( 3 , 51 )
      REAL * 8 XR ( * ) 
      REAL * 8 VECTN ( 9 , 3 ) 
C
      INTEGER  JN
C
      INTEGER  INTSN
C
      INTEGER NB1 , NB2
      INTEGER II , JJ 
C
      REAL * 8 VECNJ ( 3 ) , ANTNJ ( 3 , 3 )
      REAL * 8 KSI3S2
      REAL * 8 EPAIS
C
CDEB
C
C---- INITIALISATION
C
      CALL R8INIR ( 3 * 51 , 0.D0 , MATN , 1 )
C
C---- LES ADRESSES DES FONCTIONS DE FORME ET DE LEURS DERIVEES
C     DECALAGE DE 8 NOEUDS DE SERENDIP ET 9 NOEUDS DE LAGRANGE
C
C
      DO 100 JN = 1 , NB2
C
C------- NORMALE ET ANTISYM AU NOEUD JN
C
         DO 201 II = 1 , 3
            VECNJ ( II ) = VECTN ( JN , II )
 201     CONTINUE
C
         CALL ANTISY ( VECNJ , 1.D0 , ANTNJ )
C
         IF   ( JN . LE . NB1 ) THEN
C
C---------- NOEUDS DE SERENDIP
C
C---------- PARTIE TRANSLATION
C
            DO 400 JJ = 1 , 3
                  MATN ( JJ , ( JN - 1 ) * 6 + JJ     ) =
     &   XR ( 135 + 8 * ( INTSN - 1 ) + JN )
 400        CONTINUE
C
C---------- PARTIE ROTATION
C
            DO 300 JJ = 1 , 3 
               DO 310 II = 1 , 3 
                  MATN ( II , ( JN - 1 ) * 6 + JJ + 3 ) = 
     & - EPAIS * KSI3S2 
     & * XR ( 459 + 9 * ( INTSN - 1 ) + JN ) * ANTNJ ( II , JJ ) 
 310           CONTINUE
 300        CONTINUE
C
         ELSE
C
C------- SUPERNOEUD
C
C---------- PARTIE ROTATION SEULEMENT
C
            DO 500 JJ = 1 , 3
               DO 510 II = 1 , 3
                  MATN ( II ,    NB1     * 6 + JJ     ) =
     & - EPAIS * KSI3S2 
     & * XR ( 459 + 9 * ( INTSN - 1 ) + JN ) * ANTNJ ( II , JJ )
C
 510           CONTINUE
 500        CONTINUE
C
         ENDIF
C
 100     CONTINUE
C
CFIN
C
      END
