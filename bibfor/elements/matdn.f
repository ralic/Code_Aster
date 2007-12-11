      SUBROUTINE MATDN ( NB1 , XR , INTSN , MADN , NKS1 , NKS2 )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/12/1999   AUTEUR SABJLMA P.LATRUBESSE 
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
C                 MADN ( 3 ,  6 * NB1  + 3 ) = TRANSLATION SEULEMENT
C
C                 AUX POINTS D INTEGRATION NORMALE
C
C ......................................................................
C
C
C
      IMPLICIT NONE
C
      REAL * 8 MADN ( 3 , 51 )
      REAL * 8 NKS1 ( 3 , 51 )
      REAL * 8 NKS2 ( 3 , 51 )
      REAL * 8 XR ( * ) 
C
      INTEGER  JN
C
      INTEGER  INTSN
C
      INTEGER NB1 
      INTEGER II 
C
CDEB
C
C---- INITIALISATION
C
      CALL R8INIR ( 3 * 51 , 0.D0 , MADN , 1 )
      CALL R8INIR ( 3 * 51 , 0.D0 , NKS1 , 1 )
      CALL R8INIR ( 3 * 51 , 0.D0 , NKS2 , 1 )
C
C---- LES ADRESSES DES FONCTIONS DE FORME ET DE LEURS DERIVEES
C     DECALAGE DE 8 NOEUDS DE SERENDIP ET 9 NOEUDS DE LAGRANGE
C
C---------- NOEUDS DE SERENDIP
C
      DO 100 JN = 1 , NB1
C
C------- PARTIE TRANSLATION
C
         DO 400 II = 1 , 3
C
            MADN ( II , ( JN - 1 ) * 6 + II     ) =
     &                       XR ( 135 + 8 * ( INTSN - 1 ) + JN )
C
            NKS1 ( II , ( JN - 1 ) * 6 + II     ) =
     &                       XR ( 207 + 8 * ( INTSN - 1 ) + JN )
C
            NKS2 ( II , ( JN - 1 ) * 6 + II     ) =
     &                       XR ( 279 + 8 * ( INTSN - 1 ) + JN )
C
 400     CONTINUE
C
 100  CONTINUE
C
CFIN
C
      END
