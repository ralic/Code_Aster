      SUBROUTINE JM1DN1
     & ( INDN , INDC , NB1 , NB2 , XR        , EPAIS , KSI3S2 , INTSX ,
     &                                                     JM1 , J1DN1 )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/02/2008   AUTEUR MACOCCO K.MACOCCO 
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
C     FONCTION :  CALCUL DU PRODUIT
C
C                 J1DN1 ( 9 ,  6 * NB1  + 3 ) =
C
C                 JTILD ( 9 , 9 ) * DNDQSI1  ( 9 ,  6 * NB1  + 3 )
C
C                 POUR LA DEFORMATION TOTALE
C
C                 AUX POINTS D INTEGRATION REDUITE OU NORMALE
C
C ......................................................................
C
C
C
      IMPLICIT NONE
C
      INTEGER      JN
C
      INTEGER         NB1 , NB2
C
      INTEGER INDN , INDC
C
      INTEGER INTSX
      INTEGER INTSX1 , INTSX2
C
      INTEGER LT1 , LT2
      INTEGER I1  , I2
C
      INTEGER L1 , L2 , L3
      INTEGER           I3 , I4 , I5
C
      REAL * 8 VI ( 3 )
C
      REAL * 8 XR ( * )
C
      REAL * 8 EPAIS
C
      REAL * 8 KSI3S2
C
      REAL * 8 JM1 ( 3 )
      REAL * 8 J1DN1 ( 9 , 51 )
C
      REAL * 8 TMPI  ( 3 )
C
C
CDEB
C
C---- INITIALISATION
C
      CALL R8INIR ( 9 * 51 , 0.D0 , J1DN1 , 1 )
C
C---- LES ADRESSES DES FONCTIONS DE FORME ET DE LEURS DERIVEES
C     SELON INDN ( VOIR ROUTINE BTDFN )
C
C
      CALL VALFOR ( INDN , LT1 , LT2 , L1 , L2 , L3 )
C
C
C---- DECALAGE DE 8 NOEUDS DE SERENDIP
C
      INTSX1 = 8 * ( INTSX - 1 )
C
C---- DECALAGE DE 9 NOEUDS DE LAGRANGE
C
      INTSX2 = 9 * ( INTSX - 1 )
C
      I1 = LT1 + INTSX1
      I2 = LT2 + INTSX1
C
      I3 = L1 + INTSX2
      I4 = L2 + INTSX2
      I5 = L3 + INTSX2
C
      CALL ASSERT ((INDC.EQ.1).OR.(INDC.EQ.0))

      IF      ( INDC . EQ . 1 ) THEN
C
C----- CALCUL COMPLET AVEC TERMES ROTATION
C
        DO 100 JN = 1 , NB2
C
C------- PARTIE ROTATION
C
C------- REMPLISSAGE DE VI ( 3 )
C
         VI ( 1 ) = EPAIS * KSI3S2 * XR ( I4 + JN )
         VI ( 2 ) = EPAIS * KSI3S2 * XR ( I5 + JN )
         VI ( 3 ) = EPAIS * 0.5D0 *  XR ( I3 + JN )
C
C------- PRODUIT  JM1 ( 3 , 3 ) * VI ( 3 )
C
         CALL PROMAT ( JM1  , 3 , 3 , 3 ,
     &                 VI   , 3 , 3 , 1 ,
     &                 TMPI )
C
C------- REMPLISSAGE DE J1DN1 ( 9 , 6 * NB1 + 3 )
C
C        JTILD-1 ( 9 , 9 ) * DNDQSI ( 9 , 6 * NB1 + 3 )
C
C
C
         IF   ( JN . LE . NB1 ) THEN
C
C---------- NOEUDS DE SERENDIP
C
C---------- BLOC U
C
            J1DN1( 1 , (JN-1) * 6 + 4 )= TMPI ( 1 )
            J1DN1( 2 , (JN-1) * 6 + 4 )= TMPI ( 2 )
            J1DN1( 3 , (JN-1) * 6 + 4 )= TMPI ( 3 )
C
C---------- BLOC V
C
            J1DN1( 4 , (JN-1) * 6 + 5 )= TMPI ( 1 )
            J1DN1( 5 , (JN-1) * 6 + 5 )= TMPI ( 2 )
            J1DN1( 6 , (JN-1) * 6 + 5 )= TMPI ( 3 )
C
C---------- BLOC W
C
            J1DN1( 7 , (JN-1) * 6 + 6 )= TMPI ( 1 )
            J1DN1( 8 , (JN-1) * 6 + 6 )= TMPI ( 2 )
            J1DN1( 9 , (JN-1) * 6 + 6 )= TMPI ( 3 )
C
C
C
C---------- PARTIE TRANSLATION
C
C---------- REMPLISSAGE DE VI ( 3 )
C
            VI ( 1 ) = XR ( I1 + JN )
            VI ( 2 ) = XR ( I2 + JN )
            VI ( 3 ) = 0.D0
C
C---------- PRODUIT  JM1 ( 3 , 3 ) * VI ( 3 )
C
            CALL PROMAT ( JM1  , 3 , 3 , 3 ,
     &                    VI   , 3 , 3 , 1 ,
     &                    TMPI )
C
C---------- BLOC U      TMPI   0        0
C
            J1DN1( 1 , (JN-1) * 6 + 1 )= TMPI ( 1 )
            J1DN1( 2 , (JN-1) * 6 + 1 )= TMPI ( 2 )
            J1DN1( 3 , (JN-1) * 6 + 1 )= TMPI ( 3 )
C
C---------- BLOC V      0      TMPI     0
C
            J1DN1( 4 , (JN-1) * 6 + 2 )= TMPI ( 1 )
            J1DN1( 5 , (JN-1) * 6 + 2 )= TMPI ( 2 )
            J1DN1( 6 , (JN-1) * 6 + 2 )= TMPI ( 3 )
C
C---------- BLOC W      0      0        TMPI
C
            J1DN1( 7 , (JN-1) * 6 + 3 )= TMPI ( 1 )
            J1DN1( 8 , (JN-1) * 6 + 3 )= TMPI ( 2 )
            J1DN1( 9 , (JN-1) * 6 + 3 )= TMPI ( 3 )
C
         ELSE
C
C------- SUPERNOEUD
C
C
C
C
C---------- BLOC U      TMPI   0        0
C
            J1DN1( 1 ,  NB1 * 6   + 1 )=   TMPI ( 1 )
            J1DN1( 2 ,  NB1 * 6   + 1 )=   TMPI ( 2 )
            J1DN1( 3 ,  NB1 * 6   + 1 )=   TMPI ( 3 )
C
C---------- BLOC V      0      TMPI     0
C
            J1DN1( 4 ,  NB1 * 6   + 2 )=   TMPI ( 1 )
            J1DN1( 5 ,  NB1 * 6   + 2 )=   TMPI ( 2 )
            J1DN1( 6 ,  NB1 * 6   + 2 )=   TMPI ( 3 )
C
C---------- BLOC W      0      0        TMPI
C
            J1DN1( 7 ,  NB1 * 6   + 3 )=   TMPI ( 1 )
            J1DN1( 8 ,  NB1 * 6   + 3 )=   TMPI ( 2 )
            J1DN1( 9 ,  NB1 * 6   + 3 )=   TMPI ( 3 )
C
       ENDIF
C
 100    CONTINUE
C
C
C
C
       ELSE IF ( INDC . EQ . 0 ) THEN
C
C
C
C
C----- CALCUL INCOMPLET SANS TERMES ROTATION
C
C
        DO 200 JN = 1 , NB1
C
C---------- NOEUDS DE SERENDIP
C
C---------- PARTIE TRANSLATION
C
C---------- REMPLISSAGE DE VI ( 3 )
C
            VI ( 1 ) = XR ( I1 + JN )
            VI ( 2 ) = XR ( I2 + JN )
            VI ( 3 ) = 0.D0
C
C---------- PRODUIT  JM1 ( 3 , 3 ) * VI ( 3 )
C
            CALL PROMAT ( JM1  , 3 , 3 , 3 ,
     &                    VI   , 3 , 3 , 1 ,
     &                    TMPI )
C
C---------- BLOC U
C
            J1DN1( 1 , (JN-1) * 6 + 1 )= TMPI ( 1 )
            J1DN1( 2 , (JN-1) * 6 + 1 )= TMPI ( 2 )
            J1DN1( 3 , (JN-1) * 6 + 1 )= TMPI ( 3 )
C
C---------- BLOC V
C
            J1DN1( 4 , (JN-1) * 6 + 2 )= TMPI ( 1 )
            J1DN1( 5 , (JN-1) * 6 + 2 )= TMPI ( 2 )
            J1DN1( 6 , (JN-1) * 6 + 2 )= TMPI ( 3 )
C
C---------- BLOC W
C
            J1DN1( 7 , (JN-1) * 6 + 3 )= TMPI ( 1 )
            J1DN1( 8 , (JN-1) * 6 + 3 )= TMPI ( 2 )
            J1DN1( 9 , (JN-1) * 6 + 3 )= TMPI ( 3 )
C
 200    CONTINUE
C
C
C
       ENDIF
C
C
C
C
CFIN
C
      END
