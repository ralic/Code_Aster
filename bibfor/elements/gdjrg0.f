      SUBROUTINE GDJRG0 (KP,NNO,ENPRIM,X00,Y0,   AJACOB,ROT0)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C FONCTION: POUR UN ELEMENT DE POUTRE EN GRAND DEPLACEMENT, CALCULE, AUX
C           POINTS DE GAUSS, LE JACOBIEN ET LA MATRICE DE ROTATION DES
C           AXES PRINCIPAUX D'INERTIE EN POSITION DE REFERENCE, PAR RAP-
C           PORT AUX AXES DE COORDONNEES GENERAUX.
C
C     IN  : KP        : NUMERO DU POINT DE GAUSS
C           NNO       : NOMBRE DE NOEUDS
C           ENPRIM    : DERIVEES DES FONCTIONS DE FORME
C           X00       : COORDONNEES DES NOEUDS EN POSITION DE REFERENCE
C           Y0        : VECTEUR DE COMPOSANTES ALPHA, BETA ET GAMMA:
C                       ANGLES NAUTIQUES DES AXES PRINCIPAUX PAR RAPPORT
C                       AUX AXES GENERAUX.
C
C     OUT : AJACOB : JACOBIEN
C           ROT0   : MATRICE DE ROTATION
C ------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 ENPRIM(3,2),X00(3,3),Y0(3),ROT(3,3),ROT0(3,3),
     &       E1(3)
C
      ZERO = 0.D0
      DO 2 IC=1,3
      E1(IC) = ZERO
      DO 1 NE=1,NNO
      E1(IC) = E1(IC) + ENPRIM(NE,KP)*X00(IC,NE)
    1 CONTINUE
    2 CONTINUE
C
      AJACOB=DDOT(3,E1,1,E1,1)
      AJACOB = SQRT(AJACOB)
C
      CALL MATROT ( Y0 , ROT )
      CALL TRANSP (ROT,3,3,3,   ROT0,3)
C
      END
