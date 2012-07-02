      SUBROUTINE FPOULI (NX,L1,L2,NORML1,NORML2,   VECTER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ......................................................................
C    - FONCTION REALISEE:  CALCUL FORCES INTERNES MEPOULI
      IMPLICIT NONE
C                          OPTION : 'FULL_MECA        '
C                          OPTION : 'RAPH_MECA        '
C
C    - ARGUMENTS:
C        DONNEES:
C
C ......................................................................
C
      REAL*8             NX,L1(3),L2(3)
      REAL*8             NORML1,NORML2,COEF1,COEF2
      REAL*8             VECTER(*)
      INTEGER            I,IVEC
C
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      COEF1 = NX / NORML1
      COEF2 = NX / NORML2
      IVEC = 0
C
C*** LIGNES 1, 2, 3
C
      DO 11 I=1,3
      IVEC = IVEC + 1
      VECTER(IVEC) = COEF1 * L1(I)
   11 CONTINUE
C
C*** LIGNES 4, 5, 6
C
      DO 21 I=1,3
        IVEC = IVEC + 1
        VECTER(IVEC) = COEF2 * L2(I)
   21 CONTINUE
C
C*** LIGNES 7, 8, 9
C
      DO 31 I=1,3
        IVEC = IVEC + 1
        VECTER(IVEC) = -VECTER(I) - VECTER(I+3)
   31 CONTINUE
C
      END
