      SUBROUTINE GAUSS2(ALIAS,TYPI,XPG,YPG,NORD,HPG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/08/2002   AUTEUR ADBHHPM P.MASSIN 
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
C TOLE CRP_20
C.......................................................................

C BUT: CALCUL DES POINTS DE GAUSS ET LEUR POIDS

C ENTREES  ---> ALIAS : NOM D'ALIAS DE L'ELEMENT
C          ---> TYPI  : GAUSS CLASSIQUE, SOMMETS OU POINT INTERNE
C          ---> NORD  : L'ORDRE DE PT DE GAUSSE

C SORTIES  <--- XPG,YPG     : COORDONNEES DES POINTS DE GAUSS
C          <--- HPG         : POIDS DES POINTS DE GAUSS
C.......................................................................

      IMPLICIT NONE
      REAL*8 XPG,YPG,HPG
      INTEGER NORD,TYPI
      CHARACTER*8 ALIAS
C_______________________________________________________________________


      IF (ALIAS(1:3).EQ.'QU4') THEN

        IF (TYPI.EQ.2) THEN
C LES POINTS DE GAUSS
          IF (NORD.EQ.1) THEN
            XPG = -1/SQRT(3.D0)
            YPG = 1/SQRT(3.D0)
          ELSE IF (NORD.EQ.2) THEN
            XPG = -1/SQRT(3.D0)
            YPG = -1/SQRT(3.D0)
          ELSE IF (NORD.EQ.3) THEN
            XPG = 1/SQRT(3.D0)
            YPG = -1/SQRT(3.D0)
          ELSE IF (NORD.EQ.4) THEN
            XPG = 1/SQRT(3.D0)
            YPG = 1/SQRT(3.D0)
          ELSE
            CALL JXABOR()
          END IF
          HPG = 1.D0
C LES NOEUDS
        ELSE IF (TYPI.EQ.1) THEN

          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = 1.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = -1.D0
            YPG = -1.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = 1.D0
            YPG = -1.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = 1.D0
            YPG = 1.D0
          ELSE
            CALL JXABOR()
          END IF
          HPG = 1.D0
C SIMPSON
        ELSE IF (TYPI.EQ.3) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = 1.D0
            HPG = 1.D0/9.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = -1.D0
            YPG = -1.D0
            HPG = 1.D0/9.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = 1.D0
            YPG = -1.D0
            HPG = 1.D0/9.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = 1.D0
            YPG = 1.D0
            HPG = 1.D0/9.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG = -1.D0
            YPG = -0.D0
            HPG = 4.D0/9.D0
          ELSE IF (NORD.EQ.6) THEN
            XPG = 0.D0
            YPG = -1.D0            
            HPG = 4.D0/9.D0
          ELSE IF (NORD.EQ.7) THEN
            XPG = 1.D0
            YPG = 0.D0            
            HPG = 4.D0/9.D0
          ELSE IF (NORD.EQ.8) THEN
            XPG = 0.D0
            YPG = 1.D0            
            HPG = 4.D0/9.D0
          ELSE IF (NORD.EQ.9) THEN
            XPG = 0.D0
            YPG = 0.D0            
            HPG = 16.D0/9.D0
          ELSE
            CALL JXABOR()
          END IF
        ELSE
          CALL JXABOR()
        END IF

      ELSE IF (ALIAS(1:3).EQ.'QU8') THEN

        IF (TYPI.EQ.2) THEN
C LES POINTS DE GAUSS
          IF (NORD.EQ.1) THEN
            XPG = -1/SQRT(3.D0)
            YPG = 1/SQRT(3.D0)
          ELSE IF (NORD.EQ.2) THEN
            XPG = -1/SQRT(3.D0)
            YPG = -1/SQRT(3.D0)
          ELSE IF (NORD.EQ.3) THEN
            XPG = 1/SQRT(3.D0)
            YPG = -1/SQRT(3.D0)
          ELSE IF (NORD.EQ.4) THEN
            XPG = 1/SQRT(3.D0)
            YPG = 1/SQRT(3.D0)
          ELSE
            CALL JXABOR()
          END IF
          HPG = 1.D0
C LES NOEUDS
        ELSE IF (TYPI.EQ.1) THEN

          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = 1.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = -1.D0
            YPG = -1.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = 1.D0
            YPG = -1.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = 1.D0
            YPG = 1.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG = -1.D0
            YPG = 0.D0
          ELSE IF (NORD.EQ.6) THEN
            XPG = 0.D0
            YPG = -1.D0
          ELSE IF (NORD.EQ.7) THEN
            XPG = 1.D0
            YPG = 0.D0
          ELSE IF (NORD.EQ.8) THEN
            XPG = 0.D0
            YPG = 1.D0
          ELSE
            CALL JXABOR()
          END IF
          HPG = 1.D0/2.D0

C SIMPSON
        ELSE IF (TYPI.EQ.3) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = 1.D0
            HPG = 1.D0/9.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = -1.D0
            YPG = -1.D0
            HPG = 1.D0/9.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = 1.D0
            YPG = -1.D0
            HPG = 1.D0/9.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = 1.D0
            YPG = 1.D0
            HPG = 1.D0/9.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG = -1.D0
            YPG = -0.D0
            HPG = 4.D0/9.D0
          ELSE IF (NORD.EQ.6) THEN
            XPG = 0.D0
            YPG = -1.D0            
            HPG = 4.D0/9.D0
          ELSE IF (NORD.EQ.7) THEN
            XPG = 1.D0
            YPG = 0.D0            
            HPG = 4.D0/9.D0
          ELSE IF (NORD.EQ.8) THEN
            XPG = 0.D0
            YPG = 1.D0            
            HPG = 4.D0/9.D0
          ELSE IF (NORD.EQ.9) THEN
            XPG = 0.D0
            YPG = 0.D0            
            HPG = 16.D0/9.D0
          ELSE
            CALL JXABOR()
          END IF
        ELSE
          CALL JXABOR()
        END IF
C_______________________________________________________________________

      ELSE IF (ALIAS(1:3).EQ.'TR3') THEN

C    POINTS DE GAUSS
        IF (TYPI.EQ.2) THEN
          IF (NORD.EQ.1) THEN
            XPG = -2/3.D0
            YPG = 1/3.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = -2/3.D0
            YPG = -2/3.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = 1/3.D0
            YPG = -2/3.D0
          ELSE
            CALL JXABOR()
          END IF
          HPG = 2/3.D0
C   NOEUDS
        ELSE IF (TYPI.EQ.1) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = 1.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = -1.D0
            YPG = -1.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = 1.D0
            YPG = -1.D0
          ELSE
            CALL JXABOR()
          END IF
          HPG = 2/3.D0
C SIMPSON
        ELSE IF (TYPI.EQ.3) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = 1.D0
            HPG = 2.D0/15.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = -1.D0
            YPG = -1.D0
            HPG = 2.D0/15.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = 1.D0
            YPG = -1.D0
            HPG = 2.D0/15.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = -1.D0
            YPG = 0.D0
            HPG = 8.D0/15.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG = 0.D0
            YPG = -1.D0
            HPG = 8.D0/15.D0
          ELSE IF (NORD.EQ.6) THEN
            XPG = 0.D0
            YPG = 0.D0            
            HPG = 8.D0/15.D0
          ELSE
            CALL JXABOR()
          END IF
        ELSE
          CALL JXABOR()
        END IF

      ELSE IF (ALIAS(1:3).EQ.'TR6') THEN

C    POINTS DE GAUSS
        IF (TYPI.EQ.2) THEN
          IF (NORD.EQ.1) THEN
            XPG = -2/3.D0
            YPG = 1/3.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = -2/3.D0
            YPG = -2/3.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = 1/3.D0
            YPG = -2/3.D0
          ELSE
            CALL JXABOR()
          END IF
          HPG = 2/3.D0
C   NOEUDS
        ELSE IF (TYPI.EQ.1) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = 1.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = -1.D0
            YPG = -1.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = 1.D0
            YPG = -1.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = -1.D0
            YPG = 0.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG = 0.D0
            YPG = -1.D0
          ELSE IF (NORD.EQ.6) THEN
            XPG = 0.D0
            YPG = 0.D0
          ELSE
            CALL JXABOR()
          END IF
          HPG = 2/6.D0
C   SIMPSON
        ELSE IF (TYPI.EQ.3) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = 1.D0
            HPG = 2.D0/15.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = -1.D0
            YPG = -1.D0
            HPG = 2.D0/15.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = 1.D0
            YPG = -1.D0
            HPG = 2.D0/15.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = -1.D0
            YPG = 0.D0
            HPG = 8.D0/15.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG = 0.D0
            YPG = -1.D0
            HPG = 8.D0/15.D0
          ELSE IF (NORD.EQ.6) THEN
            XPG = 0.D0
            YPG = 0.D0
            HPG = 8.D0/15.D0
          ELSE
            CALL JXABOR()
          END IF
        ELSE
          CALL JXABOR()
        END IF



      ELSE IF (ALIAS(1:3).EQ.'SG2') THEN

C POINTS DE GAUSS

        IF (TYPI.EQ.2) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1/SQRT(3.D0)
            YPG = 0.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = 1/SQRT(3.D0)
            YPG = 0.D0
          ELSE
            CALL JXABOR()
          END IF
          HPG = 1.D0
C LES NOEUDS
        ELSE IF (TYPI.EQ.1) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = 0.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = 1.D0
            YPG = 0.D0
          ELSE
            CALL JXABOR()
          END IF
          HPG = 1.D0
C SYMPSON
        ELSE IF (TYPI.EQ.3) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = 0.D0
            HPG = 1.D0/3.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = 0.D0
            YPG = 0.D0
            HPG = 4.D0/3.D0
           ELSE IF (NORD.EQ.3) THEN
            XPG = 1.D0
            YPG = 0.D0
            HPG = 1.D0/3.D0
          ELSE
            CALL JXABOR()
          END IF
C SYMPSON1
        ELSE IF (TYPI.EQ.4) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = 0.D0
            HPG = 1.D0/6.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = -0.5D0
            YPG = 0.D0
            HPG = 2.D0/3.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = 0.D0
            YPG = 0.D0
            HPG = 1.D0/3.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = 0.5D0
            YPG = 0.D0
            HPG = 2.D0/3.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG = 1.D0
            YPG = 0.D0
            HPG = 1.D0/6.D0
          ELSE
            CALL JXABOR()
          END IF

C    SYMPSON2
        ELSE IF (TYPI.EQ.5) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = 0.D0
            HPG = 1.D0/12.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = -0.75D0
            YPG = 0.D0
            HPG = 1.D0/3.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = -0.5D0
            YPG = 0.D0
            HPG = 1.D0/6.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = -0.25D0
            YPG = 0.D0
            HPG = 1.D0/3.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG = 0.D0
            YPG = 0.D0
            HPG = 1.D0/6.D0
          ELSE IF (NORD.EQ.6) THEN
            XPG = 0.25D0
            YPG = 0.D0
            HPG = 1.D0/3.D0
          ELSE IF (NORD.EQ.7) THEN
            XPG = 0.5D0
            YPG = 0.D0
            HPG = 1.D0/6.D0
          ELSE IF (NORD.EQ.8) THEN
            XPG = 0.75D0
            YPG = 0.D0
            HPG = 1.D0/3.D0
          ELSE IF (NORD.EQ.9) THEN
            XPG = 1.D0
            YPG = 0.D0
            HPG = 1.D0/12.D0
          ELSE
            CALL JXABOR()
          END IF
        ELSE
          CALL JXABOR()
        END IF

      ELSE IF (ALIAS(1:3).EQ.'SG3') THEN

C POINTS DE GAUSS

        IF (TYPI.EQ.2) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1/SQRT(3.D0)
            YPG = 0.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = 1/SQRT(3.D0)
            YPG = 0.D0
          ELSE
            CALL JXABOR()
          END IF
          HPG = 1.D0
C LES NOEUDS
        ELSE IF (TYPI.EQ.1) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = 0.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = 0.D0
            YPG = 0.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = 1.D0
            YPG = 0.D0
          ELSE
            CALL JXABOR()
          END IF
          HPG = 2/3.D0
C SYMPSON
        ELSE IF (TYPI.EQ.3) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = 0.D0
            HPG = 1.D0/3.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = 0.D0
            YPG = 0.D0
            HPG = 4.D0/3.D0
           ELSE IF (NORD.EQ.3) THEN
            XPG = 1.D0
            YPG = 0.D0
            HPG = 1.D0/3.D0
          ELSE
            CALL JXABOR()
          END IF
C SYMPSON1
        ELSE IF (TYPI.EQ.4) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = 0.D0
            HPG = 1.D0/6.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = -0.5D0
            YPG = 0.D0
            HPG = 2.D0/3.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = 0.D0
            YPG = 0.D0
            HPG = 1.D0/3.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = 0.5D0
            YPG = 0.D0
            HPG = 2.D0/3.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG = 1.D0
            YPG = 0.D0
            HPG = 1.D0/6.D0
          ELSE
            CALL JXABOR()
          END IF

C    SYMPSON2
        ELSE IF (TYPI.EQ.5) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = 0.D0
            HPG = 1.D0/12.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = -0.75D0
            YPG = 0.D0
            HPG = 1.D0/3.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = -0.5D0
            YPG = 0.D0
            HPG = 1.D0/6.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = -0.25D0
            YPG = 0.D0
            HPG = 1.D0/3.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG = 0.D0
            YPG = 0.D0
            HPG = 1.D0/6.D0
          ELSE IF (NORD.EQ.6) THEN
            XPG = 0.25D0
            YPG = 0.D0
            HPG = 1.D0/3.D0
          ELSE IF (NORD.EQ.7) THEN
            XPG = 0.5D0
            YPG = 0.D0
            HPG = 1.D0/6.D0
          ELSE IF (NORD.EQ.8) THEN
            XPG = 0.75D0
            YPG = 0.D0
            HPG = 1.D0/3.D0
          ELSE IF (NORD.EQ.9) THEN
            XPG = 1.D0
            YPG = 0.D0
            HPG = 1.D0/12.D0
          ELSE
            CALL JXABOR()
          END IF
        ELSE
          CALL JXABOR()
        END IF
      ELSE
        CALL JXABOR()
      END IF
C_______________________________________________________________________

      END
