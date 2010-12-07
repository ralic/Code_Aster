      SUBROUTINE TRGFCT ( FCTTAB )
C_____________________________________________________________________
C
C     TRGFCT
C
C      CALCUL DES PARAMETRES TRIGONOMÉTRIQUES POUR LES (36) FACETTES
C
C_____________________________________________________________________
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/10/2010   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================


        IMPLICIT NONE

C
C     ! FACETTES POUR METHODE DE CAPRA ET MAURY
C
C       36 FACETTES
C       NOMBRE DE DIVISIONS ENTRE -PI/2 ET +PI/2
        REAL*8 FCTTAB(36,3)

C       ANGLE DE LA FACETTE (-PI/2 <= X < +PI/2)
        REAL*8 ANGLE
        REAL*8 PAS
        INTEGER          I

        FCTTAB(1,1)  = 0D0

        FCTTAB(1,2)  = 1D0
        FCTTAB(1,3)  = 0D0

C       -PI
        ANGLE = -4D0 * ATAN2(1.D0,1.D0)
C       2PI/N
        PAS = -2D0 * ANGLE / 36D0

C       POUR CHAQUE FACETTE, LES VALEURS SONT
C         C = COS^2
C         S = SIN^2
C         R = 2 SIN COS
        DO 20 I = 2, 36
          ANGLE = ANGLE + PAS
          FCTTAB(I,1) = 0.5D0 * (1D0 + COS(ANGLE))
          FCTTAB(I,2) = 1D0 - FCTTAB(I,1)
          FCTTAB(I,3) = SIN(ANGLE)
   20   CONTINUE
      END
