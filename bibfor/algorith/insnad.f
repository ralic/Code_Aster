        SUBROUTINE INSNAD ( I1 , J2 , TAU , SEUIL )
        IMPLICIT REAL*8 (A-H,O-Z) 
C       -----------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/10/96   AUTEUR INBHHOM O.MERABET 
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
C       -----------------------------------------------------------
C       NADAI_BETON :  CALCUL DU SEUIL
C                      F   =  ( TOCT + A * SOCT ) / B  - TAU
C                      CRITERE DE NADAI-COMPRESSION (BETON)
C
C      IN   :  I1 = PREMIER INVARIANT DE CONTRAINTE
C              J2 = SECOND INVARIANT DU DEVIATEUR DE CONTRAINTE
C               TAU =  ECROUISSAGE
C
C       OUT SEUIL :  SEUIL CRITERE NADAI-COMPRESSION
C       -----------------------------------------------------------
        REAL*8    SEUIL, A, B , I1, J2, TAU , SOCT , TOCT , BETA
C
       BETA = 1.16D0
       TOCT = SQRT(2.D0/3.D0 * J2)
       SOCT = I1 / 3.D0
       A = SQRT(2.D0) * (BETA - 1.D0) / (2.D0*BETA - 1.D0)
       B = SQRT(2.D0) / 3.D0 * BETA   / (2.D0*BETA - 1.D0)
       SEUIL = ( TOCT + A * SOCT ) / B  - TAU
       END
