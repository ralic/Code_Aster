      SUBROUTINE NOPG2D ( NCMP, NPG, NNO, LGPG, FONCF, NO, PG )
      IMPLICIT   NONE
      INTEGER             NCMP, NPG, NNO, LGPG
      REAL*8              FONCF(*), PG(*), NO(*)
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/05/99   AUTEUR CIBHHLV L.VIVAN 
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
C     CALCUL DES CHAMELEM AUX POINTS DE GAUSS A PARTIR DES VALEURS 
C                         AUX NOEUDS
C
C     ELEMENTS 2D ET 2D AXISYMETRIQUE
C ......................................................................
C
      INTEGER  IC, I, KP, K
      REAL*8   S
C     ------------------------------------------------------------------
C
      DO 10 IC = 1 , NCMP
         DO 12 KP = 1 , NPG
            K = (KP-1)*NNO
            S = 0.D0
            DO 14 I = 1 , NNO
               S = S + NO(LGPG*(I-1)+IC) * FONCF(K+I)
 14         CONTINUE
            PG(LGPG*(KP-1)+IC) = S
 12      CONTINUE
 10   CONTINUE
C
      END
