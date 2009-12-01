      SUBROUTINE RC32S2 ( SIJM,  SIJTH, SN )
      IMPLICIT   NONE
      REAL*8              SIJM(6), SIJTH(6), SN(2)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 16/02/2009   AUTEUR GALENNE E.GALENNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     CALCUL DU SN MAX SUR LES INSTANTS
C     ON GARDE LES DEUX VALEURS:  SIJM+STH ET SIJM-STH
C
C IN  : SIJM   : CONTRAINTES LINEARISEES OU EN PEAU (CHARGEMENTS MECA)
C IN  : STH    : CONTRAINTES LINEARISEES OU EN PEAU ( THERMOMECA)
C OUT : SN     : AMPLITUDE DE VARIATION DES CONTRAINTES DE TRESCA
C     ------------------------------------------------------------------
C
      INTEGER  I, IT1
      REAL*8   SIJMT(6), TRESCA(2)
      REAL*8   E1(2)
C DEB ------------------------------------------------------------------
C
      E1(1) = +1.D0
      E1(2) = -1.D0
      DO 12 IT1 = 1,2
        DO 14 I = 1,6
           SIJMT(I) = SIJM(I)*E1(IT1) + SIJTH(I) 
 14     CONTINUE
        CALL RCTRES ( SIJMT, TRESCA(IT1) )
 12   CONTINUE
      SN(1) = MAX(TRESCA(1),TRESCA(2) )
      SN(2) = MIN(TRESCA(1),TRESCA(2) )
C
      END
