      SUBROUTINE RC32ST ( SIJM, NBINST, STH, SN )
      IMPLICIT   NONE
      INTEGER             NBINST
      REAL*8              SIJM(6), STH(6*NBINST), SN
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 16/02/2009   AUTEUR GALENNE E.GALENNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C IN  : SIJM   : CONTRAINTES LINEARISEES OU EN PEAU (CHARGEMENTS MECA)
C IN  : NBINST : NOMBRE D'INTANTS DE CALCUL THERMOMECA
C IN  : STH    : CONTRAINTES LINEARISEES OU EN PEAU ( THERMOMECA)
C OUT : SN     : AMPLITUDE DE VARIATION DES CONTRAINTES DE TRESCA
C     ------------------------------------------------------------------
C
      INTEGER  I, IT1
      REAL*8   SIJMT(6), TRESCA
      REAL*8   E1(2)
C DEB ------------------------------------------------------------------
C
      SN = 0.D0
C
C --- CALCUL MECANIQUE :
C     ----------------
      IF ( NBINST .EQ. 0 ) THEN
         CALL RCTRES ( SIJM, TRESCA )
         SN = TRESCA
C
C --- CALCUL THERMOMECANIQUE EN TMIN ET TMAX
C     --------------------------------------
      ELSE
        E1(1) = +1.D0
        E1(2) = -1.D0
            DO 12 IT1 = 1,2
              DO 14 I = 1,6
                 SIJMT(I) = SIJM(I)*E1(IT1) + STH(I)
 14           CONTINUE
              CALL RCTRES ( SIJMT, TRESCA )
              SN = MAX( SN , TRESCA )
 12         CONTINUE
      END IF
C
      END
