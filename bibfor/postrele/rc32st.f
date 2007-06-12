      SUBROUTINE RC32ST ( TYPE, SIJM, NBINST, STH, SN )
      IMPLICIT   NONE
      INTEGER             NBINST
      REAL*8              SIJM(6), STH(6*NBINST), SN
      CHARACTER*4         TYPE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 30/05/2005   AUTEUR CIBHHLV L.VIVAN 
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
C IN  : TYPE   : ='COMB'  +-SIGM_M + SIGM_TH
C                ='SITU'    SIGM_M + SIGM_TH
C IN  : SIJM   : CONTRAINTES LINEARISEES OU EN PEAU (CHARGEMENTS MECA)
C IN  : NBINST : NOMBRE D'INTANTS DE CALCUL THERMOMECA
C IN  : STH    : CONTRAINTES LINEARISEES OU EN PEAU ( THERMOMECA)
C OUT : SN     : AMPLITUDE DE VARIATION DES CONTRAINTES DE TRESCA
C     ------------------------------------------------------------------
C
      INTEGER  I, I1, IT, IT1
      REAL*8   SIJMT(6), EQUI(6)
      REAL*8   E1(2)
C DEB ------------------------------------------------------------------
C
      SN = 0.D0
C
C --- CALCUL MECANIQUE :
C     ----------------
      IF ( NBINST .EQ. 0 ) THEN
         CALL FGEQUI ( SIJM, 'SIGM', 3, EQUI )
         SN = EQUI(2)
C
C --- CALCUL THERMOMECANIQUE (DEPENDANT DU TEMPS)
C     -------------------------------------------
      ELSE
        E1(1) = -1.D0
        E1(2) = +1.D0
        IF ( TYPE .EQ. 'COMB' ) THEN
          DO 10 IT = 1,NBINST
            DO 12 IT1 = 1,2
              DO 14 I = 1,6
                 SIJMT(I) = SIJM(I)*E1(IT1) + STH((IT-1)*6+I)
 14           CONTINUE
              CALL FGEQUI ( SIJMT, 'SIGM', 3, EQUI )
              SN = MAX( SN , EQUI(2) )
 12         CONTINUE
 10       CONTINUE
        ELSE
          DO 20 IT = 1,NBINST
            DO 22 I = 1,6
              SIJMT(I) = SIJM(I) + STH((IT-1)*6+I)
 22         CONTINUE
            CALL FGEQUI ( SIJMT, 'SIGM', 3, EQUI )
            SN = MAX( SN , EQUI(2) )
 20       CONTINUE
        END IF
      END IF
C
      END
