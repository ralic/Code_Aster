      SUBROUTINE DSTLXY ( CODI, HLT2, AN, DEPF, LAMBDA )
      IMPLICIT  NONE
      REAL*8    HLT2(4,6), AN(3,9), DEPF(9), CODI(*), LAMBDA(4)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2011   AUTEUR DESOZA T.DESOZA 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C     ------------------------------------------------------------------
C     'LAMBDA' DE L'ELEMENT DE PLAQUE DST
C     ------------------------------------------------------------------
      INTEGER  I, J, K
      REAL*8   C(3),S(3)
      REAL*8   TA(6,3),BLA(4,3),BLN(4,9)
C     ------------------------------------------------------------------
C
C       -------------- BLA = HLT2.TA ------------------------------
      C(1) = CODI(1)
      C(2) = CODI(2)
      C(3) = CODI(3)
      S(1) = CODI(4)
      S(2) = CODI(5)
      S(3) = CODI(6)
      DO 200 K = 1,6
        DO 201 J = 1,3
          TA(K,J) = 0.D0
  201   CONTINUE
  200 CONTINUE
      TA(1,1) = -8.D0*C(1)
      TA(2,3) = -8.D0*C(3)
      TA(3,1) = -4.D0*C(1)
      TA(3,2) =  4.D0*C(2)
      TA(3,3) = -4.D0*C(3)
      TA(4,1) = -8.D0*S(1)
      TA(5,3) = -8.D0*S(3)
      TA(6,1) = -4.D0*S(1)
      TA(6,2) =  4.D0*S(2)
      TA(6,3) = -4.D0*S(3)
      DO 204 I = 1,4
        DO 206 J = 1,3
          BLA(I,J) = 0.D0
          DO 208 K = 1,6
            BLA(I,J) = BLA(I,J) + HLT2(I,K)*TA(K,J)
 208      CONTINUE
 206    CONTINUE
 204  CONTINUE
C       -------- LAMBDA = BLA.AN.DEPF ------------------------------
      DO 212 I = 1,4
        LAMBDA(I) = 0.D0
 212  CONTINUE
      DO 214 I = 1,4
        DO 216 J = 1,9
          BLN(I,J) = 0.D0
          DO 218 K = 1,3
            BLN(I,J) = BLN(I,J) + BLA(I,K)*AN(K,J)
 218     CONTINUE
          LAMBDA(I) = LAMBDA(I) + BLN(I,J)*DEPF(J)
 216    CONTINUE
 214  CONTINUE
C
      END
