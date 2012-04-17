      SUBROUTINE CAATDB(NNO,A,D,B,JAC,MATUU)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/01/2007   AUTEUR DESROCHES X.DESROCHES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C     CALCUL DE TADB POUR LE HEXA8 STABILISE
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER KK,KKD,N,I,M,J,K,KL,NNO,J1
      REAL*8 MATUU(1)
      REAL*8 D(6,6),JAC,TMP,SIG(6)
      REAL*8 A(6,3,8)
      REAL*8 B(6,3,8)

        DO 1 N = 1,NNO
          DO 2 I = 1,3
            DO 3 KL = 1,6
              TMP = 0.D0
              DO 4 K = 1,6
                TMP = TMP + A(K,I,N)*D(K,KL)
4             CONTINUE
              SIG(KL) = TMP
3          CONTINUE

            KKD = (3* (N-1)+I-1)* (3* (N-1)+I)/2
            DO 5 J = 1,3
              DO 6 M = 1,N
                IF (M.EQ.N) THEN
                  J1 = I
                ELSE
                  J1 = 3
                END IF

                TMP = 0.D0
                DO 7 K = 1,6
                  TMP = TMP + SIG(K)*B(K,J,M)
7               CONTINUE

C   STOCKAGE EN TENANT COMPTE DE LA SYMETRIE

                IF (J.LE.J1) THEN
                  KK = KKD + 3* (M-1) + J
                  MATUU(KK) = MATUU(KK) + TMP*JAC
                END IF

6            CONTINUE
5         CONTINUE
2       CONTINUE
1     CONTINUE

      END
