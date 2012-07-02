      SUBROUTINE SHAKSG(K,R)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C--------------------------------------------------------
C ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
C-------------------------------------------------------
C TOURNE KSTAB VERS LE REPERE GLOBAL
      IMPLICIT NONE
      REAL*8 K(24,24),R(3,3),KRT(24,24),TMP(24,24),KR(24,24)
      INTEGER I,J,KK
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CALL ZDANUL(KRT,576)
C      CALL ZDANUL(KR,576)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL R8INIR(576,0.D0,KRT,1)
      CALL R8INIR(576,0.D0,KR,1)
      DO 30 KK = 1,8
        DO 20 J = 1,3
          DO 10 I = 1,3
CCCCCCC ATTENTION< INVERSE LE 3 NOVEMBRE 2001
            KR((KK-1)*3+I, (KK-1)*3+J) = R(I,J)
            KRT((KK-1)*3+I, (KK-1)*3+J) = R(J,I)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
      CALL MULMAT(24,24,24,K,KR,TMP)
      CALL MULMAT(24,24,24,KRT,TMP,KR)
C      CALL SHIFTD(KR,K,576)
      CALL DCOPY(576,KR,1,K,1)

      END
