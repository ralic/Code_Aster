      SUBROUTINE GDSMHY(JE,E)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/02/2007   AUTEUR MICHEL S.MICHEL 
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
      IMPLICIT NONE
      REAL*8   JE,E(6)

C ----------------------------------------------------------------------
C            GRANDES DEFORMATIONS SIMO-MIEHE OU CANO-LORENTZ
C         CORRECTION HYDROSTATIQUE DE LA DEFORMATION ELASTIQUE
C ----------------------------------------------------------------------
C IN  JE      DETERMINANT DE E CIBLE
C VAR E       DEFORMATION ELASTIQUE (XX,YY,ZZ,RAC2*XY,RAC2*XZ,RAC2*YZ)
C ----------------------------------------------------------------------
      INTEGER NRAC,I,IOPT
      REAL*8  DVE(6),EH,EQE2,DETDVE,P0,P1,P2,RAC(3),DISMIN
      REAL*8  DDOT,R8MAEM
C ----------------------------------------------------------------------
      
      
           
      CALL LCDEVI(E,DVE)
      EQE2 = 1.5D0 * DDOT(6,DVE,1,DVE,1)
      CALL LCDETE(DVE,DETDVE)
      EH = (E(1)+E(2)+E(3))/3.D0

      P0 = 8*DETDVE + JE**2
      P1 = - 4.D0/3.D0*EQE2
      P2 = 0
      CALL ZEROP3(P2,P1,P0,RAC,NRAC)
      DO 10 I = 1,NRAC
        RAC(I) = (RAC(I)+1)/2
 10   CONTINUE
      
      DISMIN = R8MAEM()
      DO 20 I = 1,NRAC
        IF (ABS(RAC(I)-EH).LT.DISMIN) THEN
          IOPT   = I
          DISMIN = ABS(RAC(I)-EH)
        END IF
 20   CONTINUE
      EH = RAC(IOPT)
      
      DO 30 I = 1,3
        E(I) = EH + DVE(I)
 30   CONTINUE
 
      END 
