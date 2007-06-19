      SUBROUTINE POUCRI(MAT,SIG,VARI,RC,RP,SEUIL)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/02/2000   AUTEUR VABHHTS J.PELLET 
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
C       ----------------------------------------------------------------
C       VMIS_POU_  :   CONVEXE ELASTO PLASTIQUE POUR (MATER,SIG,X1,X2,P)
C       ----------------------------------------------------------------
C       IN  SIG    :  CONTRAINTE
C       IN  VARI   :  VARIABLES INTERNES = ( X1 X2 P )
C       IN  MAT    :  COEFFICIENTS MATERIAU A TEMP
C       OUT RC     :  NP*SQRT(...)
C       OUT RP     :  AA*R(P)
C       OUT SEUIL  :  SEUIL  ELASTICITE
C       ----------------------------------------------------------------
      INTEGER NUMLOI
      REAL*8 PCUM,SIG(4),MAT(*),SEUIL,AY,AZ,AN,AX
      REAL*8 NP,MPY,MEY,MPZ,MEZ,MPX
      REAL*8 SU,SY,EP,AF,VARI(*),RP,ALPHAY,BETAY,ALPHAZ,BETAZ
      REAL*8 RC2,RC,NB,POUAQP
      REAL*8 AA,XNX,XMY,XMZ,XMX,QPY,QPZ,EPU
C       ----------------------------------------------------------------
      NUMLOI = NINT(MAT(1))
      MPY = MAT(2)
      MEY = MAT(3)
      ALPHAY = MAT(4)
      BETAY = MAT(5)
      MPZ = MAT(6)
      MEZ = MAT(7)
      MPX = MAT(8)
      ALPHAZ = MAT(9)
      BETAZ = MAT(10)
      SU = MAT(11)
      SY = MAT(12)
      EP = MAT(14)
      AF = MAT(15)
      AA = MAT(16)
      NP = MAT(21)

      QPY = VARI(6)
      QPZ = VARI(7)
      PCUM = VARI(5)

      XNX = SIG(1)
      XMY = SIG(2)
      XMZ = SIG(3)
      XMX = SIG(4)


      AY = POUAQP(QPY,ALPHAY,BETAY,MEY,MPY)
      AZ = POUAQP(QPZ,ALPHAZ,BETAZ,MEZ,MPZ)

      AN = 1.D0/NP/NP
      AX = 1.D0/MPX/MPX
      RC2 = AY*XMY*XMY + AZ*XMZ*XMZ + AN*XNX*XNX + AX*XMX*XMX
      RC = NP*SQRT(RC2)

      IF (NUMLOI.EQ.1) THEN
        NB = SY + EP*PCUM
      ELSE IF (NUMLOI.EQ.2) THEN
        EPU = (SU-SY)/EP
        NB = SY + EP*PCUM/ (1.D0+ (PCUM/EPU)**AF)** (1.D0/AF)
      END IF

      RP = NB*AA

      SEUIL = RC - RP

      END
