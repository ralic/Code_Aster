      SUBROUTINE COTFAC(XYZ,N1,N2,N3,N4,XPT,IRET)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 27/06/2001   AUTEUR DURAND C.DURAND 
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
C-----------------------------------------------------------------------
C  DESCRIPTION : 
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER       N1, N2, N3, N4, IRET
      REAL*8        XYZ(3,*), XPT(*)
C
C VARIABLES LOCALES
C -----------------
      REAL*8        V12(3),V23(3),V24(3),V25(3),VNO(3)
      REAL*8        RA,RB,RR
      REAL*8        EPS
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      EPS=1.D-5
      V12(1)=XYZ(1,N2)-XYZ(1,N1)
      V12(2)=XYZ(2,N2)-XYZ(2,N1)
      V12(3)=XYZ(3,N2)-XYZ(3,N1)
      V23(1)=XYZ(1,N3)-XYZ(1,N2)
      V23(2)=XYZ(2,N3)-XYZ(2,N2)
      V23(3)=XYZ(3,N3)-XYZ(3,N2)
      VNO(1) = V23(2) * V12(3) - V23(3) * V12(2)
      VNO(2) = V23(3) * V12(1) - V23(1) * V12(3)
      VNO(3) = V23(1) * V12(2) - V23(2) * V12(1)

      V24(1)=XYZ(1,N4)-XYZ(1,N2)
      V24(2)=XYZ(2,N4)-XYZ(2,N2)
      V24(3)=XYZ(3,N4)-XYZ(3,N2)

      V25(1)=XPT(1)-XYZ(1,N2)
      V25(2)=XPT(2)-XYZ(2,N2)
      V25(3)=XPT(3)-XYZ(3,N2)

      RA=VNO(1)*V24(1)+VNO(2)*V24(2)+VNO(3)*V24(3)
      RB=VNO(1)*V25(1)+VNO(2)*V25(2)+VNO(3)*V25(3)
      RR=RB/RA

      IF     (RR.GT.EPS) THEN
         IRET=1
      ELSEIF (RR.GT.-1.D0*EPS) THEN
         IRET=0
      ELSE
         IRET=-1
      ENDIF
C
      END
