      SUBROUTINE CDATRC( VR, XSI, COEFCD, CD )
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 19/07/2000   AUTEUR CIBHHAB N.RAHNI 
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
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      REAL*8     VR, XSI, COEFCD(1,11), CD
C
      INTEGER    IOR
      REAL*8     DCLDY, CD0, A0(2), DR(2)
      REAL*8     PI, HR, GR, HI, GI, XCOR, OMR
      COMPLEX*16 PR
C
C-----------------------------------------------------------------------
      IOR = 2
      PI = R8PI()
      CD0 = COEFCD(1,1)
      DCLDY = COEFCD(1,2)
      A0(1) = COEFCD(1,3)
      DR(1) = COEFCD(1,4)
      A0(2) = COEFCD(1,5)
      DR(2) = COEFCD(1,6)
C
      XCOR = DBLE(SQRT(1.0D0-XSI*XSI))
      OMR = 2.0D0*PI/VR
      PR = DCMPLX(-XSI,XCOR)*OMR
      CALL ROUTHC ( HR, HI, PR, A0, DR, IOR )
      CD = (DCLDY*HI/(OMR*XCOR)-CD0)
C
      END
