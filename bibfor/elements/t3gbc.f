      SUBROUTINE T3GBC  ( XYZL, QSI, ETA, BC )
      IMPLICIT  NONE
      REAL*8     QSI, ETA
      REAL*8     BC(2,9), XYZL(3,*)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 19/03/2012   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C
C     --------------------------------------------------------
C     MATRICE BC(2,9) AU POINT QSI ETA POUR L'ELEMENT T3GAMMA
C     --------------------------------------------------------
C
      REAL*8 BCT1(2,3), BCT2(3,9)
      REAL*8 ZERO, UN, DEUX, DEMI
      REAL*8 X12, Y12, X23, Y23, X31, Y31
      REAL*8 L1,  L2,  L3
      REAL*8 C2,  S2,  C3,  S3
      REAL*8 Q, S2SS3, C3SS3
C
      ZERO  = 0.D0
      UN    = 1.D0
      DEUX  = 2.D0
      DEMI  = UN/DEUX

C   TRIANGLE N1-N2-N3

C   COTE 1 COMPOSE DES NOEUDS N1-N2

      X12 = XYZL(1,1) - XYZL(1,2)
      Y12 = XYZL(2,1) - XYZL(2,2)
      L1  = SQRT(X12*X12+Y12*Y12)

C   COTE 2 COMPOSE DES NOEUDS N2-N3

      X23 = XYZL(1,2) - XYZL(1,3)
      Y23 = XYZL(2,2) - XYZL(2,3)
      L2  = SQRT(X23*X23+Y23*Y23)
      C2  = - X23/L2
      S2  = - Y23/L2

C   COTE 2 COMPOSE DES NOEUDS N3-N1

      X31 = XYZL(1,3) - XYZL(1,1)
      Y31 = XYZL(2,3) - XYZL(2,1)
      L3  = SQRT(X31*X31+Y31*Y31)
      C3  = - X31/L3
      S3  = - Y31/L3

C CALCUL DE LA MATRICE BC

      Q     = UN/(C2-S2*C3/S3)
      S2SS3 = S2/S3
      C3SS3 = C3/S3

      BCT1(1,1) =  UN - ETA
      BCT1(1,2) =  Q*ETA
      BCT1(1,3) = -S2SS3*Q*ETA
      BCT1(2,1) = - C3SS3  - QSI/(Q*S2)  + C3SS3*ETA
      BCT1(2,2) =   QSI/S2  - C3SS3*Q*ETA
      BCT1(2,3) =   UN/S3  - QSI/S3      + C3SS3*S2SS3*Q*ETA

      BCT2(1,1) =  -UN/L1
      BCT2(1,2) =   DEMI
      BCT2(1,3) =   ZERO
      BCT2(1,4) =   UN/L1 
      BCT2(1,5) =   DEMI 
      BCT2(1,6) =   ZERO 
      BCT2(1,7) =   ZERO 
      BCT2(1,8) =   ZERO 
      BCT2(1,9) =   ZERO 

      BCT2(2,1) =   ZERO  
      BCT2(2,2) =   ZERO  
      BCT2(2,3) =   ZERO  
      BCT2(2,4) =  -UN/L2
      BCT2(2,5) =   DEMI*C2 
      BCT2(2,6) =   DEMI*S2 
      BCT2(2,7) =   UN/L2 
      BCT2(2,8) =   DEMI*C2 
      BCT2(2,9) =   DEMI*S2
 
      BCT2(3,1) =   UN/L3 
      BCT2(3,2) =   DEMI*C3 
      BCT2(3,3) =   DEMI*S3 
      BCT2(3,4) =   ZERO  
      BCT2(3,5) =   ZERO  
      BCT2(3,6) =   ZERO  
      BCT2(3,7) =  -UN/L3
      BCT2(3,8) =   DEMI*C3 
      BCT2(3,9) =   DEMI*S3 
C
      CALL MATMUL(BCT1,BCT2,2,3,9,BC)
C
      END
