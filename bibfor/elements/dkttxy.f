      SUBROUTINE DKTTXY ( HFT2 , DEPF , R , VT )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/03/98   AUTEUR CIBHHLV L.VIVAN 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8       HFT2(2,6)
      REAL*8       DEPF(9)
      REAL*8       R(*)
      REAL*8       VT(2)
C     ------------------------------------------------------------------
C     EFFORT TRANCHANT L'ELEMENT DE PLAQUE DKT
C     ------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      REAL*8   TKT(6,9)
      REAL*8   CL(3) , SL(3) , CS(3) , CU(3) , SU(3)
      REAL*8   BC(2,9)
C     ------------------ PARAMETRAGE TRIANGLE --------------------------
      INTEGER NPG , NC , NNO
      INTEGER LJACO,LTOR,LQSI,LETA,LWGT,LXYC,LCOTE,LCOS,LSIN
               PARAMETER (NPG   = 3)
               PARAMETER (NNO   = 3)
               PARAMETER (NC    = 3)
               PARAMETER (LJACO = 2)
               PARAMETER (LTOR  = LJACO + 4)
               PARAMETER (LQSI  = LTOR  + 1)
               PARAMETER (LETA  = LQSI  + NPG + NNO )
               PARAMETER (LWGT  = LETA  + NPG + NNO )
               PARAMETER (LXYC  = LWGT  + NPG)
               PARAMETER (LCOTE = LXYC  + 2*NC)
               PARAMETER (LCOS  = LCOTE + NC)
               PARAMETER (LSIN  = LCOS  + NC)
C     ------------------------------------------------------------------
      DO 100 I = 1, NC
         CL(I) = 6.D0 * R(LCOS-1+I) / R(LCOTE-1+I)
         SL(I) = 6.D0 * R(LSIN-1+I) / R(LCOTE-1+I)
         CS(I) = 3.D0 * R(LCOS-1+I) * R(LSIN-1+I)
         CU(I) = 3.D0 * R(LCOS-1+I) * R(LCOS-1+I)
         SU(I) = 3.D0 * R(LSIN-1+I) * R(LSIN-1+I)
  100 CONTINUE
      TKT(1,1) = - CL(1) - CL(1)
      TKT(1,2) =   CU(1) + CU(1)
      TKT(1,3) =   CS(1) + CS(1)
      TKT(1,4) =   CL(1) + CL(1)
      TKT(1,5) =   CU(1) + CU(1)
      TKT(1,6) =   CS(1) + CS(1)
      TKT(1,7) =   0.D0
      TKT(1,8) =   0.D0
      TKT(1,9) =   0.D0
      TKT(2,1) =   CL(3) + CL(3)
      TKT(2,2) =   CU(3) + CU(3)
      TKT(2,3) =   CS(3) + CS(3)
      TKT(2,4) =   0.D0
      TKT(2,5) =   0.D0
      TKT(2,6) =   0.D0
      TKT(2,7) = - CL(3) - CL(3)
      TKT(2,8) =   CU(3) + CU(3)
      TKT(2,9) =   CS(3) + CS(3)
      TKT(3,1) =   CL(3) - CL(1)
      TKT(3,2) =   CU(3) + CU(1)
      TKT(3,3) =   CS(3) + CS(1)
      TKT(3,4) =   CL(1) + CL(2)
      TKT(3,5) =   CU(1) - CU(2)
      TKT(3,6) =   CS(1) - CS(2)
      TKT(3,7) = - CL(3) - CL(2)
      TKT(3,8) =   CU(3) - CU(2)
      TKT(3,9) =   CS(3) - CS(2)
      TKT(4,1) = - SL(1) - SL(1)
      TKT(4,2) =   CS(1) + CS(1)
      TKT(4,3) =   SU(1) + SU(1)
      TKT(4,4) =   SL(1) + SL(1)
      TKT(4,5) =   CS(1) + CS(1)
      TKT(4,6) =   SU(1) + SU(1)
      TKT(4,7) =   0.D0
      TKT(4,8) =   0.D0
      TKT(4,9) =   0.D0
      TKT(5,1) =   SL(3) + SL(3)
      TKT(5,2) =   CS(3) + CS(3)
      TKT(5,3) =   SU(3) + SU(3)
      TKT(5,4) =   0.D0
      TKT(5,5) =   0.D0
      TKT(5,6) =   0.D0
      TKT(5,7) = - SL(3) - SL(3)
      TKT(5,8) =   CS(3) + CS(3)
      TKT(5,9) =   SU(3) + SU(3)
      TKT(6,1) =   SL(3) - SL(1)
      TKT(6,2) =   CS(3) + CS(1)
      TKT(6,3) =   SU(3) + SU(1)
      TKT(6,4) =   SL(1) + SL(2)
      TKT(6,5) =   CS(1) - CS(2)
      TKT(6,6) =   SU(1) - SU(2)
      TKT(6,7) = - SL(3) - SL(2)
      TKT(6,8) =   CS(3) - CS(2)
      TKT(6,9) =   SU(3) - SU(2)
C
C     ------ VT = HFT2.TKT.DEPF ---------------------------------------
      DO 110 K = 1, 18
         BC(K,1) = 0.D0
  110 CONTINUE
      DO 140 I = 1, 2
         DO 130 J = 1, 3*NNO
            DO 120 K = 1, 6
               BC(I,J) = BC(I,J) + HFT2(I,K) * TKT(K,J)
  120       CONTINUE
  130    CONTINUE
  140 CONTINUE
      VT(1) = 0.D0
      VT(2) = 0.D0
      DO 160 I = 1, 2
         DO 150 J = 1, 3*NNO
            VT(I) = VT(I) + BC(I,J) * DEPF(J)
  150    CONTINUE
  160 CONTINUE
C
      END
