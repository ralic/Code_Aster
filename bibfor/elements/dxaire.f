      SUBROUTINE DXAIRE (XYZG, AIRE )
      IMPLICIT NONE
      REAL*8              XYZG(3,*), AIRE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C     CALCULE L'AIRE
C
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
C
      CHARACTER*8         ELREFE
      INTEGER       JIN, NDIM, NNO, NBFPG, NDI, NPG1
      INTEGER       JVAL, IPOIDS, IVF, IDFDX, IDFDY
      INTEGER       I, J, IPG, KDEC, LDEC, IDEC, JDEC
      REAL*8        NX, NY, NZ, SX(9,9), SY(9,9), SZ(9,9), JAC
      CHARACTER*24  CHVAL, CHCTE
C
      CALL ELREF1(ELREFE)
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM  = ZI(JIN+1-1)
      NNO   = ZI(JIN+2-1)
      NPG1  = ZI(JIN+3-1+1)
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IVF    = IPOIDS + NPG1
      IDFDX  = IVF    + NPG1 * NNO
      IDFDY  = IDFDX  + 1
C
C     --- CALCUL DE L'AIRE DE LA MAILLE ---
C
      DO 10 I = 1,NNO
         DO 20 J = 1,NNO
            SX(I,J) = XYZG(2,I)*XYZG(3,J) - XYZG(3,I)*XYZG(2,J)
            SY(I,J) = XYZG(3,I)*XYZG(1,J) - XYZG(1,I)*XYZG(3,J)
            SZ(I,J) = XYZG(1,I)*XYZG(2,J) - XYZG(2,I)*XYZG(1,J)
 20      CONTINUE
 10   CONTINUE
      AIRE = 0.D0
      DO 101 IPG = 1,NPG1
         KDEC = (IPG-1)*NNO*NDIM
         NX = 0.0D0
         NY = 0.0D0
         NZ = 0.0D0
         DO 102 I = 1,NNO
           IDEC = (I-1)*NDIM
           DO 102 J=1,NNO
              JDEC = (J-1)*NDIM
          NX = NX + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SX(I,J)
          NY = NY + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SY(I,J)
          NZ = NZ + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SZ(I,J)
 102    CONTINUE
        JAC = SQRT(NX*NX + NY*NY + NZ*NZ) * ZR(IPOIDS+IPG-1)
        AIRE = AIRE + JAC
 101  CONTINUE
C
      END
