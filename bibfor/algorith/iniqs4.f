      SUBROUTINE INIQS4 ( NNO, SDFDE, SDFDK, POIPG, COOPG )
      IMPLICIT NONE
      INTEGER             NNO
      REAL*8              SDFDE(4,4),SDFDK(4,4),COOPG(8),POIPG(4)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =============================================================
C   BUT : RECUPERER TOUS LES INDICES DU VECTEUR ZR DANS LEQUEL
C         SE TROUVE LES COORD., LES DFDE et DFDK, LE POIDS DE
C         LA DEUXIEMME FAMILLE DE PT DE GAUSS DE QUAS4.
C
C        IN   :  NNO  NOMBRE DE NOEUDS
C        OUT  :  DFDE   DERIVEE DES FF DANS REP DE REF
C        OUT  :  DFDK   DERIVEE DES FF DANS REP DE REF
C        OUT  :  POIDS  POIDS DES PTS DE GAUSS
C        OUT  :  COOPG  COORD.DES PTS DE GAUSS
C
C =============================================================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      INTEGER     I,J,K,NDIM,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO,NBPG
      CHARACTER*8 ELREFE, FAMIL
C     ------------------------------------------------------------------
C
      CALL JEMARQ()

      ELREFE = 'QU4     '
      FAMIL  = 'FPG4    '

      CALL ELRAGA ( ELREFE, FAMIL, NDIM, NBPG, COOPG, POIPG )

      CALL ELREF4 ( ELREFE, 'MASS', NDIM, NNO, NNOS, NPG, IPOIDS,
     &                                              IVF, IDFDE, JGANO )

      DO 40 I = 1,NPG
        K = 2*NNO*(I-1)
        DO 30 J = 1,NNO
          SDFDE(I,J) = ZR(IDFDE+K+2*(J-1)-1+1)
          SDFDK(I,J) = ZR(IDFDE+K+2*(J-1)-1+2)
   30   CONTINUE
   40 CONTINUE

      CALL JEDEMA()
      END
