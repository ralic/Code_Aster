      SUBROUTINE T3GB ( CARAT3, XYZL, BMAT )
      IMPLICIT  NONE
      REAL*8    XYZL(3,1), CARAT3(*), BMAT(6,1)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 10/04/2012   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C     ------------------------------------------------------------------
C --- CALCUL DE LA MATRICE (B) RELIANT LES DEFORMATIONS DU PREMIER
C --- ORDRE AUX DEPLACEMENTS AU POINT D'INTEGRATION D'INDICE IGAU
C --- POUR UN ELEMENT DE TYPE T3G
C --- (I.E. (EPS_1) = (B)*(UN))
C --- D'AUTRE_PART, ON CALCULE LE PRODUIT NOTE JACGAU = JACOBIEN*POIDS
C     ------------------------------------------------------------------
C     IN  XYZL(3,NNO)   : COORDONNEES DES CONNECTIVITES DE L'ELEMENT
C                         DANS LE REPERE LOCAL DE L'ELEMENT
C     IN  IGAU          : INDICE DU POINT D'INTEGRATION
C     OUT BMAT(6,1)     : MATRICE (B) AU POINT D'INTEGRATION COURANT
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      INTEGER  NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
      REAL*8   BM(3,6),  BF(3,9), BC(2,9), QSI, ETA
C ------------------------------------------------------------------
C
      CALL ELREF5(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,
     +                                         IVF,IDFDX,IDFD2,JGANO)
C
C --- CALCUL DE LA MATRICE B_MEMBRANE NOTEE, ICI, (BM)
C     ------------------------------------------------
      CALL DXTBM ( CARAT3(9), BM )
C
C     ------- CALCUL DE LA MATRICE BFB -------------------------------
      CALL DSTBFB ( CARAT3(9) , BF )
C
C        ---- CALCUL DE LA MATRICE BC ----------------------------------
      QSI = 1.D0/3.D0
      ETA = QSI
      CALL T3GBC(XYZL, QSI, ETA, BC)
C
C --- AFFECTATION DE LA MATRICE B COMPLETE, NOTEE (BMAT)
C --- AVEC LES MATRICES (BM), (BF) ET (BC)
C     ------------------------------------
      CALL BCOQAF ( BM, BF, BC, NNO, BMAT )
C
      END
