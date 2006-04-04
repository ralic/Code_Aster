      SUBROUTINE TE0379 ( OPTION , NOMTE )
      IMPLICIT NONE
      CHARACTER*16        OPTION  , NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2006   AUTEUR CIBHHLV L.VIVAN 
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
C ......................................................................
C    - FONCTION REALISEE: EXTENSION DU CHAM_ELEM ERREUR DES POINTS
C                         DE GAUSS AUX NOEUDS
C                         OPTION : 'ERRE_ELNO_ELEM'
C             (POUR PERMETTRE D'UTILISER POST_RELEVE_T)
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE
C ......................................................................
C
C ---------- DEBUT DECLARATIONS NORMALISEES JEVEUX --------------------
C
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16               ZK16
      CHARACTER*24                         ZK24
      CHARACTER*32                                   ZK32
      CHARACTER*80                                             ZK80
      COMMON / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --------- FIN DECLARATIONS NORMALISEES JEVEUX ----------------------
C
      INTEGER    NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO
      INTEGER    I, IERRG, IERRN
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PERREUR','L',IERRG)
      CALL JEVECH('PERRENO','E',IERRN)
C
      DO 10 I = 1 , NNO
        ZR(IERRN+9*I-9) = ZR(IERRG  )
        ZR(IERRN+9*I-8) = ZR(IERRG+1)
        ZR(IERRN+9*I-7) = ZR(IERRG+2)
        ZR(IERRN+9*I-6) = ZR(IERRG+3)
        ZR(IERRN+9*I-5) = ZR(IERRG+4)
        ZR(IERRN+9*I-4) = ZR(IERRG+5)
        ZR(IERRN+9*I-3) = ZR(IERRG+6)
        ZR(IERRN+9*I-2) = ZR(IERRG+7)
        ZR(IERRN+9*I-1) = ZR(IERRG+8)
   10 CONTINUE
C
      END
