      SUBROUTINE ARLTDS(NDIM  ,NNS   ,NPGS  ,
     &                  IPOIDS,ICOORS,IVFS  ,IDFDES,
     &                  POIJCS,FS    ,DFDXS ,DFDYS ,DFDZS )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/06/2009   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT     NONE
      INTEGER      NDIM,NNS,NPGS
      INTEGER      IVFS,IPOIDS,IDFDES,ICOORS
      REAL*8       POIJCS(NPGS)
      REAL*8       FS(NPGS*NNS)
      REAL*8       DFDXS(NPGS*NNS),DFDYS(NPGS*NNS),DFDZS(NPGS*NNS)
C
C ----------------------------------------------------------------------
C
C CALCUL DES MATRICES DE COUPLAGE ARLEQUIN
C OPTION ARLQ_MATR
C
C CALCUL DES DERIVEES DES FF DE LA MAILLE SUPPORT S
C
C ----------------------------------------------------------------------
C
C
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNS    : NOMBRE DE NOEUDS DE LA MAILLE SUPPORT S
C IN  NPGS   : NOMBRE DE POINTS DE GAUSS DE LA MAILLE SUPPORT S
C IN  IPOIDS : POINTEUR VERS POIDS DE GAUSS DE LA MAILLE SUPPORT S
C IN  ICOORS : POINTEUR VERS COORD. NOEUDS DE LA MAILLE SUPPORT S
C IN  IVFS   : POINTEUR VERS FONCTIONS DE FORME DE LA MAILLE SUPPORT S
C IN  IDFDES : POINTEUR VERS DER. FONCTIONS DE FORME DE LA MAILLE S
C OUT POIJCS : POIDS DE GAUSS*JACOBIEN
C OUT FS     : FONCTIONS DE FORME
C OUT DFDXS  : DER/X FONCTIONS DE FORME
C OUT DFDYS  : DER/Y FONCTIONS DE FORME
C OUT DFDZS  : DER/Z FONCTIONS DE FORME
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
C
C ----------------------------------------------------------------------
C
      INTEGER  L,KPGS
C
C ----------------------------------------------------------------------
C
C
C --- CALCUL DES DERIVEES DE FCT FORMES+JACOBIEN TRANSFO MAILLE SUPPORT
C
      DO 10 KPGS = 1,NPGS
        L  = NNS*(KPGS-1)+1
        CALL DCOPY(NNS,ZR(IVFS-1+L),1,FS(L)   ,1)
        IF (NDIM.EQ.2) THEN
          CALL DFDM2D(NNS       ,KPGS  ,IPOIDS,IDFDES,
     &                ZR(ICOORS),
     &                DFDXS(L),
     &                DFDYS(L),
     &                POIJCS(KPGS))
        ELSEIF (NDIM.EQ.3) THEN
          CALL DFDM3D(NNS       ,KPGS  ,IPOIDS,IDFDES,
     &                ZR(ICOORS),
     &                DFDXS(L)  ,
     &                DFDYS(L)   ,
     &                DFDZS(L)  ,
     &                POIJCS(KPGS))
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF

  10  CONTINUE
      END
