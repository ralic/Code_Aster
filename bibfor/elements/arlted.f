      SUBROUTINE ARLTED(LINCLU,NDIM  ,
     &                  NNS   ,ICOORS,
     &                  NPGS  ,IVFS  ,IDFDES,IPOIDS,
     &                  ELREF1,NDML1   ,JCOOR1,
     &                  ELREF2,NDML2   ,JCOOR2,
     &                  FCPIG1    ,FCPIG2    ,POIJCS,
     &                  DFDX1 ,DFDY1 ,DFDZ1 ,
     &                  DFDX2 ,DFDY2 ,DFDZ2 )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/10/2009   AUTEUR CAO B.CAO 
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
C TOLE CRP_21
C RESPONSABLE MEUNIER S.MEUNIER
C
      IMPLICIT     NONE
      CHARACTER*6  LINCLU
      INTEGER      NDIM
      INTEGER      NNS,NPGS
      INTEGER      ICOORS,IVFS,IPOIDS,IDFDES
      CHARACTER*8  ELREF1,ELREF2
      INTEGER      NDML1,JCOOR1
      INTEGER      NDML2,JCOOR2
      REAL*8       POIJCS(NPGS)
      REAL*8       FCPIG1(NPGS*NDML1),FCPIG2(NPGS*NDML2)
      REAL*8       DFDX1(NPGS*NDML1),DFDY1(NPGS*NDML1),DFDZ1(NPGS*NDML1)
      REAL*8       DFDX2(NPGS*NDML2),DFDY2(NPGS*NDML2),DFDZ2(NPGS*NDML2)
C
C ----------------------------------------------------------------------
C
C CALCUL DES MATRICES DE COUPLAGE ARLEQUIN
C OPTION ARLQ_MATR
C
C CALCUL DES FF ET DES DERIVEES DES FF DES MAILLES COUPLEES
C
C ----------------------------------------------------------------------
C
C
C IN  LINCLU : TYPE D'INCLUSION
C                 INCLU1: MAILLE 1 EST DANS MAILLE 2
C                 INCLU2: MAILLE 2 EST DANS MAILLE 1
C                 SSMAIL: MAILLE 1 INTERSECTE MAILLE 2
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNS    : NOMBRE DE NOEUDS DE LA MAILLE SUPPORT S
C IN  ICOORS : POINTEUR VERS COORD. NOEUDS DE LA MAILLE SUPPORT S
C IN  NPGS   : NOMBRE DE POINTS DE GAUSS DE LA MAILLE SUPPORT S
C IN  IVFS   : POINTEUR VERS FONCTIONS DE FORME DE LA MAILLE SUPPORT S
C IN  IDFDES : POINTEUR VERS DER. FONCTIONS DE FORME DE LA MAILLE S
C IN  IPOIDS : POINTEUR VERS POIDS DE GAUSS DE LA MAILLE SUPPORT S
C IN  ELREF1 : ELREFE DE LA MAILLE 1
C IN  NDML1  : NOMBRE DE NOEUDS DE LA MAILLE 1
C IN  JCOOR1 : POINTEUR VERS COORD. NOEUDS DE LA MAILLE 1
C IN  ELREF2 : ELREFE DE LA MAILLE 2
C IN  NDML2  : NOMBRE DE NOEUDS DE LA MAILLE 2
C IN  JCOOR2 : POINTEUR VERS COORD. NOEUDS DE LA MAILLE 2
C OUT FCPIG1 : FCT. FORME DE MAILLE 1 AU POINT DE GAUSS KPGS
C               DE LA MAILLE S
C OUT FCPIG2 : FCT. FORME DE MAILLE 2 AU POINT DE GAUSS KPGS
C               DE LA MAILLE S
C OUT POIJCS : POIDS DE GAUSS*VOLUME MAILLE SUPPORT
C OUT DFDXm  : DERIVEE FCT. FORME/X EN CHAQUE PT DE GAUSS DE LA MAILLE m
C OUT DFDYm  : DERIVEE FCT. FORME/Y EN CHAQUE PT DE GAUSS DE LA MAILLE m
C OUT DFDZm  : DERIVEE FCT. FORME/Z EN CHAQUE PT DE GAUSS DE LA MAILLE m
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
      INTEGER  KPGS
      INTEGER  MTL1, MTL2
      REAL*8   FCTFS(NPGS*NNS)
      REAL*8   DFDZS(NPGS*NNS),DFDXS(NPGS*NNS),DFDYS(NPGS*NNS)
C
C ----------------------------------------------------------------------
C
C --- CALCUL DES DERIVEES DE FCT FORMES DES MAILLES COURANTES
C
      IF (LINCLU.EQ.'INCLU1') THEN
C
         CALL ARLTDS(NDIM  ,NNS   ,NPGS  ,
     &               IPOIDS,ICOORS,IVFS  ,IDFDES,
     &               POIJCS,FCPIG1    ,DFDX1 ,DFDY1 ,DFDZ1 )
C
         DO 10 KPGS = 1,NPGS
C
           MTL2 = NDML2*(KPGS-1)+1
C
           CALL ARLTEP(NDIM  ,NPGS      ,KPGS      ,
     &                 NNS   ,ZR(ICOORS),ZR(IVFS)  ,
     &                 ELREF2,NDML2       ,ZR(JCOOR2),
     &                 FCPIG2(MTL2),DFDX2(MTL2) ,DFDY2(MTL2) ,
     &                 DFDZ2(MTL2))

 10      CONTINUE
C
      ELSEIF (LINCLU.EQ.'INCLU2') THEN
C
         CALL ARLTDS(NDIM  ,NNS   ,NPGS  ,
     &               IPOIDS,ICOORS,IVFS  ,IDFDES,
     &               POIJCS,FCPIG2    ,DFDX2 ,DFDY2 ,DFDZ2 )
C
         DO 11 KPGS = 1,NPGS
C
           MTL1 = NDML1*(KPGS-1)+1
C
           CALL ARLTEP(NDIM  ,NPGS      ,KPGS      ,
     &                 NNS   ,ZR(ICOORS),ZR(IVFS)  ,
     &                 ELREF1,NDML1       ,ZR(JCOOR1),
     &                 FCPIG1(MTL1),DFDX1(MTL1) ,DFDY1(MTL1) ,
     &                 DFDZ1(MTL1))
C
 11      CONTINUE
C
       ELSEIF (LINCLU.EQ.'SSMAIL') THEN
C
          CALL ARLTDS(NDIM  ,NNS   ,NPGS  ,
     &                IPOIDS,ICOORS,IVFS  ,IDFDES,
     &                POIJCS,FCTFS    ,DFDXS ,DFDYS ,DFDZS )
C
          DO 12 KPGS = 1,NPGS
C
            MTL1 = NDML1*(KPGS-1)+1
            MTL2 = NDML2*(KPGS-1)+1
C
            CALL ARLTEP(NDIM  ,NPGS      ,KPGS      ,
     &                  NNS   ,ZR(ICOORS),ZR(IVFS)  ,
     &                  ELREF1,NDML1       ,ZR(JCOOR1),
     &                  FCPIG1(MTL1),DFDX1(MTL1) ,DFDY1(MTL1) ,
     &                  DFDZ1(MTL1))
C
            CALL ARLTEP(NDIM  ,NPGS      ,KPGS      ,
     &                  NNS   ,ZR(ICOORS),ZR(IVFS)  ,
     &                  ELREF2,NDML2       ,ZR(JCOOR2),
     &                  FCPIG2(MTL2),DFDX2(MTL2) ,DFDY2(MTL2) ,
     &                  DFDZ2(MTL2))
 12       CONTINUE
C
       ELSE
          CALL ASSERT(.FALSE.)
       ENDIF
C
      END
