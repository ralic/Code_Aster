      SUBROUTINE XMMJEC(NDIM  ,JNNM ,JNNE ,NDEPLE,
     &                  NSINGE,NSINGM,FFE   ,FFM   ,NORM  ,
     &                  JGEOM ,JDEPDE,RRE   ,RRM ,
     &                  JDDLE,JDDLM,JEUCA )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/12/2010   AUTEUR MASSIN P.MASSIN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INTEGER NDIM 
      REAL*8  NORM(3)
      REAL*8  FFE(9),FFM(9)
      REAL*8  JEUCA
      INTEGER JGEOM,JDEPDE
      INTEGER JNNM(3),JNNE(3),JDDLE(2),JDDLM(2)
      INTEGER NSINGE,NSINGM
      REAL*8  RRE,RRM
C      
C ----------------------------------------------------------------------
C
C ROUTINE XFEM (METHODE XFEM-GG - TE)
C
C CALCUL DU JEU
C      
C ----------------------------------------------------------------------
C
C 
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NDDL   : NOMBRE TOTAL DE DEGRES DE LIBERTE DE LA MAILLE DE CONTACT
C IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C IN  NNC    : NOMBRE DE NOEUDS DE LA MAILLE DE CONTACT
C IN  NNES   : NOMBRE DE NOEUDS SOMMETS DE LA MAILLE ESCLAVE
C IN  NSINGE : NOMBRE DE FONCTIONS SINGULIERE ESCLAVES
C IN  NSINGM : NOMBRE DE FONCTIONS SINGULIERE MAIT RES
C IN  ddles : NOMBRE DE DDLS D'UN NOEUD SOMMET ESCLAVE
C IN  RRE    : SQRT LSN PT ESCLAVE
C IN  RRM    : SQRT LSN PT MAITRE
C IN  NORM   : VALEUR DE LA NORMALE
C IN  JGEOM  : POINTEUR JEVEUX SUR GEOMETRIE INITIALE
C IN  JDEPDE : POINTEUR JEVEUX POUR DEPDEL
C OUT JEUCA  : VALEUR DU JEU POUR LES SECONDS MEMBRES DE CONTACT
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER IDIM,INOM,ISINGE,INOES,ISINGM,IN,JN,NDDLE
      INTEGER NDEPLE,NNE,NNES,NNEM,NNM,NNMS,DDLES,DDLEM,DDLMS,DDLMM
      REAL*8  POSE(3),POSM(3)
      INTEGER PL
C
C ----------------------------------------------------------------------
C

C
C --- INNITIALISATION
C
      NNE=JNNE(1)
      NNES=JNNE(2)
      NNEM=JNNE(3)
      NNM=JNNM(1)
      NNMS=JNNM(2)
      DDLES=JDDLE(1)
      DDLEM=JDDLE(2)
      DDLMS=JDDLM(1)
      DDLMM=JDDLM(2)
      NDDLE = DDLES*NNES+DDLEM*NNEM
C
      JEUCA  = 0.D0
      CALL VECINI(3,0.D0,POSE)
      CALL VECINI(3,0.D0,POSM) 
C
C --- CALCUL DE LA POSITION COURANTE DU POINT ESCLAVE
C
      DO 10 IDIM = 1,NDIM
        DO 20 INOES = 1,NDEPLE
          IF (NNM.NE.0) THEN
            CALL INDENT(INOES,DDLES,DDLEM,NNES,IN)
            PL           = IN + IDIM
            POSE(IDIM) = POSE(IDIM) +
     &                   FFE(INOES)*(
     &                      ZR(JGEOM+NDIM*(INOES-1)+IDIM-1) +
     &                      ZR(JDEPDE-1+PL) -
     &                      ZR(JDEPDE-1+PL+NDIM)
     &                   )
          ENDIF
          DO 25 ISINGE = 1,NSINGE
            CALL INDENT(INOES+1,DDLES,DDLEM,NNES,IN)
            PL           = IN - 2*NDIM + IDIM
            POSE(IDIM) = POSE(IDIM) - RRE*FFE(INOES)*ZR(JDEPDE-1+PL)
 25       CONTINUE
 20     CONTINUE
 10   CONTINUE
C           
C --- CALCUL DE LA POSITION COURANTE DU POINT MAITRE
C
      DO 11 IDIM = 1,NDIM
        DO 30 INOM = 1,NNM
            CALL INDENT(INOM,DDLMS,DDLMM,NNMS,JN)
            PL = NDDLE + JN + IDIM

          POSM(IDIM) = POSM(IDIM)+
     &                 FFM(INOM)*(
     &                   ZR(JGEOM-1+NNE*NDIM+(INOM-1)*NDIM+IDIM) +
     &                   ZR(JDEPDE-1+PL) +
     &                   ZR(JDEPDE-1+PL+NDIM)
     &                 )
          DO 40 ISINGM = 1,NSINGM
            PL         = PL + 2*NDIM
            POSM(IDIM) = POSM(IDIM) + RRM*FFM(INOM)*ZR(JDEPDE-1+PL)
 40       CONTINUE
 30     CONTINUE
 11   CONTINUE 
C
C --- CALCUL DU JEU
C
      DO 70 IDIM = 1,NDIM
        JEUCA  = JEUCA  + NORM(IDIM)*(POSE(IDIM)-POSM(IDIM))
  70  CONTINUE
C     
      END
