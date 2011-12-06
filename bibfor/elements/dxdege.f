      SUBROUTINE DXDEGE ( NOMTE, XYZL, DEPL, PGL, EFFPG )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 05/09/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT  NONE
      REAL*8          XYZL(3,4), DEPL(*), PGL(3,3), EFFPG(32)
      CHARACTER*16    NOMTE
C     ------------------------------------------------------------------
C     IN  XYZL : COORDONNEES DES NOEUDS
C     IN  UL   : DEPLACEMENT
C     IN  PGL  : MATRICE DE PASSAGE GLOBAL - LOCAL ELEMENT
C     ------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C     NNO:    NOMBRE DE NOEUDS DE L'ELEMENT
CCC      PARAMETER (NNO=3)  POUR LES DKT
CCC      PARAMETER (NNO=4)  POUR LES DKQ
      INTEGER    NNOMX
      PARAMETER (NNOMX=4)
      INTEGER    NBDEPG
      PARAMETER (NBDEPG=6*NNOMX)
      INTEGER    NBEPSG
      PARAMETER (NBEPSG=8)
C
C ----------------------------------------------------------------------
      INTEGER NDIM,NNOEL,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
      INTEGER INO, IPG, IRET
      REAL*8 ZERO
      REAL*8 DEPG(NBDEPG)
      REAL*8 CARA(25),BMAT(NBEPSG,NBDEPG),JACGAU
      REAL*8 EPSG(NBEPSG)
      LOGICAL DKT, DKQ
      CHARACTER*4 FAMI
      CHARACTER*8 TYPMA
C     ------------------------------------------------------------------
C
      ZERO = 0.D0
C
      FAMI = 'RIGI'
      CALL ELREF5(' ',FAMI,NDIM,NNOEL,NNOS,NPG,IPOIDS,ICOOPG,
     &                                         IVF,IDFDX,IDFD2,JGANO)
C
      DKT    = .FALSE.
      DKQ    = .FALSE.

      CALL TEATTR(' ','S','ALIAS8',TYPMA,IRET)

      IF (TYPMA(6:8).EQ.'TR3') THEN
        DKT = .TRUE.
      ELSEIF (TYPMA(6:8).EQ.'QU4') THEN
        DKQ = .TRUE.
      ELSE
        CALL ASSERT(.FALSE.)
      END IF
C
C     -- INITIALISATIONS
C
      CALL MATINI(NBEPSG,NBDEPG,ZERO,BMAT)
      CALL VECINI(NBDEPG,ZERO,DEPG)

C     -- GRANDEURS GEOMETRIQUES :
C     ---------------------------
      IF ( DKT ) THEN
         CALL GTRIA3(XYZL,CARA)
      ELSEIF ( DKQ ) THEN
         CALL GQUAD4(XYZL,CARA)
      END IF
C
C     -- PARTITION DU DEPLACEMENT EN MEMBRANE/FLEXION :
C     -------------------------------------------------
      DO 20 INO = 1,NNOEL
        DEPG(6*(INO-1)+1) =  DEPL(6*(INO-1)+1)
        DEPG(6*(INO-1)+2) =  DEPL(6*(INO-1)+2)
        DEPG(6*(INO-1)+3) =  DEPL(6*(INO-1)+3)
        DEPG(6*(INO-1)+4) =  DEPL(6*(INO-1)+5)
        DEPG(6*(INO-1)+5) = -DEPL(6*(INO-1)+4)
        DEPG(6*(INO-1)+6) =  0.D0
 20   CONTINUE

C --- BOUCLE SUR LES POINTS DE GAUSS DE LA SURFACE:
C     ---------------------------------------------
      DO 100 IPG = 1 , NPG
C
C ----- CALCUL DE LA MATRICE DES DEFORMATIONS GENERALISEES
C
        CALL DXBMAT(NOMTE, CARA, XYZL, PGL, IPG, JACGAU, BMAT)
        CALL PMRVEC('ZERO',NBEPSG,NBDEPG,BMAT,DEPG,EPSG)
C
        EFFPG((IPG-1)*NBEPSG+1) =  EPSG(1)
        EFFPG((IPG-1)*NBEPSG+2) =  EPSG(2)
        EFFPG((IPG-1)*NBEPSG+3) =  EPSG(3)
        EFFPG((IPG-1)*NBEPSG+4) =  EPSG(4)
        EFFPG((IPG-1)*NBEPSG+5) =  EPSG(5)
        EFFPG((IPG-1)*NBEPSG+6) =  EPSG(6)
        EFFPG((IPG-1)*NBEPSG+7) =  EPSG(7)
        EFFPG((IPG-1)*NBEPSG+8) =  EPSG(8)

 100  CONTINUE

      END
