      SUBROUTINE TE0479 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/12/2010   AUTEUR TARDIEU N.TARDIEU 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
      IMPLICIT NONE
      CHARACTER*16        OPTION , NOMTE
C ----------------------------------------------------------------------
C     CALCUL DES COORDONNEES DES POINTS DE GAUSS
C     POUR LES ELEMENTS ISOPARAMETRIQUES 2D ET LEURS ELEMENTS DE PEAU
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER            NDIM,NNO,NNOS,NPG,JGANO,KP,ICOPG,INO
      INTEGER            IDFDE,IPOIDS,IVF,IGEOM
      REAL*8             XX,YY,RBID81(81),POIDS
      LOGICAL            LTEATT, LAXI
C DEB ------------------------------------------------------------------
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      LAXI = .FALSE.
      IF (LTEATT(' ','AXIS','OUI')) LAXI = .TRUE.
C
C ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES
C      ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
C
      CALL JEVECH('PCOORPG','E',ICOPG )
C
      DO 100 KP=1,NPG
        XX=0.D0
        YY=0.D0
        DO 50 INO=1,NNO
          XX=XX+ZR(IGEOM+2*(INO-1)+0)*ZR(IVF+(KP-1)*NNO+INO-1)
          YY=YY+ZR(IGEOM+2*(INO-1)+1)*ZR(IVF+(KP-1)*NNO+INO-1)
   50   CONTINUE
   
        ZR(ICOPG+3*(KP-1)+0)=XX
        ZR(ICOPG+3*(KP-1)+1)=YY
        
        IF (NDIM.EQ.2) THEN 
C         -- CAS DES ELEMENTS 2D
          CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),RBID81,RBID81,POIDS)
        ELSE IF (NDIM.EQ.1) THEN 
C         -- CAS DES ELEMENTS PEAU
          CALL VFF2DN(NDIM,NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),RBID81,RBID81,
     &                                                            POIDS)
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        
C       EN AXI R C'EST XX
        IF (LAXI) POIDS=POIDS*XX
        
        ZR(ICOPG+3*(KP-1)+2)=POIDS
  100 CONTINUE
C
      END
