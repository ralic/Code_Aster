      SUBROUTINE TE0194 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
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
C
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          ELEMENTS 2D AXISYMETRIQUES FOURIER
C                          OPTION : 'CHAR_MECA_FR1D2D'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
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
C
      INTEGER            NNO,KP,NPG,ICARAC,IFF,IPOIDS,IVF,IDFDE,IGEOM
      INTEGER            IVECTU,K,I,IFORC
      REAL*8             POIDS,R,FX,FY,FZ,NX,NY
      CHARACTER*24       CARAC,FF
      CHARACTER*8        ELREFE
C     ------------------------------------------------------------------
C
      CALL ELREF1(ELREFE)
C
      CARAC = '&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE ( CARAC, 'L', ICARAC )
      NNO = ZI(ICARAC)
      NPG = ZI(ICARAC+2)
C
      FF = '&INEL.'//ELREFE//'.FF'
      CALL JEVETE ( FF, 'L', IFF )
      IPOIDS = IFF
      IVF    = IPOIDS + NPG
      IDFDE  = IVF    + NPG*NNO
C
      CALL JEVECH ( 'PGEOMER', 'L', IGEOM  )
      CALL JEVECH ( 'PFR1D2D', 'L', IFORC  )
      CALL JEVECH ( 'PVECTUR', 'E', IVECTU )
C
      DO 100 KP = 1 , NPG
         K = (KP-1)*NNO
         CALL VFF2DN ( NNO, ZR(IPOIDS+KP-1), ZR(IDFDE+K), ZR(IGEOM),
     &                 NX, NY, POIDS )
C
C        --- CALCUL DE LA FORCE AUX PG (A PARTIR DES NOEUDS) ---
C
         FX = 0.0D0
         FY = 0.0D0
         FZ = 0.0D0
         DO 102 I = 1 , NNO
            FX = FX + ZR(IFORC-1+3*(I-1)+1)*ZR(IVF+K+I-1)
            FY = FY + ZR(IFORC-1+3*(I-1)+2)*ZR(IVF+K+I-1)
            FZ = FZ + ZR(IFORC-1+3*(I-1)+3)*ZR(IVF+K+I-1)
 102     CONTINUE
C
         R = 0.D0
         DO 104 I = 1 , NNO
            R = R + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
 104     CONTINUE
         POIDS = POIDS*R
C
         DO 106 I = 1 , NNO
            ZR(IVECTU+3*(I-1))   = ZR(IVECTU+3*(I-1)) +
     +                                       FX*ZR(IVF+K+I-1)*POIDS
            ZR(IVECTU+3*(I-1)+1) = ZR(IVECTU+3*(I-1)+1) +
     +                                       FY*ZR(IVF+K+I-1)*POIDS
            ZR(IVECTU+3*(I-1)+2) = ZR(IVECTU+3*(I-1)+2) +
     +                                       FZ*ZR(IVF+K+I-1)*POIDS
 106     CONTINUE
C
 100  CONTINUE
C
      END
