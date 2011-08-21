      SUBROUTINE TE0114 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 23/08/2011   AUTEUR DELMAS J.DELMAS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C ......................................................................
      IMPLICIT REAL*8 (A-H,O-Z)
C     BUT: CALCUL DES DEFORMATIONS AUX NOEUDS EN MECANIQUE
C          ELEMENTS ISOPARAMETRIQUES 2D FOURIER
C
C            OPTION : 'EPSI_ELNO   '
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
      CHARACTER*16       NOMTE,OPTION  
      REAL*8             R,XH,WI,U(3,9),DEPG(54)
      REAL*8             DFDR(9),DFDZ(9),POIDS
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM
      INTEGER            NPG,NNOS,JGANO,NDIM
      INTEGER            NNO,KP,IHARMO
C
C
      CALL ELREF4(' ','GANO',NDIM,NNO,NNOS,NPG,IPOIDS,
     &                   IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PHARMON','L',IHARMO)
      NH = ZI(IHARMO)
      XH = DBLE(NH)
      CALL JEVECH('PDEFORR','E',IDEFO )
C
            DO 112 I=1,6*NPG
                DEPG (I) = 0.0D0
112          CONTINUE
C
            DO 113 I=1,NNO
                U(1,I) = ZR(IDEPL + 3 * I - 3)
                U(2,I) = ZR(IDEPL + 3 * I - 2)
                U(3,I) = ZR(IDEPL + 3 * I - 1)
113          CONTINUE
C
C    BOUCLE SUR LES POINTS DE GAUSS
C
      DO 101 KP = 1,NPG
C
        IDPG = (KP-1) * 6
        KDEC = (KP-1) * NNO
        CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDR,DFDZ,POIDS)
        R   = 0.D0
        DO 102 I=1,NNO
          R   = R   + ZR(IGEOM+2*(I-1))*ZR(IVF+KDEC+I-1)
 102    CONTINUE
C
           DO 106 I=1,NNO
       WI = ZR(IVF+KDEC+I-1)/R
C
       DEPG(IDPG+1) = DEPG(IDPG+1) + U(1,I) * DFDR(I)
C
       DEPG(IDPG+2) = DEPG(IDPG+2) + U(2,I) * DFDZ(I)
C
       DEPG(IDPG+3) = DEPG(IDPG+3) + (U(1,I) + XH * U(3,I)) * WI
C
       DEPG(IDPG+4) = DEPG(IDPG+4) + (U(2,I)*DFDR(I) + U(1,I)*DFDZ(I))
     &                             * 0.5D0
C
       DEPG(IDPG+5) = DEPG(IDPG+5) - U(1,I) * 0.5D0 * XH * WI
     &                             + U(3,I) * 0.5D0 * (DFDR(I) - WI)
C
       DEPG(IDPG+6) = DEPG(IDPG+6) - U(2,I) * 0.5D0 * XH * WI
     &                             + U(3,I) * 0.5D0 * DFDZ(I)
C
106       CONTINUE
C
101   CONTINUE
C
      CALL PPGAN2(JGANO,1,6,DEPG,ZR(IDEFO))
C
      END
