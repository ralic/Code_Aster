      SUBROUTINE TE0266 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DE FLUX AUX POINTS DE GAUSS
C                          OPTION : 'FLUX_ELGA_TEMP  '
C                             ELEMENTS FOURIER
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*2        CODRET
      REAL*8             VALRES, FLUXR,FLUXZ,FLUXT
      REAL*8             DFDR(9),DFDZ(9),TPG,POIDS,XH
      INTEGER            NNO,KP,I,K,ITEMPE,ITEMP,INST,IFLUX,IHARM,NH
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,IMATE
      INTEGER            NPG,NNOS,JGANO,NDIM
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PHARMON','L',IHARM)
      NH = ZI(IHARM)
      XH = DBLE(NH)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PTEMPSR','L',ITEMP)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL JEVECH('PFLUX_R','E',IFLUX)
C
      CALL RCVALA ( ZI(IMATE),'THER',1,'INST',ZR(ITEMP),1,'LAMBDA',
     &              VALRES, CODRET, 'FM' )
C
       DO 101 KP=1,NPG
          K = (KP-1)*NNO
          CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDR,DFDZ,POIDS)
C
          R = 0.D0
          DO 102 I=1,NNO
            R = R + ZR(IGEOM+2*I-2) * ZR(IVF+K+I-1)
102       CONTINUE
C
          FLUXR = 0.0D0
          FLUXZ = 0.0D0
          FLUXT = 0.0D0
          DO 110 J=1,NNO
             FLUXR = FLUXR + ZR(ITEMPE+J-1)*DFDR(J)
             FLUXZ = FLUXZ + ZR(ITEMPE+J-1)*DFDZ(J)
             FLUXT = FLUXT - ZR(ITEMPE+J-1)*ZR(IVF+K+J-1)*XH/R
 110      CONTINUE
C
         ZR(IFLUX+(KP-1)*3)   = -VALRES*FLUXR
         ZR(IFLUX+(KP-1)*3+1) = -VALRES*FLUXZ
         ZR(IFLUX+(KP-1)*3+2) = -VALRES*FLUXT
C
 101  CONTINUE
C
      END
