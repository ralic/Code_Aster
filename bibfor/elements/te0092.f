      SUBROUTINE TE0092 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTION : 'RIGI_MECA_GEOM  '
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      REAL*8             DFDX(9),DFDY(9),POIDS,R,ZERO,UN,AXIS
      REAL*8             SXX,SXY,SYY
      INTEGER            NNO,KP,K,NPG,II,JJ,I,J,IMATUU,KD1,KD2,IJ1,IJ2
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,ICONTR,KC
      LOGICAL            LTEATT
C
C
      ZERO=0.D0
      UN  =1.D0
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PCONTRR','L',ICONTR)
      CALL JEVECH('PMATUUR','E',IMATUU)
C
      AXIS=ZERO
      R   =UN
      IF ( LTEATT(' ','AXIS','OUI') ) AXIS=UN
C
      DO 101 KP=1,NPG
        K=(KP-1)*NNO
        KC=ICONTR+4*(KP-1)
        SXX=ZR(KC  )
        SYY=ZR(KC+1)
        SXY=ZR(KC+3)
        CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS )
        IF (AXIS .GT. 0.5D0) THEN
           R   = ZERO
           DO 102 I=1,NNO
             R = R + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
102        CONTINUE
           DO 103 I=1,NNO
             DFDX(I)=DFDX(I)+ZR(IVF+K+I-1)/R
103        CONTINUE
           POIDS=POIDS*R
        END IF
C
        KD1=2
        KD2=1
        DO 106 I=1,2*NNO,2
          KD1=KD1+2*I-3
          KD2=KD2+2*I-1
           II = (I+1)/2
           DO 107 J=1,I,2
             JJ = (J+1)/2
             IJ1=IMATUU+KD1+J-2
             IJ2=IMATUU+KD2+J-1
             ZR(IJ2) = ZR(IJ2) +POIDS*(
     &                            DFDX(II)*(DFDX(JJ)*SXX+DFDY(JJ)*SXY)+
     &                            DFDY(II)*(DFDX(JJ)*SXY+DFDY(JJ)*SYY))
             ZR(IJ1) = ZR(IJ2)
107        CONTINUE
106      CONTINUE
C
101   CONTINUE
      END
