      SUBROUTINE TE0563 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/10/2008   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES EN 2D
C                      OPTION : 'CHAR_ALPH_ZAC  '
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      PARAMETER        ( NBRES=10 )
      CHARACTER*16       PHENOM
      CHARACTER*8        NOMRES(NBRES),NOMPAR(2)
      CHARACTER*4        FAMI
      CHARACTER*2        CODRET(NBRES)
      REAL*8             VALRES(NBRES),VALPAR(2),ZERO
      REAL*8             DFDX(9),DFDY(9),POIDS,R,EXX,EYY,EXY,EZZ
      REAL*8             A11,A22,A33,A12,A13,A23,C1
      REAL*8             G12,E,NU,NUCH,ECH
      INTEGER            NNO,KP,K,NPG1,I,ITEMPS,IVECTU
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,IMATE
      INTEGER            NBV,IALPHA,NDIM,NNOS,JGANO
      LOGICAL            LTEATT
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
      DATA  ZERO /0.D0/
C
      FAMI = 'RIGI'
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
C
      CALL JEVECH('PALPHAR','L',IALPHA)
C
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3)=  'D_SIGM_EPSI'
      NOMRES(4)=  'SY'
C
      CALL JEVECH('PVECTUR','E',IVECTU)
C
      DO 101 KP=1,NPG1
        K=(KP-1)*NNO
        IDPG=(KP-1)*4
        CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
        R = ZERO
        EXX = ZR(IALPHA+IDPG)
        EYY = ZR(IALPHA+IDPG+1)
        EZZ = ZR(IALPHA+IDPG+2)
        EXY = ZR(IALPHA+IDPG+3)
C
        DO 102 I=1,NNO
          R    = R    +  ZR(IGEOM+2*I-2)*ZR(IVF+K+I-1)
102     CONTINUE
C
C
        CALL RCVALB ( FAMI,KP,1,'+',ZI(IMATE),' ',PHENOM,0,' ',0.D0,2,
     &                   NOMRES, VALRES, CODRET, 'FM' )
C
        CALL RCVALB ( FAMI,KP,1,'+',ZI(IMATE),' ','ECRO_LINE',0,' ',
     &               0.D0,2, NOMRES(3), VALRES(3), CODRET, 'FM' )
C
C  CONSTANTES ELASTIQUES MODIFIEES
C
           E   =  VALRES(1)
           NU   = VALRES(2)
           DSDE = VALRES(3)
           C = 2.D0/3.D0*(E*DSDE)/(E-DSDE)
           ECH = 3.D0*C*E/(2.D0*E+3.D0*C)
           NUCH = (3.D0*C*NU+E)/(2.D0*E+3.D0*C)
           E = ECH
           NU = NUCH
C
           C1  = E/(1.D0 + NU)
           A11 = C1*(1.D0 - NU)/(1.D0 - 2.D0*NU)
           A12 = C1*        NU /(1.D0 - 2.D0*NU)
           A13 = A12
           A22 = A11
           A23 = A12
           A33 = A11
           G12 = C1/2.D0
C
        IF ( NOMTE(3:4) .EQ. 'CP' ) THEN
           A11=A11-A13*A13/A33
           A12=A12-A13*A23/A33
           A22=A22-A23*A23/A33
           A13=0.D0
           A23=0.D0
        ENDIF
C
        IF ( LTEATT(' ','AXIS','OUI')) THEN
           POIDS = POIDS*R
           IF (R .NE. 0.D0) THEN
             DO 103 I=1,NNO
               ZR(IVECTU+2*I-2) = ZR(IVECTU+2*I-2) + POIDS *(
     &                        (A11*EXX+A12*EYY+A13*EZZ)* DFDX(I)
     &                     +  (A13*EXX+A23*EYY+A33*EZZ)*ZR(IVF+K+I-1)/R
     &                     +  2*G12*EXY*DFDY(I))
               ZR(IVECTU+2*I-1) = ZR(IVECTU+2*I-1) + POIDS *(
     &                        (A12*EXX+A22*EYY+A23*EZZ)*DFDY(I)
     &                     +  2*G12*EXY*DFDX(I))
103          CONTINUE
           ELSE
             DO 104 I=1,NNO
               ZR(IVECTU+2*I-2) = ZR(IVECTU+2*I-2) + POIDS * (
     &                        (A11*EXX+A12*EYY+A13*EZZ)*DFDX(I)
     &                     +  (A13*EXX+A23*EYY+A33*EZZ)*DFDX(I)
     &                     +  2*G12*EXY*DFDY(I))
               ZR(IVECTU+2*I-1) = ZR(IVECTU+2*I-1) + POIDS * (
     &                        (A12*EXX+A22*EYY+A23*EZZ)*DFDY(I)
     &                     +  2*G12*EXY*DFDX(I))
104          CONTINUE
           ENDIF
C
        ELSE
           DO 105 I=1,NNO
              ZR(IVECTU+2*I-2)=ZR(IVECTU+2*I-2) + POIDS * (
     &                     (A11*EXX+A12*EYY+A13*EZZ)*DFDX(I)
     &                     +  2*G12*EXY*DFDY(I))
              ZR(IVECTU+2*I-1)=ZR(IVECTU+2*I-1) + POIDS * (
     &                     (A12*EXX+A22*EYY+A23*EZZ)*DFDY(I)
     &                     +  2*G12*EXY*DFDX(I))
105        CONTINUE
        ENDIF
101   CONTINUE
      END
