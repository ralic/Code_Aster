      SUBROUTINE TE0421 ( OPTION , NOMTE )
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
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES EN 2D
C                      OPTION : 'CHAR_MECA_EPSA_R  '
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      PARAMETER        ( NBRES=10 )
      CHARACTER*16       PHENOM
      CHARACTER*8        NOMRES(NBRES),NOMPAR(2)
      CHARACTER*2        BL2, CODRET(NBRES)
      REAL*8             VALRES(NBRES),VALPAR(2),ZERO
      REAL*8             DFDX(9),DFDY(9),POIDS,R,EXX,EYY,EXY,EZZ
      REAL*8             A11,A22,A33,A12,A13,A23,DELTA,C1
      REAL*8             NU12,NU21,NU13,NU31,NU23,NU32,G12
      INTEGER            NNO,KP,K,NPG,I,ITEMPS,IVECTU
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,IMATE,IDEFA
C
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
      DATA  ZERO / 0.D0 /
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
C
      CALL JEVECH('PDEFAPR','L',IDEFA)
      CALL TECACH('ONN','PTEMPER',1,ITEMPE,IRET)
      CALL TECACH('ONN','PTEMPSR',1,ITEMPS,IRET)
      IF (ITEMPS.EQ.0) THEN
         NBPAR = 1
         NOMPAR(1)='TEMP'
      ELSE IF (ITEMPE.EQ.0) THEN
         NBPAR = 1
         NOMPAR(1)='INST'
         VALPAR(1) = ZR(ITEMPS)
      ELSE
         NOMPAR(1)='TEMP'
         NOMPAR(2)='INST'
         NBPAR = 2
         VALPAR(2) = ZR(ITEMPS)
      END IF
C
      BL2 = '  '
      IF (PHENOM.EQ.'ELAS')  THEN
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
      ELSE IF (PHENOM.EQ.'ELAS_ORTH') THEN
        NOMRES(1) = 'E_X'
        NOMRES(2) = 'E_Y'
        NOMRES(3) = 'E_Z'
        NOMRES(4) = 'NU_XY'
        NOMRES(5) = 'NU_XZ'
        NOMRES(6) = 'NU_YZ'
        NOMRES(7) = 'G_XY'
      ELSE IF (PHENOM.EQ.'ELAS_GITR') THEN
        NOMRES(1) = 'E_XY'
        NOMRES(2) = 'E_Z'
        NOMRES(3) = 'NU_XY'
        NOMRES(4) = 'NU_Z'
      ELSE
        CALL UTMESS('F','TE0421','COMPORTEMENT ELASTIQUE INEXISTANT')
      ENDIF
      CALL JEVECH('PVECTUR','E',IVECTU)
C
      DO 101 KP=1,NPG
        K=(KP-1)*NNO
        CALL DFDM2D ( NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS )
        R = ZERO
        TPG = ZERO
        EXX = ZERO
        EYY = ZERO
        EZZ = ZERO
        EXY = ZERO
        DO 102 I=1,NNO
          R    = R    +  ZR(IGEOM+2*I-2)*ZR(IVF+K+I-1)
          TPG  = TPG  +  ZR(ITEMPE+I-1) *ZR(IVF+K+I-1)
          EXX = EXX + ZR(IDEFA-1+4*(I-1)+1) *ZR(IVF+K+I-1)
          EYY = EYY + ZR(IDEFA-1+4*(I-1)+2) *ZR(IVF+K+I-1)
          EZZ = EZZ + ZR(IDEFA-1+4*(I-1)+3) *ZR(IVF+K+I-1)
          EXY = EXY + ZR(IDEFA-1+4*(I-1)+4) *ZR(IVF+K+I-1)
102     CONTINUE
C
        IF (ITEMPE.NE.0) VALPAR(1) = TPG
C
        IF (PHENOM.EQ.'ELAS') THEN
CCC --- CAS ISOTROPE
           CALL RCVALA ( ZI(IMATE),PHENOM,NBPAR,NOMPAR,VALPAR,2,
     &                   NOMRES, VALRES, CODRET, 'FM' )
           CALL RCVALA ( ZI(IMATE),PHENOM,NBPAR,NOMPAR,VALPAR,1,
     &                   NOMRES(3), VALRES(3), CODRET(3), BL2 )
           IF (CODRET(3).NE.'OK') VALRES(3) = 0.D0
C
           C1  = VALRES(1)/(1.D0 + VALRES(2))
           A11 = C1*(1.D0 - VALRES(2))/(1.D0 - 2.D0*VALRES(2))
           A12 = C1*        VALRES(2) /(1.D0 - 2.D0*VALRES(2))
           A13 = A12
           A22 = A11
           A23 = A12
           A33 = A11
           G12 = C1/2.D0
C
        ELSE IF (PHENOM.EQ.'ELAS_ORTH') THEN
CCC --- CAS ORTHOTROPE
           CALL RCVALA ( ZI(IMATE),PHENOM,NBPAR,NOMPAR,VALPAR,7,
     &                   NOMRES, VALRES, CODRET, 'FM' )
C
           E1   = VALRES(1)
           E2   = VALRES(2)
           E3   = VALRES(3)
           NU12 = VALRES(4)
           NU13 = VALRES(5)
           NU23 = VALRES(6)
           NU21 = E1*NU12/E2
           NU31 = E1*NU13/E3
           NU32 = E2*NU23/E3
          DELTA = 1.D0-NU23*NU32-NU31*NU13-NU12*NU21-2.D0*NU23*NU31*NU12
           A11 = (1.D0 - NU23*NU32)*E1/DELTA
           A12 = (NU12 + NU13*NU32)*E1/DELTA
           A13 = (NU13 + NU12*NU23)*E1/DELTA
           A22 = (1.D0 - NU13*NU31)*E2/DELTA
           A23 = (NU23 + NU13*NU21)*E2/DELTA
           A33 = (1.D0 - NU12*NU21)*E3/DELTA
           G12 = VALRES(7)
C
C
        ELSE IF (PHENOM.EQ.'ELAS_GITR') THEN
CCC     CAS ISOTROPE_TRANSVERSE
           CALL RCVALA ( ZI(IMATE),PHENOM,NBPAR,NOMPAR,VALPAR,4,
     &                   NOMRES, VALRES, CODRET, 'FM' )
C
           E1   = VALRES(1)
           E3   = VALRES(2)
           NU12 = VALRES(3)
           NU13 = VALRES(4)
           C1   = E1/(1.D0+NU12)
           DELTA = 1.D0 - NU12 - 2.D0*NU13*NU13*E1/E3
           A11 = (1.D0 - NU13*NU13*E1/E3)/DELTA
           A12 = C1*(A11 - 1.D0)
           A11 = C1*A11
           A13 = E1*NU13/DELTA
           A22 = A11
           A23 = A13
           A33 = E3*(1.D0 - NU12)/DELTA
           G12 = C1/2.D0
C
        ENDIF
C
        IF ( NOMTE(3:4) .EQ. 'CP' ) THEN
           A11=A11-A13*A13/A33
           A12=A12-A13*A23/A33
           A22=A22-A23*A23/A33
           A13=0.D0
           A23=0.D0
        ENDIF
C
        IF ( NOMTE(3:4) .EQ. 'AX') THEN
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
             DO 203 I=1,NNO
               ZR(IVECTU+2*I-2) = ZR(IVECTU+2*I-2) + POIDS * (
     &                        (A11*EXX+A12*EYY+A13*EZZ)*DFDX(I)
     &                     +  (A13*EXX+A23*EYY+A33*EZZ)*DFDX(I)
     &                     +  2*G12*EXY*DFDY(I))
               ZR(IVECTU+2*I-1) = ZR(IVECTU+2*I-1) + POIDS * (
     &                        (A12*EXX+A22*EYY+A23*EZZ)*DFDY(I)
     &                     +  2*G12*EXY*DFDX(I))
203          CONTINUE
           ENDIF
C
        ELSE
           DO 104 I=1,NNO
              ZR(IVECTU+2*I-2)=ZR(IVECTU+2*I-2) + POIDS * (
     &                     (A11*EXX+A12*EYY+A13*EZZ)*DFDX(I)
     &                     +  2*G12*EXY*DFDY(I))
             ZR(IVECTU+2*I-1)=ZR(IVECTU+2*I-1) + POIDS * (
     &                     (A12*EXX+A22*EYY+A23*EZZ)*DFDY(I)
     &                     +  2*G12*EXY*DFDX(I))
104        CONTINUE
        ENDIF
101   CONTINUE
      END
