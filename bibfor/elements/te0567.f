      SUBROUTINE TE0567 ( OPTION , NOMTE )
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES EN 3D
C                      OPTION : 'CHAR_ALPH_ZAC  '
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      PARAMETER        ( NBRES=10 )
      CHARACTER*24       CHVAL,CHCTE
      CHARACTER*16       PHENOM
      CHARACTER*8        NOMRES(NBRES),NOMPAR(2),ELREFE
      CHARACTER*2        CODRET(NBRES)
      REAL*8             VALRES(NBRES),VALPAR(2),ZERO
      REAL*8             DFDX(27),DFDY(27),DFDZ(27),POIDS,EXX,EYY
      REAL*8             A11,A22,A33,A12,A13,A23,C1,EYZ,EXZ,EXY,EZZ
      REAL*8             G12,E,NU,NUCH,ECH
      INTEGER            NNO,KP,K,NPG,I,ITEMPS,IVECTU,NBPG(10)
      INTEGER            IPOIDS,IVF,IDFDE,IDFDK,IGEOM,IMATE
      INTEGER            NBV,IALPHA
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
      CALL ELREF1(ELREFE)
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM = ZI(JIN+1-1)
      NNO = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      DO 10 I = 1,NBFPG
        NBPG(I) = ZI(JIN+3-1+I)
10    CONTINUE
C
      NPG = NBPG(1)
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
C
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO

CJMP   PARAIT FAUX =>  ON INTERDIT
C      CALL UTMESS('F','POST_ZAC','NE MARCHE PAS ENCORE EN 3D')

C      IF(ELREFE.EQ.'TETRA10'.OR.ELREFE.EQ.'HEXA20' ) THEN
C        IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO)
C     &                  + NBPG(2)*(1+(NDIM+1)*NNO)
C       ELSE IF(ELREFE.EQ.'PENTA15' ) THEN
C        IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO)
C      ENDIF

      IVF = IPOIDS + NPG
      IDFDE  = IVF + NPG*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
C
      CALL JEVECH('PALPHAR','L',IALPHA)
      CALL TECACH(.TRUE.,.FALSE.,'PTEMPER',1,ITEMPE)
      NBPAR = 1
      NOMPAR(1)='TEMP'
C
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3)=  'D_SIGM_EPSI'
      NOMRES(4)=  'SY'
C
      CALL JEVECH('PVECTUR','E',IVECTU)
C
      DO 101 KP=1,NPG
C
        K=(KP-1)* 3 * NNO
        IT = (KP-1) * NNO
        IDPG=(KP-1) * 6
        CALL DFDM3D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &                ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS )
        TPG = ZERO
        EXX = ZR(IALPHA+IDPG)
        EYY = ZR(IALPHA+IDPG+1)
        EZZ = ZR(IALPHA+IDPG+2)
        EXY = ZR(IALPHA+IDPG+3)
        EXZ = ZR(IALPHA+IDPG+4)
        EYZ = ZR(IALPHA+IDPG+5)
C
        DO 102 I=1,NNO
          TPG  = TPG  +  ZR(ITEMPE+I-1) *ZR(IVF+IT+I-1)
102     CONTINUE
C
        IF (ITEMPE.NE.0) VALPAR(1) = TPG
C
        CALL RCVALA ( ZI(IMATE),PHENOM,NBPAR,NOMPAR,VALPAR,2,
     &                NOMRES, VALRES, CODRET, 'FM' )
C
        CALL RCVALA ( ZI(IMATE),'ECRO_LINE',NBPAR,NOMPAR,VALPAR,2,
     &                NOMRES(3), VALRES(3), CODRET, 'FM' )
C
C  CONSTANTES ELASTIQUES MODIFIEES
C
         E    =  VALRES(1)
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
         DO 105 I=1,NNO
              ZR(IVECTU+3*I-3)=ZR(IVECTU+3*I-3) + POIDS * (
     &                     (A11*EXX+A12*EYY+A13*EZZ)*DFDX(I)
     &                     +  2*G12*EXY*DFDY(I)
     &                     +  2*G12*EXZ*DFDZ(I))
              ZR(IVECTU+3*I-2)=ZR(IVECTU+3*I-2) + POIDS * (
     &                     (A12*EXX+A22*EYY+A23*EZZ)*DFDY(I)
     &                     +  2*G12*EXY*DFDX(I)
     &                     +  2*G12*EYZ*DFDZ(I))
              ZR(IVECTU+3*I-1)=ZR(IVECTU+3*I-1) + POIDS * (
     &                     (A13*EXX+A23*EYY+A33*EZZ)*DFDZ(I)
     &                     +  2*G12*EXZ*DFDX(I)
     &                     +  2*G12*EYZ*DFDY(I))
105      CONTINUE
C
101   CONTINUE
      END
