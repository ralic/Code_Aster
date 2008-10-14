      SUBROUTINE TE0376 ( OPTION , NOMTE )
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
      CHARACTER*16        OPTION , NOMTE , PHENOM
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES CONTRAINTES AUX NOEUDS EN 2D
C                          SANS LISSAGE (POUR L'ESTIMATEUR EN RESIDU)
C                          OPTION : 'SIRE_ELNO_DEPL'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      PARAMETER         ( NBRES=7)
      CHARACTER*8        NOMRES(NBRES)
      CHARACTER*2        CODRET(NBRES)
      REAL*8             VALRES(NBRES),CG(54)
      REAL*8             DDZDZ,DDRXDZ,DDRYDZ,VX,VY,MT,S,JJ,EPS(5),C1
      REAL*8             DFDX(9),DFDY(9),POIDS,X,Y
      REAL*8             A11,A12,A22,A33,G12,E1,E2,E3,DELTA
      REAL*8             NU12,NU21,NU23,NU32,NU13,NU31,EPSTHE(3)
      INTEGER            NNO,KP,I,K,IDEPL,ICONT
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,IMATE
      INTEGER            NPG1,NNOS,NCMP,NBV,NDIM,JGANO,IRET
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
      CALL ELREF4(' ','NOEU',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
C
      NCMP = 4
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PCONTRR','E',ICONT)

C
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
      IF (PHENOM.EQ.'ELAS') THEN
        NOMRES(1)='E'
        NOMRES(2)='NU'
      ELSE IF (PHENOM.EQ.'ELAS_ORTH') THEN
        NOMRES(1)='E_L'
        NOMRES(2)='E_T'
        NOMRES(3)='E_N'
        NOMRES(4)='NU_LT'
        NOMRES(5)='NU_LN'
        NOMRES(6)='NU_TN'
        NOMRES(7)='G_LT'
      ELSE IF (PHENOM.EQ.'ELAS_ISTR') THEN
        NOMRES(1)='E_L'
        NOMRES(2)='E_N'
        NOMRES(3)='NU_LT'
        NOMRES(4)='NU_LN'
        NOMRES(6)='ALPHA_N'
      ELSE
        CALL U2MESS('F','ELEMENTS_50')
      ENDIF
C
      DO 111 I=1,NCMP*NNO
        CG(I) = 0.D0
111   CONTINUE
C
      DO 101 KP=1,NNO
        K=(KP-1)*NNO
        CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
        X   = 0.D0
        Y   = 0.D0
        DO 103 I=1,5
           EPS(I) = 0.0D0
103     CONTINUE
        DO 102 I=1,NNO
           X   = X   + ZR(IGEOM+2*I-2)*ZR(IVF+K+I-1)
           Y   = Y   + ZR(IGEOM+2*I-1)*ZR(IVF+K+I-1)
           EPS(1) = EPS(1) + DFDX(I)       * ZR(IDEPL+2*(I-1)  )
           EPS(3) = EPS(3) + ZR(IVF+K+I-1) * ZR(IDEPL+2*(I-1))
           EPS(2) = EPS(2) + DFDY(I)       * ZR(IDEPL+2*(I-1)+1)
           EPS(4) = EPS(4) + DFDY(I)       * ZR(IDEPL+2*(I-1)  )
           EPS(5) = EPS(5) + DFDX(I)       * ZR(IDEPL+2*(I-1)+1)
102     CONTINUE
C
        IF (PHENOM.EQ.'ELAS') THEN
CCC --- CAS ISOTROPE
           CALL RCVALB ('NOEU',KP,1,'+', ZI(IMATE),' ',PHENOM,0,' ',
     &                   0.D0,2,NOMRES,VALRES, CODRET, 'FM' )
C
           CALL VERIFT('NOEU',KP,1,'+',ZI(IMATE),PHENOM,1,EPSTHE,IRET)
           C1  = VALRES(1)/(1.D0 + VALRES(2))
           A11 = C1*(1.D0 - VALRES(2))/(1.D0 - 2.D0*VALRES(2))
           A12 = A11 - C1
           A13 = A12
           A22 = A11
           A23 = A12
           A33 = A11
           G12 = C1/2.D0
           AL1 = EPSTHE(1)
           AL2 = AL1
           AL3 = AL1
C
        ELSE IF (PHENOM.EQ.'ELAS_ORTH') THEN
CCC --- CAS ORTHOTROPE
           CALL RCVALB ('NOEU',KP,1,'+', ZI(IMATE),' ',PHENOM,0,' ',
     &                   0.D0,7,NOMRES,VALRES, CODRET, 'FM' )
C
           CALL VERIFT('NOEU',KP,1,'+',ZI(IMATE),PHENOM,3,EPSTHE,IRET)
           E1   = VALRES(1)
           E2   = VALRES(2)
           E3   = VALRES(3)
           NU12 = VALRES(4)
           NU13 = VALRES(5)
           NU23 = VALRES(6)
           NU21 = E2*NU12/E1
           NU31 = E1*NU13/E3
           NU32 = E2*NU23/E3
          DELTA = 1.D0-NU23*NU32-NU31*NU13-NU21*NU12-2.D0*NU23*NU31*NU21
           A11 = (1.D0 - NU23*NU32)*E1/DELTA
           A12 = (NU21 + NU13*NU32)*E1/DELTA
           A13 = (NU13 + NU21*NU23)*E1/DELTA
           A22 = (1.D0 - NU13*NU31)*E2/DELTA
           A23 = (NU23 + NU13*NU12)*E2/DELTA
           A33 = (1.D0 - NU21*NU12)*E3/DELTA
           G12 = VALRES(7)
           AL1 = EPSTHE(1)
           AL2 = EPSTHE(2)
           AL3 = EPSTHE(3)
C
        ELSE IF (PHENOM.EQ.'ELAS_ISTR') THEN
CCC     CAS ISOTROPE_TRANSVERSE
           CALL RCVALB ('NOEU',KP,1,'+', ZI(IMATE),' ',PHENOM,0,' ',
     &                   0.D0,6,NOMRES,VALRES, CODRET, 'FM' )
C
           CALL VERIFT('NOEU',KP,1,'+',ZI(IMATE),PHENOM,2,EPSTHE,IRET)
           E1   = VALRES(1)
           E3   = VALRES(2)
           NU12 = VALRES(3)
           NU13 = VALRES(4)
           C1   = E1/(1.D0+NU12)
           DELTA = 1.D0 - NU12 - 2.D0*NU13*NU13*E1/E3
           A11 = C1*(1.D0 - NU13*NU13*E1/E3)/DELTA
           A12 = A11 - C1
           A13 = E1*NU13/DELTA
           A22 = A11
           A23 = A13
           A33 = E3*(1.D0 - NU12)/DELTA
           G12 = C1/2.D0
           AL1 = EPSTHE(1)
           AL2 = AL1
           AL3 = EPSTHE(3)
C
        ENDIF
        IF ( LTEATT(' ','AXIS','OUI')) THEN
           IF ( X .NE. 0.D0 ) THEN
              EPS(3) = EPS(3) / X
           ELSE
              EPS(3) = EPS(1)
           ENDIF

        ELSE IF (NOMTE(3:4) .EQ. 'CP' ) THEN
           EPS(3) = AL3-(A13*(EPS(1)-AL1)+A23*(EPS(2)-AL2))/A33
        ELSE
           EPS(3) = 0.D0
        ENDIF
        CG(NCMP*(KP-1)+1) = A11*(EPS(1)-AL1)+A12*(EPS(2)-AL2)+
     &                      A13*(EPS(3)-AL3)
        CG(NCMP*(KP-1)+2) = A12*(EPS(1)-AL1)+A22*(EPS(2)-AL2)+
     &                      A23*(EPS(3)-AL3)
        CG(NCMP*(KP-1)+3) = A13*(EPS(1)-AL1)+A23*(EPS(2)-AL2)+
     &                      A33*(EPS(3)-AL3)
        CG(NCMP*(KP-1)+4) = G12*(EPS(4)+EPS(5))
C
101   CONTINUE
C
        DO 105 KP=1,NNO
          ZR(ICONT+NCMP*(KP-1) )  = CG(NCMP*(KP-1)+1)
          ZR(ICONT+NCMP*(KP-1)+1) = CG(NCMP*(KP-1)+2)
          ZR(ICONT+NCMP*(KP-1)+2) = CG(NCMP*(KP-1)+3)
          ZR(ICONT+NCMP*(KP-1)+3) = CG(NCMP*(KP-1)+4)
105     CONTINUE
      END
