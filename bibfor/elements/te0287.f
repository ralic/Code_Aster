      SUBROUTINE TE0287 ( OPTION , NOMTE )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX,t FRANCE.
C ======================================================================
C
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTION : 'RIGI_LAGR        '
C
C    - ARGUMENTS DONNES:
C                OPTION   ---->  OPTION DE CALCUL
C                NOMTE    ---->  NOM DU TYPE ELEMENT
C ......................................................................
C
      INTEGER            NNO,KP,NPG1,II,JJ,I,J,K,IMATUU,NBRES
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,IMATE
      INTEGER            KD1,KD2,IJ1,IJ2,IDEPL,IFORC,MATER,ITHET,IALPH
      INTEGER            NNOS,JGANO,NDIM
      PARAMETER         ( NBRES = 2 )
C
      REAL*8             VALRES(NBRES),BIDON(7),BIDON1(7),TGD(2)
      REAL*8             DFDX(9),DFDY(9),DTDM(7),C1,C2,C3,POIDS,R,ALPHA
      REAL*8             XG,YG,CP1,CP2,CP3,DP1,DP2,DP3,VFI,VFJ
      REAL*8             A11,A12,A21,A22,A,AAX
C
      CHARACTER*8        NOMRES(NBRES)
      CHARACTER*4        FAMI
      INTEGER ICODRE(NBRES)

      LOGICAL            LTEATT
C ......................................................................
C
      FAMI = 'RIGI'
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
C
      CALL JEVECH('PMATERC','L',IMATE)
      MATER = ZI(IMATE)
      NOMRES(1)='E'
      NOMRES(2)='NU'
      CALL JEVECH('PMATUUR','E',IMATUU)
      CALL JEVECH('PTHETAR','L',ITHET)
      CALL JEVECH('PALPHAR','L',IALPH)
C
C   ATTENTION , ICI ''PDEPLAR'' ET ''PFRVOLU'' N'EXISTENT PAS DANS
C   L'OPTION MAIS ON PEUT LEUR AFFECTER  DES VALEURS 'BIDONS'
C   CAR ILS NE NOUS SERVENT PAS ICI
C
      IDEPL = ITHET
      IFORC = ITHET
C
      ALPHA = ZR(IALPH)
C
C

      DO 101 KP=1,NPG1
        K=(KP-1)*NNO
        CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
        R   = 0.D0
        XG  = 0.D0
        YG  = 0.D0
        DO 102 I=1,NNO
          R   = R   + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
          XG  = XG  + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
          YG  = YG  + ZR(IGEOM+2*(I-1)+1)*ZR(IVF+K+I-1)
 102    CONTINUE
        CALL RCVALB(FAMI,KP,1,'+',MATER,' ','ELAS',0,' ',0.D0,
     &              2,NOMRES,VALRES, ICODRE, 1)
C
        IF ( LTEATT(' ','AXIS','OUI') ) THEN
           POIDS = POIDS*R
           C1 = VALRES(1)/(1.D0 + VALRES(2))
           C2 = (1.D0 - VALRES(2))/(1.D0 - 2.D0*VALRES(2))
           C3 = VALRES(2)/(1.D0 - 2.D0*VALRES(2))
           CALL GDFONC ( FAMI,DFDX,DFDY,KP,ZR(IVF),ZR(IDEPL),ZR(ITHET),
     &                   ZR(IFORC),NNO,BIDON,DTDM,BIDON1,TGD)
           DTDM(4) = DTDM(4)/R
           KD1=2
           KD2=1
           A11 = (1.D0+ALPHA*DTDM(4)) * (1.D0+ALPHA*DTDM(2))
           A12 = -ALPHA*DTDM(3)*(1.D0+ALPHA*DTDM(4))
           A21 = -ALPHA*DTDM(5)*(1.D0+ALPHA*DTDM(4))
           A22 = (1.D0+ALPHA*DTDM(1)) * (1.D0+ALPHA*DTDM(4))
           AAX = ( (1.D0+ALPHA*DTDM(1))*(1.D0+ALPHA*DTDM(2)) )-
     &           ALPHA*ALPHA*DTDM(3)*DTDM(5)
           A   = 1.D0 + ALPHA*(DTDM(1)+DTDM(2)+DTDM(4))+ ALPHA*ALPHA*(
     &           DTDM(1)*DTDM(2) + DTDM(4)*DTDM(2) -
     &           DTDM(5)*DTDM(3) + DTDM(4)*DTDM(1) )
     &         + (ALPHA**3)*DTDM(4)*(DTDM(1)*DTDM(2)-DTDM(3)*DTDM(5))
C
           DO 103 I=1,2*NNO,2
             KD1=KD1+2*I-3
             KD2=KD2+2*I-1
             II = (I+1)/2
             DO 104 J=1,I,2
               JJ = (J+1)/2
               IJ1=IMATUU+KD1+J-2
               IJ2=IMATUU+KD2+J-2
               VFI=ZR(IVF+K+II-1)
               VFJ=ZR(IVF+K+JJ-1)
               ZR(IJ1  ) = ZR(IJ1  ) + POIDS/A*C1*(
     &                   C2*A11*A11*DFDX(II)*DFDX(JJ) +
     &                   C2*A21*A21*DFDY(II)*DFDY(JJ) +
     &                   C2*A11*A21*( DFDX(II)*DFDY(JJ) +
     &                               DFDX(JJ)*DFDY(II) ) +
     &                   C3*AAX*( VFI/R *(A11*DFDX(JJ) +
     &                                    A21*DFDY(JJ)) +
     &                            VFJ/R *(A11*DFDX(II) +
     &                                    A21*DFDY(II)) ) +
     &                   A12*A12/2.D0*DFDX(II)*DFDX(JJ) +
     &                   A22*A22/2.D0*DFDY(II)*DFDY(JJ) +
     &                   A12*A22/2.D0*(DFDX(II)*DFDY(JJ) +
     &                              DFDX(JJ)*DFDY(II)) +
     &                   C2*AAX*AAX*VFI*VFJ/(R*R)
     &                                          )
               ZR(IJ2) = ZR(IJ2) + POIDS/A*C1*(
     &                               C3*(A12*A11*DFDX(II)*
     &                                     DFDX(JJ) + A21*A22*
     &                                     DFDY(II)*DFDY(JJ) +
     &                                     A12*A21*DFDX(II)*
     &                                     DFDY(JJ) + A11*A22*
     &                                     DFDY(II)*DFDX(JJ)) +
     &                                1/2.D0*(A12*A11*DFDX(JJ)*
     &                                     DFDX(II) + A21*A22*
     &                                     DFDY(JJ)*DFDY(II) +
     &                                     A12*A21*DFDX(JJ)*
     &                                     DFDY(II) + A11*A22*
     &                                     DFDY(JJ)*DFDX(II)) +
     &                        AAX*C3*VFJ/R*(A12*DFDX(II)+
     &                                      A22*DFDY(II))
     &                                      )
               ZR(IJ2+1) = ZR(IJ2+1 ) + POIDS/A*C1*(
     &                     C2*A12*A12*DFDX(II)*DFDX(JJ) +
     &                     C2*A22*A22*DFDY(II)*DFDY(JJ) +
     &                     C2*A12*A22*(DFDX(II)*DFDY(JJ) +
     &                                 DFDX(JJ)*DFDY(II)) +
     &                     A11*A11/2.D0*DFDX(II)*DFDX(JJ) +
     &                     A21*A21/2.D0*DFDY(II)*DFDY(JJ) +
     &                     A11*A21/2.D0*(DFDX(II)*DFDY(JJ) +
     &                                DFDX(JJ)*DFDY(II))
     &                                           )
104          CONTINUE
C
             DO 105 J=1,I-2,2
               JJ = (J+1)/2
               IJ1=IMATUU+KD1+J-2
               VFI=ZR(IVF+K+II-1)
               VFJ=ZR(IVF+K+JJ-1)
               ZR(IJ1+1) = ZR(IJ1+1) + POIDS/A*C1*(
     &                               C3*(A12*A11*DFDX(JJ)*
     &                                     DFDX(II) + A21*A22*
     &                                     DFDY(JJ)*DFDY(II) +
     &                                     A12*A21*DFDX(JJ)*
     &                                     DFDY(II) + A11*A22*
     &                                     DFDY(JJ)*DFDX(II)) +
     &                                1/2.D0*(A12*A11*DFDX(II)*
     &                                     DFDX(JJ) + A21*A22*
     &                                     DFDY(II)*DFDY(JJ) +
     &                                     A12*A21*DFDX(II)*
     &                                     DFDY(JJ) + A11*A22*
     &                                     DFDY(II)*DFDX(JJ)) +
     &                                AAX*C3*VFI/R*(A12*DFDX(JJ)+
     &                                      A22*DFDY(JJ))
     &                                      )
105          CONTINUE
103        CONTINUE
C
C
         ELSE
      CP1 = VALRES(1)/(1.D0 -VALRES(2)*VALRES(2))
      CP2 = VALRES(2)
      CP3 = (1.D0 -VALRES(2))/2.D0
      DP1 = VALRES(1)*(1.D0-VALRES(2))/((1.D0+VALRES(2))*
     &                                 (1.D0-2.D0*VALRES(2)))
      DP2 = VALRES(2)/(1.D0-VALRES(2))
      DP3 = (1.D0-2.D0*VALRES(2))/(2.D0*(1.D0-VALRES(2)))
C
      CALL GDFONC ( FAMI,DFDX,DFDY,KP,ZR(IVF),ZR(IDEPL),
     &              ZR(ITHET),ZR(IFORC),
     &              NNO,BIDON,DTDM,BIDON1,TGD)
        A22 = 1 + ALPHA * DTDM(2)
        A11 = 1 + ALPHA * DTDM(1)
        A12 =   - ALPHA * DTDM(3)
        A21 =   - ALPHA * DTDM(5)
        A   = 1 + ALPHA * (DTDM(1)+DTDM(2)) +
     &          ALPHA*ALPHA*(DTDM(1)*DTDM(2)-DTDM(3)*DTDM(5))
C
         IF ( LTEATT(' ','D_PLAN','OUI') ) THEN
           C1 = DP1
           C2 = DP2
           C3 = DP3
         ELSE
           C1 = CP1
           C2 = CP2
           C3 = CP3
         ENDIF
C
           KD1=2
           KD2=1
           DO 106 I=1,2*NNO,2
             KD1=KD1+2*I-3
             KD2=KD2+2*I-1
             II = (I+1)/2
C
             DO 107 J=1,I,2
               JJ = (J+1)/2
               IJ1=IMATUU+KD1+J-2
               IJ2=IMATUU+KD2+J-2
               VFI=ZR(IVF+K+II-1)
               VFJ=ZR(IVF+K+JJ-1)
C
               ZR(IJ1  ) = ZR(IJ1  ) + POIDS/A*C1*
     &         (A22*A22*DFDX(II)*DFDX(JJ)+A21*A21*DFDY(II)*DFDY(JJ)+
     &          A21*A22*(DFDX(JJ)*DFDY(II)+DFDX(II)*DFDY(JJ))+
     &          (A11*A11*DFDY(II)*DFDY(JJ)+A12*A12*DFDX(II)*DFDX(JJ)+
     &          A11*A12*(DFDY(JJ)*DFDX(II)+DFDX(JJ)*DFDY(II)))*C3)
C
               ZR(IJ2  ) = ZR(IJ2  ) + POIDS/A*C1*(
     &          C2*(A22*A11*DFDX(JJ)*DFDY(II)+A22*A12*DFDX(JJ)*DFDX(II)
     &          +A21*A11*DFDY(JJ)*DFDY(II)+A21*A12*DFDY(JJ)*DFDX(II))+
     &          C3*(A11*A22*DFDY(JJ)*DFDX(II)+A11*A21*DFDY(JJ)*DFDY(II)
     &          +A12*A22*DFDX(JJ)*DFDX(II)+A12*A21*DFDX(JJ)*DFDY(II)))
C
               ZR(IJ2+1) = ZR(IJ2+1) + POIDS/A*C1*
     &         (A11*A11*DFDY(JJ)*DFDY(II)+A12*A12*DFDX(II)*DFDX(JJ)+
     &          A11*A12*(DFDY(JJ)*DFDX(II)+DFDX(JJ)*DFDY(II))+
     &          (A22*A22*DFDX(II)*DFDX(JJ)+A21*A21*DFDY(JJ)*DFDY(II)+
     &          A22*A21*(DFDX(JJ)*DFDY(II)+DFDY(JJ)*DFDX(II)))*C3)
C
107          CONTINUE
             DO 108 J=1,I-2,2
               JJ = (J+1)/2
               IJ1=IMATUU+KD1+J-2
               VFI=ZR(IVF+K+II-1)
               VFJ=ZR(IVF+K+JJ-1)
C
               ZR(IJ1+1) = ZR(IJ1+1) + POIDS/A*C1*(
     &          C2*(A11*A22*DFDX(II)*DFDY(JJ)+A11*A21*DFDY(JJ)*DFDY(II)
     &          +A12*A22*DFDX(JJ)*DFDX(II)+A12*A21*DFDX(JJ)*DFDY(II))+
     &          C3*(A11*A22*DFDX(JJ)*DFDY(II)+A22*A12*DFDX(II)*DFDX(JJ)
     &          +A21*A11*DFDY(JJ)*DFDY(II)+A21*A12*DFDY(JJ)*DFDX(II)))
C
108          CONTINUE
106        CONTINUE
        ENDIF
101   CONTINUE
      END
