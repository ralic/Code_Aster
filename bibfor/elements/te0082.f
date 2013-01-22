      SUBROUTINE TE0082 ( OPTION , NOMTE )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/01/2013   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    - CALCULE DES MATRICES ELEMENTAIRES
C                          OPTION : 'MASS_MECA'
C    - CALCULE DES VECTEURS ELEMENTAIRES
C                          OPTION : 'M_GAMMA'
C    - CALCULE DES GRANDEURS ELEMENTAIRES
C                          OPTION : 'ECIN_ELEM'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
C
      CHARACTER*16       PHENOM
      CHARACTER*1        STOPZ(3)
      INTEGER ICODRE
C      CHARACTER*4        FAMI
      REAL*8             VALRES, DFDX(9),DFDY(9),POIDS,R,R8B,VFI,VFJ
      REAL*8             MATP(18,18), MATV(171),MASVIT(18),MASDEP(18)
      REAL*8             DDOT
      REAL*8             VECT1(18), VECT2(18)
      INTEGER            NNO,KP,NNOS,NPG2,II,JJ,I,J,K,IMATUU,JGANO
      INTEGER            L,N1,N2,I2,J2
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,IMATE
      INTEGER            KD1,KD2,IJ1,IJ2,NDDL,NVEC,IACCE,IVECT,NDIM
      INTEGER            IDEPL,IVITE,IFREQ,IECIN
      LOGICAL            LTEATT
      INTEGER            MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5),IBI
      INTEGER            IDEC,IRET
C ......................................................................
C
      CALL ELREF4(' ','MASS',NDIM,NNO,NNOS,NPG2,IPOIDS,IVF,IDFDE,JGANO)
      NDDL = 2 * NNO
      NVEC = NDDL * ( NDDL + 1 ) / 2
      PRESS1(1) = 0
      PRESS2(1) = 0
      TEMPE(1) = 0
      CALL GRDTHM(NOMTE,.FALSE.,.FALSE.,2,MECANI,PRESS1,PRESS2,TEMPE,
     &            IBI,IBI,IBI,IBI,IBI,IBI)
      IDEC = PRESS1(1) + PRESS2(1) + TEMPE(1)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
C
      CALL RCCOMA(ZI(IMATE),'ELAS',1,PHENOM,ICODRE)
      CALL RCVALB('FPG1',1,1,'+',ZI(IMATE),' ',PHENOM,0,' ',R8B,
     &              1,'RHO',VALRES, ICODRE,1)
C
      DO 2 K = 1,NVEC
         MATV(K) = 0.0D0
 2    CONTINUE
C
      DO 10 KP=1,NPG2
         K = (KP-1)*NNO
         CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
         IF ( LTEATT(' ','AXIS','OUI') ) THEN
            R = 0.0D0
            DO 20 I=1,NNO
               R = R + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
 20         CONTINUE
            POIDS = POIDS*R
         ENDIF
         POIDS = POIDS*VALRES
C
         KD1 = 2
         KD2 = 1
         DO 30 I=1,2*NNO,2
            KD1 = KD1+2*I-3
            KD2 = KD2+2*I-1
            II  = (I+1)/2
            DO 40 J=1,I,2
               JJ  = (J+1)/2
               IJ1 = KD1+J-1
               IJ2 = KD2+J-1
               VFI = ZR(IVF+K+II-1)
               VFJ = ZR(IVF+K+JJ-1)
               MATV(IJ1  ) = MATV(IJ1  ) + POIDS*VFI*VFJ
               MATV(IJ2+1) = MATV(IJ2+1) + POIDS*VFI*VFJ
 40         CONTINUE
 30      CONTINUE
 10   CONTINUE
C
      IF ( OPTION .EQ. 'MASS_MECA' ) THEN
         CALL JEVECH('PMATUUR','E',IMATUU)
         IF (IDEC.EQ.0) THEN
           DO 100 I = 1 , NVEC
              ZR(IMATUU+I-1) = MATV(I)
 100       CONTINUE
         ELSE
           DO 101 K = 1 , NNO
             DO 102 N1 = 1 , 2
               I = 2*K+N1-2
               IF (K.LE.NNOS) THEN
                 I2 = I+IDEC*(K-1)
               ELSE
                 I2 = I+IDEC*NNOS
               ENDIF
               DO 103 L = 1 , NNO
                 DO 104 N2 = 1 , 2
                   J = 2*L+N2-2
                   IF (J.GT.I) GOTO 105
                   IF (L.LE.NNOS) THEN
                     J2 = J+IDEC*(L-1)
                   ELSE
                     J2 = J+IDEC*NNOS
                   ENDIF
                   ZR(IMATUU+I2*(I2-1)/2+J2-1) = MATV(I*(I-1)/2+J)
 104             CONTINUE
 103           CONTINUE
 105           CONTINUE
 102         CONTINUE
 101       CONTINUE
         ENDIF
C
      ELSEIF ( OPTION .EQ. 'M_GAMMA' ) THEN
         CALL JEVECH('PACCELR','L',IACCE)
         CALL JEVECH('PVECTUR','E',IVECT)
         CALL VECMA(MATV,NVEC,MATP,NDDL)
         IF (IDEC.EQ.0) THEN
           CALL PMAVEC('ZERO',NDDL,MATP,ZR(IACCE),ZR(IVECT))
         ELSE
           DO 120 K = 1,NDDL
             VECT1(K) = 0.0D0
             VECT2(K) = 0.0D0
 120       CONTINUE
           DO 111 K = 1 , NNO
             DO 112 N1 = 1 , 2
               I = 2*K+N1-2
               IF (K.LE.NNOS) THEN
                 I2 = I+IDEC*(K-1)
               ELSE
                 I2 = I+IDEC*NNOS
               ENDIF
               VECT1(I) = ZR(IACCE+I2-1)
 112         CONTINUE
 111       CONTINUE
           CALL PMAVEC('ZERO',NDDL,MATP,VECT1,VECT2)
           DO 113 K = 1 , NNO
             DO 114 N1 = 1 , 2
               I = 2*K+N1-2
               IF (K.LE.NNOS) THEN
                 I2 = I+IDEC*(K-1)
               ELSE
                 I2 = I+IDEC*NNOS
               ENDIF
               ZR(IVECT+I2-1) = VECT2(I)
 114         CONTINUE
 113       CONTINUE
         ENDIF

C OPTION ECIN_ELEM : CALCUL DE L'ENERGIE CINETIQUE

      ELSEIF ( OPTION .EQ. 'ECIN_ELEM' ) THEN
        STOPZ(1)='O'
        STOPZ(2)='N'
        STOPZ(3)='O'
        CALL TECACH(STOPZ,'PVITESR','L',1,IVITE,IRET)
C IRET NE PEUT VALOIR QUE 0 (TOUT EST OK) OU 2 (CHAMP NON FOURNI)
        IF (IRET.EQ.0) THEN
          CALL JEVECH('PENERCR','E',IECIN)
          CALL VECMA(MATV,NVEC,MATP,NDDL)
          CALL PMAVEC('ZERO',NDDL,MATP,ZR(IVITE),MASVIT)
          ZR(IECIN) = .5D0*DDOT(NDDL,ZR(IVITE),1,MASVIT,1)
        ELSE
          CALL TECACH(STOPZ,'PDEPLAR','L',1,IDEPL,IRET)
          IF (IRET.EQ.0) THEN
            CALL JEVECH('PENERCR','E',IECIN)
            CALL JEVECH('POMEGA2','L',IFREQ)
            CALL VECMA(MATV,NVEC,MATP,NDDL)
            CALL PMAVEC('ZERO',NDDL,MATP,ZR(IDEPL),MASDEP)
            ZR(IECIN) = .5D0*DDOT(NDDL,ZR(IDEPL),1,MASDEP,1)*ZR(IFREQ)
          ELSE
            CALL U2MESK('F','ELEMENTS2_1',1,OPTION)
          ENDIF
        ENDIF

      ELSE
CC OPTION DE CALCUL INVALIDE
        CALL ASSERT(.FALSE.)
      END IF
C
      END
