      SUBROUTINE TE0082 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
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
C
C    - CALCULE DES MATRICES ELEMENTAIRES
C                          OPTION : 'MASS_MECA'
C    - CALCULE DES VECTEURS ELEMENTAIRES
C                          OPTION : 'M_GAMMA'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
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
      CHARACTER*16       PHENOM
      CHARACTER*2        CODRET
      REAL*8             VALRES, DFDX(9),DFDY(9),POIDS,R,R8B,VFI,VFJ
      REAL*8             MATP(18,18), MATV(171),MASVIT(18)
      REAL*8             DDOT
      INTEGER            NNO,KP,NNOS,NPG2,II,JJ,I,J,K,IMATUU,JGANO
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,IMATE
      INTEGER            KD1,KD2,IJ1,IJ2,NDDL,NVEC,IACCE,IVECT,NDIM
      INTEGER            IVITE,IFREQ,IECIN
      LOGICAL            LTEATT
C ......................................................................
C
      CALL ELREF4(' ','MASS',NDIM,NNO,NNOS,NPG2,IPOIDS,IVF,IDFDE,JGANO)
      NDDL = 2 * NNO
      NVEC = NDDL * ( NDDL + 1 ) / 2
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
C
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
      CALL RCVALA ( ZI(IMATE),' ',PHENOM,0,' ',R8B,1,'RHO',VALRES,
     &              CODRET,'FM')
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
         DO 100 I = 1 , NVEC
            ZR(IMATUU+I-1) = MATV(I)
 100     CONTINUE
C
      ELSEIF ( OPTION .EQ. 'M_GAMMA' ) THEN
         CALL JEVECH('PDEPLAR','L',IACCE)
         CALL JEVECH('PVECTUR','E',IVECT)
         CALL VECMA(MATV,NVEC,MATP,NDDL)
         CALL PMAVEC('ZERO',NDDL,MATP,ZR(IACCE),ZR(IVECT))

C OPTION ECIN_ELEM_DEPL : CALCUL DE L'ENERGIE CINETIQUE

      ELSEIF ( OPTION .EQ. 'ECIN_ELEM_DEPL' ) THEN
         CALL JEVECH('PDEPLAR','L',IVITE)
         CALL JEVECH('PENERCR','E',IECIN)
         CALL JEVECH('PFREQR','E',IFREQ)
         CALL VECMA(MATV,NVEC,MATP,NDDL)
         CALL PMAVEC('ZERO',NDDL,MATP,ZR(IVITE),MASVIT)

         ZR(IECIN) = .5D0*DDOT(NDDL,ZR(IVITE),1,MASVIT,1)*ZR(IFREQ)

      ELSE
CC OPTION DE CALCUL INVALIDE
        CALL ASSERT(.FALSE.)
      END IF
C
      END
