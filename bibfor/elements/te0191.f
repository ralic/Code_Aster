      SUBROUTINE TE0191 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/01/2012   AUTEUR PELLET J.PELLET 
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
C
C    - FONCTION REALISEE:  CALCUL DES MATRICES DE MASSE ELEMENTAIRES
C                          POUR LES ELEMENTS DE FOURIER
C                          OPTION : 'MASS_MECA       '
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
      CHARACTER*8 FAMI,POUM
      CHARACTER*16  PHENOM
      INTEGER ICODRE
      REAL*8        A(3,3,9,9),DFDX(9),DFDY(9),POIDS,R,R8B,RHO
      REAL*8        MATP(27,27), MATV(378)
      INTEGER      NNO,KP,NNOS,NPG2,I,J,K,L,IMATUU,NDDL,NVEC,IACCE,IVECT
      INTEGER      IPOIDS,IVF,IDFDE,IGEOM,IMATE,IJKL,IK,KPG,SPT
      INTEGER      NDIM,JGANO
C ......................................................................
C
      CALL ELREF4(' ','MASS',NDIM,NNO,NNOS,NPG2,IPOIDS,IVF,IDFDE,JGANO)
      NDDL = 3 * NNO
      NVEC = NDDL * ( NDDL + 1 ) / 2
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
C
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,ICODRE)
      CALL RCVALB(FAMI,KPG,SPT,POUM,ZI(IMATE),' ',PHENOM,0,' ',R8B,1,
     &            'RHO',RHO,ICODRE,1)
C
      DO 113 K=1,3
        DO 113 L=1,3
          DO 113 I=1,NNO
            DO 113 J=1,I
              A(K,L,I,J) = 0.0D0
113   CONTINUE
C
C    BOUCLE SUR LES POINTS DE GAUSS
C
      DO 101 KP=1,NPG2
        K=(KP-1)*NNO
        CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
C
        R = 0.0D0
        DO 102 I=1,NNO
           R = R + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
102     CONTINUE
        POIDS = POIDS*R*RHO
C
        DO 106 I=1,NNO
           DO 107 J=1,I
              A(1,1,I,J) = A(1,1,I,J) +
     &                      POIDS * ZR(IVF+K+I-1) * ZR(IVF+K+J-1)
107        CONTINUE
106     CONTINUE
101   CONTINUE
C
      DO 108 I=1,NNO
         DO 109 J=1,I
            A(2,2,I,J) = A(1,1,I,J)
            A(3,3,I,J) = A(1,1,I,J)
109      CONTINUE
108   CONTINUE
C
      IF ( OPTION .EQ. 'MASS_MECA' ) THEN
C
         CALL JEVECH('PMATUUR','E',IMATUU)
C
C PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
C
         DO 112 K=1,3
           DO 112 L=1,3
              DO 112 I=1,NNO
                IK = ((3*I+K-4) * (3*I+K-3)) / 2
                DO 112 J=1,I
                  IJKL = IK + 3 * (J-1) + L
                  ZR(IMATUU+IJKL-1) = A(K,L,I,J)
112      CONTINUE
C
      ELSEIF ( OPTION .EQ. 'M_GAMMA' ) THEN
C
         CALL JEVECH('PACCELR','L',IACCE)
         CALL JEVECH('PVECTUR','E',IVECT)
         DO 210 K = 1,NVEC
            MATV(K) = 0.0D0
 210     CONTINUE
         DO 212 K = 1,3
            DO 212 L = 1,3
               DO 214 I = 1,NNO
                  IK = ((3*I+K-4) * (3*I+K-3)) / 2
                  DO 216 J=1,I
                     IJKL = IK + 3 * (J-1) + L
                     MATV(IJKL) = A(K,L,I,J)
 216              CONTINUE
 214           CONTINUE
 212     CONTINUE
         CALL VECMA(MATV,NVEC,MATP,NDDL)
         CALL PMAVEC('ZERO',NDDL,MATP,ZR(IACCE),ZR(IVECT))
C
      ELSE
CC OPTION DE CALCUL INVALIDE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      END
