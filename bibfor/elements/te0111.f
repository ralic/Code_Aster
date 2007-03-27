      SUBROUTINE TE0111 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 28/03/2007   AUTEUR PELLET J.PELLET 
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
C    - BUT :  CALCUL DES MATRICES DE RAIDEUR CENTRIFUGE ELEMENTAIRES
C                          POUR LES ELEMENTS DE FOURIER
C                          OPTION : 'RIGI_MECA_RO    '
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*2        CODRET
      CHARACTER*4        FAMI
      REAL*8             A(3,3,9,9), DFDX(9),DFDY(9),POIDS,R
      INTEGER            NNO,KP,NPG2,I,J,IMATUU,NNOS,NDIM,JGANO
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,IMATE
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
      FAMI = 'MASS'
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG2,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PROTATR','L',IROTA)
      CALL JEVECH('PMATUUR','E',IMATUU)
C
      OMEGA1 = ZR(IROTA+1) * ZR(IROTA)
      OMEGA2 = ZR(IROTA+2) * ZR(IROTA)
      OMEGA3 = ZR(IROTA+3) * ZR(IROTA)
C
      DO 113 K=1,3
         DO 113 L=1,3
            DO 113 I=1,NNO
            DO 113 J=1,I
               A(K,L,I,J) = 0.D0
113   CONTINUE
C
C    BOUCLE SUR LES POINTS DE GAUSS
C
      DO 101 KP=1,NPG2
C
        K=(KP-1)*NNO
        CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
C
        R   = 0.D0
        DO 102 I=1,NNO
          R   = R + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
102     CONTINUE
        POIDS = POIDS*R
        CALL RCVALB(FAMI,KP,1,'+',ZI(IMATE),' ','ELAS',0,' ',0.D0,
     &              1,'RHO',RHO,CODRET,'FM')
C
         DO 106 I=1,NNO
C
            DO 107 J=1,I
C
               WIJ = RHO * POIDS * ZR(IVF+L+I-1) * ZR(IVF+L+J-1)
C
               A(1,1,I,J) = A(1,1,I,J) - (OMEGA2**2 + OMEGA3**2) * WIJ
C
               A(2,2,I,J) = A(2,2,I,J) - (OMEGA1**2 + OMEGA3**2) * WIJ
C
               A(3,3,I,J) = A(3,3,I,J) - (OMEGA1**2 + OMEGA2**2) * WIJ
C
               A(2,1,I,J) = A(2,1,I,J) + OMEGA1 * OMEGA2 * WIJ
C
               A(3,1,I,J) = A(3,1,I,J) + OMEGA1 * OMEGA3 * WIJ
C
               A(3,2,I,J) = A(3,2,I,J) + OMEGA2 * OMEGA3 * WIJ
C
107         CONTINUE
C
106      CONTINUE
C
101   CONTINUE
C
           DO 108 I=1,NNO
              DO 109 J=1,I
             A(1,2,I,J) = A(2,1,I,J)
             A(1,3,I,J) = A(3,1,I,J)
             A(2,3,I,J) = A(3,2,I,J)
109        CONTINUE
108        CONTINUE
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
112         CONTINUE
C
      END
