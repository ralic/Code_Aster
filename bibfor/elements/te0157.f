      SUBROUTINE TE0157 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/11/2009   AUTEUR DESROCHES X.DESROCHES 
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
C     CALCUL DE L' OPTION: 'MASS_INER' ELEMENTS FLUIDES
C                                       2-D AXI D-PLAN, C-PLAN
C
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C
C     ------------------------------------------------------------------
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
      INTEGER            NBRES,NNO,KP,NNOS,NPG2,I,J,K,LCASTR
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,IMATE
      INTEGER            NDIM,JGANO
      PARAMETER         ( NBRES=2 )
      REAL*8             VALRES(NBRES)
      REAL*8             RHO, XG, YG, DEPI, R8DEPI, ZERO
      REAL*8             DFDX(9),DFDY(9),POIDS,R,X(9),Y(9),VOLUME
      REAL*8             MATINE(6), XXI, YYI, XYI, R8BID, R8PREM
      CHARACTER*2        CODRET(NBRES)
      CHARACTER*8        NOMRES(NBRES)
      LOGICAL            LTEATT
C     ------------------------------------------------------------------
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG2,IPOIDS,IVF,IDFDE,JGANO)
C
      ZERO = 0.D0
      DEPI = R8DEPI()
C
      CALL JEVECH('PMATERC','L',IMATE)
C
      NOMRES(1) = 'RHO'
      NOMRES(2) = 'CELE_R'
      CALL RCVALA ( ZI(IMATE),' ','FLUIDE',0,' ',R8BID,2,NOMRES,VALRES,
     &              CODRET, 'FM' )
      RHO    = VALRES(1)
      IF(RHO.LE.R8PREM()) THEN
         CALL U2MESS('F','ELEMENTS5_45')
      ENDIF
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      DO 10 I=1,NNO
         X(I) =  ZR(IGEOM-2+2*I)
         Y(I) =  ZR(IGEOM-1+2*I)
 10   CONTINUE
C
      CALL JEVECH('PMASSINE','E',LCASTR)
      DO 20 I = 0,3
         ZR(LCASTR+I) = ZERO
 20   CONTINUE
      DO 22 I = 1,6
         MATINE(I) = ZERO
 22   CONTINUE
C
C     --- BOUCLE SUR LES POINTS DE GAUSS ---
      VOLUME = ZERO
      DO 100 KP = 1,NPG2
         K = (KP-1) * NNO
         CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
         IF ( LTEATT(' ','AXIS','OUI') ) THEN
            R = ZERO
            DO 102 I = 1,NNO
                R = R + ZR(IGEOM-2+2*I)*ZR(IVF+K+I-1)
102         CONTINUE
            POIDS = POIDS*R
         ENDIF
         VOLUME = VOLUME + POIDS
         DO 104 I = 1,NNO
C           --- CDG ---
            ZR(LCASTR+1) = ZR(LCASTR+1)+POIDS*X(I)*ZR(IVF+K+I-1)
            ZR(LCASTR+2) = ZR(LCASTR+2)+POIDS*Y(I)*ZR(IVF+K+I-1)
C           --- INERTIE ---
            XXI = 0.D0
            XYI = 0.D0
            YYI = 0.D0
            DO 106 J =  1,NNO
               XXI = XXI + X(I)*ZR(IVF+K+I-1)*X(J)*ZR(IVF+K+J-1)
               XYI = XYI + X(I)*ZR(IVF+K+I-1)*Y(J)*ZR(IVF+K+J-1)
               YYI = YYI + Y(I)*ZR(IVF+K+I-1)*Y(J)*ZR(IVF+K+J-1)
 106        CONTINUE
            MATINE(1) = MATINE(1) + POIDS*YYI
            MATINE(2) = MATINE(2) + POIDS*XYI
            MATINE(3) = MATINE(3) + POIDS*XXI
 104     CONTINUE
 100  CONTINUE
C
      IF ( LTEATT(' ','AXIS','OUI') ) THEN
         YG = ZR(LCASTR+2) / VOLUME
         ZR(LCASTR)   = DEPI * VOLUME * RHO
         ZR(LCASTR+3) = YG
         ZR(LCASTR+1) = ZERO
         ZR(LCASTR+2) = ZERO
C
C        --- ON DONNE LES INERTIES AU CDG ---
         MATINE(6) = MATINE(3) * RHO * DEPI
         MATINE(1) = MATINE(1) * RHO * DEPI + MATINE(6)/2.D0
     +                                      - ZR(LCASTR)*YG*YG
         MATINE(2) = ZERO
         MATINE(3) = MATINE(1)
C
      ELSE
         ZR(LCASTR)   = VOLUME * RHO
         ZR(LCASTR+1) = ZR(LCASTR+1) / VOLUME
         ZR(LCASTR+2) = ZR(LCASTR+2) / VOLUME
         ZR(LCASTR+3) = ZERO
C
C        --- ON DONNE LES INERTIES AU CDG ---
         XG = ZR(LCASTR+1)
         YG = ZR(LCASTR+2)
         MATINE(1) = MATINE(1)*RHO - ZR(LCASTR)*YG*YG
         MATINE(2) = MATINE(2)*RHO - ZR(LCASTR)*XG*YG
         MATINE(3) = MATINE(3)*RHO - ZR(LCASTR)*XG*XG
         MATINE(6) = MATINE(1) + MATINE(3)
      ENDIF
      ZR(LCASTR+4) = MATINE(1)
      ZR(LCASTR+5) = MATINE(3)
      ZR(LCASTR+6) = MATINE(6)
      ZR(LCASTR+7) = MATINE(2)
      ZR(LCASTR+8) = MATINE(4)
      ZR(LCASTR+9) = MATINE(5)
C
      END
