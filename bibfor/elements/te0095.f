      SUBROUTINE TE0095(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT NONE

C.......................................................................
C FONCTION REALISEE:

C      CALCUL
C      DE LA FORME BILINEAIRE SYMETRIQUE G(U,V)
C      POUR LES ELEMENTS ISOPARAMETRIQUES 3D

C      OPTION : 'G_BILI','G_BILI_F'

C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................

      INCLUDE 'jeveux.h'
      INTEGER ICODRE(3)
      INTEGER ICORSV(3)
      CHARACTER*8 NOMRES(3),NOMPAR(4)
      CHARACTER*16 NOMTE,OPTION,PHENOM

      REAL*8 EPSI,R8PREM
      REAL*8 DFDI(81),F(3,3),EPS(6),FNOU(81),FNOV(81)
      REAL*8 DUDM(3,4),DVDM(3,4)
      REAL*8 DFUDM(3,4),DFVDM(3,4),DER(4),DTDM(3,4)
      REAL*8 RBID,E,NU,ALPHA,TREF,TTRGU,TTRGV,K3A
      REAL*8 THET,TGU,TGV
      REAL*8 XG,YG
      REAL*8 C1,C2,C3,TGM,TGP
      REAL*8 VALRES(3),DEVRES(3),VALRSV(3),DEVRSV(3)
      REAL*8 VAPARU(4),VAPARV(4)
      REAL*8 GELEM,GUV3,G,POIDS, PULS
      REAL*8 TGUDM(3),TGVDM(3)
      REAL*8 RHO,OM,OMO

      INTEGER JGANO,IPOIDS,IVF,IDFDE,NNO,KP,NPG1,COMPT
      INTEGER IGEOM,ITHET,IFIC,IDEPU,IDEPV
      INTEGER IMATE,K,I,J,L,NDIM,NNOS
      INTEGER IFORFU,IFORFV,IFORCU,IFORCV
      INTEGER IRET,IEPSRU,IEPSRV,IRET0,IRET1,IRET2,IRET3
      INTEGER IPESAU,IPESAV,IROTAU,IROTAV,ITMPSU,ITMPSV

      INTEGER KK,IER
      LOGICAL FONC


      CALL JEMARQ()
      EPSI = R8PREM()

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
      CALL JEVECH('PTHETAR','L',ITHET)


C - PAS DE CALCUL DE G POUR LES ELEMENTS OU LA VALEUR DE THETA EST NULLE

      COMPT = 0
      DO 30 I = 1,NNO
        THET = 0.D0
        DO 20 J = 1,NDIM
          THET = THET + ABS(ZR(ITHET+NDIM* (I-1)+J-1))
   20   CONTINUE
        IF (THET.LT.EPSI) COMPT = COMPT + 1
   30 CONTINUE
      IF (COMPT.EQ.NNO) GO TO 100
C
C RECUPERATION CHARGE, MATER...
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAU','L',IDEPU)
      CALL JEVECH('PDEPLAV','L',IDEPV)
      CALL JEVECH('PMATERC','L',IMATE)
C
      CALL TECACH('ONN','UPESANR',1,IPESAU,IRET)
      CALL TECACH('ONN','UROTATR',1,IROTAU,IRET)
      CALL TECACH('ONN','VPESANR',1,IPESAV,IRET)
      CALL TECACH('ONN','VROTATR',1,IROTAV,IRET)
C

      IF (OPTION .EQ. 'G_BILI_F') THEN
        FONC = .TRUE.
        CALL JEVECH('UPFFVOL','L',IFORFU)
        CALL JEVECH('VPFFVOL','L',IFORFV)

        CALL JEVECH('UTEMPSR','L',ITMPSU)
        CALL JEVECH('VTEMPSR','L',ITMPSV)
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'Z'
        NOMPAR(4) = 'INST'
        VAPARU(4) = ZR(ITMPSU)
        VAPARV(4) = ZR(ITMPSV)
        CALL TECACH('ONN','UEPSINF',1,IEPSRU,IRET)
        CALL TECACH('ONN','VEPSINF',1,IEPSRV,IRET)
      ELSE
        FONC = .FALSE.
        CALL JEVECH('UPFRVOL','L',IFORCU)
        CALL JEVECH('VPFRVOL','L',IFORCV)
        CALL TECACH('ONN','UEPSINR',1,IEPSRU,IRET)
        CALL TECACH('ONN','VEPSINR',1,IEPSRV,IRET)
      END IF

      CALL JEVECH('PGTHETA','E',IFIC)

      GUV3 = 0.D0
      CALL RCVARC(' ','TEMP','REF','NOEU',1,1,TREF,IRET0)
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'ALPHA'
C
C - RECUPERATION DES CHARGES ET DEFORMATIONS INITIALES ----------------
C
      IF (FONC) THEN
        DO 150 I = 1,NNO
          DO 130 J = 1,NDIM
            VAPARU(J) = ZR(IGEOM+NDIM*(I-1)+J-1)
            VAPARV(J) = ZR(IGEOM+NDIM*(I-1)+J-1)
 130      CONTINUE
          DO 140 J = 1,NDIM
            KK = NDIM*(I-1) + J
            CALL FOINTE('FM',ZK8(IFORFU+J-1),3,
     &                  NOMPAR,VAPARU,FNOU(KK),IER)
            CALL FOINTE('FM',ZK8(IFORFV+J-1),3,
     &                  NOMPAR,VAPARV,FNOV(KK),IER)
 140      CONTINUE
 150    CONTINUE
      ELSE
        DO 8000 I = 1,NNO
          DO 6000 J = 1,NDIM
            FNOU(NDIM*(I-1)+J) = ZR(IFORCU+NDIM*(I-1)+J-1)
            FNOV(NDIM*(I-1)+J) = ZR(IFORCV+NDIM*(I-1)+J-1)
 6000     CONTINUE
 8000   CONTINUE
      END IF
C
      IF ((IPESAU.NE.0) .OR. (IROTAU.NE.0)) THEN
        CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,ICODRE)
        CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ',PHENOM,
     &              1,' ',RBID,1,'RHO',
     &              RHO,ICODRE,1)
        IF (IPESAU .NE. 0) THEN
          DO 160 I = 1,NNO
            DO 161 J = 1,NDIM
              KK = NDIM*(I-1) + J
              FNOU(KK) = FNOU(KK) + RHO*ZR(IPESAU)*ZR(IPESAU+J)
 161        CONTINUE
 160      CONTINUE
        END IF
        IF (IROTAU .NE. 0) THEN
          OM = ZR(IROTAU)
          DO 170 I = 1,NNO
            OMO = 0.D0
            DO 171 J = 1,NDIM
              OMO = OMO + ZR(IROTAU+J)*ZR(IGEOM+NDIM*(I-1)+J-1)
 171        CONTINUE
            DO 172 J = 1,NDIM
              KK = NDIM*(I-1) + J
              FNOU(KK) =
     &          FNOU(KK) + RHO*OM*OM*(ZR(IGEOM+KK-1)-OMO*ZR(IROTAU+J))
 172        CONTINUE
 170      CONTINUE
        END IF
      END IF
C
      IF ((IPESAV.NE.0) .OR. (IROTAV.NE.0)) THEN
        CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,ICODRE)
        CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ',PHENOM,
     &              1,' ',RBID,1,'RHO',
     &              RHO,ICODRE,1)
        IF (IPESAV .NE. 0) THEN
          DO 260 I = 1,NNO
            DO 261 J = 1,NDIM
              KK = NDIM*(I-1) + J
              FNOV(KK) = FNOV(KK) + RHO*ZR(IPESAV)*ZR(IPESAV+J)
 261        CONTINUE
 260      CONTINUE
        END IF
        IF (IROTAV .NE. 0) THEN
          OM = ZR(IROTAV)
          DO 270 I = 1,NNO
            OMO = 0.D0
            DO 271 J = 1,NDIM
              OMO = OMO + ZR(IROTAV+J)*ZR(IGEOM+NDIM*(I-1)+J-1)
 271        CONTINUE
            DO 272 J = 1,NDIM
              KK = NDIM*(I-1) + J
              FNOV(KK) =
     &          FNOV(KK) + RHO*OM*OM*(ZR(IGEOM+KK-1)-OMO*ZR(IROTAV+J))
 272        CONTINUE
 270      CONTINUE
        END IF
      END IF
C
C ======================================================================
C - BOUCLE SUR LES POINTS DE GAUSS
C
      DO 90 KP = 1,NPG1
        L = (KP-1)*NNO
        TGU = 0.D0
        TGV = 0.D0
        XG = 0.D0
        YG = 0.D0
        DO 50 I = 1,3
          TGUDM(I) = 0.D0
          TGVDM(I) = 0.D0
          DO 40 J = 1,4
            DUDM(I,J) = 0.D0
            DVDM(I,J) = 0.D0
            DTDM(I,J) = 0.D0
            DFUDM(I,J) = 0.D0
            DFVDM(I,J) = 0.D0
   40     CONTINUE
   50   CONTINUE
C
C - CALCUL DES ELEMENTS GEOMETRIQUES

        CALL NMGEOM(NDIM,NNO,.FALSE.,.FALSE.,ZR(IGEOM),KP,
     &              IPOIDS,IVF,IDFDE,
     &              ZR(IDEPU),.TRUE.,POIDS,DFDI,F,EPS,RBID)
C
C - CALCULS DES GRADIENTS DE U ET V (DUDM ET DVDM),THETA (DTDM),
C   FU ET FV (DFUDM ET DFVDM)
C   CALCUL DES CHAMPS DE TEMPERATURE (TGU ET TGV) ET DE LEURS GRADIENTS
C   (TGUDM ET TGVDM)AUX POINTS DE GAUSS
C
        IRET3 = 0
        DO 80 I = 1,NNO
          DER(1) = DFDI(I)
          DER(2) = DFDI(I+NNO)
          DER(3) = DFDI(I+2*NNO)
          DER(4) = ZR(IVF+L+I-1)
          CALL RCVARC(' ','TEMP','-','NOEU',I,1,TGM,IRET1)
          CALL RCVARC(' ','TEMP','+','NOEU',I,1,TGP,IRET2)
          IF ((IRET1+IRET2).EQ.0) THEN
            IF (IRET0.EQ.1)  CALL U2MESS('F','CALCULEL_31')
            TGU = TGU + TGM*DER(4)
            TGV = TGV + TGP*DER(4)
            DO 75 J = 1,NDIM
              TGUDM(J) = TGUDM(J) + TGM*DER(J)
              TGVDM(J) = TGVDM(J) + TGP*DER(J)
  75        CONTINUE
          ELSE
            IRET3 = IRET3+1
            TGU=0.D0
            TGV=0.D0
          ENDIF

          XG = XG + ZR(IGEOM+2* (I-1))*DER(4)
          YG = YG + ZR(IGEOM+2* (I-1)+1)*DER(4)
          DO 70 J = 1,NDIM
            DO 60 K = 1,NDIM
              DUDM(J,K) = DUDM(J,K) + ZR(IDEPU+NDIM* (I-1)+J-1)*DER(K)
              DVDM(J,K) = DVDM(J,K) + ZR(IDEPV+NDIM* (I-1)+J-1)*DER(K)
              DTDM(J,K) = DTDM(J,K) + ZR(ITHET+NDIM* (I-1)+J-1)*DER(K)
              DFUDM(J,K) = DFUDM(J,K) + FNOU(NDIM*(I-1)+J)*DER(K)
              DFVDM(J,K) = DFVDM(J,K) + FNOV(NDIM*(I-1)+J)*DER(K)
   60       CONTINUE
            DUDM(J,4) = DUDM(J,4) + ZR(IDEPU+NDIM*(I-1)+J-1)*DER(4)
            DVDM(J,4) = DVDM(J,4) + ZR(IDEPV+NDIM*(I-1)+J-1)*DER(4)
            DTDM(J,4) = DTDM(J,4) + ZR(ITHET+NDIM*(I-1)+J-1)*DER(4)
            DFUDM(J,4) = DFUDM(J,4) + FNOU(NDIM*(I-1)+J)*DER(4)
            DFVDM(J,4) = DFVDM(J,4) + FNOV(NDIM*(I-1)+J)*DER(4)
   70     CONTINUE
   80   CONTINUE

C - RECUPERATION DES DONNEES MATERIAUX
        IF (IRET3.EQ.0) THEN
          TTRGU = TGU - TREF
          TTRGV = TGV - TREF
        ELSE
          DO 85 J = 1,NDIM
            TGUDM(J) = 0.D0
            TGVDM(J) = 0.D0
  85      CONTINUE
          TTRGU = 0.D0
          TTRGV = 0.D0
        ENDIF
        CALL RCVADA(ZI(IMATE),'ELAS',TGU,3,NOMRES,VALRES,DEVRES,ICODRE)
        CALL RCVADA(ZI(IMATE),'ELAS',TGV,3,NOMRES,VALRSV,DEVRSV,ICORSV)
        IF (IRET0.EQ.0) THEN
          IF ((ICODRE(3) .NE.0).OR.((ICORSV(3) .NE.0)))  THEN
            CALL U2MESS('F','CALCULEL_31')
          ENDIF
        ELSE
            VALRES(3) = 0.D0
            DEVRES(3) = 0.D0
            VALRSV(3) = 0.D0
            DEVRSV(3) = 0.D0
        ENDIF
        IF ((VALRES(1) .NE. VALRSV(1)).OR.((VALRES(2) .NE. VALRSV(2)))
     &      .OR.(VALRES(3) .NE. VALRSV(3))) THEN
          CALL U2MESS('F','ELEMENTS3_12')
        END IF
        E = VALRES(1)
        NU = VALRES(2)
        ALPHA = VALRES(3)
        K3A = ALPHA * E / (1.D0-2.D0*NU)
        C3 = E/ (2.D0* (1.D0+NU))
        C1 = E* (1.D0-NU)/ ((1.D0+NU)* (1.D0-2.D0*NU))
        C2 = NU/ (1.D0-NU)*C1
C
        GELEM = 0.D0
C PAS DE TERME DYNAMIQUE DANS GBIL
        PULS = 0.D0
        RHO = 0.D0
        CALL GBIL3D(DUDM,DVDM,DTDM,DFUDM,DFVDM,TGUDM,TGVDM,
     &              TTRGU,TTRGV,POIDS,C1,C2,C3,K3A,RHO,PULS,GELEM)
        GUV3 = GUV3 + GELEM
   90 CONTINUE

      G = GUV3

      ZR(IFIC) = G
  100 CONTINUE
      CALL JEDEMA()
      END
