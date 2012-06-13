      SUBROUTINE TUSIEF(OPTION,NOMTE,NBRDDL,B,VIN,MAT,PASS,VTEMP)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
      INCLUDE 'jeveux.h'
      CHARACTER*16 OPTION,NOMTE
C ......................................................................

C   FONCTION REALISEE:  CALCUL DES OPTIONS EPSI_ELGA,
C                                          DEGE_ELGA,
C                                          DEGE_ELNO,
C                                          SIEF_ELGA POUR UN TUYAU
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................

      INTEGER NBRES,NBRDDL
      PARAMETER (NBRES=9)
      CHARACTER*8 NOMRES(NBRES),NOMPAR
      INTEGER ICODRE(NBRES)
      REAL*8 VALRES(NBRES),VALPAR,DEGG(24)
      REAL*8 H,A,L,E,NU,BETA,CISAIL,G,OMEGA,DHK
      REAL*8 SINFI,FI,DEUXPI,R,R8PI,AT1,AT2,VPG(4),VNO(4)
      REAL*8 B(4,NBRDDL),C(4,4),EPSTHE,HK,SIGTH(2),XPG(4)
      REAL*8 PGL(3,3),VIN(NBRDDL),VOUT(4),MAT(4,NBRDDL)
      REAL*8 PGL1(3,3),PGL2(3,3),PGL3(3,3),RAYON,THETA
      REAL*8 VTEMP(NBRDDL),PASS(NBRDDL,NBRDDL),PGL4(3,3)
      INTEGER NNO,NPG,NBCOU,NBSEC,ICOUDE,NDIM,JCOOPG,NSPG
      INTEGER IPOIDS,IVF,KPGS,K,NNOS
      INTEGER IMATE,ICAGEP,NBPAR
      INTEGER IGAU,ICOU,ISECT,I,J,JIN,JOUT,IRET,INDICE
      INTEGER LORIEN,ICOUD2,MMT,JNBSPI
      INTEGER NDDL,M,IDFDK,JDFD2,JGANO


      CALL ELREF5(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,JCOOPG,IVF,IDFDK,
     &            JDFD2,JGANO)

      DEUXPI = 2.D0*R8PI()

      CALL JEVECH('PNBSP_I','L',JNBSPI)
      NBCOU = ZI(JNBSPI-1+1)
      NBSEC = ZI(JNBSPI-1+2)

      CALL JEVECH('PCAGEPO','L',ICAGEP)
C     H = EPAISSEUR, A= RMOY
      H = ZR(ICAGEP+1)
      A = ZR(ICAGEP) - H/2.D0

      M = 3
      IF (NOMTE.EQ.'MET6SEG3') M = 6

      DO 10 I = 1,NPG
        XPG(I) = ZR(JCOOPG-1+I)
   10 CONTINUE

C     --- RECUPERATION DES ORIENTATIONS ---

      CALL JEVECH('PCAORIE','L',LORIEN)
      CALL CARCOU(ZR(LORIEN),L,PGL,RAYON,THETA,PGL1,PGL2,PGL3,PGL4,NNO,
     &            OMEGA,ICOUD2)
      IF (ICOUD2.GE.10) THEN
        ICOUDE = ICOUD2 - 10
        MMT = 0
      ELSE
        ICOUDE = ICOUD2
        MMT = 1
      END IF

      IF (OPTION.EQ.'SIEF_ELGA') THEN

        CALL JEVECH('PMATERC','L',IMATE)
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NOMRES(3) = 'ALPHA'
        NSPG=(2*NBSEC + 1)*(2*NBCOU + 1)
        IRET=0
        CALL MOYTEM('RIGI',NPG,NSPG,'+',VALPAR,IRET)
        IF (IRET.NE.0) VALPAR=0.D0
        NBPAR = 1
        NOMPAR = 'TEMP'
        CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ','ELAS',
     &              NBPAR,NOMPAR,VALPAR,2,NOMRES,
     &              VALRES,ICODRE,1)
        E = VALRES(1)
        NU = VALRES(2)

C        DEFINITION DE LA MATRICE DE COMPORTEMENT C

        BETA = E/ (1.D0-NU**2)
        G = E/ (2.D0* (1.D0+NU))
        CISAIL = 1.D0

        C(1,1) = BETA
        C(1,2) = NU*BETA
        C(1,3) = 0.D0
        C(1,4) = 0.D0

        C(2,1) = NU*BETA
        C(2,2) = BETA
        C(2,3) = 0.D0
        C(2,4) = 0.D0

        C(3,1) = 0.D0
        C(3,2) = 0.D0
        C(3,3) = G
        C(3,4) = 0.D0

        C(4,1) = 0.D0
        C(4,2) = 0.D0
        C(4,3) = 0.D0
        C(4,4) = G*CISAIL

C        FIN DE LA CONSTRUCTION DE LA MATRICE DE COMPORTEMENT C
      END IF

      CALL JEVECH('PDEPLAR','L',JIN)
      DO 20 I = 1,NBRDDL
        VIN(I) = ZR(JIN-1+I)
   20 CONTINUE
      IF (ICOUDE.EQ.0) THEN
        CALL VLGGL(NNO,NBRDDL,PGL,VIN,'GL',PASS,VTEMP)
      ELSE
        CALL VLGGLC(NNO,NBRDDL,PGL1,PGL2,PGL3,PGL4,VIN,'GL',PASS,VTEMP)
      END IF

      IF (OPTION.EQ.'EPSI_ELGA') THEN
        CALL JEVECH('PDEFOPG','E',JOUT)
C
      ELSE IF (OPTION.EQ.'DEGE_ELNO'.OR.
     &          OPTION.EQ.'DEGE_ELGA') THEN

        IF(OPTION.EQ.'DEGE_ELGA') CALL JEVECH('PDEFOPG','E',JOUT)
        IF(OPTION.EQ.'DEGE_ELNO') CALL JEVECH('PDEFOGR','E',JOUT)
        CALL R8INIR(24,0.D0,DEGG,1)
        NBRDDL = NNO* (6+3+6* (M-1))
        NDDL = (6+3+6* (M-1))
C
      ELSE IF (OPTION.EQ.'SIEF_ELGA') THEN
        CALL JEVECH('PCONTRR','E',JOUT)
C
      ELSE
        CALL U2MESK('F','ELEMENTS4_49',1,OPTION)
C
      END IF

      KPGS = 0
      SIGTH(1) = 0.D0
      SIGTH(2) = 0.D0
      NSPG=(2*NBSEC + 1)*(2*NBCOU + 1)

C --- BOUCLE SUR LES POINTS DE GAUSS
C ---- BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR
      DO 120 IGAU = 1,NPG
        IF (OPTION.EQ.'SIEF_ELGA') THEN
C         ATTENTION IRET NON INITIALISE PAR VERIFG
          IRET=0
          CALL VERIFG('RIGI',IGAU,NSPG,'+',ZI(IMATE),'ELAS',1,
     &                 EPSTHE,IRET)
          IF (IRET.NE.0) EPSTHE=0.D0
          AT1 = (C(1,1)+C(1,2))*EPSTHE
          AT2 = (C(2,1)+C(2,2))*EPSTHE
        END IF

        IF ((OPTION.EQ.'SIEF_ELGA').OR.
     &      (OPTION.EQ.'EPSI_ELGA'))    THEN
C --- BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE
          DO 100 ICOU = 1,2*NBCOU + 1

            IF (MMT.EQ.0) THEN
              R = A
            ELSE
              R = A + (ICOU-1)*H/ (2.D0*NBCOU) - H/2.D0
            END IF

            DO 90 ISECT = 1,2*NBSEC + 1
              KPGS = KPGS + 1

              IF (ICOUDE.EQ.0) THEN
                CALL BCOUDE(IGAU,ICOU,ISECT,L,H,A,M,NNO,NBCOU,
     &                      NBSEC,ZR(IVF),ZR(IDFDK),ZR(JDFD2),MMT,B)
              ELSE IF (ICOUDE.EQ.1) THEN
                FI = (ISECT-1)*DEUXPI/ (2.D0*NBSEC)
C                  FI = FI - OMEGA
                SINFI = SIN(FI)
                L = THETA* (RAYON+R*SINFI)
                CALL BCOUDC(IGAU,ICOU,ISECT,H,A,M,OMEGA,XPG,NNO,
     &                      NBCOU,NBSEC,ZR(IVF),ZR(IDFDK),ZR(JDFD2),
     &                      RAYON,THETA,MMT,B)
              END IF

              IF (OPTION.EQ.'EPSI_ELGA') THEN
                DO 80 I = 1,4
                  DO 70 J = 1,NBRDDL
                    MAT(I,J) = B(I,J)
   70             CONTINUE
   80           CONTINUE
              ELSE IF (OPTION.EQ.'SIEF_ELGA') THEN
                SIGTH(1) = AT1
                SIGTH(2) = AT2
                CALL PROMAT(C,4,4,4,B,4,4,NBRDDL,MAT)
              END IF
              IRET = 0
              CALL PRMAVE(0,MAT,4,4,NBRDDL,VIN,NBRDDL,VOUT,4,IRET)

C  STOCKAGE DU VECTEUR VOUT

              INDICE = JOUT - 1 + 6* (KPGS-1)
              ZR(INDICE+1) = VOUT(1) - SIGTH(1)
              ZR(INDICE+2) = VOUT(2) - SIGTH(2)
              ZR(INDICE+3) = 0.D0
              ZR(INDICE+4) = VOUT(3)
              ZR(INDICE+5) = VOUT(4)
              ZR(INDICE+6) = 0.D0

   90       CONTINUE
  100     CONTINUE

         ELSE IF (OPTION.EQ.'DEGE_ELNO'.OR.OPTION.EQ.'DEGE_ELGA') THEN
C            DEFORMATIONS GENERALISEES DE POUTRE

          DO 110 K = 1,NNO
            IF (ICOUDE.EQ.1) THEN
              L = THETA*RAYON
            END IF
            HK = ZR(IVF-1+NNO* (IGAU-1)+K)
            DHK = ZR(IVF-1+NNO*NPG+NNO* (IGAU-1)+K)* (2.D0/L)
            DHK = ZR(IDFDK-1+NNO* (IGAU-1)+K)* (2.D0/L)

C           EPXX=DU/DX
            DEGG(6* (IGAU-1)+1) = DEGG(6* (IGAU-1)+1) +
     &                            DHK*VIN(NDDL* (K-1)+1)
C              GAXY=DV/DX -DRZ
            DEGG(6* (IGAU-1)+2) = DEGG(6* (IGAU-1)+2) +
     &                            DHK*VIN(NDDL* (K-1)+2) -
     &                            HK*VIN(NDDL* (K-1)+6)
C              GAXZ=DW/DX +DRY
            DEGG(6* (IGAU-1)+3) = DEGG(6* (IGAU-1)+3) +
     &                            DHK*VIN(NDDL* (K-1)+3) +
     &                            HK*VIN(NDDL* (K-1)+5)
C              GAT=D(DRX)/DX
            DEGG(6* (IGAU-1)+4) = DEGG(6* (IGAU-1)+4) +
     &                            DHK*VIN(NDDL* (K-1)+4)
C              KY=D(DRY)/DX
            DEGG(6* (IGAU-1)+5) = DEGG(6* (IGAU-1)+5) +
     &                            DHK*VIN(NDDL* (K-1)+5)
C              KZ=D(DRZ)/DX
            DEGG(6* (IGAU-1)+6) = DEGG(6* (IGAU-1)+6) +
     &                            DHK*VIN(NDDL* (K-1)+6)
  110     CONTINUE

        END IF

  120 CONTINUE

C

      IF (OPTION.EQ.'DEGE_ELNO'.OR.OPTION.EQ.'DEGE_ELGA') THEN
        DO 150 I = 1,6
          DO 130 IGAU = 1,NPG
            VPG(IGAU) = DEGG(6* (IGAU-1)+I)
            IF(OPTION.EQ.'DEGE_ELGA') ZR(JOUT+6*(IGAU-1)+I-1)=VPG(IGAU)
  130     CONTINUE
          IF(OPTION.EQ.'DEGE_ELNO') THEN
            CALL PPGAN2(JGANO,1,1,VPG,VNO)
            DO 140 J = 1,NNO
              ZR(JOUT+6* (J-1)+I-1) = VNO(J)
  140       CONTINUE
          ENDIF
  150   CONTINUE
      END IF

      END
