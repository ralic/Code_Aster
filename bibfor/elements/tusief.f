      SUBROUTINE TUSIEF(OPTION,NOMTE,NBRDDL,B,VIN,MAT,PASS,VTEMP)
      IMPLICIT NONE
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*16 OPTION,NOMTE
C ......................................................................

C   FONCTION REALISEE:  CALCUL DES OPTIONS EPSI_ELGA_DEPL,DEGE_ELNO_DEPL
C                          ET SIEF_ELGA_DEPL POUR UN TUYAU
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................

      INTEGER NBRES,NBRDDL
      PARAMETER (NBRES=9)
      CHARACTER*8 NOMRES(NBRES),NOMPAR,NOMPU(2)
      CHARACTER*2 CODRET(NBRES)
      REAL*8 VALRES(NBRES),VALPAR,VALPU(2),DEGG(24)
      REAL*8 H,A,L,E,NU,BETA,CISAIL,G,OMEGA,DHK
      REAL*8 SINFI,FI,DEUXPI,R,R8PI,AT1,AT2,VPG(4),VNO(4)
      REAL*8 B(4,NBRDDL),C(4,4),ALPHA,TMOY1,TINF1,TSUP1
      REAL*8 TEMPGM(4),TEMPGS(4),TEMPGI(4),TREF,HK,COE1
      REAL*8 TMOY(4),TINF(4),TSUP(4),SIGTH(2),XPG(4)
      REAL*8 PGL(3,3),VIN(NBRDDL),VOUT(4),MAT(4,NBRDDL)
      REAL*8 PGL1(3,3),PGL2(3,3),PGL3(3,3),RAYON,THETA
      REAL*8 VTEMP(NBRDDL),PASS(NBRDDL,NBRDDL),PGL4(3,3)
      INTEGER NNO,NPG,NBCOU,NBSEC,ICOUDE,NDIM,JCOOPG
      INTEGER IPOIDS,IVF,KPGS,IBID,IER,K,JTREF,NNOS
      INTEGER IMATE,ITEMP,ICAGEP,IGEOM,NBPAR
      INTEGER IGAU,ICOU,ISECT,I,J,JIN,JOUT,IRET,INDICE
      INTEGER LORIEN,ICOUD2,MMT,JNBSPI
      INTEGER ITAB(8),NDDL,M,IDFDK,JDFD2,JGANO

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      CALL ELREF5(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,JCOOPG,IVF,IDFDK,
     &            JDFD2,JGANO)

      DEUXPI = 2.D0*R8PI()

      CALL JEVECH('PNBSP_I','L',JNBSPI)
      NBCOU = ZI(JNBSPI-1+1)
      NBSEC = ZI(JNBSPI-1+2)


      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PCAGEPO','L',ICAGEP)
      H = ZR(ICAGEP+1)
      A = ZR(ICAGEP) - H/2.D0
C A= RMOY, H = EPAISSEUR


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


      IF (OPTION(1:14).EQ.'SIEF_ELGA_DEPL') THEN

        CALL JEVECH('PMATERC','L',IMATE)
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NOMRES(3) = 'ALPHA'

        CALL TECACH('NNN','PTEMPER',8,ITAB,IRET)
        ITEMP = ITAB(1)
        IF (ITEMP.EQ.0) THEN
          NBPAR = 0
          NOMPAR = ' '
          VALPAR = 0.D0
        ELSE
          NBPAR = 1
          NOMPAR = 'TEMP'
          CALL DXTPIF(ZR(ITEMP),ZL(ITAB(8)))
          VALPAR = ZR(ITEMP)
        END IF

        CALL RCVALA(ZI(IMATE),' ','ELAS',NBPAR,NOMPAR,VALPAR,2,NOMRES,
     &              VALRES,CODRET,'FM')
        CALL RCVALA(ZI(IMATE),' ','ELAS',NBPAR,NOMPAR,VALPAR,1,
     &              NOMRES(3),VALRES(3),CODRET(3),'  ')
        E = VALRES(1)
        NU = VALRES(2)
        IF (CODRET(3).NE.'OK') THEN
          ALPHA = 0.D0
        ELSE
          ALPHA = VALRES(3)
        END IF

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

      IF (OPTION(1:14).EQ.'EPSI_ELGA_DEPL') THEN

        CALL JEVECH('PDEFORR','E',JOUT)

      ELSE IF (OPTION(1:14).EQ.'DEGE_ELNO_DEPL') THEN

        CALL JEVECH('PDEFOGR','E',JOUT)
        CALL R8INIR(24,0.D0,DEGG,1)
        NBRDDL = NNO* (6+3+6* (M-1))
        NDDL = (6+3+6* (M-1))

      ELSE IF (OPTION(1:14).EQ.'SIEF_ELGA_DEPL') THEN

C===============================================================
C        -- RECUPERATION DE LA TEMPERATURE :
C        -- SI LA TEMPERATURE EST CONNUE AUX NOEUDS :
        CALL TECACH('NNN','PTEMPER',8,ITAB,IRET)
        ITEMP = ITAB(1)
        IF (IRET.EQ.0 .OR. IRET.EQ.3) THEN
          DO 30 I = 1,NNO
            CALL DXTPIF(ZR(ITEMP+3* (I-1)),ZL(ITAB(8)+3* (I-1)))
            TMOY(I) = ZR(ITEMP+3* (I-1))
            TINF(I) = ZR(ITEMP+3* (I-1)+1)
            TSUP(I) = ZR(ITEMP+3* (I-1)+2)
   30     CONTINUE
        END IF
C        -- SI LA TEMPERATURE EST UNE FONCTION DE 'INST' ET 'EPAIS'
        CALL TECACH('NNN','PTEMPEF',1,ITEMP,IRET)
        IF (IRET.EQ.0) THEN
          NOMPU(1) = 'INST'
          NOMPU(2) = 'EPAIS'
          CALL JEVECH('PTEMPSR','L',IBID)
          VALPU(1) = ZR(IBID)
          VALPU(2) = 0.D0
          CALL FOINTE('FM',ZK8(ITEMP),2,NOMPU,VALPU,TMOY1,IER)
          VALPU(2) = -H/2.D0
          CALL FOINTE('FM',ZK8(ITEMP),2,NOMPU,VALPU,TINF1,IER)
          VALPU(2) = +H/2.D0
          CALL FOINTE('FM',ZK8(ITEMP),2,NOMPU,VALPU,TSUP1,IER)
          DO 40,I = 1,NNO
            TMOY(I) = TMOY1
            TINF(I) = TINF1
            TSUP(I) = TSUP1
   40     CONTINUE
        END IF

C         PASSAGE DE LA TEMPERATURE DES NOEUDS AUX POINTS DE GAUSS

        DO 60,IGAU = 1,NPG
          TEMPGI(IGAU) = 0.D0
          TEMPGM(IGAU) = 0.D0
          TEMPGS(IGAU) = 0.D0
          DO 50,K = 1,NNO
            HK = ZR(IVF-1+NNO* (IGAU-1)+K)
            TEMPGI(IGAU) = HK*TINF(K) + TEMPGI(IGAU)
            TEMPGM(IGAU) = HK*TMOY(K) + TEMPGM(IGAU)
            TEMPGS(IGAU) = HK*TSUP(K) + TEMPGS(IGAU)
   50     CONTINUE
   60   CONTINUE

        CALL JEVECH('PTEREF','L',JTREF)
        TREF = ZR(JTREF)
C===============================================================

        CALL JEVECH('PCONTRR','E',JOUT)

      ELSE
        CALL U2MESK('F','ELEMENTS4_49',1,OPTION)
      END IF


C BOUCLE SUR LES POINTS DE GAUSS

C BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR

      KPGS = 0
      SIGTH(1) = 0.D0
      SIGTH(2) = 0.D0
      DO 120 IGAU = 1,NPG
        IF (OPTION(1:14).EQ.'SIEF_ELGA_DEPL') THEN
          COE1 = (TEMPGI(IGAU)+TEMPGS(IGAU)+TEMPGM(IGAU))/3.D0 - TREF
          AT1 = (C(1,1)+C(1,2))*COE1*ALPHA
          AT2 = (C(2,1)+C(2,2))*COE1*ALPHA
        END IF

        IF (OPTION(1:14).NE.'DEGE_ELNO_DEPL') THEN

          DO 100 ICOU = 1,2*NBCOU + 1

C BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE

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

              IF (OPTION(1:14).EQ.'EPSI_ELGA_DEPL') THEN

                DO 80 I = 1,4
                  DO 70 J = 1,NBRDDL
                    MAT(I,J) = B(I,J)
   70             CONTINUE
   80           CONTINUE
              ELSE IF (OPTION(1:14).EQ.'SIEF_ELGA_DEPL') THEN
                SIGTH(1) = AT1
                SIGTH(2) = AT2
                CALL PROMAT(C,4,4,4,B,4,4,NBRDDL,MAT)
              END IF
              IRET = 0
              CALL PRODMV(0,MAT,4,4,NBRDDL,VIN,NBRDDL,VOUT,4,IRET)

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

        ELSE IF (OPTION(1:14).EQ.'DEGE_ELNO_DEPL') THEN
C            DEFORMATIONS GENRALISEES DE POUTRE

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

      IF (OPTION.EQ.'DEGE_ELNO_DEPL') THEN
        DO 150 I = 1,6
          DO 130 IGAU = 1,NPG
            VPG(IGAU) = DEGG(6* (IGAU-1)+I)
  130     CONTINUE
          CALL PPGAN2(JGANO,1,VPG,VNO)
          DO 140 J = 1,NNO
            ZR(JOUT+6* (J-1)+I-1) = VNO(J)
  140     CONTINUE
  150   CONTINUE
      END IF

      END
