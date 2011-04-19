      SUBROUTINE TE0583(OPTION,NOMTE)
      IMPLICIT NONE
C MODIF ELEMENTS  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
      CHARACTER*16 OPTION,NOMTE
C    - FONCTION REALISEE:  CALCUL DU SECOND MEMBRE : TRAVAIL DE LA
C                          PRESSION ET FORCES LINEIQUES TUYAUX
C     OPTIONS :'CHAR_MECA_PESA_R' 'CHAR_MECA_FR1D1D''CHAR_MECA_PRES_R'
C              'CHAR_MECA_PRES_F'
C ......................................................................
      INTEGER NBRDDM
      PARAMETER (NBRDDM=156)
      REAL*8 H,A,L,PRESNO(4),PRESPG(4),RINT
      REAL*8 VPESAN(6),FPESAN(6),PESAN,F(NBRDDM)
      REAL*8 PI,DEUXPI,FI,PASS(NBRDDM,NBRDDM)
      REAL*8 FPESA1(6),FPESA2(6),FPESA3(6),VTEMP(NBRDDM)
      REAL*8 VPESA1(6),VPESA2(6),VPESA3(6),VPESA4(6)
      REAL*8 PGL(3,3),PGL1(3,3),PGL2(3,3),PGL3(3,3),OMEGA
      REAL*8 HK,POIDS,R8PI,RAYON,THETA,TK(4),CK,SK
      REAL*8 COSFI,SINFI,TE,PGL4(3,3),FPESA4(6),XPG(4)
      REAL*8 R8B,REXT,SEC,RHO,R,TIME,VALPAR(4)
      INTEGER CODRES
      CHARACTER*8 NOMPAR(4)
      CHARACTER*16 PHENOM
      INTEGER NBCOU,NBSEC,M,LORIEN,ICOUDE
      INTEGER IPOIDS,IVF,I,ICOU,IBLOC,INO,NBPAR,ICOMPX,NITER,ITER
      INTEGER ICAGEP,IGEOM,LMATER,JPESA,JOUT,LFORC,IRET
      INTEGER IGAU,ISECT,IPRES,K,IVECT,NBRDDL,INDIC0
      INTEGER INDIC1,INDIC2,INDIC3,INDIC4,INDIC5,J
      INTEGER JNBSPI,NBSECM,NBCOUM,ITEMPS,IER
      INTEGER NDIM,NNOS,NNO,JCOOPG,IDFDK,JDFD2,JGANO,NPG
      PARAMETER (NBSECM=32,NBCOUM=10)
      REAL*8 POICOU(2*NBCOUM+1),POISEC(2*NBSECM+1)
      LOGICAL NORMAL,GLOBAL
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
      CALL ELREF5(' ','MASS',NDIM,NNO,NNOS,NPG,IPOIDS,JCOOPG,IVF,IDFDK,
     &            JDFD2,JGANO)

      IF (OPTION.EQ.'CHAR_MECA_FC1D1D') THEN
        ICOMPX = 1
        NITER = 2
      ELSE
        ICOMPX = 0
        NITER = 1
      END IF
      PI = R8PI()
      DEUXPI = 2.D0*PI
      CALL JEVECH('PNBSP_I','L',JNBSPI)
      NBCOU = ZI(JNBSPI-1+1)
      NBSEC = ZI(JNBSPI-1+2)

C     -- CALCUL DES POIDS DES COUCHES ET DES SECTEURS:
      POICOU(1) = 1.D0/3.D0
      DO 10 I = 1,NBCOU - 1
        POICOU(2*I) = 4.D0/3.D0
        POICOU(2*I+1) = 2.D0/3.D0
   10 CONTINUE
      POICOU(2*NBCOU) = 4.D0/3.D0
      POICOU(2*NBCOU+1) = 1.D0/3.D0
      POISEC(1) = 1.D0/3.D0
      DO 20 I = 1,NBSEC - 1
        POISEC(2*I) = 4.D0/3.D0
        POISEC(2*I+1) = 2.D0/3.D0
   20 CONTINUE
      POISEC(2*NBSEC) = 4.D0/3.D0
      POISEC(2*NBSEC+1) = 1.D0/3.D0
      M = 3
      IF (NOMTE.EQ.'MET6SEG3') M = 6


      DO 30 I = 1,NPG
        XPG(I) = ZR(JCOOPG-1+I)
   30 CONTINUE
      NBRDDL = NNO* (6+3+6* (M-1))
      IF (NBRDDL.GT.NBRDDM) THEN
        CALL U2MESS('F','ELEMENTS4_40')
      END IF
      IF (NOMTE.EQ.'MET3SEG3') THEN
        IF (NBRDDL.NE.63) THEN
          CALL U2MESS('F','ELEMENTS4_41')
        END IF
      ELSE IF (NOMTE.EQ.'MET6SEG3') THEN
        IF (NBRDDL.NE.117) THEN
          CALL U2MESS('F','ELEMENTS4_41')
        END IF
      ELSE IF (NOMTE.EQ.'MET3SEG4') THEN
        IF (NBRDDL.NE.84) THEN
          CALL U2MESS('F','ELEMENTS4_41')
        END IF
      ELSE
        CALL U2MESS('F','ELEMENTS4_42')
      END IF
      CALL JEVECH('PCAORIE','L',LORIEN)
      CALL CARCOU(ZR(LORIEN),L,PGL,RAYON,THETA,PGL1,PGL2,PGL3,PGL4,NNO,
     &            OMEGA,ICOUDE)
      IF (ICOUDE.GE.10) THEN
        ICOUDE = ICOUDE - 10
      END IF
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PCAGEPO','L',ICAGEP)
      H = ZR(ICAGEP+1)
      A = ZR(ICAGEP) - H/2.D0
      RINT = A - H/2.D0
      REXT = A + H/2.D0
      SEC = PI* (REXT**2-RINT**2)
C A= RMOY, H = EPAISSEUR RINT = RAYON INTERIEUR
      IF (NNO.EQ.3) THEN
        TK(1) = 0.D0
        TK(2) = THETA
        TK(3) = THETA/2.D0
      ELSE IF (NNO.EQ.4) THEN
        TK(1) = 0.D0
        TK(2) = THETA
        TK(3) = THETA/3.D0
        TK(4) = 2.D0*THETA/3.D0
      END IF
C  LA PRESSION NE TRAVAILLE QUE SUR LE TERME WO
      IF (OPTION(1:14).EQ.'CHAR_MECA_PRES') THEN
        IF     (OPTION(15:16).EQ.'_R') THEN
          CALL JEVECD('PPRESSR',IPRES,0.D0)
          DO 40 I = 1,NNO
             PRESNO(I) = ZR(IPRES-1+I)
   40     CONTINUE
        ELSEIF (OPTION(15:16).EQ.'_F') THEN
          CALL JEVECH ('PPRESSF', 'L', IPRES)
          CALL JEVECH ('PTEMPSR', 'L', ITEMPS)
          VALPAR(4) = ZR(ITEMPS)
          NOMPAR(4) = 'INST'
          NOMPAR(1) = 'X'
          NOMPAR(2) = 'Y'
          NOMPAR(3) = 'Z'
          DO 45 I = 1, NNO
            VALPAR(1) = ZR(IGEOM+3*(I-1)  )
            VALPAR(2) = ZR(IGEOM+3*(I-1)+1)
            VALPAR(3) = ZR(IGEOM+3*(I-1)+2)
            CALL FOINTE('FM',ZK8(IPRES),4,NOMPAR,VALPAR,PRESNO(I),IER)
  45      CONTINUE
        ENDIF
        DO 60,IGAU = 1,NPG
          PRESPG(IGAU) = 0.D0
          DO 50,K = 1,NNO
            HK = ZR(IVF-1+NNO* (IGAU-1)+K)
            PRESPG(IGAU) = HK*PRESNO(K) + PRESPG(IGAU)
   50     CONTINUE
   60   CONTINUE
        CALL JEVECH('PVECTUR','E',IVECT)
        DO 90 K = 1,NNO
C           TRAVAIL SUR UX
          INDIC0 = IVECT - 1 + (6+6* (M-1)+3)* (K-1) + 1
C           TRAVAIL SUR UY
          INDIC1 = IVECT - 1 + (6+6* (M-1)+3)* (K-1) + 2
C           TRAVAIL SUR UZ
          INDIC2 = IVECT - 1 + (6+6* (M-1)+3)* (K-1) + 3
C           TRAVAIL SUR W0
          INDIC3 = IVECT - 1 + (6+6* (M-1)+3)* (K-1) + 1 + 6 + 6* (M-1)
C           TRAVAIL SUR WI1
          INDIC4 = IVECT - 1 + (6+6* (M-1)+3)* (K-1) + 2 + 6 + 6* (M-1)
C           TRAVAIL SUR W01
          INDIC5 = IVECT - 1 + (6+6* (M-1)+3)* (K-1) + 3 + 6 + 6* (M-1)
          DO 80 IGAU = 1,NPG
C BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR
            HK = ZR(IVF-1+NNO* (IGAU-1)+K)
            IF (ICOUDE.EQ.1) THEN
              CK = COS((1.D0+XPG(IGAU))*THETA/2.D0-TK(K))
              SK = SIN((1.D0+XPG(IGAU))*THETA/2.D0-TK(K))
            ELSE
              CK = 1.D0
              SK = 0.D0
            END IF
C BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE
            DO 70 ISECT = 1,2*NBSEC + 1
              IF (ICOUDE.EQ.0) THEN
                POIDS = ZR(IPOIDS-1+IGAU)*POISEC(ISECT)* (L/2.D0)*
     &                  DEUXPI/ (2.D0*NBSEC)*RINT
                ZR(INDIC3) = ZR(INDIC3) + HK*POIDS*PRESPG(IGAU)
              ELSE
                FI = (ISECT-1)*DEUXPI/ (2.D0*NBSEC)
                COSFI = COS(FI)
                SINFI = SIN(FI)
                TE = FI - OMEGA
                L = THETA* (RAYON+RINT*SINFI)
                POIDS = ZR(IPOIDS-1+IGAU)*POISEC(ISECT)* (L/2.D0)*
     &                  DEUXPI/ (2.D0*NBSEC)*RINT
                ZR(INDIC0) = ZR(INDIC0) + HK*POIDS*PRESPG(IGAU)*SINFI*SK
                ZR(INDIC1) = ZR(INDIC1) - HK*POIDS*PRESPG(IGAU)*SINFI*CK
                ZR(INDIC2) = ZR(INDIC2) - HK*POIDS*PRESPG(IGAU)*COSFI
                ZR(INDIC3) = ZR(INDIC3) + HK*POIDS*PRESPG(IGAU)
                ZR(INDIC4) = ZR(INDIC4) + HK*POIDS*PRESPG(IGAU)*COS(TE)
                ZR(INDIC5) = ZR(INDIC5) + HK*POIDS*PRESPG(IGAU)*SIN(TE)
              END IF
   70       CONTINUE
   80     CONTINUE
   90   CONTINUE
        IF (ICOUDE.NE.0) THEN
          CALL VLGGLC(NNO,NBRDDL,PGL1,PGL2,PGL3,PGL4,ZR(IVECT),'LG',
     &                PASS,VTEMP)
        END IF
C CAS PESANTEUR ET FORCE LINEIQUE
      ELSE IF ((OPTION.EQ.'CHAR_MECA_PESA_R') .OR.
     &         (OPTION.EQ.'CHAR_MECA_FR1D1D') .OR.
     &         (OPTION.EQ.'CHAR_MECA_FC1D1D')) THEN
        DO 250 ITER = 1,NITER
          IF (OPTION.EQ.'CHAR_MECA_PESA_R') THEN
            CALL JEVECH('PMATERC','L',LMATER)
            CALL RCCOMA(ZI(LMATER),'ELAS',PHENOM,CODRES)
            IF (PHENOM.EQ.'ELAS' .OR. PHENOM.EQ.'ELAS_FO' .OR.
     &          PHENOM.EQ.'ELAS_ISTR' .OR. PHENOM.EQ.'ELAS_ISTR_FO' .OR.
     &          PHENOM.EQ.'ELAS_ORTH' .OR.
     &          PHENOM.EQ.'ELAS_ORTH_FO') THEN
              CALL RCVALA(ZI(LMATER),' ',PHENOM,0,' ',R8B,1,'RHO',RHO,
     &                    CODRES,1)
            ELSE
              CALL U2MESS('F','ELEMENTS4_43')
            END IF
            CALL JEVECH('PPESANR','L',JPESA)
            PESAN = ZR(JPESA)
            DO 100 I = 1,3
              VPESAN(I) = RHO*PESAN*ZR(JPESA+I)
  100       CONTINUE
            DO 110 I = 4,6
              VPESAN(I) = 0.D0
  110       CONTINUE
          ELSE
            IF (ICOMPX.EQ.0) THEN
              CALL JEVECH('PFR1D1D','L',LFORC)
            ELSE
              CALL JEVECH('PFC1D1D','L',LFORC)
            END IF
            IF (ICOMPX.EQ.1) THEN
              IF (ITER.EQ.1) THEN
                DO 120 I = 1,3
                  VPESAN(I) = DBLE(ZC(LFORC-1+I))/SEC
  120           CONTINUE
              ELSE
                DO 130 I = 1,3
                  VPESAN(I) = DIMAG(ZC(LFORC-1+I))/SEC
  130           CONTINUE
              END IF
            ELSE
              DO 140 I = 1,3
                VPESAN(I) = ZR(LFORC-1+I)/SEC
  140         CONTINUE
            END IF
            DO 150 I = 4,6
              VPESAN(I) = 0.D0
  150       CONTINUE
          END IF
          DO 160 I = 1,NBRDDL
            F(I) = 0.D0
  160     CONTINUE
          IF (ICOUDE.EQ.0) THEN
            CALL UTPVGL(1,6,PGL,VPESAN(1),FPESAN(1))
          ELSE
            CALL UTPVGL(1,6,PGL1,VPESAN(1),FPESA1(1))
            CALL UTPVGL(1,6,PGL2,VPESAN(1),FPESA2(1))
            CALL UTPVGL(1,6,PGL3,VPESAN(1),FPESA3(1))
            IF (NNO.EQ.4) THEN
              CALL UTPVGL(1,6,PGL4,VPESAN(1),FPESA4(1))
            END IF
          END IF
C BOUCLE SUR LES POINTS DE GAUSS DANS LA LONGUEUR
          DO 210 IGAU = 1,NPG
C BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR
            DO 200 ICOU = 1,2*NBCOU + 1
              R = A + (ICOU-1)*H/ (2.D0*NBCOU) - H/2.D0
C BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE
              DO 190 ISECT = 1,2*NBSEC + 1
                IF (ICOUDE.EQ.0) THEN
                  POIDS = ZR(IPOIDS-1+IGAU)*POICOU(ICOU)*POISEC(ISECT)*
     &                     (L/2.D0)*H*DEUXPI/ (4.D0*NBCOU*NBSEC)*R
                  DO 170 K = 1,NNO
                    HK = ZR(IVF-1+NNO* (IGAU-1)+K)
                    IBLOC = (9+6* (M-1))* (K-1)
                    F(IBLOC+1) = F(IBLOC+1) + POIDS*HK*FPESAN(1)
                    F(IBLOC+2) = F(IBLOC+2) + POIDS*HK*FPESAN(2)
                    F(IBLOC+3) = F(IBLOC+3) + POIDS*HK*FPESAN(3)
  170             CONTINUE
                ELSE IF (ICOUDE.EQ.1) THEN
                  FI = (ISECT-1)*DEUXPI/ (2.D0*NBSEC)
                  COSFI = COS(FI)
                  SINFI = SIN(FI)
                  L = THETA* (RAYON+R*SINFI)
                  POIDS = ZR(IPOIDS-1+IGAU)*POICOU(ICOU)*POISEC(ISECT)*
     &                     (L/2.D0)*H*DEUXPI/ (4.D0*NBCOU*NBSEC)*R
                  DO 180 K = 1,3
                    HK = ZR(IVF-1+NNO* (IGAU-1)+1)
                    IBLOC = (9+6* (M-1))* (1-1)
                    F(IBLOC+K) = F(IBLOC+K) + POIDS*HK*FPESA1(K)
                    HK = ZR(IVF-1+NNO* (IGAU-1)+2)
                    IBLOC = (9+6* (M-1))* (2-1)
                    F(IBLOC+K) = F(IBLOC+K) + POIDS*HK*FPESA2(K)
                    HK = ZR(IVF-1+NNO* (IGAU-1)+3)
                    IBLOC = (9+6* (M-1))* (3-1)
                    F(IBLOC+K) = F(IBLOC+K) + POIDS*HK*FPESA3(K)
                    IF (NNO.EQ.4) THEN
                      IBLOC = (9+6* (M-1))* (4-1)
                      F(IBLOC+K) = F(IBLOC+K) + POIDS*HK*FPESA4(K)
                    END IF
  180             CONTINUE
                END IF
  190         CONTINUE
  200       CONTINUE
  210     CONTINUE
          IF (ICOUDE.EQ.0) THEN
            CALL VLGGL(NNO,NBRDDL,PGL,F,'LG',PASS,VTEMP)
          ELSE
            CALL VLGGLC(NNO,NBRDDL,PGL1,PGL2,PGL3,PGL4,F,'LG',PASS,
     &                  VTEMP)
          END IF
          IF (ICOMPX.EQ.1) THEN
            CALL JEVECH('PVECTUC','E',JOUT)
            IF (ITER.EQ.1) THEN
              DO 220 J = 1,NBRDDL
                ZC(JOUT-1+J) = F(J)
  220         CONTINUE
            ELSE
              DO 230 J = 1,NBRDDL
                ZC(JOUT-1+J) = DCMPLX(DBLE(ZC(JOUT-1+J)),DBLE(F(J)))
  230         CONTINUE
            END IF
          ELSE
            CALL JEVECH('PVECTUR','E',JOUT)
            DO 240 I = 1,NBRDDL
              ZR(JOUT-1+I) = F(I)
  240       CONTINUE
          END IF
  250   CONTINUE
C CAS FORCE LINEIQUE FONCTION
      ELSE IF ((OPTION.EQ.'CHAR_MECA_FF1D1D')) THEN
        CALL JEVECH('PFF1D1D','L',LFORC)
        NORMAL = ZK8(LFORC+6) .EQ. 'VENT'
        GLOBAL = ZK8(LFORC+6) .EQ. 'GLOBAL'
        IF (NORMAL) THEN
          CALL U2MESK('F','ELEMENTS4_44',1,OPTION)
        END IF
        IF (.NOT.GLOBAL) THEN
          CALL U2MESK('F','ELEMENTS4_45',1,OPTION)
        END IF
        NOMPAR(4) = 'INST'
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'Z'
        CALL TECACH('NNN','PTEMPSR',1,ITEMPS,IRET)
        IF (ITEMPS.NE.0) THEN
          TIME = ZR(ITEMPS)
          VALPAR(4) = TIME
          NBPAR = 4
        ELSE
          NBPAR = 3
        END IF
C        NOEUDS 1 A 3
        INO = 1
        DO 260 I = 1,3
          VALPAR(I) = ZR(IGEOM-1+3* (INO-1)+I)
  260   CONTINUE
        DO 270 I = 1,3
          CALL FOINTE('FM',ZK8(LFORC+I-1),NBPAR,NOMPAR,VALPAR,VPESA1(I),
     &                IER)
          VPESA1(I) = VPESA1(I)/SEC
  270   CONTINUE
        INO = 2
        DO 280 I = 1,3
          VALPAR(I) = ZR(IGEOM-1+3* (INO-1)+I)
  280   CONTINUE
        DO 290 I = 1,3
          CALL FOINTE('FM',ZK8(LFORC+I-1),NBPAR,NOMPAR,VALPAR,VPESA2(I),
     &                IER)
          VPESA2(I) = VPESA2(I)/SEC
  290   CONTINUE
        INO = 3
        DO 300 I = 1,3
          VALPAR(I) = ZR(IGEOM-1+3* (INO-1)+I)
  300   CONTINUE
        DO 310 I = 1,3
          CALL FOINTE('FM',ZK8(LFORC+I-1),NBPAR,NOMPAR,VALPAR,VPESA3(I),
     &                IER)
          VPESA3(I) = VPESA3(I)/SEC
  310   CONTINUE
        IF (NNO.EQ.4) THEN
          INO = 4
          DO 320 I = 1,3
            VALPAR(I) = ZR(IGEOM-1+3* (INO-1)+I)
  320     CONTINUE
          DO 330 I = 1,3
            CALL FOINTE('FM',ZK8(LFORC+I-1),NBPAR,NOMPAR,VALPAR,
     &                  VPESA4(I),IER)
            VPESA4(I) = VPESA4(I)/SEC
  330     CONTINUE
        END IF
        DO 340 I = 4,6
          VPESA1(I) = 0.D0
          VPESA2(I) = 0.D0
          VPESA3(I) = 0.D0
          VPESA4(I) = 0.D0
  340   CONTINUE
        DO 350 I = 1,NBRDDL
          F(I) = 0.D0
  350   CONTINUE
        IF (ICOUDE.EQ.0) THEN
          CALL UTPVGL(1,6,PGL,VPESA1(1),FPESA1(1))
          CALL UTPVGL(1,6,PGL,VPESA2(1),FPESA2(1))
          CALL UTPVGL(1,6,PGL,VPESA3(1),FPESA3(1))
          IF (NNO.EQ.4) THEN
            CALL UTPVGL(1,6,PGL,VPESA4(1),FPESA4(1))
          END IF
        ELSE
          CALL UTPVGL(1,6,PGL1,VPESAN(1),FPESA1(1))
          CALL UTPVGL(1,6,PGL2,VPESAN(1),FPESA2(1))
          CALL UTPVGL(1,6,PGL3,VPESAN(1),FPESA3(1))
          IF (NNO.EQ.4) THEN
            CALL UTPVGL(1,6,PGL4,VPESAN(1),FPESA4(1))
          END IF
        END IF
C BOUCLE SUR LES POINTS DE GAUSS DANS LA LONGUEUR
        DO 400 IGAU = 1,NPG
C BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR
          DO 390 ICOU = 1,2*NBCOU + 1
            R = A + (ICOU-1)*H/ (2.D0*NBCOU) - H/2.D0
C BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE
            DO 380 ISECT = 1,2*NBSEC + 1
              IF (ICOUDE.EQ.0) THEN
                POIDS = ZR(IPOIDS-1+IGAU)*POICOU(ICOU)*POISEC(ISECT)*
     &                  (L/2.D0)*H*DEUXPI/ (4.D0*NBCOU*NBSEC)*R
                DO 360 K = 1,3
                  HK = ZR(IVF-1+NNO* (IGAU-1)+1)
                  IBLOC = (9+6* (M-1))* (1-1)
                  F(IBLOC+K) = F(IBLOC+K) + POIDS*HK*FPESA1(K)
                  HK = ZR(IVF-1+NNO* (IGAU-1)+2)
                  IBLOC = (9+6* (M-1))* (2-1)
                  F(IBLOC+K) = F(IBLOC+K) + POIDS*HK*FPESA2(K)
                  HK = ZR(IVF-1+NNO* (IGAU-1)+3)
                  IBLOC = (9+6* (M-1))* (3-1)
                  F(IBLOC+K) = F(IBLOC+K) + POIDS*HK*FPESA3(K)
                  IF (NNO.EQ.4) THEN
                    IBLOC = (9+6* (M-1))* (4-1)
                    F(IBLOC+K) = F(IBLOC+K) + POIDS*HK*FPESA4(K)
                  END IF
  360           CONTINUE
              ELSE IF (ICOUDE.EQ.1) THEN
                FI = (ISECT-1)*DEUXPI/ (2.D0*NBSEC)
                COSFI = COS(FI)
                SINFI = SIN(FI)
                L = THETA* (RAYON+R*SINFI)
                POIDS = ZR(IPOIDS-1+IGAU)*POICOU(ICOU)*POISEC(ISECT)*
     &                  (L/2.D0)*H*DEUXPI/ (4.D0*NBCOU*NBSEC)*R
                DO 370 K = 1,3
                  HK = ZR(IVF-1+NNO* (IGAU-1)+1)
                  IBLOC = (9+6* (M-1))* (1-1)
                  F(IBLOC+K) = F(IBLOC+K) + POIDS*HK*FPESA1(K)
                  HK = ZR(IVF-1+NNO* (IGAU-1)+2)
                  IBLOC = (9+6* (M-1))* (2-1)
                  F(IBLOC+K) = F(IBLOC+K) + POIDS*HK*FPESA2(K)
                  HK = ZR(IVF-1+NNO* (IGAU-1)+3)
                  IBLOC = (9+6* (M-1))* (3-1)
                  F(IBLOC+K) = F(IBLOC+K) + POIDS*HK*FPESA3(K)
                  IF (NNO.EQ.4) THEN
                    IBLOC = (9+6* (M-1))* (4-1)
                    F(IBLOC+K) = F(IBLOC+K) + POIDS*HK*FPESA4(K)
                  END IF
  370           CONTINUE
              END IF
  380       CONTINUE
  390     CONTINUE
  400   CONTINUE
        IF (ICOUDE.EQ.0) THEN
          CALL VLGGL(NNO,NBRDDL,PGL,F,'LG',PASS,VTEMP)
        ELSE
          CALL VLGGLC(NNO,NBRDDL,PGL1,PGL2,PGL3,PGL4,F,'LG',PASS,VTEMP)
        END IF
        CALL JEVECH('PVECTUR','E',JOUT)
        DO 410,I = 1,NBRDDL
          ZR(JOUT-1+I) = F(I)
  410   CONTINUE
      END IF
      END
