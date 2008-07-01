      SUBROUTINE TE0038(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*(*) OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/06/2008   AUTEUR PROIX J-M.PROIX 
C ======================================================================
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
C     CALCULE DES TERMES PROPRES A UN STRUCTURE  (ELEMENTS DE POUTRE)
C TOLE CRP_6
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C       'MASS_INER      : CALCUL DES CARACTERISTIQUES DE STRUCTURES
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C       'MECA_POU_D_E'  : POUTRE DROITE D'EULER       (SECTION VARIABLE)
C       'MECA_POU_D_T'  : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
C       'MECA_POU_C_T'  : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
C       'MECA_POU_D_EM' : POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
C       'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
C       'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
C                         MULTI-FIBRES SECTION CONSTANTE
C     ------------------------------------------------------------------

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      CHARACTER*2  CODRES
      CHARACTER*8 MATERI
      CHARACTER*16 CH16,PHENOM
      REAL*8 RHO,A1,IY1,IZ1,A2,CDG(3),AB2,AB3,AB4,AMB,APB,EP
      REAL*8 RAD,ANG,ANGARC,ANGS2,XFL,XL,XL2,MATINL(6)
      REAL*8 MATINE(6),PGL(3,3),PGL1(3,3),PGL2(3,3),ANGL(3)
      REAL*8 P1(3,3),P2(3,3),P3(3,3),CDGL(3),XFLY,XFLZ,R8B
      REAL*8 PGL3(3,3),PI,PO,POXI2,R8PI,RAYON,REXT,RINT,RMOY,RR
      REAL*8 RY1,RY2,RZ1,RZ2,THETA,UNPR2,UNPR4,UNPRR,XA,XB,XI
      REAL*8 XIG,XISL,XIXX,XIXZ,XIZZ,XZIG,YIG,ZERO,ZIG,PGL4(3,3)
      REAL*8 T1(3),T2(3),NORME1,NORME2,N(3),NORMEN,X3(3),Y3(3)
      REAL*8 COO1(3),COO2(3),COO3(3),PREC,R8PREM,OMEGA,TRIGOM
      REAL*8 CASECT(6),CASEC1(6), VAL

      INTEGER LMATER,LX,LORIEN,NNO,NC,LCASTR,LSECT,LSECT2,ITYPE,ICOUDE
      INTEGER I,N1,N2,LRCOU,IADZI,IAZK24,NN2
      INTEGER INBF, JACF, ICOMPO, ISDCOM, NBGF, NCARFI, IG
      INTEGER NUGF, ICP, NBFIG, IPOS
C     ------------------------------------------------------------------
      PREC = R8PREM()
      ZERO = 0.0D0

C     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---

      CALL JEVECH('PMATERC','L',LMATER)

      IF( NOMTE.NE.'MECA_POU_D_EM') THEN
         CALL RCCOMA(ZI(LMATER),'ELAS',PHENOM,CODRES)

         IF (PHENOM.EQ.'ELAS' .OR. PHENOM.EQ.'ELAS_FO' .OR.
     &       PHENOM.EQ.'ELAS_ISTR' .OR. PHENOM.EQ.'ELAS_ISTR_FO' .OR.
     &       PHENOM.EQ.'ELAS_FLUI' .OR.
     &       PHENOM.EQ.'ELAS_ORTH' .OR. PHENOM.EQ.'ELAS_ORTH_FO') THEN
           CALL RCVALA(ZI(LMATER),' ',PHENOM,0,' ',R8B,1,'RHO',RHO,
     &              CODRES,'FM')
         ELSE
           CALL U2MESS('F','ELEMENTS_50')
         END IF
      ENDIF

C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---

      CALL JEVECH('PGEOMER','L',LX)
      LX = LX - 1
      XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2 +
     &           (ZR(LX+5)-ZR(LX+2))**2 +
     &           (ZR(LX+6)-ZR(LX+3))**2 )
      IF (XL.EQ.0.D0) THEN
         CH16 = ' ?????????'
         CALL U2MESK('F','ELEMENTS2_80',1,CH16(:8))
      END IF


C     --- ORIENTATION DE LA POUTRE ---

      CALL JEVECH('PCAORIE','L',LORIEN)
      CALL MATROT(ZR(LORIEN),PGL)
      NNO = 1
      NC = 3

      IF (OPTION.EQ.'MASS_INER') THEN
         CALL JEVECH('PMASSINE','E',LCASTR)
         DO 10 I = 1,6
            MATINE(I) = 0.D0
            MATINL(I) = 0.D0
10       CONTINUE

         IF( NOMTE.EQ.'MECA_POU_D_EM') THEN
C           RECUPERATION DES CARACTERISTIQUES DES FIBRES :
            CALL JEVECH('PNBSP_I','L',INBF)
            NBGF=ZI(INBF+1)
            CALL JEVECH('PFIBRES','L',JACF)
            NCARFI = 3
C           RECUPERATION DES MATERIAUX DANS SDCOMP DANS COMPOR
            CALL JEVECH('PCOMPOR','L',ICOMPO)
            CALL JEVEUO(ZK16(ICOMPO-1+7),'L',ISDCOM)
         ENDIF

         IF ((NOMTE.NE.'MET3SEG3') .AND. (NOMTE.NE.'MET6SEG3') .AND.
     &       (NOMTE.NE.'MET3SEG4')) THEN
C           RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS
            CALL JEVECH('PCAGNPO','L',LSECT)
            LSECT = LSECT - 1
            LSECT2 = LSECT + 11
            ITYPE = NINT(ZR(LSECT+23))
C           SECTION INITIALE
            A1 = ZR(LSECT+1)
            IY1 = ZR(LSECT+2)
            IZ1 = ZR(LSECT+3)
            RY1 = ZR(LSECT+9)
            RZ1 = ZR(LSECT+10)
C           SECTION FINALE
            A2 = ZR(LSECT2+1)
            RY2 = ZR(LSECT2+9)
            RZ2 = ZR(LSECT2+10)
         ELSE
C           RECUPERATION DES CARACTERISTIQUES  DES TUYAUX
            ITYPE = -999
            CALL JEVECH('PCAGEPO','L',LSECT)
            REXT = ZR(LSECT)
            EP = ZR(LSECT+1)
            RINT = REXT - EP
            RMOY = REXT - EP/2.D0
            PI = R8PI()
            A1 = PI* (REXT*REXT-RINT*RINT)
            IY1 = PI* (REXT**4-RINT**4)/4.D0
            IZ1 = IY1

            CALL TECAEL(IADZI,IAZK24)
            NN2 = ZI(IADZI-1+2)
            CALL CARCOU(ZR(LORIEN),XL,PGL,RAYON,THETA,PGL1,PGL2,PGL3,
     &                  PGL4,NN2,OMEGA,ICOUDE)
            IF (ICOUDE.GE.10) THEN
               ICOUDE = ICOUDE - 10
            END IF

            IF (ICOUDE.EQ.1) XL = THETA*RAYON
            XL2 = XL*XL
            ANGS2 = THETA/2.D0
C           CALCUL D'UN REPERE MOYEN
            IF (NN2.EQ.4) THEN
               DO 20 I = 1,3
                  ANGL(I) = 0.D0
                  COO1(I) = ZR(LX+I)
                  COO2(I) = ZR(LX+3+I)
                  COO3(I) = (ZR(LX+6+I)+ZR(LX+9+I))*0.5D0
20             CONTINUE
               CALL VDIFF(3,COO3,COO1,T1)
               CALL VDIFF(3,COO2,COO3,T2)
               CALL NORMEV(T1,NORME1)
               CALL NORMEV(T2,NORME2)
               CALL PROVEC(T2,T1,N)
               CALL NORMEV(N,NORMEN)
               CALL VDIFF(3,COO2,COO1,X3)
               CALL PROVEC(X3,N,Y3)
               CALL ANGVXY(X3,Y3,ANGL)
               CALL MATROT(ANGL,PGL3)
            END IF

C           ------- CALCULS  -----------------------------
C           -------- MASSE
            ZR(LCASTR) = RHO*A1*XL
C           -------- CDG
            CDGL(1) = 0.D0
            IF (ICOUDE.EQ.1) THEN
               CDGL(2) = -RAYON* (SIN(ANGS2)/ANGS2*
     &             (1.D0+ (RMOY*RMOY+EP*EP/4.D0)/ (2.D0*RAYON**2))-
     &                COS(ANGS2))
            ELSE
               CDGL(2) = 0.D0
            END IF
            CDGL(3) = 0.D0
            N1 = 1
            N2 = 3
            IF (ICOUDE.EQ.1) THEN
               CALL UTPVLG(N1,N2,PGL3,CDGL,CDG)
            ELSE
               CALL UTPVLG(N1,N2,PGL,CDGL,CDG)
            END IF
            ZR(LCASTR+1) = CDG(1) + (ZR(LX+4)+ZR(LX+1))/2.D0
            ZR(LCASTR+2) = CDG(2) + (ZR(LX+5)+ZR(LX+2))/2.D0
            ZR(LCASTR+3) = CDG(3) + (ZR(LX+6)+ZR(LX+3))/2.D0
C           -------- INERTIE
C           --- INERTIE DE L'ELEMENT ---
            IF (ICOUDE.EQ.1) THEN
               XB = RAYON*SIN(ANGS2)/ANGS2* (1.D0+
     &           (RMOY*RMOY+EP*EP/4.D0)/ (2.D0*RAYON**2))
               MATINL(1) = RHO*XL* (IY1+ (A1*RAYON**2+3*IZ1)*
     &                  (1.D0/2.D0+SIN(THETA)/ (4.D0*THETA))) -
     &                  ZR(LCASTR)*XB*XB
               MATINL(2) = 0.D0
               MATINL(3) = RHO*XL* (IY1+ (A1*RAYON**2+3*IZ1)*
     &                  (1.D0/2.D0-SIN(THETA)/ (4.D0*THETA)))
               MATINL(4) = 0.D0
               MATINL(5) = 0.D0
               MATINL(6) = RHO*XL*(A1*RAYON**2+3*IZ1)-ZR(LCASTR)*XB*XB
               CALL UTPSLG(NNO,NC,PGL3,MATINL,MATINE)
            ELSE
               MATINL(1) = RHO* (IY1+IZ1)*XL
               MATINL(2) = 0.D0
               MATINL(3) = RHO*XL* (IY1+A1*XL2/12.D0)
               MATINL(4) = 0.D0
               MATINL(5) = 0.D0
               MATINL(6) = RHO*XL* (IZ1+A1*XL2/12.D0)
               CALL UTPSLG(NNO,NC,PGL,MATINL,MATINE)
            END IF
         END IF

C     --- ORIENTATION DE LA POUTRE ---
         IF (NOMTE.EQ.'MECA_POU_C_T') THEN
            CALL JEVECH('PCAARPO','L',LRCOU)
            RAD = ZR(LRCOU)
            ANGARC = ZR(LRCOU+1)
            XFL = ZR(LRCOU+2)
            XFLY = XFL
            XFLZ = XFL
            IF (XFL.EQ.ZERO) THEN
               XFLY = ZR(LRCOU+4)
               XFLZ = ZR(LRCOU+6)
            END IF
            ANGS2 = TRIGOM('ASIN', XL/ (2.D0*RAD) )
            ANG = ANGS2*2.D0
            XL = RAD*ANG
            IY1 = IY1/XFLY
            IZ1 = IZ1/XFLZ
            ANGL(1) = ZR(LORIEN)
            ANGL(2) = ZR(LORIEN+1)
            ANGL(3) = ANGARC
            CALL MATROT(ANGL,P1)
            ANGL(1) = 0.D0
            ANGL(2) = 0.D0
            ANGL(3) = ZR(LORIEN+2)
            CALL MATROT(ANGL,P2)
            CALL PMAT(3,P2,P1,P3)
         END IF

C     --- CALCUL DES CARACTERISTIQUES ELEMENTAIRES 'MASS_INER' ----
         MATINL(3) = IY1
         MATINL(6) = IZ1
         XL2 = XL*XL

C        --- POUTRE A SECTION CONSTANTE ---
         IF (ITYPE.EQ.0) THEN
C           -------- MASSE
            IF( NOMTE.EQ.'MECA_POU_D_EM') THEN
               DO 15 I = 1,6
                  CASECT(I) = ZERO
   15          CONTINUE
C              BOUCLE SUR LES GROUPES DE FIBRE
               IPOS=JACF
               DO 100 IG=1,NBGF
                  NUGF=ZI(INBF+1+IG)
                  ICP=ISDCOM-1+(NUGF-1)*6
                  READ(ZK16(ICP+6),'(I16)')NBFIG
                  MATERI=ZK16(ICP+2)(1:8)
C                 CALCUL DES CARACTERISTIQUES DU GROUPE ---
                  CALL PMFITG(NBFIG,NCARFI,ZR(IPOS),CASEC1)
C                 ON MULTIPLIE PAR RHO (CONSTANT SUR LE GROUPE)
                  CALL RCVALA(ZI(LMATER),MATERI,'ELAS',0,' ',
     &                     ZERO,1,'RHO',VAL,CODRES,'  ')
                  IF ( CODRES .EQ. 'NO' ) THEN
                     CALL RCVALA(ZI(LMATER),MATERI,'ELAS_FLUI',0,' ',
     &                        ZERO,1,'RHO',VAL,CODRES,'FM')
                  ENDIF
                  DO 25 I = 1,6
                     CASECT(I) = CASECT(I) + VAL*CASEC1(I)
25                CONTINUE
                  IPOS=IPOS+NBFIG*NCARFI
100            CONTINUE

               ZR(LCASTR) = CASECT(1)*XL
C           -------- CDG
               ZR(LCASTR+1) = (ZR(LX+4)+ZR(LX+1))/2.D0
               ZR(LCASTR+2) = (ZR(LX+5)+ZR(LX+2))/2.D0
               ZR(LCASTR+3) = (ZR(LX+6)+ZR(LX+3))/2.D0
C           -------- INERTIE
               MATINL(1) = (CASECT(4)+CASECT(5))*XL
               MATINL(2) = 0.D0
               MATINL(3) = XL*CASECT(5)+ CASECT(1)*XL*XL2/12.D0
               MATINL(4) = 0.D0
               MATINL(5) = 0.D0
               MATINL(6) = XL*CASECT(4)+ CASECT(1)*XL*XL2/12.D0
            ELSE
               ZR(LCASTR) = RHO*A1*XL
C           -------- CDG
               ZR(LCASTR+1) = (ZR(LX+4)+ZR(LX+1))/2.D0
               ZR(LCASTR+2) = (ZR(LX+5)+ZR(LX+2))/2.D0
               ZR(LCASTR+3) = (ZR(LX+6)+ZR(LX+3))/2.D0
C           -------- INERTIE
               MATINL(1) = RHO* (IY1+IZ1)*XL
               MATINL(2) = 0.D0
               MATINL(3) = RHO*XL* (IY1+A1*XL2/12.D0)
               MATINL(4) = 0.D0
               MATINL(5) = 0.D0
               MATINL(6) = RHO*XL* (IZ1+A1*XL2/12.D0)
            ENDIF
            CALL UTPSLG(NNO,NC,PGL,MATINL,MATINE)

C        --- POUTRE A SECTION VARIABLE AFFINE ---
         ELSE IF (ITYPE.EQ.1) THEN
            IF ((ABS(A1- (4.D0*RY1*RZ1)).GT. (A1*PREC)) .OR.
     &          (ABS(A2- (4.D0*RY2*RZ2)).GT. (A2*PREC))) THEN
               CALL U2MESS('F','ELEMENTS2_81')
            END IF
C           -------- MASSE
            ZR(LCASTR) = RHO*XL* (A1+A2)/2.D0
C           -------- CDG
            XISL = (RZ1+2.D0*RZ2)/ (3.D0* (RZ1+RZ2))
            ZR(LCASTR+1) = ZR(LX+1) + (ZR(LX+4)-ZR(LX+1))*XISL
            ZR(LCASTR+2) = ZR(LX+2) + (ZR(LX+5)-ZR(LX+2))*XISL
            ZR(LCASTR+3) = ZR(LX+3) + (ZR(LX+6)-ZR(LX+3))*XISL
C           -------- INERTIE
            XA = XL* (RZ1+RZ2)
            AMB = RZ1 - RZ2
            APB = RZ1 + RZ2
            AB2 = RZ1**2 + RZ2**2 + 4.D0*RZ1*RZ2
            AB3 = RZ1**3 + 3.D0*RZ1**2*RZ2 - 3.D0*RZ1*RZ2**2 - RZ2**3
            AB4 = RZ1**4 + RZ2**4 + 2.D0*RZ1*RZ2* (RZ1**2+RZ2**2)
C     ------------------------------------------------------------------
            XIXX = XL* (4.D0*AB4-2.D0*AMB*AB3+AMB**2*AB2)/ (18.D0*APB)
            XIZZ = XL**3*AB2/ (18.D0*APB)
            XIXZ = XL**2* (AB3-AMB*AB2)/ (18.D0*APB)
            XIG = RHO* ((XA*2.D0*RY1**3/3.D0)+2.D0*RY1*XIXX)
            YIG = RHO* (2.D0*RY1* (XIXX+XIZZ))
            ZIG = RHO* ((XA*2.D0*RY1**3/3.D0)+2.D0*RY1*XIZZ)
            XZIG = RHO* (2.D0*RY1*XIXZ)
            MATINL(1) = XIG
            MATINL(2) = 0.D0
            MATINL(3) = YIG
            MATINL(4) = XZIG
            MATINL(5) = 0.D0
            MATINL(6) = ZIG
            CALL UTPSLG(NNO,NC,PGL,MATINL,MATINE)

C        --- POUTRE A SECTION VARIABLE HOMOTHETIQUE ---
         ELSE IF (ITYPE.EQ.2) THEN
            IF (A1.EQ.0.D0) THEN
               CALL U2MESS('F','ELEMENTS2_82')
            END IF
            IF (A1.EQ.A2) THEN
C             SI A1 = A2 LA SECTION EST CONSTANTE.
C             ON NE DEVRAIT DONC PAS PASSER PAR CETTE BRANCHE
               CALL U2MESS('F','ELEMENTS2_83')
            END IF
C           -------- MASSE
            ZR(LCASTR) = RHO* (A1+A2+SQRT(A1*A2))*XL/3.D0
C           -------- CDG
            RR = SQRT(A2/A1)
            UNPRR = 1.D0 + RR + RR**2
            XI = (1.D0+2.D0*RR+3.D0*RR**2)/ (4.D0*UNPRR)
            ZR(LCASTR+1) = ZR(LX+1)* (1.D0-XI) + ZR(LX+4)*XI
            ZR(LCASTR+2) = ZR(LX+2)* (1.D0-XI) + ZR(LX+5)*XI
            ZR(LCASTR+3) = ZR(LX+3)* (1.D0-XI) + ZR(LX+6)*XI
C           -------- INERTIE
            UNPR4 = UNPRR + RR**3 + RR**4
            UNPR2 = 1.D0 + 3.D0*RR + 6.D0*RR**2
            PO    = RHO*XL*A1*UNPRR/3.D0
            XIG   = RHO*XL*(IY1+IZ1)*UNPR4/5.D0
            POXI2 = RHO*XL**3*A1*UNPR2/30.D0 - PO*XI**2*XL**2
            YIG   = RHO*XL*IY1*UNPR4/5.D0 + POXI2
            ZIG   = RHO*XL*IZ1*UNPR4/5.D0 + POXI2
            MATINL(1) = XIG
            MATINL(2) = 0.D0
            MATINL(3) = YIG
            MATINL(4) = 0.D0
            MATINL(5) = 0.D0
            MATINL(6) = ZIG
            CALL UTPSLG(NNO,NC,PGL,MATINL,MATINE)

C        --- POUTRE COURBE ---
         ELSE IF (ITYPE.EQ.10) THEN
C           -------- MASSE
            ZR(LCASTR) = RHO*A1*XL
C           -------- CDG
            CDGL(1) = 0.D0
            CDGL(2) = RAD* (SIN(ANGS2)/ANGS2-COS(ANGS2))
            CDGL(3) = 0.D0
            N1 = 1
            N2 = 3
            CALL UTPVLG(N1,N2,P3,CDGL,CDG)
            ZR(LCASTR+1) = CDG(1) + (ZR(LX+4)+ZR(LX+1))/2.D0
            ZR(LCASTR+2) = CDG(2) + (ZR(LX+5)+ZR(LX+2))/2.D0
            ZR(LCASTR+3) = CDG(3) + (ZR(LX+6)+ZR(LX+3))/2.D0
C           -------- INERTIE
            MATINL(1) = RHO* (IY1+IZ1)*XL
            MATINL(2) = 0.D0
            MATINL(3) = RHO*XL* (IY1+A1*XL2/12.D0)
            MATINL(4) = 0.D0
            MATINL(5) = 0.D0
            MATINL(6) = RHO*XL* (IZ1+A1*XL2/12.D0)
            CALL UTPSLG(NNO,NC,PGL,MATINL,MATINE)
        END IF

        ZR(LCASTR+3+1) = MATINE(1)
        ZR(LCASTR+3+2) = MATINE(3)
        ZR(LCASTR+3+3) = MATINE(6)
        ZR(LCASTR+3+4) = MATINE(2)
        ZR(LCASTR+3+5) = MATINE(4)
        ZR(LCASTR+3+6) = MATINE(5)
      ELSE
        CH16 = OPTION
        CALL U2MESK('F','ELEMENTS2_84',1,CH16)
      END IF

      END
