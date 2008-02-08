      SUBROUTINE TE0039(OPTION,NOMTE)
C MODIF ELEMENTS  DATE 08/02/2008   AUTEUR MACOCCO K.MACOCCO 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C TOLE CRP_20
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C IN OPTION    : K16 :  OPTION DE CALCUL
C     'SIEF_ELNO_ELGA_DU_MONDE'
C     'EQUI_ELNO_SIGM'
C     'EQUI_ELGA_SIGM'
C     'EFGE_ELNO_CART'
C     'FORC_NODA'
C     'SIGM_ELNO_CART'
C     'CHAR_MECA_EPSI_R'
C IN NOMTE     : K16 : NOM DU TYPE ELEMENT
C     'MECA_DIS_T_N'    : DISCRET
C     'MECA_DIS_T_L'    : DISCRET
C     'MECA_DIS_TR_N'   : DISCRET
C      MECA_DIS_TR_L'   : DISCRET
C     'MECA_2D_DIS_T_N' : DISCRET
C     'MECA_2D_DIS_T_L' : DISCRET
C     'MECA_2D_DIS_TR_N': DISCRET
C      MECA_2D_DIS_TR_L': DISCRET
C     'MECA_POU_D_E' : POUTRE DROITE D'EULER       (SECTION VARIABLE)
C     'MECA_POU_D_EM': POUTRE DROITE D'EULER (SECTION MULTIFIBRES)
C     'MECA_POU_D_T' : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
C     'MECA_POU_D_TG': POUTRE DROITE DE TIMOSHENKO AVEC GAUCHISSEMENT
C     'MECA_POU_D_TGM':POUTRE DROITE DE TIMOSHENKO AVEC GAUCHISSEMENT
C                      (SECTION MULTIFIBRES)
C     'MECA_POU_C_T' : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)

C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C     ------------------------------------------------------------------
      INTEGER    NBRES
      PARAMETER (NBRES=4)

      CHARACTER*2  BL2,CODRES(NBRES)
      CHARACTER*8  NOMRES(NBRES),NOMPAR,MATERI,NOMAIL
      CHARACTER*16 CH16

      REAL*8   VALRES(NBRES),MATK(78),ALFAY,ALFAZ,XNU,XL,XIZ,XIY2,XIY
      REAL*8   XFLZ,XFLY,XFL,UN,ZERO,DEUX,XJX,A,A2,XIZ2,PM1,PMPB1,PMPB2
      REAL*8   PM2,R1,R2,VALPAR,E,G,EPX,XKY,XKZ,ANG,TRIGOM,ALONG
      REAL*8   PGL(3,3),PGL1(3,3),PGL2(3,3)
      REAL*8   BSM(12,12),DE(12),FS(14),N1,N2,MX1,MX2,MY1,MY2,MZ1,MZ2
      REAL*8   FSS(14)
      REAL*8   RAD,ANGARC,ANGS2
      REAL*8   CARSEC(6), R8BID, ALPHA

      INTEGER  NCC,NNOC,LORIEN,J,IND,LRCOU,LX,IDEFI,NBPAR,LMATER,IN
      INTEGER  LSECT2,LSECT,LSECR,I,IVECTU,ICONTG,NEQ,NC,NNO,ITSEC
      INTEGER  IELEM,IREPE,NDIM,IRET,ICONTN,IADZI,IAZK24
      INTEGER  NEQ1
      INTEGER  INBF,JACF,IPOS,ICP,NBFIG,NBGF,NCARFI,IG,NUGF
      INTEGER  ISDCOM,ICOMPO
      CALL JEMARQ()

      BL2 = '  '
      IELEM = 0
      IREPE = 0
      ZERO  = 0.0D0
      UN    = 1.0D0
      DEUX  = 2.0D0
      IF (NOMTE.EQ.'MECA_POU_D_T') THEN
         IELEM = 1
         NNO   = 2
         NC    = 6
         NEQ   = 12
         NDIM  = 3
      ELSE IF (NOMTE.EQ.'MECA_POU_D_E') THEN
         IELEM = 1
         NNO   = 2
         NC    = 6
         NEQ   = 12
         NDIM  = 3
      ELSE IF (NOMTE.EQ.'MECA_POU_D_EM') THEN
         IELEM = 1
         NNO   = 2
         NC    = 6
         NEQ   = 12
         NDIM  = 3
      ELSE IF (NOMTE.EQ.'MECA_POU_C_T') THEN
         IELEM = 1
         NNO   = 2
         NC    = 6
         NEQ   = 12
         NDIM  = 3
      ELSE IF ( (NOMTE.EQ.'MECA_POU_D_TG') .OR.
     &          (NOMTE.EQ.'MECA_POU_D_TGM') ) THEN
         NNO   = 2
         NC    = 7
         NEQ   = 14
         NDIM  = 3
      ELSE IF (NOMTE.EQ.'MECA_DIS_TR_L') THEN
         IELEM = 1
         NNO   = 2
         NC    = 6
         NEQ   = 12
         NDIM  = 3
      ELSE IF (NOMTE.EQ.'MECA_DIS_TR_N') THEN
         NNO   = 1
         NC    = 6
         NEQ   = 6
         NDIM  = 3
      ELSE IF (NOMTE.EQ.'MECA_DIS_T_L') THEN
         IELEM = 1
         NNO   = 2
         NC    = 3
         NEQ   = 6
         NDIM  = 3
      ELSE IF (NOMTE.EQ.'MECA_DIS_T_N') THEN
         NNO   = 1
         NC    = 3
         NEQ   = 3
         NDIM  = 3
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_TR_L') THEN
         IELEM = 1
         NNO   = 2
         NC    = 3
         NEQ   = 6
         NDIM  = 2
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_TR_N') THEN
         NNO   = 1
         NC    = 3
         NEQ   = 3
         NDIM  = 2
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_T_L') THEN
         IELEM = 1
         NNO   = 2
         NC    = 2
         NEQ   = 4
         NDIM  = 2
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_T_N') THEN
         NNO   = 1
         NC    = 2
         NEQ   = 2
         NDIM  = 2
      ELSE
         CH16 = NOMTE
         CALL U2MESK('F','ELEMENTS2_42',1,CH16)
      END IF

      IF (OPTION.EQ.'SIEF_ELNO_ELGA') THEN
         CALL JEVECH('PCONTRR','L',ICONTG)
         CALL JEVECH('PSIEFNOR','E',IVECTU)
         DO 10 I = 1,NEQ
            ZR(IVECTU-1+I) = ZR(ICONTG-1+I)
10       CONTINUE

      ELSE IF (OPTION.EQ.'SIEF_ELGA_ELNO') THEN
         CALL JEVECH('PCONTRR','L',ICONTN)
         CALL JEVECH('PSIEFGR','E',IVECTU)
         DO 20 I = 1,NEQ
            ZR(IVECTU-1+I) = ZR(ICONTN-1+I)
20       CONTINUE

      ELSE IF ((OPTION.EQ.'PMPB_ELNO_SIEF') .OR.
     &         (OPTION.EQ.'PMPB_ELGA_SIEF')) THEN

         IF (OPTION.EQ.'PMPB_ELNO_SIEF') THEN
            CALL JEVECH('PSIEFNOR','L',ICONTG)
         ELSE
            CALL JEVECH('PCONTRR','L',ICONTG)
         END IF
         CALL JEVECH('PCONTEQ','E',IVECTU)
         CALL JEVECH('PCAGNPO','L',LSECT)
         CALL JEVECH('PCAGEPO','L',LSECR)

C        --- CARACTERISTIQUES GENERALES DES SECTIONS ---
         LSECT = LSECT - 1
C        --- SECTION INITIALE ---
         A     = ZR(LSECT+1)
         XIY   = ZR(LSECT+2)
         XIZ   = ZR(LSECT+3)
         ALFAY = ZR(LSECT+4)
         ALFAZ = ZR(LSECT+5)
         XJX   = ZR(LSECT+8)
C        --- SECTION FINALE ---
         LSECT2 = LSECT + 11
         A2     = ZR(LSECT2+1)
         XIY2   = ZR(LSECT2+2)
         XIZ2   = ZR(LSECT2+3)
C        N1 N2 EFFORT NORMAUX NOEUDS 1 ET 2
C        MX1, MY1, MZ1 MOMENTS DE TORSION ET FLEXION NOEUDS 1
         N1  = ZR(ICONTG)
         N2  = ZR(ICONTG+6)
         MY1 = ZR(ICONTG+4)
         MZ1 = ZR(ICONTG+5)
         MY2 = ZR(ICONTG+10)
         MZ2 = ZR(ICONTG+11)

         LSECR = LSECR - 1
         ITSEC = NINT(ZR(LSECR+13))
         IF (ITSEC.EQ.1) THEN
            CALL U2MESS('A','ELEMENTS2_85')
            PM1   = 0.D0
            PMPB1 = 0.D0
            PMPB2 = 0.D0
            PM2   = 0.D0
         ELSE IF (ITSEC.EQ.2) THEN
C        --- SECTION CIRCULAIRE SECTIONS INITIALE ET FINALE
            R1 = ZR(LSECR+9)
            R2 = ZR(LSECR+11)
C           B3600 : TUYAUX DROITS SOUS PRESSION
C              B3600 : PM = P.R/E + SQRT(MX**2+MY**2+MZ**2)*R/I
C              ICI, ON REMPLACE P.R/E PAR ABS(N/S)
C           B3200 : PAROI DE RESERVOIR. SUR UNE SECTION DE PAROI :
C              B3200 : PM = MAX(TRESCA(SIGMA_MOYEN(I,J,T)))
C              B3200 : PM = MAX(TRESCA(SIGMA_FLEXION(I,J,T)))
C              POUR UN TUYAU MINCE, ON TROUVE, SI E<<R :
C              B3200 : PM = N/S + SQRT(MX**2+MY**2+MZ**2)*R/I
C                      PB NEGLIGEABLE
C           ICI ON CHOISIT LE
C           G3000 :  PM = ABS(N/S)
C                    PMPB = ABS(N/S) + SQRT(MY**2+MZ**2)*R/I
            PM1   = ABS(N1/A)
            PMPB1 = ABS(N1/A) + (R1/XIY)*SQRT(MY1**2+MZ1**2)
            PM2   = ABS(N2/A2)
            PMPB2 = ABS(N2/A2) + (R2/XIY2)*SQRT(MY2**2+MZ2**2)
         ELSE
            CALL U2MESS('A','ELEMENTS2_86')
            PM1   = 0.D0
            PM2   = 0.D0
            PMPB1 = 0.D0
            PMPB2 = 0.D0
         END IF
         ZR(IVECTU) = PM1
         ZR(IVECTU+1) = PMPB1
         ZR(IVECTU+2) = PM2
         ZR(IVECTU+3) = PMPB2

      ELSE

         IF (OPTION.EQ.'SIGM_ELNO_CART') THEN
            CALL JEVECH('PCONTRR','L',ICONTG)
            CALL JEVECH('PCONTGL','E',IVECTU)
            NEQ1 = NEQ
            IF ( NOMTE(1:13).EQ.'MECA_POU_D_TG') NEQ1 = 12
            DO 30 IN = 1,NEQ1
               FS(IN) = ZR(ICONTG+IN-1)
30          CONTINUE

        ELSE IF (OPTION.EQ.'EFGE_ELNO_CART') THEN
            CALL JEVECH('PCONTRR','L',ICONTG)
            CALL JEVECH('PEFFORR','E',IVECTU)
            DO 40 IN = 1,NEQ
               FS(IN) = ZR(ICONTG+IN-1)
40          CONTINUE
            CALL TECACH('NNN','PCAORIR',1,IREPE,IRET)
         ELSE IF (OPTION.EQ.'FORC_NODA') THEN
            CALL JEVECH('PCONTMR','L',ICONTG)
            CALL JEVECH('PVECTUR','E',IVECTU)
            IF (IELEM.EQ.0) THEN
               DO 50 IN = 1,NEQ
                  FS(IN) = ZR(ICONTG+IN-1)
50             CONTINUE
            ELSE
               DO 60 IN = 1,NC
                  FS(IN)    = -ZR(ICONTG+IN-1)
                  FS(IN+NC) = ZR(ICONTG+IN+NC-1)
60             CONTINUE
            END IF
         ELSE IF (OPTION.EQ.'CHAR_MECA_EPSI_R') THEN

C           --- CARACTERISTIQUES MATERIAUX ---
            CALL JEVECH('PMATERC','L',LMATER)
            NBPAR  = 0
            NOMPAR = '  '
            VALPAR = ZERO
            DO 70 I = 1,NBRES
               VALRES(I) = ZERO
70          CONTINUE
            NOMRES(1) = 'E'
            NOMRES(2) = 'NU'
            NOMRES(3) = 'ALPHA'
            NOMRES(4) = 'RHO'

            CALL RCVALA(ZI(LMATER),' ','ELAS',NBPAR,NOMPAR,VALPAR,2,
     &                  NOMRES,VALRES,CODRES,'FM')
            CALL RCVALA(ZI(LMATER),' ','ELAS',NBPAR,NOMPAR,VALPAR,2,
     &                  NOMRES(3),VALRES(3),CODRES(3),BL2)
            IF (CODRES(3).NE.'OK') VALRES(3) = ZERO
            IF (CODRES(4).NE.'OK') VALRES(4) = ZERO
            E   = VALRES(1)
            XNU = VALRES(2)
            G = E/ (DEUX* (UN+XNU))

C           --- CARACTERISTIQUES GENERALES DES SECTIONS ---
            CALL JEVECH('PCAGNPO','L',LSECT)
            LSECT = LSECT - 1
C           --- SECTION INITIALE ---
            A     = ZR(LSECT+1)
            XIY   = ZR(LSECT+2)
            XIZ   = ZR(LSECT+3)
            ALFAY = ZR(LSECT+4)
            ALFAZ = ZR(LSECT+5)
            XJX   = ZR(LSECT+8)

            IF ( (NOMTE.NE.'MECA_POU_D_TG') .AND.
     &           (NOMTE.NE.'MECA_POU_D_TGM') ) THEN
C              --- SECTION FINALE ---
               LSECT2 = LSECT + 11
               A2     = ZR(LSECT2+1)
               XIY2   = ZR(LSECT2+2)
               XIZ2   = ZR(LSECT2+3)
            END IF

            CALL JEVECH('PEPSINR','L',IDEFI)

            CALL JEVECH('PVECTUR','E',IVECTU)

            EPX = ZR(IDEFI)
            XKY = ZR(IDEFI+1)
            XKZ = ZR(IDEFI+2)

            IF (NOMTE.NE.'MECA_POU_C_T') THEN
               FS(1) = E*A*EPX
               FS(2) = 0.D0
               FS(3) = 0.D0
               FS(4) = 0.D0
               FS(5) = E*XIY*XKY
               FS(6) = E*XIZ*XKZ

               IF ( (NOMTE.EQ.'MECA_POU_D_TG').OR.
     &              (NOMTE.EQ.'MECA_POU_D_TGM') )THEN
                  FS( 7) = 0.D0
                  FS( 8) = E*A*EPX
                  FS( 9) = 0.D0
                  FS(10) = 0.D0
                  FS(11) = 0.D0
                  FS(12) = E*XIY*XKY
                  FS(13) = E*XIZ*XKZ
                  FS(14) = 0.D0
               ELSE IF (NOMTE.EQ.'MECA_POU_D_EM') THEN
C     --- RECUPERATION DES CARACTERISTIQUES DES FIBRES :
                  CALL PMFITX(ZI(LMATER),1,CARSEC,R8BID)
                  FS(1) = CARSEC(1)*EPX
                  FS(2) = 0.D0
                  FS(3) = 0.D0
                  FS(4) = 0.D0
                  FS(5) = CARSEC(5)*XKY
                  FS(6) = CARSEC(4)*XKZ
                  FS(7) = FS(1)
                  FS(8) = 0.D0
                  FS(9) = 0.D0
                  FS(10) = 0.D0
                  FS(11) = FS(5)
                  FS(12) = FS(6)
               ELSE
                  FS( 7) = E*A2*EPX
                  FS( 8) = 0.D0
                  FS( 9) = 0.D0
                  FS(10) = 0.D0
                  FS(11) = E*XIY2*XKY
                  FS(12) = E*XIZ2*XKZ
               END IF
               FS(1) = -FS(1)
               FS(2) = -FS(2)
               FS(3) = -FS(3)
               FS(4) = -FS(4)
               FS(5) = -FS(5)
               FS(6) = -FS(6)

            ELSE IF (NOMTE.EQ.'MECA_POU_C_T') THEN
C              --- COORDONNEES DES NOEUDS ---
               CALL JEVECH('PGEOMER','L',LX)
               LX = LX - 1
               XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2 +
     &                    (ZR(LX+5)-ZR(LX+2))**2 +
     &                    (ZR(LX+6)-ZR(LX+3))**2 )
               IF (XL.EQ.ZERO) THEN
                 CALL TECAEL(IADZI,IAZK24)
                 NOMAIL = ZK24(IAZK24-1+3)(1:8)
                 CALL U2MESK('F','ELEMENTS2_43',1,NOMAIL)
               END IF
               CALL JEVECH('PCAARPO','L',LRCOU)
               RAD = ZR(LRCOU)
               ANGARC = ZR(LRCOU+1)
               XFL  = ZR(LRCOU+2)
               XFLY = XFL
               XFLZ = XFL
               IF (XFL.EQ.ZERO) THEN
                  XFLY = ZR(LRCOU+4)
                  XFLZ = ZR(LRCOU+6)
               END IF
               ANGS2 = TRIGOM('ASIN',XL/ (DEUX*RAD))
               ANG  = ANGS2*DEUX
               XL   = RAD*ANG
               XIY  = XIY/XFLY
               XIZ  = XIZ/XFLZ
               XIY2 = XIY2/XFLY
               XIZ2 = XIZ2/XFLZ

               CALL PTKA10(MATK,E,A,XIY,XIZ,XJX,G,ALFAY,ALFAZ,RAD,ANG,1)

C              --- REMPLISSAGE DE LA MATRICE CARREE
               IND = 0
               DO 90 I = 1,12
                  FS(I) = ZERO
                  DE(I) = ZERO
                  DO 80 J = 1,I - 1
                     IND = IND + 1
                     BSM(I,J) = MATK(IND)
                     BSM(J,I) = MATK(IND)
80                CONTINUE
                  IND = IND + 1
                  BSM(I,I) = MATK(IND)
90             CONTINUE
               ALONG = DEUX*RAD*EPX*SIN(ANGS2)
               DE(1) = -ALONG*COS(ANGS2)
               DE(2) = ALONG*SIN(ANGS2)
               DE(7) = -DE(1)
               DE(8) = DE(2)
C              --- CALCUL DES FORCES INDUITES ---
               DO 110 I = 1,6
                  FS(I) = 0.D0
                  FS(I+6) = 0.D0
                  DO 100 J = 1,6
                     FS(I)   = FS(I) + BSM(I,J)*DE(J)
                     FS(I+6) = FS(I+6) + BSM(I+6,J+6)*DE(J+6)
100               CONTINUE
110            CONTINUE
            END IF
         ELSE
            CH16 = OPTION
            CALL U2MESK('F','ELEMENTS2_47',1,CH16)
         END IF
C        --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA
         CALL JEVECH('PCAORIE','L',LORIEN)

C        --- MATRICE DE ROTATION MGL
         IF (NOMTE.EQ.'MECA_POU_C_T') THEN
C           --- POUTRE COURBE DE TIMOSKENKO A 6 DDL
C           --- COORDONNEES DES NOEUDS
            CALL JEVECH('PGEOMER','L',LX)
            LX = LX - 1
            XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2 +
     &                 (ZR(LX+5)-ZR(LX+2))**2 +
     &                 (ZR(LX+6)-ZR(LX+3))**2 )
            IF (XL.EQ.ZERO) THEN
               CH16 = ' ?????????'
               CALL U2MESK('F','ELEMENTS2_43',1,CH16(:8))
            END IF
            CALL JEVECH('PCAARPO','L',LRCOU)
            RAD    = ZR(LRCOU)
            ANGARC = ZR(LRCOU+1)
            ANGS2  = TRIGOM('ASIN',XL/ (DEUX*RAD))
            CALL MATRO2(ZR(LORIEN),ANGARC,ANGS2,PGL1,PGL2)
            NNOC = 1
            NCC  = 6
            CALL UTPVLG(NNOC,NCC,PGL1,FS,ZR(IVECTU))
            CALL UTPVLG(NNOC,NCC,PGL2,FS(7),ZR(IVECTU+6))
            IF (IREPE.NE.0) THEN
               CALL MATRO2(ZR(IREPE),ANGARC,ANGS2,PGL1,PGL2)
               CALL UTPVLG(NNOC,NCC,PGL1,ZR(IVECTU),ZR(IVECTU))
               CALL UTPVLG(NNOC,NCC,PGL2,ZR(IVECTU+6),ZR(IVECTU+6))
            END IF
         ELSE
            CALL MATROT(ZR(LORIEN),PGL)
            IF (NDIM.EQ.3) THEN
               IF ( NOMTE(1:13).EQ.'MECA_POU_D_TG' .AND. 
     &              OPTION.EQ.'EFGE_ELNO_CART') THEN     
                        CALL UTPVLG(NNO,NC,PGL,FS,FSS)
                     IF (IREPE.NE.0) THEN
                        CALL MATROT(ZR(IREPE),PGL)
                        CALL UTPVLG(NNO,NC,PGL,FSS,FSS)
                     END IF
                     DO 120 IN=1,6
                        ZR(IVECTU-1+IN)   = FSS(IN) 
                        ZR(IVECTU-1+IN+6) = FSS(IN+7) 
120                  CONTINUE
               ELSE IF ( NOMTE(1:13).EQ.'MECA_POU_D_TG' .AND. 
     &              OPTION.EQ.'SIGM_ELNO_CART') THEN     
                        CALL UTPVLG(NNO,6,PGL,FS,FSS)
                     IF (IREPE.NE.0) THEN
                        CALL MATROT(ZR(IREPE),PGL)
                        CALL UTPVLG(NNO,6,PGL,FSS,FSS)
                     END IF
                     DO 130 IN=1,6
                        ZR(IVECTU-1+IN)   = FSS(IN) 
                        ZR(IVECTU-1+IN+6) = FSS(IN+6) 
130                  CONTINUE
              ELSE      
                 CALL UTPVLG(NNO,NC,PGL,FS,ZR(IVECTU))
                 IF (IREPE.NE.0) THEN
                    CALL MATROT(ZR(IREPE),PGL)
                    CALL UTPVLG(NNO,NC,PGL,ZR(IVECTU),ZR(IVECTU))
                 END IF
              ENDIF  
            ELSE IF (NDIM.EQ.2) THEN
               CALL UT2VLG(NNO,NC,PGL,FS,ZR(IVECTU))
               IF (IREPE.NE.0) THEN
                  CALL MATROT(ZR(IREPE),PGL)
                  CALL UT2VLG(NNO,NC,PGL,ZR(IVECTU),ZR(IVECTU))
               END IF
            END IF
         END IF
      END IF
      CALL JEDEMA()
      END
