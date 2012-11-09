      SUBROUTINE TE0517(OPTION,NOMTE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C
C ======================================================================
C     CALCUL DES OPTIONS POUR L'ELEMENT POU_D_TGM (MULTI-FIBRES)
C        VARI_ELNO
C        FORC_NODA
C
C     CALCUL DES OPTIONS POUR L'ELEMENT POU_D_EM (MULTI-FIBRES)
C        FORC_NODA
C
C IN  OPTION : OPTION DE CALCUL
C IN  NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
C
      INTEGER NC,NNO
      INTEGER CODRES(2)
      CHARACTER*2 NOMRES(2)

      REAL*8 PGL(3,3),FL(14),XIY,XIZ
      REAL*8 NX,TY,TZ,MX,MY,MZ

      INTEGER NBFIB,KP,NCOMP,I,JACF
      INTEGER ICOMPO,ICGP,ICONTN,IORIEN,IVECTU
      INTEGER JTAB(7), LX,INO,ISTRXR,ISTRXM,NBSP
C
      INTEGER IDEPLM,IDEPLP,IGEOM,IRET,ISECT,IMATE,K,NPG,IFGM
      REAL*8  UTG(14),XUG(6),XD(3),ANG1(3),DDOT,EY,EZ,TEMP
      REAL*8  XL,XL2,TET1,TET2,ALFA1,BETA1,GAMMA1,GAMMA,VALRES(2)
      REAL*8  XLS2,D1B(7,14),CO(3),AA,E,NU,G,ALFAY,ALFAZ,PHIY,PHIZ
      REAL*8  FORREF,MOMREF
      LOGICAL REACTU

C ----------------------------------------------------------------------
      NNO = 2

C     NOMBRE DE COMPOSANTES DES CHAMPS PSTRX? PAR POINTS DE GAUSS
      IF(NOMTE.EQ.'MECA_POU_D_EM')THEN
         NCOMP  = 15
      ELSEIF(NOMTE.EQ.'MECA_POU_D_TGM')THEN
         NCOMP=18
      ENDIF

      IF ( OPTION .EQ. 'REFE_FORC_NODA  ' ) THEN
         IF(NOMTE.EQ.'MECA_POU_D_EM')THEN
            NC  = 6
         ELSEIF(NOMTE.EQ.'MECA_POU_D_TGM')THEN
            NC  = 7
         ENDIF
         CALL JEVECH ( 'PVECTUR','E',IVECTU)

         CALL TEREFE('EFFORT_REFE','MECA_POUTRE',FORREF)
         CALL TEREFE('MOMENT_REFE','MECA_POUTRE',MOMREF)

         DO 501 INO=1,NNO
            DO 503  I=1,3
               ZR(IVECTU+(INO-1)*NC+I-1)=FORREF
503         CONTINUE
            DO 502 I=4,NC
               ZR(IVECTU+(INO-1)*NC+I-1)=MOMREF
502         CONTINUE
501      CONTINUE

      ELSE
        NC  = 7
C       --- RECUPERATION DES CARACTERISTIQUES DES FIBRES
        CALL JEVECH('PNBSP_I','L',I)
        NBFIB = ZI(I)
        CALL JEVECH('PFIBRES','L',JACF)

C       --- NOMBRE DE POINT DE GAUSS
        NPG = 3
      ENDIF

C     --------------------------------------
      IF ( OPTION .EQ. 'VARI_ELNO' ) THEN
C  CETTE OPTION EXISTE DANS LE CATALOGUE :
C  A QUOI SERT-ELLE, POUR CET ELEMENT ?
         CALL U2MESS('F','ELEMENTS4_16')

C     --------------------------------------
C     --------------------------------------
      ELSEIF ( OPTION .EQ. 'FORC_NODA') THEN

         CALL JEVECH('PCAORIE','L',IORIEN)

         IF(NOMTE.EQ.'MECA_POU_D_EM') THEN
            NNO = 2
            NC  = 6
            CALL JEVECH('PGEOMER','L',IGEOM)

            IF(OPTION.EQ.'FORC_NODA') THEN
               CALL TECACH('OON','PCONTMR',7,JTAB,IRET)
               NBSP=JTAB(7)
               IF (NBSP.NE.NBFIB) CALL U2MESS('F','ELEMENTS_4')
               CALL JEVECH ( 'PSTRXMR','L',ISTRXR)
            ELSE
               CALL TECACH('OON','PCONTRR',7,JTAB,IRET)
               NBSP=JTAB(7)
               IF (NBSP.NE.NBFIB) CALL U2MESS('F','ELEMENTS_4')
               CALL JEVECH ( 'PSTRXRR','L',ISTRXR)
            ENDIF
C ---       LONGUEUR DE L'ELEMENT ---
            LX = IGEOM - 1
            XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2 +
     &                 (ZR(LX+5)-ZR(LX+2))**2 +
     &                 (ZR(LX+6)-ZR(LX+3))**2 )
            XL2 = XL/2.D0

            NX=ZR(ISTRXR-1+1)
            TY=ZR(ISTRXR-1+2)
            TZ=ZR(ISTRXR-1+3)
            MX=ZR(ISTRXR-1+4)
            MY=ZR(ISTRXR-1+5)
            MZ=ZR(ISTRXR-1+6)

C ---       ET ENFIN LE VECTEUR NODAL

            FL(7) = NX
            FL(8) = TY
            FL(9) = TZ
            FL(10) = MX
            DO 10 I = 1,4
               FL(I) = -FL(I+6)
10          CONTINUE
            FL(5) = -MY + TZ*XL2
            FL(6) = -MZ - TY*XL2
            FL(11) = MY + TZ*XL2
            FL(12) = MZ - TY*XL2
C
            IF(OPTION.EQ.'FORC_NODA') THEN
C           --- FORC_NODA : PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
               CALL MATROT(ZR(IORIEN),PGL)
               CALL JEVECH('PVECTUR','E',IVECTU)
               CALL UTPVLG(NNO,NC,PGL,FL,ZR(IVECTU))
            ELSE
               CALL JEVECH('PEFFORR','E',ICONTN)
               DO 20 I = 1,12
                  ZR(ICONTN+I-1) = FL(I)
20             CONTINUE
            ENDIF
         ELSE
            CALL JEVECH('PVECTUR','E',IVECTU)
            CALL JEVECH('PCONTMR','L',ICGP)
            CALL JEVECH('PSTRXMR','L',ISTRXM)
C           --- IL FAUT TENIR COMPTE DU CHGT DE GEOMETRIE
C           --- POUR CALCULER LA MATRICE DE PASSAGE
            CALL JEVECH('PCOMPOR','L',ICOMPO)
            REACTU = (ZK16(ICOMPO+2).EQ.'GROT_GDEP')
            CALL JEVECH('PGEOMER','L',IGEOM)

            IF ( REACTU ) THEN
               CALL TECACH('ONN','PDEPLMR',1,IDEPLM,IRET)
               IF ( IRET .NE. 0 ) THEN
                  DO 100 I = 1,14
                     UTG(I) = 0.D0
100               CONTINUE
               ELSE
                  DO 102 I = 1,14
                     UTG(I) = ZR(IDEPLM-1+I)
102               CONTINUE
               ENDIF
               CALL TECACH('ONN','PDEPLPR',1,IDEPLP,IRET)
               IF ( IRET .EQ. 0 ) THEN
                  DO 104 I = 1,14
                     UTG(I) = UTG(I) + ZR(IDEPLP-1+I)
104               CONTINUE
               ENDIF
               DO 110 I = 1,3
                  XUG(I) = UTG(I) + ZR(IGEOM-1+I)
                  XUG(I+3) = UTG(I+7) + ZR(IGEOM-1+I+3)
110            CONTINUE
               CALL VDIFF(3,XUG(4),XUG(1),XD)
               XL2=DDOT(3,XD,1,XD,1)
               XL = SQRT(XL2)
               TET1=DDOT(3,UTG(4),1,XD,1)
               TET2=DDOT(3,UTG(11),1,XD,1)
               TET1 = TET1/XL
               TET2 = TET2/XL
               CALL ANGVX(XD,ALFA1,BETA1)
               GAMMA = ZR(ISTRXM+18-1)
               GAMMA1 = GAMMA + (TET1+TET2)/2.D0
               ANG1(1) = ALFA1
               ANG1(2) = BETA1
               ANG1(3) = GAMMA1
            ELSE
               CALL VDIFF(3,ZR(IGEOM-1+4),ZR(IGEOM),XD)
               XL2=DDOT(3,XD,1,XD,1)
               XL = SQRT(XL2)
               ANG1(1) = ZR(IORIEN+0)
               ANG1(2) = ZR(IORIEN+1)
               ANG1(3) = ZR(IORIEN+2)
            END IF
            CALL MATROT(ANG1,PGL)
            CALL JEVECH('PCAGNPO','L',ISECT)

C           -- CARACTERISTIQUES DE LA SECTION
            AA    = ZR(ISECT)
            XIY   = ZR(ISECT + 1)
            XIZ   = ZR(ISECT + 2)
            ALFAY = ZR(ISECT + 3)
            ALFAZ = ZR(ISECT + 4)
C           -- PASSAGE DE G (CENTRE DE GRAVITE) A C (CENTRE DE TORSION)
            EY = -ZR(ISECT + 5)
            EZ = -ZR(ISECT + 6)

            CALL JEVECH('PMATERC','L',IMATE)
            CALL MOYTEM('RIGI',NPG,1,'+',TEMP,IRET)
            NOMRES(1) = 'E'
            NOMRES(2) = 'NU'
            CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ','ELAS',
     &              0,'TEMP',TEMP,2,
     &              NOMRES, VALRES, CODRES, 1)
            E = VALRES(1)
            NU = VALRES(2)
            G = E / (2.D0*(1.D0+NU))
            PHIY = E*XIZ*12.D0*ALFAY/ (XL*XL*G*AA)
            PHIZ = E*XIY*12.D0*ALFAZ/ (XL*XL*G*AA)
            XLS2 = 0.5D0 * XL
C           POIDS DES POINTS DE GAUSS
            CO(1) = 5.D0/9.D0
            CO(2) = 8.D0/9.D0
            CO(3) = 5.D0/9.D0




            CALL R8INIR(2*NC,0.D0,FL,1)
            DO 400 KP = 1,3
               CALL JSD1FF(KP,XL,PHIY,PHIZ,D1B)
               IFGM=NCOMP*(KP-1)-1
               DO 410 K = 1,2*NC
                  DO 420 I = 1,NC
                   FL(K)=FL(K) + XLS2*ZR(ISTRXM+IFGM+I)*D1B(I,K)*CO(KP)
420               CONTINUE
410            CONTINUE
400         CONTINUE
            DO 430 I = 1,2
               FL(7*(I-1)+4) = FL(7*(I-1)+4) -
     &                      EZ*FL(7*(I-1)+2) + EY*FL(7*(I-1)+3)
430         CONTINUE
            CALL UTPVLG(NNO,NC,PGL,FL,ZR(IVECTU))
         ENDIF
      ENDIF

      END
