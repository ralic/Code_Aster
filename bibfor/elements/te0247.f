      SUBROUTINE TE0247(OPTION,NOMTE)
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*(*)     OPTION,NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 05/03/2013   AUTEUR CHEIGNON E.CHEIGNON 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CALCUL DES OPTIONS FULL_MECA OU RAPH_MECA POUR
C     LES ELEMENTS DE POUTRE 'MECA_POU_D_E/D_T'
C
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C        'MECA_POU_D_E' : POUTRE DROITE D'EULER       (SECTION VARIABLE)
C        'MECA_POU_D_T' : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
C        'MECA_POU_C_T' : POUTRE COURBE DE TIMOSHENKO
C     ------------------------------------------------------------------
C
C
      INTEGER IGEOM,ICOMPO,IMATE,ISECT,IORIEN,ND,NK
      INTEGER IINSTM,IINSTP,ICARCR,ICONTM,IDEPLM,IDEPLP,IMATUU
      INTEGER IVECTU,ICONTP,ITYPE,NNO,NC,IVARIM,IVARIP,ITEMP,I
      INTEGER JTAB(7),JCRET,KK,LGPG,IRET,IRETM,IRETP,IRET2
      INTEGER NPG,NDIMEL,NNOEL,NNOSEL,IPLOUF
      INTEGER LX,LRCOU,ISTRXM,ISTRXP,LDEP
      PARAMETER    (NNO=2,NC=6,ND=NC*NNO,NK=ND*(ND+1)/2)
      REAL*8       E , NU  , G,    EM,     NUM
      REAL*8       A , XIY , XIZ , ALFAY , ALFAZ , XJX , EZ,  EY
      REAL*8       A2, XIY2, XIZ2, ALFAY2, ALFAZ2, XJX2, XL
C AUTRES
      INTEGER    NBCLMA, NBCLEM
      PARAMETER (NBCLMA=12,NBCLEM=7)
      REAL*8     COELMA(NBCLMA),COELEM(NBCLEM)
      INTEGER CODLMA(NBCLMA),CODLEM(NBCLEM)
      CHARACTER*8  NOMLMA(NBCLMA),NOMLEM(NBCLEM)
      CHARACTER*24 VALK(2)

      REAL*8       PGL(3,3), FL(ND), KLV(NK), KLS(NK), FLC, EFFNOC
      REAL*8       PGL1(3,3), PGL2(3,3), RAD, ANGARC, ANGS2, TRIGOM
      REAL*8       ZERO,DEUX
      REAL*8       XFL, XFLY, XFLZ, ANG
      REAL*8       EFFNOM, TEMPM, TEMPP
      REAL*8       IRRAM,IRRAP,EPSTHE
      REAL*8       SIGMA(ND),RGEOM(NK),GAMMA,ANGP(3)
      LOGICAL      REACTU,MATRIC,VECTEU

      DATA NOMLMA / 'K','N','DE_0','P','P1','P2','M','RM',
     &              'A_0','Y_0','Y_I','B'/
      DATA NOMLEM / 'N', 'UN_SUR_K', 'UN_SUR_M', 'QSR_K',
     &              'BETA','PHI_ZERO','L'/
C     ------------------------------------------------------------------


      ZERO = 0.D0
      DEUX = 2.D0
C
      CALL JEVECH ('PGEOMER','L',IGEOM)
      CALL JEVECH ('PCOMPOR','L',ICOMPO)
      CALL JEVECH ('PMATERC','L',IMATE)
      CALL JEVECH ('PCAGNPO','L',ISECT)
      CALL JEVECH ('PCAORIE','L',IORIEN)
      CALL JEVECH ('PINSTMR','L',IINSTM)
      CALL JEVECH ('PINSTPR','L',IINSTP)
      CALL JEVECH ('PCARCRI','L',ICARCR)
      CALL JEVECH ('PVARIMR','L',IVARIM)
      CALL JEVECH ('PCONTMR','L',ICONTM)
      CALL JEVECH ('PDEPLMR','L',IDEPLM)

      IF (NOMTE.EQ.'MECA_POU_C_T') THEN
         IF(ZK16(ICOMPO)   .NE. 'ELAS' .OR.
     &      ZK16(ICOMPO+2) .NE. 'PETIT' ) THEN
             CALL U2MESS('F','ELEMENTS5_43')
         ENDIF
      ENDIF

C ---- LA PRESENCE DU CHAMP DE DEPLACEMENT A L INSTANT T+
C ---- DEVRAIT ETRE CONDITIONNE  PAR L OPTION (AVEC RIGI_MECA_TANG
C ---- CA N A PAS DE SENS).
C ---- CEPENDANT CE CHAMP EST INITIALISE A 0 PAR LA ROUTINE NMMATR.
      CALL JEVECH ('PDEPLPR','L',IDEPLP)
C
C --- POINT DE GAUSS DE L'ELEMENT
C
      CALL ELREF4(' ','RIGI',NDIMEL,NNOEL,NNOSEL,
     &            NPG,IPLOUF,IPLOUF,IPLOUF,IPLOUF)
      CALL ASSERT( (NPG.EQ.2).OR.(NPG.EQ.3) )

C     -- BOOLEENS PRATIQUES :
      MATRIC = OPTION .EQ. 'FULL_MECA' .OR. OPTION .EQ. 'RIGI_MECA_TANG'
      VECTEU = OPTION .EQ. 'FULL_MECA' .OR. OPTION .EQ. 'RAPH_MECA'
C
C --- PARAMETRES EN SORTIE
C
      IF(MATRIC) THEN
        CALL JEVECH('PMATUUR','E',IMATUU)
      ENDIF

      IF(VECTEU) THEN
         CALL JEVECH('PVECTUR','E',IVECTU)
         CALL JEVECH('PCONTPR','E',ICONTP)
         CALL JEVECH('PVARIPR','E',IVARIP)
      ENDIF
C
      CALL CARAPO(ZR(ISECT), ZR(IGEOM), ZR(IORIEN), XL, PGL,
     &            ITYPE, A,     XIY,  XIZ,    XJX,
     &            ALFAY, ALFAZ, EY,   EZ,     A2,
     &            XIY2,  XIZ2,  XJX2, ALFAY2, ALFAZ2)
C

      IF (ZK16(ICOMPO+2).NE.'PETIT'.AND.
     &    ZK16(ICOMPO+2).NE.'GROT_GDEP')THEN
        VALK(1) = ZK16(ICOMPO+2)
        VALK(2) = NOMTE
        CALL U2MESK('F','ELEMENTS3_40',2,VALK)
      ENDIF
      REACTU =  ZK16(ICOMPO+2) .EQ. 'GROT_GDEP'

      IF ( REACTU .AND.(ZK16(ICOMPO).EQ.'ELAS')) THEN

C        RECUPERATION DU 3EME ANGLE NAUTIQUE AU TEMPS T-
         CALL JEVECH ('PSTRXMR','L',ISTRXM)
         GAMMA = ZR(ISTRXM+3-1)

C        CALCUL DE PGL,XL ET ANGP
         CALL POREA1(NNO,NC,ZR(IDEPLM),ZR(IDEPLP),ZR(IGEOM),GAMMA,
     &               VECTEU,PGL,XL,ANGP)

C        SAUVEGARDE DES ANGLES NAUTIQUES
         IF (VECTEU)THEN
            CALL JEVECH('PSTRXPR','E',ISTRXP)
            ZR(ISTRXP+1-1) = ANGP(1)
            ZR(ISTRXP+2-1) = ANGP(2)
            ZR(ISTRXP+3-1) = ANGP(3)
         ENDIF

      ENDIF
C
      IF (ZK16(ICOMPO).EQ.'ELAS') THEN
C
         CALL MOYTEM('RIGI',NPG,1,'+',TEMPP,IRETP)
         CALL MOYTEM('RIGI',NPG,1,'-',TEMPM,IRETM)
         ITEMP = 0
         IF ((IRETP+IRETM).EQ.0) ITEMP=1
         CALL MATELA(ZI(IMATE),' ',ITEMP,TEMPP,E,NU)
         CALL MATELA(ZI(IMATE),' ',ITEMP,TEMPM,EM,NUM)
         CALL VERIFM('RIGI',NPG,1,'T',ZI(IMATE),'ELAS',1,EPSTHE,IRET)
         G      = E / (2.D0*(1.D0+NU))
C
C        --- CALCUL DES MATRICES ELEMENTAIRES ----
         IF ( ITYPE .EQ. 0 ) THEN
            IF (NOMTE .EQ. 'MECA_POU_D_E') THEN
               ALFAY  = 0.D0
               ALFAZ  = 0.D0
            ENDIF
            CALL PTKA01(KLV, E,   A, XL,    XIY,
     &                  XIZ, XJX, G, ALFAY, ALFAZ,
     &                  EY,  EZ,  1)
         ELSE IF ( ITYPE .EQ. 1 .OR. ITYPE .EQ. 2 ) THEN
            IF (NOMTE .EQ. 'MECA_POU_D_E') THEN
               ALFAY2 = 0.D0
               ALFAZ2 = 0.D0
               ALFAY  = 0.D0
               ALFAZ  = 0.D0
            ENDIF
            CALL PTKA02(ITYPE, KLV,    E,    A,     A2,
     &                  XL,    XIY,    XIY2, XIZ,   XIZ2,
     &                  XJX,   XJX2,   G,    ALFAY, ALFAY2,
     &                  ALFAZ, ALFAZ2, EY,   EZ,    1)

          ELSE IF ( ITYPE .EQ. 10 ) THEN
            IF (NOMTE.EQ.'MECA_POU_C_T') THEN
C        --- POUTRE COURBE DE TIMOSKENKO A 6 DDL ---
               CALL JEVECH('PCAARPO','L',LRCOU)
               RAD = ZR(LRCOU)
               XFL = ZR(LRCOU+2)
               XFLY = XFL
               XFLZ = XFL
               IF (XFL.EQ.ZERO) THEN
                  XFLY = ZR(LRCOU+4)
                  XFLZ = ZR(LRCOU+6)
               END IF
               ANGS2 = TRIGOM('ASIN', XL/(DEUX*RAD) )
               ANG = ANGS2*DEUX
               XL = RAD*ANG
               XIY = XIY/XFLY
               XIZ = XIZ/XFLZ
               LX = IGEOM - 1
               XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2 +
     &                    (ZR(LX+5)-ZR(LX+2))**2 +
     &                    (ZR(LX+6)-ZR(LX+3))**2 )
               ANGARC = ZR(LRCOU+1)
               ANGS2 = TRIGOM('ASIN', XL/(DEUX*RAD) )
               CALL MATRO2(ZR(IORIEN),ANGARC,ANGS2,PGL1,PGL2)
               CALL PTKA10(KLV,E,A,XIY,XIZ,XJX,G,ALFAY,ALFAZ,RAD,ANG,1)
            ENDIF
         ENDIF
C
         IF (OPTION.EQ.'RAPH_MECA'.OR.OPTION.EQ.'FULL_MECA') THEN
            IF ((ITEMP.NE.0).AND.(NU.NE.NUM)) THEN
               CALL U2MESS('A','ELEMENTS3_59')
            ENDIF
            CALL NMPOEL(NOMTE, NPG,        KLV,  XL,     NNO, NC,
     &                  PGL,   PGL1,       PGL2, ZR(IDEPLP), EPSTHE, E,
     &                  EM,    ZR(ICONTM), FL,   ZR(ICONTP),ANGS2,RAD)
         ENDIF
C
      ELSEIF ((ZK16(ICOMPO).EQ.'LMARC_IRRA').OR.
     &        (ZK16(ICOMPO).EQ.'LEMAITRE_IRRA')) THEN
C
         CALL TECACH('OON','PVARIMR','L',7,JTAB,IRET)
         LGPG = MAX(JTAB(6),1)*JTAB(7)
C
C-- RECUPERATION DES CARACTERISTIQUES ELASTIQUES
         CALL MOYTEM('RIGI',NPG,1,'+',TEMPP,IRETP)
         CALL MOYTEM('RIGI',NPG,1,'-',TEMPM,IRETM)
         ITEMP = 0
         IF ((IRETP+IRETM).EQ.0) ITEMP=1
         CALL MATELA(ZI(IMATE),' ',ITEMP,TEMPP,E,NU)
         CALL MATELA(ZI(IMATE),' ',ITEMP,TEMPM,EM,NUM)
         CALL VERIFM('RIGI',NPG,1,'T',ZI(IMATE),'ELAS',1,EPSTHE,IRET)
         G  = E / (2.D0*(1.D0+NU))
C
C        --- CALCUL DES MATRICES ELEMENTAIRES ELASTIQUES ----
         IF ( ITYPE .EQ. 0 ) THEN
            IF (NOMTE .EQ. 'MECA_POU_D_E') THEN
               ALFAY  = 0.D0
               ALFAZ  = 0.D0
            ENDIF
            CALL PTKA01(KLV, E,   A, XL,    XIY,
     &                  XIZ, XJX, G, ALFAY, ALFAZ,
     &                  EY,  EZ,  1)
         ELSE IF ( ITYPE .EQ. 1 .OR. ITYPE .EQ. 2 ) THEN
            IF (NOMTE .EQ. 'MECA_POU_D_E') THEN
               ALFAY2 = 0.D0
               ALFAZ2 = 0.D0
               ALFAY  = 0.D0
               ALFAZ  = 0.D0
            ENDIF
            CALL PTKA02(ITYPE, KLV,    E,    A,     A2,
     &                  XL,    XIY,    XIY2, XIZ,   XIZ2,
     &                  XJX,   XJX2,   G,    ALFAY, ALFAY2,
     &                  ALFAZ, ALFAZ2, EY,   EZ,    1)
         ENDIF
         IF ((ITEMP.NE.0).AND.(NU.NE.NUM)) THEN
            CALL U2MESS('A','ELEMENTS3_59')
         ENDIF
C
         CALL NMPOEL(NOMTE,  NPG,        KLV,   XL,          NNO,   NC,
     &               PGL,    PGL1,       PGL2,  ZR(IDEPLP), EPSTHE, E,
     &               EM,     ZR(ICONTM), FL,    ZR(ICONTP),ANGS2,RAD)
C
C-- CALCUL DE LA MATRICE TANGENTE ET CORRECTION DES TERMES D'ALLONGEMENT
C-- DES VECTEURS FORCES NODALES ET EFFORTS
         IF (VECTEU) THEN
            IF(XL.EQ.0.D0) THEN
               CALL U2MESS('F','ELEMENTS3_60')
            ENDIF
            EFFNOM = ZR(ICONTM)
C
            IF (ZK16(ICOMPO).EQ.'LMARC_IRRA') THEN
               CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ','LMARC_IRRA',
     &              0,' ',0.D0,
     &              12,NOMLMA(1),COELMA(1),CODLMA(1), 1)
               CALL RCVARC(' ','IRRA','-','RIGI',1,1,IRRAM,IRET2)
               IF (IRET2.GT.0) IRRAM=0.D0
               CALL RCVARC(' ','IRRA','+','RIGI',1,1,IRRAP,IRET2)
               IF (IRET2.GT.0) IRRAP=0.D0
               CALL NMLMAB(PGL,NNO,NPG,NC,ZR(IDEPLP),
     &               EFFNOM, TEMPM, TEMPP, ZI(IMATE), ZR(ICARCR),
     &               ZR(IINSTM), ZR(IINSTP), XL, E, A,
     &               COELMA, IRRAM, IRRAP, ZR(IVARIM),
     &               ZR(IVARIP), KLS, FLC, EFFNOC, EM,
     &               IRET)
               DO 52 KK = 1, 4
                  ZR(IVARIP+LGPG+KK-1)   = ZR(IVARIP+KK-1)
                  ZR(IVARIP+2*LGPG+KK-1) = ZR(IVARIP+KK-1)
 52            CONTINUE
               CALL RCVARC(' ','IRRA','+','RIGI',1,1,IRRAP,IRET2)
               IF (IRET2.GT.0) IRRAP=0.D0
               ZR(IVARIP+5-1)         = IRRAP
               CALL RCVARC(' ','IRRA','+','RIGI',2,1,IRRAP,IRET2)
               IF (IRET2.GT.0) IRRAP=0.D0
               ZR(IVARIP+LGPG+5-1)    = IRRAP
               CALL RCVARC(' ','IRRA','+','RIGI',3,1,IRRAP,IRET2)
               IF (IRET2.GT.0) IRRAP=0.D0
               ZR(IVARIP+2*LGPG+5-1)  = IRRAP
C
            ELSEIF (ZK16(ICOMPO).EQ.'LEMAITRE_IRRA') THEN
               CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ',
     &               'LEMAITRE_IRRA',0,' ',0.D0,
     &               7,NOMLEM(1),COELEM(1),CODLEM(1), 1)
               CALL RCVARC(' ','IRRA','-','RIGI',1,1,IRRAM,IRET2)
               IF (IRET2.GT.0) IRRAM=0.D0
               CALL RCVARC(' ','IRRA','+','RIGI',1,1,IRRAP,IRET2)
               IF (IRET2.GT.0) IRRAP=0.D0
               CALL NMFGAS('RIGI',NPG,ZI(IMATE),PGL,NNO,
     &                  NC, ZR(IDEPLP), EFFNOM, ZR(IVARIM), ZR(ICARCR),
     &                  ZR(IINSTM), ZR(IINSTP), XL, A, COELEM,
     &                  IRRAM, IRRAP, KLS, FLC,
     &                  EFFNOC, ZR(IVARIP))
               ZR(IVARIP+1)    = IRRAP
               ZR(IVARIP+LGPG) = ZR(IVARIP)
               CALL RCVARC(' ','IRRA','+','RIGI',2,1,IRRAP,IRET2)
               IF (IRET2.GT.0) IRRAP=0.D0
               ZR(IVARIP+LGPG+1) = IRRAP
               ZR(IVARIP+2*LGPG) = ZR(IVARIP)
               CALL RCVARC(' ','IRRA','+','RIGI',3,1,IRRAP,IRET2)
               IF (IRET2.GT.0) IRRAP=0.D0
               ZR(IVARIP+2*LGPG+1) = IRRAP
            ENDIF
C
C           MODIFICATION DE L'EFFORT NORMAL
            IF ( NPG .EQ. 2 ) THEN
               ZR(ICONTP)    = EFFNOC
               ZR(ICONTP+6)  = EFFNOC
            ELSE
               ZR(ICONTP)    = EFFNOC
               ZR(ICONTP+6)  = EFFNOC
               ZR(ICONTP+12) = EFFNOC
            ENDIF
C
            FL(1) = -FLC
            FL(7) =  FLC
C
            IF (OPTION.EQ.'FULL_MECA') THEN
               KLV(1)  = KLS(1)
               KLV(22) = KLS(22)
               KLV(28) = KLS(28)
            ENDIF
C
         ENDIF
      ELSE
         CALL U2MESK('F','ELEMENTS3_61',1,ZK16(ICOMPO))
      ENDIF
      IF (MATRIC)THEN
C       CALCUL DE LA MATRICE DE RIGIDITE GEOMETRIQUE
        IF ( REACTU .AND.(ZK16(ICOMPO).EQ.'ELAS')) THEN
          IF (OPTION.EQ.'FULL_MECA') THEN
            LDEP = ICONTP
          ELSE
            LDEP = ICONTM
          ENDIF
          IF ( NPG .EQ. 2 ) THEN
            DO 15 I=1,NC
              SIGMA(I)    = ZR(LDEP+I-1)
              SIGMA(I+NC) = ZR(LDEP+NC+I-1)
15          CONTINUE
          ELSE
            DO 17 I=1,NC
              SIGMA(I)    = ZR(LDEP+I-1)
              SIGMA(I+NC) = ZR(LDEP+NC+NC+I-1)
17          CONTINUE
          ENDIF
          IF ( ITYPE.NE.10 )  THEN
            CALL R8INIR(NK,0.0D0,RGEOM,1)
            CALL PTKG00(SIGMA,A,A2,XIZ,XIZ2,XIY,XIY2,XL,EY,EZ,RGEOM)
            CALL LCSOVN(NK,KLV, RGEOM, KLV)
          ELSE
            CALL U2MESS('A','ELEMENTS3_28')
          ENDIF
        ENDIF

      ENDIF


C
C        --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
C
      IF(NOMTE .EQ. 'MECA_POU_C_T' ) THEN
         CALL JEVECH('PGEOMER','L',LX)
         LX = LX - 1
         XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2 +
     &              (ZR(LX+5)-ZR(LX+2))**2 +
     &              (ZR(LX+6)-ZR(LX+3))**2 )
         CALL JEVECH('PCAARPO','L',LRCOU)
         RAD = ZR(LRCOU)
         ANGARC = ZR(LRCOU+1)
         ANGS2 = TRIGOM('ASIN',XL/ (DEUX*RAD))
         CALL MATRO2(ZR(IORIEN),ANGARC,ANGS2,PGL1,PGL2)
      ENDIF
      IF (MATRIC) THEN
         IF(NOMTE .EQ. 'MECA_POU_C_T' ) THEN
            CALL CHGREP('LG',PGL1,PGL2, KLV, ZR(IMATUU))
         ELSE
            CALL UTPSLG ( NNO, NC, PGL, KLV, ZR(IMATUU) )
         ENDIF
      ENDIF
      IF (VECTEU) THEN
         IF(NOMTE .EQ. 'MECA_POU_C_T' ) THEN
            CALL UTPVLG ( 1, 6, PGL1, FL,    ZR(IVECTU) )
            CALL UTPVLG ( 1, 6, PGL2, FL(7), ZR(IVECTU+6) )
         ELSE
            CALL UTPVLG ( NNO, NC, PGL, FL, ZR(IVECTU) )
         ENDIF
         CALL JEVECH ( 'PCODRET', 'E', JCRET )
         ZI(JCRET) = 0
      ENDIF
C
      END
