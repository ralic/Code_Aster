      SUBROUTINE TE0247(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*(*)     OPTION,NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 19/02/2008   AUTEUR FLEJOU J-L.FLEJOU 
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
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER IGEOM,ICOMPO,IMATE,ISECT,IORIEN,ND,NK
      INTEGER IINSTM,IINSTP,ICARCR,ICONTM,IDEPLM,IDEPLP,IMATUU
      INTEGER IVECTU,ICONTP,ITYPE,NNO,NC,IVARIM,IVARIP,ITEMP,I
      INTEGER JTAB(7),JCRET,KK,LGPG,IRET,IRETM,IRETP,IRET2
      INTEGER NPG,NDIMEL,NNOEL,NNOSEL,IPLOUF
      PARAMETER    (NNO=2,NC=6,ND=NC*NNO,NK=ND*(ND+1)/2)
      REAL*8       E , NU  , G,    EM,     NUM
      REAL*8       A , XIY , XIZ , ALFAY , ALFAZ , XJX , EZ,  EY
      REAL*8       A2, XIY2, XIZ2, ALFAY2, ALFAZ2, XJX2, XL
C AUTRES
      INTEGER    NBCLMA, NBCLEM
      PARAMETER (NBCLMA=12,NBCLEM=7)
      REAL*8     COELMA(NBCLMA),COELEM(NBCLEM),DDOT
      CHARACTER*2  CODLMA(NBCLMA),CODLEM(NBCLEM)
      CHARACTER*8  NOMLMA(NBCLMA),NOMLEM(NBCLEM)

C GRANDISSEMENT
      INTEGER    NBCLGR
      PARAMETER (NBCLGR=3)
      REAL*8     COEFGR(NBCLGR)
      CHARACTER*8 NOMGRD(NBCLGR)
      CHARACTER*2 K2B(NBCLGR)

      REAL*8       PGL(3,3), FL(ND), KLV(NK), KLS(NK), FLC, EFFNOC
      REAL*8       EFFNOM, TEMPM, TEMPP, U(ND), DU(ND)
      REAL*8       HOEL(7*7),HOTA(7*7),D1B(7*14),WORK(14*7),RG0(14*14)
      REAL*8       XUG(6),UTG(12),XD(3),XL2,TET1,TET2,ALFA1,BETA1,GAMMA
      REAL*8       GAMMA1,ANG1(3),IRRAM,IRRAP,EPSTHE
      LOGICAL      REACTU

      DATA NOMLMA / 'K','N','DE_0','P','P1','P2','M','RM',
     &              'A_0','Y_0','Y_I','B'/
      DATA NOMLEM / 'N', 'UN_SUR_K', 'UN_SUR_M', 'QSR_K',
     &              'BETA','PHI_ZERO','L'/
      DATA NOMGRD / 'GRAN_A' , 'GRAN_B' , 'GRAN_S' /
C     ------------------------------------------------------------------

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

C ---- LA PRESENCE DU CHAMP DE DEPLACEMENT A L INSTANT T+
C ---- DEVRAIT ETRE CONDITIONNE  PAR L OPTION (AVEC RIGI_MECA_TANG
C ---- CA N A PAS DE SENS).
C ---- CEPENDANT CE CHAMP EST INITIALISE A 0 PAR LA ROUTINE NMMATR.
      CALL JEVECH ('PDEPLPR','L',IDEPLP)
C
      IF ( ZK16(ICOMPO+3) .EQ. 'COMP_ELAS' ) THEN
         CALL U2MESS('F','ELEMENTS2_90')
      ENDIF
C
C --- POINT DE GAUSS DE L'ELEMENT
C
      CALL ELREF4(' ','RIGI',NDIMEL,NNOEL,NNOSEL,
     &            NPG,IPLOUF,IPLOUF,IPLOUF,IPLOUF)
      CALL ASSERT( (NPG.EQ.2).OR.(NPG.EQ.3) )
C
C --- PARAMETRES EN SORTIE
C
      IF( OPTION .EQ. 'RIGI_MECA_TANG') THEN
         CALL JEVECH('PMATUUR','E',IMATUU)
      ELSEIF( OPTION .EQ. 'FULL_MECA' ) THEN
         CALL JEVECH('PMATUUR','E',IMATUU)
         CALL JEVECH('PVECTUR','E',IVECTU)
         CALL JEVECH('PCONTPR','E',ICONTP)
         CALL JEVECH('PVARIPR','E',IVARIP)
      ELSEIF( OPTION .EQ. 'RAPH_MECA' ) THEN
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
      REACTU =  ZK16(ICOMPO+2) .EQ. 'PETIT_REAC'
      IF ( REACTU .AND.
     &      ( (ZK16(ICOMPO).EQ.'ELAS') .OR.
     &        (ZK16(ICOMPO).EQ.'VMIS_POU_LINE').OR.
     &        (ZK16(ICOMPO).EQ.'VMIS_POU_FLEJOU') ) ) THEN
C
         CALL U2MESS('A','ELEMENTS3_58')
C
         DO 511 I = 1,12
            UTG(I)  = ZR(IDEPLM-1+I)+ZR(IDEPLP-1+I)
511      CONTINUE
         DO 512 I = 1,3
            XUG(I)  =ZR(IGEOM-1+I)  + UTG(I)
            XUG(I+3)=ZR(IGEOM-1+I+3)+ UTG(I+6)
512      CONTINUE
         CALL VDIFF(3,XUG(4),XUG(1),XD)
         XL2=DDOT(3,XD,1,XD,1)
         XL = SQRT(XL2)
         TET1=DDOT(3,UTG(4),1,XD,1)
         TET2=DDOT(3,UTG(10),1,XD,1)
         TET1 = TET1/XL
         TET2 = TET2/XL
         CALL ANGVX(XD,ALFA1,BETA1)
         GAMMA = ZR(IORIEN+2)
         GAMMA1 = GAMMA + (TET1+TET2)/2.D0
         ANG1(1) = ALFA1
         ANG1(2) = BETA1
         ANG1(3) = GAMMA1
         CALL MATROT ( ANG1 , PGL )
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
         ENDIF
C
         IF (OPTION.EQ.'RAPH_MECA'.OR.OPTION.EQ.'FULL_MECA') THEN
            IF ((ITEMP.NE.0).AND.(NU.NE.NUM)) THEN
               CALL U2MESS('A','ELEMENTS3_59')
            ENDIF
            CALL NMPOEL(NPG,        KLV,        XL,     NNO, NC,
     &                  PGL,        ZR(IDEPLP), EPSTHE, E,   EM,
     &                  ZR(ICONTM), FL,         ZR(ICONTP))
         ENDIF
C
      ELSEIF ((ZK16(ICOMPO).EQ.'LMARC_IRRA').OR.
     &        (ZK16(ICOMPO).EQ.'LEMAITRE_IRRA')) THEN
C
         CALL TECACH('OON','PVARIMR',7,JTAB,IRET)
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
C-- RECUPERATION DES CARACTERISTIQUES DE FLUAGE ET DE GRANDISSEMENT
         IF (ZK16(ICOMPO).EQ.'LMARC_IRRA') THEN
            CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ','LMARC_IRRA',
     &                  0,' ',0.D0,3,NOMGRD,COEFGR,K2B, 'FM' )
         ELSE
            CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ','LEMAITRE_IRRA',
     &                  0,' ',0.D0, 3,NOMGRD,COEFGR,K2B, 'FM' )
         ENDIF
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
         CALL NMPOEL(NPG,        KLV,        XL,     NNO, NC,
     &               PGL,        ZR(IDEPLP), EPSTHE, E,   EM,
     &               ZR(ICONTM), FL,         ZR(ICONTP))
C
C-- CALCUL DE LA MATRICE TANGENTE ET CORRECTION DES TERMES D'ALLONGEMENT
C-- DES VECTEURS FORCES NODALES ET EFFORTS
         IF (OPTION.EQ.'RAPH_MECA'.OR.OPTION.EQ.'FULL_MECA') THEN
            IF(XL.EQ.0.D0) THEN
               CALL U2MESS('F','ELEMENTS3_60')
            ENDIF
            EFFNOM = ZR(ICONTM)
C
            IF (ZK16(ICOMPO).EQ.'LMARC_IRRA') THEN
               CALL RCVALB('RIGI',1,1,'+',ZI(IMATE),' ','LMARC_IRRA',
     &              0,' ',0.D0,
     &              12,NOMLMA(1),COELMA(1),CODLMA(1), 'FM' )
               CALL RCVARC(' ','IRRA','-','RIGI',1,1,IRRAM,IRET2)
               IF (IRET2.GT.0) IRRAM=0.D0
               CALL RCVARC(' ','IRRA','+','RIGI',1,1,IRRAP,IRET2)
               IF (IRET2.GT.0) IRRAP=0.D0
               CALL NMLMAB(PGL,NNO,NPG,NC,ZR(IDEPLP),
     &               EFFNOM, TEMPM, TEMPP, ZI(IMATE), ZR(ICARCR),
     &               ZR(IINSTM), ZR(IINSTP), XL, E, A,
     &               COELMA, COEFGR, IRRAM, IRRAP, ZR(IVARIM),
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
     &               7,NOMLEM(1),COELEM(1),CODLEM(1), 'FM' )
               CALL RCVARC(' ','IRRA','-','RIGI',1,1,IRRAM,IRET2)
               IF (IRET2.GT.0) IRRAM=0.D0
               CALL RCVARC(' ','IRRA','+','RIGI',1,1,IRRAP,IRET2)
               IF (IRET2.GT.0) IRRAP=0.D0
               CALL NMFGAS('RIGI',NPG,ZI(IMATE),PGL,NNO,
     &                  NC, ZR(IDEPLP), EFFNOM, ZR(IVARIM), ZR(ICARCR),
     &                  ZR(IINSTM), ZR(IINSTP), XL, A, COELEM,
     &                  COEFGR, IRRAM, IRRAP, KLS, FLC,
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
      ELSEIF((ZK16(ICOMPO).EQ.'VMIS_POU_LINE').OR.
     &       (ZK16(ICOMPO).EQ.'VMIS_POU_FLEJOU')) THEN
C
C        PASSAGE DES DEPLACEMENTS DANS LE REPERE LOCAL:
C
         CALL UTPVGL ( NNO, NC, PGL, ZR(IDEPLM),  U )
         CALL UTPVGL ( NNO, NC, PGL, ZR(IDEPLP), DU )
C
         CALL NMVMPO('RIGI', NPG, OPTION, NOMTE, NC,
     &            XL, ZI(IMATE), ZR(ISECT), ZR(ICARCR), ZK16(ICOMPO),
     &            U, DU, ZR(ICONTM), ZR(IVARIM), HOEL,
     &            HOTA, D1B, WORK, RG0, ZR(IVARIP),
     &            ZR(ICONTP), FL, KLV)
      ELSE
         CALL U2MESK('F','ELEMENTS3_61',1,ZK16(ICOMPO))
      ENDIF
C
C        --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
C
      IF ( OPTION.EQ.'RIGI_MECA_TANG'.OR.OPTION.EQ.'FULL_MECA' ) THEN
         CALL UTPSLG ( NNO, NC, PGL, KLV, ZR(IMATUU) )
      ENDIF
      IF (OPTION.EQ.'RAPH_MECA'.OR.OPTION.EQ.'FULL_MECA') THEN
         CALL UTPVLG ( NNO, NC, PGL, FL, ZR(IVECTU) )
         CALL JEVECH ( 'PCODRET', 'E', JCRET )
         ZI(JCRET) = 0
      ENDIF
C
      END
