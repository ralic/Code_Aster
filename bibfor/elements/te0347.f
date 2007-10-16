      SUBROUTINE TE0347(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/10/2007   AUTEUR SALMONA L.SALMONA 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C     FONCTION REALISEE:  CALCUL DES OPTIONS :
C     SIEF_ELNO_ELGA, FORC_NODA ET VARI_ELNO_ELGA
C     ELEMENT  POU_D_TG POU_D_T POU_D_E
C     POUR LES VARIABLES INTERNES :
C       - COMPORTEMENT VMIS_POU*
C       ==> ON NE FAIT QUE RECOPIER LE PG1 DANS LE NOEUD 1
C                                ET LE PG3 DANS LE NOEUD 2
C       - COMPORTEMENT ELAS : RECOPIE DES POINTS 1 ET 2
C
C     POUR LES CONTRAINTES ET LES FORC_NODA
C        - COMPORTEMENT VMIS_POU*
C       ==> ON CALCULE BT*SIGMA AU 2 NOEUDS
C       - COMPORTEMENT ELAS : RECOPIE DES POINTS 1 ET 2
C        QUI CONTIENNENT DEJA LES EFFORTS AUX NOEUDS
C
C IN  OPTION : OPTION DE CALCUL
C IN  NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER        ZI,JTAB(7)
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
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
C
      REAL*8      D1B(7,14),ZERO,DEUX,E,NU,G,ALFA,BETA,GAMMA
      REAL*8      XD(3),RESIDU(14),XL,XL2,XLS2,A,XIY,XIZ
      REAL*8      PGL(3,3),VALRES(2),ANG1(3),FS(14),D1B6(6,12)
      REAL*8      ALFAY,ALFAZ,PHIY,PHIZ,EY,EZ
      REAL*8      XUG(6),UTG(14),CO(3),TPP,TPM
      INTEGER     KP,NPG,IRET
      CHARACTER*8 NOMRES(2), NOMPAR
      CHARACTER*4 FAMI
      CHARACTER*2 CODRES(2)
      LOGICAL     REAGEO
      CHARACTER*16 COMPOR
      REAL*8       D1B3(2,3),KSI1
C
      DATA NOMRES/'E','NU'/
C ----------------------------------------------------------------------
C
      DEUX = 2.D0
      CO(1) = 5.D0/9.D0
      CO(2) = 8.D0/9.D0
      CO(3) = 5.D0/9.D0
      NNO = 2
      FAMI = 'RIGI'
      NPG = 3
      IF (NOMTE .EQ. 'MECA_POU_D_TG') THEN
         NC  = 7
      ELSE
         NC = 6
      ENDIF
      NEQ = 2*NC
C
      IF ( OPTION .EQ. 'VARI_ELNO_ELGA  ' ) THEN
C     --------------------------------------
         CALL JEVECH ( 'PVARIGR', 'L', ICHG   )
         CALL JEVECH ( 'PCOMPOR', 'L', ICOMPO )
         CALL JEVECH ( 'PVARINR', 'E', ICHN   )

         CALL TECACH('OON','PVARIGR',7,JTAB,IRET)
         LGPG = MAX(JTAB(6),1)*JTAB(7)
         READ (ZK16(ICOMPO+1),'(I16)') NBVAR
C
C     POUR LES VARIABLES INTERNES, ON PROJETTE AVEC LES FCTS DE FORME
C     SUR LES NOEUDS DEBUT ET FIN DE L'ELEMENT

C        POUR LE POINT 1
         KSI1 = -SQRT( 5.D0 / 3.D0 )
         D1B3(1,1) = KSI1*(KSI1-1.D0)/2.0D0
         D1B3(1,2) = 1.D0-KSI1*KSI1
         D1B3(1,3) = KSI1*(KSI1+1.D0)/2.0D0
C        POUR LE POINT 2
         KSI1 = SQRT( 5.D0 / 3.D0 )
         D1B3(2,1) = KSI1*(KSI1-1.D0)/2.0D0
         D1B3(2,2) = 1.D0-KSI1*KSI1
         D1B3(2,3) = KSI1*(KSI1+1.D0)/2.0D0

         DO 11 I = 1,NBVAR
           DO 12 K = 1 , 3
             ZR(ICHN     +I-1) = ZR(ICHN     +I-1) +
     &                           ZR(ICHG + LGPG*(K-1) + I-1)*D1B3(1,K)
             ZR(ICHN+LGPG+I-1) = ZR(ICHN+LGPG+I-1) +
     &                           ZR(ICHG + LGPG*(K-1) + I-1)*D1B3(2,K)
12         CONTINUE
11       CONTINUE

C
C
      ELSEIF ( OPTION .EQ. 'SIEF_ELNO_ELGA' .OR.
     +         OPTION .EQ. 'FORC_NODA'      ) THEN
C     --------------------------------------
C     POUR LES CONTRAINTES ET LES FORC_NODA
C        - COMPORTEMENT VMIS_POU*
C       ==> ON CALCULE BT*SIGMA AU 2 NOEUDS
C       - COMPORTEMENT ELAS : RECOPIE DES POINTS 1 ET 2
C        QUI CONTIENNENT DEJA LES EFFORTS AUX NOEUDS
C
         CALL TECACH('ONN','PCOMPOR',1,ICOMPO,IRET)
         COMPOR = ' '
         IF (IRET.EQ.0) THEN
            COMPOR = ZK16(ICOMPO)
         ENDIF
         IF (COMPOR(1:4).EQ.'VMIS' ) THEN
C     POUR LES CONTRAINTES ET LES FORC_NODA
C        - COMPORTEMENT VMIS_POU*
C       ==> ON CALCULE BT*SIGMA AU 2 NOEUDS
C
              CALL JEVECH ( 'PGEOMER', 'L', IGEOM  )
              CALL JEVECH ( 'PMATERC', 'L', IMATE  )
              CALL JEVECH ( 'PCAGNPO', 'L', LSECT  )
              CALL JEVECH ( 'PCAORIE', 'L', LORIEN )
C
              CALL MOYTEM(FAMI,NPG,1,'+',VALPAR,IRET)
              NBPAR  = 1
              NOMPAR = 'TEMP'
              CALL RCVALA(ZI(IMATE),' ','ELAS',NBPAR,NOMPAR,VALPAR,2,
     +                              NOMRES, VALRES, CODRES, ' ' )

              E  = VALRES(1)
              NU = VALRES(2)
              G = E / ( DEUX * ( 1.D0 + NU ) )
C
              LSECT = LSECT - 1
              A     =  ZR(LSECT+1)
              XIY   =  ZR(LSECT+2)
              XIZ   =  ZR(LSECT+3)
              ALFAY =  ZR(LSECT+4)
              ALFAZ =  ZR(LSECT+5)
              EY    = -ZR(LSECT+6)
              EZ    = -ZR(LSECT+7)
C
              IF ( ICOMPO .EQ. 0 ) THEN
                 REAGEO = .FALSE.
              ELSE
                 REAGEO = ZK16(ICOMPO+2)(6:10) .EQ. '_REAC'
              ENDIF
C
              IF (OPTION.EQ.'SIEF_ELNO_ELGA') THEN
                 CALL JEVECH ( 'PCONTRR' , 'L', ICGP   )
                 CALL JEVECH ( 'PDEPPLU' , 'L', IDEPLP )
                 CALL JEVECH ( 'PSIEFNOR', 'E', ICONTN )
                 DO 20 I = 1,NEQ
                    UTG(I) = ZR(IDEPLP-1+I)
 20              CONTINUE
C
              ELSE IF (OPTION.EQ.'FORC_NODA') THEN
                 CALL JEVECH ( 'PCONTMR', 'L', ICGP   )
                 CALL JEVECH ( 'PVECTUR', 'E', ICONTN )
                 CALL TECACH('ONN','PDEPLMR',1,IDEPLM,IRET)
C
                 IF ( IRET .NE. 0 ) THEN
                    DO 30 I = 1,NEQ
                       UTG(I) = 0.D0
 30                 CONTINUE
                 ELSE
                    DO 32 I = 1,NEQ
                       UTG(I) = ZR(IDEPLM-1+I)
 32                 CONTINUE
                 ENDIF
C
                 CALL TECACH('ONN','PDEPLPR',1,IDEPLP,IRET)
                 IF ( IRET .EQ. 0 ) THEN
                    DO 34 I = 1,NEQ
                       UTG(I) = UTG(I) + ZR(IDEPLP-1+I)
 34                 CONTINUE
                 ENDIF
              ENDIF
C
C             --- GEOMETRIE EVENTUELLEMENT  REACTUALISEE :
C             -----------------------------------------------
C
              IGEOM = IGEOM - 1
              IF ( REAGEO ) THEN
                 DO 40 I = 1,3
                    XUG(I)   = UTG(I)   + ZR(IGEOM+I)
                    XUG(I+3) = UTG(I+NC) + ZR(IGEOM+I+3)
 40              CONTINUE
                 CALL VDIFF(3,XUG(4),XUG(1),XD)
                 XL2=DDOT(3,XD,1,XD,1)
                 XL = SQRT(XL2)
                 XLS2 = XL/2.D0
                 TET1=DDOT(3,UTG(4),1,XD,1)
                 TET2=DDOT(3,UTG(4+NC),1,XD,1)
                 TET1 = TET1/XL
                 TET2 = TET2/XL
                 CALL ANGVX(XD,ANG1(1),ANG1(2))
                 GAMMA = ZR(LORIEN+2)
                 ANG1(3) = GAMMA + (TET1+TET2)/2.D0
                 CALL MATROT ( ANG1 , PGL )
              ELSE
                 CALL VDIFF(3,ZR(IGEOM+4),ZR(IGEOM+1),XD)
                 XL2=DDOT(3,XD,1,XD,1)
                 XL = SQRT(XL2)
                 XLS2 = XL/2.D0
                 CALL MATROT ( ZR(LORIEN) , PGL )
              ENDIF
C
              PHIY = E*XIZ*12.D0*ALFAY/ (XL2*G*A)
              PHIZ = E*XIY*12.D0*ALFAZ/ (XL2*G*A)
C
C             --- BOUCLE SUR LES POINTS DE GAUSS :
C             --------------------------------------
C
              CALL R8INIR(NEQ,0.D0,RESIDU,1)
C ELEMENT POU_D_TG
              IF (NC.EQ.7) THEN
                 DO 50 KP = 1,3
                    CALL JSD1FF(KP,XL,PHIY,PHIZ,D1B)
                    DO 52 K = 1,14
                       DO 54 KK = 1,7
                       RESIDU(K)=RESIDU(K)+XLS2*ZR(ICGP-1+7*(KP-1)+KK)*
     +                                   D1B(KK,K)*CO(KP)
 54                    CONTINUE
 52                 CONTINUE
 50              CONTINUE
C
                 DO 60 I = 1,2
                    RESIDU(7*(I-1)+4) = RESIDU(7*(I-1)+4) -
     +                          EZ*RESIDU(7*(I-1)+2) +
     +                          EY*RESIDU(7*(I-1)+3)
 60              CONTINUE
              ELSE
                 DO 55 KP = 1,3
                    CALL JPD1FF(KP,XL,PHIY,PHIZ,D1B6)
                    DO 56 K = 1,12
                       DO 57 KK = 1,6
                       RESIDU(K)=RESIDU(K)+XLS2*ZR(ICGP-1+6*(KP-1)+KK)*
     +                                   D1B6(KK,K)*CO(KP)
 57                    CONTINUE
 56                 CONTINUE
 55              CONTINUE
C
             ENDIF
C
C             POUR LES FORC_NODA :
C
C             --- ON CALCULE LE RESIDU DANS LE REPERE GLOBAL :
C            ---------------------------------------------------
C
C             POUR LES SIEF_ELNO_ELGA
C
C             ---  ON CHANGE LE SIGNE DU NOEUD 1 :
C             ------------------------------------
C
              IF ( OPTION .EQ. 'FORC_NODA' ) THEN
                CALL UTPVLG ( NNO,NC,PGL,RESIDU(1),ZR(ICONTN))
              ELSEIF ( OPTION .EQ. 'SIEF_ELNO_ELGA' ) THEN
                DO 70 I = 1,NC
                   ZR(ICONTN-1+I) = -RESIDU(I)
 70           CONTINUE
                DO 80 I = NC+1,2*NC
                   ZR(ICONTN-1+I) =  RESIDU(I)
 80             CONTINUE
              ENDIF
         ELSE
C       - COMPORTEMENT ELAS : RECOPIE DES POINTS 1 ET 2
C        QUI CONTIENNENT DEJA LES EFFORTS AUX NOEUDS
              IF ( OPTION .EQ. 'SIEF_ELNO_ELGA' ) THEN
                 CALL JEVECH ( 'PCONTRR' , 'L', ICGP   )
                 CALL JEVECH ( 'PSIEFNOR', 'E', ICONTN )
                 DO 100 I = 1 , NEQ
                    ZR(ICONTN-1+I) = ZR(ICGP-1+I)
 100             CONTINUE
              ELSEIF ( OPTION .EQ. 'FORC_NODA' ) THEN
                 CALL JEVECH('PCONTMR','L',ICONTG)
                 CALL JEVECH ( 'PCAORIE', 'L', LORIEN )
                 CALL JEVECH('PVECTUR','E',IVECTU)
                 DO 222 IN = 1,NC
                    FS(IN)      = -ZR(ICONTG+IN-1)
                    FS(IN+NC) =  ZR(ICONTG+IN+NC-1)
 222             CONTINUE
                 CALL MATROT ( ZR(LORIEN) , PGL )
                 CALL UTPVLG ( NNO, NC, PGL, FS, ZR(IVECTU) )
              ENDIF
C
         ENDIF
C
      ENDIF
      END
