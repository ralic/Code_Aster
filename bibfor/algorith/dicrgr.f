      SUBROUTINE DICRGR(FAMI,OPTION,NEQ,NC,ICODMA,ULM,
     &                  DUL,SIM,VARIM,PGL,KLV,VARIP,FONO,SIP,
     &                  IRRAP)
C ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NEQ,ICODMA,NC
      REAL*8 ULM(NEQ),DUL(NEQ),SIM(NEQ),SIP(NEQ),VARIM(5)
      REAL*8 PGL(3,3),VARIP(5),FONO(NEQ)
      REAL*8 IRRAP,KLV(78)
      CHARACTER*(*) FAMI
      CHARACTER*16 OPTION
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/10/2007   AUTEUR SALMONA L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C RESPONSABLE GODARD V.GODARD


C  COMPORTEMENT DIS_GRICRA : APPLICATION : LIAISON GRILLE-CRAYON COMBU
C           RELATION DE COMPORTEMENT : ELASTIQUE PARTOUT
C           SAUF SUIVANT Y LOCAL : FROTTEMENT DE COULOMB
C       ELEMENTS MECA_DIS_TR_L ET MECA_DIS_T_L

C IN  : OPTION : RIGI_MECA*, FULL_MECA* OU RAPH_MECA
C       NEQ    : NOMBRE DE DDL DE L'ELEMENT
C       NC     : NOMBRE DE DDL PAR NOEUD = 6
C       ICODMA : ADRESSE DU MATERIAU CODE
C       DUL    : INCREMENT DE DEPLACEMENT REPERE LOCAL
C       SIM    : EFFORTS GENERALISES A L'INSTANT PRECEDENT
C       VARIM  : VARIABLE INTERNE A L'INSTANT PRECEDENT
C       PGL    : MATRICE DE PASSAGE REPERE GLOBAL -> LOCAL
C       ITEMP  : INDICATEUR DU CHAMP DE TEMPERATURE
C       TEMPM  : TEMPERATURE A L'INSTANT PRECEDENT
C       TEMPP  : TEMPERATURE A L'INSTANT ACTUEL
C       IRRAP  : IRRADIATION
C
C OUT : KLV    : MATRICE TANGENTE
C       VARIP  : VARIABLE INTERNE REACTUALISEE
C       FONI   : FORCES NODALES
C       SIP    : EFFORTS INTERNES
C------------------------------------------------------------------
C------------------------------------------------------------------

C **************** DEBUT COMMUNS NORMALISES JEVEUX *********************

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


      CHARACTER*2 CODRE1(4),CODRE2(2),CODRE3,CODRE4(5),CODRE5(5)
      CHARACTER*8 NOMRE1(4),NOMRE2(2),NOMRE3,NOMRE4(5),NOMRE5(5)
      CHARACTER*8 NOMPAR(2)

      INTEGER     NPG,NNO,I,NBPAR,J,K,L,P,Q
      INTEGER     ICOD,ICOD2,KP,IRETP,IRETM
      REAL*8      VALRE1(4),VALRE2(2),VALRE3,VALRE4(5),VALRE5(5)
      REAL*8      VALPAR(2)
      REAL*8      FNO,H1
      REAL*8      DUX,DUY,DPH,DTH
      REAL*8      UXM,PHM,THM
      REAL*8      KXX,KYY,KZZ,KPP,KTT
      REAL*8      FL(12)
      REAL*8      TEMP,TEMM,FTEMP
      REAL*8      KTAX,MUAX,ETAX,KNAX
      REAL*8      KTRIG,DP,PM,RTM,RTP,RTE
      REAL*8      SIELEQ,BETA,SEUIL,RM
      REAL*8      PHIPL,PPM,FPHI,MOPHI,DKH,KHM
      REAL*8      PHITAN,THETAN,KKK,ECRO
      REAL*8      PHIC,KPHI,THETAC,KTHETA,EPHI
      REAL*8      DPP,DPHIPL,SEUROT,KTHET2
      REAL*8      TEMPP,TEMPM

      DATA NOMRE1/'KN_AX','KT_AX','ET_AX','ET_ROT'/
      DATA NOMRE2/'F_SER','COUL_AX'/
      DATA NOMRE3/'F_SER_FO'/
      DATA NOMRE4/'ANG1','ANG2','PEN1','PEN2','PEN3'/
      DATA NOMRE5/'ANG1_FO','ANG2_FO','PEN1_FO','PEN2_FO','PEN3_FO'/



C TYPE D'ELEMENT: SEG2
      NPG = 2
C
C---procedure pour la liaison grille crayon
C
C on travaille ici dans le repère local
C
C
C  recuperer les deplacements et rotation
C  recuperer les variables internes  (moins)
C  recuperer les parametres de defi_materiau

      CALL R8INIR(NEQ,0.D0,FL,1)
C
C recuperation des donnees materiau pour le discret
      CALL RCVALB(FAMI,1,1,'+',ICODMA,' ','DIS_GRICRA',
     &            0,' ',0.D0,4,
     &            NOMRE1,VALRE1,CODRE1,' ')

      KNAX=VALRE1(1)
      KTAX=VALRE1(2)/4.D0
      ETAX=VALRE1(3)*KTAX

      CALL RCVALB(FAMI,1,1,'+',ICODMA,' ','DIS_GRICRA',0,' ',0.D0,2,
     &            NOMRE2,VALRE2,CODRE2,' ')
      IF (CODRE2(1).EQ.'OK') THEN
        FNO=VALRE2(1)/4.D0
        MUAX=VALRE2(2)
      ELSE
        CALL MOYTEM(FAMI,NPG,1,'+',TEMPP,IRETP)
        CALL MOYTEM(FAMI,NPG,1,'-',TEMPM,IRETM)
        IF ((IRETP+IRETM).GE.1) CALL U2MESS('F','CALCULEL_31')
        TEMP = (TEMPP+TEMPM)/2.D0

        NBPAR=2
        NOMPAR(2)='IRRA'
        NOMPAR(1)='TEMP'
        VALPAR(2)=IRRAP
        VALPAR(1)=TEMP
        CALL RCVALB(FAMI,1,1,'+',ICODMA,' ','DIS_GRICRA',
     &              NBPAR,NOMPAR,VALPAR,1,
     &              NOMRE3,VALRE3,CODRE3,' ')
        IF (CODRE3.EQ.'OK') THEN
            FNO=VALRE3/4.D0
            MUAX=VALRE2(2)
        ENDIF
      ENDIF

      CALL RCVALB(FAMI,1,1,'+',ICODMA,' ','DIS_GRICRA',
     &            0,' ',0.D0,5,
     &            NOMRE4,VALRE4,CODRE4,' ')

      IF (CODRE4(1).EQ.'OK') THEN
        PHIC=VALRE4(1)
        THETAC=VALRE4(2)
        KTHETA=VALRE4(4)/2.D0
        KPHI=VALRE4(3)/2.D0-KTHETA
        KTHET2=VALRE4(5)/2.D0
      ELSE
        CALL MOYTEM(FAMI,NPG,1,'+',TEMPP,IRETP)
        CALL MOYTEM(FAMI,NPG,1,'-',TEMPM,IRETM)
        IF ((IRETP+IRETM).GE.1) CALL U2MESS('F','CALCULEL_31')
        TEMP = (TEMPP + TEMPM)/2.D0
        NBPAR=2
        NOMPAR(2)='IRRA'
        NOMPAR(1)='TEMP'
        VALPAR(2)=IRRAP
        VALPAR(1)=TEMP
        CALL RCVALB(FAMI,1,1,'+',ICODMA,' ','DIS_GRICRA',
     &              NBPAR,NOMPAR,VALPAR,5,
     &              NOMRE5,VALRE5,CODRE5,' ')
        PHIC=VALRE5(1)
        THETAC=VALRE5(2)
        KTHETA=VALRE5(4)/2.D0
        KPHI=VALRE5(3)/2.D0-KTHETA
        KTHET2=VALRE5(5)/2.D0
      ENDIF
      EPHI=VALRE1(4)*KPHI

C
C Variables internes de contact au temps moins
       H1=1.D0-VARIM(3)

C
C
C---calcul de l'evolution des variables internes et des forces
C---pour FULL_MECA et RAPH_MECA

      IF ((OPTION(1:9).EQ.'FULL_MECA').OR.(OPTION(1:9).EQ.'RAPH_MECA'))
     &        THEN
C
C  extension de l'element
C
C  extension au pas de temps precedent
        UXM = ULM(1+NC) - ULM(1)
        PHM = ULM(4+NC) - ULM(4)
        KHM = ULM(5+NC) - ULM(5)
        THM = ULM(6+NC) - ULM(6)
C  variation d'extension
        DUX = DUL(1+NC) - DUL(1)
        DUY = DUL(2+NC) - DUL(2)
        DPH = DUL(4+NC) - DUL(4)
        DKH = DUL(5+NC) - DUL(5)
        DTH = DUL(6+NC) - DUL(6)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  CALCUL DES FORCES EN TRANSLATION
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  calcul de la force (translation)
C
C on garde -FN0 dans la direction du discret
C possibilité de frottement dans la direction 2 (verticalement)
C force nulle dans la 3e direction

        RTM = (SIM(2+NC)+SIM(2))/2.D0
        SEUIL = MUAX*FNO
        PM = VARIM(1)
        RTE = RTM + KTAX*DUY
        SIELEQ = ABS(RTE)
        BETA = KTAX*ETAX/ (KTAX-ETAX)
        RM = BETA*PM + SEUIL
        IF (SIELEQ.LE.RM) THEN
          DP = 0.D0
          RTP = RTE
          KTRIG = KTAX
          VARIP(1) = PM
          VARIP(2) = 0.D0
        ELSE
          VARIP(2) = 1.D0
          DP = ABS(RTE) - (SEUIL+BETA*PM)
          DP = DP/ (BETA+KTAX)
          RTP = (SEUIL + BETA* (PM+DP))*RTE/ABS(RTE)
          VARIP(1) = PM + DP
          KTRIG = ETAX
        END IF
        SIP(1)=-FNO+KNAX*(UXM+DUX)
        SIP(2)=RTP
        SIP(3)=0.D0
        SIP(1+NC)=-FNO+KNAX*(UXM+DUX)
        SIP(2+NC)=RTP
        SIP(3+NC)=0.D0

        FL(2)=-SIP(2)
        FL(1)=-SIP(1)
        FL(3)=-SIP(3)
        FL(2+NC)=SIP(2)
        FL(1+NC)=SIP(1)
        FL(3+NC)=SIP(3)


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  CALCUL DES FORCES EN ROTATION
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  Calcul des forces et evolution des variables internes
C   sur chacun des sous éléments du système de liaison
C
C
C  angle theta (DRZ): possibilite de decollement
C  cela equivaut a une loi bilineaire

        IF ((THETAC-THM-DTH).LT.0.D0) THEN
          H1=0.D0
          SIP(6)=KTHETA*THETAC+KTHET2*(THM+DTH-THETAC)
        ELSEIF ((THETAC+THM+DTH).LT.0.D0) THEN
          H1=0.D0
          SIP(6)=-KTHETA*THETAC+KTHET2*(THM+DTH+THETAC)
        ELSE
          H1=1.D0
          SIP(6)=KTHETA*(THM+DTH)
        ENDIF

        SIP(6+NC)=SIP(6)
        FL(6)=-SIP(6)
        FL(6+NC)=SIP(6)
        THETAN=H1*KTHETA+(1.D0-H1)*KTHET2
        VARIP(3)=1.D0-H1


C  angle phi (DRX): loi de frottement

        PHIPL=VARIM(4)
        PPM=VARIM(5)
        FPHI=PHM+DPH-PHIPL
        ECRO=EPHI/KPHI

        SEUROT=ABS(FPHI)-PHIC-ECRO*PPM
        IF (SEUROT.LT.0.D0) THEN
          MOPHI=KPHI*FPHI
          VARIP(4)=VARIM(4)
          VARIP(5)=VARIM(5)
          PHITAN=KPHI
        ELSE
          DPP=-(PHIC+ECRO*PPM-ABS(PHM-PHIPL+DPH))/(1.D0+ECRO)
          DPHIPL=DPP*DPH/ABS(DPH)
          VARIP(4)=VARIM(4)+DPHIPL
          VARIP(5)=VARIM(5)+DPP
          MOPHI=KPHI*(PHM+DPH-PHIPL-DPHIPL)
          PHITAN=EPHI
        ENDIF

        SIP(4)=MOPHI
        SIP(4+NC)=MOPHI
        FL(4)=-MOPHI
        FL(4+NC)=MOPHI

C pour le dernier angle, on met quand meme une rigidite, meme si
Cles conditions limites doivent imposer que ça ne tourne pas
        SIP(5)=KPHI*(KHM+DKH)
        SIP(5+NC)=KPHI*(KHM+DKH)
        FL(5)=-KPHI*(KHM+DKH)
        FL(5+NC)=KPHI*(KHM+DKH)


      ENDIF
C
C
C
C---Matrice tangente pour FULL_MECA(_ELAS) et RIGI_MECA(_ELAS)

      IF ((OPTION(1:9).EQ.'FULL_MECA')
     &           .OR. (OPTION(1:9).EQ.'RIGI_MECA')) THEN
C
        IF ((OPTION(1:9).EQ.'RIGI_MECA').OR.(OPTION(10:14).EQ.'_ELAS'))
     &               THEN
C
          KXX=KNAX
          KYY=KTAX
          KZZ=0.D0
          KPP=KPHI
          KKK=KPHI
          KTT=KTHETA
        ELSE
          KXX=KNAX
          KYY=KTRIG
          KZZ=0.D0
          KPP=PHITAN
          KKK=KPHI
          KTT=THETAN
        ENDIF
C
C
        CALL R8INIR(78,0.D0,KLV,1)
        KLV(1)=KXX
        KLV(3)=KYY
        KLV(6)=KZZ
        KLV(10)=KPP
        KLV(15)=KKK
        KLV(21)=KTT
        KLV(28)=KXX
        KLV(36)=KYY
        KLV(45)=KZZ
        KLV(55)=KPP
        KLV(66)=KKK
        KLV(78)=KTT
        KLV(22)=-KXX
        KLV(30)=-KYY
        KLV(39)=-KZZ
        KLV(49)=-KPP
        KLV(60)=-KKK
        KLV(72)=-KTT

      ENDIF
C
C---Calcul des forces nodales dans le repère global
C
      IF (OPTION(1:9).EQ.'FULL_MECA' .OR. OPTION(1:9).EQ.'RAPH_MECA')
     &           THEN
        NNO = 2
        CALL UTPVLG(NNO,NC,PGL,FL,FONO)
      ENDIF

      END
