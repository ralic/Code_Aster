       SUBROUTINE CACOGT (OPTION,MECA,THMC,HYDR,IMATE,NDIM,DIMDEF,
     +               DIMCON,NVIMEC,NVITH,YAMEC,YATE,ADDEME,ADCOME,
     +               ADVITH,ADDEP1,ADCP11,ADCP12,ADDEP2,ADCP21,ADDETE,
     +               ADCOTE,CONGEM,CONGEP,VINTM,VINTP,DSDE,EPSV,DEPSV,
     +               P1,P2,DP1,DP2,T,DT,PHI,PVP,H11,H12,H21,RHO11,PHI0,
     +               PVP0,P10,P20,T0,SAT,RV0,G1D,G1F,G1C,J1D,J1F,J1C,
     +               J2,J3,G2,G3)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
C RESPONSABLE UFBHHLL C.CHAVANT
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
C TOLE CRP_21
C **********************************************************************
C CLONE CE CALCCO POUR LES LOIS DU CERMES QUI N FIGURENT PLUS
C          DANS CALCCO
C **********************************************************************
C
C  VARIABLES IN / OUT
C
      IMPLICIT NONE
      CHARACTER*16   OPTION,MECA,THMC,HYDR
      INTEGER        NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,IMATE
      INTEGER        YAMEC,YATE
      INTEGER        ADCOME,ADCP11,ADCP12,ADCP21,ADCOTE
      INTEGER        ADDEME,ADDEP1,ADDEP2,ADDETE,ADVITH
      REAL*8         CONGEM(DIMCON),CONGEP(DIMCON)
      REAL*8         VINTM(NVIMEC+NVITH),VINTP(NVIMEC+NVITH)
      REAL*8         DSDE(DIMCON,DIMDEF)
      REAL*8         EPSV,DEPSV,P1,DP1,P2,DP2,T,DT,P1M,P2M
      REAL*8         PHI,PVP,H11,H12,H21,RHO11
      REAL*8         PHI0,PVP0,P10,P20,T0
C
C  VARIABLES LOCALES
C
C
      REAL*8         SATM,EPSVM,PHIM,RHO11M,RHO12M,RHO21M,PVPM
      REAL*8         BIDON
C       
      INTEGER  I,K
      REAL*8   S0GAT,RHO110,RHO120,RHO210
      REAL*8   YOUNG,NU,BIOT,K0,CS,ALPHA0,ALPLIQ,CLIQ
      REAL*8   CP11,CP12,CP21,SAT,DSATP1,N,M,MAMOLV,MAMOLG
      REAL*8   R,RHOS,C0EPS,CSIGM,VARIA,ALP11,ALP12,ALP21
      REAL*8   RHO12,RHO21,BID
C  BG SATURE
      REAL*8    COMPS,XKL,XKB,BTK,ALFAD,RKB,TALS
      REAL*8    RDKBDT,RBBP,RDBBDP,RDBBDT,RDBBDE
      REAL*8    RBTP,RDBTDP,RDBTDT,RDBTDE
      REAL*8    RALP,RFXP,COMPW  
      REAL*8  ELOAD,EUNLD,XN,RF,TM1
      REAL*8  ANPHI,COHES,BETAT
      REAL*8  S1,S2
      REAL*8  SR,SMAX,SM1,TRAC,AA
      REAL*8  ET,BTMAX,BTMIN
      REAL*8  ALPH0,ALPH1,ALPH2,ALPH3,RDXMDT
      REAL*8  ALPHAT,ALPHBT,ALPHCT,XM
      REAL*8  PP
      REAL*8  RKAPAT,TKAPA,SIGMMO,RDKPDT,EVS,RKBINI
C  
C  B.G. NONSAT

      REAL*8   SRAS,SRBS,SRCS,SRDS,CC,DD,G1,G2,G3,SUC0
      REAL*8   TT1,UWT,RV00,AX1,AX,HUMR0,HENRY,RV0,HUMR
      REAL*8   G1D,G1F,G1C,J1D,J1F,J1C
      REAL*8   DRV0DT,DHDSR,DHDN,DHDT
      REAL*8   AVSR,BVN,EVT,AASR,BAN,EAT
      REAL*8   SUC,SY,ATMP,SUCN,SYN,DSUC,DSY
      REAL*8   EVAE,EVBE,EVCE,EVDE,EVCTE,EVSIGB,EVKB,EVXM
      REAL*8   SIGBN,ASIG,BSUC,DNEXP,DNOM1,DNOM2
      REAL*8   EV,BT,XM1,XMT,DC,DF,J1,J2,J3
      REAL*8   SMEAN,S3,AP1,TENS,TEN,BB
      REAL*8   RDBTP1,RDBTP2
C
C  DECLARATIONS PERMETTANT DE RECUPERER LES CONSTANTES MATERIAU  
C 
      INTEGER     NLIQ,NGAZ,NVAP,NHOM,NRESMA,NSAT,NINIG
      PARAMETER   (NLIQ=5,NGAZ=3,NVAP=3,NHOM=4,NSAT=2)
C   NRESMA EST LE MAX DE NELAS,NLIQ,NGAZ,NVAP,NHOM
      PARAMETER   (NRESMA = 18)
      REAL*8      LIQ(NLIQ),GAZ(NGAZ),VAP(NVAP),HOM(NHOM)
      REAL*8      SATUR(NSAT)
      CHARACTER*8 NCRA2(NLIQ),NCRA3(NGAZ),NCRA4(NVAP)
      CHARACTER*8 NCRA5(NHOM),NCRA6(NSAT)
      CHARACTER*2 CODRET(NRESMA)
CCCCC
      INTEGER      NNELA,NBPAR,NSURM,NSATB,NSUREV,NSATM,NNPLA,NCOF
      PARAMETER    (NNELA=7, NSURM=15,NSATB=4,NSUREV=7,NSATM=15)
      PARAMETER    (NNPLA=14)
      PARAMETER    (NINIG=2,NCOF=1)
      REAL*8       INIGAT(NINIG),COFGAT(NCOF)
      REAL*8       VALRES(NNELA),VALPAR,VALSUR(NSURM)
      REAL*8       VALSAT(NSATB),SUREV(NSUREV), VSATSU(NSATM)
      REAL*8       VALPES(NNPLA) 
      CHARACTER*8  BGCR1(NNELA),NOMPAR
      CHARACTER*8  BGCR7(NSATM),BGCR8(NNPLA)
      CHARACTER*8  BGCR3(NINIG),BGCR9(NCOF)
      CHARACTER*8  BGCR5(NSATB),BGCR4(NSUREV)
C BG
      DATA BGCR1 
     > /'RHO_S','UN_SUR_KS','E','KB','D_KB_T','ALPHA_S','ALPHA_D'/
      DATA BGCR3 /'DEGR_SATU','PRES_ATMO'/
      DATA BGCR4 /'EV_A','EV_B','EV_CT',
     &            'EV_SIGB','EV_KB','EV_XM','RESI_TRAC'/
      DATA BGCR5 
     &   /'A_SURF_SATU','B_SURF_SATU','C_SURF_SATU','D_SURF_SATU'/
      DATA BGCR7 /'E_CHAR','E_DECHAR','XN','RF','EV_KB'
     &            ,'EV_XM','D_E_T','ALPHA0','ALPHA1','ALPHA2','ALPHA3'
     &            ,'ALPHA_S','ANG_FRT','COHE','RESI_TRAC'/
       DATA BGCR8 /'NU','LAMBDA','KAPA','M','PRES_CRIT',
     &             'GAMA','A0_PC','A1_PC','A2_PC','ALPHA0_PC',
     &             'ALPHA1_PC','ALPHA2_PC','ALPHA3_PC','ALPHA_S'/
       DATA BGCR9 /'COEF_HENRY'/
CCCCCC

      DATA NCRA2 / 'RHO','UN_SUR_K','ALPHA','CP','VISC' /
      DATA NCRA3 / 'MASS_MOL','CP','VISC' /
      DATA NCRA4 / 'MASS_MOL','CP','VISC' /
      DATA NCRA5 / 'R_GAZ','RHO','CP','BIOT_COE' / 
      DATA NCRA6 / 'SATU_PRE','D_SATU_P' /
C
      DATA RKBINI /250.D5/
C **********************************************************************
C --- AJOUT BIDON RF POUR COMPIL
C --- CES VALEURS SONT NECESSAIRES POUR UNE LOI DE COUPLAGE AUTRE
C --- QUE LIQU_SATU_GAT OU LIQU_NSAT_GAT 
C **********************************************************************
      ALPHA0 = 0.0D0
      CS     = 0.0D0
      BIOT   = 0.0D0
      K0     = 0.0D0
C
C CALCUL EPSV AU TEMPS MOINS
      EPSVM=EPSV-DEPSV
C
C INITIALISATION VARIABLE PORELA
C 
C
C **********************************************************************
C  RECUPERATION DES DONNEES
C **********************************************************************
C 
C  CALCUL DE S0GAT : SATURATION A L'INSTANT INITIAL 
C
      IF (THMC.EQ.'LIQU_NSAT_GAT') THEN
C    CALCUL DE S0GAT
C
      SUC0=P20-P10
      IF(SUC0.LE.0.D0) SUC0=0.D0
C
      SY=0.D0
         DO 105 I=1,3
            SY=SY+CONGEM(ADCOME-1+I)
 105     CONTINUE
      SY=SY/3.D0
C
          IF (MECA.EQ.'SURF_ETAT_NSAT')THEN
C         
            NBPAR  = 0
            VALPAR = 0.D0
            NOMPAR = ' '
             CALL RCVALA ( IMATE,'SURF_ETAT_NSAT',NBPAR,NOMPAR,VALPAR,
     &              NSATB,BGCR5,VALSAT,CODRET, 'FM' )

            SRAS    = VALSAT(1)
            SRBS    = VALSAT(2)        
            SRCS    = VALSAT(3)
            SRDS    = VALSAT(4)
C
C       CALCUL DE SURFACE D'ETAT DE SATURATION INITIALE
C
      CC=EXP(-SRCS*SUC0)
      DD=EXP(SRDS*DT)
      S0GAT=1.D0-(SRAS+SRBS*SY)*(1.D0-CC)*DD
          ENDIF
      ENDIF
        
C   CALCUL DES COEFFICIENTS
C       
      CALL RCVALA(IMATE,'THM_DIFFU',1,'PCAP',P1,NHOM,
     &            NCRA5,HOM,CODRET,'FM')
      R = HOM(1)
      IF (YATE.EQ.1) CSIGM=HOM(3) 
CC 
C
C   COEFFICIENTS MECANIQUES
C
      IF (YAMEC.EQ.1) THEN
C
        IF (MECA.EQ.'ELAS_THM') THEN
C
C  RECUPERATION DES COEFFICIENTS DU MATERIAU PORO_THM_GAT 
C
            NBPAR  = 0
            VALPAR = 0.D0
            NOMPAR = ' '
             CALL RCVALA ( IMATE,'ELAS_THM',NBPAR,NOMPAR,VALPAR,
     &                      NNELA,BGCR1,VALRES,CODRET, 'FM' )
            RHOS   = VALRES(1)
            COMPS  = VALRES(2)        
            XKB    = VALRES(4)
            BTK    = VALRES(5)
            TALS   = VALRES(6)
            ALFAD  = VALRES(7)
C     
C ICI ON DEFINIT LES LOIS POUR THM_PORO_GAT
C
            RKB=XKB+BTK*T
            RDKBDT=BTK
            RBBP=1.D0-RKB*COMPS
            RDBBDT=-RDKBDT*COMPS
            RBTP=3.D0*RKB*ALFAD
            RDBTDT=RDKBDT*ALFAD
            BETAT=ALFAD
       ENDIF
C
        IF (MECA.EQ.'SURF_ETAT_SATU') THEN
C
C  RECUPERATION DES COEFFICIENTS DU MATERIAU PORO_THM_SAT 
C
            NBPAR  = 0
            VALPAR = 0.D0
            NOMPAR = ' '
             CALL RCVALA ( IMATE,'ELAS_THM',NBPAR,NOMPAR,VALPAR,
     &                      2,BGCR1,VALRES,CODRET, 'FM' )
            RHOS   = VALRES(1)
            COMPS  = VALRES(2)
            CALL RCVALA ( IMATE, 'SURF_ETAT_SATU' ,NBPAR,NOMPAR,VALPAR,
     &              NSATM,BGCR7,VSATSU,CODRET, 'FM' )        
            ELOAD   = VSATSU(1)
            EUNLD   = VSATSU(2)
            XN      = VSATSU(3)
            RF      = VSATSU(4)
            XKB     = VSATSU(5)
            XM      = VSATSU(6)
            TM1     = VSATSU(7)
            ALPH0   = VSATSU(8)
            ALPH1   = VSATSU(9)
            ALPH2   = VSATSU(10)
            ALPH3   = VSATSU(11)
            TALS    = VSATSU(12)
            ANPHI   = VSATSU(13)
            COHES   = VSATSU(14)
            TEN     = VSATSU(15)
C
            CALL RCVALA(IMATE,'THM_INIT',0,' ',0.D0,NINIG,BGCR3,
     &               INIGAT,CODRET,'FM')
            ATMP = INIGAT(2)
            CALL PRCIPE(CONGEP,DIMCON,S1,S2,S3,NDIM)
      SY=0.D0
         DO 20 I=1,3
            SY=SY+CONGEP(ADCOME-1+I)
 20     CONTINUE
      SY=SY/3.D0
      SYN=SY/ATMP
      SMEAN=S3
      ALPHAT=ALPH0+ALPH2*DT
      ALPHBT=ALPH1+ALPH3*DT
      ALPHCT=ALPH1+2.D0*ALPH3*DT
C
C     CHECK FOR TENSILE FAILURE
C
      TENS=S3+TEN
      AP1=0.1D0*ATMP
      IF(S3.GT.0.0D0) GO TO 40
      IF(TENS.GT.0.0D0) GO TO 30
      SMEAN=AP1
      BB=1.D0
      IF(XM.GT.0.000001D0) BB=(SMEAN/ATMP)**XM
      RKB=XKB*ATMP*BB*(1.D0+ALPHBT*DT)
      IF (XM.EQ.1.D0)THEN
      XMT= ALPHAT
      RDXMDT=ALPH2
      ELSE
      XMT= ALPHAT+ALPHCT*(SMEAN/ATMP)**(1.D0-XM)/XKB/(1.D0-XM)
      RDXMDT=ALPH2+2.D0*ALPH3*(SMEAN/ATMP)**(1.D0-XM)/XKB/(1.D0-XM)
      ENDIF
      RDKBDT=XKB*ATMP*BB*ALPHCT
      ET=0.01D0*RKB
      SR=1.D0
      GO TO 99
30    CONTINUE
      SMEAN=TENS
40    CONTINUE
C
C     CHECK FOR SHEAR FAILURE
C
      PP=ANPHI
      SR=(1.D0-SIN(PP))*(S1-SMEAN)/(2.D0*COHES*COS(PP)
     &     +2.D0*SMEAN*SIN(PP))
      IF(SR.LT.0.95D0) GO TO 50
      SR=0.95D0
C
C     CALCULATE LOAD/UNLOAD MODULUS
C
50    CONTINUE   
      TRAC=(S1-S3)/2.D0
C       DEFINITION DE SM1
      SM1=MAX(ABS(CONGEP(ADCOME+2)-CONGEP(ADCOME)),
     &        ABS(CONGEP(ADCOME+1)-CONGEP(ADCOME)),
     &        ABS(CONGEP(ADCOME+2)-CONGEP(ADCOME+1)))
      SM1=SM1/2.D0
      AA=1.D0
      BB=1.D0
      IF(SMEAN.LT.AP1) SMEAN=AP1
      IF(XN.GT.0.000001D0) AA=(SMEAN/ATMP)**XN
      IF(XM.GT.0.000001D0) BB=(SMEAN/ATMP)**XM
      IF(TRAC.GE.0.99D0*SM1) GO TO 60
      ET=EUNLD*AA*ATMP+T*TM1
      RKB=XKB*ATMP*BB*(1.D0+ALPHBT*DT)
      RDKBDT =XKB*ATMP*BB*ALPHCT
      IF (XM.EQ.1.D0)THEN
      XMT= ALPHAT
      RDXMDT=ALPH2
      ELSE
      XMT= ALPHAT+ALPHCT*(SMEAN/ATMP)**(1.D0-XM)/XKB/(1.D0-XM)
      RDXMDT=ALPH2+2.D0*ALPH3*(SMEAN/ATMP)**(1.D0-XM)/XKB/(1.D0-XM)
      ENDIF
      GO TO  90
60    CONTINUE   
      ET=(ELOAD*AA*ATMP+T*TM1)*(1.D0-RF*SR)**2.D0
      RKB=XKB*ATMP*BB*(1.D0+ALPHBT*DT)
      RDKBDT =XKB*ATMP*BB*ALPHCT
      IF (XM.EQ.1.D0)THEN
      XMT= ALPHAT
      RDXMDT=ALPH2
      ELSE
      XMT= ALPHAT+ALPHCT*(SMEAN/ATMP)**(1.D0-XM)/XKB/(1.D0-XM)
      RDXMDT=ALPH2+2.D0*ALPH3*(SMEAN/ATMP)**(1.D0-XM)/XKB/(1.D0-XM)
      ENDIF
90    CONTINUE   
      BTMAX=17.0D0*ET
      BTMIN=0.33D0*ET
      IF(RKB.LT.BTMIN) RKB=BTMIN
      IF(RKB.GT.BTMAX) RKB=BTMAX
 99   CONTINUE   
C
      RBBP=1.D0-RKB*COMPS
      RDBBDT=-RDKBDT*COMPS
      RBTP=3.D0*RKB*XMT
      RDBTDT=3.D0*(RDKBDT*XMT+RKB*RDXMDT)
      BETAT=XMT
      ENDIF
          IF (MECA.EQ.'CAM_CLAY_THM')THEN
C
      NBPAR  = 0
      VALPAR = 0.D0
      NOMPAR = ' '
      CALL RCVALA ( IMATE,'ELAS_THM',NBPAR,NOMPAR,VALPAR,
     &                      2,BGCR1,VALRES,CODRET, 'FM' )
            RHOS   = VALRES(1)
            COMPS  = VALRES(2)
      CALL RCVALA ( IMATE,'CAM_CLAY_THM',NBPAR,NOMPAR,VALPAR,
     &              NNPLA,BGCR8,VALPES,CODRET, 'FM' )
C       
       TKAPA  = VALPES(3)
       ALPH0   = VALPES(10)
       ALPH1   = VALPES(11)
       ALPH3   = VALPES(13)
       TALS   = VALPES(14)



C     --  CALCUL DE SIGMMO ET ESV0 :
C     -------------------------------
      EVS=PHI0/(1.D0-PHI0)
      RKAPAT=TKAPA+(ALPH1+ALPH3*DT)*DT*(1.D0+EVS)
      SIGMMO = 0.D0
      DO 104 K =1,3
        SIGMMO = SIGMMO + CONGEM(ADCOME+K-1)
 104  CONTINUE
      SIGMMO = SIGMMO /3.D0
       RDKPDT=(1.D0+EVS)*(ALPH1+2.D0*ALPH3*DT)
       IF(SIGMMO.EQ.0.0D0)THEN
       RKB=RKBINI 
       ELSE
       RKB=(1.D0+EVS)/RKAPAT*ABS(SIGMMO)
       ENDIF
       RDKBDT=RDKPDT*ABS(SIGMMO)*(1.D0+EVS)/(RKB*RKB)
C
      BETAT=ALPH0
      RBTP=3.D0*RKB*BETAT
      RDBTDT=3.D0*RDKBDT*BETAT
      RBBP=1.D0-RKB*COMPS
      RDBBDT=-RDKBDT*COMPS
          ENDIF       
          IF (MECA.EQ.'SURF_ETAT_NSAT')THEN
            CALL RCVALA(IMATE,'THM_AIR_DISS',0,' ',0.D0,NCOF,BGCR9,
     &               COFGAT,CODRET,'FM')
            HENRY=COFGAT(1)
            CALL RCVALA(IMATE,'THM_INIT',0,' ',0.D0,NINIG,BGCR3,
     &               INIGAT,CODRET,'FM')
            ATMP = INIGAT(2)
C         LIRE MASSE VOLUMISUE ET COMPRESSIBILITE
            NBPAR  = 0
            VALPAR = 0.D0
            NOMPAR = ' '
             CALL RCVALA ( IMATE,'ELAS_THM',NBPAR,NOMPAR,VALPAR,
     &                      2,BGCR1,VALRES,CODRET, 'FM' )
            RHOS   = VALRES(1)
            COMPS  = VALRES(2)
          ENDIF
      ENDIF
C
C   COEFFICIENTS HYDRAULIQUES
C
      IF (THMC.EQ.'LIQU_SATU') THEN
         CALL RCVALA(IMATE,'THM_LIQU',1,'TEMP',T,NLIQ,NCRA2,
     &               LIQ,CODRET,'FM')
         RHO110=LIQ(1)
         CLIQ=LIQ(2)
         ALPLIQ=LIQ(3)
         CP11=LIQ(4)
      ENDIF
      IF (THMC.EQ.'LIQU_SATU_GAT') THEN
         CALL RCVALA(IMATE,'THM_LIQU',1,'TEMP',T,NLIQ,NCRA2,
     &               LIQ,CODRET,'FM')
         RHO110=LIQ(1)
         CLIQ=LIQ(2)
         ALPLIQ=LIQ(3)
         CP11=LIQ(4)
            CALL RCVALA(IMATE,'THM_AIR_DISS',0,' ',0.D0,NCOF,BGCR9,
     &               COFGAT,CODRET,'FM')
            HENRY=COFGAT(1)
            CALL RCVALA(IMATE,'THM_INIT',0,' ',0.D0,NINIG,BGCR3,
     &               INIGAT,CODRET,'FM')
            SAT  = INIGAT(1)
            ATMP = INIGAT(2)
C
C     CALCUL DE LA COMPRESSIBILITE DU FLUIDE
C
      COMPW=CLIQ
      IF(COMPW.GT.0.D0) GO TO 106
      IF(SAT.GE.0.999D0) GO TO 106
      COMPW=(1.D0-SAT+SAT*HENRY)*ATMP/(ATMP+P1)**2.D0
 106  CONTINUE
C
      ENDIF
      IF (THMC.EQ.'GAZ') THEN
         CALL RCVALA(IMATE,'THM_GAZ',1,'TEMP',T,NGAZ,NCRA3,
     &               GAZ,CODRET,'FM')
         MAMOLG=GAZ(1)
         CP11=GAZ(2)
         RHO110=MAMOLG*P10/R/T0
      ENDIF
      IF (((THMC.EQ.'LIQU_VAPE_GAZ').OR.(THMC.EQ.'LIQU_GAZ')).
     &     OR.(THMC.EQ.'LIQU_GAZ_ATM')) THEN
         CALL RCVALA(IMATE,'THM_LIQU',1,'TEMP',T,NLIQ,NCRA2,
     &               LIQ,CODRET,'FM')
         CALL RCVALA(IMATE,'THM_VAPE_GAZ',1,'TEMP',T,NVAP,NCRA4,
     &               VAP,CODRET,'FM')
         CALL RCVALA(IMATE,'THM_GAZ',1,'TEMP',T,NGAZ,NCRA3,
     &               GAZ,CODRET,'FM')
         RHO110=LIQ(1)
         CLIQ=LIQ(2)
         ALPLIQ=LIQ(3)
         CP11=LIQ(4)
         MAMOLG=GAZ(1)
         CP21=GAZ(2)
         RHO210=MAMOLG*P20/R/T0
C
C LIQUIDE GAZ 1 = GAZ + VAPEUR : ON RAJOUTE DONC LA VAPEUR
C  RHO21 A UNE NOUVELLE EXPRESSION
C
         IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
            MAMOLV=VAP(1)
            CP12=VAP(2)
            RHO120=MAMOLV*PVP0/R/T0
            RHO210=MAMOLG*(P20-PVP0)/R/T0
         ENDIF
         IF (HYDR.EQ.'HYDR_UTIL') THEN     
             CALL RCVALA(IMATE,'THM_DIFFU',1,'PCAP',P1,NSAT,
     &            NCRA6,SATUR,CODRET,'FM')
             SAT=SATUR(1)
             DSATP1=SATUR(2)
         ELSE
             CALL SATURA(HYDR,P1,SAT,DSATP1)
         ENDIF
         IF (HYDR.EQ.'HYDR_UTIL') THEN     
             CALL RCVALA(IMATE,'THM_DIFFU',1,'PCAP',P1-DP1,NSAT,
     &            NCRA6,SATUR,CODRET,'FM')
             SATM=SATUR(1)
             DSATP1=SATUR(2)
         ELSE
             CALL SATURA(HYDR,P1-DP1,SATM,BIDON)
         ENDIF
      ENDIF
C
        IF (THMC.EQ.'LIQU_NSAT_GAT')THEN
         CALL RCVALA(IMATE,'THM_LIQU',1,'TEMP',T,NLIQ,NCRA2,
     &               LIQ,CODRET,'FM')
         CALL RCVALA(IMATE,'THM_VAPE_GAZ',1,'TEMP',T,NVAP,NCRA4,
     &               VAP,CODRET,'FM')
         CALL RCVALA(IMATE,'THM_GAZ',1,'TEMP',T,NGAZ,NCRA3,
     &               GAZ,CODRET,'FM')

         RHO110=LIQ(1)
         CLIQ=LIQ(2)
         ALPLIQ=LIQ(3)
         CP11=LIQ(4)
         CALL RCVALA(IMATE,'THM_AIR_DISS',0,' ',0.D0,NCOF,BGCR9,
     &               COFGAT,CODRET,'FM')
            HENRY=COFGAT(1)
            CALL RCVALA(IMATE,'THM_INIT',0,' ',0.D0,NINIG,BGCR3,
     &               INIGAT,CODRET,'FM')
            ATMP = INIGAT(2)
         MAMOLG=GAZ(1)
         CP21=GAZ(2)
         MAMOLV=VAP(1)
         CP12=VAP(2)
         RV00=1.D-3*EXP(19.819D0-4975.9D0/T0)
         HUMR0=(1.D0+((PHI0*S0GAT)/(.04D0*RV00))**(-4.27D0))**(-.42D0)
         RHO120=RV00*HUMR0
         PVP0=RHO120*R*T0/MAMOLV
         RHO210=MAMOLG*(P20-PVP0)/R/T0
         COMPW=CLIQ
        ENDIF    
C
C **********************************************************************
C CALCUL DE LA VARIABLE INTERNE PHI A L'INSTANT PLUS
C
      IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &   (OPTION(1:9).EQ.'FULL_MECA')) THEN
         IF (YAMEC.EQ.1) THEN
          IF (MECA.EQ.'ELAS_THM'.OR.MECA.EQ.'SURF_ETAT_SATU'
     &                        .OR.MECA.EQ.'CAM_CLAY_THM')THEN
            VARIA=DEPSV
            IF (YATE.EQ.1) THEN
                VARIA=VARIA-3.D0*BETAT*DT 
            ENDIF         
            IF (THMC.EQ.'LIQU_SATU_GAT') THEN
               VARIA=VARIA+DP1*COMPS
               VINTP(ADVITH)=
     >             RBBP-PHI0-(RBBP-VINTM(ADVITH)-PHI0)*EXP(-VARIA)
            ENDIF
C
          ENDIF
          IF (MECA.EQ.'SURF_ETAT_NSAT')THEN
C          CALCUL DE DN
            DSUC=DP2-DP1
            SUCN=ABS(DSUC)/ATMP
            DSY=0.D0
              DO 107 I=1,3
                 DSY=DSY+CONGEP(ADCOME-1+I)-CONGEM(ADCOME-1+I)
 107          CONTINUE
            DSY=DSY/3.D0
            SYN=ABS(DSY)/ATMP

C         LIRE LES COEFFICIENTS CALCUL LES LOIS
            NBPAR  = 0
            VALPAR = 0.D0
            NOMPAR = ' '
             CALL RCVALA ( IMATE,'ELAS_THM',NBPAR,NOMPAR,VALPAR,
     &                      2,BGCR1,VALRES,CODRET, 'FM' )
            RHOS   = VALRES(1)
            COMPS  = VALRES(2)
             CALL RCVALA ( IMATE,'SURF_ETAT_NSAT',NBPAR,NOMPAR,VALPAR,
     &              NSUREV,BGCR4,SUREV,CODRET, 'FM' )
            EVAE    = SUREV(1)
            EVBE    = SUREV(2)        
            EVCTE   = SUREV(3)
            EVSIGB  = SUREV(4)
            EVKB    = SUREV(5)
            EVXM    = SUREV(6)
C

               EVCE= EVKB*ATMP
               SIGBN=EVSIGB/ATMP
               DNEXP=EVCE*(1.D0-EVXM)/ATMP
               DNOM1=EVAE-EVBE*SUCN/SIGBN
               DNOM2=DNOM1*SYN+EVBE*SUCN
                 IF(EVXM.EQ.1.D0)THEN
                    VARIA=+EVCTE*DT
                 ELSE
                    VARIA=(DNOM2)**(1.D0-EVXM)/DNEXP+EVCTE*DT
                 ENDIF
               VINTP(ADVITH)=
     >             1.D0-PHI0-(1.D0-VINTM(ADVITH)-PHI0)*EXP(-VARIA)
          ENDIF
         ENDIF
      ENDIF
C    
C **********************************************************************
C  CALCUL DE LA VARIABLE INTERNE RHO11 A L'INSTANT PLUS SI LIQUIDE
C
C
      IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &   (OPTION(1:9).EQ.'FULL_MECA')) THEN
         IF ((THMC.EQ.'LIQU_SATU')) THEN
            VARIA=DP1*CLIQ
            IF (YATE.EQ.1) THEN 
               VARIA=VARIA-3.D0*ALPLIQ*DT
            ENDIF
            VINTP(ADVITH+1)=-RHO110+(VINTM(ADVITH+1)+RHO110)*EXP(VARIA)
         ENDIF
         IF ((THMC.EQ.'LIQU_SATU_GAT')) THEN
            VARIA=DP1*COMPW
            IF (YATE.EQ.1) THEN 
               VARIA=VARIA-3.D0*ALPLIQ*DT
            ENDIF
            VINTP(ADVITH+1)=-RHO110+(VINTM(ADVITH+1)+RHO110)*EXP(VARIA)
         ENDIF
         IF ((THMC.EQ.'LIQU_GAZ_ATM')) THEN
            VARIA=-DP1*CLIQ
            IF (YATE.EQ.1) THEN 
               VARIA=VARIA-3.D0*ALPLIQ*DT
            ENDIF
            VINTP(ADVITH+1)=-RHO110+(VINTM(ADVITH+1)+RHO110)*EXP(VARIA)
         ENDIF
         IF ((THMC.EQ.'LIQU_VAPE_GAZ').OR.(THMC.EQ.'LIQU_GAZ')) THEN
            VARIA=(DP2-DP1)*CLIQ
            IF (YATE.EQ.1) THEN 
               VARIA=VARIA-3.D0*ALPLIQ*DT
            ENDIF
            VINTP(ADVITH+1)=-RHO110+(VINTM(ADVITH+1)+RHO110)*EXP(VARIA)
         ENDIF
         IF (THMC.EQ.'LIQU_NSAT_GAT')THEN
            VARIA=DP1*COMPW
            IF (YATE.EQ.1) THEN 
               VARIA=VARIA-3.D0*ALPLIQ*DT
            ENDIF
            VINTP(ADVITH+1)=-RHO110+(VINTM(ADVITH+1)+RHO110)*EXP(VARIA)
         ENDIF    
      ENDIF
      
C
C **********************************************************************
C CALCUL DE PHI ET DE RHO11 (SI LIQ) A L'INSTANT COURANT
C
      IF (OPTION(1:16).EQ.'RIGI_MECA_TANG') THEN
         IF (YAMEC.EQ.1) THEN
            PHI=VINTM(ADVITH)+PHI0
         ELSE
            PHI=PHI0
         ENDIF
         IF ((THMC.EQ.'LIQU_SATU').OR.
     >       (THMC.EQ.'LIQU_GAZ_ATM').OR.
     >       (THMC.EQ.'LIQU_SATU_GAT').OR.
     >       (THMC.EQ.'LIQU_VAPE_GAZ').OR.
     >       (THMC.EQ.'LIQU_GAZ').OR.
     >       (THMC.EQ.'LIQU_NSAT_GAT')) THEN
               RHO11=VINTM(ADVITH+1)+RHO110
               RHO11M=VINTM(ADVITH+1)+RHO110
         ENDIF    
      ELSE
         IF (YAMEC.EQ.1) THEN
            PHI =VINTP(ADVITH)+PHI0
            PHIM=VINTM(ADVITH)+PHI0
         ELSE
            PHI=PHI0
         ENDIF
         IF ((THMC.EQ.'LIQU_SATU').OR.
     >       (THMC.EQ.'LIQU_GAZ_ATM').OR.
     >       (THMC.EQ.'LIQU_SATU_GAT').OR.
     >       (THMC.EQ.'LIQU_VAPE_GAZ').OR.
     >       (THMC.EQ.'LIQU_GAZ').OR.
     >       (THMC.EQ.'LIQU_NSAT_GAT')) THEN
            RHO11=VINTP(ADVITH+1)+RHO110
            RHO11M=VINTM(ADVITH+1)+RHO110
         ENDIF     
      ENDIF
      IF (THMC.EQ.'GAZ') THEN
         RHO11=MAMOLG*P1/R/T
         RHO11M=MAMOLG*(P1-DP1)/R/(T-DT)
      ENDIF
C
C **********************************************************************
C CALCUL DES AUTRES COEFFICIENTS DEDUITS : DILATATIONS ALPHA
C ET C0EPS DANS LE CAS D'UN SEUL FLUIDE
C
      IF (YATE.EQ.1) THEN
         IF (THMC.EQ.'LIQU_SATU') THEN
            ALP11=ALPLIQ*PHI
            C0EPS=(1.D0-PHI)*RHOS*CSIGM+PHI*RHO11*CP11
     >            -9.D0*T*K0*ALPHA0*ALPHA0
            IF (YAMEC.EQ.1) THEN
               ALP11=ALP11+ALPHA0*(BIOT-PHI)
            ENDIF
         ENDIF 
         IF (THMC.EQ.'LIQU_SATU_GAT') THEN
            ALP11=ALPLIQ*PHI
            RALP=PHI*RHO11*CP11+(1.D0-PHI)*RHOS*CSIGM
            RFXP=3.D0*(1.D0-PHI)*RHOS*CSIGM*TALS
            C0EPS=RALP-RFXP*T
            IF (YAMEC.EQ.1) THEN
               ALP11=ALP11+TALS*(1.D0-PHI)+(RBBP-1.D0)*BETAT
            ENDIF
         ENDIF         
         IF (THMC.EQ.'GAZ') THEN
            ALP11=PHI/3.D0/T
            C0EPS=(1.D0-PHI)*RHOS*CSIGM+PHI*RHO11*CP11
     >            -9.D0*T*K0*ALPHA0*ALPHA0
            IF (YAMEC.EQ.1) THEN
               ALP11=ALP11+ALPHA0*(BIOT-PHI)   
            ENDIF  
         ENDIF
         IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
            ALP11=ALPLIQ*PHI
            IF (YAMEC.EQ.1) THEN
               ALP11=ALP11+ALPHA0*(BIOT-PHI)
            ENDIF
            C0EPS=(1.D0-PHI)*RHOS*CSIGM+PHI*RHO11*CP11
     >            -9.D0*T*K0*ALPHA0*ALPHA0
         ENDIF            
         IF ((THMC.EQ.'LIQU_VAPE_GAZ').OR.(THMC.EQ.'LIQU_GAZ')) THEN
            IF (YAMEC.NE.1) ALPHA0=0.D0
            ALP11=SAT*(BIOT-PHI)*ALPHA0+ALPLIQ*PHI*SAT
            ALP12=(BIOT-PHI)*(1.D0-SAT)*ALPHA0+PHI*
     &                 (1.D0-SAT)/3.D0/T
            ALP21=ALP12
         ENDIF
         IF (THMC.EQ.'LIQU_NSAT_GAT')THEN
C           RIEN A AJOUTER
         ENDIF    
      ENDIF
      IF (THMC.EQ.'LIQU_GAZ') THEN
         RHO21=MAMOLG*P2/R/T 
         RHO21M=MAMOLG*(P2-DP2)/R/(T-DT)
         IF (YATE.EQ.1) THEN
            C0EPS=(1.D0-PHI)*RHOS*CSIGM+PHI*SAT*RHO11*CP11
     &            +PHI*(1.D0-SAT)*(RHO21*CP21)
     >            -9.D0*T*K0*ALPHA0*ALPHA0
         ENDIF
      ENDIF
C
C      
C **********************************************************************
C CALCUL ENTHALPIES ET DERIVEES DES ENTHALPIES
C CALCUL FAIT EN AVANCE CAR ON A BESOIN DE H11 ET H12 POUR PVP
C
C  CAS LIQUIDE ET GAZ
C
      IF (((THMC.EQ.'LIQU_VAPE_GAZ').AND.(YATE.EQ.1)).OR.
     &    ((THMC.EQ.'LIQU_GAZ').AND.(YATE.EQ.1)))  THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            DSDE(ADCP11+NDIM+1,ADDEP2)=DSDE(ADCP11+NDIM+1,ADDEP2)
     &                              +(1.D0-3.D0*ALPLIQ*T)/RHO11
            DSDE(ADCP11+NDIM+1,ADDEP1)=DSDE(ADCP11+NDIM+1,ADDEP1)
     &                              -(1.D0-3.D0*ALPLIQ*T)/RHO11
            DSDE(ADCP11+NDIM+1,ADDETE)=DSDE(ADCP11+NDIM+1,ADDETE)+CP11
            DSDE(ADCP21+NDIM+1,ADDETE)=DSDE(ADCP21+NDIM+1,ADDETE)+CP21
            IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
              DSDE(ADCP12+NDIM+1,ADDETE)=DSDE(ADCP12+NDIM+1,ADDETE)+CP12
            ENDIF
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCP11+NDIM+1)=CONGEP(ADCP11+NDIM+1)
     &                                  +(1.D0-3.D0*ALPLIQ*T)/RHO11*DP2
     &                            -(1.D0-3.D0*ALPLIQ*T)/RHO11*DP1
     &                            +CP11*DT
            IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
               CONGEP(ADCP12+NDIM+1)=CONGEP(ADCP12+NDIM+1)+CP12*DT
            ENDIF
            CONGEP(ADCP21+NDIM+1)=CONGEP(ADCP21+NDIM+1)+CP21*DT
         ENDIF
      ENDIF
      IF ((THMC.EQ.'LIQU_NSAT_GAT').AND.(YATE.EQ.1))  THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            DSDE(ADCP11+NDIM+1,ADDETE)=DSDE(ADCP11+NDIM+1,ADDETE)+CP11
            DSDE(ADCP12+NDIM+1,ADDETE)=DSDE(ADCP12+NDIM+1,ADDETE)+CP12
            DSDE(ADCP21+NDIM+1,ADDETE)=DSDE(ADCP21+NDIM+1,ADDETE)+CP21
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCP11+NDIM+1)=CONGEP(ADCP11+NDIM+1)+CP11*DT
            CONGEP(ADCP12+NDIM+1)=CONGEP(ADCP12+NDIM+1)+CP12*DT
            CONGEP(ADCP21+NDIM+1)=CONGEP(ADCP21+NDIM+1)+CP21*DT
         ENDIF
      ENDIF
C
C  CAS UN SEUL CONSTITUANT : LIQUIDE 1
C
      IF ((THMC.EQ.'LIQU_SATU').AND.(YATE.EQ.1)) THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            DSDE(ADCP11+NDIM+1,ADDEP1)=DSDE(ADCP11+NDIM+1,ADDEP1)
     &                              +(1.D0-3.D0*ALPLIQ*T)/RHO11
            DSDE(ADCP11+NDIM+1,ADDETE)=DSDE(ADCP11+NDIM+1,ADDETE)+CP11
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCP11+NDIM+1)=CONGEP(ADCP11+NDIM+1)
     &                     +(1.D0-3.D0*ALPLIQ*T)/RHO11*DP1+CP11*DT
         ENDIF
      ENDIF
C
      IF ((THMC.EQ.'LIQU_SATU_GAT').AND.(YATE.EQ.1)) THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            DSDE(ADCP11+NDIM+1,ADDETE)=DSDE(ADCP11+NDIM+1,ADDETE)+CP11
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCP11+NDIM+1)=CONGEP(ADCP11+NDIM+1)
     &                     +CP11*DT
         ENDIF
      ENDIF
C
C  CAS UN SEUL CONSTITUANT : LIQUIDE 2 MAIS NON SATURE
C
      IF ((THMC.EQ.'LIQU_GAZ_ATM').AND.(YATE.EQ.1)) THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            DSDE(ADCP11+NDIM+1,ADDEP1)=DSDE(ADCP11+NDIM+1,ADDEP1)
     &                              -(1.D0-3.D0*ALPLIQ*T)/RHO11
            DSDE(ADCP11+NDIM+1,ADDETE)=DSDE(ADCP11+NDIM+1,ADDETE)+CP11
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCP11+NDIM+1)=CONGEP(ADCP11+NDIM+1)
     &                     -(1.D0-3.D0*ALPLIQ*T)/RHO11*DP1+CP11*DT
         ENDIF
      ENDIF
C
C  CAS UN SEUL CONSTITUANT : GAZ
C
      IF ((THMC.EQ.'GAZ').AND.(YATE.EQ.1)) THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            DSDE(ADCP11+NDIM+1,ADDETE)=DSDE(ADCP11+NDIM+1,ADDETE)+CP11
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCP11+NDIM+1)=CONGEP(ADCP11+NDIM+1)+CP11*DT
         ENDIF
      ENDIF
C
C **********************************************************************
C  CALCUL DES ENTHALPIES INTERMEDIAIRES H11,H12,H21
C  UNIQUEMENT SI CHANGEMENT DE PHASE (I.E. LIQUIDE GAZ 1 )
C 
      IF ((THMC.EQ.'LIQU_VAPE_GAZ').AND.(YATE.EQ.1).OR.
     &     (THMC.EQ.'LIQU_NSAT_GAT').AND.(YATE.EQ.1)) THEN
         IF (OPTION.EQ.'RIGI_MECA_TANG') THEN
           H11=CONGEM(ADCP11+NDIM+1)
           H12=CONGEM(ADCP12+NDIM+1)
           H21=CONGEM(ADCP21+NDIM+1)
         ELSE      
           H11=CONGEP(ADCP11+NDIM+1)
           H12=CONGEP(ADCP12+NDIM+1)
           H21=CONGEP(ADCP21+NDIM+1)
         ENDIF
      ENDIF
C      
C **********************************************************************
      IF ((THMC.EQ.'LIQU_NSAT_GAT').AND.(YATE.EQ.1)) THEN
         IF (OPTION.EQ.'RIGI_MECA_TANG') THEN
         P1M=P1-DP1
         P2M=P2-DP2
            CALL PRCIPE(CONGEM,DIMCON,S1,S2,S3,NDIM)
            CALL STATVT(IMATE,MECA,DIMCON,ADCOME,CONGEM,ATMP,S3,P1M,
     &               P2M,J1,J2,J3, BT,DF,DC,RDBTDT,RDBTP1,RDBTP2,PHI)
            J1D= BT*J1
C            J1F=0.D0
C            J1C=0.0D0 
            J1F=DF*J1
            J1C=DC*J1 
C         CALCUL DE DEGRE DE SATURATION ET SES DERIVEES
            CALL STATSR(IMATE,MECA,ADCOME,CONGEM,DIMCON,P1M,P2M,DT,
     &                   SAT,G1,G2,G3)
              G1D= BT*G1
C              G1F= 0.D0
C              G1C= 0.D0
              G1F= DF*G1
              G1C= DC*G1
         ELSE      
            CALL PRCIPE(CONGEP,DIMCON,S1,S2,S3,NDIM)
            CALL STATVT(IMATE,MECA,DIMCON,ADCOME,CONGEP,ATMP,S3,P1,P2,
     &                  J1,J2,J3, BT,DF,DC,RDBTDT,RDBTP1,RDBTP2,PHI)
            J1D= BT*J1
C            J1F=0.D0
C            J1C=0.0D0 
            J1F=DF*J1
            J1C=DC*J1 
C         CALCUL DE DEGRE DE SATURATION ET SES DERIVEES
            CALL STATSR(IMATE,MECA,ADCOME,CONGEP,DIMCON,P1,P2,DT,
     &                   SAT,G1,G2,G3)
              G1D= BT*G1
C              G1F= 0.D0
C              G1C= 0.D0
              G1F= DF*G1
              G1C= DC*G1
         ENDIF
      ENDIF
C      
C **********************************************************************
C  CALCUL DE LA VARIABLE INTERNE PRESSION VAPEUR
C   ET DES AUTRES MASSES VOLUMIQUES AVEC 2 FLUIDES, AINSI
C    QUE COEPS
C
      IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
         IF (OPTION(1:16).EQ.'RIGI_MECA_TANG') THEN 
            PVP=VINTM(ADVITH+2)+PVP0
            PVPM=VINTM(ADVITH+2)+PVP0
         ELSE
            VARIA=MAMOLV/R/T*(DP2-DP1)/RHO11
            IF (YATE.EQ.1)THEN 
               VARIA=VARIA+(CONGEM(ADCP12+NDIM+1)-CONGEM(ADCP11+NDIM+1))
     &               *(1.D0/(T-DT)-1.D0/T)*MAMOLV/R
               VARIA=VARIA+(CP12-CP11)*(LOG(T/(T-DT))-(DT/T))
     &               *MAMOLV/R
            ENDIF
            VINTP(ADVITH+2)=-PVP0+(VINTM(ADVITH+2)+PVP0)*EXP(VARIA)
            PVP =VINTP(ADVITH+2)+PVP0
            PVPM=VINTM(ADVITH+2)+PVP0 
         ENDIF
         RHO12=MAMOLV*PVP/R/T
         RHO21=MAMOLG*(P2-PVP)/R/T
         RHO12M=MAMOLV*PVPM/R/(T-DT)
         RHO21M=MAMOLG*(P2-DP2-PVPM)/R/(T-DT)
         IF (YATE.EQ.1) THEN
            C0EPS=(1.D0-PHI)*RHOS*CSIGM+PHI*SAT*RHO11*CP11
     &            +PHI*(1.D0-SAT)*(RHO12*CP12+RHO21*CP21)
     >            -9.D0*T*K0*ALPHA0*ALPHA0
         ENDIF
      ENDIF
      IF (THMC.EQ.'LIQU_NSAT_GAT') THEN
C     CALCUL DE RHO21, RHO12  ET COEPS
        CALL CONDT(OPTION,PHI,SAT,T,P2,R,MAMOLV,MAMOLG
     &                ,DT,RV00,VINTM,VINTP,NVIMEC,NVITH,ADVITH
     &                ,AVSR,BVN,EVT,AASR,BAN,EAT,HENRY
     &                ,RV0,HUMR,RHO12,RHO21)
         IF (YATE.EQ.1) THEN
            C0EPS=(1.D0-PHI)*RHOS*CSIGM+PHI*SAT*RHO11*CP11
     &            +PHI*(1.D0-SAT)*RHO12*CP12
     &            +RHO21*CP21*(1.D0-(1.D0-HENRY)*SAT)*PHI
     &            -RHOS*CSIGM*(J3-J1C)*(T-DT/2)
         ENDIF
      ENDIF
C
C **********************************************************************
C  CALCUL DE SIGMAP
C
      IF (((THMC.EQ.'LIQU_VAPE_GAZ').AND.(YAMEC.EQ.1)).OR.
     &    ((THMC.EQ.'LIQU_GAZ').AND.(YAMEC.EQ.1))) THEN  
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            DSDE(ADCOME+6,ADDEP1)=DSDE(ADCOME+6,ADDEP1)+BIOT*SAT
            DSDE(ADCOME+6,ADDEP2)=DSDE(ADCOME+6,ADDEP2)-BIOT
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCOME+6)=CONGEP(ADCOME+6)+BIOT*SAT*DP1-BIOT*DP2
         ENDIF
      ENDIF
      IF ((THMC.EQ.'LIQU_NSAT_GAT').AND.(YAMEC.EQ.1)) THEN  
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            DSDE(ADCOME+6,ADDEP1)=DSDE(ADCOME+6,ADDEP1)-DF
            DSDE(ADCOME+6,ADDEP2)=DSDE(ADCOME+6,ADDEP2)+(DF-1.D0)
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCOME+6)=CONGEP(ADCOME+6)-DF*DP1+(DF-1.D0)*DP2
         ENDIF
      ENDIF

C
      IF ((THMC.EQ.'LIQU_SATU').OR.(THMC.EQ.'GAZ')) THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
             DSDE(ADCOME+6,ADDEP1)=DSDE(ADCOME+6,ADDEP1)-BIOT
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCOME+6)=CONGEP(ADCOME+6)-BIOT*DP1
         ENDIF
      ENDIF
CCCC
C
      IF ((THMC.EQ.'LIQU_SATU_GAT')) THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
             DSDE(ADCOME+6,ADDEP1)=DSDE(ADCOME+6,ADDEP1)-RBBP
             DSDE(ADCOME+6,ADDETE)=DSDE(ADCOME+6,ADDETE)-RDBBDT*DP1
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCOME+6)=CONGEP(ADCOME+6)-RBBP*DP1
         ENDIF
      ENDIF
CCCCC
      IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
             DSDE(ADCOME+6,ADDEP1)=DSDE(ADCOME+6,ADDEP1)+BIOT*SAT
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCOME+6)=CONGEP(ADCOME+6)+BIOT*SAT*DP1
         ENDIF
      ENDIF
C
C **********************************************************************
C  CALCUL DES APPORTS MASSIQUES  
C
      IF ((THMC.EQ.'LIQU_SATU').OR.(THMC.EQ.'GAZ').OR.
     &    (THMC.EQ.'LIQU_SATU_GAT')) THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
            IF (THMC.EQ.'LIQU_SATU') THEN
               N=(BIOT-PHI)*CS+PHI*CLIQ
            ELSE IF (THMC.EQ.'LIQU_SATU_GAT') THEN
               N=(RBBP-PHI)*COMPS+PHI*COMPW
               BIOT=RBBP
            ELSE IF (THMC.EQ.'GAZ') THEN
               N=(BIOT-PHI)*CS+PHI/P1
            ENDIF 
            DSDE(ADCP11,ADDEP1)=DSDE(ADCP11,ADDEP1)+RHO11*N
            IF (YATE.EQ.1) THEN
               DSDE(ADCP11,ADDETE)=DSDE(ADCP11,ADDETE)-3.D0*RHO11*ALP11
            ENDIF
            IF (YAMEC.EQ.1) THEN
               DO 200 I=1,3
                  DSDE(ADCP11,ADDEME+NDIM-1+I)=
     &                   DSDE(ADCP11,ADDEME+NDIM-1+I)+RHO11*BIOT
 200           CONTINUE
            ENDIF
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
            CONGEP(ADCP11)=CONGEM(ADCP11)+
     >          PHI*RHO11*(1.D0+EPSV)-PHIM*RHO11M*(1.D0+EPSVM)
         ENDIF
      ENDIF
C
      IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            DSDE(ADCP11,ADDEP1)=DSDE(ADCP11,ADDEP1)
     &         +RHO11*(DSATP1*PHI-SAT*PHI*CLIQ-SAT*SAT*(BIOT-PHI)*CS)
            IF (YATE.EQ.1) THEN
               DSDE(ADCP11,ADDETE)=DSDE(ADCP11,ADDETE)-3.D0*RHO11*ALP11
            ENDIF
            IF (YAMEC.EQ.1) THEN
               DO 100 I=1,3
                  DSDE(ADCP11,ADDEME+NDIM-1+I)=
     &                   DSDE(ADCP11,ADDEME+NDIM-1+I)+RHO11*BIOT*SAT
 100           CONTINUE
            ENDIF
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
            CONGEP(ADCP11)=CONGEM(ADCP11)+
     >       PHI*SAT*RHO11*(1.D0+EPSV)-PHIM*SATM*RHO11M*(1.D0+EPSVM)
         ENDIF
      ENDIF
C       
      IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN          
            DSDE(ADCP11,ADDEP1)=DSDE(ADCP11,ADDEP1)
     &         +RHO11*(DSATP1*PHI-SAT*PHI*CLIQ-SAT*SAT*(BIOT-PHI)*CS)
            DSDE(ADCP11,ADDEP2)=DSDE(ADCP11,ADDEP2)
     &         +RHO11*SAT*(PHI*CLIQ+(BIOT-PHI)*CS)
            DSDE(ADCP12,ADDEP1)=DSDE(ADCP12,ADDEP1)
     &         +RHO12*(-DSATP1*PHI-(1.D0-SAT)*SAT*(BIOT-PHI)*CS)
     &         -RHO12*PHI*(1.D0-SAT)/PVP*RHO12/RHO11
            DSDE(ADCP12,ADDEP2)=DSDE(ADCP12,ADDEP2)
     &         +RHO12*((BIOT-PHI)*(1.D0-SAT)*CS)
     &         +RHO12*PHI*(1.D0-SAT)/PVP*RHO12/RHO11
            DSDE(ADCP21,ADDEP1)=DSDE(ADCP21,ADDEP1)
     &         +RHO21*(-DSATP1*PHI-(1.D0-SAT)*SAT*(BIOT-PHI)*CS)
     &         +RHO21*PHI*(1.D0-SAT)/(P2-PVP)*RHO12/RHO11
            DSDE(ADCP21,ADDEP2)=DSDE(ADCP21,ADDEP2)
     &         +RHO21*((BIOT-PHI)*(1.D0-SAT)*CS)
     &         +RHO21*(PHI*(1.D0-SAT)/(P2-PVP)*(RHO11-RHO12)/RHO11)
            IF (YAMEC.EQ.1) THEN
               DO 101 I=1,3
                  DSDE(ADCP11,ADDEME+NDIM-1+I)=
     &               DSDE(ADCP11,ADDEME+NDIM-1+I)+RHO11*BIOT*SAT
                  DSDE(ADCP12,ADDEME+NDIM-1+I)=
     &               DSDE(ADCP12,ADDEME+NDIM-1+I)+RHO12*BIOT*(1.D0-SAT)
                  DSDE(ADCP21,ADDEME+NDIM-1+I)=
     &               DSDE(ADCP21,ADDEME+NDIM-1+I)+RHO21*BIOT*(1.D0-SAT)
 101            CONTINUE
            ENDIF
            IF (YATE.EQ.1) THEN
               DSDE(ADCP11,ADDETE)=DSDE(ADCP11,ADDETE)
     &            -3.D0*RHO11*ALP11
               DSDE(ADCP12,ADDETE)=DSDE(ADCP12,ADDETE)
     &            +RHO12*RHO12*PHI*(1.D0-SAT)/PVP*(H12-H11)/T
     &            -3.D0*RHO12*ALP12
               DSDE(ADCP21,ADDETE)=DSDE(ADCP21,ADDETE)
     &            -RHO21*RHO12*PHI*(1.D0-SAT)/(P2-PVP)*(H12-H11)/T
     &            -3.D0*RHO21*ALP21
            ENDIF 
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &         (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCP11)=CONGEM(ADCP11)+
     >          PHI*SAT*RHO11*(1.D0+EPSV)-
     >          PHIM*SATM*RHO11M*(1.D0+EPSVM)
            CONGEP(ADCP12)=CONGEM(ADCP12)+
     >          PHI *(1.D0-SAT )*RHO12 *(1.D0+EPSV )-
     >          PHIM*(1.D0-SATM)*RHO12M*(1.D0+EPSVM)
            CONGEP(ADCP21)=CONGEM(ADCP21)+
     >          PHI *(1.D0-SAT )*RHO21 *(1.D0+EPSV )-
     >          PHIM*(1.D0-SATM)*RHO21M*(1.D0+EPSVM)
         ENDIF
      ENDIF
C
C  CALCUL DES APPORTS MASSIQUES ET LEURS DERIVEES 
C       
      IF (THMC.EQ.'LIQU_NSAT_GAT') THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN          
            DSDE(ADCP11,ADDEP1)=DSDE(ADCP11,ADDEP1)
     &        +RHO11*(PHI*(G1F-G2)+SAT*(J1F-J2)+PHI*SAT*CLIQ)
            DSDE(ADCP11,ADDEP2)=DSDE(ADCP11,ADDEP2)
     &        -RHO11*(PHI*(G1F-G2)+SAT*(J1F-J2))
            DSDE(ADCP12,ADDEP1)=DSDE(ADCP12,ADDEP1)
     &        +RHO12*(PHI*AVSR*(G1F-G2)+(1.D0-SAT)*BVN*(J1F-J2))
            DSDE(ADCP12,ADDEP2)=DSDE(ADCP12,ADDEP2)
     &        -RHO12*(PHI*AVSR*(G1F-G2)+(1.D0-SAT)*BVN*(J1F-J2))
            DSDE(ADCP21,ADDEP1)=DSDE(ADCP21,ADDEP1)
     &        +RHO21*(-PHI*AASR*(G1F-G2)
     &                -(1.D0-(1.D0-HENRY)*SAT)*BAN*(J2-J1F))
            DSDE(ADCP21,ADDEP2)=DSDE(ADCP21,ADDEP2)
     &        +RHO21*(PHI*AASR*(G1F-G2)+(1.D0-(1.D0-HENRY)*SAT)*(J2-J1F)
     &        *BAN+PHI*(1.D0-(1.D0-HENRY)*SAT)*MAMOLG/RHO21/R/T)
            IF (YAMEC.EQ.1) THEN
               DO 111 I=1,3
               DSDE(ADCP11,ADDEME+NDIM-1+I)=DSDE(ADCP11,ADDEME+NDIM-1+I)
     &            +RHO11*(PHI*G1D+SAT*J1D)
               DSDE(ADCP12,ADDEME+NDIM-1+I)=DSDE(ADCP12,ADDEME+NDIM-1+I)
     &            +RHO12*(PHI*AVSR*G1D+(1.D0-SAT)*BVN*J1D)
               DSDE(ADCP21,ADDEME+NDIM-1+I)=DSDE(ADCP21,ADDEME+NDIM-1+I)
     &            +RHO21*(-PHI*AASR*G1D+(1.D0-(1.D0-HENRY)*SAT)*BAN*J1D)
 111            CONTINUE
            ENDIF
            IF (YATE.EQ.1) THEN
               DSDE(ADCP11,ADDETE)=DSDE(ADCP11,ADDETE)+
     &             RHO11*(PHI*(G3-G1C)+SAT*(J3-J1C)-3.D0*PHI*SAT*ALPLIQ)
               DSDE(ADCP12,ADDETE)=DSDE(ADCP12,ADDETE)+
     &             RHO12*(PHI*AVSR*(G3-G1C)+(1.D0-SAT)*BVN*(J3-J1C)
     &                    +PHI*(1.D0-SAT)*EVT/RHO12)
               DSDE(ADCP21,ADDETE)=DSDE(ADCP21,ADDETE)+
     &            RHO21*(-PHI*AASR*(G3-G1C)+(1.D0-(1.D0-HENRY)*SAT)*BAN*
     &                   (J3-J1C)-PHI*(1.D0-(1.D0-HENRY)*SAT)*EAT/RHO21)
            ENDIF 
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &         (OPTION(1:9).EQ.'FULL_MECA')) THEN 
C
C POUR LE COMPORTEMENT 
C
              CONGEP(ADCP11)=PHI*SAT*RHO11*(1.D0+EPSV)-PHI0*S0GAT*RHO110
              CONGEP(ADCP12)=PHI*(1.D0-SAT)*RHO12*(1.D0+EPSV)
     &                        -PHI0*(1.D0-S0GAT)*RHO120
              CONGEP(ADCP21)=PHI*(1.D0-(1.D0-HENRY)*SAT)*RHO21*
     &                 (1.D0+EPSV)-PHI0*(1.D0-(1.D0-HENRY)*S0GAT)*RHO210
         ENDIF
      ENDIF
      IF (THMC.EQ.'LIQU_GAZ') THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN          
            DSDE(ADCP11,ADDEP1)=DSDE(ADCP11,ADDEP1)
     &         +RHO11*(DSATP1*PHI-SAT*PHI*CLIQ-SAT*SAT*(BIOT-PHI)*CS)
            DSDE(ADCP11,ADDEP2)=DSDE(ADCP11,ADDEP2)
     &         +RHO11*SAT*(PHI*CLIQ+(BIOT-PHI)*CS)
            DSDE(ADCP21,ADDEP1)=DSDE(ADCP21,ADDEP1)
     &         +RHO21*(-DSATP1*PHI-(1.D0-SAT)*SAT*(BIOT-PHI)*CS)
            DSDE(ADCP21,ADDEP2)=DSDE(ADCP21,ADDEP2)
     &         +RHO21*((BIOT-PHI)*(1.D0-SAT)*CS)
     &         +RHO21*(PHI*(1.D0-SAT)/P2)
            IF (YAMEC.EQ.1) THEN
               DO 103 I=1,3
                  DSDE(ADCP11,ADDEME+NDIM-1+I)=
     &               DSDE(ADCP11,ADDEME+NDIM-1+I)+RHO11*BIOT*SAT
                  DSDE(ADCP21,ADDEME+NDIM-1+I)=
     &               DSDE(ADCP21,ADDEME+NDIM-1+I)+RHO21*BIOT*(1.D0-SAT)
 103            CONTINUE
            ENDIF
            IF (YATE.EQ.1) THEN
               DSDE(ADCP11,ADDETE)=DSDE(ADCP11,ADDETE)
     &            -3.D0*RHO11*ALP11
               DSDE(ADCP21,ADDETE)=DSDE(ADCP21,ADDETE)
     &            -3.D0*RHO21*ALP21
            ENDIF 
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &         (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCP11)=CONGEM(ADCP11)+
     >          PHI*SAT*RHO11*(1.D0+EPSV)-
     >          PHIM*SATM*RHO11M*(1.D0+EPSVM)
            CONGEP(ADCP21)=CONGEM(ADCP21)+
     >          PHI *(1.D0-SAT )*RHO21 *(1.D0+EPSV )-
     >          PHIM*(1.D0-SATM)*RHO21M*(1.D0+EPSVM)
         ENDIF
      ENDIF
C      
C
C **********************************************************************
C    CALCUL DE LA CHALEUR REDUITE Q'
C
      IF (YATE.EQ.1) THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
C   TERME SUIVANTE  EST SANS CONTROLE, TOUT LE MONDE Y PASSE
C        DQ/DT  
             DSDE(ADCOTE,ADDETE)=DSDE(ADCOTE,ADDETE)+C0EPS
            IF (YAMEC.EQ.1) THEN
             IF (MECA.EQ.'ELAS_THM'.OR.MECA.EQ.'SURF_ETAT_SATU'
     &                           .OR.MECA.EQ.'CAM_CLAY_THM') THEN
C        DQ/DEPSILON
               DO 112 I=1,3
                  DSDE(ADCOTE,ADDEME+NDIM-1+I)=
     &                   DSDE(ADCOTE,ADDEME+NDIM-1+I)
     &                   +RBTP*T
 112           CONTINUE
             ENDIF
             IF (MECA.EQ.'SURF_ETAT_NSAT') THEN
C            CALCUL DE DQ/D(EPSILON)
               DO 114 I=1,3
                  DSDE(ADCOTE,ADDEME+NDIM-1+I)=
     &                   DSDE(ADCOTE,ADDEME+NDIM-1+I)
     &                     +DC*T
C     &                   -RHOS*CSIGM*T*J1D
 114           CONTINUE
             ENDIF
            ENDIF
            IF ((THMC.EQ.'LIQU_VAPE_GAZ').OR.
     &          (THMC.EQ.'LIQU_GAZ')) THEN
               DSDE(ADCOTE,ADDEP1)=DSDE(ADCOTE,ADDEP1)+3.D0*ALP11*T
               DSDE(ADCOTE,ADDEP2)=DSDE(ADCOTE,ADDEP2)
     &                             -3.D0*(ALP11+ALP12)*T
            ENDIF
            IF ((THMC.EQ.'LIQU_NSAT_GAT')) THEN
C            DQ/DP1
               DSDE(ADCOTE,ADDEP1)=DSDE(ADCOTE,ADDEP1)
     &                             +RHOS*CSIGM*T*(J2-J1F)
C            DQ/DP2
               DSDE(ADCOTE,ADDEP2)=DSDE(ADCOTE,ADDEP2)
     &                             -RHOS*CSIGM*T*(J2-J1F)
            ENDIF
            IF ((THMC.EQ.'LIQU_SATU').OR.(THMC.EQ.'GAZ')) THEN
               DSDE(ADCOTE,ADDEP1)=DSDE(ADCOTE,ADDEP1)-3.D0*ALP11*T
            ENDIF
                IF ((THMC.EQ.'LIQU_SATU_GAT')) THEN
C               DSDE(ADCOTE,ADDEP1)=DSDE(ADCOTE,ADDEP1)+RCTP*T
               DSDE(ADCOTE,ADDEP1)=DSDE(ADCOTE,ADDEP1)-3.D0*ALP11*T
                ENDIF
            IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
               DSDE(ADCOTE,ADDEP1)=DSDE(ADCOTE,ADDEP1)+3.D0*ALP11*T
            ENDIF
         ENDIF
C
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
C   TERME SUIVANTE  EST SANS CONTROLE, TOUT LE MONDE Y PASSE
            CONGEP(ADCOTE)=CONGEP(ADCOTE)+C0EPS*DT
            IF (YAMEC.EQ.1) THEN
             IF (MECA.EQ.'ELAS_THM'.OR.MECA.EQ.'SURF_ETAT_SATU'
     &                           .OR.MECA.EQ.'CAM_CLAY_THM') THEN
               CONGEP(ADCOTE)=CONGEP(ADCOTE)
     &                  +RBTP*(T-DT/2)*DEPSV
             ENDIF
             IF (MECA.EQ.'SURF_ETAT_NSAT') THEN
C            CALCUL DE Q, PARTIE MECANIQUE
               CONGEP(ADCOTE)=CONGEP(ADCOTE)
     &                     +DC*(T-DT/2)*DEPSV
             ENDIF 
            ENDIF
            IF ((THMC.EQ.'LIQU_SATU').OR.(THMC.EQ.'GAZ')) THEN
               CONGEP(ADCOTE)=CONGEP(ADCOTE)
     &                   -3.D0*ALP11*(T-DT/2)*DP1
            ENDIF
            IF ((THMC.EQ.'LIQU_SATU_GAT')) THEN
C               CONGEP(ADCOTE)=CONGEP(ADCOTE)+RCTP*(T-DT/2)*DP1
               CONGEP(ADCOTE)=CONGEP(ADCOTE)
     &                   -3.D0*ALP11*(T-DT/2)*DP1
            ENDIF
            IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
               CONGEP(ADCOTE)=CONGEP(ADCOTE)
     &                     +3.D0*ALP11*(T-DT/2)*DP1
            ENDIF
            IF ((THMC.EQ.'LIQU_VAPE_GAZ').OR.
     &          (THMC.EQ.'LIQU_GAZ')) THEN
               CONGEP(ADCOTE)=CONGEP(ADCOTE)+3.D0*ALP11*(T-DT/2)*DP1
               CONGEP(ADCOTE)=CONGEP(ADCOTE)-3.D0*(ALP11*(T-DT/2)
     &                        +ALP12*(T-DT/2))*DP2
            ENDIF   
            IF ((THMC.EQ.'LIQU_NSAT_GAT')) THEN
               CONGEP(ADCOTE)=CONGEP(ADCOTE)
     &                        +RHOS*CSIGM*(J2-J1F)*(T-DT/2)*DP1
     &                        -RHOS*CSIGM*(J2-J1F)*(T-DT/2)*DP2
            ENDIF   
         ENDIF
       ENDIF
C
      END
