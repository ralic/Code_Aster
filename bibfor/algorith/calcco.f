       SUBROUTINE CALCCO (OPTION,COMPOR,MECA,THMC,THER,HYDR,
     >                           INMECA,INTHMC,INTHER,INHYDR,
     >                           IMATE,
     &                           NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,
     &                           YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE,
     &                           ADDEME,ADCOME,ADVIME,ADVITH,
     &                           ADDEP1,ADCP11,ADCP12,
     &                           ADDEP2,ADCP21,ADCP22,
     &                           ADDETE,ADCOTE,
     &                           CONGEM,CONGEP,
     &                           VINTM,VINTP,
     &                           DSDE,
     &                           EPSV,DEPSV,P1,P2,DP1,DP2,T,DT,PHI,
     &                           PVP,H11,H12,H21,RHO11,PHI0,PVP0,
     &                           P10,P20,T0,SAT,RETCOM)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/09/2002   AUTEUR UFBHHLL C.CHAVANT 
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
C TOLE CRP_20
C TOLE CRP_21
C **********************************************************************
C ROUTINE CALCCO : CETTE ROUTINE CALCULE LES CONTRAINTES GENERALISEES
C   ET LA MATRICE TANGENTE DES GRANDEURS COUPLEES, A SAVOIR CELLES QUI
C   NE SONT PAS DES GRANDEURS DE MECANIQUE PURE OU DES FLUX PURS
C **********************************************************************
C
C OUT RETCOM : RETOUR LOI DE COMPORTEMENT
C COMMENTAIRE DE NMCONV : 
C                       = 0 OK
C                       = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
C                       = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
C  VARIABLES IN / OUT
C
      IMPLICIT NONE
      CHARACTER*16   OPTION,MECA,THMC,THER,HYDR,COMPOR(*)    
      INTEGER  INMECA,INTHMC,INTHER,INHYDR
      INTEGER        NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,IMATE
      INTEGER        YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE
      INTEGER        ADCOME,ADCP11,ADCP12,ADCP21,ADCP22,ADCOTE
      INTEGER        ADDEME,ADDEP1,ADDEP2,ADDETE,ADVIME,ADVITH
      REAL*8         CONGEM(DIMCON),CONGEP(DIMCON)
      REAL*8         VINTM(NVIMEC+NVITH),VINTP(NVIMEC+NVITH)
      REAL*8         DSDE(DIMCON,DIMDEF)
      REAL*8         EPSV,DEPSV,P1,DP1,P2,DP2,T,DT
      REAL*8         PHI,PVP,H11,H12,H21,RHO11
      REAL*8         PHI0,PVP0,P10,P20,T0   
      REAL*8         BMPH,UMS,SAT2,PHIDS
      INTEGER RETCOM

C
C  VARIABLES LOCALES
C
C
      REAL*8         SATM,EPSVM,PHIM,RHO11M,RHO12M,RHO21M,PVPM
      REAL*8         BIDON
      INTEGER ELA,CJS,CAMCLA,LAIGLE
      LOGICAL PORELA
C       
      INTEGER  I
      REAL*8   RHO110,DPVP,DPVPL,DPVPT
C      REAL*8   RHO120,RHO210
      REAL*8   YOUNG,NU,BIOT,K0,CS,ALPHA0,ALPLIQ,CLIQ
      REAL*8   CP11,CP12,CP21,SAT,DSATP1,N,MAMOLV,MAMOLG
      REAL*8   R,RHO0,C0EPS,CSIGM,VARIA,ALP11,ALP12,ALP21      
C
C     UMPRHS = RHOS*(1-PHI)
C
      REAL*8   UMPRHS
      REAL*8   RHO12,RHO21,MASRT
C
      CHARACTER*8     NOMAIL
           INTEGER         IADZI,IAZK24
      REAL*8          VARBIO,VARLQ,VARVP
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER  ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C
        INTEGER   UMESS,IUNIFI
      REAL*8 EXPMAX
      PARAMETER(EXPMAX = 5.D0)
C
C  DECLARATIONS PERMETTANT DE RECUPERER LES CONSTANTES MATERIAU  
C 
      INTEGER     NELAS,NLIQ,NGAZ,NVAP,NHOM,NRESMA,NSAT
      PARAMETER   (NELAS=4,NLIQ=5,NGAZ=3,NVAP=3,NHOM=4,NSAT=2)
C   NRESMA EST LE MAX DE NELAS,NLIQ,NGAZ,NVAP,NHOM
      PARAMETER   (NRESMA = 18)
      REAL*8      ELAS(NELAS),LIQ(NLIQ),GAZ(NGAZ),VAP(NVAP),HOM(NHOM)
      REAL*8      SATUR(NSAT)
      CHARACTER*8 NCRA1(NELAS),NCRA2(NLIQ),NCRA3(NGAZ),NCRA4(NVAP)
      CHARACTER*8 NCRA5(NHOM),NCRA6(NSAT)
      CHARACTER*2 CODRET(NRESMA)
CCCCC
      DATA NCRA1 / 'E','NU','RHO','ALPHA' /
      DATA NCRA2 / 'RHO','UN_SUR_K','ALPHA','CP','VISC' /
      DATA NCRA3 / 'MASS_MOL','CP','VISC' /
      DATA NCRA4 / 'MASS_MOL','CP','VISC' /
      DATA NCRA5 / 'R_GAZ','RHO','CP','BIOT_COE' / 
      DATA NCRA6 / 'SATU_PRE','D_SATU_P' /
C
      DATA ELA    /1/
      DATA CJS    /2/ 
      DATA CAMCLA /7/
      DATA LAIGLE /8/
C
C     POUR EVITER DES PB AVEC OPTIMISEUR
C     ON MET UNE VALEUR DANS  CES VARIABES 
C     POUR QU ELLES AIENT UNE VALEUR MEME
C     DANS LES CAS OU ELLES NE SONT THEOTIQUEMENT PAS
C     UTILISEES 
      DPVP=0.D0
      DPVPL=0.D0
      DPVPT=0.D0 
      CSIGM = 0.D0
      RHO0 = 0.D0
      MAMOLV = 0.D0
      CP12 = 0.D0
C
C
      RETCOM = 0
      UMESS = IUNIFI('MESSAGE')
      CALL TECAEL ( IADZI, IAZK24 )
      NOMAIL = ZK24(IAZK24-1+3)(1:8) 
C
C INITIALISATION VARIABLE PORELA
C 
C
      IF (YAMEC.EQ.1) THEN
       PORELA = (INMECA.EQ.ELA)    .OR.
     +          (INMECA.EQ.CJS)    .OR.
     +          (INMECA.EQ.CAMCLA) .OR.
     +          (INMECA.EQ.LAIGLE)
      ELSE
        PORELA = .FALSE.
      ENDIF
C        
C   CALCUL DES COEFFICIENTS
C       
      CALL RCVALA(IMATE,'THM_DIFFU',1,'PCAP',P1,NHOM,
     &            NCRA5,HOM,CODRET,'FM')
      R = HOM(1)
      IF (YATE.EQ.1) THEN
       RHO0=HOM(2)
       CSIGM=HOM(3)
      ENDIF
C 
C
C   COEFFICIENTS MECANIQUES
C
      IF (YAMEC.EQ.1) THEN
        IF (PORELA) THEN
           CALL RCVALA(IMATE,'ELAS',0,' ',0.D0,NELAS,
     &                 NCRA1,ELAS,CODRET,'FM')
           YOUNG=ELAS(1)
           NU=ELAS(2)
C           
C           RHOS=ELAS(3)
C
           ALPHA0=ELAS(4)
           K0=YOUNG/3.D0/(1.D0-2.D0*NU)
           BIOT=HOM(4)
           CS=(1.D0-BIOT)/K0
        ENDIF
      ELSE
C
C  EN ABSENCE DE MECA ALPHA0 = 0 et 1/KS = 0
C
        ALPHA0 = 0.D0
        CS     = 0.D0
        BIOT   = PHI0
        PHI    = PHI0
        PHIM   = PHI0
        EPSV   = 0.D0
        DEPSV  = 0.D0
      ENDIF
C
C CALCUL EPSV AU TEMPS MOINS
C
C
      EPSVM=EPSV-DEPSV
C 
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
         SAT = 1.D0
         DSATP1 = 0.D0
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
C         RHO210=MAMOLG*P20/R/T0
C
C LIQUIDE GAZ 1 = GAZ + VAPEUR : ON RAJOUTE DONC LA VAPEUR
C  RHO21 A UNE NOUVELLE EXPRESSION
C
         IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
            MAMOLV=VAP(1)
            CP12=VAP(2)
C            RHO120=MAMOLV*PVP0/R/T0
C            RHO210=MAMOLG*(P20-PVP0)/R/T0
         ENDIF
         IF (HYDR.EQ.'HYDR_UTIL') THEN     
             CALL RCVALA(IMATE,'THM_DIFFU',1,'PCAP',P1,NSAT,
     &            NCRA6,SATUR,CODRET,'FM')
             SAT=SATUR(1)
             DSATP1=SATUR(2)
         ELSE
             CALL SATURA(HYDR,P1,SAT,DSATP1)
         ENDIF 
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     >       (OPTION(1:9).EQ.'FULL_MECA')) THEN
          IF ( THMC.EQ.'LIQU_VAPE_GAZ')THEN
            VINTP(ADVITH+3)=SAT
          ELSEIF ( (THMC.EQ.'LIQU_GAZ').OR.
     >             (THMC.EQ.'LIQU_GAZ_ATM')) THEN
            VINTP(ADVITH+2)=SAT
          ENDIF
         ENDIF
         IF (HYDR.EQ.'HYDR_UTIL') THEN     
             CALL RCVALA(IMATE,'THM_DIFFU',1,'PCAP',P1-DP1,NSAT,
     &            NCRA6,SATUR,CODRET,'FM')
             SATM=SATUR(1)
         ELSE
             CALL SATURA(HYDR,P1-DP1,SATM,BIDON)
         ENDIF
      ENDIF
      IF (THMC.EQ.'LIQU_VAPE') THEN
         CALL RCVALA(IMATE,'THM_LIQU',1,'TEMP',T,NLIQ,NCRA2,
     &               LIQ,CODRET,'FM')
         RHO110=LIQ(1)
         CLIQ=LIQ(2)
         ALPLIQ=LIQ(3)
         CP11=LIQ(4)
         CALL RCVALA(IMATE,'THM_VAPE_GAZ',1,'TEMP',T,NVAP,NCRA4,
     &               VAP,CODRET,'FM')
         RHO110=LIQ(1)
         CLIQ=LIQ(2)
         ALPLIQ=LIQ(3)
         MAMOLV=VAP(1)
         CP12=VAP(2)
C         RHO120=MAMOLV*PVP0/R/T0
         PVPM=VINTM(ADVITH+2)+PVP0
C
C ON CALCULE SATM POUR UTILISATION DANS LES ESTIMATIONS 
C COEFS D EXPONENTIELLES 
C
         IF (HYDR.EQ.'HYDR_UTIL') THEN     
             CALL RCVALA(IMATE,'THM_DIFFU',1,'PCAP',PVPM-P1+DP1,NSAT,
     &            NCRA6,SATUR,CODRET,'FM')
             SATM=SATUR(1)
         ELSE
             CALL SATURA(HYDR,PVPM-P1+DP1,SATM,BIDON)
         ENDIF
      ENDIF
C
C   CALCUL DES ARGUMENTS D EXP  
C
         IF ( T.LE.0.D0) THEN
                  WRITE(UMESS,9001)'CALCCO',' T <0  '
     &            ,'A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
         ENDIF
      IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &   (OPTION(1:9).EQ.'FULL_MECA')) THEN
C
         VARVP = 0.D0
         IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN  
C
C ON FAIT ICI UNE APPROXIMATON EN REMPLACANT RHO11 PAR RHO110
C
            VARVP=MAMOLV/R/T*(DP2-DP1)/RHO110
            IF (YATE.EQ.1)THEN 
               VARVP=VARVP+(CONGEM(ADCP12+NDIM+1)-CONGEM(ADCP11+NDIM+1))
     &               *(1.D0/(T-DT)-1.D0/T)*MAMOLV/R
               VARVP=VARVP+(CP12-CP11)*(LOG(T/(T-DT))-(DT/T))
     &               *MAMOLV/R
            ENDIF
         ENDIF
         IF (THMC.EQ.'LIQU_VAPE') THEN  
C
C ON FAIT ICI UNE APPROXIMATON EN REMPLACANT RHO11 PAR RHO110
C
            VARVP=MAMOLV/R/T*DP1/RHO110
            IF (YATE.EQ.1)THEN 
               VARVP=VARVP+(CONGEM(ADCP12+NDIM+1)-CONGEM(ADCP11+NDIM+1))
     &               *(1.D0/(T-DT)-1.D0/T)*MAMOLV/R
               VARVP=VARVP+(CP12-CP11)*(LOG(T/(T-DT))-(DT/T))
     &               *MAMOLV/R
            ENDIF
         ENDIF
         IF (YAMEC.EQ.1) THEN
          IF (INMECA.EQ.ELA   .OR.
     +        INMECA.EQ.CJS   .OR.
     +        INMECA.EQ.CAMCLA.OR.
     +        INMECA.EQ.LAIGLE    ) THEN
            VARBIO=DEPSV
            IF (YATE.EQ.1) THEN
                VARBIO=VARBIO-3.D0*ALPHA0*DT 
            ENDIF         
             IF ((THMC.EQ.'LIQU_SATU').OR.(THMC.EQ.'GAZ')) THEN
               VARBIO=VARBIO+DP1*CS
             ELSEIF (THMC.EQ.'LIQU_GAZ_ATM') THEN
               VARBIO=VARBIO+CS*(-SAT*DP1)
             ELSEIF((THMC.EQ.'LIQU_VAPE_GAZ').OR.
     >             (THMC.EQ.'LIQU_GAZ'))THEN
               VARBIO=VARBIO+CS*(DP2-SAT*DP1)
             ELSEIF(THMC.EQ.'LIQU_VAPE') THEN
C
C  ESTIMATION DPVP POUR DPC
C
               DPVP = PVPM*(EXP(VARVP)-1.D0)
               VARBIO=VARBIO-CS*SATM*(DPVP-DP1)
             ENDIF
            ENDIF
          ENDIF
               
         VARLQ = 0.D0
         IF (YATE.EQ.1) THEN 
               VARLQ=-3.D0*ALPLIQ*DT
         ELSE
               VARLQ = 0.D0
         ENDIF
         IF ((THMC.EQ.'LIQU_GAZ_ATM')) THEN
            VARLQ=VARLQ-DP1*CLIQ
         ELSEIF ((THMC.EQ.'LIQU_VAPE_GAZ').OR.(THMC.EQ.'LIQU_GAZ')) THEN
            VARLQ=VARLQ+(DP2-DP1)*CLIQ
         ELSEIF (THMC.EQ.'LIQU_VAPE') THEN
            VARLQ=VARLQ+DP1*CLIQ
         ENDIF
C
C TESTS SUR LES ESTIMATIONS DE COEF D EXPONENTIELLES
C
         IF ( YAMEC.EQ.1) THEN
          IF ( -VARBIO.GT.EXPMAX) THEN
                  WRITE(UMESS,9001)'CALCCO','-VARBIO > EXPMAX  '
     &            ,'A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
          ENDIF
         ENDIF
         IF ( VARLQ.NE.0.D0) THEN
          IF ( VARLQ.GT.EXPMAX) THEN
                  WRITE(UMESS,9001)'CALCCO','VARLQ > EXPMAX  '
     &            ,'A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
          ENDIF
          IF ( VARLQ.LT.-EXPMAX) THEN
                  WRITE(UMESS,9001)'CALCCO','VARLQ <- EXPMAX  '
     &            ,'A LA MAILLE: ', NOMAIL
             RETCOM=1 
            GOTO 9000
          ENDIF
         ENDIF
         IF ( VARVP.GT.EXPMAX) THEN
                  WRITE(UMESS,9001)'CALCCO','VARVP > EXPMAX  '
     &            ,'A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
         ENDIF
         IF ( VARVP.LT.-EXPMAX) THEN
                  WRITE(UMESS,9001)'CALCCO','VARVP <- EXPMAX  '
     &            ,'A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
         ENDIF
        ENDIF             
C
C EN LIQU_VAPE CALCUL DE RHO11, DES ENTHALPIES DE PVP  ET RHOVP
C 
      IF (THMC.EQ.'LIQU_VAPE') THEN
        IF (OPTION(1:16).EQ.'RIGI_MECA_TANG') THEN
          RHO11=VINTM(ADVITH+1)+RHO110
          RHO11M=VINTM(ADVITH+1)+RHO110
        ELSE
            VARIA=DP1*CLIQ
            IF (YATE.EQ.1) THEN 
               VARIA=VARIA-3.D0*ALPLIQ*DT
            ENDIF
                  VINTP(ADVITH+1)=
     >            -RHO110+(VINTM(ADVITH+1)+RHO110)*EXP(VARIA)
            RHO11=VINTP(ADVITH+1)+RHO110
            RHO11M=VINTM(ADVITH+1)+RHO110
        ENDIF
C   
C **********************************************************************
C CALCUL ENTHALPIES ET DERIVEES DES ENTHALPIES
C
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            DSDE(ADCP11+NDIM+1,ADDEP1)=DSDE(ADCP11+NDIM+1,ADDEP1)
     &                              +(1.D0-3.D0*ALPLIQ*T)/RHO11
            DSDE(ADCP11+NDIM+1,ADDETE)=DSDE(ADCP11+NDIM+1,ADDETE)+CP11
              DSDE(ADCP12+NDIM+1,ADDETE)=DSDE(ADCP12+NDIM+1,ADDETE)+CP12
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCP11+NDIM+1)=CONGEP(ADCP11+NDIM+1)
     &                            +(1.D0-3.D0*ALPLIQ*T)/RHO11*DP1
     &                            +CP11*DT
               CONGEP(ADCP12+NDIM+1)=CONGEP(ADCP12+NDIM+1)+CP12*DT
         ENDIF
C 
      IF (YATE.EQ.1) THEN
         IF (OPTION.EQ.'RIGI_MECA_TANG') THEN
           H11=CONGEM(ADCP11+NDIM+1)
           H12=CONGEM(ADCP12+NDIM+1)
         ELSE      
           H11=CONGEP(ADCP11+NDIM+1)
           H12=CONGEP(ADCP12+NDIM+1)
         ENDIF
      ENDIF
C      
C **********************************************************************
C  CALCUL DE LA VARIABLE INTERNE PRESSION VAPEUR
C    ET DES AUTRES MASSES VOLUMIQUES AVEC 2 FLUIDES, MAIS PAS
C    C0EPS QUI NECESSITE SAT ET PHI
C
         IF (OPTION(1:16).EQ.'RIGI_MECA_TANG') THEN 
            PVP=VINTM(ADVITH+2)+PVP0
            PVPM=VINTM(ADVITH+2)+PVP0
         ELSE
            VARIA=MAMOLV/R/T*DP1/RHO11
            IF (YATE.EQ.1)THEN 
               VARIA=VARIA+(CONGEM(ADCP12+NDIM+1)-CONGEM(ADCP11+NDIM+1))
     &               *(1.D0/(T-DT)-1.D0/T)*MAMOLV/R
               VARIA=VARIA+(CP12-CP11)*(LOG(T/(T-DT))-(DT/T))
     &               *MAMOLV/R
            ENDIF
            VINTP(ADVITH+2)=-PVP0+(VINTM(ADVITH+2)+PVP0)*EXP(VARIA)
            PVP =VINTP(ADVITH+2)+PVP0
            PVPM=VINTM(ADVITH+2)+PVP0 
            DPVP = PVP-PVPM
         ENDIF
         RHO12=MAMOLV*PVP/R/T
         RHO12M=MAMOLV*PVPM/R/(T-DT)
C
C ON PEUT MAINTENANT CALCULER SAT DANS LE CAS LIQU_VAPE
C
         IF (HYDR.EQ.'HYDR_UTIL') THEN     
             CALL RCVALA(IMATE,'THM_DIFFU',1,'PCAP',PVP-P1,NSAT,
     &            NCRA6,SATUR,CODRET,'FM')
             SAT=SATUR(1)
             DSATP1=SATUR(2)
         ELSE
             CALL SATURA(HYDR,PVP-P1,SAT,DSATP1)
         ENDIF 
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     >        (OPTION(1:9).EQ.'FULL_MECA')) THEN
             VINTP(ADVITH+3)=SAT
         ENDIF
         IF (HYDR.EQ.'HYDR_UTIL') THEN     
             CALL RCVALA(IMATE,'THM_DIFFU',1,'PCAP',PVPM-P1+DP1,NSAT,
     &            NCRA6,SATUR,CODRET,'FM')
             SATM=SATUR(1)
         ELSE
             CALL SATURA(HYDR,PVPM-P1+DP1,SATM,BIDON)
         ENDIF
      ENDIF            
C
C **********************************************************************
C CALCUL DE LA VARIABLE INTERNE PHI A L'INSTANT PLUS
C
      IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &   (OPTION(1:9).EQ.'FULL_MECA')) THEN
         IF (YAMEC.EQ.1) THEN
          IF (INMECA.EQ.ELA   .OR.
     +        INMECA.EQ.CJS   .OR.
     +        INMECA.EQ.CAMCLA.OR.
     +        INMECA.EQ.LAIGLE    ) THEN
            VARIA=DEPSV
            IF (YATE.EQ.1) THEN
                VARIA=VARIA-3.D0*ALPHA0*DT 
            ENDIF         
            IF ((THMC.EQ.'LIQU_SATU').OR.(THMC.EQ.'GAZ')) THEN
               VARIA=VARIA+DP1*CS
                 VINTP(ADVITH)=
     &             BIOT-PHI0-(BIOT-VINTM(ADVITH)-PHI0)*EXP(-VARIA)
            ENDIF
            IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
               VARIA=VARIA+CS*(-SAT*DP1)
                 VINTP(ADVITH)=
     &             BIOT-PHI0-(BIOT-VINTM(ADVITH)-PHI0)*EXP(-VARIA)
            ENDIF
            IF((THMC.EQ.'LIQU_VAPE_GAZ').OR.(THMC.EQ.'LIQU_GAZ'))THEN
               VARIA=VARIA+CS*(DP2-SAT*DP1)
                 VINTP(ADVITH)=
     &             BIOT-PHI0-(BIOT-VINTM(ADVITH)-PHI0)*EXP(-VARIA)
            ENDIF
            IF(THMC.EQ.'LIQU_VAPE')THEN
               VARIA=VARIA-CS*SAT*(DPVP-DP1)
                 VINTP(ADVITH)=
     &             BIOT-PHI0-(BIOT-VINTM(ADVITH)-PHI0)*EXP(-VARIA)
            ENDIF
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
         IF (THMC.EQ.'LIQU_SATU') THEN
            VARIA=DP1*CLIQ
            IF (YATE.EQ.1) THEN 
               VARIA=VARIA-3.D0*ALPLIQ*DT
            ENDIF
                  VINTP(ADVITH+1)=
     >            -RHO110+(VINTM(ADVITH+1)+RHO110)*EXP(VARIA)
         ENDIF
         IF ((THMC.EQ.'LIQU_GAZ_ATM')) THEN
            VARIA=-DP1*CLIQ
            IF (YATE.EQ.1) THEN 
               VARIA=VARIA-3.D0*ALPLIQ*DT
            ENDIF
            VINTP(ADVITH+1)=
     >       -RHO110+(VINTM(ADVITH+1)+RHO110)*EXP(VARIA)
         ENDIF
         IF ((THMC.EQ.'LIQU_VAPE_GAZ').OR.(THMC.EQ.'LIQU_GAZ')) THEN
            VARIA=(DP2-DP1)*CLIQ
            IF (YATE.EQ.1) THEN 
               VARIA=VARIA-3.D0*ALPLIQ*DT
            ENDIF
            VINTP(ADVITH+1)=
     >        -RHO110+(VINTM(ADVITH+1)+RHO110)*EXP(VARIA)
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
     >       (THMC.EQ.'LIQU_VAPE').OR.
     >       (THMC.EQ.'LIQU_GAZ_ATM').OR.
     >       (THMC.EQ.'LIQU_VAPE_GAZ').OR.
     >       (THMC.EQ.'LIQU_GAZ')) THEN
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
     >       (THMC.EQ.'LIQU_VAPE').OR.
     >       (THMC.EQ.'LIQU_GAZ_ATM').OR.
     >       (THMC.EQ.'LIQU_VAPE_GAZ').OR.
     >       (THMC.EQ.'LIQU_GAZ')) THEN
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
C
C     UMPRHS = RHOS*(1-PHI)
C
            UMPRHS=(RHO0-RHO11*PHI)
            IF ( UMPRHS.LE.0.D0) THEN
                  WRITE(UMESS,9001)'CALCCO',' RHOS(1-PHI) <=0 '
     >       ,'  A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
            ENDIF
            C0EPS=UMPRHS*CSIGM+PHI*RHO11*CP11
            IF ( YAMEC.EQ.1) THEN
              C0EPS=C0EPS -9.D0*T*K0*ALPHA0*ALPHA0
            ENDIF
            IF ( C0EPS.LE.0.D0) THEN
                  WRITE(UMESS,9001)'CALCCO','C0EPS <=0  '
     &            ,'A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
            ENDIF
            IF (YAMEC.EQ.1) THEN
               ALP11=ALP11+ALPHA0*(BIOT-PHI)
            ENDIF
         ENDIF          
         IF (THMC.EQ.'GAZ') THEN
            ALP11=PHI/3.D0/T   
C
C     UMPRHS = RHOS*(1-PHI)
C
            UMPRHS=(RHO0-RHO11*PHI)
            IF ( UMPRHS.LE.0.D0) THEN
                  WRITE(UMESS,9001)'CALCCO',' RHOS(1-PHI) <=0 ',
     >                        '  A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
            ENDIF
            C0EPS=UMPRHS*CSIGM+PHI*RHO11*CP11
            IF ( YAMEC.EQ.1) THEN
              C0EPS=C0EPS -9.D0*T*K0*ALPHA0*ALPHA0
            ENDIF
            IF ( C0EPS.LE.0.D0) THEN
                  WRITE(UMESS,9001)'CALCCO','C0EPS <=0  '
     &            ,'A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
            ENDIF
            IF (YAMEC.EQ.1) THEN
               ALP11=ALP11+ALPHA0*(BIOT-PHI)   
            ENDIF  
         ENDIF
         IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
            ALP11=ALPLIQ*PHI    
C
C     UMPRHS = RHOS*(1-PHI)
C
            UMPRHS=(RHO0-RHO11*SAT*PHI)
            IF ( UMPRHS.LE.0.D0) THEN
                  WRITE(UMESS,9001)'CALCCO',' RHOS(1-PHI) <=0  '
     >       ,' A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
            ENDIF
            IF (YAMEC.EQ.1) THEN
               ALP11=ALP11+ALPHA0*(BIOT-PHI)
            ENDIF
            C0EPS=UMPRHS*CSIGM+PHI*SAT*RHO11*CP11
            IF ( YAMEC.EQ.1) THEN
              C0EPS=C0EPS -9.D0*T*K0*ALPHA0*ALPHA0
            ENDIF
            IF ( C0EPS.LE.0.D0) THEN
                  WRITE(UMESS,9001)'CALCCO','C0EPS <=0  '
     &            ,'A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
            ENDIF
         ENDIF            
         IF ((THMC.EQ.'LIQU_VAPE_GAZ').OR.
     >       (THMC.EQ.'LIQU_GAZ').OR.
     >       (THMC.EQ.'LIQU_VAPE')) THEN
            IF (YAMEC.NE.1) ALPHA0=0.D0
            ALP11=SAT*(BIOT-PHI)*ALPHA0+ALPLIQ*PHI*SAT
            ALP12=(BIOT-PHI)*(1.D0-SAT)*ALPHA0+PHI*
     &                 (1.D0-SAT)/3.D0/T
            ALP21=ALP12
         ENDIF 
      ENDIF
      IF (THMC.EQ.'LIQU_GAZ') THEN
         RHO21=MAMOLG*P2/R/T 
         RHO21M=MAMOLG*(P2-DP2)/R/(T-DT)
         IF (YATE.EQ.1) THEN  
C
C     UMPRHS = RHOS*(1-PHI)
C
            UMPRHS=(RHO0-RHO11*SAT*PHI-RHO21*(1.D0-SAT)*PHI)
            IF ( UMPRHS.LE.0.D0) THEN
                  WRITE(UMESS,9001)'CALCCO',' RHOS(1-PHI) <=0  '
     >       ,'A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
            ENDIF
            C0EPS=UMPRHS*CSIGM+PHI*SAT*RHO11*CP11
     >            +PHI*(1.D0-SAT)*(RHO21*CP21)
            IF ( YAMEC.EQ.1) THEN
              C0EPS=C0EPS -9.D0*T*K0*ALPHA0*ALPHA0
            ENDIF
            IF ( C0EPS.LE.0.D0) THEN
                  WRITE(UMESS,9001)'CALCCO','C0EPS <=0  '
     &            ,'A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
            ENDIF
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
      IF ((THMC.EQ.'LIQU_VAPE_GAZ').AND.(YATE.EQ.1)) THEN
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
C  CALCUL DE LA VARIABLE INTERNE PRESSION VAPEUR
C    ET DES AUTRES MASSES VOLUMIQUES AVEC 2 FLUIDES, AINSI
C    QUE C0EPS
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
         MASRT=MAMOLG/R/T
         IF ( (P2-PVP).LT.0.D0) THEN
                  WRITE(UMESS,9001)'CALCCO','PAIR <=0  '
     &            ,'A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
         ENDIF
         RHO12M=MAMOLV*PVPM/R/(T-DT)
         RHO21M=MAMOLG*(P2-DP2-PVPM)/R/(T-DT)
         IF (YATE.EQ.1) THEN
C
C     UMPRHS = RHOS*(1-PHI)
C
            UMPRHS=RHO0-RHO11*SAT*PHI
     >           -(RHO12+RHO21)*(1.D0-SAT)*PHI
            IF ( UMPRHS.LE.0.D0) THEN
                  WRITE(UMESS,9001)'CALCCO',
     >       ' RHOS(1-PHI) <=0  '
     >        ,' A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
            ENDIF
            C0EPS=UMPRHS*CSIGM+PHI*SAT*RHO11*CP11
     >            +PHI*(1.D0-SAT)*(RHO12*CP12+RHO21*CP21)
            IF ( YAMEC.EQ.1) THEN
              C0EPS=C0EPS -9.D0*T*K0*ALPHA0*ALPHA0
            ENDIF
            IF ( C0EPS.LE.0.D0) THEN
                   WRITE(UMESS,9001)'CALCCO','C0EPS <=0  '
     &            ,'A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
            ENDIF
         ENDIF
      ENDIF       
         IF ((YATE.EQ.1).AND.(THMC.EQ.'LIQU_VAPE')) THEN
C
C     UMPRHS = RHOS*(1-PHI)
C
            UMPRHS=RHO0-RHO11*SAT*PHI-RHO12*(1.D0-SAT)*PHI
            IF ( UMPRHS.LE.0.D0) THEN
                  WRITE(UMESS,9001)'CALCCO',' RHOS(1-PHI) <=0  '
     &            ,'A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
            ENDIF
            C0EPS=UMPRHS*CSIGM+PHI*SAT*RHO11*CP11
     >            +PHI*(1.D0-SAT)*RHO12*CP12
            IF ( YAMEC.EQ.1) THEN
              C0EPS=C0EPS -9.D0*T*K0*ALPHA0*ALPHA0
            ENDIF
            IF ( C0EPS.LE.0.D0) THEN
                   WRITE(UMESS,9001)'CALCCO','C0EPS <=0  '
     &            ,'A LA MAILLE: ', NOMAIL
             RETCOM=1 
             GOTO 9000
            ENDIF
         ENDIF
C
C  DERIVEES DES PRESSIONS DE VAPEUR EN LIQU_VAPE
C
C  DPVPL DERIVEE PRESSION DE VAPEUR / PRESSION DE LIQUIDE
C  DPVPT DERIVEE PRESSION DE VAPEUR / TEMP
C         
      IF (THMC.EQ.'LIQU_VAPE') THEN  
       IF (OPTION(1:16).EQ.'RIGI_MECA_TANG') THEN
        DPVPL=RHO12M/RHO11M
        IF ( YATE.EQ.1) THEN
         DPVPT=RHO12M*(CONGEM(ADCP12+NDIM+1)-CONGEM(ADCP11+NDIM+1))/T
        ENDIF
       ELSE
        DPVPL=RHO12/RHO11
        IF ( YATE.EQ.1) THEN
         DPVPT=RHO12*(CONGEP(ADCP12+NDIM+1)-CONGEP(ADCP11+NDIM+1))/T
        ENDIF    
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
C
      IF (((THMC.EQ.'LIQU_SATU').OR.(THMC.EQ.'GAZ'))
     >                             .AND.(YAMEC.EQ.1)) THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
             DSDE(ADCOME+6,ADDEP1)=DSDE(ADCOME+6,ADDEP1)-BIOT
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCOME+6)=CONGEP(ADCOME+6)-BIOT*DP1
         ENDIF
      ENDIF
C
      IF (THMC.EQ.'LIQU_GAZ_ATM'.AND.(YAMEC.EQ.1)) THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
             DSDE(ADCOME+6,ADDEP1)=DSDE(ADCOME+6,ADDEP1)+BIOT*SAT
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCOME+6)=CONGEP(ADCOME+6)+BIOT*SAT*DP1
         ENDIF
      ENDIF
      IF ((THMC.EQ.'LIQU_VAPE').AND.(YAMEC.EQ.1)) THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
             DSDE(ADCOME+6,ADDEP1)=DSDE(ADCOME+6,ADDEP1)-BIOT*SAT
     >       -BIOT*(1.D0-SAT)*DPVPL
           IF ( YATE.EQ.1) THEN
             DSDE(ADCOME+6,ADDETE)=DSDE(ADCOME+6,ADDETE)
     >       -BIOT*(1.D0-SAT)*DPVPT
           ENDIF
         ENDIF
         IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN 
            CONGEP(ADCOME+6)=CONGEP(ADCOME+6)-BIOT*SAT*DP1
     >       -BIOT*(1.D0-SAT)*DPVP
         ENDIF
      ENDIF
C
C **********************************************************************
C  CALCUL DES APPORTS MASSIQUES  
C
      IF ((THMC.EQ.'LIQU_SATU').OR.(THMC.EQ.'GAZ')) THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
            IF (THMC.EQ.'LIQU_SATU') THEN
               N=(BIOT-PHI)*CS+PHI*CLIQ
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
     &         +MASRT*PHI*(1.D0-SAT)*RHO12/RHO11
            DSDE(ADCP21,ADDEP2)=DSDE(ADCP21,ADDEP2)
     &         +RHO21*((BIOT-PHI)*(1.D0-SAT)*CS)
     &         +MASRT*(PHI*(1.D0-SAT)*(RHO11-RHO12)/RHO11)
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
     &            -MASRT*RHO12*PHI*(1.D0-SAT)*(H12-H11)/T
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
      BMPH = BIOT - PHI
      UMS  = 1.D0-SAT
      SAT2 =SAT*SAT 
      PHIDS=PHI*DSATP1
       
C   
      IF (THMC.EQ.'LIQU_VAPE') THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN          
          DSDE(ADCP11,ADDEP1)=DSDE(ADCP11,ADDEP1)
     >    +RHO11*(PHIDS-SAT2*BMPH*CS+SAT*BMPH*CS)*DPVPL
     >    -RHO11*(PHIDS-SAT*PHI*CLIQ-SAT2*BMPH*CS)
            DSDE(ADCP12,ADDEP1)=DSDE(ADCP12,ADDEP1)
     &      +RHO12*(-PHIDS-UMS*SAT*BMPH*CS+BMPH*UMS*CS)*DPVPL
     &      -RHO12*(-PHIDS-UMS*SAT*BMPH*CS-PHI*UMS*RHO12/RHO11/PVP)
            IF (YAMEC.EQ.1) THEN
               DO 111 I=1,3
                  DSDE(ADCP11,ADDEME+NDIM-1+I)=
     &               DSDE(ADCP11,ADDEME+NDIM-1+I)+RHO11*BIOT*SAT
                  DSDE(ADCP12,ADDEME+NDIM-1+I)=
     &               DSDE(ADCP12,ADDEME+NDIM-1+I)+RHO12*BIOT*UMS
     
 111            CONTINUE
            ENDIF
            IF (YATE.EQ.1) THEN
               DSDE(ADCP11,ADDETE)=DSDE(ADCP11,ADDETE)
     &            -3.D0*RHO11*ALP11
     >    +RHO11*(PHIDS-SAT2*BMPH*CS+SAT*BMPH*CS)*DPVPT
               DSDE(ADCP12,ADDETE)=DSDE(ADCP12,ADDETE)
     &            +RHO12*RHO12*PHI*UMS/PVP*(H12-H11)/T
     &            -3.D0*RHO12*ALP12
     &      +RHO12*(-PHIDS-UMS*SAT*BMPH*CS+BMPH*UMS*CS)*DPVPT
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
         ENDIF
      ENDIF
     
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
             IF (INMECA.EQ.ELA   .OR.
     +           INMECA.EQ.CJS   .OR.
     +           INMECA.EQ.CAMCLA.OR.
     +           INMECA.EQ.LAIGLE    ) THEN
               DO 102 I=1,3
                  DSDE(ADCOTE,ADDEME+NDIM-1+I)=
     &                   DSDE(ADCOTE,ADDEME+NDIM-1+I)
     &                   +ALPHA0*YOUNG/(1.D0-2.D0*NU)*T
 102           CONTINUE
             ENDIF
            ENDIF
            IF ((THMC.EQ.'LIQU_VAPE_GAZ').OR.
     &          (THMC.EQ.'LIQU_GAZ')) THEN
               DSDE(ADCOTE,ADDEP1)=DSDE(ADCOTE,ADDEP1)+3.D0*ALP11*T
               DSDE(ADCOTE,ADDEP2)=DSDE(ADCOTE,ADDEP2)
     &                             -3.D0*(ALP11+ALP12)*T
            ENDIF
            IF (THMC.EQ.'LIQU_VAPE') THEN
               DSDE(ADCOTE,ADDEP1)=DSDE(ADCOTE,ADDEP1)
     >          -3.D0*ALP11*T-3.D0*ALP12*T*DPVPL
               DSDE(ADCOTE,ADDETE)=DSDE(ADCOTE,ADDETE)
     >          -3.D0*ALP12*T*DPVPT
            ENDIF
            IF ((THMC.EQ.'LIQU_SATU').OR.(THMC.EQ.'GAZ')) THEN
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
             IF (INMECA.EQ.ELA   .OR.
     +           INMECA.EQ.CJS   .OR.
     +           INMECA.EQ.CAMCLA.OR.
     +           INMECA.EQ.LAIGLE    ) THEN
               CONGEP(ADCOTE)=CONGEP(ADCOTE)
     &              +ALPHA0*YOUNG/(1.D0-2.D0*NU)*(T-DT/2)*DEPSV
             ENDIF 
            ENDIF
            IF ((THMC.EQ.'LIQU_SATU').OR.(THMC.EQ.'GAZ')) THEN
               CONGEP(ADCOTE)=CONGEP(ADCOTE)
     &                   -3.D0*ALP11*(T-DT/2)*DP1
            ENDIF
            IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
               CONGEP(ADCOTE)=CONGEP(ADCOTE)
     &                     +3.D0*ALP11*(T-DT/2)*DP1
            ENDIF
            IF ((THMC.EQ.'LIQU_VAPE_GAZ').OR.
     &          (THMC.EQ.'LIQU_GAZ')) THEN
            CONGEP(ADCOTE)=CONGEP(ADCOTE)+3.D0*ALP11*(T-DT/2.D0)*DP1
            CONGEP(ADCOTE)=CONGEP(ADCOTE)-3.D0*(ALP11*(T-DT/2.D0)
     &                        +ALP12*(T-DT/2.D0))*DP2
            ENDIF
            IF (THMC.EQ.'LIQU_VAPE') THEN
             CONGEP(ADCOTE)=CONGEP(ADCOTE)-3.D0*ALP11*(T-DT/2.D0)*DP1
     >                     -3.D0*ALP12*(T-DT/2.D0)*DPVP
            ENDIF   
         ENDIF
       ENDIF
 9000  CONTINUE
 9001  FORMAT(A10,2X,A20,2X,A20,2X,A8)
C
      END
