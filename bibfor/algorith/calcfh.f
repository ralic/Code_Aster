        SUBROUTINE CALCFH(OPTION,MECA,THMC,THER,HYDR,IMATE,NDIM,DIMDEF,
     +                    DIMCON,NVIMEC,NVITH,YAMEC,YATE,ADDEP1,ADDEP2,
     +                    ADCP11,ADCP12,ADCP21,ADCP22,ADDEME,ADDETE,
     +                    VINTM, VINTP,
     +                    CONGEM,CONGEP,ADVIME,ADVITH,DSDE,P1,P2,
     +                    GRAP1,GRAP2,T,GRAT,PHI,PVP,RHO11,H11,H12,H21,
     +                    R, RHOD, CPD, BIOT, SAT, DSATP1,
     +                    PESA, PERMFH, PERMLI, DPERML, PERMGZ, DPERMS,
     +                    DPERMP, FICK, DFICKT, DFICKG, LAMBDD, DLAMBD,
     +                    RHOL, CLIQ, ALPLIQ, CPL, LAMBDL, DLAMBL,
     +                    VISCL, DVISCL, MAMOLG, CPG, LAMBDG, DLAMBG,
     +                    VISCG, DVISCG, MAMOLV, CPVG, VISCVG, DVISVG,
     +                    RETCOM)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 06/10/2003   AUTEUR ROMEO R.FERNANDES 
C RESPONSABLE UFBHHLL C.CHAVANT
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
C ======================================================================
C ROUTINE CALC_FLUX_HYDRO
C CALCULE LES CONTRAINTES GENERALISEES ET LA MATRICE TANGENTE DES FLUX
C HYDRAULIQUES AU POINT DE GAUSS CONSIDERE
C ======================================================================
C IN CORRESPONDANCE ANCIENNE PROGRAMMATION -----------------------------
C COND(1) -> PERMFH : PERM_IN OU PERM_END SOUS THM_DIFFU ---------------
C COND(2) -> PERMLI : PERM_LIQU           SOUS THM_DIFFU ---------------
C COND(3) -> DPERML : D_PERM_LIQU         SOUS THM_DIFFU ---------------
C COND(4) -> PERMGZ : PERM_GAZ            SOUS THM_DIFFU ---------------
C COND(5) -> DPERMS : D_PERM_SATU_GAZ     SOUS THM_DIFFU ---------------
C COND(6) -> DPERMP : D_PERM_PRES_GAZ     SOUS THM_DIFFU ---------------
C COND(7) -> FICK   : FICK                SOUS THM_DIFFU ---------------
C COND(8) -> DFICKT : D_FICK_TEMP         SOUS THM_DIFFU ---------------
C COND(9) -> DFICKG : D_FICK_GAZ_PRES     SOUS THM_DIFFU ---------------
C ======================================================================
C OUT RETCOM : RETOUR LOI DE COMPORTEMENT
C ======================================================================
      IMPLICIT      NONE
      INTEGER       NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,IMATE,YAMEC,YATE
      INTEGER       ADDEME,ADDEP1,ADDEP2,ADDETE,ADCP11,ADCP12,ADCP21
      INTEGER       ADCP22,ADVIME,ADVITH,RETCOM
      REAL*8        CONGEM(1:DIMCON),CONGEP(1:DIMCON)
      REAL*8        VINTM(1:NVIMEC+NVITH),VINTP(1:NVIMEC+NVITH)
      REAL*8        DSDE(1:DIMCON,1:DIMDEF),P1,GRAP1(3),P2,MAMOLV
      REAL*8        GRAP2(3),T,GRAT(3),PHI,PVP,H11,H12,H21,RHO11
      REAL*8        R, RHOD, CPD, BIOT, SAT, DSATP1, PESA(3), PERMFH
      REAL*8        PERMLI, DPERML, PERMGZ, DPERMS, DPERMP, FICK
      REAL*8        DFICKT, DFICKG, LAMBDD, DLAMBD, RHOL, CLIQ, ALPLIQ
      REAL*8        CPL, LAMBDL, DLAMBL, VISCL, DVISCL, CPG, LAMBDG
      REAL*8        DLAMBG, VISCG, DVISCG, MAMOLG, CPVG, VISCVG, DVISVG
      CHARACTER*16  OPTION,MECA,THMC,THER,HYDR
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER       I,J
      REAL*8        LAMBD1(5),LAMBD2(5),F(5),KREL,DKREL
      REAL*8        RHO12,RHO21,MASRT,ENDOM,VISCO,DVISCO
C ======================================================================
C --- VARIABLES LOCALES PERMETTANT D'EXPRIMER LES DERIVEES DES ---------
C --- GRADIENTS DES PRESSIONS DE GAZ ET DE VAPEUR ----------------------
C ======================================================================
C --- ON UTILISE LES CONVENTIONS SUIVANTES : ---------------------------
C ======================================================================
C --- D = DERIVEE --- G = GRADIENT --- P = PRESSION VAPEUR -------------
C --- C = PVP/PGZ ------------------------------------------------------
C ======================================================================
C --- CVP=PVP/PGZ , GP = GRADIENT DE PVP , GC = GRADIENT DE CVP --------
C ======================================================================
      REAL*8 CVP,GP(3),GC(3)
C ======================================================================
C --- DERIVEES DE PVP PAR RAPPORT A P1, P2, T --------------------------
C ======================================================================
      REAL*8 DPP1,DPP2,DPT
C ======================================================================
C --- DERIVEES DE CVP PAR RAPPORT A P1, P2, T --------------------------
C ======================================================================
      REAL*8 DCP1,DCP2,DCT
C ======================================================================
C --- DERIVEES DE RHO11 PAR RAPPORT A P1, P2, T ------------------------
C ======================================================================
      REAL*8 DR11P1,DR11P2,DR11T
C ======================================================================
C --- DERIVEES DE RHO12 PAR RAPPORT A P1, P2, T ------------------------
C ======================================================================
      REAL*8 DR12P1,DR12P2,DR12T
C ======================================================================
C --- DERIVEES DE RHO21 PAR RAPPORT A P1, P2, T ------------------------
C ======================================================================
      REAL*8 DR21P1,DR21P2,DR21T
C ======================================================================
C --- DERIVEES DU TERME AUXILIAIRE PAR RAPPORT A P1, P2, T -------------
C --- TERME AUX = RHO12*(H12-H11)/T ------------------------------------
C ======================================================================
      REAL*8 DAUXP1,DAUXP2,DAUXT
C ======================================================================
C --- DERIVEES DU GRADIENT DE PVP PAR RAPPORT A P1, P2, T --------------
C ======================================================================
      REAL*8 DGPP1(3),DGPP2(3),DGPT(3)
C ======================================================================
C --- DERIVEES DU GRADIENT DE CVP PAR RAPPORT A P1, P2, T --------------
C ======================================================================
      REAL*8 DGCP1(3),DGCP2(3),DGCT(3)
C ======================================================================
C --- DERIVEES DU GRADIENT DE PVP PAR RAPPORT AUX GRADIENTS DE P1,P2,T -
C ======================================================================
      REAL*8 DGPGP1,DGPGP2,DGPGT
C ======================================================================
C --- DERIVEES DU GRADIENT DE CVP PAR RAPPORT AUX GRADIENTS DE P1,P2,T -
C ======================================================================
      REAL*8 DGCGP1,DGCGP2,DGCGT
C ======================================================================
C --- QUELQUES INITIALISATIONS -----------------------------------------
C ======================================================================
      DO 1 I = 1 , 3
         DGPP1(I) = 0.D0
         DGPP2(I) = 0.D0
         DGPT(I)  = 0.D0
         DGCP1(I) = 0.D0
         DGCP2(I) = 0.D0
         DGCT(I)  = 0.D0
         GC(I)    = 0.D0
         GP(I)    = 0.D0
 1    CONTINUE
      DGPGP1 = 0.D0
      DGPGP2 = 0.D0
      DGPGT  = 0.D0
      DGCGP1 = 0.D0
      DGCGP2 = 0.D0
      DGCGT  = 0.D0
      DPP1   = 0.D0
      DPP2   = 0.D0
      DPT    = 0.D0
      DCP1   = 0.D0
      DCP2   = 0.D0
      DCT    = 0.D0
C ======================================================================
      IF (THMC.EQ.'LIQU_SATU') THEN
         KREL   = 1.D0
         DKREL  = 0.D0
         VISCO  = VISCL
         DVISCO = DVISCL
      ENDIF
C
      IF (THMC.EQ.'GAZ') THEN
         KREL   = 1.D0
         DKREL  = 0.D0
         VISCO  = VISCG
         DVISCO = DVISCG
      ENDIF
C
      IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
         VISCO  = VISCL
         DVISCO = DVISCL
         KREL   = PERMLI
         DKREL  = DPERML*DSATP1
      ENDIF
C
      IF ( (THMC.EQ.'LIQU_VAPE_GAZ') .OR.
     +     (THMC.EQ.'LIQU_GAZ')      .OR.
     +     (THMC.EQ.'LIQU_VAPE')          ) THEN
         KREL   = PERMLI
         DKREL  = DPERML*DSATP1
         VISCO  = VISCL
         DVISCO = DVISCL
C
         IF ((THMC.EQ.'LIQU_VAPE_GAZ')) THEN
            F(1) = FICK
            F(2) = 0.D0
            F(3) = 0.D0
            F(4) = DFICKG
            F(5) = DFICKT
         ELSE
            F(1) = 0.D0
            F(2) = 0.D0
            F(3) = 0.D0
            F(4) = 0.D0
            F(5) = 0.D0
         ENDIF
C ======================================================================
C --- CALCUL DE LAMBDA2 ------------------------------------------------
C ======================================================================
C --- LAMBD2(1) = CONDUC_HYDRO_GAZ -------------------------------------
C --- LAMBD2(2) = D(CONDUC_HYDRO_GAZ)/DP1 ------------------------------
C --- LAMBD2(3) = D(CONDUC_HYDRO_GAZ)/DP2 ------------------------------
C --- LAMBD2(4) = D(CONDUC_HYDRO_GAZ)/DT -------------------------------
C ======================================================================
         LAMBD2(1) =   PERMFH*PERMGZ/VISCG
         LAMBD2(2) =   0.0D0
         LAMBD2(3) =   PERMFH*DPERMS*DSATP1/VISCG
         LAMBD2(4) =   PERMFH*DPERMP/VISCG
         LAMBD2(5) = - PERMFH*PERMGZ/VISCG/VISCG*DVISCG
      ENDIF
C ======================================================================
C --- CALCUL DE LAMBDA1 ------------------------------------------------
C ======================================================================
      LAMBD1(1) =   PERMFH*KREL/VISCO
      LAMBD1(2) =   0.0D0
      LAMBD1(3) =   PERMFH*DKREL/VISCO
      LAMBD1(4) =   0.0D0
      LAMBD1(5) = - PERMFH*KREL/VISCO/VISCO*DVISCO
C ======================================================================
C  CALCUL DES MASSES VOLUMIQUES, PRESSION DE VAPEUR, GRADIENTS DE VAPEUR
C ======================================================================
      IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
         RHO12=MAMOLV*PVP/R/T
         RHO21=MAMOLG*(P2-PVP)/R/T
         MASRT=MAMOLG/R/T
         CVP=PVP/P2
         DO 100 I=1,NDIM
            GP(I)=RHO12/RHO11*(GRAP2(I)-GRAP1(I))
            IF (YATE.EQ.1) THEN
               GP(I)=GP(I)+RHO12*(H12-H11)/T*GRAT(I)
            ENDIF
            GC(I)=GP(I)/P2-PVP/P2/P2*GRAP2(I)
 100     CONTINUE
      ENDIF
      IF (THMC.EQ.'LIQU_VAPE') THEN
         RHO12=MAMOLV*PVP/R/T
         DO 110 I=1,NDIM
            GP(I)=RHO12/RHO11*GRAP1(I)
            IF (YATE.EQ.1) THEN
               GP(I)=GP(I)+RHO12*(H12-H11)/T*GRAT(I)
            ENDIF
 110     CONTINUE
      ENDIF
      IF (THMC.EQ.'LIQU_GAZ') THEN
         RHO21=MAMOLG*P2/R/T
         RHO12=0.D0
         DR12P1=0.D0
         DR12P2=0.D0
         DR12T=0.D0
         CVP=0.D0
      ENDIF
C ======================================================================
C --- CALCUL DES DERIVEES DES PRESSIONS DE VAPEUR ----------------------
C ======================================================================
      IF ( (OPTION(1:16).EQ.'RIGI_MECA_TANG') .OR.
     +     (OPTION(1:9).EQ.'FULL_MECA')            ) THEN
         IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
            DPP1=-RHO12/RHO11
            DPP2=RHO12/RHO11
            IF (YATE.EQ.1) THEN
               DPT=RHO12*(H12-H11)/T
            ENDIF
            DCP1=DPP1/P2
            DCP2=DPP2/P2-PVP/P2/P2
            IF (YATE.EQ.1) THEN
               DCT=DPT/P2
            ENDIF
         ENDIF
         IF (THMC.EQ.'LIQU_VAPE') THEN
            DPP1=RHO12/RHO11
            IF (YATE.EQ.1) THEN
               DPT=RHO12*(H12-H11)/T
            ENDIF
         ENDIF
      ENDIF
C ======================================================================
C --- CALCUL DES DERIVEES DES MASSES VOLUMIQUES ET DU TERME AUXILIAIRE -
C ======================================================================
      IF ( (OPTION(1:16).EQ.'RIGI_MECA_TANG') .OR.
     +     (OPTION(1:9).EQ.'FULL_MECA')            ) THEN
         IF (THMC.EQ.'LIQU_SATU') THEN
            DR11P1 = RHO11*CLIQ
            IF (YATE.EQ.1) THEN
               DR11T = -3.D0*ALPLIQ*RHO11
            ENDIF
         ENDIF
         IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
            DR11P1=-RHO11*CLIQ
            IF (YATE.EQ.1) THEN
               DR11T=-3.D0*ALPLIQ*RHO11
            ENDIF
         ENDIF
         IF (THMC.EQ.'GAZ') THEN
            DR11P1=RHO11/P1
            IF (YATE.EQ.1) THEN
               DR11T=-RHO11/T
            ENDIF
         ENDIF
         IF (THMC.EQ.'LIQU_GAZ') THEN
            DR11P1=-RHO11*CLIQ
            DR11P2= RHO11*CLIQ
            DR11T=-3.D0*ALPLIQ*RHO11
            DR21P1= 0.D0
            DR21P2= RHO21/P2
            DR21T=-RHO21/T
         ENDIF
         IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
            DR11P1=-RHO11*CLIQ
            DR11P2= RHO11*CLIQ
     
            DR12P1=RHO12/PVP*DPP1
            DR12P2=RHO12/PVP*DPP2
     
C            DR21P1=RHO21/(P2-PVP)*(-DPP1)
C            DR21P2=RHO21/(P2-PVP)*(1.D0-DPP2)     
            DR21P1=MASRT*(-DPP1)
            DR21P2=MASRT*(1.D0-DPP2)
            
     
            IF (YATE.EQ.1) THEN
               DR11T=-3.D0*ALPLIQ*RHO11
               DR12T=RHO12*(DPT/PVP-1.D0/T)
C               DR21T=RHO21*(-DPT/(P2-PVP)-1.D0/T)
               DR21T=-MASRT*DPT-RHO21/T
C ======================================================================
C TERME AUXILIAIRE 
C ======================================================================
               DAUXP1=(H12-H11)/T*DR12P1
     &                  +RHO12/T*(DSDE(ADCP12+NDIM+1,ADDEP1)
     &                  -DSDE(ADCP11+NDIM+1,ADDEP1))
               DAUXP2=(H12-H11)/T*DR12P2
     &                  +RHO12/T*(DSDE(ADCP12+NDIM+1,ADDEP2)
     &                  -DSDE(ADCP11+NDIM+1,ADDEP2))
               DAUXT=(H12-H11)/T*DR12T
     &                  +RHO12/T*(DSDE(ADCP12+NDIM+1,ADDETE)-
     &                   DSDE(ADCP11+NDIM+1,ADDETE))-RHO12*(H12-H11)/T/T
            ENDIF
C ======================================================================
C --- CALCUL DES DERIVEES DE GRADPVP ET GRADCVP ------------------------
C ======================================================================
            DO 101 I=1,NDIM
               DGPP1(I)=(GRAP2(I)-GRAP1(I))/RHO11*DR12P1
               DGPP1(I)=DGPP1(I)-(GRAP2(I)-GRAP1(I))*RHO12/RHO11/RHO11
     &                           *DR11P1
               DGPP2(I)=(GRAP2(I)-GRAP1(I))/RHO11*DR12P2
               DGPP2(I)=DGPP2(I)-(GRAP2(I)-GRAP1(I))*RHO12/RHO11/RHO11
     &                           *DR11P2
               IF (YATE.EQ.1) THEN
                  DGPP1(I)=DGPP1(I)+DAUXP1*GRAT(I)
                  DGPP2(I)=DGPP2(I)+DAUXP2*GRAT(I)
                  DGPT(I)=(GRAP2(I)-GRAP1(I))/RHO11*DR12T
     &                           +DAUXT*GRAT(I)
                  DGPT(I)=DGPT(I)-(GRAP2(I)-GRAP1(I))*RHO12/RHO11/RHO11
     &                           *DR11T
               ENDIF
               DGPGP1=-RHO12/RHO11
               DGPGP2=RHO12/RHO11
               IF (YATE.EQ.1) THEN
                  DGPGT=RHO12*(H12-H11)/T
               ENDIF
C ======================================================================
C --- DERIVEES DE GRADCVP ----------------------------------------------
C ======================================================================
               DGCP1(I)=DGPP1(I)/P2 - GRAP2(I)/P2/P2*DPP1
               DGCP2(I)=DGPP2(I)/P2
     &                     -GP(I)/P2/P2-GRAP2(I)/P2/P2*DPP2
     &                     +2.D0*PVP*GRAP2(I)/P2/P2/P2
               IF (YATE.EQ.1) THEN
                  DGCT(I)=DGPT(I)/P2
     &                     -GRAP2(I)/P2/P2*DPT
               ENDIF
               DGCGP1=DGPGP1/P2
               DGCGP2=DGPGP2/P2-PVP/P2/P2
               IF (YATE.EQ.1) THEN
                  DGCGT=DGPGT/P2
               ENDIF     
 101        CONTINUE
         ENDIF
      ENDIF
C ======================================================================
         IF (THMC.EQ.'LIQU_VAPE') THEN
            DR11P1=RHO11*CLIQ
            DR12P1=RHO12/PVP*DPP1
            IF (YATE.EQ.1) THEN
               DR11T=-3.D0*ALPLIQ*RHO11
               DR12T=RHO12*(DPT/PVP-1.D0/T)
C ======================================================================
C TERME AUXILIAIRE 
C ======================================================================
               DAUXP1=(H12-H11)/T*DR12P1
     &                  +RHO12/T*(DSDE(ADCP12+NDIM+1,ADDEP1)
     &                  -DSDE(ADCP11+NDIM+1,ADDEP1))
               DAUXT=(H12-H11)/T*DR12T
     &                  +RHO12/T*(DSDE(ADCP12+NDIM+1,ADDETE)-
     &                   DSDE(ADCP11+NDIM+1,ADDETE))-RHO12*(H12-H11)/T/T
            ENDIF
C ======================================================================
C --- CALCUL DES DERIVEES DE GRADPVP ET GRADCVP ------------------------
C ======================================================================
            DO 111 I=1,NDIM
               DGPP1(I)=GRAP1(I)/RHO11*DR12P1
               DGPP1(I)=DGPP1(I)-GRAP1(I)*RHO12/RHO11/RHO11*DR11P1
               IF (YATE.EQ.1) THEN
                  DGPP1(I)=DGPP1(I)+DAUXP1*GRAT(I)
                  DGPT(I)=(GRAP1(I))/RHO11*DR12T+DAUXT*GRAT(I)
                  DGPT(I)=DGPT(I)-(GRAP1(I))*RHO12/RHO11/RHO11*DR11T
               ENDIF
               DGPGP1=RHO12/RHO11
               IF (YATE.EQ.1) THEN
                  DGPGT=RHO12*(H12-H11)/T
               ENDIF     
 111        CONTINUE
         ENDIF
C ======================================================================
C --- CALCUL DES FLUX HYDRAULIQUES -------------------------------------
C ======================================================================
      IF ( (OPTION(1:9).EQ.'RAPH_MECA') .OR.
     +     (OPTION(1:9).EQ.'FULL_MECA')      ) THEN 
         IF (((THMC.EQ.'LIQU_SATU').OR.(THMC.EQ.'GAZ')).OR.
     &        (THMC.EQ.'LIQU_GAZ_ATM')) THEN
            DO 102 I=1,NDIM
              IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
                 CONGEP(ADCP11+I)=RHO11*LAMBD1(1)
     &                         *(GRAP1(I)+RHO11*PESA(I))
              ELSE 
                 CONGEP(ADCP11+I)=RHO11*LAMBD1(1)
     &                         *(-GRAP1(I)+RHO11*PESA(I))
              ENDIF
 102        CONTINUE
         ENDIF
         IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
            DO 103 I=1,NDIM
               CONGEP(ADCP11+I)=RHO11*LAMBD1(1)
     &                          *(-GRAP2(I)+GRAP1(I)+RHO11*PESA(I))
               CONGEP(ADCP12+I)=RHO12*LAMBD2(1)
     &                          *(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
     &                          -RHO12*(1.D0-CVP)*F(1)*GC(I)
               CONGEP(ADCP21+I)=RHO21*LAMBD2(1)
     &                          *(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
     &                          +RHO21*CVP*F(1)*GC(I)
 103        CONTINUE
         ENDIF
         IF (THMC.EQ.'LIQU_VAPE') THEN
            DO 113 I=1,NDIM
               CONGEP(ADCP11+I)=RHO11*LAMBD1(1)
     &                          *(-GRAP1(I)+RHO11*PESA(I))
               CONGEP(ADCP12+I)=RHO12*LAMBD2(1)
     &                          *(-GP(I)+RHO12*PESA(I))
 113        CONTINUE
         ENDIF
         IF (THMC.EQ.'LIQU_GAZ') THEN
            DO 104 I=1,NDIM
               CONGEP(ADCP11+I)=RHO11*LAMBD1(1)
     &                          *(-GRAP2(I)+GRAP1(I)+RHO11*PESA(I))
               CONGEP(ADCP21+I)=RHO21*LAMBD2(1)
     &                          *(-GRAP2(I)+RHO21*PESA(I))
 104        CONTINUE
         ENDIF
      ENDIF
C ======================================================================
      IF ( (OPTION(1:16).EQ.'RIGI_MECA_TANG') .OR.
     &     (OPTION(1:9).EQ.'FULL_MECA')            ) THEN
         IF ((THMC.EQ.'LIQU_SATU').OR.(THMC.EQ.'GAZ').OR.
     &        (THMC.EQ.'LIQU_GAZ_ATM')) THEN
            DO 108 I=1,NDIM
               IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
                  DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &             +DR11P1*LAMBD1(1)*(GRAP1(I)+RHO11*PESA(I))
                  DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &             +RHO11*LAMBD1(3)*(GRAP1(I)+RHO11*PESA(I))
                  DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &             +RHO11*LAMBD1(1)*(DR11P1*PESA(I))
                  DSDE(ADCP11+I,ADDEP1+I)=DSDE(ADCP11+I,ADDEP1+I)
     &             +RHO11*LAMBD1(1)
               ELSE
                 DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &             +DR11P1*LAMBD1(1)*(-GRAP1(I)+RHO11*PESA(I))
                 DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &             +RHO11*LAMBD1(3)*(-GRAP1(I)+RHO11*PESA(I))
                 DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &             +RHO11*LAMBD1(1)*(DR11P1*PESA(I))
                 DSDE(ADCP11+I,ADDEP1+I)=DSDE(ADCP11+I,ADDEP1+I)
     &             -RHO11*LAMBD1(1)
               ENDIF
C ======================================================================
C --- PRESENCE DE CALCUL MECANIQUE -------------------------------------
C ======================================================================
               IF (YAMEC.EQ.1) THEN
                  DO 107 J=1,3
                     IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
                        DSDE(ADCP11+I,ADDEME+NDIM-1+I)=
     &                     DSDE(ADCP11+I,ADDEME+NDIM-1+I)
     &                     +RHO11*LAMBD1(2)*(GRAP1(I)+RHO11*PESA(I))
                     ELSE
                       DSDE(ADCP11+I,ADDEME+NDIM-1+I)=
     &                     DSDE(ADCP11+I,ADDEME+NDIM-1+I)
     &                     +RHO11*LAMBD1(2)*(-GRAP1(I)+RHO11*PESA(I))
                     ENDIF                        
 107              CONTINUE
               ENDIF
C ======================================================================
C --- PRESENCE DE CALCUL THERMIQUE -------------------------------------
C ======================================================================
               IF (YATE.EQ.1) THEN 
                  IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
                    DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &               +DR11T*LAMBD1(1)*(GRAP1(I)+RHO11*PESA(I))
                    DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &               +RHO11*LAMBD1(5)*(GRAP1(I)+RHO11*PESA(I))
                    DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &               +RHO11*LAMBD1(1)*(DR11T*PESA(I))
                  ELSE
                    DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &               +DR11T*LAMBD1(1)*(-GRAP1(I)+RHO11*PESA(I))
                    DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &               +RHO11*LAMBD1(5)*(-GRAP1(I)+RHO11*PESA(I))
                    DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &               +RHO11*LAMBD1(1)*(DR11T*PESA(I))
                  ENDIF        
               ENDIF
 108        CONTINUE
         ENDIF
         IF (THMC.EQ.'LIQU_VAPE_GAZ'.OR.
     >       THMC.EQ.'LIQU_GAZ') THEN
            DO 105 I=1,NDIM
C ======================================================================
C --- DERIVEE DU FLUX LIQUIDE ------------------------------------------
C ======================================================================
               DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &           +DR11P1*LAMBD1(1)*(-GRAP2(I)+GRAP1(I)+RHO11*PESA(I))
     &           +RHO11*LAMBD1(1)*(DR11P1*PESA(I))
     &           +RHO11*LAMBD1(3)*(-GRAP2(I)+GRAP1(I)+RHO11*PESA(I))
               DSDE(ADCP11+I,ADDEP2)=DSDE(ADCP11+I,ADDEP2)
     &           +DR11P2*LAMBD1(1)*(-GRAP2(I)+GRAP1(I)+RHO11*PESA(I))
     &           +RHO11*LAMBD1(1)*(DR11P2*PESA(I))
     &           +RHO11*LAMBD1(4)*(-GRAP2(I)+GRAP1(I)+RHO11*PESA(I))
               DSDE(ADCP11+I,ADDEP1+I)=DSDE(ADCP11+I,ADDEP1+I)
     &           +RHO11*LAMBD1(1)
               DSDE(ADCP11+I,ADDEP2+I)=DSDE(ADCP11+I,ADDEP2+I)
     &           -RHO11*LAMBD1(1)
C ======================================================================
C --- DERIVEE DU FLUX DE VAPEUR ----------------------------------------
C ======================================================================
               IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN              
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           +DR12P1*LAMBD2(1)*(-GRAP2(I)+(RHO12+RHO21)*PESA(I))    
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           +RHO12*LAMBD2(3)*(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           +RHO12*LAMBD2(1)*((DR12P1+DR21P1)*PESA(I))
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           -DR12P1*(1.D0-CVP)*F(1)*GC(I)
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           +RHO12*(DCP1)*F(1)*GC(I)
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           -RHO12*(1.D0-CVP)*F(3)*GC(I)
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           -RHO12*(1.D0-CVP)*F(1)*DGCP1(I)
                DSDE(ADCP12+I,ADDEP2)=DSDE(ADCP12+I,ADDEP2)
     &           +DR12P2*LAMBD2(1)*(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                DSDE(ADCP12+I,ADDEP2)=DSDE(ADCP12+I,ADDEP2)
     &           +RHO12*LAMBD2(4)*(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                DSDE(ADCP12+I,ADDEP2)=DSDE(ADCP12+I,ADDEP2)
     &           +RHO12*LAMBD2(1)*((DR12P2+DR21P2)*PESA(I))
                DSDE(ADCP12+I,ADDEP2)=DSDE(ADCP12+I,ADDEP2)
     &           -DR12P2*(1.D0-CVP)*F(1)*GC(I)
                DSDE(ADCP12+I,ADDEP2)=DSDE(ADCP12+I,ADDEP2)
     &           +RHO12*(DCP2)*F(1)*GC(I)
                DSDE(ADCP12+I,ADDEP2)=DSDE(ADCP12+I,ADDEP2)
     &           -RHO12*(1.D0-CVP)*F(4)*GC(I)
                DSDE(ADCP12+I,ADDEP2)=DSDE(ADCP12+I,ADDEP2)
     &           -RHO12*(1.D0-CVP)*F(1)*DGCP2(I)
                DSDE(ADCP12+I,ADDEP1+I)=DSDE(ADCP12+I,ADDEP1+I)
     &           -RHO12*(1.D0-CVP)*F(1)*DGCGP1
                DSDE(ADCP12+I,ADDEP2+I)=DSDE(ADCP12+I,ADDEP2+I)
     &           -RHO12*LAMBD2(1)-RHO12*(1.D0-CVP)*F(1)*DGCGP2
               ENDIF
C ======================================================================
C --- DERIVEE DU FLUX D'AIR SEC ----------------------------------------
C ======================================================================
               DSDE(ADCP21+I,ADDEP1)=DSDE(ADCP21+I,ADDEP1)
     &           +DR21P1*LAMBD2(1)*(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
               DSDE(ADCP21+I,ADDEP1)=DSDE(ADCP21+I,ADDEP1)
     &           +RHO21*LAMBD2(3)*(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
               DSDE(ADCP21+I,ADDEP1)=DSDE(ADCP21+I,ADDEP1)
     &           +RHO21*LAMBD2(1)*((DR12P1+DR21P1)*PESA(I))
               DSDE(ADCP21+I,ADDEP1)=DSDE(ADCP21+I,ADDEP1)
     &           +DR21P1*CVP*F(1)*GC(I)
               DSDE(ADCP21+I,ADDEP1)=DSDE(ADCP21+I,ADDEP1)
     &           +RHO21*(DCP1)*F(1)*GC(I)
               DSDE(ADCP21+I,ADDEP1)=DSDE(ADCP21+I,ADDEP1)
     &           +RHO21*CVP*F(3)*GC(I)
               DSDE(ADCP21+I,ADDEP1)=DSDE(ADCP21+I,ADDEP1)
     &           +RHO21*CVP*F(1)*DGCP1(I)
               DSDE(ADCP21+I,ADDEP2)=DSDE(ADCP21+I,ADDEP2)
     &           +DR21P2*LAMBD2(1)*(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
               DSDE(ADCP21+I,ADDEP2)=DSDE(ADCP21+I,ADDEP2)
     &           +RHO21*LAMBD2(4)*(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
               DSDE(ADCP21+I,ADDEP2)=DSDE(ADCP21+I,ADDEP2)
     &           +RHO21*LAMBD2(1)*((DR12P2+DR21P2)*PESA(I))
               DSDE(ADCP21+I,ADDEP2)=DSDE(ADCP21+I,ADDEP2)
     &           +DR21P2*CVP*F(1)*GC(I)
               DSDE(ADCP21+I,ADDEP2)=DSDE(ADCP21+I,ADDEP2)
     &           +RHO21*(DCP2)*F(1)*GC(I)
               DSDE(ADCP21+I,ADDEP2)=DSDE(ADCP21+I,ADDEP2)
     &           +RHO21*CVP*F(4)*GC(I)
               DSDE(ADCP21+I,ADDEP2)=DSDE(ADCP21+I,ADDEP2)
     &           +RHO21*CVP*F(1)*DGCP2(I)
               DSDE(ADCP21+I,ADDEP1+I)=DSDE(ADCP21+I,ADDEP1+I)
     &           +RHO21*CVP*F(1)*DGCGP1
               DSDE(ADCP21+I,ADDEP2+I)=DSDE(ADCP21+I,ADDEP2+I)
     &           -RHO21*LAMBD2(1)+RHO21*CVP*F(1)*DGCGP2
C ======================================================================
C --- TERMES COMPLEMENTAIRES DE MECANIQUE ET THERMIQUE -----------------
C ======================================================================
               IF (YAMEC.EQ.1) THEN
                  DO 106 J=1,3
                     DSDE(ADCP11+I,ADDEME+NDIM-1+J)=
     &               DSDE(ADCP11+I,ADDEME+NDIM-1+J)
     &               +RHO11*LAMBD1(2)*(-GRAP2(I)+GRAP1(I)+RHO11*PESA(I))
                     DSDE(ADCP12+I,ADDEME+NDIM-1+J)=
     &               DSDE(ADCP12+I,ADDEME+NDIM-1+J)
     &                +RHO12*LAMBD2(2)*(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                     DSDE(ADCP12+I,ADDEME+NDIM-1+J)=
     &               DSDE(ADCP12+I,ADDEME+NDIM-1+J)
     &                 -RHO12*(1.D0-CVP)*F(2)*GC(I)
                     DSDE(ADCP21+I,ADDEME+NDIM-1+J)=
     &               DSDE(ADCP21+I,ADDEME+NDIM-1+J)
     &                +RHO21*LAMBD2(2)*(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                     DSDE(ADCP21+I,ADDEME+NDIM-1+J)=
     &               DSDE(ADCP21+I,ADDEME+NDIM-1+J)
     &                +RHO21*CVP*F(2)*GC(I)
 106              CONTINUE
               ENDIF
               IF (YATE.EQ.1) THEN
                  DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &              +DR11T*LAMBD1(1)*(-GRAP2(I)+GRAP1(I)+RHO11*PESA(I))
     &              +RHO11*LAMBD1(5)*(-GRAP2(I)+GRAP1(I)+RHO11*PESA(I))
     &              +RHO11*LAMBD1(1)*DR11T*PESA(I)
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              +DR12T*LAMBD1(1)*(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              +RHO12*LAMBD1(5)*(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              +RHO12*LAMBD1(1)*((DR12T+DR21T)*PESA(I))
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              -DR12T*(1.D0-CVP)*F(1)*GC(I)
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              +RHO12*DCT*F(1)*GC(I)
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              -RHO12*(1.D0-CVP)*F(5)*GC(I)
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              -RHO12*(1.D0-CVP)*F(1)*DGCT(I)
                  DSDE(ADCP12+I,ADDETE+I)=DSDE(ADCP12+I,ADDETE+I)
     &              -RHO12*(1.D0-CVP)*F(1)*DGCGT
                  DSDE(ADCP21+I,ADDETE)=DSDE(ADCP21+I,ADDETE)
     &              +DR21T*LAMBD2(1)*(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                  DSDE(ADCP21+I,ADDETE)=DSDE(ADCP21+I,ADDETE)
     &              +RHO21*LAMBD2(5)*(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                  DSDE(ADCP21+I,ADDETE)=DSDE(ADCP21+I,ADDETE)
     &              +RHO21*LAMBD2(1)*((DR12T+DR21T)*PESA(I))
                  DSDE(ADCP21+I,ADDETE)=DSDE(ADCP21+I,ADDETE)
     &              +DR21T*CVP*F(1)*GC(I)
                  DSDE(ADCP21+I,ADDETE)=DSDE(ADCP21+I,ADDETE)
     &              +RHO21*DCT*F(1)*GC(I)
                  DSDE(ADCP21+I,ADDETE)=DSDE(ADCP21+I,ADDETE)
     &              +RHO21*CVP*F(5)*GC(I)
                  DSDE(ADCP21+I,ADDETE)=DSDE(ADCP21+I,ADDETE)
     &              +RHO21*CVP*F(1)*DGCT(I)
                  DSDE(ADCP21+I,ADDETE+I)=DSDE(ADCP21+I,ADDETE+I)
     &              +RHO21*CVP*F(1)*DGCGT
               ENDIF
 105        CONTINUE
         ENDIF
C ======================================================================
         IF (THMC.EQ.'LIQU_VAPE') THEN
            DO 115 I=1,NDIM
C ======================================================================
C --- DERIVEE DU FLUX LIQUIDE ------------------------------------------
C ======================================================================
               DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &           +DR11P1*LAMBD1(1)*(-GRAP1(I)+RHO11*PESA(I))
     &           +RHO11*LAMBD1(1)*(DR11P1*PESA(I))
     &           +RHO11*LAMBD1(3)*(-GRAP1(I)+RHO11*PESA(I))
               DSDE(ADCP11+I,ADDEP1+I)=DSDE(ADCP11+I,ADDEP1+I)
     &           -RHO11*LAMBD1(1)
C ======================================================================
C --- DERIVEE DU FLUX DE VAPEUR ----------------------------------------
C ======================================================================
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           +DR12P1*LAMBD2(1)*(-GP(I)+RHO12*PESA(I))    
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           +RHO12*LAMBD2(3)*(-GP(I)+RHO12*PESA(I))
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           +RHO12*LAMBD2(1)*((DR12P1)*PESA(I))
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           -RHO12*LAMBD2(1)*DGPP1(I)
                DSDE(ADCP12+I,ADDEP1+I)=DSDE(ADCP12+I,ADDEP1+I)
     &           -RHO12*LAMBD2(1)*DGPGP1
C ======================================================================
C --- TERMES COMPLEMENTAIRES DE MECANIQUE ET THERMIQUE -----------------
C ======================================================================
               IF (YAMEC.EQ.1) THEN
                  DO 116 J=1,3
                     DSDE(ADCP11+I,ADDEME+NDIM-1+J)=
     &               DSDE(ADCP11+I,ADDEME+NDIM-1+J)
     &               +RHO11*LAMBD1(2)*(-GRAP1(I)+RHO11*PESA(I))
                     DSDE(ADCP12+I,ADDEME+NDIM-1+J)=
     &               DSDE(ADCP12+I,ADDEME+NDIM-1+J)
     &                +RHO12*LAMBD2(2)*(-GP(I)+RHO12*PESA(I))
 116              CONTINUE
               ENDIF
               IF (YATE.EQ.1) THEN
                  DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &              +DR11T*LAMBD1(1)*(-GRAP1(I)+RHO11*PESA(I))
     &              +RHO11*LAMBD1(5)*(-GRAP1(I)+RHO11*PESA(I))
     &              +RHO11*LAMBD1(1)*DR11T*PESA(I)
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              +DR12T*LAMBD1(1)*(-GP(I)+RHO12*PESA(I))
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              +RHO12*LAMBD1(5)*(-GP(I)+RHO12*PESA(I))
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              +RHO12*LAMBD1(1)*((DR12T)*PESA(I))
                DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &           -RHO12*LAMBD2(1)*DGPT(I)
                DSDE(ADCP12+I,ADDETE+I)=DSDE(ADCP12+I,ADDETE+I)
     &           -RHO12*LAMBD2(1)*DGPGT
               ENDIF
 115        CONTINUE
         ENDIF
      ENDIF
C ======================================================================
      END
