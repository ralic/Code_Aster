        SUBROUTINE CALCFH(OPTION,MECA,THMC,THER,HYDR,IMATE,NDIM,DIMDEF,
     +                    DIMCON,YAMEC,YATE,ADDEP1,ADDEP2,
     +                    ADCP11,ADCP12,ADCP21,ADCP22,ADDEME,ADDETE,
     +                    CONGEM,CONGEP,DSDE,P1,P2,
     +                    GRAP1,GRAP2,T,GRAT,PHI,PVP,PAD,RHO11,H11,H12,
     +                    H21,H22,R, RHOD, CPD, BIOT, SAT, DSATP1,
     +                    PESA, PERMFH, PERMLI, DPERML, KREL2, DKR2S,
     +                    DKR2P, FICK, DFICKT, DFICKG, FICKAD,DFADT,
     +                    LAMBDD, DLAMBD,KH,
     +                    RHOL, CLIQ, ALPLIQ, CPL, LAMBDL, DLAMBL,
     +                    VISCL, DVISCL, MAMOLG, CPG, LAMBDG, DLAMBG,
     +                    VISCG, DVISCG, MAMOLV, CPVG, VISCVG, DVISVG,
     +                    RETCOM,ISOT)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 28/09/2004   AUTEUR GRANET S.GRANET 
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
C COND(4) -> KREL2 : PERM_GAZ            SOUS THM_DIFFU ---------------
C COND(5) -> DKR2S : D_PERM_SATU_GAZ     SOUS THM_DIFFU ---------------
C COND(6) -> DKR2P : D_PERM_PRES_GAZ     SOUS THM_DIFFU ---------------
C COND(7) -> FICK   : FICK                SOUS THM_DIFFU ---------------
C COND(8) -> DFICKT : D_FICK_TEMP         SOUS THM_DIFFU ---------------
C COND(9) -> DFICKG : D_FICK_GAZ_PRES     SOUS THM_DIFFU ---------------
C ======================================================================
C OUT RETCOM : RETOUR LOI DE COMPORTEMENT
C ======================================================================
C
C
      IMPLICIT      NONE
      INTEGER       NDIM,DIMDEF,DIMCON,IMATE,YAMEC,YATE,ADCP22,RETCOM
      INTEGER       ADDEME,ADDEP1,ADDEP2,ADDETE,ADCP11,ADCP12,ADCP21
      REAL*8        CONGEM(1:DIMCON),CONGEP(1:DIMCON)
      REAL*8        DSDE(1:DIMCON,1:DIMDEF),P1,GRAP1(3),P2,T
      REAL*8        GRAP2(3),GRAT(3),PHI,PVP,PAD,H11,H12,H21,H22,RHO11
      REAL*8        R, RHOD, CPD, BIOT, SAT, DSATP1, PESA(3), PERMFH
      REAL*8        PERMLI, DPERML, KREL2, DKR2S, DKR2P, FICK
      REAL*8        DFICKT, DFICKG, LAMBDD, DLAMBD, RHOL, CLIQ, ALPLIQ
      REAL*8        FICKAD, DFADT
      REAL*8        CPL, LAMBDL, DLAMBL, VISCL, DVISCL, CPG, LAMBDG
      REAL*8        DLAMBG, VISCG, DVISCG, CPVG, VISCVG, DVISVG
      REAL*8        MAMOLG,MAMOLV,ISOT(3)
      CHARACTER*16  OPTION,MECA,THMC,THER,HYDR
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER         I,J
      REAL*8          LAMBD1(5),LAMBD2(5),FV(5),FA(5),VISCO,DVISCO
      REAL*8          KREL1,DKREL1,RHO12,RHO21,MASRT,ENDOM
      REAL*8          RHO22, KH
C    
C VARIABLES LOCALES PERMETTANT D'EXPRIMER LES DERIVEES DES
C GRADIENTS DES PRESSIONS DE GAZ ET DE VAPEUR
C
C ON UTILISE LES CONVENTIONS SUIVANTES :
C    
C  D = DERIVEE   G = GRADIENT   P = PRESSION VAPEUR
C  C = PVP/PGZ   P1,P2,T 
C 
C   CVP=PVP/PGZ , GP = GRADIENT DE PVP , GC = GRADIENT DE CVP
      REAL*8 CVP,GP(3),GC(3)
C   CAD=RHOAD/MAMOLG , GPA = GRADIENT DE PAD , GCA = GRADIENT DE CAD
      REAL*8 GPA(3),GCA(3)
C DERIVEES DE PW PAR RAPPORT A P1, P2, T
      REAL*8          DP11P1,DP11P2,DP11T
C DERIVEES DE PAS PAR RAPPORT A P1, P2, T
      REAL*8          DP21P1,DP21P2,DP21T
C DERIVEES DE PAD PAR RAPPORT A P1, P2, T
      REAL*8          DP22P1,DP22P2,DP22T
C   DERIVEES DE PVP PAR RAPPORT A P1, P2, T
      REAL*8 DP12P1,DP12P2,DP12T
C   DERIVEES DE CVP PAR RAPPORT A P1, P2, T
      REAL*8 DCVP1,DCVP2,DCVT
C   DERIVEES DE RHO11 PAR RAPPORT A P1, P2, T
      REAL*8 DR11P1,DR11P2,DR11T
C   DERIVEES DE RHO12 PAR RAPPORT A P1, P2, T
      REAL*8 DR12P1,DR12P2,DR12T
C   DERIVEES DE RHO22 PAR RAPPORT A P1, P2, T
      REAL*8 DR22P1,DR22P2,DR22T
C   DERIVEES DE RHO21 PAR RAPPORT A P1, P2, T
      REAL*8 DR21P1,DR21P2,DR21T
C   DERIVEES DU TERME AUXILIAIRE PAR RAPPORT A P1, P2, T
C   TERME AUX = RHO12*(H12-H11)/T
      REAL*8 DAUXP1,DAUXP2,DAUXT
C  DERIVEES DU GRADIENT DE PVP PAR RAPPORT A P1, P2, T
      REAL*8 DGPVP1(3),DGPVP2(3),DGPVT(3)
C  DERIVEES DU GRADIENT DE PAD PAR RAPPORT A P1, P2, T
      REAL*8 DGPAP1(3),DGPAP2(3),DGPAT(3)
C  DERIVEES DU GRADIENT DE CVP PAR RAPPORT A P1, P2, T
      REAL*8 DGCVP1(3),DGCVP2(3),DGCVT(3)
C  DERIVEES DU GRADIENT DE CAD PAR RAPPORT A P1, P2, T
      REAL*8 DGCAP1(3),DGCAP2(3),DGCAT(3)
C  DERIVEES DU GRADIENT DE PVP (1) OU PAD(2) PAR RAPPORT
C  AUX GRADIENTS DE P1,P2,T
      REAL*8 DGPGP1(2),DGPGP2(2),DGPGT(2)
C  DERIVEES DU GRADIENT DE CVP (1) OU CAD (2) PAR RAPPORT
C  AUX GRADIENTS DE P1,P2,T
      REAL*8 DGCGP1(2),DGCGP2(2),DGCGT(2)
C  DERIVEES SECONDES DE PVP (1) OU PAD (2) 
      REAL*8 DP1PP1(2),DP2PP1(2),DTPP1(2),DP1PP2(2),DP2PP2(2)
      REAL*8 DTPP2(2),DP1PT(2),DP2PT(2),DTPT(2)
C 
C ======================================================================
C --- QUELQUES INITIALISATIONS -----------------------------------------
C ======================================================================
C
      DO 1 I = 1 , 3
       DGPVP1(I)=0.D0
       DGPVP2(I)=0.D0
       DGPVT(I) =0.D0
       DGPAP1(I)=0.D0
       DGPAP2(I)=0.D0
       DGPAT(I) =0.D0
       DGCVP1(I)=0.D0
       DGCVP2(I)=0.D0
       DGCVT(I)=0.D0
       DGCAP1(I)=0.D0
       DGCAP2(I)=0.D0
       DGCAT(I)=0.D0
       GC(I)=0.D0
       GP(I)=0.D0
       GCA(I)=0.D0
       GPA(I)=0.D0
 1    CONTINUE
       DP12P1=0.D0
       DP12P2=0.D0
       DP12T=0.D0
       DP11P1=0.D0
       DP11P2=0.D0
       DP11T=0.D0
       DP21P1=0.D0
       DP21P2=0.D0
       DP21T=0.D0
       DP22P1=0.D0
       DP22P2=0.D0
       DP22T=0.D0
      DO 2 I = 1 , 2
       DGPGP1(I)=0.D0
       DGPGP2(I)=0.D0
       DGPGT(I)=0.D0
       DGCGP1(I)=0.D0
       DGCGP2(I)=0.D0
       DGCGT(I)=0.D0
       DP1PP2(I)=0.D0
       DP2PP2(I)=0.D0
       DTPP2(I)=0.D0
       DP1PP1(I)=0.D0
       DP2PP1(I)=0.D0
       DTPP1(I)=0.D0
       DP1PT(I)=0.D0
       DP2PT(I)=0.D0
       DTPT(I)=0.D0
 2    CONTINUE
       DCVP1=0.D0
       DCVP2=0.D0
       DCVT=0.D0
       DR22P1=0.D0
       DR22P2=0.D0
       DR22T=0.D0
       RHO22=0.D0
C **********************************************************************
C   RECUPERATION DES COEFFICIENTS 
C ======================================================================
      IF (THMC.EQ.'LIQU_SATU') THEN
         KREL1   = 1.D0
         DKREL1  = 0.D0
         VISCO  = VISCL
         DVISCO = DVISCL
      ENDIF
C
      IF (THMC.EQ.'GAZ') THEN
         KREL1   = 1.D0
         DKREL1  = 0.D0
         VISCO  = VISCG
         DVISCO = DVISCG
      ENDIF
C
      IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
         VISCO  = VISCL
         DVISCO = DVISCL
         KREL1   = PERMLI
         DKREL1  = DPERML*DSATP1
      ENDIF
C
      IF ( (THMC.EQ.'LIQU_VAPE_GAZ')    .OR.
     +    (THMC.EQ.'LIQU_AD_GAZ_VAPE') .OR.
     +    (THMC.EQ.'LIQU_GAZ')         .OR.
     +    (THMC.EQ.'LIQU_VAPE')          ) THEN
         KREL1   = PERMLI
         DKREL1  = DPERML*DSATP1
         VISCO  = VISCL
         DVISCO = DVISCL
C
         IF ((THMC.EQ.'LIQU_VAPE_GAZ').OR.
     &      (THMC.EQ.'LIQU_AD_GAZ_VAPE')) THEN
            FV(1) = FICK
            FV(2) = 0.D0
            FV(3) = 0.D0
            FV(4) = DFICKG
            FV(5) = DFICKT
         ELSE
            FV(1) = 0.D0
            FV(2) = 0.D0
            FV(3) = 0.D0
            FV(4) = 0.D0
            FV(5) = 0.D0
         ENDIF
         IF ((THMC.EQ.'LIQU_AD_GAZ_VAPE')) THEN
          FA(1)=FICKAD
          FA(2)=0.D0
          FA(3)=0.D0
          FA(4)=0.D0
          FA(5)=DFADT
         ELSE
          FA(1)=0.D0
          FA(2)=0.D0
          FA(3)=0.D0
          FA(4)=0.D0
          FA(5)=0.D0
         ENDIF
C ======================================================================
C --- CALCUL DE LAMBDA2 ------------------------------------------------
C ======================================================================
C --- LAMBD2(1) = CONDUC_HYDRO_GAZ -------------------------------------
C --- LAMBD2(2) = D(CONDUC_HYDRO_GAZ)/DEPSV ----------------------------
C --- LAMBD2(3) = D(CONDUC_HYDRO_GAZ)/DP1 ------------------------------
C --- LAMBD2(4) = D(CONDUC_HYDRO_GAZ)/DP2 ------------------------------
C --- LAMBD2(5) = D(CONDUC_HYDRO_GAZ)/DT -------------------------------
C ======================================================================
        IF(THMC.EQ.'LIQU_VAPE')THEN
           RHO12=MAMOLV*PVP/R/T
           LAMBD2(1) =   PERMFH*KREL2/VISCG
           LAMBD2(2) =   0.0D0
           LAMBD2(3) =   PERMFH*DKR2S*DSATP1*(RHO12/RHO11-1.D0)/VISCG
           LAMBD2(4) =   PERMFH*DKR2P/VISCG
           LAMBD2(5) = - PERMFH*KREL2/VISCG/VISCG*DVISCG+PERMFH*
     +               DKR2S*DSATP1*RHO12*(H12-H11)/T/VISCG
        ELSE
           LAMBD2(1) =   PERMFH*KREL2/VISCG
           LAMBD2(2) =   0.0D0
           LAMBD2(3) =   PERMFH*DKR2S*DSATP1/VISCG
           LAMBD2(4) =   PERMFH*DKR2P/VISCG
           LAMBD2(5) = - PERMFH*KREL2/VISCG/VISCG*DVISCG
         ENDIF
      ENDIF
C ======================================================================
C --- CALCUL DE LAMBDA1 ------------------------------------------------
C ======================================================================
C --- LAMBD1(1) = CONDUC_HYDRO_LIQ -------------------------------------
C --- LAMBD1(2) = D(CONDUC_HYDRO_LIQ)/DEPSV ----------------------------
C --- LAMBD1(3) = D(CONDUC_HYDRO_LIQ)/DP1 ------------------------------
C --- LAMBD1(4) = D(CONDUC_HYDRO_LIQ)/DP2 ------------------------------
C --- LAMBD1(5) = D(CONDUC_HYDRO_LIQ)/DT -------------------------------
C ======================================================================
      IF(THMC.EQ.'LIQU_VAPE')THEN
        LAMBD1(1) =   PERMFH*KREL1/VISCO
        LAMBD1(2) =   0.0D0
        LAMBD1(3) =   PERMFH*DKREL1*(RHO12/RHO11-1.D0)/VISCO
        LAMBD1(4) =   0.0D0
        LAMBD1(5) = - PERMFH*KREL1/VISCO/VISCO*DVISCO+PERMFH*
     +               DKREL1*RHO12*(H12-H11)/T/VISCO
      ELSE
        LAMBD1(1) =   PERMFH*KREL1/VISCO
        LAMBD1(2) =   0.0D0
        LAMBD1(3) =   PERMFH*DKREL1/VISCO
        LAMBD1(4) =   0.0D0
        LAMBD1(5) = - PERMFH*KREL1/VISCO/VISCO*DVISCO
      ENDIF
C
C **********************************************************************
C  CALCUL DES MASSES VOLUMIQUES, PRESSION DE VAPEUR, GRADIENTS DE VAPEUR
C
C
      IF ((THMC.EQ.'LIQU_VAPE_GAZ')) THEN
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
C ***********************************************************
C  CALCUL DES MASSES VOLUMIQUES, PRESSION D AIR DISSOUS, 
C  GRADIENTS D'AIR DISSOUS ET VAPEUR
C
      IF (THMC.EQ.'LIQU_AD_GAZ_VAPE') THEN
         RHO12=MAMOLV*PVP/R/T
         RHO21=MAMOLG*(P2-PVP)/R/T
         MASRT=MAMOLG/R/T
         RHO22=MAMOLG*PAD/R/T
         CVP=PVP/P2
C         
C CALCUL DES DERIVEES PARTIELLES PREMIERES ET SECONDES         
C
         CALL HMDERP(YATE,T,R,KH,PVP,PAD,RHO11,RHO12,
     &               H11,H12,CLIQ,ALPLIQ,
     &               DP11P1,DP11P2,DP11T,DP12P1,DP12P2,DP12T,
     &               DP21P1,DP21P2,DP21T,DP22P1,DP22P2,DP22T,
     &               DP1PP1,DP2PP1,DTPP1,DP1PP2,DP2PP2,DTPP2,
     &               DP1PT,DP2PT,DTPT
     &                       )
         DO 200 I=1,NDIM
            GP(I) = DP12P2 * GRAP2(I) + DP12P1 * GRAP1(I)
            GPA(I)= DP22P2 * GRAP2(I) + DP22P1 * GRAP1(I)
            IF (YATE.EQ.1) THEN
               GP(I) =GP(I)+DP12T*GRAT(I)
               GPA(I)=GPA(I)+DP22T*GRAT(I)
            ENDIF
            GC(I)=GP(I)/P2-PVP/P2/P2*GRAP2(I)
            GCA(I)=MAMOLG*GPA(I)/R/T
            IF (YATE.EQ.1) THEN
              GCA(I)=GCA(I)-MAMOLG*PAD/R/T/T*GRAT(I)
            ENDIF
 200     CONTINUE
      ENDIF

C
C *********************************************************************
C CALCUL DES DERIVEES DES PRESSIONS DE VAPEUR ET DES CVP
C
      IF ((OPTION(1:9).EQ.'RIGI_MECA').OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
         IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
            DP12P1=-RHO12/RHO11
            DP12P2=RHO12/RHO11
            IF (YATE.EQ.1) THEN
               DP12T=RHO12*(H12-H11)/T
            ENDIF
            DCVP1=DP12P1/P2
            DCVP2=DP12P2/P2-PVP/P2/P2
            IF (YATE.EQ.1) THEN
               DCVT=DP12T/P2
            ENDIF
         ENDIF
C
         IF (THMC.EQ.'LIQU_AD_GAZ_VAPE') THEN
            DCVP1=DP12P1/P2
            DCVP2=DP12P2/P2-PVP/P2/P2
            IF (YATE.EQ.1) THEN
               DCVT=DP12T/P2
            ENDIF
         ENDIF
         IF (THMC.EQ.'LIQU_VAPE') THEN
            DP12P1=RHO12/RHO11
            IF (YATE.EQ.1) THEN
               DP12T=RHO12*(H12-H11)/T
            ENDIF
         ENDIF
      ENDIF
C
C **********************************************************************
C CALCUL DES DERIVEES DES MASSES VOLUMIQUES ET DU TERME AUXILIAIRE
C
      IF ((OPTION(1:9).EQ.'RIGI_MECA').OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
         IF (THMC.EQ.'LIQU_SATU') THEN
            DR11P1=RHO11*CLIQ
            IF (YATE.EQ.1) THEN
               DR11T=-3.D0*ALPLIQ*RHO11
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
     
            DR12P1=RHO12/PVP*DP12P1
            DR12P2=RHO12/PVP*DP12P2
     
C            DR21P1=RHO21/(P2-PVP)*(-DP12P1)
C            DR21P2=RHO21/(P2-PVP)*(1.D0-DP12P2)     
            DR21P1=MASRT*(-DP12P1)
            DR21P2=MASRT*(1.D0-DP12P2)
            
     
            IF (YATE.EQ.1) THEN
               DR11T=-3.D0*ALPLIQ*RHO11
               DR12T=RHO12*(DP12T/PVP-1.D0/T)
               DR21T=-MASRT*DP12T-RHO21/T
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
     &                   DSDE(ADCP11+NDIM+1,ADDETE))-
     &                   RHO12*(H12-H11)/T/T
            ENDIF
C
C
C **********************************************************************
C CALCUL DES DERIVEES DE GRADPVP ET GRADCVP
C
            DO 101 I=1,NDIM
               DGPVP1(I)=(GRAP2(I)-GRAP1(I))/RHO11*DR12P1
               DGPVP1(I)=DGPVP1(I)-(GRAP2(I)-GRAP1(I))*RHO12/RHO11/RHO11
     &                           *DR11P1
               DGPVP2(I)=(GRAP2(I)-GRAP1(I))/RHO11*DR12P2
               DGPVP2(I)=DGPVP2(I)-(GRAP2(I)-GRAP1(I))*RHO12/RHO11/RHO11
     &                           *DR11P2
               IF (YATE.EQ.1) THEN
                  DGPVP1(I)=DGPVP1(I)+DAUXP1*GRAT(I)
                  DGPVP2(I)=DGPVP2(I)+DAUXP2*GRAT(I)
                  DGPVT(I)=(GRAP2(I)-GRAP1(I))/RHO11*DR12T
     &                           +DAUXT*GRAT(I)
                  DGPVT(I)=DGPVT(I)-(GRAP2(I)-GRAP1(I))*
     &                           RHO12/RHO11/RHO11*DR11T
               ENDIF
               DGPGP1(1)=-RHO12/RHO11
               DGPGP2(1)=RHO12/RHO11
               IF (YATE.EQ.1) THEN
                  DGPGT(1)=RHO12*(H12-H11)/T
               ENDIF
C **********************************************************************
C DERIVEES DE GRADCVP
C
               DGCVP1(I)=DGPVP1(I)/P2
     &                     -GRAP2(I)/P2/P2*DP12P1
               DGCVP2(I)=DGPVP2(I)/P2
     &                     -GP(I)/P2/P2-GRAP2(I)/P2/P2*DP12P2
     &                     +2.D0*PVP*GRAP2(I)/P2/P2/P2
               IF (YATE.EQ.1) THEN
                  DGCVT(I)=DGPVT(I)/P2
     &                     -GRAP2(I)/P2/P2*DP12T
               ENDIF
               DGCGP1(1)=DGPGP1(1)/P2
               DGCGP2(1)=DGPGP2(1)/P2-PVP/P2/P2
               IF (YATE.EQ.1) THEN
                  DGCGT(1)=DGPGT(1)/P2
               ENDIF     
 101        CONTINUE
         ENDIF
      ENDIF
C
         IF (THMC.EQ.'LIQU_VAPE') THEN
            DR11P1=RHO11*CLIQ
            DR12P1=RHO12/PVP*DP12P1
            IF (YATE.EQ.1) THEN
               DR11T=-3.D0*ALPLIQ*RHO11
               DR12T=RHO12*(DP12T/PVP-1.D0/T)
C    
C TERME AUXILIAIRE 

               DAUXP1=(H12-H11)/T*DR12P1
     &                  +RHO12/T*(DSDE(ADCP12+NDIM+1,ADDEP1)
     &                  -DSDE(ADCP11+NDIM+1,ADDEP1))
               DAUXT=(H12-H11)/T*DR12T
     &                  +RHO12/T*(DSDE(ADCP12+NDIM+1,ADDETE)-
     &                   DSDE(ADCP11+NDIM+1,ADDETE))-RHO12*
     &                   (H12-H11)/T/T
            ENDIF
C
C
C **********************************************************************
C CALCUL DES DERIVEES DE GRADPVP ET GRADCVP
C
            DO 111 I=1,NDIM
               DGPVP1(I)=GRAP1(I)/RHO11*DR12P1
               DGPVP1(I)=DGPVP1(I)-GRAP1(I)*RHO12/RHO11/RHO11
     &                           *DR11P1
               IF (YATE.EQ.1) THEN
                  DGPVP1(I)=DGPVP1(I)+DAUXP1*GRAT(I)
                  DGPVT(I)=(GRAP1(I))/RHO11*DR12T
     &                           +DAUXT*GRAT(I)
                  DGPVT(I)=DGPVT(I)-(GRAP1(I))*RHO12/RHO11/RHO11
     &                           *DR11T
               ENDIF
               DGPGP1(1)=RHO12/RHO11
               IF (YATE.EQ.1) THEN
                  DGPGT(1)=RHO12*(H12-H11)/T
               ENDIF     
 111        CONTINUE
         ENDIF
C  
C**********************************************************************
C CAS AVEC AIR DISSOUS
C  CALCUL DES DERIVEES DES MASSES VOLUMIQUES
         IF (THMC.EQ.'LIQU_AD_GAZ_VAPE') THEN
            DR11P1=RHO11*DP11P1*CLIQ
            DR11P2=RHO11*DP11P2*CLIQ
     
            DR12P1=RHO12/PVP*DP12P1
            DR12P2=RHO12/PVP*DP12P2
     
            DR21P1=MASRT*DP21P1
            DR21P2=MASRT*DP21P2
     
            DR22P1=MAMOLG/KH*DP21P1
            DR22P2=MAMOLG/KH*DP21P2
     
            IF (YATE.EQ.1) THEN
               DR11T=RHO11*CLIQ*DP11T-3.D0*ALPLIQ*RHO11
               DR12T=RHO12*(DP12T/PVP-1.D0/T)
               DR21T=MASRT*DP12T-RHO21/T
               DR22T = MAMOLG/KH*DP22T
            ENDIF
C
C **********************************************************************
C CALCUL DES DERIVEES DE GRADPVP ET GRADPAP
C
            DO 201 I=1,NDIM
               DGPVP1(I)=DP1PP2(1)*GRAP2(I)+DP1PP1(1)*GRAP1(I)
               DGPVP2(I)=DP2PP2(1)*GRAP2(I)+DP2PP1(1)*GRAP1(I)
               DGPAP1(I)=DP1PP2(2)*GRAP2(I)+DP1PP1(2)*GRAP1(I)
               DGPAP2(I)=DP2PP2(2)*GRAP2(I)+DP2PP1(2)*GRAP1(I)
               IF (YATE.EQ.1) THEN
                  DGPVP1(I)=DGPVP1(I)+DP1PT(1)*GRAT(I)
                  DGPVP2(I)=DGPVP2(I)+DP2PT(1)*GRAT(I)
                  DGPVT(I)= DTPP2(1)*GRAP2(I)+DTPP1(1)*GRAP1(I)
     &                      + DTPT(1)*GRAT(I)
                  DGPAP1(I)=DGPAP1(I)+DP1PT(2)*GRAT(I)
                  DGPAP2(I)=DGPAP2(I)+DP2PT(2)*GRAT(I)
                  DGPAT(I)= DTPP2(2)*GRAP2(I)+DTPP1(2)*GRAP1(I)
     &                      + DTPT(2)*GRAT(I)
               ENDIF
               DGPGP1(1)=DP12P1
               DGPGP2(1)=DP12P2
               DGPGP1(2)=DP22P1
               DGPGP2(2)=DP22P2
               IF (YATE.EQ.1) THEN
                  DGPGT(1)=DP12T
                  DGPGT(2)=DP22T
               ENDIF
C **********************************************************************
C DERIVEES DE GRADCVP ET DE GRADCAD
C
               DGCVP1(I)=DGPVP1(I)/P2
     &                     -GRAP2(I)/P2/P2*DP12P1
               DGCVP2(I)=DGPVP2(I)/P2
     &                     -GP(I)/P2/P2-GRAP2(I)/P2/P2*DP12P2
     &                     +2.D0*PVP*GRAP2(I)/P2/P2/P2
               DGCAP1(I)=MAMOLG*DGPAP1(I)/R/T
               DGCAP2(I)=MAMOLG*DGPAP2(I)/R/T
               IF (YATE.EQ.1) THEN
                  DGCVT(I)=DGPVT(I)/P2-GRAP2(I)/P2/P2*DP12T
                  DGCAP1(I)=DGCAP1(I)-MAMOLG*1/R/T/T*DP22P1*GRAT(I)
                  DGCAP2(I)=DGCAP2(I)-MAMOLG*1/R/T/T*DP22P2*GRAT(I)
                  DGCAT(I)=MASRT*DGPAT(I)-MAMOLG*1/R/T/T*DP22T*GRAT(I)
     &                 +MAMOLG*(2*1/R/T*PAD/T/T*GRAT(I)-1/R/T/T*GPA(I))
               ENDIF
               DGCGP1(1)=DGPGP1(1)/P2
               DGCGP2(1)=DGPGP2(1)/P2-PVP/P2/P2
               DGCGP1(2)=MAMOLG*1/R/T*DGPGP1(2)
               DGCGP2(2)=MAMOLG*1/R/T*DGPGP2(2)
               IF (YATE.EQ.1) THEN
                  DGCGT(1)=DGPGT(1)/P2
                  DGCGT(2)=MAMOLG*(1/R/T*DGPGT(2)-1/R/T*PAD/T)
               ENDIF     
 201        CONTINUE
         ENDIF
C 
C **********************************************************************
C CALCUL DES FLUX HYDRAULIQUES
C
      IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN 
         IF (((THMC.EQ.'LIQU_SATU').OR.(THMC.EQ.'GAZ')).OR.
     &        (THMC.EQ.'LIQU_GAZ_ATM')) THEN
            DO 102 I=1,NDIM
              IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
                 CONGEP(ADCP11+I)=RHO11*LAMBD1(1)
     &                         *(GRAP1(I)+RHO11*PESA(I))*ISOT(I)
              ELSE 
                 CONGEP(ADCP11+I)=RHO11*LAMBD1(1)
     &                         *(-GRAP1(I)+RHO11*PESA(I))*ISOT(I)
              ENDIF
 102        CONTINUE
         ENDIF
         IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
            DO 103 I=1,NDIM
               CONGEP(ADCP11+I)=RHO11*LAMBD1(1)*ISOT(I)
     &                          *(-GRAP2(I)+GRAP1(I)+RHO11*PESA(I))
               CONGEP(ADCP12+I)=RHO12*LAMBD2(1)*ISOT(I)
     &                          *(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
     &                          -RHO12*(1.D0-CVP)*FV(1)*GC(I)
               CONGEP(ADCP21+I)=RHO21*LAMBD2(1)*ISOT(I)
     &                          *(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
     &                          +RHO21*CVP*FV(1)*GC(I)
 103        CONTINUE
         ENDIF
         IF (THMC.EQ.'LIQU_AD_GAZ_VAPE') THEN
            DO 203 I=1,NDIM
               CONGEP(ADCP11+I)=RHO11*LAMBD1(1)*ISOT(I)
     &                      *(-GRAP2(I)+GRAP1(I)+(RHO11+RHO22)*PESA(I))
               CONGEP(ADCP12+I)=RHO12*LAMBD2(1)*ISOT(I)
     &                          *(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
     &                          -RHO12*(1.D0-CVP)*FV(1)*GC(I)
               CONGEP(ADCP21+I)=RHO21*LAMBD2(1)*ISOT(I)
     &                          *(-GRAP2(I)+(RHO12+RHO21)*PESA(I))
     &                          +RHO21*CVP*FV(1)*GC(I)
               CONGEP(ADCP22+I)=RHO22*LAMBD1(1)*ISOT(I)
     &                          *(GRAP1(I)-GRAP2(I)+(RHO22+RHO11)
     &                          *PESA(I))-FA(1)*GCA(I) 
 203        CONTINUE
         ENDIF
         IF (THMC.EQ.'LIQU_VAPE') THEN
            DO 113 I=1,NDIM
               CONGEP(ADCP11+I)=RHO11*LAMBD1(1)*ISOT(I)
     &                          *(-GRAP1(I)+RHO11*PESA(I))
               CONGEP(ADCP12+I)=RHO12*LAMBD2(1)*ISOT(I)
     &                          *(-GP(I)+RHO12*PESA(I))
 113        CONTINUE
         ENDIF
         IF (THMC.EQ.'LIQU_GAZ') THEN
            DO 104 I=1,NDIM
               CONGEP(ADCP11+I)=RHO11*LAMBD1(1)*ISOT(I)
     &                          *(-GRAP2(I)+GRAP1(I)+RHO11*PESA(I))
               CONGEP(ADCP21+I)=RHO21*LAMBD2(1)*ISOT(I)
     &                          *(-GRAP2(I)+RHO21*PESA(I))
 104        CONTINUE
         ENDIF
      ENDIF
C
C
      IF ((OPTION(1:9).EQ.'RIGI_MECA').OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
         IF ((THMC.EQ.'LIQU_SATU').OR.(THMC.EQ.'GAZ').OR.
     &        (THMC.EQ.'LIQU_GAZ_ATM')) THEN
            DO 108 I=1,NDIM
               IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
                  DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &             +DR11P1*LAMBD1(1)*ISOT(I)*(GRAP1(I)+RHO11*PESA(I))
                  DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &             +RHO11*LAMBD1(3)*ISOT(I)*(GRAP1(I)+RHO11*PESA(I))
                  DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &             +RHO11*LAMBD1(1)*ISOT(I)*(DR11P1*PESA(I))
                  DSDE(ADCP11+I,ADDEP1+I)=DSDE(ADCP11+I,ADDEP1+I)
     &             +RHO11*LAMBD1(1)*ISOT(I)
               ELSE
                 DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &             +DR11P1*LAMBD1(1)*ISOT(I)*(-GRAP1(I)+RHO11*PESA(I))
                 DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &             +RHO11*LAMBD1(3)*ISOT(I)*(-GRAP1(I)+RHO11*PESA(I))
                 DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &             +RHO11*LAMBD1(1)*ISOT(I)*(DR11P1*PESA(I))
                 DSDE(ADCP11+I,ADDEP1+I)=DSDE(ADCP11+I,ADDEP1+I)
     &             -RHO11*LAMBD1(1)*ISOT(I)
               ENDIF
               IF (YAMEC.EQ.1) THEN
                  DO 107 J=1,3
                     IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
                        DSDE(ADCP11+I,ADDEME+NDIM-1+I)=
     &                     DSDE(ADCP11+I,ADDEME+NDIM-1+I)
     &                     +RHO11*LAMBD1(2)*ISOT(I)*
     &                      (GRAP1(I)+RHO11*PESA(I))
                     ELSE
                       DSDE(ADCP11+I,ADDEME+NDIM-1+I)=
     &                     DSDE(ADCP11+I,ADDEME+NDIM-1+I)
     &                     +RHO11*LAMBD1(2)*ISOT(I)
     &                     *(-GRAP1(I)+RHO11*PESA(I))
                     ENDIF                        
 107              CONTINUE
               ENDIF
C       
               IF (YATE.EQ.1) THEN 
                  IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
                    DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &               +DR11T*LAMBD1(1)*ISOT(I)*(GRAP1(I)+RHO11*PESA(I))
                    DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &               +RHO11*LAMBD1(5)*ISOT(I)*(GRAP1(I)+RHO11*PESA(I))
                    DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &               +RHO11*LAMBD1(1)*ISOT(I)*(DR11T*PESA(I))
                  ELSE
                    DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &               +DR11T*LAMBD1(1)*ISOT(I)*(-GRAP1(I)+RHO11*PESA(I))
                    DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &               +RHO11*LAMBD1(5)*ISOT(I)*(-GRAP1(I)+RHO11*PESA(I))
                    DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &               +RHO11*LAMBD1(1)*ISOT(I)*(DR11T*PESA(I))
                  ENDIF        
               ENDIF
 108        CONTINUE
         ENDIF
C
         IF (THMC.EQ.'LIQU_VAPE_GAZ'.OR.THMC.EQ.'LIQU_AD_GAZ_VAPE'.OR.
     >       THMC.EQ.'LIQU_GAZ') THEN
            DO 105 I=1,NDIM
C     
C DERIVEE DU FLUX LIQUIDE
C
               DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &         +DR11P1*LAMBD1(1)*ISOT(I)*
     &         (-GRAP2(I)+GRAP1(I)+(RHO22+RHO11)*PESA(I))
               DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &           +RHO11*LAMBD1(3)*ISOT(I)*
     &           (-GRAP2(I)+GRAP1(I)+(RHO22+RHO11)*PESA(I))
               DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &           +RHO11*LAMBD1(1)*ISOT(I)*((DR22P1+DR11P1)*PESA(I))
               DSDE(ADCP11+I,ADDEP2)=DSDE(ADCP11+I,ADDEP2)
     &           +DR11P2*LAMBD1(1)*ISOT(I)*
     &           (-GRAP2(I)+GRAP1(I)+(RHO22+RHO11)*PESA(I))
               DSDE(ADCP11+I,ADDEP2)=DSDE(ADCP11+I,ADDEP2)
     &           +RHO11*LAMBD1(4)*ISOT(I)*
     &          (-GRAP2(I)+GRAP1(I)+(RHO22+RHO11)*PESA(I))
               DSDE(ADCP11+I,ADDEP2)=DSDE(ADCP11+I,ADDEP2)
     &           +RHO11*LAMBD1(1)*ISOT(I)*((DR22P2+DR11P2)*PESA(I))
               DSDE(ADCP11+I,ADDEP1+I)=DSDE(ADCP11+I,ADDEP1+I)
     &           +RHO11*LAMBD1(1)*ISOT(I)
               DSDE(ADCP11+I,ADDEP2+I)=DSDE(ADCP11+I,ADDEP2+I)
     &           -RHO11*LAMBD1(1)*ISOT(I)
C
C DERIVEE DU FLUX DE VAPEUR
C 
               IF ((THMC.EQ.'LIQU_VAPE_GAZ').OR. 
     &          (THMC.EQ.'LIQU_AD_GAZ_VAPE'))THEN              
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           +DR12P1*LAMBD2(1)*ISOT(I)*
     &           (-GRAP2(I)+(RHO12+RHO21)*PESA(I))    
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           +RHO12*LAMBD2(3)*ISOT(I)*
     &           (-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           +RHO12*LAMBD2(1)*ISOT(I)*
     &           ((DR12P1+DR21P1)*PESA(I))
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           -DR12P1*(1.D0-CVP)*FV(1)*GC(I)
               DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           +RHO12*(DCVP1)*FV(1)*GC(I)
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           -RHO12*(1.D0-CVP)*FV(3)*GC(I)
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           -RHO12*(1.D0-CVP)*FV(1)*DGCVP1(I)
                DSDE(ADCP12+I,ADDEP2)=DSDE(ADCP12+I,ADDEP2)
     &           +DR12P2*LAMBD2(1)*ISOT(I)*
     &           (-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                DSDE(ADCP12+I,ADDEP2)=DSDE(ADCP12+I,ADDEP2)
     &           +RHO12*LAMBD2(4)*ISOT(I)*
     &           (-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                DSDE(ADCP12+I,ADDEP2)=DSDE(ADCP12+I,ADDEP2)
     &           +RHO12*LAMBD2(1)*ISOT(I)*
     &           ((DR12P2+DR21P2)*PESA(I))
                DSDE(ADCP12+I,ADDEP2)=DSDE(ADCP12+I,ADDEP2)
     &           -DR12P2*(1.D0-CVP)*FV(1)*GC(I)
                DSDE(ADCP12+I,ADDEP2)=DSDE(ADCP12+I,ADDEP2)
     &           +RHO12*(DCVP2)*FV(1)*GC(I)
                DSDE(ADCP12+I,ADDEP2)=DSDE(ADCP12+I,ADDEP2)
     &           -RHO12*(1.D0-CVP)*FV(4)*GC(I)
                DSDE(ADCP12+I,ADDEP2)=DSDE(ADCP12+I,ADDEP2)
     &           -RHO12*(1.D0-CVP)*FV(1)*DGCVP2(I)
                DSDE(ADCP12+I,ADDEP1+I)=DSDE(ADCP12+I,ADDEP1+I)
     &           -RHO12*(1.D0-CVP)*FV(1)*DGCGP1(1)
                DSDE(ADCP12+I,ADDEP2+I)=DSDE(ADCP12+I,ADDEP2+I)
     &           -RHO12*LAMBD2(1)*ISOT(I)
     &           -RHO12*(1.D0-CVP)*FV(1)*DGCGP2(1)
               ENDIF
C       
C DERIVEE DU FLUX D'AIR SEC
C
               DSDE(ADCP21+I,ADDEP1)=DSDE(ADCP21+I,ADDEP1)
     &           +DR21P1*LAMBD2(1)*ISOT(I)*
     &           (-GRAP2(I)+(RHO12+RHO21)*PESA(I))
               DSDE(ADCP21+I,ADDEP1)=DSDE(ADCP21+I,ADDEP1)
     &           +RHO21*LAMBD2(3)*ISOT(I)*
     &           (-GRAP2(I)+(RHO12+RHO21)*PESA(I))
               DSDE(ADCP21+I,ADDEP1)=DSDE(ADCP21+I,ADDEP1)
     &           +RHO21*LAMBD2(1)*ISOT(I)*
     &           ((DR12P1+DR21P1)*PESA(I))
               DSDE(ADCP21+I,ADDEP1)=DSDE(ADCP21+I,ADDEP1)
     &           +DR21P1*CVP*FV(1)*GC(I)
               DSDE(ADCP21+I,ADDEP1)=DSDE(ADCP21+I,ADDEP1)
     &           +RHO21*(DCVP1)*FV(1)*GC(I)
               DSDE(ADCP21+I,ADDEP1)=DSDE(ADCP21+I,ADDEP1)
     &           +RHO21*CVP*FV(3)*GC(I)
               DSDE(ADCP21+I,ADDEP1)=DSDE(ADCP21+I,ADDEP1)
     &           +RHO21*CVP*FV(1)*DGCVP1(I)
               DSDE(ADCP21+I,ADDEP2)=DSDE(ADCP21+I,ADDEP2)
     &           +DR21P2*LAMBD2(1)*ISOT(I)*
     &           (-GRAP2(I)+(RHO12+RHO21)*PESA(I))
               DSDE(ADCP21+I,ADDEP2)=DSDE(ADCP21+I,ADDEP2)
     &           +RHO21*LAMBD2(4)*ISOT(I)*
     &           (-GRAP2(I)+(RHO12+RHO21)*PESA(I))
               DSDE(ADCP21+I,ADDEP2)=DSDE(ADCP21+I,ADDEP2)
     &           +RHO21*LAMBD2(1)*ISOT(I)*
     &           ((DR12P2+DR21P2)*PESA(I))
               DSDE(ADCP21+I,ADDEP2)=DSDE(ADCP21+I,ADDEP2)
     &           +DR21P2*CVP*FV(1)*GC(I)
               DSDE(ADCP21+I,ADDEP2)=DSDE(ADCP21+I,ADDEP2)
     &           +RHO21*(DCVP2)*FV(1)*GC(I)
               DSDE(ADCP21+I,ADDEP2)=DSDE(ADCP21+I,ADDEP2)
     &           +RHO21*CVP*FV(4)*GC(I)
               DSDE(ADCP21+I,ADDEP2)=DSDE(ADCP21+I,ADDEP2)
     &           +RHO21*CVP*FV(1)*DGCVP2(I)
               DSDE(ADCP21+I,ADDEP1+I)=DSDE(ADCP21+I,ADDEP1+I)
     &           +RHO21*CVP*FV(1)*DGCGP1(1)
               DSDE(ADCP21+I,ADDEP2+I)=DSDE(ADCP21+I,ADDEP2+I)
     &           -RHO21*LAMBD2(1)*ISOT(I)
     &           +RHO21*CVP*FV(1)*DGCGP2(1)
C       
C DERIVEE DU FLUX D'AIR DISSOUS
C
               IF (THMC.EQ.'LIQU_AD_GAZ_VAPE')THEN 
                DSDE(ADCP22+I,ADDEP1)=DSDE(ADCP22+I,ADDEP1)
     &           +DR22P1*LAMBD1(1)*ISOT(I)*
     &          (-GRAP2(I)+GRAP1(I)+(RHO22+RHO11)*PESA(I)) 
                DSDE(ADCP22+I,ADDEP1)=DSDE(ADCP22+I,ADDEP1)
     &           +RHO22*LAMBD1(3)*ISOT(I)*(-GRAP2(I)+GRAP1(I)+
     &            (RHO22+RHO11)*PESA(I))
                DSDE(ADCP22+I,ADDEP1)=DSDE(ADCP22+I,ADDEP1)
     &           +RHO22*LAMBD1(1)*ISOT(I)*((DR22P1+DR11P1)*PESA(I))
C ici     
                DSDE(ADCP22+I,ADDEP1)=DSDE(ADCP22+I,ADDEP1)
     &           -FA(3)*GCA(I)
C ici     
                DSDE(ADCP22+I,ADDEP1)=DSDE(ADCP22+I,ADDEP1)
     &           -FA(1)*DGCAP1(I)
     
                DSDE(ADCP22+I,ADDEP2)=DSDE(ADCP22+I,ADDEP2)
     &           +DR22P2*LAMBD1(1)*ISOT(I)*
     &          (-GRAP2(I)+GRAP1(I)+(RHO22+RHO11)*PESA(I))
                DSDE(ADCP22+I,ADDEP2)=DSDE(ADCP22+I,ADDEP2)
     &           +RHO22*LAMBD1(4)*ISOT(I)*(-GRAP2(I)+GRAP1(I)+
     &           (RHO22+RHO11)*PESA(I))
                DSDE(ADCP22+I,ADDEP2)=DSDE(ADCP22+I,ADDEP2)
     &           +RHO22*LAMBD1(1)*ISOT(I)*((DR22P2+DR11P2)*PESA(I))
                DSDE(ADCP22+I,ADDEP2)=DSDE(ADCP22+I,ADDEP2)
     &           -FA(4)*GCA(I)
                DSDE(ADCP22+I,ADDEP2)=DSDE(ADCP22+I,ADDEP2)
     &           -FA(1)*DGCAP2(I)
                DSDE(ADCP22+I,ADDEP1+I)=DSDE(ADCP22+I,ADDEP1+I)
     &           +RHO22*LAMBD1(1)*ISOT(I)-FA(1)*DGCGP1(2)
                DSDE(ADCP22+I,ADDEP2+I)=DSDE(ADCP22+I,ADDEP2+I)
     &           -RHO22*LAMBD1(1)*ISOT(I)-FA(1)*DGCGP2(2)
               ENDIF
C
C TERMES COMPLEMENTAIRES DE MECANIQUE ET THERMIQUE
C      
               IF (YAMEC.EQ.1) THEN
                  DO 106 J=1,3
                     DSDE(ADCP11+I,ADDEME+NDIM-1+J)=
     &              DSDE(ADCP11+I,ADDEME+NDIM-1+J)
     &               +(RHO11+RHO22)*LAMBD1(2)*ISOT(I)
     &               *(-GRAP2(I)+GRAP1(I)+(RHO11+RHO22)*PESA(I))
                     DSDE(ADCP12+I,ADDEME+NDIM-1+J)=
     &              DSDE(ADCP12+I,ADDEME+NDIM-1+J)
     &                +RHO12*LAMBD2(2)*ISOT(I)*
     &                (-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                     DSDE(ADCP12+I,ADDEME+NDIM-1+J)=
     &              DSDE(ADCP12+I,ADDEME+NDIM-1+J)
     &                 -RHO12*(1.D0-CVP)*FV(2)*GC(I)
                     DSDE(ADCP21+I,ADDEME+NDIM-1+J)=
     &              DSDE(ADCP21+I,ADDEME+NDIM-1+J)
     &                +RHO21*LAMBD2(2)*ISOT(I)*
     &                (-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                     DSDE(ADCP21+I,ADDEME+NDIM-1+J)=
     &              DSDE(ADCP21+I,ADDEME+NDIM-1+J)
     &                +RHO21*CVP*FV(2)*GC(I)
                     IF (THMC.EQ.'LIQU_AD_GAZ_VAPE')THEN 
                       DSDE(ADCP22+I,ADDEME+NDIM-1+J)=
     &                DSDE(ADCP22+I,ADDEME+NDIM-1+J)
     &                  +RHO22*LAMBD1(2)*ISOT(I)*(-GRAP2(I)+GRAP1(I)
     &                  +(RHO22+RHO11)*PESA(I))
                       DSDE(ADCP22+I,ADDEME+NDIM-1+J)=
     &                DSDE(ADCP22+I,ADDEME+NDIM-1+J)
     &                   -FA(2)*GCA(I)
                     ENDIF
 106              CONTINUE
C           
               ENDIF
C       
               IF (YATE.EQ.1) THEN
                  DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &              +DR11T*LAMBD1(1)*ISOT(I)*
     &             (-GRAP2(I)+GRAP1(I)+(RHO22+RHO11)*PESA(I))
                  DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &              +RHO11*LAMBD1(5)*ISOT(I)*
     &             (-GRAP2(I)+GRAP1(I)+(RHO22+RHO11)*PESA(I))
                  DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &              +RHO11*LAMBD1(1)*ISOT(I)*((DR22T+DR11T)*PESA(I))
     
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              +DR12T*LAMBD2(1)*ISOT(I)*
     &              (-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              +RHO12*LAMBD2(5)*ISOT(I)*
     &              (-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              +RHO12*LAMBD2(1)*ISOT(I)*((DR12T+DR21T)*PESA(I))
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              -DR12T*(1.D0-CVP)*FV(1)*GC(I)
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              +RHO12*DCVT*FV(1)*GC(I)
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              -RHO12*(1.D0-CVP)*FV(5)*GC(I)
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              -RHO12*(1.D0-CVP)*FV(1)*DGCVT(I)
                  DSDE(ADCP12+I,ADDETE+I)=DSDE(ADCP12+I,ADDETE+I)
     &              -RHO12*(1.D0-CVP)*FV(1)*DGCGT(1)
     
                  DSDE(ADCP21+I,ADDETE)=DSDE(ADCP21+I,ADDETE)
     &              +DR21T*LAMBD2(1)*ISOT(I)*
     &              (-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                  DSDE(ADCP21+I,ADDETE)=DSDE(ADCP21+I,ADDETE)
     &              +RHO21*LAMBD2(5)*ISOT(I)*
     &              (-GRAP2(I)+(RHO12+RHO21)*PESA(I))
                  DSDE(ADCP21+I,ADDETE)=DSDE(ADCP21+I,ADDETE)
     &              +RHO21*LAMBD2(1)*ISOT(I)*
     &              ((DR12T+DR21T)*PESA(I))
                  DSDE(ADCP21+I,ADDETE)=DSDE(ADCP21+I,ADDETE)
     &              +DR21T*CVP*FV(1)*GC(I)
                  DSDE(ADCP21+I,ADDETE)=DSDE(ADCP21+I,ADDETE)
     &              +RHO21*DCVT*FV(1)*GC(I)
                  DSDE(ADCP21+I,ADDETE)=DSDE(ADCP21+I,ADDETE)
     &              +RHO21*CVP*FV(5)*GC(I)
                  DSDE(ADCP21+I,ADDETE)=DSDE(ADCP21+I,ADDETE)
     &              +RHO21*CVP*FV(1)*DGCVT(I)
                  DSDE(ADCP21+I,ADDETE+I)=DSDE(ADCP21+I,ADDETE+I)
     &              +RHO21*CVP*FV(1)*DGCGT(1)
     
                  IF (THMC.EQ.'LIQU_AD_GAZ_VAPE')THEN 
                    DSDE(ADCP22+I,ADDETE)=DSDE(ADCP22+I,ADDETE)
     &                +DR22T*LAMBD1(1)*ISOT(I)*
     &                (-GRAP2(I)+GRAP1(I)+(RHO22+RHO11)*PESA(I))
                    DSDE(ADCP22+I,ADDETE)=DSDE(ADCP22+I,ADDETE)
     &                +RHO22*LAMBD1(5)*ISOT(I)*(-GRAP2(I)+GRAP1(I)+
     &              (RHO22+RHO11)*PESA(I))
                    DSDE(ADCP22+I,ADDETE)=DSDE(ADCP22+I,ADDETE)
     &                +RHO22*LAMBD1(1)*ISOT(I)*((DR22T+DR11T)*PESA(I))
                    DSDE(ADCP22+I,ADDETE)=DSDE(ADCP22+I,ADDETE)
     &                -FA(5)*GCA(I)
                    DSDE(ADCP22+I,ADDETE)=DSDE(ADCP22+I,ADDETE)
     &                -FA(1)*DGCAT(I)
                    DSDE(ADCP22+I,ADDETE+I)=DSDE(ADCP22+I,ADDETE+I)
     &                -FA(1)*DGCGT(2)
                  ENDIF
               ENDIF
C            
 105        CONTINUE
         ENDIF
C

C
         IF (THMC.EQ.'LIQU_VAPE') THEN
            DO 115 I=1,NDIM
C     
C DERIVEE DU FLUX LIQUIDE
C
               DSDE(ADCP11+I,ADDEP1)=DSDE(ADCP11+I,ADDEP1)
     &           +DR11P1*LAMBD1(1)*ISOT(I)*(-GRAP1(I)+RHO11*PESA(I))
     &           +RHO11*LAMBD1(1)*ISOT(I)*(DR11P1*PESA(I))
     &           +RHO11*LAMBD1(3)*ISOT(I)*(-GRAP1(I)+RHO11*PESA(I))
               DSDE(ADCP11+I,ADDEP1+I)=DSDE(ADCP11+I,ADDEP1+I)
     &           -RHO11*LAMBD1(1)*ISOT(I)
C
C DERIVEE DU FLUX DE VAPEUR
C 
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           +DR12P1*LAMBD2(1)*ISOT(I)*(-GP(I)+RHO12*PESA(I))    
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           +RHO12*LAMBD2(3)*ISOT(I)*(-GP(I)+RHO12*PESA(I))
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           +RHO12*LAMBD2(1)*ISOT(I)*((DR12P1)*PESA(I))
                DSDE(ADCP12+I,ADDEP1)=DSDE(ADCP12+I,ADDEP1)
     &           -RHO12*LAMBD2(1)*ISOT(I)*DGPVP1(I)
                DSDE(ADCP12+I,ADDEP1+I)=DSDE(ADCP12+I,ADDEP1+I)
     &           -RHO12*LAMBD2(1)*ISOT(I)*DGPGP1(1)
             
C       
C
C TERMES COMPLEMENTAIRES DE MECANIQUE ET THERMIQUE
C      
               IF (YAMEC.EQ.1) THEN
                  DO 116 J=1,3
                     DSDE(ADCP11+I,ADDEME+NDIM-1+J)=
     &               DSDE(ADCP11+I,ADDEME+NDIM-1+J)
     &               +RHO11*LAMBD1(2)*ISOT(I)*(-GRAP1(I)+RHO11*PESA(I))
                      DSDE(ADCP12+I,ADDEME+NDIM-1+J)=
     &               DSDE(ADCP12+I,ADDEME+NDIM-1+J)
     &                +RHO12*LAMBD2(2)*ISOT(I)*(-GP(I)+RHO12*PESA(I))
 116              CONTINUE
C           
               ENDIF
C       
               IF (YATE.EQ.1) THEN
                  DSDE(ADCP11+I,ADDETE)=DSDE(ADCP11+I,ADDETE)
     &              +DR11T*LAMBD1(1)*ISOT(I)*(-GRAP1(I)+RHO11*PESA(I))
     &              +RHO11*LAMBD1(5)*ISOT(I)*(-GRAP1(I)+RHO11*PESA(I))
     &              +RHO11*LAMBD1(1)*ISOT(I)*DR11T*PESA(I)
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              +DR12T*LAMBD2(1)*ISOT(I)*(-GP(I)+RHO12*PESA(I))
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              +RHO12*LAMBD2(5)*ISOT(I)*(-GP(I)+RHO12*PESA(I))
                  DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &              +RHO12*LAMBD2(1)*ISOT(I)*((DR12T)*PESA(I))
                DSDE(ADCP12+I,ADDETE)=DSDE(ADCP12+I,ADDETE)
     &           -RHO12*LAMBD2(1)*ISOT(I)*DGPVT(I)
                DSDE(ADCP12+I,ADDETE+I)=DSDE(ADCP12+I,ADDETE+I)
     &           -RHO12*LAMBD2(1)*ISOT(I)*DGPGT(1)
               ENDIF
C            
 115        CONTINUE
         ENDIF
C
      ENDIF
C
      END
