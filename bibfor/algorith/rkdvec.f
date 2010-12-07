        SUBROUTINE RKDVEC( FAMI,KPG,KSP,MOD,   IMAT,  MATCST,
     &                     NVI,   VINI,  COEFT,
     &                     E, NU, ALPHA, X,     DTIME,NMAT,COEL,SIGI,
     &                     EPSD, DETOT,
     &                     DVIN)
        IMPLICIT REAL*8(A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/12/2010   AUTEUR GENIAUT S.GENIAUT 
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
C TOLE CRS_505
C TOLE CRS_506
C ==================================================================
C      MODELE VISCOPLASTIQUE A ECROUISSAGE ISOTRROPE COUPLE A DE
C      L ENDOMMAGEMENT ISOTROPE
C ==================================================================
C INTEGRATION DE LA LOI (VENDOCHAB) PAR UNE METHODE DE RUNGE KUTTA
C
C     CETTE ROUTINE FOURNIT LA DERIVEE DE L ENSEMBLE DES VARIABLES
C     INTERNES DU MODELE
C     ----------------------------------------------------------------
      CHARACTER*8 MOD
      CHARACTER*2     CERR
      CHARACTER*3     MATCST
      CHARACTER*(*)   FAMI
      INTEGER         IMAT
      INTEGER         KPG,KSP,NDT
      REAL*8          VPAR(2)
      CHARACTER*8     NOMCOE,NOMPAR(2)
      REAL*8 E, NU, ALPHA
      REAL*8 X, DTIME, HSDT
      REAL*8 TPERD, DTPERD, TEMP,TF
      REAL*8 COEFT(NMAT),COEL(NMAT)
      REAL*8 VINI(NVI)
      REAL*8 DVIN(NVI)
      REAL*8 SEDVP,  CRITV,      E0,         EPSIEC
      REAL*8 SMX(6), SIGI(6),    EPSD(6),    DETOT(6)
      REAL*8 EVI(6),    ECROU,    DMG
      REAL*8 DEVI(6),DEVCUM,   DECROU,   DDMG
      REAL*8 ZE, TD, VALP(3)
      REAL*8 S, ALPHA_D, BETA_D, N, UNSMVP, KVP, RD, AD , KD
C     ----------------------------------------------------------------
      PARAMETER(ZE=0.0D0)
      PARAMETER(TD=1.5D0)
      PARAMETER ( EPSIEC = 1.D-8 )
      PARAMETER ( EPSI = 1.D-15 )
C
      COMMON /TDIM/   NDT,    NDI
C     ----------------------------------------------------------------

C
C --    COEFFICIENTS MATERIAU INELASTIQUES
C
      S    = COEFT(4)
      ALPHA_D = COEFT(5)
      BETA_D = COEFT(6)
      N    = COEFT(1)
      UNSMVP    = COEFT(2)
      KVP    = (1.D0/COEFT(3))
      RD     = COEFT(7)
      AD     = COEFT(8)
C     ----------------------------------------------------------------
C     KD VA ETRE EXTRAIT PLUS LOIN UNE FOIS QUE SEDVP
C     (CONTRAINTE EQUIVALENTE D ENDOMMAGEMENT VISCOPLASTIQUE)
C     SERA CALCULEE
C     ----------------------------------------------------------------
C
C --  VARIABLES INTERNES
C
      DO 5 ITENS=1,6
        EVI(ITENS) = VINI(ITENS)
    5 CONTINUE
        ECROU = VINI(8)
C     D---------------------18/06/96----------------------------
      IF (ECROU.LE.EPSIEC) THEN
        ECROU=EPSIEC
      ENDIF
C     F---------------------18/06/96----------------------------
        DMG  = VINI(9)
C
C----------------------------------------------------------------
      E0=E
      E=E0*(1.D0-DMG)
        CALL CALSIG(FAMI,KPG,KSP,EVI,MOD,E,NU,ALPHA,X,DTIME,EPSD,
     &              DETOT,NMAT,COEL,SIGI)
      E=E0
C
C----- VARIABLES INTERNES
      TRSIG=(SIGI(1)+SIGI(2)+SIGI(3))
C----- CALCUL DE GRJ0(SIGI) : MAX DES CONTRAINTES PRINCIPALES
      IF (ALPHA_D.LE.(1.0D-15)) THEN
         GRJ0=0.0D0
      ELSE
         CALL CALCJ0(SIGI,GRJ0,VALP)
      ENDIF
C     -------------------------PROVISOIRE----------------------------
C----- CALCUL DE GRJ1(SIGI) : PREMIER INVARIANT (TRACE)
      GRJ1= TRSIG
C----- CALCUL DE GRJ2(SIGI) : SECOND INVARIANT (SIGEQ DE VON MISES)
      GRJ2V=0.0D0
      DO 10 ITENS=1,6
        SMX(ITENS)=SIGI(ITENS)
        IF (ITENS.LE.3) SMX(ITENS)=SMX(ITENS)-(GRJ1)/3.D0
        GRJ2V=GRJ2V+SMX(ITENS)**2
   10 CONTINUE
      GRJ2V=SQRT(1.5D0*GRJ2V)
C----- CALCUL DE SEDVP : CONTRAINTE EQUIVALENTE DE FLUAGE
      SEDVP=ALPHA_D*GRJ0+BETA_D*GRJ1+(1-ALPHA_D-BETA_D)*GRJ2V
C
C----- CALCUL DE KD A PARTIR DU MATERIAU CODE
C
      IF (MATCST.EQ.'NAP') THEN
C
        NOMCOE    = 'K_D     '
        NOMPAR(1) = 'TEMP    '
        NOMPAR(2) = 'X       '
C
C -- TEMPERATURE
C
        CALL RCVARC(' ','TEMP','-',FAMI,KPG,KSP,TPERD,IRET1)
        IF ( IRET1.NE.0 ) TPERD = 0.D0
        CALL RCVARC(' ','TEMP','+',FAMI,KPG,KSP,TF,IRET2)
        IF ( IRET2.NE.0 ) TF = 0.D0
        DTPERD = TF-TPERD
        
        HSDT=X/DTIME
        TEMP=TPERD+HSDT*DTPERD
C
        VPAR(1) = TEMP
        VPAR(2) = SEDVP
C
        CALL RCVALB(FAMI,1,1,'+',IMAT,' ', 'VENDOCHAB', 
     &              2,  NOMPAR, VPAR, 1,
     &              NOMCOE,  KD,  CERR, 'F ' )
      ELSE
        KD    = COEFT(9)
      ENDIF
C
C----- LA FONCTION SEUIL NE FAIT PAS APPARAITRE D INFLUENCE DE L
C----- ECROUISSAGE
      CRITV=GRJ2V-S*(1-DMG)
      IF (CRITV.LE.0.0D0) THEN
        DEVCUM=0.0D0
        DECROU=0.0D0
        DDMG=0.0D0
        DO 11 ITENS=1,6
          DEVI(ITENS)=0.0D0
   11   CONTINUE
      ELSE
C
C------ EQUATION DONNANT LA DERIVEE DE L ENDOMMAGEMENT
C
        DDMG=SEDVP/(AD)
        DOMCPL=MAX(EPSI,(1-DMG))
        DDMG=(MAX(0.D0,DDMG)**RD)*(1/(DOMCPL)**KD)
C
C------ EQUATION DONNANT LA DERIVEE DE L ECROUISSAGE
C
        DECROU=CRITV/((1-DMG)*KVP*ECROU**(UNSMVP))
        DECROU=MAX(0.D0,DECROU)
        DECROU=DECROU**N
C
C------ EQUATION DONNANT LA DERIVEE DE LA DEF VISCO PLAST
C------ CUMULEE
C
        DEVCUM=DECROU/(1-DMG)
C
C------ EQUATION DONNANT LA DERIVEE DE LA DEF VISCO PLAST
C
        DO 12 ITENS=1,6
           DEVI(ITENS)=TD*DEVCUM*SMX(ITENS)/GRJ2V
   12   CONTINUE
      END IF
C------ NE SERT A RIEN
        DETAT=ZE
C
C
C --    DERIVEES DES VARIABLES INTERNES
C
      DO 30 ITENS=1,6
        DVIN(ITENS)      = DEVI(ITENS)
   30 CONTINUE
        DVIN(7) = DEVCUM
        DVIN(8) = DECROU
        DVIN(9) = DDMG
        DVIN(10) = DETAT
C
      END
