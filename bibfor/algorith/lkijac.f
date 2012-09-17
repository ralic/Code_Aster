      SUBROUTINE LKIJAC (MOD,NMAT,MATERF,TIMED,TIMEF,YF,DEPS,NR,NVI,
     &                   VIND,VINF,YD,DY,DRDY,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/09/2012   AUTEUR FOUCAULT A.FOUCAULT 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE FOUCAULT A.FOUCAULT
C TOLE CRS_1404 CRP_20
      IMPLICIT   NONE
C     --------------------------------------------------------------
C     CALCUL DU JACOBIEN DE LETK = DRDY(DY)
C     IN  MOD    :  TYPE DE MODELISATION
C         NMAT   :  DIMENSION MATER
C         MATERF :  COEFFICIENTS MATERIAU A T+DT
C         YF	 :  VARIABLES A T + DT =  ( SIGF DLAMBDA XI_P XI_VP)
C         DEPS   :  INCREMENT DE DEFORMATION
C         TIMED  :  INSTANT  T
C         TIMEF  :  INSTANT  T+DT
C         NR	 :  DIMENSION DECLAREE DRDY
C         NVI	 :  NOMBRE DE VARIABLES INTERNES
C         VIND   :  VARIABLE INTERNES A T
C         VINF   :  VARIABLE INTERNES A T+DT
C         YD	 :  VARIABLES A T  = ( SIGD  0 XI_P XI_VP) A T
C         DY	 :  SOLUTION	   = ( DSIG  DLAMBDA  DXI_P DXI_VP )
C     OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
C         IRET   :  CODE RETOUR
C     --------------------------------------------------------------
      INTEGER         NR,NMAT,IRET,NVI
      REAL*8          DEPS(6),DRDY(NR,NR),YF(NR),DY(NR),YD(NR)
      REAL*8          MATERF(NMAT,2)
      REAL*8          TIMED,TIMEF,VIND(NVI),VINF(NVI)
      CHARACTER*8     MOD
C
      INTEGER         I,J,VARV,VALV,VALP,RETCOM,NDT,NDI
      REAL*8          SIGFT(6),DEPST(6),ZERO
      REAL*8          DEVGII,VINT(NVI),DT,DEVSIG(6),I1
      REAL*8          XIVMAX,XIPPIC,DSDENL(6,6),KK,MU
      REAL*8          UCRIV,SEUILV,DEPSV(6),DGAMV
      REAL*8          SEUILP,UCRIP,SEUIVM,DHDS(6),DS2HDS(6)
      REAL*8          PARAEP(3),VARPL(4),DFDSP(6),BPRIMP,LKBPRI
      REAL*8          VECNP(6),GP(6),VETEMP(6),DERPAR(3),DFDXIP
      REAL*8          UN,DEUX,TROIS,DEPSE(6),HOOK(6,6),MUE,KE
      REAL*8          DSIGE(6),VIDENT(6),DHOKDS(6,6),PATM,NELAS
      REAL*8          PARAVI(3),VARAVI(4),DFVDSI(6)
      REAL*8          DGPDS(6,6),DGVDS(6,6)
      REAL*8          DLDGDS(6,6),DLAMBD,HNLDGP(6,6),DSGVDS(6,6)
      REAL*8          HNLDGV(6,6),BPRIMV,VECNV(6),UCRIM,DEVGIV,GV(6)
      REAL*8          HNLGV(6),HNLDFG(6,6),PHIV,AV,NV,DPHIV
      REAL*8          DPHVDS(6),DR1DY3(6),DGPDXI(6),DFSDXP(6)
      REAL*8          TERM1,TERM2,TERM3
      REAL*8          DNDXIP(6),DFSDXV(6),DPADXP(3)
      REAL*8          DNDXIV(6),DFDXIV,DPADXV(3),DPHIDX
      REAL*8          DPHDXG(6),DGVDXI(6),PHDGDX(6)
      REAL*8          DR1DY4(6),DGIPDS(6),DGIVDS(6),DGIPDX,DGIVDX
      REAL*8          MIDENT(6,6),KRON(6),KRON2(6,6),UNSTRO
      REAL*8          DSDSIG(6,6),DGTVDS(6,6),DGTPDS(6,6),KRON3(6,6)
      REAL*8          DEVGP(6),DEVGV(6),DGTPDX(6),DGTVDX(6),DXIV
      REAL*8          R8PREM
      LOGICAL         PLAS
      PARAMETER       (ZERO  =  0.D0 )
      PARAMETER       (UN    =  1.D0 )
      PARAMETER       (DEUX  =  2.D0 )
      PARAMETER       (TROIS =  3.D0 )
C     --------------------------------------------------------------
      COMMON /TDIM/   NDT  , NDI
C     --------------------------------------------------------------
C ------------------------------------------------------------------
C --- PASSAGE EN CONVENTION MECANIQUE DES SOLS
C ------------------------------------------------------------------
      DO 10 I = 1, NDT
        SIGFT(I)  = -YF(I)
        DEPST(I)  = -DEPS(I)
  10  CONTINUE
C ------------------------------------------------------------------
C --- VARIABLES LOCALES TEMPORAIRES
C ------------------------------------------------------------------
      CALL LCINMA(ZERO,MIDENT)
      DO 20 I = 1, NDT
        MIDENT(I,I) = UN
  20  CONTINUE

      VARV   = 0
      DEVGII = ZERO
      DLAMBD = YF(NDT+1)
C --- VECTEUR VARIABLES INTERNES TEMPORAIRES
      CALL LCEQVN ( NVI  ,  VIND , VINT )
      IF(YF(NDT+2).GE.VIND(1))THEN
        VINT(1) = YF(NDT+2)
      ELSE
        VINT(1) = VIND(1)
      ENDIF
      IF(YF(NDT+3).GE.VIND(3))THEN
        VINT(3) = YF(NDT+3)
      ELSE
        VINT(3) = VIND(3)
      ENDIF
C --- INCREMENT DE TEMPS
      DT = TIMEF - TIMED
C --- CONSTRUCTION TENSEUR DEVIATOIRE DES CONTRAINTES ET 1ER INVARIA
      CALL LCDEVI(SIGFT,DEVSIG)
      I1 = SIGFT(1)+SIGFT(2)+SIGFT(3)
C --- DONNEES MATERIAU : VALEUR MAX DE XIV; XI_PIC 
      XIVMAX = MATERF(20,2)
      XIPPIC = MATERF(18,2)
C --- CONSTRUCTION TENSEUR ELASTIQUE NON LINEAIRE DSDENL
      CALL LKELAS ( NDI, NDT, MOD , NMAT, MATERF,
     &              DEPST, SIGFT, DSDENL, KK, MU)
C ------------------------------------------------------------------
C --- A) - BUT : CALCUL DE LA DEFORMATION VISQUEUSE -DEPSV- ET DU 
C ---      PARAMETRE D ECROUISSAGE VISQUEUX -DGAMV-
C ------------------------------------------------------------------
C --- A-1) INDICATEUR SUR ANGLE DE DILATANCE VISQUEUX PSI -> VAL = 0
      VALV = 0
C --- A-2) VARIABLE D'ECROUISSAGE VISQUEUSE VINTR = YF(NDT+3)
C --- A-3) CALCUL SEUIL VISQUEUX PAR RAPPORT A YF(1:6)=SIGF ->SEUILV
C --- A-3-1)  XIT   = YF(NDT+3)
      SEUILV = ZERO
      CALL LKCRIV(VINT(3),I1,DEVSIG,VINT,NMAT,MATERF,UCRIV,SEUILV)
      IF(SEUILV.GE.ZERO)THEN
        CALL LKDGDE(VALV,VINT(3),DT,SEUILV,UCRIV,I1,DEVSIG,VINT,
     &              NMAT,MATERF,DEPSV,DGAMV,RETCOM)
      ELSE
        DGAMV = ZERO
        DO 30 I = 1, NDT
          DEPSV(I) = ZERO
  30    CONTINUE
        SEUILV = ZERO
        UCRIV  = ZERO 
      ENDIF
C ------------------------------------------------------------------
C --- B) - BUT : CALCUL DE LA DEFORMATION PLASTIQUE -DEPSP- ET DU 
C ---       PARAMETRE D ECROUISSAGE PLASTIQUE -DGAMP-
C ------------------------------------------------------------------
      CALL LKDHDS(NMAT,MATERF,I1,DEVSIG,DHDS,RETCOM)
      CALL LKDS2H(NMAT,MATERF,I1,DEVSIG,DHDS,DS2HDS,RETCOM)
C --- B-1) CALCUL FONCTION SEUIL PLASTIQUE EN YF
      SEUILP = ZERO
      CALL LKCRIP(I1,DEVSIG,VINT,NMAT,MATERF,UCRIP,SEUILP)
C --- B-2)SI SEUILP >= 0 ALORS PLASTICITE A PRENDRE EN COMPTE 
      IF((SEUILP.GE.ZERO).OR.(VINF(7).GT.ZERO))THEN
C --- B-2-B-1) INDICATEUR ANGLE DE DILATANCE PLASTIQUE PSI -> 0 OU 1
        IF(YF(NDT+2).LE.XIPPIC)THEN
          VALP = 0
        ELSE
          VALP = 1
        ENDIF
C --- B-2-B-2) INDICATEUR CONTRACTANCE OU DILATANCE -> VARV = 0 OU 1
C --- B-2-B-2)-1) CALCUL POSITION YF PAR RAPPORT SEUIL VISQUEUX MAX
        SEUIVM = ZERO
        CALL LKCRIV(XIVMAX,I1,DEVSIG,VINT,NMAT,MATERF,UCRIM,SEUIVM)
C --- B-2-B-2)-2) TEST SUR SEUIL >0 OU <0 POUR DEFINIR VARV
        IF(SEUIVM.LE.ZERO)THEN
          VARV = 0
        ELSE
          VARV = 1
        ENDIF
C --- B-2-B-3) CALCUL DE DF/DSIG 
        CALL LKVARP(VINT, NMAT, MATERF, PARAEP)
        CALL LKVACP(NMAT, MATERF,PARAEP, VARPL)
        CALL LKDEPP(VINT, NMAT, MATERF, PARAEP, DERPAR)
        CALL LKDFDS(NMAT,MATERF,DEVSIG,PARAEP,VARPL,DS2HDS,
     &              UCRIP,DFDSP)
C --- B-2-B-4) CALCUL DE G_EP
        BPRIMP = LKBPRI (VALP,VINT,NMAT,MATERF,PARAEP,I1,DEVSIG)
        CALL LCINVE(ZERO,VECNP)
        CALL LKCALN(DEVSIG, BPRIMP, VECNP, RETCOM)
        CALL LKCALG(DFDSP,VECNP,GP,DEVGII)
C --- CALCUL DEFORMATION ELASTIQUE
        DO 40 I = 1, NDT
          DEPSE(I) = DEPST(I)-YF(NDT+1)*GP(I)-DEPSV(I) 
  40    CONTINUE
C --- CALCUL DE DGP/DSIGMA
        CALL LKDGDS(NMAT,MATERF,PARAEP,VARPL,DEVSIG,I1,VALP,DS2HDS,
     &              VECNP,DFDSP,BPRIMP,NVI,VINT,DHDS,DGPDS,IRET)
C --- PRODUIT MATRICIEL HOOK_NL*D_LAMBDA*DGP/DSIGMA     
        CALL LCPRSM(DLAMBD,DGPDS,DLDGDS)
        CALL LCPRMM(DSDENL,DLDGDS,HNLDGP)
C --- CALCUL DE D(DFPDSIG)/DXI
        PLAS = .TRUE.
        CALL LKFSXI(NMAT,MATERF,I1,DEVSIG,DS2HDS,PLAS,VINT(1),
     &              PARAEP,VARPL,DFSDXP,DPADXP)
C --- CALCUL DE DN/DXI
        CALL LKDNDX(NMAT,MATERF,I1,DEVSIG,BPRIMP,VALP,PARAEP,
     &              VINT(1),DERPAR,DNDXIP)
C --- PAS DE PLASTICITE A GERER
      ELSE
        DO 50 I = 1, NDT
          DEPSE(I) = DEPST(I)-DEPSV(I) 
  50    CONTINUE
        CALL LCINMA(ZERO,HNLDGP)
        CALL LCINMA(ZERO,DGPDS)
        CALL LCINVE(ZERO,DFDSP)
        CALL LCINVE(ZERO,GP)
        CALL LCINVE(ZERO,VECNP)
        CALL LCINVE(ZERO,DFSDXP)
        CALL LCINVE(ZERO,DNDXIP)
        DEVGII = ZERO
      ENDIF
C ##################################################################
C --- CALCUL DE DR1/DY                        
C ##################################################################
C ------------------------------------------------------------------
C --- I.1 CALCUL DE DR1DY1 -> Y1 = SIGMA
C ------------------------------------------------------------------
C --- CONSTRUCTION TENSEUR ELASTIQUE LINEAIRE
      MUE = MATERF(4,1)
      KE  = MATERF(5,1)
      CALL LCINMA(ZERO,HOOK)
      DO 110 I = 1,NDI
        DO 120 J = 1,NDI
          HOOK(I,J) = KE - DEUX*MUE/TROIS
 120    CONTINUE
 110  CONTINUE

      DO 130 I = 1,NDT
        HOOK(I,I) = HOOK(I,I) + DEUX*MUE
 130  CONTINUE
C --- INCREMENT CONTRAINTE "ELASTIQUE"
      CALL LCPRMV(HOOK,DEPSE,DSIGE)
C --- PRODUIT TENSORIEL DSIGE X VECTEUR(IDENTITE) (=1 1 1 0 0 0)
      PATM  = MATERF(1,2)
      NELAS = MATERF(2,2)
      CALL LCINVE(ZERO,VIDENT)
      DO 150 I = 1, NDI
        VIDENT(I) = NELAS/TROIS/PATM*(I1/(TROIS*PATM))**(NELAS-UN)
 150  CONTINUE
      CALL LCPRTE(DSIGE,VIDENT,DHOKDS)
C --- CALCUL DE DFV/DSIGMA
      CALL LKVARV(VINT(3),NMAT, MATERF, PARAVI)
      CALL LKVACV(NMAT, MATERF, PARAVI, VARAVI)      
      BPRIMV = LKBPRI (VALV,VINT,NMAT,MATERF,PARAVI,I1,DEVSIG)
      CALL LKCALN(DEVSIG, BPRIMV, VECNV, RETCOM)
      CALL LKDFDS(NMAT,MATERF,DEVSIG,PARAVI,VARAVI,DS2HDS,
     &            UCRIV,DFVDSI)
C --- CALCUL DE G_VISQUEUX
      CALL LKCALG(DFVDSI,VECNV,GV,DEVGIV)
C --- CALCUL DE DGV/DSIGMA
      CALL LKDGDS(NMAT,MATERF,PARAVI,VARAVI,DEVSIG,I1,VALV,
     &     DS2HDS,VECNV,DFVDSI,BPRIMV,NVI,VINT,DHDS,DGVDS,IRET)
C --- PRODUIT MATRICIEL HOOK_NL*PHIV*DGV/DSIGMA     
      AV   = MATERF(21,2)
      NV   = MATERF(22,2)
      PHIV = AV * (SEUILV/PATM)**NV
      CALL LCPRSM(PHIV,DGVDS,DSGVDS)
      CALL LCPRMM(DSDENL,DSGVDS,HNLDGV)
C --- PRODUIT MATRICIEL HOOK_NL*DPHIV/DSIG*GV     
      DPHIV = AV*NV/PATM*(SEUILV/PATM)**(NV-UN)
      CALL LCPRSV(DPHIV,DFVDSI,DPHVDS)
      CALL LCPRMV(DSDENL,GV,HNLGV)
      CALL LCPRTE(HNLGV,DPHVDS,HNLDFG)
C --- ASSEMBLAGE FINAL
      DO 160 I = 1, NDT
        DO 170 J = 1, NDT
          DRDY(I,J) = -(MIDENT(I,J)-DHOKDS(I,J)+HNLDGP(I,J)
     &                +HNLDGV(I,J)*DT+HNLDFG(I,J)*DT)/MU
 170    CONTINUE
 160  CONTINUE
C ------------------------------------------------------------------
C --- I.2 CALCUL DE DR1DY2 -> Y2 = DLAMBDA
C ------------------------------------------------------------------
      CALL LCPRMV(DSDENL,GP,VETEMP)
      DO 180 I = 1, NDT 
        DRDY(I,NDT+1) = VETEMP(I)/MU
 180  CONTINUE
C ------------------------------------------------------------------
C --- I.3 CALCUL DE DR1DY3 -> Y3 = XIP
C ------------------------------------------------------------------
C --- ASSEMBLAGE DE DGPDXI = 
C D(DFPDSIG)/DXI-D(DFPDSIG)/DXI.N*N-DFPDSIG.DNDXI*N-DFPDSIG.N*DNDXI
      CALL LCPRSC(DFSDXP,VECNP,TERM1)
      CALL LCPRSC(DFDSP,DNDXIP,TERM2)
      CALL LCPRSC(DFDSP,VECNP,TERM3)
      DO 190 I = 1, NDT
        DGPDXI(I) = DFSDXP(I)-TERM1*VECNP(I)-TERM2*VECNP(I)
     &              -TERM3*DNDXIP(I)
 190  CONTINUE
C --- ASSEMBLAGE FINAL --- DR1DY3 = DSDENL*DLAMBD*DGPDXI
      CALL LCPRMV(DSDENL,DGPDXI,DR1DY3)
      CALL LCPRSV(DLAMBD,DR1DY3,DR1DY4)
      DO 200 I = 1, NDT
        DRDY(I,NDT+2) = DR1DY4(I)/MU
 200  CONTINUE     
C ------------------------------------------------------------------
C --- I.4 CALCUL DE DR1DY4 -> Y4 = XIVP
C ------------------------------------------------------------------
      DXIV = MIN(DGAMV,XIVMAX-YD(NDT+3))
      IF(ABS(DXIV-DGAMV).LT.R8PREM())THEN
C --- CALCUL DE D(DFVDSIG)/DXIV
        PLAS = .FALSE.
        CALL LKFSXI(NMAT,MATERF,I1,DEVSIG,DS2HDS,PLAS,VINT(3),
     &            PARAVI,VARAVI,DFSDXV,DPADXV)
C --- CALCUL DE DN/DXI
        CALL LKDNDX(NMAT,MATERF,I1,DEVSIG,BPRIMV,VALV,PARAVI,
     &            VINT(3),DPADXV,DNDXIV)
C --- ASSEMBLAGE DE DGVDXIV = 
        CALL LCPRSC(DFSDXV,VECNV,TERM1)
        CALL LCPRSC(DFVDSI,DNDXIV,TERM2)
        CALL LCPRSC(DFVDSI,VECNV,TERM3)
        DO 210 I = 1, NDT
          DGVDXI(I) = DFSDXV(I)-TERM1*VECNV(I)-TERM2*VECNV(I)
     &                -TERM3*DNDXIV(I)
 210    CONTINUE
C --- CALCUL DE D(PHIV)/DXIV = 
        CALL LKDFDX(NMAT,MATERF,UCRIV,I1,DEVSIG,PARAVI,VARAVI,
     &            DPADXV,DFDXIV)
C --- ASSEMBLAGE DE DR1DY4
        DPHIDX = DPHIV*DFDXIV
        CALL LCPRSV(DPHIDX,GV,DPHDXG)
        CALL LCPRSV(PHIV,DGVDXI,PHDGDX)
        CALL LCSOVE(DPHDXG,PHDGDX,VETEMP)
        CALL LCPRMV(DSDENL,VETEMP,DR1DY4)
        DO 220 I = 1, NDT
          DRDY(I,NDT+3)= DR1DY4(I)/MU*DT
 220    CONTINUE
      ELSE
        DO 230 I = 1, NDT
          DRDY(I,NDT+3)= ZERO
 230    CONTINUE
      ENDIF
C ##################################################################
C --- CALCUL DE DR2/DY                        
C ##################################################################
C --- APPLICATION DE LA CONDITION DE KHUN-TUCKER SUR R(NDT+1)
      IF((SEUILP.LT.ZERO).AND.(VINF(7).EQ.ZERO))THEN
C ------------------------------------------------------------------
C --- II.1 CALCUL DE DR2DY1 -> Y1 = SIGMA
C ------------------------------------------------------------------
        DO 240 I = 1, NDT
          DRDY(NDT+1,I) = ZERO
 240    CONTINUE
C ------------------------------------------------------------------
C --- II.2 CALCUL DE DR2DY2 -> Y2 = DLAMBDA
C ------------------------------------------------------------------
        DRDY(NDT+1,NDT+1) = UN
C ------------------------------------------------------------------
C --- II.3 CALCUL DE DR2DY3 -> Y3 = XIP
C ------------------------------------------------------------------
        DRDY(NDT+1,NDT+2) = ZERO
      ELSE
C ------------------------------------------------------------------
C --- II.1 CALCUL DE DR2DY1 -> Y1 = SIGMA
C ------------------------------------------------------------------
        DO 250 I = 1, NDT
          DRDY(NDT+1,I) = -DFDSP(I)/MU
 250    CONTINUE
C ------------------------------------------------------------------
C --- II.2 CALCUL DE DR2DY2 -> Y2 = DLAMBDA
C ------------------------------------------------------------------
        DRDY(NDT+1,NDT+1) = ZERO
C ------------------------------------------------------------------
C --- II.3 CALCUL DE DR2DY3 -> Y3 = XIP
C ------------------------------------------------------------------
C --- RECUPERATION DE DF/DXIP -------------------------------------
        CALL LKDFDX(NMAT,MATERF,UCRIP,I1,DEVSIG,PARAEP,VARPL,
     &            DERPAR,DFDXIP)
        DRDY(NDT+1,NDT+2) = DFDXIP/MU
      ENDIF
C ------------------------------------------------------------------
C --- II.4 CALCUL DE DR2DY4 -> Y4 = XIVP
C ------------------------------------------------------------------
      DRDY(NDT+1,NDT+3) = ZERO
C ##################################################################
C --- CALCUL DE DR3/DY                        
C ##################################################################
C ------------------------------------------------------------------
C --- III.1 CALCUL DE DR3DY1 -> Y1 = SIGMA
C ------------------------------------------------------------------
C --- CONSTRUCTION DE KRONECKER
      CALL LCINVE(ZERO,KRON)
      DO 300 I = 1, NDI
        KRON(I) = UN
 300  CONTINUE
C --- CONSTRUCTION DE DS/DSIGMA
      UNSTRO = UN / TROIS
      CALL LCPRTE(KRON,KRON,KRON2)
      CALL LCPRSM(UNSTRO,KRON2,KRON3)
      CALL LCDIMA(MIDENT,KRON3,DSDSIG)
C --- CONSTRUCTION DE DEVG
      CALL LCDEVI(GV,DEVGV)
      CALL LCDEVI(GP,DEVGP)
C --- CONSTRUCTION DE D(DEVGII)/DSIGMA
      CALL LCPRMM(DSDSIG,DGVDS,DGTVDS)
      CALL LCPRMM(DSDSIG,DGPDS,DGTPDS)
      CALL LCINVE(ZERO,DGIVDS)
      CALL LCINVE(ZERO,DGIPDS)
      IF((SEUILP.GE.ZERO).OR.(VINF(7).GT.ZERO))THEN
        DO 310 I = 1, NDT
          DO 320 J = 1, NDT
            DGIVDS(I) = DGIVDS(I)+DEVGV(J)/DEVGIV*DGTVDS(J,I)
            DGIPDS(I) = DGIPDS(I)+DEVGP(J)/DEVGII*DGTPDS(J,I)
 320      CONTINUE
 310    CONTINUE
      ELSE
        DO 330 I = 1, NDT
          DO 340 J = 1, NDT
            DGIVDS(I) = DGIVDS(I)+DEVGV(J)/DEVGIV*DGTVDS(J,I)
 340      CONTINUE
 330    CONTINUE
      ENDIF
      IF(VARV.EQ.0)THEN
        DO 350 I = 1, NDT
          DRDY(NDT+2,I) = DLAMBD*SQRT(DEUX/TROIS)*DGIPDS(I)
 350    CONTINUE
      ELSE
        DO 360 I = 1, NDT
          DRDY(NDT+2,I) = SQRT(DEUX/TROIS)*(DLAMBD*DGIPDS(I)+
     &                  (DPHVDS(I)*DEVGIV+PHIV*DGIVDS(I))*DT)
 360    CONTINUE
      ENDIF
C ------------------------------------------------------------------
C --- III.2 CALCUL DE DR3DY2 -> Y2 = DLAMBDA
C ------------------------------------------------------------------
      DRDY(NDT+2,NDT+1) = -DEVGII*SQRT(DEUX/TROIS)
C ------------------------------------------------------------------
C --- III.3 CALCUL DE DR3DY3 -> Y3 = XIP
C ------------------------------------------------------------------
      CALL LCPRMV(DSDSIG,DGPDXI,DGTPDX)
      CALL LCPRSC(DEVGP,DGTPDX,DGIPDX)
      IF((SEUILP.GE.ZERO).OR.(VINF(7).GT.ZERO))THEN
        DRDY(NDT+2,NDT+2)= UN - DLAMBD*SQRT(DEUX/TROIS)
     &                     *DGIPDX/DEVGII
      ELSE
        DRDY(NDT+2,NDT+2)= UN
      ENDIF
C ------------------------------------------------------------------
C --- III.4 CALCUL DE DR3DY4 -> Y4 = XIVP
C ------------------------------------------------------------------
C --- TEST POUR SAVOIR SI ON EST EN BUTEE SUR XIVP
      IF(ABS(DXIV-DGAMV).LT.R8PREM())THEN
        CALL LCPRMV(DSDSIG,DGVDXI,DGTVDX)
        CALL LCPRSC(DEVGV,DGTVDX,DGIVDX)
        IF(VARV.EQ.0)THEN
          DRDY(NDT+2,NDT+3)= ZERO
        ELSE
          DRDY(NDT+2,NDT+3)= -(DPHIDX*DEVGIV+PHIV*DGIVDX/DEVGIV)
     &                       *SQRT(DEUX/TROIS)*DT
        ENDIF
       ELSE       
         DRDY(NDT+2,NDT+3)= ZERO
       ENDIF
C ##################################################################
C --- CALCUL DE DR4/DY                        
C ##################################################################
C --- TEST POUR SAVOIR SI ON EST EN BUTEE SUR XIVP
      IF(ABS(DXIV-DGAMV).LT.R8PREM())THEN
C ------------------------------------------------------------------
C --- IV.1 CALCUL DE DR4DY1 -> Y1 = SIGMA
C ------------------------------------------------------------------
        DO 400 I = 1, NDT
          DRDY(NDT+3,I) = (DPHVDS(I)*DEVGIV+PHIV*DGIVDS(I))
     &                     *SQRT(DEUX/TROIS)*DT
 400    CONTINUE
C ------------------------------------------------------------------
C --- IV.2 CALCUL DE DR4DY2 -> Y2 = DLAMBDA
C ------------------------------------------------------------------
        DRDY(NDT+3,NDT+1) = ZERO
C ------------------------------------------------------------------
C --- IV.3 CALCUL DE DR4DY3 -> Y3 = XIP
C ------------------------------------------------------------------
        DRDY(NDT+3,NDT+2) = ZERO
C ------------------------------------------------------------------
C --- IV.4 CALCUL DE DR4DY4 -> Y4 = XIVP
C ------------------------------------------------------------------
        DRDY(NDT+3,NDT+3) = UN - SQRT(DEUX/TROIS)*DT*
     &                      (DPHIDX*DEVGIV+PHIV*DGIVDX/DEVGIV)
      ELSE
C ------------------------------------------------------------------
C --- IV.1 CALCUL DE DR4DY1 -> Y1 = SIGMA
C ------------------------------------------------------------------
        DO 410 I = 1, NDT
          DRDY(NDT+3,I) = ZERO
 410    CONTINUE
C ------------------------------------------------------------------
C --- IV.2 CALCUL DE DR4DY2 -> Y2 = DLAMBDA
C ------------------------------------------------------------------
        DRDY(NDT+3,NDT+1) = ZERO
C ------------------------------------------------------------------
C --- IV.3 CALCUL DE DR4DY3 -> Y3 = XIP
C ------------------------------------------------------------------
        DRDY(NDT+3,NDT+2) = ZERO
C ------------------------------------------------------------------
C --- IV.4 CALCUL DE DR4DY4 -> Y4 = XIVP
C ------------------------------------------------------------------
        DRDY(NDT+3,NDT+3) = UN
      ENDIF
      
      END
