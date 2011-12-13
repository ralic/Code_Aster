      SUBROUTINE BURSIF(MATERD,MATERF,NMAT,AN,BN,CN,DEPS,NR,YD,DSIG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/12/2011   AUTEUR FOUCAULT A.FOUCAULT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C=====================================================================
C ROUTINE QUI CALCUL L INCREMENT DE CONTRAINTES (ELASTIQUE)
C  CORRIGE PAR LE FLUAGE TOTAL (PROPRE + DESSICCATION)
C
C IN  MATERD   : TABLEAU DES PROPRIETES MATERIAUX A T
C     MATERF   : TABLEAU DES PROPRIETES MATERIAUX A T+DT
C     NMAT     : DIMENSION DU TABLEAU (NMAT,2)  
C     AN       : PRISE EN COMPTE DES DEFORMATIONS DE FLUAGE 
C     BN       : PRISE EN COMPTE DES DEFORMATIONS DE FLUAGE 
C     CN       : PRISE EN COMPTE DES DEFORMATIONS DE FLUAGE 
C     DEPS     : INCREMENT DE DEFORMATION TOTALE -(RETRAITS)
C     NR       : DIMENSION VECTEUR INCONNUES
C     Y  D     : VECTEUR INCONNUES A T
C OUT DSIG     : INCREMENT DE CONTRAINTES ESTIME
C_______________________________________________________________________
C
      IMPLICIT NONE
      INTEGER  I,J,NDT,NDI,NMAT,IRET,NR
      REAL*8   AN(6),BN(6,6),CN(6,6)
      REAL*8   YD(NR),SIGF(6),DEPS(6)
      REAL*8   MATERF(NMAT,2),MATERD(NMAT,2),HOOK(6,6),HOOKM(6,6)
      REAL*8   CNXE(6,6),IDENT(6,6),INVER(6,6),SIGE(6)
      REAL*8   DET,DEPSC(6),BNXSID(6)
      REAL*8   DSIG(6),SIGD(6),SIGT(6)
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT ,NDI
C     ----------------------------------------------------------------
C === =================================================================
C --- INITIALISATION DES VARIABLES
C === =================================================================
      DO 1 I=1,NDT
        SIGF(I) = 0.D0
        SIGD(I) = YD(I) 
        DO 2 J=1,NDT
          CNXE(I,J)  = 0.D0
          IDENT(I,J) = 0.D0
 2      CONTINUE
 1    CONTINUE
C === =================================================================
C --- CONSTRUCTION TENSEUR RIGIDITE ELASTIQUE
C === =================================================================
      CALL LCOPLI ( 'ISOTROPE', '3D      ', MATERF, HOOK)
C === =================================================================
C --- CONSTRUCTION TENSEUR ORDRE 4 (I+E.CN)
C === =================================================================
      CALL LCPRMM(HOOK,CN,CNXE)
      DO 4 I=1,NDT
        CNXE(I,I) = 1.D0 + CNXE(I,I)
 4    CONTINUE

      DO 5 I=1,NDT
        IDENT(I,I) = 1.D0
 5    CONTINUE
C === =================================================================
C --- INVERSION DU TENSEUR ORDRE 4 --> (I+E.CN)^(-1)
C === =================================================================
      CALL MGAUSS('NFVP',CNXE, IDENT, 6, NDT, NDT, DET, IRET)
C === =================================================================
C --- CONSTRUCTION TENSEUR ORDRE 2 DEPSC=(DEPS-AN-BN*SIGD)
C === =================================================================
      CALL LCPRMV(BN,SIGD,BNXSID)
      DO 6 I=1,NDT
         DEPSC(I)=DEPS(I)-AN(I)-BNXSID(I)
 6    CONTINUE
C === =================================================================
C --- CONSTRUCTION TENSEUR ORDRE 2 DSIG = HOOK * DEPSC
C === =================================================================
      CALL LCPRMV(HOOK,DEPSC,DSIG)
C === =================================================================
C --- CONSTRUCTION TENSEUR ORDRE 2 HOOK(T+)*(HOOK(T-)^)-1*SIGD = SIGT
C === =================================================================
      CALL LCOPLI ( 'ISOTROPE', '3D      ', MATERD, HOOKM)
      CALL LCINMA(0.D0,INVER)
      DO 7 I=1,NDT
        INVER(I,I) = 1.D0
 7    CONTINUE
      CALL MGAUSS('NFVP',HOOKM, INVER, 6, NDT, NDT, DET, IRET)
      CALL LCPRMM(HOOK,INVER,HOOKM)
      CALL LCPRMV(HOOKM,SIGD,SIGT)
C === =================================================================
C --- CONSTRUCTION TENSEUR ORDRE 2 (SIGT+DSIG) = SIGF
C === =================================================================
      CALL LCSOVE(SIGT,DSIG,SIGE)
C === =================================================================
C --- CONSTRUCTION TENSEUR ORDRE 2  (I+E.CN)^(-1)*SIGF=SIGF
C === =================================================================
      CALL LCPRMV(IDENT,SIGE,SIGF)
C === =================================================================
C --- CONSTRUCTION TENSEUR ORDRE 2  DSIG = SIGF - SIGD
C === =================================================================
      CALL LCDIVE(SIGF,SIGD,DSIG)

      END
