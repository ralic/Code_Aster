        SUBROUTINE LKCNVX (SIGD,SIGF,NVI,VIND,NMAT,MATER,SEUIL,VINF)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/05/2012   AUTEUR FOUCAULT A.FOUCAULT 
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
        IMPLICIT  NONE
C RESPONSABLE FOUCAULT A.FOUCAULT
C ----------------------------------------------------------------------
C --- BUT : CONVEXE ELASTO-VISCO-PLASTIQUE DE LETK A T+DT --------------
C ---       POUR (SIGF , VINT) DONNES ----------------------------------
C ----------------------------------------------------------------------
C -IN : SIGF   :  CONTRAINTE ELASTIQUE ---------------------------------
C --- : SIGD   :  CONTRAINTE A T ---------------------------------------
C --- : NVI    :  NOMBRE DE VARIABLES INTERNES -------------------------
C --- : VIND   :  VARIABLES INTERNES A T -------------------------------
C --- : NMAT   :  DIMENSION MATER --------------------------------------
C --- : MATER  :  COEFFICIENTS MATERIAU --------------------------------
C OUT : SEUIL  :  SEUIL  PLASTICITE  ET VISCOSITE ----------------------
C ----  SI SEUILV OU SEUILP > 0 -> SEUIL = 1.D0 (NEWTON LOCAL ENCLENCHE)
C ----  VINF(7):  0 OU 1 POUR PRISE EN COMPTE PLASTICITE DANS LCPLAS ---
C ----------------------------------------------------------------------
C ======================================================================
        INTEGER         NMAT,NVI
        REAL*8          MATER(NMAT,2),SEUIL
        REAL*8          SIGD(6),SIGF(6),VIND(NVI),VINF(NVI)
C
        INTEGER         NDT,NDI,I
        REAL*8          I1,DEVSIG(6),UBID,SIGT(6),SIGU(6)
        REAL*8          XIT,SEUILP,SEUILV,ZERO,UN,SOMME,R8PREM

        PARAMETER       (ZERO  =  0.D0 )
        PARAMETER       (UN    =  1.D0 )
C       --------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       --------------------------------------------------------------
C --------------------------------------------------------------------
C --- PASSAGE EN CONVENTION MECANIQUE DES SOLS
C --------------------------------------------------------------------
        DO 10 I = 1, NDT
          SIGT(I) = -SIGF(I)
          SIGU(I) = -SIGD(I)
  10    CONTINUE

C --------------------------------------------------------------------
C --- VERIFICATION D'UN ETAT INITIAL PLASTIQUEMENT ADMISSIBLE -----
C --------------------------------------------------------------------
      SOMME = ZERO
      DO 20 I = 1, NVI
        SOMME = SOMME + VIND(I)
  20  CONTINUE
      IF(ABS(SOMME).LT.R8PREM())THEN
        I1 = SIGU(1)+SIGU(2)+SIGU(3)
        CALL LCDEVI(SIGU,DEVSIG)
        CALL LKCRIP(I1,DEVSIG,VIND,NMAT,MATER,UBID,SEUILP)
        IF(SEUILP/MATER(4,1).GT.1.0D-6)THEN
          CALL U2MESS('F','ALGORITH2_2')
        ENDIF
      ENDIF

C --------------------------------------------------------------------
C --- CONSTRUCTION TENSEUR DEVIATOIRE DES CONTRAINTES ET 1ER INVARIANT
C --------------------------------------------------------------------
        CALL LCDEVI(SIGT,DEVSIG)
        I1 = SIGT(1)+SIGT(2)+SIGT(3)

C ----------------------------------------------------------------------
C --- CALCUL FONCTION SEUIL PLASTIQUE EN SIGF
C ----------------------------------------------------------------------
        CALL LKCRIP(I1,DEVSIG,VIND,NMAT,MATER,UBID,SEUILP)
        IF(SEUILP.GE.ZERO)THEN
          VINF(7) = UN
        ELSE
          VINF(7) = ZERO
        ENDIF
C ----------------------------------------------------------------------
C --- CALCUL FONCTION SEUIL VISQUEUX EN SIGF
C ----------------------------------------------------------------------
        XIT   = VIND(3)
        CALL LKCRIV(XIT,I1,DEVSIG,VIND,NMAT,MATER,UBID,SEUILV)

        IF((SEUILV.GE.ZERO).OR.(SEUILP.GE.ZERO))THEN
          SEUIL = 1.D0
        ELSE
          SEUIL = -1.D0
        ENDIF
 
        END
