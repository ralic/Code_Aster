        SUBROUTINE LKSIGE (MOD,NMAT,MATERD,DEPS,SIGD,SIGF)
        IMPLICIT  NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/03/2012   AUTEUR PROIX J-M.PROIX 
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
C       --------------------------------------------------------------
C       INTEGRATION ELASTIQUE NON LINEAIRE SUR DT POUR LETK
C       IN  MOD    :  MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERD :  COEFFICIENTS MATERIAU A T
C           SIGD   :  CONTRAINTE  A T
C           DEPS   :  INCREMENT DE DEFORMATION
C       OUT SIGF   :  CONTRAINTE ELASTIQUE A T+DT
C       --------------------------------------------------------------
        INTEGER         NMAT,NDT,NDI
        REAL*8          MATERD(NMAT,2)
        REAL*8          SIGD(6),SIGF(6)
        REAL*8          DEPS(6)
        CHARACTER*8     MOD
C --- VARIABLES LOCALES
        INTEGER         I
        REAL*8          DSDE(6,6),KK,MU,DEPSV
        REAL*8          DEUX,TROIS,UN,ZERO,KRON(6)
        REAL*8          I1ML,IEL,DEVSIG(6),DEPSD(6)
        REAL*8          SIGDT(6),SIGFT(6),DEPST(6)
        PARAMETER       (TROIS =  3.D0 )
        PARAMETER       (DEUX  =  2.D0 )
        PARAMETER       (UN    =  1.D0 )
        PARAMETER       (ZERO  =  0.D0 )
C       --------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       --------------------------------------------------------------
        DATA   KRON /UN , UN , UN , ZERO ,ZERO ,ZERO/
C       --------------------------------------------------------------

C --------------------------------------------------------------------
C --- PASSAGE DES TENSEURS CONTRAINTES ET DEFORMATIONS A LA CONVENTION
C --- MECANIQUE DES SOLS --- INVERSE DE MMC
C --- TRACTION -> NEGATIF / COMPRESSION -> POSITIF
C --------------------------------------------------------------------
        DO 10 I = 1, NDT
            SIGDT(I) = -SIGD(I)
            DEPST(I) = -DEPS(I)
  10    CONTINUE
C --------------------------------------------------------------------
C --- CONSTRUCTION TENSEUR DE RIGIDITE ELASTIQUE NON LINEAIRE
C --------------------------------------------------------------------
        CALL LKELAS ( NDI, NDT, MOD , NMAT, MATERD,
     &              DEPST, SIGDT, DSDE, KK, MU)

C --------------------------------------------------------------------
C --- DEFINITION DE L'INCREMENT DEFORMATION VOLUMIQUE
C --------------------------------------------------------------------
        DEPSV = DEPST(1)+DEPST(2)+DEPST(3)

C --------------------------------------------------------------------
C --- DEFINITION DU 1ER INVARIANT DES CONTRAINTES ELASTIQUES: IEL
C --------------------------------------------------------------------
        I1ML = SIGDT(1)+SIGDT(2)+SIGDT(3)
        IEL = I1ML + TROIS*KK*DEPSV

C --------------------------------------------------------------------
C --- CONSTRUCTION TENSEUR DEVIATOIRE DES CONTRAINTES A T
C --------------------------------------------------------------------
        CALL LCDEVI(SIGDT,DEVSIG)

C --------------------------------------------------------------------
C --- CONSTRUCTION TENSEUR DEVIATOIRE DE L'INCREMENT DES DEFORMATIONS
C --------------------------------------------------------------------
        CALL LCDEVI(DEPST,DEPSD)

C --------------------------------------------------------------------
C --- CONSTRUCTION TENSEUR DEVIATOIRE DES CONTRAINTES ELASTIQUES
C --------------------------------------------------------------------
        DO 20 I = 1, NDT
          DEVSIG(I) = DEVSIG(I) + DEUX* MU *DEPSD(I)
  20    CONTINUE

C --------------------------------------------------------------------
C --- CONSTRUCTION TENSEUR DES CONTRAINTES ELASTIQUES
C --------------------------------------------------------------------
        DO 30 I = 1, NDT
          SIGFT(I) = DEVSIG(I) + IEL/TROIS*KRON(I)
  30    CONTINUE

C --------------------------------------------------------------------
C --- RETOUR DES CONTRAINTES ELASTIQUES A LA CONVENTION MMC
C --------------------------------------------------------------------
        DO 40 I = 1, NDT
          SIGF(I) = -SIGFT(I)
  40    CONTINUE

        END  
