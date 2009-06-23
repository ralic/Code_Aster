      SUBROUTINE DPVPLC(TYPMOD,OPTION,IMATE,CRIT,INSTAM,INSTAP,
     &                TD,TF,TR,DEPSM,SIGM,VIM,SIG,VIP,DSIDEP,IRET)
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/06/2009   AUTEUR ELGHARIB J.EL-GHARIB 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT      NONE
      INTEGER       IMATE,IRET
      REAL*8        DEPSM(6),VIM(*),VIP(*),SIG(6),DSIDEP(6,6)
      REAL*8        SIGM(6),TD,TF,TR
      REAL*8        INSTAM,INSTAP,CRIT(3)
      CHARACTER*8   TYPMOD(*)
      CHARACTER*16  OPTION
C =====================================================================
C --- LOI DE COMPORTEMENT DE TYPE DRUCKER PRAGER VISCOPLASTIQUE - 
C --- VISC_DRUC_PRAG --------------------------------------------------
C --- ECROUISSAGE LINEAIRE --------------------------------------------
C ----VISCOSITE DE TYPE PERZYNA ---------------------------------------
C =====================================================================
C IN  DEPSM    INCREMENT DU CHAMP DE DEFORMATION
C IN  SIGM     CONTRAINTES EN T-
C IN  VIM     VARIABLES INTERNES EN T-
C               1   : P
C               2   : INDICATEUR DE PLASTICITE
C               3   : POSITION DE PAR RAPPORT A PPIC et à PULT
C               4   : NOMBRE D ITERATIONS POUR LA CONVERGENCE LOCALE
C OUT SIG    CONTRAINTES EN T+          
C VAR VIP     VARIABLES INTERNES EN T+
C OUT DSIDEP  MATRICE TANGENTE      
C OUT IRET    CODE RETOUR (0 = OK)
C =====================================================================
      INTEGER      NBMAT,NDT,NDI,NVI,INDAL, NBRE
      PARAMETER    (NBMAT  = 50 )
      REAL*8       MATERD(NBMAT,2),MATERF(NBMAT,2),DEPS(6)
      CHARACTER*3  MATCST
C =====================================================================
      COMMON /TDIM/   NDT, NDI
C =====================================================================
      MATCST = 'OUI'   
C =====================================================================
C --- RECUPERATION DU TYPE DE LOI DE COMPORTEMENT DP ------------------
C =====================================================================
      CALL DPVPMA(TYPMOD, IMATE, NBMAT, TD, MATERD, MATERF,
     &              MATCST, NDT, NDI, NVI, INDAL)
C =====================================================================
C --- RETRAIT DE LA DEFORMATION DUE A LA DILATATION THERMIQUE ---------
C =====================================================================
      CALL DPVPDI(NBMAT,MATERF, TD, TF, TR, DEPSM, DEPS)

C =====================================================================
C --- RESOLTUTION DE LA LOI DRUCKER PRAGER VISCOPLASTIQUE -------------
C =====================================================================
      CALL DPVPRE(TYPMOD,NVI,OPTION,CRIT,INSTAM,INSTAP,NBMAT,
     &            MATERF,SIGM,DEPS,VIM,VIP,SIG,NBRE,DSIDEP,IRET)
C =====================================================================
      END
