      SUBROUTINE LCDRPR(TYPMOD,OPTION,IMATE,SIGM,TD,TF,TR,
     &                                   DEPSM,VIM,VIP,SIG,DSIDEP,IRET)
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/12/2003   AUTEUR GRANET S.GRANET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =====================================================================
      IMPLICIT      NONE
      INTEGER       IMATE,IRET
      REAL*8        DEPSM(6),VIM(*),VIP(*),SIG(6),DSIDEP(6,6)
      REAL*8        SIGM(6),TD,TF,TR
      CHARACTER*8   TYPMOD(*)
      CHARACTER*16  OPTION
C ======================================================================
C --- LOI DE COMPORTEMENT DE TYPE DRUCKER PRAGER -----------------------
C --- ELASTICITE ISOTROPE ----------------------------------------------
C --- PLASTICITE DE VON MISES + TERME DE TRACE -------------------------
C --- ECROUISSAGE ISOTROPE LINEAIRE ------------------------------------
C ======================================================================
C IN  OPTION  OPTION DE CALCUL (RAPH_MECA, RIGI_MECA_TANG OU FULL_MECA)
C IN  IMATE   NATURE DU MATERIAU
C IN  EPSM    CHAMP DE DEFORMATION EN T-
C IN  DEPS    INCREMENT DU CHAMP DE DEFORMATION
C IN  VIM     VARIABLES INTERNES EN T-
C               1   : ENDOMMAGEMENT (D)
C               2   : INDICATEUR DISSIPATIF (1) OU ELASTIQUE (0)
C VAR VIP     VARIABLES INTERNES EN T+
C              IN  ESTIMATION (ITERATION PRECEDENTE)     
C              OUT CALCULEES                             
C OUT SIGP    CONTRAINTES EN T+          
C OUT DSIDEP  MATRICE TANGENTE      
C OUT IRET    CODE RETOUR (0 = OK)
C ======================================================================
      INTEGER      TYPEDP,NDT,NDI,NVI
      REAL*8       MATERF(4,2),DEPS(6)
      CHARACTER*2  CODRET
      CHARACTER*8  MOD
C ======================================================================
      COMMON /TDIM/   NDT, NDI
C ======================================================================
C --- RECUPERATION DU TYPE DE LOI DE COMPORTEMENT DP -------------------
C ======================================================================
      MOD    = TYPMOD(1)
      CALL DPMATE(MOD, IMATE, MATERF, NDT, NDI, NVI, TYPEDP)
C ======================================================================
C --- RETRAIT DE LA DEFORMATION DUE A LA DILATATION THERMIQUE ----------
C ======================================================================
      CALL DPDEDI(MATERF, TD, TF, TR, DEPSM, DEPS)
C ======================================================================
      IF (TYPEDP.EQ.1) THEN
C ======================================================================
C --- CAS LINEAIRE -----------------------------------------------------
C ======================================================================
         CALL LCDPLI(MOD,NVI,OPTION,MATERF,SIGM,
     &                                     DEPS,VIM,VIP,SIG,DSIDEP,IRET)
      ELSE IF (TYPEDP.EQ.2) THEN
C ======================================================================
C --- CAS PARABOLIQUE --------------------------------------------------
C ======================================================================
         CALL LCDPPA(MOD,NVI,OPTION,MATERF,SIGM,
     &                                     DEPS,VIM,VIP,SIG,DSIDEP,IRET)
      ENDIF
C ======================================================================
      END
