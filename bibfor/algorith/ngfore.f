      SUBROUTINE NGFORE(NDDL,NEPS,NPG,W,B,NI2LDC,SIGREF,FREF)

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
       IMPLICIT NONE

       INTEGER NDDL,NEPS,NPG
       REAL*8  W(0:NPG-1),NI2LDC(0:NEPS-1),B(0:NEPS*NPG-1,NDDL)
       REAL*8  SIGREF(0:NEPS-1),FREF(NDDL)
C ----------------------------------------------------------------------
C     REFE_FORC_NODA - FORMULATION GENERIQUE
C ----------------------------------------------------------------------
C IN  NDDL    : NOMBRE DE DEGRES DE LIBERTE 
C IN  NEPS    : NOMBRE DE COMPOSANTES DE DEFORMATION ET CONTRAINTE
C IN  NPG     : NOMBRE DE POINTS DE GAUSS
C IN  W       : POIDS DES POINTS DE GAUSS 
C IN  B       : MATRICE CINEMATIQUE : DEFORMATION = B.DDL
C IN  LI2LDC  : CONVERSION CONTRAINTE STOCKEE -> CONTRAINTE LDC (RAC2)
C IN  SIGREF  : CONTRAINTES DE REFERENCE (PAR COMPOSANTE)
C OUT FREF    : FORCES DE REFERENCE
C ----------------------------------------------------------------------
      INTEGER    NPGMAX,EPSMAX
      PARAMETER (NPGMAX=27,EPSMAX=20)
C ----------------------------------------------------------------------
      INTEGER NEPG,IEG,I
      REAL*8  SIGPDS(0:EPSMAX*NPGMAX-1)
C ----------------------------------------------------------------------

C    INITIALISATION
      NEPG  = NEPS*NPG
 
C    CONTRAINTE AVEC RAC2 ET POIDS DU POINT DE GAUSS
      DO 10 IEG = 0,NEPG-1
        SIGPDS(IEG) = SIGREF(MOD(IEG,NEPS))*NI2LDC(MOD(IEG,NEPS))
     &                * W(IEG/NEPS)
 10   CONTINUE

C    FINT = SOMME(G) W(G).ABS(BT).SIGREF
      DO 20 I = 1,NDDL
        FREF(I)=0
        DO 30 IEG = 0,NEPG-1
          FREF(I) = FREF(I) + ABS(B(IEG,I))*SIGPDS(IEG)
 30     CONTINUE
 20   CONTINUE
 
      END
