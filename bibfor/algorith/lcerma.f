      SUBROUTINE LCERMA (MAT,FAMI,KPG,KSP,POUM)

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
      INTEGER MAT,KPG,KSP
      CHARACTER*1 POUM
      CHARACTER*(*) FAMI
C ----------------------------------------------------------------------
C   ENDOMMAGEMENT FRAGILE A GRADIENT DE VARIABLE INTERNE ENDO_SCALAIRE
C                      LECTURE DES PARAMETRES
C ----------------------------------------------------------------------
C IN  MAT    ADRESSE DU MATERIAU
C IN  FAMI   FAMILLE DE POINTS D'INTEGRATION (SI 'NONE', PAS DE TEMP.)
C IN  KPG    NUMERO DU POINT D'INTEGRATION
C IN  KSP    NUMERO DU SOUS-POINT
C IN  POUM   LECTURE DES PARAMETRES EN DEBUT '-' OU FIN '+' DU PAS
C ----------------------------------------------------------------------
      INTEGER NBEL,NBER,NBNL
      PARAMETER (NBEL=2,NBER=6,NBNL=2)
      INTEGER IOK(NBEL+NBER+NBNL)
      REAL*8 VALEL(NBEL),VALER(NBER),VALNL(NBNL),ALPHA,TEMP,TREF,COEF
      REAL*8 E,NU,CC,CV
      CHARACTER*8 NOMEL(NBEL),NOMER(NBER),NOMNL(NBNL)
C ----------------------------------------------------------------------
      REAL*8 LAMBDA,DEUXMU,TROISK,RIGMIN,PC,PR,EPSTH
      COMMON /LCEE/ LAMBDA,DEUXMU,TROISK,RIGMIN,PC,PR,EPSTH
C ----------------------------------------------------------------------
      REAL*8 PK,PM,PP
      COMMON /LCES/ PK,PM,PP
C ----------------------------------------------------------------------
      REAL*8 PCT,PCH,PCS
      COMMON /LCER/ PCH,PCT,PCS
C ----------------------------------------------------------------------
      DATA NOMEL /'E','NU'/
      DATA NOMER /'K','M','P','C_VOLU','C_COMP','COEF_RIGI_MINI'/
      DATA NOMNL /'C_GRAD_VARI','PENA_LAGR'/
C ----------------------------------------------------------------------

C - LECTURE DES PARAMETRES MECANIQUES

      CALL RCVALB(FAMI,KPG,KSP,POUM,MAT,' ','ELAS',         0,' ',0.D0,
     &  NBEL,NOMEL,VALEL,IOK,2)
      CALL RCVALB(FAMI,KPG,KSP,POUM,MAT,' ','NON_LOCAL',    0,' ',0.D0,
     &  NBNL,NOMNL,VALNL,IOK,2)
      CALL RCVALB(FAMI,KPG,KSP,POUM,MAT,' ','ENDO_SCALAIRE',0,' ',0.D0,
     &  NBER,NOMER,VALER,IOK,2)

      PC     = VALNL(1)
      PR     = VALNL(2)
      
      E      = VALEL(1)
      NU     = VALEL(2)
      LAMBDA = E*NU / (1-2*NU) / (1+NU)
      DEUXMU = E / (1+NU)
      TROISK = E / (1-2*NU)
      COEF   = TROISK/(2*DEUXMU)
      
      PK     = VALER(1)
      PM     = VALER(2)
      PP     = VALER(3)
      CV     = VALER(4)
      CC     = VALER(5)
      RIGMIN = VALER(6)
      PCS    = 0.5D0*E / ( (1-2*NU)*CC + SQRT(COEF*CV*
     &                     (1-2*NU)**2 + (1+NU)**2) )**2
      PCH    = CV*COEF*PCS
      PCT    = CC*SQRT(PCS)


C - LECTURE DES PARAMETRES THERMIQUES

      IF (FAMI.EQ.'NONE') THEN
        EPSTH = 0.D0
        GOTO 9999
      END IF
      
      CALL RCVALB(FAMI,KPG,KSP,POUM,MAT,' ','ELAS',0,' ',0.D0,1,'ALPHA',
     &  ALPHA,IOK,0)

      IF (IOK(1).EQ.0) THEN
        CALL RCVARC('F','TEMP','REF',FAMI,KPG,KSP,TREF,IOK)
        CALL RCVARC('F','TEMP',POUM, FAMI,KPG,KSP,TEMP,IOK)
        EPSTH = ALPHA*(TEMP-TREF)
      ELSE
        EPSTH = 0
      END IF

 9999 CONTINUE
      END
