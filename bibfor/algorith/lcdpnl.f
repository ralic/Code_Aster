      SUBROUTINE LCDPNL(TYPMOD,NDIM,OPTION,IMATE,SIGM,EPSM,
     &                         TM,TP,TREF,DEPS,VIM,VIP,SIG,DSIDPT,IRET)
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
      INTEGER       NDIM, IMATE,IRET
      REAL*8        SIGM(6),DEPS(6,2),VIM(*),VIP(*),SIG(6)
      REAL*8        DSIDPT(12,6),EPSM(6,2),TM,TP,TREF
      CHARACTER*8   TYPMOD(*)
      CHARACTER*16  OPTION
C =====================================================================
C --- APPLICATION DE LA LOI DE COMPORTEMENT DE TYPE DRUCKER PRAGER ----
C --- LINEAIRE AVEC PRISE EN COMPTE DES PHENOMENES DE NON LOCALISATION 
C =====================================================================
C IN  NDIM    DIMENSION DE L'ESPACE
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
C =====================================================================
      LOGICAL      RIGI,RESI
      INTEGER      NDIMSI,I,J,K,L,NDT,NDI
      REAL*8       KRON(6),TRE,TRER,VALRES(2),DSDP1(6,6),DSDP2(6,6)
      REAL*8       DEUXMU,LAMBDA,DSDP1B(6,6),DSDP2B(6,6),YOUNG,NU
      CHARACTER*2  CODRET(2)
      CHARACTER*8  NOMRES(2)
C ======================================================================
      COMMON /TDIM/   NDT, NDI
C =====================================================================
      DATA KRON   /1.D0, 1.D0, 1.D0, 0.D0, 0.D0, 0.D0/
      DATA NOMRES /'E','NU'/
C =====================================================================
C --- INITIALISATION --------------------------------------------------
C =====================================================================
      NDIMSI = 2*NDIM
      RIGI = OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RIGI_MECA_TANG'
      RESI = OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RAPH_MECA'
C =====================================================================
C --- CARACTERISTIQUES MATERIAU ---------------------------------------
C =====================================================================
      CALL RCVALA(IMATE,'ELAS',0,' ',0.D0,2,NOMRES,VALRES,CODRET,'F ')
      YOUNG  = VALRES(1)
      NU     = VALRES(2)
      DEUXMU = YOUNG / ( 1.0D0 + NU )
      LAMBDA = YOUNG * NU / ( 1.0D0 + NU ) / ( 1.0D0 - 2.D0 * NU )
C =====================================================================
C --- COMPORTEMENT LOCAL ----------------------------------------------
C =====================================================================
      CALL LCDRPR(TYPMOD,OPTION,IMATE,SIGM,TM,TP,TREF,
     &                        DEPS(1,2),VIM,VIP,SIG,DSDP2,IRET)
C =====================================================================
C --- CORRECTION NON LOCALE -------------------------------------------
C =====================================================================
      IF (RESI) THEN
         TRE  = EPSM(1,1)+EPSM(2,1)+EPSM(3,1)
     &        + DEPS(1,1)+DEPS(2,1)+DEPS(3,1)
         TRER = EPSM(1,2)+EPSM(2,2)+EPSM(3,2)
     &        + DEPS(1,2)+DEPS(2,2)+DEPS(3,2)
     
         DO 100 I = 1,NDIMSI
            SIG(I) = SIG(I) + LAMBDA*(TRE - TRER)*KRON(I)
     &           + DEUXMU*(EPSM(I,1)+DEPS(I,1)-EPSM(I,2)-DEPS(I,2))
 100     CONTINUE
      ENDIF

      IF (RIGI) THEN
  
         CALL LCINMA ( 0.0D0, DSDP1  )
         CALL LCINMA ( 0.0D0, DSDP1B )
         CALL LCINMA ( 0.0D0, DSDP2B )
 
         DO 10 I = 1,3
            DO 20 J = 1,3
               DSDP1(I,J) = LAMBDA
 20         CONTINUE
 10      CONTINUE
 
         DO 30 I = 1,NDT
            DSDP1(I,I) = DSDP1(I,I) + DEUXMU
 30      CONTINUE

         CALL LCPRSM(-1.0D0,DSDP1,DSDP1B)
         CALL LCSOMA(DSDP1B,DSDP2,DSDP2B)
C =====================================================================
C --- STOCKAGE DE LA MATRICE SOUS FORME COMPACTE ----------------------
C =====================================================================
         DO 40 L=1,NDT
            DO 50 K=1,NDT
               DSIDPT(K,L)   = DSDP1 (K,L)  
               DSIDPT(K+6,L) = DSDP2B(K,L)
 50         CONTINUE
 40      CONTINUE
      ENDIF
C =====================================================================
      END
