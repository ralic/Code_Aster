      SUBROUTINE NMFIHM(NDIM,NDDL,NNO1,NNO2,NPG,LGPG,IPG,WREF,VFF1,VFF2,
     &                  IDF2,DFFR2,MATE,OPTION,GEOM,DDLM,DDLD,IU,IP,
     &                  SIGM,SIGP,VECT,MATR,VIM,VIP,TM,TP,CRIT,
     &                  COMPOR,TYPMOD)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/12/2012   AUTEUR LAVERNE J.LAVERNE 
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
C TOLE CRP_21
C TOLE CRS_1404
C RESPONSABLE LAVERNE J.LAVERNE

      IMPLICIT NONE
      INTEGER NDIM,MATE,NPG,IPG,IDF2,LGPG,NNO1,NNO2,NDDL,IU(3,16),IP(4)
      REAL*8  VFF1(NNO1,NPG),VFF2(NNO2,NPG),DFFR2(NDIM-1,NNO2,NPG)
      REAL*8  WREF(NPG),GEOM(NDIM,NNO2),DDLM(NDDL),DDLD(NDDL),TM,TP
      REAL*8  SIGM(2*NDIM-1,NPG),SIGP(2*NDIM-1,NPG)
      REAL*8  VECT(NDDL),MATR(NDDL*NDDL)
      REAL*8  VIM(LGPG,NPG),VIP(LGPG,NPG)
      CHARACTER*8  TYPMOD(*)
      CHARACTER*16 OPTION, COMPOR(*)
C-----------------------------------------------------------------------
C  OPTIONS DE MECANIQUE NON LINEAIRE POUR LES JOINTS HM 2D ET 3D
C
C    OPTIONS DE CALCUL 
C       * RAPH_MECA      : DDL = DDL- + DDDL  ->   SIGP , FINT
C       * FULL_MECA      : DDL = DDL- + DDDL  ->   SIGP , FINT , KTAN
C       * RIGI_MECA_TANG : DDL = DDL-       ->                  KTAN
C-----------------------------------------------------------------------
C IN  NDIM   DIMENSION DE L'ESPACE
C IN  NDDL   NOMBRE DE DEGRES DE LIBERTE TOTAL
C IN  NNO1   NOMBRE DE NOEUDS DE LA FACE POUR LES DEPLACEMENTS
C IN  NNO2   NOMBRE DE NOEUDS DE LA FACE POUR LES PRESSIONS ET LA GEOM
C IN  NPG    NOMBRE DE POINTS DE GAUSS
C IN  LGPG   NOMBRE DE VARIABLES INTERNES
C IN  WREF   POIDS DE REFERENCE DES POINTS DE GAUSS
C IN  VFF1   VALEUR DES FONCTIONS DE FORME (DE LA FACE) POUR U
C IN  VFF2   VALEUR DES FONCTIONS DE FORME (DE LA FACE) POUR P ET X
C IN  DFFR2  DERIVEE DES FONCTIONS DE FORME DE REFERENCE DE P ET X EN G
C IN  MATE   MATERIAU CODE
C IN  OPTION OPTION DE CALCUL
C IN  GEOM   COORDONNEES NOEUDS DE PRESSION (2D:SEG2,3D:TRIA3 OU QUAD4)
C IN  DDLM   VALEURS DES DEGRES DE LIBERTE A L'INSTANT MOINS 
C IN  DDLD   VALEURS DES INCREMEMNT DES DEGRES DE LIBERTE
C IN  IU     DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT
C IN  IP     DECALAGE D'INDICE POUR ACCEDER AUX DDL DE PRESSION
C IN  SIGM   CONTRAINTES -         (RAPH_MECA   ET FULL_MECA_*)
C IN  VIM    VARIABLES INTERNES AU DEBUT DU PAS DE TEMPS
C IN  TM     INSTANT -
C IN  TP     INSTANT +
C IN  CRIT   VALEURS DE L'UTILISATEUR POUR LES CRITERES DE CONVERGENCE
C IN  COMPOR NOM DE LA LOI DE COMPORTEMENT
C IN  TYPMOD TYPE DE LA MODELISATION

C OUT SIGP    : CONTRAINTES +         (RAPH_MECA   ET FULL_MECA_*)
C OUT VIP     : VARIABLES INTERNES    (RAPH_MECA   ET FULL_MECA_*)
C OUT MATR    : MATRICE DE RIGIDITE   (RIGI_MECA_* ET FULL_MECA_*)
C OUT VECT    : FORCES INTERIEURES    (RAPH_MECA   ET FULL_MECA_*)
C-----------------------------------------------------------------------

      LOGICAL RESI,RIGI,AXI
      INTEGER I,J,KK,M,N,OS,P,Q,IBID,KPG,NCOORO
      REAL*8 DSIDEP(6,6),B(2*NDIM-1,NDIM+1,2*NNO1+NNO2)
      REAL*8 SIGMO(6),SIGMA(6),EPSM(6),DEPS(6),WG
      REAL*8 COOPG(NDIM+1,NPG),ROT(NDIM*NDIM),COOROT(NDIM+NDIM*NDIM,NPG)
      REAL*8 CRIT,RBID,PRESGM,PRESGD,TEMP
                  
      AXI  = .FALSE.
      RESI = OPTION.EQ.'RAPH_MECA' .OR. OPTION(1:9).EQ.'FULL_MECA'
      RIGI = OPTION(1:9).EQ.'FULL_MECA'.OR.OPTION(1:10).EQ.'RIGI_MECA_'

      IF (.NOT. RESI .AND. .NOT. RIGI)
     &  CALL U2MESK('F','ALGORITH7_61',1,OPTION)

C     INITIALISATIONS :
      IF (RESI) CALL R8INIR(NDDL , 0.D0, VECT,1)
      IF (RIGI) CALL R8INIR(NDDL*NDDL, 0.D0, MATR,1)

C     CALCUL DES COORDONNEES DES POINTS DE GAUSS
      CALL GEDISC(NDIM,NNO2,NPG,VFF2,GEOM,COOPG)

C     BOUCLE SUR LES PG
      DO 11 KPG=1,NPG

C       CALCUL DE LA MATRICE CINEMATIQUE
        CALL EJCINE(NDIM,AXI,NNO1,NNO2,VFF1(1,KPG),VFF2(1,KPG),
     &              WREF(KPG),DFFR2(1,1,KPG),GEOM,WG,KPG,IPG,IDF2,ROT,B)
     
C       CALCUL DES DEFORMATIONS (SAUTS ET GRADIENTS DE PRESSION)
        CALL R8INIR(6, 0.D0, EPSM ,1)
        CALL R8INIR(6, 0.D0, DEPS ,1)
        
        DO 150 I = 1,NDIM
          DO 160 J = 1,NDIM    
          DO 161 N = 1,2*NNO1
            EPSM(I) = EPSM(I) + B(I,J,N)*DDLM(IU(J,N))
            DEPS(I) = DEPS(I) + B(I,J,N)*DDLD(IU(J,N))
 161      CONTINUE
 160      CONTINUE
 150    CONTINUE

        DO 151 I = NDIM+1,2*NDIM-1
          DO 163 N = 1,NNO2
            EPSM(I) = EPSM(I) + B(I,NDIM+1,2*NNO1+N)*DDLM(IP(N))
            DEPS(I) = DEPS(I) + B(I,NDIM+1,2*NNO1+N)*DDLD(IP(N))
 163      CONTINUE
 151    CONTINUE

C       CALCUL DE LA PRESSION AU POINT DE GAUSS   
        PRESGM = 0.D0
        PRESGD = 0.D0
        DO 164 N=1,NNO2
           PRESGM = PRESGM + DDLM(IP(N))*VFF2(N,KPG)
           PRESGD = PRESGD + DDLD(IP(N))*VFF2(N,KPG)
  164   CONTINUE        

C       STOCKAGE DE LA PRESSION DE FLUIDE AU PG
C       POUR LA VI DE POST-TRAITEMENT DANS LA LDC
        EPSM(2*NDIM) = PRESGM
        DEPS(2*NDIM) = PRESGD

C       COOROT : COORDONNEES DU PG + MATRICE DE ROTATION 
C       (MATRICE UTILE POUR LES VI DE POST-TRAITEMENT DANS LA LDC)
        DO 165 J=1,NDIM
          COOROT(J,KPG)=COOPG(J,KPG)
  165   CONTINUE
        DO 166 J=1,NDIM*NDIM
          COOROT(NDIM+J,KPG)=ROT(J)
  166   CONTINUE
        NCOORO=NDIM+NDIM*NDIM
        
C       CONTRAINTES -        
        CALL R8INIR(6, 0.D0, SIGMO,1)
        DO 13 N = 1,2*NDIM-1
          SIGMO(N) = SIGM(N,KPG)
 13     CONTINUE
 
C - APPEL A LA LOI DE COMPORTEMENT
        CALL NMCOMP('RIGI',KPG,1,NDIM,TYPMOD,MATE,COMPOR,CRIT,
     &              TM,TP,
     &              6,EPSM,DEPS,
     &              6,SIGMO,VIM(1,KPG),
     &              OPTION,
     &              RBID,
     &              NCOORO,COOROT(1,KPG),
     &              SIGMA,VIP(1,KPG),36,DSIDEP,1,RBID,IBID)

C - CONTRAINTE ET EFFORTS INTERIEURS

        IF (RESI) THEN

C         CONTRAINTES +        
          DO 12 N = 1,2*NDIM-1
            SIGP(N,KPG) = SIGMA(N)
 12       CONTINUE

C         VECTEUR FINT : U
          DO 300 N=1,2*NNO1
            DO 301 I=1,NDIM
          
              KK = IU(I,N)
              TEMP = 0.D0
              DO 320 J = 1,NDIM
                TEMP = TEMP + B(J,I,N)*SIGP(J,KPG)
 320          CONTINUE
  
              VECT(KK) = VECT(KK) + WG*TEMP
            
 301        CONTINUE
 300      CONTINUE

C         VECTEUR FINT : P
          DO 302 N=1,NNO2
          
            KK = IP(N)
            TEMP = 0.D0
            DO 321 I = NDIM+1,2*NDIM-1
              TEMP = TEMP + B(I,NDIM+1,2*NNO1+N)*SIGP(I,KPG)
 321        CONTINUE
 
            VECT(KK) = VECT(KK) + WG*TEMP
                        
 302      CONTINUE

        ENDIF 

C - MATRICE TANGENTE

        IF (RIGI) THEN

C         MATRICE K:U(I,N),U(J,M)      
          DO 500 N = 1,2*NNO1
          DO 501 I = 1,NDIM
          
            OS = (IU(I,N)-1)*NDDL
                      
            DO 520 M = 1,2*NNO1
            DO 521 J = 1,NDIM
            
              KK = OS + IU(J,M) 
              TEMP = 0.D0
              
              DO 540 P = 1,NDIM
                DO 550 Q = 1,NDIM
                  TEMP = TEMP + B(P,I,N)*DSIDEP(P,Q)*B(Q,J,M)
 550            CONTINUE             
 540          CONTINUE
 
              MATR(KK) = MATR(KK) + WG*TEMP
                                                      
 521        CONTINUE
 520        CONTINUE
 
 501      CONTINUE
 500      CONTINUE

C         MATRICE K:P(N),P(M)      
          DO 502 N = 1,NNO2
          
            OS = (IP(N)-1)*NDDL
                      
            DO 522 M = 1,NNO2
            
              KK = OS + IP(M) 
              TEMP = 0.D0
              
              DO 542 P = NDIM+1,2*NDIM-1
                DO 552 Q = NDIM+1,2*NDIM-1
                  TEMP = TEMP + B(P,NDIM+1,2*NNO1+N)*DSIDEP(P,Q)
     &                                     *B(Q,NDIM+1,2*NNO1+M)
 552            CONTINUE             
 542          CONTINUE
 
              MATR(KK) = MATR(KK) + WG*TEMP
              
 522        CONTINUE
 502      CONTINUE
       
C         MATRICE K:P(N),U(J,M) 
C         ATTENTION, TERME MIS A ZERO, VERIFICATION NECESSAIRE     
          DO 503 N = 1,NNO2
          
            OS = (IP(N)-1)*NDDL
                      
            DO 523 M = 1,2*NNO1
            DO 533 J = 1,NDIM
            
              KK = OS + IU(J,M) 
              TEMP = 0.D0

              DO 543 P = NDIM+1,2*NDIM-1
                DO 553 Q = 1,NDIM
                
                  TEMP = TEMP + B(P,NDIM+1,2*NNO1+N)  
     &                         *DSIDEP(P,Q)*B(Q,J,M)*0.D0
 553            CONTINUE             
 543          CONTINUE
              
              MATR(KK) = MATR(KK) + WG*TEMP
              
 533        CONTINUE
 523        CONTINUE
 503      CONTINUE
        
C         MATRICE K:U(I,N),P(M)      
          DO 504 N = 1,2*NNO1
          DO 514 I = 1,NDIM
          
            OS = (IU(I,N)-1)*NDDL
                      
            DO 524 M = 1,NNO2
            
              KK = OS + IP(M) 
              TEMP = -B(1,I,N)*VFF2(M,KPG)
              MATR(KK) = MATR(KK) + WG*TEMP
                                        
 524        CONTINUE
 
 514      CONTINUE
 504      CONTINUE
 
        ENDIF 


 11   CONTINUE
 
      END
