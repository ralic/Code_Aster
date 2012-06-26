      SUBROUTINE FMACGR(NBF, NBPTOT, SIGM, EPSM, EPSPM,
     &           NOMCRI,NOMMAT,NOMFOR,GRDVIE,FORVIE,VRESU)
     
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16  NOMCRI, NOMFOR,FORVIE
      CHARACTER*8   GRDVIE,NOMMAT
      INTEGER       NBF, NBPTOT
      REAL*8        VRESU(24), VALA,VALB, SIGM(NBF*NBPTOT)
      REAL*8        EPSM(NBF*NBPTOT), EPSPM(NBF*NBPTOT)
      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/06/2012   AUTEUR TRAN V-X.TRAN 
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
C TOLE CRP_20 CRP_21 CRS_512 CRS_1404
C ---------------------------------------------------------------------
C BUT: POUR LA FATIGUE A AMPLITUDE CONSTANTE
C      CALCULER DES GRANDEURS SERVANT A EVALUER LES CRITERES D4AMORCAGE
C      ET CALCULER LA GRANDEUR EQUIVALENT 
C ---------------------------------------------------------------------
C ARGUMENTS :
C ---------------------------------------------------------------------
C ------------------------------------------------------------------
C23456

      REAL*8     GRDEQ(2), DTAUM(2), NRUPT(2), DOM(2)    
      REAL*8     NORMAX(2), NORMOY(2),EPNMAX(2), EPNMOY(2), VALPU(2)
      REAL*8     PHYDRO, PHYDRM 
      REAL*8     SIG(6), VALPAR(24),EPS(6), EPSE(6), EPSP(6),VEPSP(6)
      REAL*8     EPSL(6), EPSEL(6), EPSPL(6),EQEPSP, JACAUX(3)
      REAL*8     VSIG(6), SIGL(6), EQSIG, VSIGE
      REAL*8     R8MAEM, PHYMIN, VEPSPE , LCIV2E 
      REAL*8     NM1X, NM1Y, NM1Z, BR(6),VECPRO(3,3),VALPRO(3)
      REAL*8     EPRMAX, EPRMIN, SIGNM1, TOL, TOLDYN, AR(6)
      REAL*8     FXM, FYM, FZM, SINM1M, SOMDEF
      REAL*8     DEVSIG(6), DVEPSE(6), DENDIS, DSIGL(6),EQUI(17)
      REAL*8     DEPSL(6), SOMDEN,DENDIE, ETREMA, ETREMI
      REAL*8     SIGMAX, EXM, EYM, EZM,EPSNM1,EPNM1M, SIGMIN
      REAL*8     STREMA, STREMI, VEPS(6), EQEPS,VEPST , EPSPAC
      INTEGER    NPERM,ITYPE,IORDRE, NVP, NITJAC
      INTEGER    I,J,K,L,IBID,JPROF, NPARMA, NP,ICODRE,IPAR
      INTEGER    PARACT(30)
      CHARACTER*24 CHNOM, CBID
      CHARACTER*16 TYPCHA
      CHARACTER*8  NOMPF(20), NOMPAR(20), NOMGRD
      LOGICAL      ENDUR, LBID
C-----------------------------------------------------------------------
C       DATA  LSIG/ 'SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ', 'SIYZ' /
C C
C       DATA  LEPS/ 'EPXX', 'EPYY', 'EPZZ', 'EPXY', 'EPXZ', 'EPYZ' /
C C
C       DATA  LGRD/  'DTAUM1', 'VNM1X', 'VNM1Y', 'VNM1Z', 'SINMAX1',
C      &             'SINMOY', 'EPNMAX', 'EPNMOY', 'SIGEQ1', 'NBRUP1',
C      &             'ENDO1', 'DTAUM2', 'VNM2X', 'VNM2Y', 'VNM2Z',
C      &        'SINMAX2', 'SINMOY2', 'EPNMAX2', 'EPNMOY2', 'SIGEQ2',
C      &             'NBRUP2', 'ENDO2' ,'VMIS', 'TRESCA' /

C     ---------------------------------------------------------------
        DATA  NOMPAR/   'DTAUMA', 'PHYDRM', 'NORMAX', 'NORMOY',  
     &                  'EPNMAX', 'EPNMOY', 'DEPSPE', 'EPSPR1', 
     &                  'SIGNM1', 'DENDIS', 'DENDIE', 'APHYDR',
     &                  'MPHYDR', 'DSIGEQ', 'SIGPR1', 'EPSNM1',
     &                  'INVA2S', 'DSITRE', 'DEPTRE', 'EPSPAC'   /
C     ---------------------------------------------------------------

C -------------------------------------------------------------------
C
C RECUPERER LA LISTE DE GRANDEURS ACTIVES
         
        TYPCHA = 'PERIODIQUE'

        CALL ANACRI( NOMCRI,NOMFOR,TYPCHA,'NON', PARACT,
     &            LBID, LBID, LBID, LBID,LBID)

C ------------------------------------------------------------------
C---  CALCULER DES GRANDEURS ACTIVES
C-------------------------------------------------------------------
C INITIALISATION 
      EPNMAX = 0
      EPNMOY = 0
      NORMAX = 0
      NORMOY = 0
      PHYDRM = 0.D0    
      PHYMIN = 0.D0
      PHYDRO = 0.D0
      VEPSPE = 0.D0
      VSIGE = 0.D0
      EPRMAX = 0.D0
      NM1X = 0.D0
      NM1Y = 0.D0
      NM1Z = 0.D0
      SIGNM1 = 0.D0
      SINM1M = 0.D0
      DENDIS = 0.D0
      DENDIE = 0.D0
      ETREMA = 0.D0
      ETREMI = 0.D0
      STREMA = 0.D0
      STREMI = 0.D0
      EPNM1M = 0.D0
      SIGMAX = 0.D0
      SIGMIN = 0.D0
      VEPST  = 0.D0
      EPSPAC = 0.D0
      L = 0

      DO 27 I = 1, 3
         VALPRO(I) = 0.0D0
         VECPRO (I,1) = 0.0D0
27    CONTINUE 

      DO 35 I = 1, 6
         SIG(I) = 0.0D0
         EPS(I) = 0.0D0
         EPSE(I)= 0.0D0
         EPSP(I)= 0.0D0
         VEPSP(I)= 0.0D0
         VSIG(I)= 0.0D0
         EPSL(I)= 0.0D0
         EPSEL(I)= 0.0D0
         EPSPL(I)= 0.0D0
         SIGL(I) = 0.0D0
35    CONTINUE      
    
      DO 10 J= 1, NBPTOT
C     LA CONTRAINTE ET LES DEFORMATION 
         DO 1001 I = 1, 6
            SIG(I) = SIGM((J-1)*6+I)
            EPS(I) = EPSM((J-1)*6+I)
            EPSP(I) = EPSPM((J-1)*6+I)
            EPSE(I) = EPS(I) - EPSP(I)
1001     CONTINUE

C RECUPERER LES TENSEURS A CE NUMERO D'ORDRE        
C ORDRE DU TENSEUR SIXX, SIYY, SIZZ, SIXY, SIXZ, SIYZ,
C SIMILAIRE POUR 
C   EPS  : DEFORMATION TOTALE
C   EPSE : DEFORMATION ELASTIQUE
C   EPSP : DEFORMATION PLASTIQUE
        
C ---------------------------------------------------------------
C ON CALCULE PHYDRM QU'UNE FOIS, LA PRESSION HYDROSTATIQUE
C EST CONSTANTE PAR RAPPORT AU vect_n.

C -- CALCULER LES GRANDEURS PRESSION HYDROSTATIQUE
 
C    CALCULER PRESSION HYDROSTATIQUE MAXIMALE = Max_t(1/3 Tr[SIG])  
         IF  ( (PARACT(2) .EQ. 1) .OR. (PARACT(12) .EQ. 1)
     &                  .OR. (PARACT(13) .EQ. 1) ) THEN
     
            PHYDRO = (SIG(1) + SIG(2) + SIG(3))/3.0D0

            IF (PHYDRO .GT. PHYDRM) THEN
               PHYDRM = PHYDRO
            ENDIF
            
            IF (PHYDRO .LT. PHYMIN) THEN
               PHYMIN = PHYDRO
            ENDIF
            
         ENDIF 
         
C ---------------------------------------------------------------
C -- CALCULER LA DEMI-AMPLITUDE DE LA DEFORMATION PLASIQUE EQVA 
C POUR LE CRIETRE MANSON_COFF
       
         IF (PARACT(7) .EQ. 1)  THEN
         
            DO 11 L = J, NBPTOT  
   
               DO 12 I = 1, 6
                  EPSPL(I) = EPSPM((L-1)*6+I)            
                  VEPSP(I)=  EPSP(I) - EPSPL(I)
12             CONTINUE 
C               EQEPSP = LCIV2E(VEPSP)
               
C
                CALL FGEQUI(VEPSP,'EPSI',3,EQUI)
C
                EQEPSP = EQUI(1)
               
               IF (VEPSPE .LT. EQEPSP) THEN
                  VEPSPE = EQEPSP
               ENDIF
11          CONTINUE   
  
         ENDIF
          
C SIG PRIN MAX-----------------------------------------------
      IF ((PARACT(8) .EQ. 1) .OR. (PARACT(9) .EQ. 1) .OR.
     &          (PARACT(19) .EQ. 1) ) THEN   
            NVP = 3
            NPERM = 12
            TOL = 1.D-10
            TOLDYN = 1.D-2
            ITYPE = 0
            IORDRE = 1
            AR(1) = EPS(1)
            AR(2) = EPS(4)
            AR(3) = EPS(5)
            AR(4) = EPS(2)
            AR(5) = EPS(6)
            AR(6) = EPS(3)
            BR(1) = 1.D0
            BR(2) = 0.D0
            BR(3) = 0.D0
            BR(4) = 1.D0
            BR(5) = 0.D0
            BR(6) = 1.D0      
            
C             CALL FGEQUI(EPS,'EPSI',3,EQUI)
C             
C             VALPRO(1) = EQUI(4)
C             VALPRO(3) = EQUI(2)  
C             VECPRO (1,1) = EQUI(12)
C             VECPRO (2,1) = EQUI(13)
C             VECPRO (3,1) = EQUI(14)
                     
             CALL JACOBI(NVP,NPERM,TOL,TOLDYN,AR,BR,VECPRO,VALPRO,
     &                        JACAUX,NITJAC,ITYPE,IORDRE)
     
            IF (EPRMAX .LT. VALPRO(1)) THEN
               EPRMAX = VALPRO(1)
               NM1X = VECPRO (1,1)
               NM1Y = VECPRO (2,1)
               NM1Z = VECPRO (3,1)
C CALCvect_F = [SIG].vect_n

               FXM = SIG(1)*NM1X + SIG(4)*NM1Y + SIG(5)*NM1Z
               FYM = SIG(4)*NM1X + SIG(2)*NM1Y + SIG(6)*NM1Z
               FZM = SIG(5)*NM1X + SIG(6)*NM1Y + SIG(3)*NM1Z

C CALCNORM = vect_F.vect_n

               SIGNM1 = FXM*NM1X + FYM*NM1Y + FZM*NM1Z

               IF (ABS(SIGNM1) .GT. SINM1M) THEN
                  SINM1M = ABS(SIGNM1)
               ENDIF
               
            ENDIF
            
            IF (EPRMIN .GT. VALPRO(1)) THEN
               EPRMIN = VALPRO(1)
            ENDIF
                                 
            IF (ETREMA .LT. (VALPRO(1)-VALPRO(3))) THEN
               ETREMA = (VALPRO(1)-VALPRO(3))
            ENDIF
            
            IF (ETREMI .GT. (VALPRO(1)-VALPRO(3))) THEN
               ETREMI = VALPRO(1)-VALPRO(3)
            ENDIF
         ENDIF

C ---------------------------------------------------------------
C CALCULER DENSITE D'ENERGIE DISTORSION ELASTIQUE
C    
         IF (PARACT(10) .EQ. 1)  THEN

            CALL LCDEVI(SIG,DEVSIG)
            CALL LCDEVI(EPSE, DVEPSE)

            
            IF (J .LT. NBPTOT) THEN
               L = J+1
               DO 1003 I = 1, 6
                  SIGL(I) = SIGM((L-1)*6+I)
                  EPSL(I) = EPSM((L-1)*6+I)
                  EPSPL(I) = EPSPM((L-1)*6+I)
                  EPSEL(I) = EPSL(I) - EPSPL(I)
1003           CONTINUE

               CALL LCDEVI(SIGL, DSIGL)
               CALL LCDEVI(EPSEL, DEPSL)
               
               
               SOMDEN = 0.D0
               DO 32 I = 1, 6      
                  SOMDEN= SOMDEN + 0.5D0*(DEVSIG(I)+DSIGL(I))
     &                             *(DEPSL(I)- DVEPSE(I))              
32             CONTINUE                   
  
              IF (SOMDEN .GT. 0) THEN 
                  DENDIS = DENDIS + SOMDEN
              ENDIF
               
            ENDIF

         ENDIF

C ---------------------------------------------------------------
C CALCULER DENSITE D'ENERGIE DISSIPISE PLASTIQUE
C    
         IF (PARACT(11) .EQ. 1)  THEN
            
            IF (J .LT. NBPTOT) THEN
            
               L = J+1
            
               DO 1004 I = 1, 6
                  SIGL(I) = SIGM((L-1)*6+I)
                  EPSPL(I) = EPSPM((L-1)*6+I)
1004           CONTINUE
               
               SOMDEN = 0.D0
               DO 33 I = 1, 6      
                  SOMDEN= SOMDEN + 0.5D0*(SIG(I)+SIGL(I))
     &                             *(EPSPL(I)- EPSP(I))              
33             CONTINUE                   
  
               DENDIE = DENDIE + SOMDEN
               
            ENDIF

         ENDIF
         
C ---------------------------------------------------------------
C -- CALCULER LA DEMI-AMPLITUDE DE LA CONTRAINTE EQVALENTE 
C POUR LE CRIETRE MANSON_COFF
       
         IF (PARACT(14) .EQ. 1)  THEN
         
            DO 21 L = J+1, NBPTOT  
               DO 1005 I = 1, 6
                  SIGL(I) = SIGM((L-1)*6+I)
                  VSIG(I)= SIG(I) - SIGL(I)
1005           CONTINUE

               CALL FGEQUI(VSIG,'SIGM',3,EQUI)
               EQSIG = EQUI(1)
               IF (VSIGE .LT. EQSIG) THEN
                  VSIGE = EQSIG
               ENDIF
21          CONTINUE 
 
         ENDIF
C ---------------------------------------------------------------
C CALCULER CONTRAINTES PRINCIPALES MAX ET LA DEF TRACTION DU PLAN
         IF ((PARACT(15) .EQ. 1) .OR. (PARACT(16) .EQ. 1) .OR.
     &          (PARACT(18) .EQ. 1) ) THEN   
            NVP = 3
            NPERM = 12
            TOL = 1.D-10
            TOLDYN = 1.D-2
            ITYPE = 0
            IORDRE = 1
            AR(1) = SIG(1)
            AR(2) = SIG(4)
            AR(3) = SIG(5)
            AR(4) = SIG(2)
            AR(5) = SIG(6)
            AR(6) = SIG(3)
            BR(1) = 1.D0
            BR(2) = 0.D0
            BR(3) = 0.D0
            BR(4) = 1.D0
            BR(5) = 0.D0
            BR(6) = 1.D0      
         
            CALL JACOBI(NVP,NPERM,TOL,TOLDYN,AR,BR,VECPRO,VALPRO,
     &                        JACAUX,NITJAC,ITYPE,IORDRE)

            IF (SIGMAX .LT. VALPRO(1)) THEN
               SIGMAX = VALPRO(1)
               NM1X = VECPRO (1,1)
               NM1Y = VECPRO (2,1)
               NM1Z = VECPRO (3,1)
C CALCvect_F = [SIG].vect_n

               EXM = EPS(1)*NM1X + EPS(4)*NM1Y + EPS(5)*NM1Z
               EYM = EPS(4)*NM1X + EPS(2)*NM1Y + EPS(6)*NM1Z
               EZM = EPS(5)*NM1X + EPS(6)*NM1Y + EPS(3)*NM1Z

C CALCNORM = vect_F.vect_n

               EPSNM1 = EXM*NM1X + EYM*NM1Y + EZM*NM1Z

               IF (ABS(EPSNM1) .GT. EPNM1M) THEN
                  EPNM1M = ABS(EPSNM1)
               ENDIF
               
            ENDIF
            
            IF (SIGMIN .GT. VALPRO(1)) THEN
               SIGMIN = VALPRO(1)
            ENDIF
                                 
            IF (STREMA .LT. (VALPRO(1)-VALPRO(3))) THEN
               STREMA = (VALPRO(1)-VALPRO(3))
            ENDIF
            
            IF (STREMI .GT. (VALPRO(1)-VALPRO(3))) THEN
               STREMI = VALPRO(1)-VALPRO(3)
            ENDIF
         ENDIF         

C --------------------------------------------------------------- 
C CALCULER CONTRAINTES PRINCIPALES MAX ET LA TRACTION DE CE PLAN
         IF (PARACT(17) .EQ. 1)  THEN
         
            DO 41 L = J+1, NBPTOT  
               DO 1006 I = 1, 6
                  EPSL(I) = EPSM((L-1)*6+I)
1006           CONTINUE
           
               DO 42 I = 1, 6      
                  VEPS(I)= EPS(I) - EPSL(I)
42             CONTINUE 
               EQEPS = LCIV2E(VEPS)
               
               IF (VEPST .LT. EQEPS) THEN
                  VEPST = EQEPS
               ENDIF
41          CONTINUE   
  
         ENDIF
         
C --------------------------------------------------------------- 
C CALCULER DEFORMATION PLASTIQUE ACCUMULEE         
         IF (PARACT(20) .EQ. 1)  THEN
         
            IF (J .LT. NBPTOT ) THEN 
               L = J+1
            
               DO 1007 I = 1, 6
                  EPSPL(I) = EPSPM((L-1)*6+I)
1007           CONTINUE
          
               SOMDEF = 0.D0
               DO 43 K=1,6
                  SOMDEF = SOMDEF + (EPSPL(K)-EPSP(K))*
     &                               (EPSPL(K)-EPSP(K))    
43             CONTINUE
               EPSPAC = EPSPAC + SOMDEF**0.5D0
            ENDIF
            
         ENDIF
         
10    CONTINUE

C ---------------------------------------------------------------
C POUR LES GRANDEURS DES CRITERERS "CISSAILEMENT PLAN CRITIQUE", 
C ACMATA  CALCULE LES 8 PREMIER GRANDEURS ET CEUX DE 13-20  
C
      IF  ( (PARACT(1) .EQ. 1) .OR. (PARACT(3) .EQ. 1) .OR.
     &      (PARACT(4) .EQ. 1) .OR. (PARACT(5) .EQ. 1)
     &                  .OR. (PARACT(6) .EQ. 1)) THEN  

C------- CALCUL DU RAYON DE LA SPHERE CIRCONSCRITE ----
C
            CALL FMRAYO(NBF,NBPTOT,SIGM,DTAUM)
C

C
C------- CALCUL DU CRITERE  
          
      ENDIF
      
C -----------------------------------------------------------------
C --  EVALUER LES CRITERES EXISTANTS
C -----------------------------------------------------------------
 
 
                           
C ---------------------------------------------------------------
C           EVALUER CRITERES FOURNIS PAR FORMULE        
C---------------------------------------------------------------
      DO 100 K = 1,1
       
          IF (NOMCRI(1:7) .EQ. 'FORMULE') THEN         
C        NOMBRE DE PARAMETRES DISPONIBLES
             NPARMA = 20
C        VALEURS DE CES PARAMETRES, CORRESSPOND A NOMPAR         
             VALPAR(1) = DTAUM(K)
             VALPAR(2) = PHYDRM
             VALPAR(3) = NORMAX(K)
             VALPAR(4) = NORMOY(K) 
             VALPAR(5) = EPNMAX(K)
             VALPAR(6) = EPNMOY(K)
             VALPAR(7) = VEPSPE/2.D0
             VALPAR(8) = (EPRMAX - EPRMIN)/2.D0
             VALPAR(9) = SINM1M
             VALPAR(10) = DENDIS
             VALPAR(11) = DENDIE
             VALPAR(12) = (PHYDRM - PHYMIN)/2.D0
             VALPAR(13) = (PHYDRM + PHYMIN)/2.D0
             VALPAR(14) = VSIGE/2.D0
             VALPAR(15) = (SIGMAX - SIGMIN)/2.D0
             VALPAR(16) = EPNM1M
             VALPAR(17) = VEPST/2.D0
             VALPAR(18) = (STREMA -STREMI)/4.D0
             VALPAR(19) = (ETREMA -ETREMI)/4.D0
             VALPAR(20) = EPSPAC
                
CRECUPERER LES NOMS DE PARAMETRES FOURNIS PAR L'UTILISATEUR         
             CHNOM(20:24) = '.PROL'
             CHNOM(1:19) = NOMFOR
             
             CALL JEVEUO(CHNOM,'L',JPROF)
             CALL FONBPA ( NOMFOR, ZK24(JPROF), CBID, NPARMA,  
     &                     NP, NOMPF )
     
             DO 30 J = 1, NP
                DO 25 IPAR = 1, NPARMA
                   IF (NOMPF(J).EQ.NOMPAR(IPAR)) THEN
                      VALPU(J) =  VALPAR(IPAR) 
                      GOTO 30            
                   ENDIF
25              CONTINUE             
30           CONTINUE          

             CALL FOINTE('F',NOMFOR, NP, NOMPF,VALPU, GRDEQ(K),IBID)

          
C PAS DE CRITERE DE FATEMI ET SOCIE EN ELASTIQUE ET AMPLITUDE CONSTANTE,
C CELAAS DE SENS.

C C        CALC NOMBRE DE CYCLES A LA RUPTURE ET DU DOMMAGE
C             CALL RCCOME ( NOMMAT, 'FATIGUE', PHENOM, ICODRE )
C             IF ( ICODRE .EQ. 1 ) CALL U2MESS('F','FATIGUE1_24')
             
C        POUR CRITERE= FORMULE      
 
 
            CALL LIMEND( NOMMAT,GRDEQ(K),GRDVIE,FORVIE, ENDUR)
           
            IF (ENDUR) THEN
               NRUPT(K)=R8MAEM()
            ELSE
            
               IF (GRDVIE(1:6) .EQ. 'WOHLER') THEN          
                  NOMGRD = 'SIGM    '
                  GRDVIE(7:8) = '  '    
                  CALL RCVALE(NOMMAT,'FATIGUE',1,NOMGRD,GRDEQ(K),
     &              1,GRDVIE,NRUPT(K),ICODRE,1)
               ENDIF

               IF (GRDVIE(1:8) .EQ. 'MANSON_C') THEN          
                  NOMGRD = 'EPSI    '
                  CALL RCVALE(NOMMAT,'FATIGUE',1,NOMGRD,GRDEQ(K),
     &              1,GRDVIE,NRUPT(K),ICODRE,1)
               ENDIF
               
               IF (GRDVIE(1:8) .EQ. 'FORM_VIE') THEN
               
                  CALL RENRFA(FORVIE, GRDEQ(K), NRUPT(K),ICODRE)
                  
               ENDIF
               
               DOM(K) = 1.D0/NRUPT(K)
               NRUPT(K) = NINT(NRUPT(K))
            ENDIF 

         ENDIF     
                  
100    CONTINUE 
       
C ------------------------------------------------------------------
C---  SORTIE LES RESULTAT
C-------------------------------------------------------------------
                  
C        CONSON D'UN CHAM_ELEM SIMPLE PUIS D'UN CHAM_ELEM CONTENANT
C        POURE POINT DE GAUSS DE CHAQUE MAILLE MAX DE DTAU_MAX ET LE
C        VECTRMAL ASSOCIE.
      DO 40 I = 1, 24
         VRESU(I) = 0.0D0
40    CONTINUE  

      DO 101 K = 1,NPARMA
         VRESU(K) = VALPAR(K)
         
101   CONTINUE
         VRESU(21) = GRDEQ(1)
         VRESU(22) = NRUPT(1)
         VRESU(23) = DOM(1)
C
C      CALL JEDEMA()
      END
