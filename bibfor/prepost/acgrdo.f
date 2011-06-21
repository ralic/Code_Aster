      SUBROUTINE ACGRDO(JVECTN, JVECTU, JVECTV, NBORDR, KWORK,
     &                  SOMPGW, JRWORK, TSPAQ, IPG, JVECPG,JDTAUM, 
     &                  JRESUN,NOMMET,NOMMAT,NOMCRI,VALA,COEFPA,
     &                  NOMFOR,GRDVIE,FORVIE,VRESU)
      IMPLICIT   NONE
      INTEGER    JVECTN, JVECTU, JVECTV, NBORDR, KWORK
      INTEGER    SOMPGW, JRWORK, TSPAQ, IPG, JVECPG, JDTAUM,JRESUN
      CHARACTER*16  NOMMET, NOMCRI, NOMFOR,FORVIE
      CHARACTER*8   NOMMAT,GRDVIE
      REAL*8     VRESU(24), VALA,COEFPA

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 20/06/2011   AUTEUR TRAN V-X.TRAN 
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
C TOLE CRP_20 CRP_21 CRS_512
C ---------------------------------------------------------------------
C BUT: POUR LA FATIGUE A AMPLITUDE CONSTANTE
C      CALCULER DES GRANDEURS SERVANT A EVALUER LES CRITERES D4AMORCAGE
C      ET CALCULER LA GRANDEUR EQUIVALENT
C
C REMARQUE: CETTE SUBROUTINE EST APPLICABLE POUR UN NOEUD OU IPG EGALE
C           A 1 ET SOMPGW = SOMNOW,JVECPG = JVECNO  
C ----------------------------------------------------------------------
C ARGUMENTS :
C     JVECTN  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
C                     VECTEURS NORMAUX.
C     JVECTU  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
C                     VECTEURS u DU PLAN DE CISAILLEMENT.
C     JVECTV  : IN  : ADRESSE DU VECTEUR CONTENANT LES COMPOSANTES DES
C                     VECTEURS v DU PLAN DE CISAILLEMENT.
C     NBORDR  : IN  : NOMBRE DE NUMEROS D'ORDRE.
C     KWORK   : IN  : KWORK = 0 ON TRAITE LA 1ERE MAILLE DU PAQUET DE
C                               MAILLES ;
C                     KWORK = 1 ON TRAITE LA IEME (I>1) MAILLE DU PAQUET
C                               MAILLES.
C     SOMPGW  : IN  : SOMME DES POINTS DE GAUSS DES N MAILLES PRECEDANT 
C                     LA MAILLE COURANTE.
C     JRWORK  : IN  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT 
C                     L'HISTORIQUE DES TENSEURS DES CONTRAINTES
C                     ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
C                     DU <<PAQUET>> DE MAILLES.
C     TSPAQ   : IN  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
C                     COURANT.
C     IPG     : IN  : IEME POINT DE GAUSS.
C     JVECPG  : IN  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT 
C                     LES COMPOSANTES u ET v DU VECTEUR TAU 
C                     (CISAILLEMENT), POUR TOUS LES NUMEROS
C                     D'ORDRE DE CHAQUE VECTEUR NORMAL.
C    JDTAU      IN    ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
C                     LES VALEURS DE DELTA_TAU_MAX POUR CHAQUE VECTEUR.
C    JVECN      IN    ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
C                     LA VALEUR DU POINTEUR PERMETTANT D'ACCEDER AU
C                     VECTEUR NORMAL ASSOCIE A DELTA_TAU_MAX.
C    NOMMET     IN    NOM DE METHOD D'APPROCHEMENT DE CERCLE ("CERCLE 
C                     EXACT" ET "CERCLE APPROCHE")
C    VALA       IN    VALEUR DU PARAMETRE a ASSOCIE AU CRITERE.
C    COEFPA     IN    COEFFICIENT DE PASSAGE CISAILLEMENT - UNIAXIAL.
C    VRESU      OUT   TABLEAU DES RESULTATS (GRANDEURS ET DOMMAGE).
C                     POUR L'INSTANT, LA DIMENSION DE VRESU EST 24
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ------------------------------------------------------------------
C23456

      REAL*8     RESUPC(24), GRDEQ(2), DTAUM(2), NXM(2), NYM(2), COEPRE
      REAL*8     NZM(2), NRUPT(2), DOM(2), SIGEQ(2) 
      REAL*8     NORMAX(2), NORMOY(2),EPNMAX(2), EPNMOY(2), VALPU(2)
      REAL*8     PHYDRO, PHYDRM, VALE,VALNU, R8B
      REAL*8     SIXX, SIYY, SIZZ, SIXY, SIXZ, SIYZ, VALPAR(24)
      REAL*8     R8MAEM, C1, C2      
      INTEGER    I,J,K,IBID,JPROF, NPARMA, NP,ICODRE,ADR,IRET,IPAR
      CHARACTER*24 CHNOM, CBID
      CHARACTER*16 PHENOM
      CHARACTER*8  NOMPF(3), NOMPAR(3), NOMGRD
      LOGICAL      ENDUR
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

       DATA  NOMPAR/  'DTAUMA', 'PHYDRM' , 'NORMAX'/
C-----------------------------------------------------------------------

C 
C RECUPERER STRESS, MODULE YOUNG ET COEFF POISSON ET PUIS
CALCULER STRAIN

      CALL RCVALE(NOMMAT,'ELAS',0,'        ',R8B,1,'E       ',
     &             VALE,ICODRE,0)
      IF (ICODRE .EQ. 1) THEN
         CALL U2MESS('F','PREPOST_11')
      ENDIF
      CALL RCVALE(NOMMAT,'ELAS',0,'        ',R8B,1,'NU      ',
     &               VALNU,ICODRE,0)
      IF (ICODRE .EQ. 1) THEN
         CALL U2MESS('F','PREPOST_12')
      ENDIF
      C1 = (1+VALNU)/VALE
      C2 = VALNU/VALE
      PHYDRM = 0.D0
      DO 10 J=1, NBORDR
         ADR = (J-1)*TSPAQ+KWORK*SOMPGW*6+(IPG-1)*6
         SIXX = ZR(JRWORK + ADR + 0)
         SIYY = ZR(JRWORK + ADR + 1)
         SIZZ = ZR(JRWORK + ADR + 2)
         SIXY = ZR(JRWORK + ADR + 3)
         SIXZ = ZR(JRWORK + ADR + 4)
         SIYZ = ZR(JRWORK + ADR + 5)
         
C CALCLA PRESSION HYDROSTATIQUE MAXIMALE = Max_t(1/3 Tr[SIG])    

C ON C PHYDRM QU'UNE FOIS, PARCE QUE LA PRESSION HYDROSTATIQUE
C EST ANTE PAR RAPPORT AU vect_n.

         PHYDRO = (SIXX + SIYY + SIZZ)/3.0D0

         IF (PHYDRO .GT. PHYDRM) THEN
            PHYDRM = PHYDRO
         ENDIF
         
C C LE COMPORTEMENT EST ELASTIQUE         
C          EPSXX = C1*SIXX - C2*(SIXX + SIYY + SIZZ)
C          EPSYY = C1*SIYY - C2*(SIXX + SIYY + SIZZ)
C          EPSZZ = C1*SIZZ - C2*(SIXX + SIYY + SIZZ)
C          EPSXY = C1*SIXY
C          EPSXZ = C1*SIXZ
C          EPSYZ = C1*SIYZ

10    CONTINUE 

C ---------------------------------------------------------------
C     =========================================
C        CRITERES AVEC PLANS CRITIQUES        /
C     =========================================
C ---------------------------------------------------------------
C  
C POUR LES CRITERERS "CISSAILEMENT PLAN CRITIQUE", ACMATA  CALCULE LES
C 8 PREMIER GRANDEURS ET CEUX DE 13-20  
C     
      DO 20 I = 1, 24
         RESUPC(I) = 0.0D0
20    CONTINUE  
  
      CALL ACMATA ( JVECTN, JVECTU, JVECTV, NBORDR, KWORK,
     &                SOMPGW, JRWORK, TSPAQ, IPG, JVECPG,JDTAUM, 
     &                JRESUN, NOMMET, NOMMAT, RESUPC)

      DO 100 K = 1,2
         
         DTAUM(K) =    RESUPC(1+(K-1)*11)
         NXM(K)   =    RESUPC(2+(K-1)*11)
         NYM(K)   =    RESUPC(3+(K-1)*11)
         NZM(K)   =    RESUPC(4+(K-1)*11)
         NORMAX(K)=    RESUPC(5+(K-1)*11)
         NORMOY(K)=    RESUPC(6+(K-1)*11)
         EPNMAX(K)=    RESUPC(7+(K-1)*11)
         EPNMOY(K)=    RESUPC(8+(K-1)*11)

C

C RECUPERATION DU COEFFICIENT DE PRE-ECROUISSAGE DONNE 
C PAR L'UTILISATE

         CALL GETVR8(' ','COEF_PREECROU',1,1,1,COEPRE,IRET)
                   
C        1/ C DE MATAKE
         IF (NOMCRI(1:14) .EQ. 'MATAKE_MODI_AC') THEN
            IF ( NORMAX(K) .GT. 0.0D0 ) THEN
               SIGEQ(K) = COEPRE*DTAUM(K) + (VALA*NORMAX(K))
               SIGEQ(K) = SIGEQ(K)*COEFPA
            ELSE
               SIGEQ(K) = COEPRE*DTAUM(K)
               SIGEQ(K) = SIGEQ(K)*COEFPA
            ENDIF
            GRDEQ(K) = SIGEQ(K)
         
         ENDIF

C        2/ C DE DANG VAN
         IF (NOMCRI(1:16) .EQ. 'DANG_VAN_MODI_AC') THEN
            IF ( PHYDRM .GT. 0.0D0 ) THEN
               SIGEQ(K)  = COEPRE*DTAUM(K) + (VALA*PHYDRM)
               SIGEQ(K)  = SIGEQ(K)*COEFPA
            ELSE
               SIGEQ(K)  = COEPRE*DTAUM(K)
               SIGEQ(K)  = SIGEQ(K)*COEFPA
            ENDIF
            GRDEQ(K) = SIGEQ(K)
            
         ENDIF
                
C ---------------------------------------------------------------
C            =========================================
C               CRITERES FOURNIS PAR FORMULE        /
C            =========================================
C---------------------------------------------------------------
          IF (NOMCRI(1:7) .EQ. 'FORMULE') THEN         

C 
C        NOMBRE DE PARAMETRES DISPONIBLES
             NPARMA = 3
C        VALEURIS DE CES PARAMETRES, CORRESSPOND A NOMPAR         
             VALPAR(1) = DTAUM(K)
             VALPAR(2) = PHYDRM
             VALPAR(3) = NORMAX(K)
                
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
             
         ENDIF

C PAS DE CRITERE DE FATEMI ET SOCIE EN ELASTIQUE ET AMPLITUDE CONSTANTE,
C CELAAS DE SENS.

C        CALC NOMBRE DE CYCLES A LA RUPTURE ET DU DOMMAGE
         CALL RCCOME ( NOMMAT, 'FATIGUE', PHENOM, ICODRE )
         IF ( ICODRE .EQ. 1 ) CALL U2MESS('F','FATIGUE1_24')
             
C        POUR CRITERE= FORMULE      
         IF (NOMCRI(1:15) .EQ. 'FORMULE_CRITERE') THEN 
 
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
            
             
C        POUR CRITERE= DANG_VAN OU MATAKE   
         ELSE

            CALL LIMEND( NOMMAT,GRDEQ,'WOHLER',' ', ENDUR)
            IF (ENDUR) THEN
               NRUPT(K)=R8MAEM()
            ELSE
         
               CALL RCVALE(NOMMAT,'FATIGUE',1,'SIGM    ',GRDEQ(K),
     &              1,'WOHLER  ',NRUPT(K),ICODRE,1)
            ENDIF
            DOM(K) = 1.D0/NRUPT(K)
            NRUPT(K)= NINT(NRUPT(K))   

         ENDIF
                    
100    CONTINUE 
             
                  
C        CONSON D'UN CHAM_ELEM SIMPLE PUIS D'UN CHAM_ELEM CONTENANT
C        POURE POINT DE GAUSS DE CHAQUE MAILLE MAX DE DTAU_MAX ET LE
C        VECTRMAL ASSOCIE.
      DO 40 I = 1, 24
         VRESU(I) = 0.0D0
40    CONTINUE  

      DO 101 K = 1,2
      
         VRESU(1+(K-1)*11)  = DTAUM(K)
         VRESU(2+(K-1)*11)  = NXM(K)
         VRESU(3+(K-1)*11)  = NYM(K)
         VRESU(4+(K-1)*11)  = NZM(K)
         VRESU(5+(K-1)*11)  = NORMAX(K)
         VRESU(6+(K-1)*11)  = NORMOY(K)
         VRESU(7+(K-1)*11)  = EPNMAX(K)
         VRESU(8+(K-1)*11)  = EPNMOY(K)
         VRESU(9+(K-1)*11)  = GRDEQ(K)
         VRESU(10+(K-1)*11) = NRUPT(K)
         VRESU(11+(K-1)*11) = DOM(K)
         VRESU(23) = 0.0D0
         VRESU(24) = 0.0D0
101   CONTINUE
C
C      CALL JEDEMA()
      END
