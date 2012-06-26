      SUBROUTINE ANACRI( NOMCRI,NOMFOR, TYPCHA,IMPGRD, PARACT, FORDEF,
     &                  CRSIGM, CREPST, CREPSE, CREPSP)
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

      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      CHARACTER*3  IMPGRD
      CHARACTER*16 NOMCRI, NOMFOR, TYPCHA
      INTEGER      PARACT (30)
      LOGICAL      FORDEF, CRSIGM, CREPST, CREPSE,CREPSP

C ---------------------------------------------------------------------
C BUT: ANALYSER LE CRITERE POUR DETERMINER LES GRANDEURS NECESSAIARES 
C                                A CALCUCLER
C ---------------------------------------------------------------------
C ARGUMENTS:
C NOMCRI   IN    K16: NOM DU CRITERE D'ENDOMMAGEMENT PAR FATIGUE.
C NOMFOR   IN    K16: LE NOM DE FORMULE DE GRNADUER EQUIVALENTE 
C IMPGRD   IN    K3 : 'OUI' : IMPRIMER LES GRANDEURS A CALCULER
C                     'NON':  PAS IMPRIMER
C TYPCHA   IN    K16: TYPE DE CHARGEMENT (PERIODIQUE OU NON).
C PARACT   OUT   REAL: INDICATEUR DU GRANDEUR ACTIVE 
C                      PARACT(K) = 1: K-IEME GRANDEUR EST ACTIVE  
C
C FORDEF  LOGICAL : 'OUI' POUR LA PROJECTION DE L'HISTOIRE 
C       DE DEFORMATION CISSAILLEMEMENT ET 'NON' POUR LA PROJECTION DE 
C                   L'HISTOIRE DE CONTRAINET CISSAILLEMEMENT 
C CRSIGM  LOGICAL : HISTOIRE DE CONTRAINTE NECESSAIRE
C CREPST  LOGICAL : HISTOIRE DE DEFORMATION TOTALE NECESSAIRE
C CREPSE  LOGICAL : HISTOIRE DE DEFORMATION TOTALE NECESSAIRE
C CREPSP  LOGICAL : HISTOIRE DE DEFORMATION PLASTIQUE NECESSAIRE
   
C-----------------------------------------------------------------------
      INTEGER       IP, ID, NPARMA, JPROF, NP, L
      CHARACTER*2   NOMTY1(22), NOMTY2(30)
      CHARACTER*8   NOMPA1(22), NOMPA2(30), NOMPF(30)
      CHARACTER*24  CHNOM, CBID
      LOGICAL       GRDEXI
C
C     ---------------------------------------------------------------
        DATA  NOMPA1/   'DTAUMA', 'PHYDRM', 'NORMAX', 'NORMOY',  
     &                  'EPNMAX', 'EPNMOY', 'DEPSPE', 'EPSPR1', 
     &                  'SIGNM1', 'DENDIS', 'DENDIE', 'APHYDR',
     &                  'MPHYDR', 'DSIGEQ', 'SIGPR1', 'EPSNM1',
     &                  'INVA2S', 'DSITRE', 'DEPTRE', 'EPSPAC',
     &                  'RAYSPH', 'AMPCIS'  /
C     ---------------------------------------------------------------
C     ---------------------------------------------------------------
C      C = CONTRAINTE, T = DEF TOTALE, E = DEF ELAS, P = DEF PLAS

        DATA  NOMTY1/   'CC', 'CC', 'CC', 'CC',  
     &                  'TT', 'TT', 'PP', 'TT', 
     &                  'CT', 'CP', 'CE', 'CC',
     &                  'CC', 'CC', 'CC', 'TC',
     &                  'TT', 'CC', 'TT', 'PP',
     &                  'CC', 'CC'  /
C     ---------------------------------------------------------------
C     ---------------------------------------------------------------
        DATA  NOMPA2/  'TAUPR_1','TAUPR_2','SIGN_1',  'SIGN_2',
     &                 'PHYDR_1','PHYDR_2','EPSPR_1', 'EPSPR_2', 
     &                 'SIPR1_1','SIPR1_2','EPSN1_1', 'EPSN1_2',
     &                 'ETPR1_1','ETPR1_2','SITN1_1', 'SITN1_2',
     &                 'EPPR1_1','EPPR1_2','SIPN1_1', 'SIPN1_2',
     &                 'SIGEQ_1','SIGEQ_2', 'ETEQ_1', 'ETEQ_2',
     &                 'EPEQ_1', 'EPEQ_2',  'INVJ2_1','INVJ2_2',
     &                 'SITRE_1', 'SITRE_2'     /
C       -------------------------------------------------------------
        DATA  NOMTY2/  'CC','CC','CC',  'CC',
     &                 'CC','CC','TT', 'TT', 
     &                 'CC','CC','TC', 'TC',
     &                 'TT','TT','CT', 'CT',
     &                 'PP','PP','CP', 'CP',
     &                 'CC','CC', 'TT', 'TT',
     &                 'PP','PP', 'TT','TT',
     &                 'CC', 'CC'     /

C
C-----------------------------------------------------------------------
C234567                                                              012
C
      CALL JEMARQ()
   
        
C NOMBRE MAX DE PARAMETRES DISPONIBLES
      NPARMA = 30
         
C     INITIALISATION
      DO 15 IP = 1, NPARMA
         PARACT(IP) = 0
15    CONTINUE

      FORDEF =  .FALSE.
      IF (NOMCRI(1:7) .EQ. 'FORMULE') THEN
         
C RECUPERER LES NOMS DE PARAMETRES FOURNIS PAR L'UTILISATEUR         
         CHNOM(20:24) = '.PROL'
         CHNOM(1:19) = NOMFOR
      
         CALL JEVEUO(CHNOM,'L',JPROF)
         CALL FONBPA ( NOMFOR, ZK24(JPROF), CBID, NPARMA, NP, NOMPF )

C VERIFIER QUE LE NOM DE GRANDEUR A CALCULER EST BON        
         IF (TYPCHA .EQ. 'NON_PERIODIQUE') THEN
             DO 10 ID = 1, NP
                GRDEXI = .FALSE.
                DO 40 IP = 1,NPARMA
                   IF  ( NOMPF(ID) .EQ. NOMPA2(IP) ) THEN
                      GRDEXI = .TRUE.
                      PARACT(IP) = 1
                   ENDIF                 
40              CONTINUE
                IF ( .NOT. GRDEXI) THEN
                   CALL U2MESK('F','FATIGUE1_91',1, NOMPF(ID))
                ENDIF
                
                IF ( NOMPF(ID)(1:3) .EQ. 'EPS') THEN
                   FORDEF =  .TRUE.
                   DO 20 IP = 1, NP
                      IF ( NOMPF(IP)(1:3) .EQ. 'TAU') THEN
                         CALL U2MESS('F','FATIGUE1_92')
                      ENDIF
20                 CONTINUE
                ENDIF
                IF ( NOMPF(ID)(1:3) .EQ. 'TAU') THEN
                   DO 30 IP = 1, NP
                      IF ( NOMPF(IP)(1:3) .EQ. 'EPS') THEN
                         CALL U2MESS('F','FATIGUE1_92')
                      ENDIF
30                 CONTINUE
                ENDIF
10           CONTINUE 

         ELSE
         
             DO 60 ID = 1, NP
                GRDEXI = .FALSE.
                DO 50 IP = 1,NPARMA
                   IF  ( NOMPF(ID) .EQ. NOMPA1(IP) ) THEN
                      GRDEXI = .TRUE.
                      PARACT(IP) = 1
                   ENDIF                 
50              CONTINUE

                IF ( .NOT. GRDEXI) THEN
                   CALL U2MESK('F','FATIGUE1_91',1, NOMPF(ID))
                ENDIF
                
60           CONTINUE             
         ENDIF
            
      ENDIF
      
      IF (NOMCRI(1:14) .EQ. 'MATAKE_MODI_AC') THEN
         PARACT(1) = 1
         PARACT(3) = 1
         PARACT(4) = 1
         PARACT(5) = 1
         PARACT(6) = 1
      ENDIF
      
      IF (NOMCRI(1:16) .EQ. 'DANG_VAN_MODI_AC') THEN
         PARACT(1) = 1
         PARACT(2) = 1
         PARACT(4) = 1
         PARACT(5) = 1
         PARACT(6) = 1
      ENDIF
      
      IF (NOMCRI(1:14) .EQ. 'MATAKE_MODI_AV') THEN
         PARACT(1) = 1
         PARACT(2) = 1
         PARACT(3) = 1
         PARACT(4) = 1
      ENDIF
      
      IF (NOMCRI(1:16) .EQ. 'DANG_VAN_MODI_AV') THEN
         PARACT(1) = 1
         PARACT(2) = 1
         PARACT(5) = 1
         PARACT(6) = 1
      ENDIF
      
      IF (NOMCRI(1:16) .EQ. 'FATESOCI_MODI_AV') THEN
         PARACT(3) = 1
         PARACT(4) = 1
         PARACT(7) = 1
         PARACT(8) = 1
      ENDIF
      
      IF (NOMCRI(1:11) .EQ. 'VMIS_TRESCA') THEN
          PARACT(14) = 1
          PARACT(18) = 1
      ENDIF

C DANS POST_FATIGUE
      IF (NOMCRI(1:9) .EQ. 'CROSSLAND') THEN
         PARACT(2) = 1
         PARACT(22) = 1
      ENDIF
      
      IF (NOMCRI(1:12) .EQ. 'PAPADOPOULOS') THEN
         PARACT(2) = 1
         PARACT(21) = 1
      ENDIF
C POUR OPERATEUR POST_FATIGUE

C ANALYSER LES HISTORES NECESSAIRE
      CRSIGM = .FALSE. 
      CREPST = .FALSE. 
      CREPSE = .FALSE. 
      CREPSP = .FALSE. 
      
      DO 80 IP = 1, NPARMA
         IF (PARACT(IP) .EQ. 1) THEN
            DO 81 L = 1,2
               IF (TYPCHA .EQ. 'PERIODIQUE') THEN
                   IF (NOMTY1(IP)(L:L) .EQ. 'C') THEN
                      CRSIGM = .TRUE.
                   ENDIF
                   IF (NOMTY1(IP)(L:L) .EQ. 'T') THEN
                      CREPST = .TRUE.
                   ENDIF
                   IF (NOMTY1(IP)(L:L) .EQ. 'E') THEN
                      CREPSE = .TRUE.
                   ENDIF
                   IF (NOMTY1(IP)(L:L) .EQ. 'P') THEN
                      CREPSP = .TRUE.
                   ENDIF
               ELSE
                   IF (NOMTY2(IP)(L:L) .EQ. 'C') THEN
                      CRSIGM = .TRUE.
                   ENDIF
                   IF (NOMTY2(IP)(L:L) .EQ. 'T') THEN
                      CREPST = .TRUE.
                   ENDIF
                   IF (NOMTY2(IP)(L:L) .EQ. 'E') THEN
                      CREPSE = .TRUE.
                   ENDIF
                   IF (NOMTY2(IP)(L:L) .EQ. 'P') THEN
                      CREPSP = .TRUE.
                   ENDIF
               ENDIF
81          CONTINUE            
         ENDIF
80    CONTINUE

C IMPRIMER DES INFO      
      IF (IMPGRD .EQ. 'OUI') THEN
         WRITE(6,*)'CRITERE AMORCAGE A UTILISER ==>',NOMCRI
         WRITE(6,*)' '
         WRITE(6,*)'LES GRANDEURS A CALCULER : '
         DO 70 IP = 1, NPARMA
            IF (PARACT(IP) .EQ. 1) THEN
       
               IF (TYPCHA .EQ. 'PERIODIQUE') THEN
                  WRITE(6,*)'    ', NOMPA1(IP)
                  WRITE(6,*) ' '
               ELSE 
                  WRITE(6,*)'    ', NOMPA2(IP)
                  WRITE(6,*) ' '
               ENDIF 
            ENDIF 
         
70       CONTINUE

         WRITE(6,*)'HISTOIRES DE CHARGEMENT DOIVENT CONSISTER :'
         
         IF (CRSIGM) THEN 
            WRITE(6,*) '    CONTRAINTE'
         ENDIF
         
         IF (CREPST) THEN 
            WRITE(6,*) '    DEFORMATION TOTALE'
         ENDIF 
         
         IF (CREPSE) THEN 
            WRITE(6,*) '    DEFORMATION ELASTIQUE'
         ENDIF 
         
         IF (CREPSP) THEN 
            WRITE(6,*) '    DEFORMATION PLASTIQUE'
         ENDIF
         WRITE(6,*) ' ' 
         
         IF (CREPSE) THEN 
            WRITE(6,*) 'ON NOTE: DEFORMATION ELASTIQUE = DEFORMATION
     &      TOTALE - DEFORMATION PLASTIQUE'
            IF ( .NOT. CREPST ) THEN
               WRITE(6,*) 'LE CHARGEMENT DOIT CONSISTER EN PLUS: 
     &      DEFORMATION TOTALE (OBLIGATOIRE)'
            ENDIF
            
            IF ( .NOT. CREPSP ) THEN
               WRITE(6,*) 'LE CHARGEMENT DOIT CONSISTER EN PLUS: 
     &      DEFORMATION PLASTIQUE (OPTIONEL)'
            ENDIF 
     
         ENDIF 
      ENDIF
C
      CALL JEDEMA()
      END
