      SUBROUTINE ANACRI( NOMCRI,NOMFOR, TYPCHA,IMPGRD, PARACT, FORDEF)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
      LOGICAL      FORDEF

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
C FORDEF  LOGICAL
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER       I, ID, NPARMA, JPROF, NP
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
        DATA  NOMPA2/  'TAUPR_1','TAUPR_2','SIGN_1',  'SIGN_2',
     &                 'PHYDR_1','PHYDR_2','EPSPR_1', 'EPSPR_2', 
     &                 'SIPR1_1','SIPR1_2','EPSN1_1', 'EPSN1_2',
     &                 'ETPR1_1','ETPR1_2','SITN1_1', 'SITN1_2',
     &                 'EPPR1_1','EPPR1_2','SIPN1_1', 'SIPN1_2',
     &                 'SIGEQ_1','SIGEQ_2', 'ETEQ_1', 'ETEQ_2',
     &                 'EPEQ_1', 'EPEQ_2',  'INVJ2_1','INVJ2_2',
     &                 'SITRE_1', 'SITRE_2'     /
C       -------------------------------------------------------------


C
C-----------------------------------------------------------------------
C234567                                                              012
C
      CALL JEMARQ()
   
        
C NOMBRE MAX DE PARAMETRES DISPONIBLES
      NPARMA = 30
         
C     INITIALISATION
      DO 15 I = 1, NPARMA
         PARACT(I) = 0
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
                DO 40 I = 1,NPARMA
                   IF  ( NOMPF(ID) .EQ. NOMPA2(I) ) THEN
                      GRDEXI = .TRUE.
                      PARACT(I) = 1
                   ENDIF                 
40              CONTINUE
                IF ( .NOT. GRDEXI) THEN
                   CALL U2MESK('F','FATIGUE1_91',1, NOMPF(ID))
                ENDIF
                
                IF ( NOMPF(ID)(1:3) .EQ. 'EPS') THEN
                   FORDEF =  .TRUE.
                   DO 20 I = 1, NP
                      IF ( NOMPF(I)(1:3) .EQ. 'TAU') THEN
                         CALL U2MESS('F','FATIGUE1_92')
                      ENDIF
20                 CONTINUE
                ENDIF
                IF ( NOMPF(ID)(1:3) .EQ. 'TAU') THEN
                   DO 30 I = 1, NP
                      IF ( NOMPF(I)(1:3) .EQ. 'EPS') THEN
                         CALL U2MESS('F','FATIGUE1_92')
                      ENDIF
30                 CONTINUE
                ENDIF
10           CONTINUE 

         ELSE
         
             DO 60 ID = 1, NP
                GRDEXI = .FALSE.
                DO 50 I = 1,NPARMA
                   IF  ( NOMPF(ID) .EQ. NOMPA1(I) ) THEN
                      GRDEXI = .TRUE.
                      PARACT(I) = 1
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
      
      IF (IMPGRD .EQ. 'OUI') THEN
         WRITE(6,*)'CRITERE AMORCAGE A UTILISER ==>',NOMCRI
         WRITE(6,*)' '
         WRITE(6,*)'         LES GRANDEURS A CALCULER : '
         DO 70 I = 1, NPARMA
            IF (PARACT(I) .EQ. 1) THEN
       
               IF (TYPCHA .EQ. 'PERIODIQUE') THEN
                  WRITE(6,*)'GRANDEUR : ', NOMPA1(I)
                  WRITE(6,*) ' '
               ELSE 
                  WRITE(6,*)'GRANDEUR : ', NOMPA2(I)
                  WRITE(6,*) ' '
               ENDIF 
            ENDIF 
         
70       CONTINUE
      ENDIF

C
      CALL JEDEMA()
      END
