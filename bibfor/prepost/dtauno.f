      SUBROUTINE DTAUNO(JRWORK, LISNOE, NBNOT, NBORDR, NNOINI, NBNOP,
     &                  NUMPAQ, TSPAQ, NOMMET, NOMCRI, NOMMAI, CNSR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 11/06/2003   AUTEUR F1BHHAJ J.ANGLES 
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
C RESPONSABLE F1BHHAJ
C TOLE  CRP_20
      IMPLICIT     NONE
      INTEGER      JRWORK, NBNOT, LISNOE(NBNOT), NBORDR, NNOINI, NBNOP 
      INTEGER      NUMPAQ, TSPAQ
      CHARACTER*8  NOMMAI
      CHARACTER*16 NOMCRI, NOMMET
      CHARACTER*19 CNSR
C ---------------------------------------------------------------------
C BUT: DETERMINER LE PLAN INCLINE POUR LEQUEL DELTA_TAU EST MAXIMUM
C      POUR CHAQUE NOEUD D'UN <<PAQUET>> DE NOEUDS.
C ---------------------------------------------------------------------
C ARGUMENTS:
C JRWORK     IN    I  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
C                       L'HISTORIQUE DES TENSEURS DES CONTRAINTES
C                       ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
C                       DU <<PAQUET>> DE MAILLES.
C LISNOE     IN    I  : LISTE COMPLETE DES NOEUDS A TRAITER.
C NBNOT      IN    I  : NOMBRE TOTAL DE NOEUDS A TRAITER.
C NBORDR     IN    I  : NOMBRE DE NUMERO D'ORDRE STOCKE DANS LA
C                       STRUCTURE DE DONNEES RESULTAT.
C NNOINI     IN    I  : NUMERO DU 1ER NOEUD DU <<PAQUET>> DE
C                       NOEUDS COURANT.
C NBNOP      IN    I  : NOMBRE DE NOEUDS DANS LE <<PAQUET>> DE
C                       NOEUDS COURANT.
C NUMPAQ     IN    I  : NUMERO DU PAQUET DE NOEUDS COURANT.
C TSPAQ      IN    I  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE NOEUDS
C                       COURANT.
C NOMMET     IN    K16: NOM DE LA METHODE DE CALCUL DU CERCLE
C                       CIRCONSCRIT.
C NOMCRI     IN    K16: NOM DU CRITERE AVEC PLANS CRITIQUES.
C NOMMAI     IN    K8 : NOM UTILISATEUR DU MAILLAGE. 
C CNSR       IN    K19: NOM DU CHAMP SIMPLE DESTINE A RECEVOIR LES
C                       RESULTATS.
C
C REMARQUE :
C  - LA TAILLE DU SOUS-PAQUET EST EGALE A LA TAILLE DU <<PAQUET>> DE
C    NOEUDS DIVISEE PAR LE NOMBRE DE NUMERO D'ORDRE (NBORDR).
C-----------------------------------------------------------------------

C---- COMMUNS NORMALISES  JEVEUX
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNOM,JEXNUM,JEXATR
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ------------------------------------------------------------------
      INTEGER      I, J, K, L, N, JCNRD, JCNRL, JCNRV, JAD
      INTEGER      IRET, NBMA, ADRMA, NUMA, IMAP, ICESD, ICESL, ICESV
      INTEGER      IAD, IBID, IVECT, IORDR, TNECES, TDISP, JVECNO
      INTEGER      JVECTN, JVECTU, JVECTV, NBVEC, NGAM, TAB2(18), INOP
      INTEGER      NUNOE, IBIBD, JDTAUM, JRESUN, MNMAX(2)
      INTEGER      JVNO1, JVNO2, LOR8EM, LOISEM, JTYPMA, JTYP
      INTEGER      JVECN2, JVECU2, JVECV2, JVECN1, JVECU1, JVECV1
      INTEGER      NBPAR, ICMP, ADRS, KWORK, SOMNOW, CNBNO
C
      REAL*8       EPSILO, DGAM, GAMMA, PI, R8PI, DPHI, TAB1(18), PHI
      REAL*8       GAMMAM, PHIM, DGAM2, DPHI2, DTAUM(2)
      REAL*8       NXM(2), NYM(2), NZM(2)
      REAL*8       SIXX, SIYY, SIZZ, SIXY, SIXZ, SIYZ, FXM(2), FYM(2)
      REAL*8       FZM(2), EPSXX, EPSYY, EPSZZ, EPSXY, EPSXZ, EPSYZ
      REAL*8       NORM(2), NORMAX(2), SNORM(2), EPSXM(2), EPSYM(2)
      REAL*8       EPSZM(2), EPNORM(2), EPNMAX(2), SEPNMX(2), NORMOY(2)
      REAL*8       EPNMOY(2), R8B, VALE, VALNU, C1, C2, VALA, VALB
      REAL*8       PHYDRO, PHYDRM
      REAL*8       COEFPA, SIGEQ(2), NRUPT(2), DOM(2), VRESU(22)
      REAL*8       R8MAEM
C
      CHARACTER*2  CODRET, CODWO
      CHARACTER*8  NOMPAR, NOMRES, CHMAT1, NOMMAT, KTYP, DIMK, K8B
      CHARACTER*16 PHENOM
      CHARACTER*19 CHMAT, CESMAT, NCNCIN
      CHARACTER*24 TYPMA
      LOGICAL      ENDUR
C
C-----------------------------------------------------------------------
C234567                                                              012
C-----------------------------------------------------------------------
      DATA  TAB1/ 180.0D0, 60.0D0, 30.0D0, 20.0D0, 15.0D0, 12.857D0,
     &             11.25D0, 10.588D0, 10.0D0, 10.0D0, 10.0D0, 10.588D0,
     &             11.25D0, 12.857D0, 15.0D0, 20.0D0, 30.0D0, 60.0D0 /
C
      DATA  TAB2/ 1, 3, 6, 9, 12, 14, 16, 17, 18, 18, 18, 17, 16, 14,
     &           12, 9, 6, 3 /
C
      EPSILO = 1.0D-7
      PI = R8PI()
C-----------------------------------------------------------------------
C
      CALL JEMARQ()

C CONSTRUCTION DU VECTEUR CONTENANT DELTA_TAU_MAX
C CONSTRUCTION DU VECTEUR CONTENANT LA VALEUR DU POINTEUR PERMETTANT
C              DE RETROUVER LE VECTEUR NORMAL ASSOCIE A DELTA_TAU_MAX
C
      CALL WKVECT('&&DTAUNO.DTAU_MAX', 'V V R', 209, JDTAUM)
      CALL WKVECT('&&DTAUNO.RESU_N', 'V V I', 209, JRESUN)
C
C CONSTRUCTION DU VECTEUR NORMAL SUR UNE DEMI SPHERE
C CONSTRUCTION DU VECTEUR U DANS LE PLAN TANGENT, SUR UNE DEMI SPHERE
C CONSTRUCTION DU VECTEUR V DANS LE PLAN TANGENT, SUR UNE DEMI SPHERE
C
      CALL WKVECT( '&&DTAUNO.VECT_NORMA', 'V V R', 630, JVECTN )
      CALL WKVECT( '&&DTAUNO.VECT_TANGU', 'V V R', 630, JVECTU )
      CALL WKVECT( '&&DTAUNO.VECT_TANGV', 'V V R', 630, JVECTV )
C
      CALL WKVECT( '&&DTAUNO.VECT_NORMA1', 'V V R', 27, JVECN1 )
      CALL WKVECT( '&&DTAUNO.VECT_TANGU1', 'V V R', 27, JVECU1 )
      CALL WKVECT( '&&DTAUNO.VECT_TANGV1', 'V V R', 27, JVECV1 )
      CALL WKVECT( '&&DTAUNO.VECT_NORMA2', 'V V R', 27, JVECN2 )
      CALL WKVECT( '&&DTAUNO.VECT_TANGU2', 'V V R', 27, JVECU2 )
      CALL WKVECT( '&&DTAUNO.VECT_TANGV2', 'V V R', 27, JVECV2 )
C
      CALL WKVECT( '&&DTAUNO.VECTNO1', 'V V R', 18*NBORDR, JVNO1 )
      CALL WKVECT( '&&DTAUNO.VECTNO2', 'V V R', 18*NBORDR, JVNO2 )
C
C OBTENTION DES ADRESSES '.CESD', '.CESL' ET '.CESV' DU CHAMP SIMPLE
C DESTINE A RECEVOIR LES RESULTATS : DTAUM, ....
C
      CALL JEVEUO(CNSR//'.CNSD','L',JCNRD)
      CALL JEVEUO(CNSR//'.CNSL','E',JCNRL)
      CALL JEVEUO(CNSR//'.CNSV','E',JCNRV)
C
C RECUPERATION MAILLE PAR MAILLE DU MATERIAU DONNE PAR L'UTILISATEUR
C
      CALL GETVID(' ','CHAM_MATER',1,1,1,CHMAT1,IRET)
      CHMAT = CHMAT1//'.CHAMP_MAT'
      CESMAT = '&&DTAUNO.CESMAT'
      CALL CARCES(CHMAT,'ELEM',' ','V',CESMAT,IRET)
      CALL JEVEUO(CESMAT//'.CESD','L',ICESD)
      CALL JEVEUO(CESMAT//'.CESL','L',ICESL)
      CALL JEVEUO(CESMAT//'.CESV','L',ICESV)
C
      TNECES = 209*NBORDR*2
      CALL JEDISP(1, TDISP)
      TDISP =  (TDISP * LOISEM()) / LOR8EM() 
      IF (TDISP .LT. TNECES ) THEN
         CALL UTDEBM('F', 'DTAUNO.1', 'LA TAILLE MEMOIRE '//
     &       ' NECESSAIRE AU VECTEUR DE TRAVAIL DANS '//
     &       ' LEQUEL NOUS STOCKONS LES COMPOSANTES '//
     &       ' u ET v DU VECTEUR TAU EST TROP IMPORTANTE '//
     &       ' PAR RAPPORT A LA PLACE DISPONIBLE.')
         CALL UTIMPI('L', 'TAILLE DISPONIBLE : ', 1, TDISP)
         CALL UTIMPI('L', 'TAILLE NECESSAIRE : ', 1, TNECES)
         CALL UTFINM( )
      ELSE
         CALL WKVECT( '&&DTAUNO.VECTNO', 'V V R', TNECES, JVECNO )
         CALL JERAZO( '&&DTAUNO.VECTNO', TNECES, 1 )
      ENDIF
C
      DGAM = 10.0D0
C
      N = 0
      DO 300 J=1, 18
         GAMMA=(J-1)*DGAM*(PI/180.0D0)
         DPHI=TAB1(J)*(PI/180.0D0)
         NGAM=TAB2(J)
         DO 320 I=1, NGAM
            PHI=(DPHI/2.0D0) + (I-1)*DPHI
            N = N + 1
C
            ZR(JVECTN + (N-1)*3)     = SIN(GAMMA)*COS(PHI)
            ZR(JVECTN + (N-1)*3 + 1) = SIN(GAMMA)*SIN(PHI)
            ZR(JVECTN + (N-1)*3 + 2) = COS(GAMMA)
C
            ZR(JVECTU + (N-1)*3)     = -SIN(PHI)
            ZR(JVECTU + (N-1)*3 + 1) = COS(PHI)
            ZR(JVECTU + (N-1)*3 + 2) = 0.0D0
C
            ZR(JVECTV + (N-1)*3)     = -COS(GAMMA)*COS(PHI)
            ZR(JVECTV + (N-1)*3 + 1) = -COS(GAMMA)*SIN(PHI)
            ZR(JVECTV + (N-1)*3 + 2) = SIN(GAMMA)
C
 320     CONTINUE
 300  CONTINUE
C
C CONSTRUCTION DU VECTEUR : CONTRAINTE = F(NUMERO D'ORDRE) EN CHAQUE
C NOEUDS DU PAQUET DE MAILLES.
      L = 1
      CNBNO = 0
      KWORK = 0
      SOMNOW = 0
C
      DO 400 INOP=NNOINI, NNOINI+(NBNOP-1)
C
         IF ( INOP .GT. NNOINI ) THEN
            KWORK = 1
            SOMNOW = SOMNOW + 1
         ENDIF
C
         CNBNO = CNBNO + 1
         IF ( (L*INT(NBNOT/10.0D0)) .LT. CNBNO ) THEN
           WRITE(6,*)NUMPAQ,'   ',(CNBNO-1)
           L = L + 1
         ENDIF
C
C RECUPERATION DU NOM DU MATERIAU AFFECTE A LA MAILLE OU AUX MAILLES
C QUI PORTENT LE NOEUD COURANT.
C
         NCNCIN = '&&DTAUNO.CNCINV'
         CALL CNCINV ( NOMMAI, IBID, 0, 'V', NCNCIN )
C
         NUNOE = LISNOE(INOP)
         CALL JELIRA( JEXNUM(NCNCIN,NUNOE), 'LONMAX', NBMA, K8B )
         CALL JEVEUO( JEXNUM(NCNCIN,NUNOE), 'L', ADRMA )
C
         TYPMA = NOMMAI//'.TYPMAIL'
         CALL JEVEUO( TYPMA, 'L', JTYPMA )
C
         K = 0
         DO 410, I=1, NBMA
            NUMA = ZI(ADRMA-1 + I)
            JTYP = JTYPMA - 1 + NUMA
            CALL JENUNO( JEXNUM( '&CATA.TM.NOMTM', ZI(JTYP)), KTYP )
            CALL DISMOI( 'F', 'TYPE_TYPMAIL',KTYP,'TYPE_MAILLE',IBID,
     &                   DIMK, IRET )
C
            IF (DIMK .EQ. 'VOLU') THEN
               K = K + 1
               CALL CESEXI('C',ICESD,ICESL,NUMA,1,1,1,IAD)
               IF (IAD .LE. 0) THEN
                  CALL UTMESS('F', 'DTAUNO.2', 'HORS BORNES DEFINIES'//
     &                        ' DANS CESMAT OU CMP NON AFFECTEE.')
               ELSE
                  IF ( (K .GT. 1) .AND. 
     &                 (NOMMAT .NE. ZK8(ICESV - 1 + IAD)) ) THEN
                     CALL UTMESS('F', 'DTAUNO.3', 'LES MAILLES '//
     &                        'ATTACHEES AU NOEUD TRAITE NE SONT '//
     &                        'PAS AFFECTEES DU MEME MATERIAU.')
                  ELSE            
                     NOMMAT = ZK8(ICESV - 1 + IAD)
                  ENDIF
               ENDIF
            ENDIF
C
            IF ( K .EQ. 0 ) THEN
               CALL UTMESS('F', 'DTAUNO.4', 'LE NOEUD TRAITE '//
     &                  'N''EST ASSOCIE A AUCUNE MAILLE VOLUMIQUE.')
            ENDIF
 410     CONTINUE
         CALL JEDETR('&&DTAUNO.CNCINV')
C
C RECUPERATION DES PARAMETRES ASSOCIES AU CRITERE DE MATAKE POUR
C LA MAILLE COURANTE
         CALL RCCOME (NOMMAT,'CISA_PLAN_CRIT',PHENOM,CODRET)
         IF(CODRET(1:2) .EQ. 'NO') THEN
           CALL UTMESS('F','DTAUNO.3',
     &    'POUR CALCULER LE CISAILLEMENT MAX ET LE PLAN CRITIQUE IL'//
     &    ' FAUT RENSEIGNER CISA_PLAN_CRIT DANS DEFI_MATERIAU')
         ENDIF
C
         IF (NOMCRI(1:6) .EQ. 'MATAKE') THEN
            CALL RCVALE(NOMMAT,'CISA_PLAN_CRIT',0,' ',R8B,1,
     &                                    'MATAKE_A',VALA,CODRET,'  ')
            IF (CODRET(1:2) .EQ. 'NO') THEN
               CALL UTMESS('F', 'DTAUNO.4', 'NOUS NE POUVONS '//
     &             ' PAS RECUPERER LA VALEUR DU PARAMETRE A DU'//
     &             ' CRITERE DE MATAKE, cf. COMMANDE: '//
     &             ' DEFI_MATERIAU, OPERANDE: CISA_PLAN_CRIT.')
            ENDIF
            CALL RCVALE(NOMMAT,'CISA_PLAN_CRIT',0,' ',R8B,1,
     &                                    'MATAKE_B',VALB,CODRET,'  ')
            IF (CODRET(1:2) .EQ. 'NO') THEN
               CALL UTMESS('F', 'DTAUNO.5', 'NOUS NE POUVONS '//
     &             ' PAS RECUPERER LA VALEUR DU PARAMETRE B DU'//
     &             ' CRITERE DE MATAKE, cf. COMMANDE: '//
     &             ' DEFI_MATERIAU, OPERANDE: CISA_PLAN_CRIT.')
            ENDIF
C
            CALL RCVALE(NOMMAT,'CISA_PLAN_CRIT',0,' ',R8B,1,
     &                           'COEF_FLE',COEFPA,CODRET,'  ')
            IF (CODRET(1:2) .EQ. 'NO') THEN
               CALL UTMESS('F', 'DTAUNO.6', 'NOUS NE POUVONS'//
     &            ' PAS RECUPERER LA VALEUR DU COEFFICIENT DE'//
     &            ' PASSAGE FLEXION-TORSION, cf. COMMANDE: '//
     &            ' DEFI_MATERIAU, OPERANDE: CISA_PLAN_CRIT.')
            ENDIF
C
C RECUPERATION DES PARAMETRES ASSOCIES AU CRITERE DE DANG VAN POUR
C LA MAILLE COURANTE
         ELSEIF (NOMCRI(1:8) .EQ. 'DANG_VAN') THEN
            CALL RCVALE(NOMMAT,'CISA_PLAN_CRIT',0,' ',R8B,1,
     &                                 'D_VAN_A',VALA,CODRET,'  ')
            IF (CODRET(1:2) .EQ. 'NO') THEN
               CALL UTMESS('F', 'DTAUNO.7', 'NOUS NE POUVONS '//
     &             ' PAS RECUPERER LA VALEUR DU PARAMETRE A DU'//
     &             ' CRITERE DE DANG_VAN, cf. COMMANDE: '//
     &             ' DEFI_MATERIAU, OPERANDE: CISA_PLAN_CRIT.')
            ENDIF
C
            CALL RCVALE(NOMMAT,'CISA_PLAN_CRIT',0,' ',R8B,1,
     &                                 'D_VAN_B',VALB,CODRET,'  ')
            IF (CODRET(1:2) .EQ. 'NO') THEN
               CALL UTMESS('F', 'DTAUNO.8', 'NOUS NE POUVONS '//
     &             ' PAS RECUPERER LA VALEUR DU PARAMETRE B DU'//
     &             ' CRITERE DE DANG_VAN, cf. COMMANDE: '//
     &             ' DEFI_MATERIAU, OPERANDE: CISA_PLAN_CRIT.')
            ENDIF
C
            CALL RCVALE(NOMMAT,'CISA_PLAN_CRIT',0,' ',R8B,1,
     &                           'COEF_CIS',COEFPA,CODRET,'  ')
            IF (CODRET(1:2) .EQ. 'NO') THEN
               CALL UTMESS('F', 'DTAUNO.9', 'NOUS NE POUVONS '//
     &            ' PAS RECUPERER LA VALEUR DU COEFFICIENT DE'//
     &            ' PASSAGE CISAILLEMENT-TRACTION, cf. COMMANDE: '//
     &            ' DEFI_MATERIAU, OPERANDE: CISA_PLAN_CRIT.')
            ENDIF
         ENDIF
C
            CALL JERAZO('&&DTAUNO.VECTNO', TNECES, 1)
C
            NBVEC = 209
            CALL TRLONO(NBVEC, JVECTN, JVECTU, JVECTV, NBORDR, KWORK,
     &                  SOMNOW, JRWORK, TSPAQ, INOP, JVECNO)
C
C CALCUL DU MAX DES DELTA_TAU MAX ET DU VECTEUR NORMAL ASSOCIE POUR
C LE NOEUD COURANT.
C
C 1/ REMISE A ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
C    DELTA_TAU POUR UN POINT DE GAUSS ET DU VECTEUR DE TRAVAIL
C    PERMETTANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.
C
            CALL JERAZO('&&DTAUNO.DTAU_MAX', NBVEC, 1)
            CALL JERAZO('&&DTAUNO.RESU_N', NBVEC, 1)
C
C 2/ CALCUL DU RAYON CIRCONSCRIT
C
            CALL RAYCIR(JVECNO, JDTAUM, JRESUN, NBORDR, NBVEC, NOMMET)
C
C 3/ CALCUL DU 1ER MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE
C
            DTAUM(1) = 0.0D0
            DTAUM(2) = 0.0D0
            MNMAX(1) = 1
            MNMAX(2) = 1
C
            DO 430 I=1, NBVEC
               IF ( (ABS(ZR(JDTAUM + (I-1)) - DTAUM(1)) .GT. EPSILO)
     &               .AND. ( ZR(JDTAUM + (I-1)) .GT. DTAUM(1)) ) THEN
                  DTAUM(2) = DTAUM(1)
                  MNMAX(2) = MNMAX(1)
                  DTAUM(1) = ZR(JDTAUM + (I-1))
                  MNMAX(1) = I
               ELSEIF ( ABS(ZR(JDTAUM + (I-1)) - DTAUM(1))
     &                   .LT. EPSILO ) THEN
                  DTAUM(2) = ZR(JDTAUM + (I-1))
                  MNMAX(2) = I
               ENDIF
 430        CONTINUE
C
C 4/ PREMIER RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
C    ET DU MAX DES DELTA_TAU (DETERMINATION DU VECTEUR NORMAL A 2
C    DEGRES PRES).
C
            PHYDRO = 0.0D0
            PHYDRM = 0.0D0
C
            DO 440 K=1, 2
               NORM(K) = 0.0D0
               NORMAX(K) = 0.0D0
               SNORM(K) = 0.0D0
               EPNORM(K) = 0.0D0
               EPNMAX(K) = 0.0D0
               SEPNMX(K) = 0.0D0
               NXM(K) = ZR(JVECTN + (MNMAX(K)-1)*3)
               NYM(K) = ZR(JVECTN + (MNMAX(K)-1)*3 + 1)
               NZM(K) = ZR(JVECTN + (MNMAX(K)-1)*3 + 2)
               GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM(K)**2)),NZM(K))
               IF (GAMMAM .LT. 0.0D0) THEN
                  GAMMAM = GAMMAM + PI
               ENDIF
C
               IF ((ABS(NYM(K)) .LT. EPSILO) .AND.
     &             (ABS(NXM(K)) .LT. EPSILO)) THEN
                 PHIM = 0.0D0
               ELSE
                 PHIM = ATAN2(ABS(NYM(K)),NXM(K))
               ENDIF
               IF (PHIM .LT. 0.0D0) THEN
                 PHIM = PHIM + PI
               ENDIF
C
               IF (ABS(GAMMAM) .LT. EPSILO) THEN
                  GAMMA = 5.0D0*(PI/180.0D0)
                  DPHI2 = 60.0D0*(PI/180.0D0)
                  DO 450 I=1, 6
                     PHI = 0.0D0 + (I-1)*DPHI2
C
                     ZR(JVECN2 + (I-1)*3)     = SIN(GAMMA)*COS(PHI)
                     ZR(JVECN2 + (I-1)*3 + 1) = SIN(GAMMA)*SIN(PHI)
                     ZR(JVECN2 + (I-1)*3 + 2) = COS(GAMMA)
C
                     ZR(JVECU2 + (I-1)*3)     = -SIN(PHI)
                     ZR(JVECU2 + (I-1)*3 + 1) = COS(PHI)
                     ZR(JVECU2 + (I-1)*3 + 2) = 0.0D0
C
                     ZR(JVECV2 + (I-1)*3)     = -COS(GAMMA)*COS(PHI)
                     ZR(JVECV2 + (I-1)*3 + 1) = -COS(GAMMA)*SIN(PHI)
                     ZR(JVECV2 + (I-1)*3 + 2) = SIN(GAMMA)
C
 450              CONTINUE
C
                  GAMMA = 0.0D0
                  PHI = PI
C
                  ZR(JVECN2 + 6*3)     = SIN(GAMMA)*COS(PHI)
                  ZR(JVECN2 + 6*3 + 1) = SIN(GAMMA)*SIN(PHI)
                  ZR(JVECN2 + 6*3 + 2) = COS(GAMMA)
C
                  ZR(JVECU2 + 6*3)     = -SIN(PHI)
                  ZR(JVECU2 + 6*3 + 1) = COS(PHI)
                  ZR(JVECU2 + 6*3 + 2) = 0.0D0
C
                  ZR(JVECV2 + 6*3)     = -COS(GAMMA)*COS(PHI)
                  ZR(JVECV2 + 6*3 + 1) = -COS(GAMMA)*SIN(PHI)
                  ZR(JVECV2 + 6*3 + 2) = SIN(GAMMA)
C
                  NBVEC = 7
                  CALL TRLONO(NBVEC, JVECN2, JVECU2, JVECV2, NBORDR,
     &                        KWORK, SOMNOW, JRWORK, TSPAQ, INOP, JVNO2)
               ELSE
                  DGAM2 = 2.0D0*(PI/180.0D0)
                  DPHI2 = DGAM2/SIN(GAMMAM)
                  N = 0
                  DO 460 J=1, 3
                     GAMMA = GAMMAM + (J-2)*DGAM2
                     DO 470 I=1, 3
                        PHI = PHIM + (I-2)*DPHI2
                        N = N + 1
C
                        ZR(JVECN2 + (N-1)*3)     = SIN(GAMMA)*COS(PHI)
                        ZR(JVECN2 + (N-1)*3 + 1) = SIN(GAMMA)*SIN(PHI)
                        ZR(JVECN2 + (N-1)*3 + 2) = COS(GAMMA)
C
                        ZR(JVECU2 + (N-1)*3)     = -SIN(PHI)
                        ZR(JVECU2 + (N-1)*3 + 1) = COS(PHI)
                        ZR(JVECU2 + (N-1)*3 + 2) = 0.0D0
C
                        ZR(JVECV2 + (N-1)*3)     = -COS(GAMMA)*COS(PHI)
                        ZR(JVECV2 + (N-1)*3 + 1) = -COS(GAMMA)*SIN(PHI)
                        ZR(JVECV2 + (N-1)*3 + 2) = SIN(GAMMA)
C
 470                 CONTINUE
 460              CONTINUE
C
                  NBVEC = 9
                  CALL TRLONO(NBVEC, JVECN2, JVECU2, JVECV2, NBORDR,
     &                        KWORK, SOMNOW, JRWORK, TSPAQ, INOP, JVNO2)
               ENDIF
C
C 4-1/ REMISE A ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
C     DELTA_TAU POUR UN POINT DE GAUSS ET DU VECTEUR DE TRAVAIL
C     PERMETTANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.
C
               CALL JERAZO('&&DTAUNO.DTAU_MAX', NBVEC, 1)
               CALL JERAZO('&&DTAUNO.RESU_N', NBVEC, 1)
C
C 4-2/ CALCUL DU RAYON CIRCONSCRIT
C
               CALL RAYCIR(JVNO2, JDTAUM, JRESUN, NBORDR, NBVEC, NOMMET)
C
C 4-3/ CALCUL DU 2EME MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE
C
               DTAUM(K) = 0.0D0
               MNMAX(K) = 1
C
               DO 480 I=1, NBVEC
                  IF ( ZR(JDTAUM + (I-1)) .GT. DTAUM(K)) THEN
                     DTAUM(K) = ZR(JDTAUM + (I-1))
                     MNMAX(K) = I
                  ENDIF
 480           CONTINUE
C
C 5/ DEUXIEME RAFFINEMENT CONCERNANT LA DETERMINATION DU VECTEUR NORMAL
C    ET DU MAX DES DELTA_TAU (DETERMINATION DU VECTEUR NORMAL A 1
C    DEGRE PRES).
C
               NXM(K) = ZR(JVECN2 + (MNMAX(K)-1)*3)
               NYM(K) = ZR(JVECN2 + (MNMAX(K)-1)*3 + 1)
               NZM(K) = ZR(JVECN2 + (MNMAX(K)-1)*3 + 2)
               GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM(K)**2)),NZM(K))
               IF (GAMMAM .LT. 0.0D0) THEN
                  GAMMAM = GAMMAM + PI
               ENDIF
C
               IF ((ABS(NYM(K)) .LT. EPSILO) .AND.
     &             (ABS(NXM(K)) .LT. EPSILO)) THEN
                 PHIM = 0.0D0
               ELSE
                 PHIM = ATAN2(ABS(NYM(K)),NXM(K))
               ENDIF
               IF (PHIM .LT. 0.0D0) THEN
                 PHIM = PHIM + PI
               ENDIF
C
               IF (ABS(GAMMAM) .LT. EPSILO) THEN
                  GAMMA = 1.0D0*(PI/180.0D0)
                  DPHI2 = 60.0D0*(PI/180.0D0)
                  DO 500 I=1, 6
                     PHI = 0.0D0 + (I-1)*DPHI2
C
                     ZR(JVECN1 + (I-1)*3)     = SIN(GAMMA)*COS(PHI)
                     ZR(JVECN1 + (I-1)*3 + 1) = SIN(GAMMA)*SIN(PHI)
                     ZR(JVECN1 + (I-1)*3 + 2) = COS(GAMMA)
C
                     ZR(JVECU1 + (I-1)*3)     = -SIN(PHI)
                     ZR(JVECU1 + (I-1)*3 + 1) = COS(PHI)
                     ZR(JVECU1 + (I-1)*3 + 2) = 0.0D0
C
                     ZR(JVECV1 + (I-1)*3)     = -COS(GAMMA)*COS(PHI)
                     ZR(JVECV1 + (I-1)*3 + 1) = -COS(GAMMA)*SIN(PHI)
                     ZR(JVECV1 + (I-1)*3 + 2) = SIN(GAMMA)
C
 500              CONTINUE
C
                  GAMMA = 0.0D0
                  PHI = PI
C
                  ZR(JVECN1 + 6*3)     = SIN(GAMMA)*COS(PHI)
                  ZR(JVECN1 + 6*3 + 1) = SIN(GAMMA)*SIN(PHI)
                  ZR(JVECN1 + 6*3 + 2) = COS(GAMMA)
C
                  ZR(JVECU1 + 6*3)     = -SIN(PHI)
                  ZR(JVECU1 + 6*3 + 1) = COS(PHI)
                  ZR(JVECU1 + 6*3 + 2) = 0.0D0
C
                  ZR(JVECV1 + 6*3)     = -COS(GAMMA)*COS(PHI)
                  ZR(JVECV1 + 6*3 + 1) = -COS(GAMMA)*SIN(PHI)
                  ZR(JVECV1 + 6*3 + 2) = SIN(GAMMA)
C
                  NBVEC = 7
                  CALL TRLONO(NBVEC, JVECN1, JVECU1, JVECV1, NBORDR,
     &                        KWORK, SOMNOW, JRWORK, TSPAQ, INOP, JVNO1)
               ELSE
                  DGAM2 = 1.0D0*(PI/180.0D0)
                  DPHI2 = DGAM2/SIN(GAMMAM)
                  N = 0
                  DO 510 J=1, 3
                     GAMMA = GAMMAM + (J-2)*DGAM2
                     DO 520 I=1, 3
                        PHI = PHIM + (I-2)*DPHI2
                        N = N + 1
C
                        ZR(JVECN1 + (N-1)*3)     = SIN(GAMMA)*COS(PHI)
                        ZR(JVECN1 + (N-1)*3 + 1) = SIN(GAMMA)*SIN(PHI)
                        ZR(JVECN1 + (N-1)*3 + 2) = COS(GAMMA)
C
                        ZR(JVECU1 + (N-1)*3)     = -SIN(PHI)
                        ZR(JVECU1 + (N-1)*3 + 1) =  COS(PHI)
                        ZR(JVECU1 + (N-1)*3 + 2) =  0.0D0
C
                        ZR(JVECV1 + (N-1)*3)     = -COS(GAMMA)*COS(PHI)
                        ZR(JVECV1 + (N-1)*3 + 1) = -COS(GAMMA)*SIN(PHI)
                        ZR(JVECV1 + (N-1)*3 + 2) =  SIN(GAMMA)
C
 520                 CONTINUE
 510              CONTINUE
C
                  NBVEC = 9
                  CALL TRLONO(NBVEC, JVECN1, JVECU1, JVECV1, NBORDR,
     &                        KWORK, SOMNOW, JRWORK, TSPAQ, INOP, JVNO1)
               ENDIF
C
C 5-1/ REMISE A ZERO DU VECTEUR DE TRAVAIL CONTENANT LES VALEURS DE
C     DELTA_TAU POUR UN POINT DE GAUSS ET DU VECTEUR DE TRAVAIL
C     PERMETTANT DE POINTER SUR LE VECTEUR NORMAL ASSOCIE.
C
               CALL JERAZO('&&DTAUNO.DTAU_MAX', NBVEC, 1)
               CALL JERAZO('&&DTAUNO.RESU_N', NBVEC, 1)
C
C 5-2/ CALCUL DU RAYON CIRCONSCRIT
C
               CALL RAYCIR(JVNO1, JDTAUM, JRESUN, NBORDR, NBVEC, NOMMET)
C
C 5-3/ CALCUL DU 2EME MAX DES DELTA_TAU ET DU VECTEUR NORMAL ASSOCIE
C
               DTAUM(K) = 0.0D0
               MNMAX(K) = 1
C
               DO 530 I=1, NBVEC
                  IF ( ZR(JDTAUM + (I-1)) .GT. DTAUM(K)) THEN
                     DTAUM(K) = ZR(JDTAUM + (I-1))
                     MNMAX(K) = I
                  ENDIF
 530           CONTINUE
C
               NXM(K) = ZR(JVECN1 + (MNMAX(K)-1)*3)
               NYM(K) = ZR(JVECN1 + (MNMAX(K)-1)*3 + 1)
               NZM(K) = ZR(JVECN1 + (MNMAX(K)-1)*3 + 2)
               GAMMAM = ATAN2(SQRT(ABS(1.0D0-NZM(K)**2)),NZM(K))
               IF (GAMMAM .LT. 0.0D0) THEN
                  GAMMAM = GAMMAM + PI
               ENDIF
C
               IF ((ABS(NYM(K)) .LT. EPSILO) .AND.
     &             (ABS(NXM(K)) .LT. EPSILO)) THEN
                 PHIM = 0.0D0
               ELSE
                 PHIM = ATAN2(ABS(NYM(K)),NXM(K))
               ENDIF
               IF (PHIM .LT. 0.0D0) THEN
                 PHIM = PHIM + PI
               ENDIF
C
C CALCUL DE LA CONTRAINTE NORMALE MAXIMALE SUR LE PLAN CRITIQUE,
C DE LA CONTRAINTE NORMALE MOYENNE SUR LE PLAN CRITIQUE,
C DE LA DEFORMATION NORMALE MAXIMALE SUR LE PLAN CRITIQUE,
C DE LA DEFORMATION NORMALE MOYENNE SUR LE PLAN CRITIQUE.
C
               CALL RCVALE(NOMMAT,'ELAS',0,' ',R8B,1,'E',VALE,CODRET,
     &                     '  ')
               IF (CODRET(1:2) .EQ. 'NO') THEN
                  CALL UTMESS('F', 'DTAUNO.10', 'NOUS NE POUVONS PAS'//
     &                   ' RECUPERER LA VALEUR DU MODULE D''YOUNG : E.')
               ENDIF
               CALL RCVALE(NOMMAT,'ELAS',0,' ',R8B,1,'NU',VALNU,CODRET,
     &                     '  ')
               IF (CODRET(1:2) .EQ. 'NO') THEN
                  CALL UTMESS('F', 'DTAUNO.11', 'NOUS NE POUVONS PAS'//
     &                    ' RECUPERER LA VALEUR DU COEFFICIENT DE ' //
     &                    'POISSON : NU.')
               ENDIF
               C1 = (1+VALNU)/VALE
               C2 = VALNU/VALE
C
               DO 540 IORDR=1, NBORDR
                  ADRS = (IORDR-1)*TSPAQ + KWORK*SOMNOW*6 + (INOP-1)*6
                  SIXX = ZR(JRWORK + ADRS + 0 )
                  SIYY = ZR(JRWORK + ADRS + 1 )
                  SIZZ = ZR(JRWORK + ADRS + 2 )
                  SIXY = ZR(JRWORK + ADRS + 3 )
                  SIXZ = ZR(JRWORK + ADRS + 4 )
                  SIYZ = ZR(JRWORK + ADRS + 5 )
C
C CALCUL DE LA PRESSION IDROSTATIQUE MAXIMALE = Max_t(1/3 Tr[SIG])
C
                  IF ( K .LT. 2 ) THEN
C
C ON CALCULE PHYDRM UNE FOIS, PARCE QUE LA PRESSION IDROSTATIQUE
C EST INVARIANT PAR RAPPORT AU vect_n.
C
                     PHYDRO = (SIXX + SIYY + SIZZ)/3.0D0
C
                     IF (PHYDRO .GT. PHYDRM) THEN
                        PHYDRM = PHYDRO
                     ENDIF
                  ENDIF
C
                  EPSXX = C1*SIXX - C2*(SIXX + SIYY + SIZZ)
                  EPSYY = C1*SIYY - C2*(SIXX + SIYY + SIZZ)
                  EPSZZ = C1*SIZZ - C2*(SIXX + SIYY + SIZZ)
                  EPSXY = C1*SIXY
                  EPSXZ = C1*SIXZ
                  EPSYZ = C1*SIYZ
C
C CALCUL DE vect_F = [SIG].vect_n
C
                  FXM(K) = SIXX*NXM(K) + SIXY*NYM(K) + SIXZ*NZM(K)
                  FYM(K) = SIXY*NXM(K) + SIYY*NYM(K) + SIYZ*NZM(K)
                  FZM(K) = SIXZ*NXM(K) + SIYZ*NYM(K) + SIZZ*NZM(K)
C
C CALCUL DE NORM = vect_F.vect_n
C
                  NORM(K) = FXM(K)*NXM(K) + FYM(K)*NYM(K) +
     &                      FZM(K)*NZM(K)
C
                  IF (ABS(NORM(K)) .GT. NORMAX(K)) THEN
                     NORMAX(K) = NORM(K)
                  ENDIF
C
                  SNORM(K) = SNORM(K) + NORM(K)

C CALCUL DE vect_EPS = [EPS].vect_n

                  EPSXM(K) = EPSXX*NXM(K) + EPSXY*NYM(K) + EPSXZ*NZM(K)
                  EPSYM(K) = EPSXY*NXM(K) + EPSYY*NYM(K) + EPSYZ*NZM(K)
                  EPSZM(K) = EPSXZ*NXM(K) + EPSYZ*NYM(K) + EPSZZ*NZM(K)

C CALCUL DE EPSILON NORMALE = vect_EPS.vect_n

                  EPNORM(K) = EPSXM(K)*NXM(K) + EPSYM(K)*NYM(K) +
     &                        EPSZM(K)*NZM(K)
C
                  IF (ABS(EPNORM(K)) .GT. EPNMAX(K)) THEN
                     EPNMAX(K) = EPNORM(K)
                  ENDIF
C
                  SEPNMX(K) = SEPNMX(K) + EPNORM(K)
 540           CONTINUE
C
               NORMOY(K) = SNORM(K)/NBORDR
               EPNMOY(K) = SEPNMX(K)/NBORDR
C
C ---------------------------------------------------------------------
C       =============================================
C       /      CRITERES AVEC PLANS CRITIQUES        /
C       =============================================
C ---------------------------------------------------------------------
C
C
C 1/ CRITERE DE MATAKE
               IF (NOMCRI(1:6) .EQ. 'MATAKE') THEN
                  SIGEQ(K) = DTAUM(K) + (VALA*NORMAX(K))
                  SIGEQ(K) = SIGEQ(K)*COEFPA
               ENDIF
C
C 2/ CRITERE DE DANG VAN
               IF (NOMCRI(1:8) .EQ. 'DANG_VAN') THEN
                  IF ( (VALA*PHYDRM) .GT. 0.0D0 ) THEN
                     SIGEQ(K) = DTAUM(K) + (VALA*PHYDRM)
                     SIGEQ(K) = SIGEQ(K)*COEFPA
                  ELSE
                     SIGEQ(K) = DTAUM(K)
                     SIGEQ(K) = SIGEQ(K)*COEFPA
                  ENDIF
               ENDIF
C
C CALCUL DU NOMBRE DE CYCLES A LA RUPTURE ET DU DOMMAGE
C
               CALL RCCOME ( NOMMAT, 'FATIGUE', PHENOM, CODRET )
               IF ( CODRET .EQ. 'NO' ) CALL UTMESS('F','DTAUNO.12',
     &            'POUR CALCULER LE DOMMAGE IL FAUT DEFINIR LE '//
     &            'COMPORTEMENT "FATIGUE" DANS DEFI_MATERIAU' )
C
               CALL RCPARE( NOMMAT, 'FATIGUE', 'WOHLER', CODWO )
               IF ( CODWO .EQ. 'OK' ) THEN
                  CALL LIMEND( NOMMAT,SIGEQ(K),ENDUR)
                  IF (ENDUR) THEN
                     NRUPT(K)=R8MAEM()
                  ELSE
                  CALL RCVALE(NOMMAT,'FATIGUE',1,'SIGM',SIGEQ(K),1,
     &                        'WOHLER',NRUPT(K),CODRET,'F')
                  ENDIF
               ENDIF
C
               DOM(K) = 1.D0/NRUPT(K)
               NRUPT(K) = NINT(NRUPT(K))
C
 440        CONTINUE
C
C CONSTRUCTION D'UN CHAM_ELEM SIMPLE PUIS D'UN CHAM_ELEM CONTENANT
C POUR CHAQUE POINT DE GAUSS DE CHAQUE MAILLE MAX DE DTAU_MAX ET LE
C VECTEUR NORMAL ASSOCIE.
C
            VRESU(1) = DTAUM(1)
            VRESU(2) = NXM(1)
            VRESU(3) = NYM(1)
            VRESU(4) = NZM(1)
            VRESU(5) = NORMAX(1)
            VRESU(6) = NORMOY(1)
            VRESU(7) = EPNMAX(1)
            VRESU(8) = EPNMOY(1)
            VRESU(9) = SIGEQ(1)
            VRESU(10) = NRUPT(1)
            VRESU(11) = DOM(1)
            VRESU(12) = DTAUM(2)
            VRESU(13) = NXM(2)
            VRESU(14) = NYM(2)
            VRESU(15) = NZM(2)
            VRESU(16) = NORMAX(2)
            VRESU(17) = NORMOY(2)
            VRESU(18) = EPNMAX(2)
            VRESU(19) = EPNMOY(2)
            VRESU(20) = SIGEQ(2)
            VRESU(21) = NRUPT(2)
            VRESU(22) = DOM(2)
C
C AFFECTATION DES RESULTATS DANS UN CHAM_ELEM SIMPLE
C
            DO 550 ICMP=1, 22
                  JAD = 22*(NUNOE-1) + ICMP
                  ZL(JCNRL - 1 + JAD) = .TRUE.
                  ZR(JCNRV - 1 + JAD) = VRESU(ICMP)

 550        CONTINUE
C
 400  CONTINUE
C
C MENAGE
C
      CALL DETRSD('CHAM_ELEM_S',CESMAT)
C
      CALL JEDETR('&&DTAUNO.DTAU_MAX')
      CALL JEDETR('&&DTAUNO.RESU_N')
      CALL JEDETR('&&DTAUNO.VECT_NORMA')
      CALL JEDETR('&&DTAUNO.VECT_TANGU')
      CALL JEDETR('&&DTAUNO.VECT_TANGV')
      CALL JEDETR('&&DTAUNO.VECT_NORMA1')
      CALL JEDETR('&&DTAUNO.VECT_TANGU1')
      CALL JEDETR('&&DTAUNO.VECT_TANGV1')
      CALL JEDETR('&&DTAUNO.VECT_NORMA2')
      CALL JEDETR('&&DTAUNO.VECT_TANGU2')
      CALL JEDETR('&&DTAUNO.VECT_TANGV2')
      CALL JEDETR('&&DTAUNO.VECTNO1')
      CALL JEDETR('&&DTAUNO.VECTNO2')
      CALL JEDETR('&&DTAUNO.VECTNO')
      CALL JEDETR('&&DTAUNO.CNCINV')
C
      CALL JEDEMA()
      END
