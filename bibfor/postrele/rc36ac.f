      SUBROUTINE RC36AC ( NOMA, NCNCIN, CHINDI, CHCARA, 
     +                    NBMA, LISTMA, CHRESU )
      IMPLICIT   NONE
      INTEGER             NBMA, LISTMA(*)
      CHARACTER*8         NOMA
      CHARACTER*24        NCNCIN, CHINDI, CHCARA, CHRESU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 25/03/2003   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
C
C     CALCUL DES AMPLITUDES DE CONTRAINTES
C     CALCUL DU FACTEUR D'USAGE 
C
C     Pour chaque noeud de chaque maille:
C
C     pour une situation P, on a 2 états stabilisés
C     pour une situation Q, on a 2 états stabilisés
C
C     Soit 2 états stabilisés I et J appartenant respectivement aux 
C     situations P et Q :
C
C     on calcule le SALT(I,J) = 0,5*(EC/E)*Ke*Sn(P,Q)*Sp(I,J)
C
C     avec Sn(P,Q) = Max( Sn(I,J) )
C          Sn(I,J) = Max( Max(Sn(I,J,ThP)), Max(Sn(I,J,ThQ)) )
C
C     avec Sp(I,J) = Max( Max(Sp(I,J,ThP)), Max(Sp(I,J,ThQ)) )
C
C
C Etape 1 : on calcule le SALT qui correspond aux combinaisons de tous
C           les états stabilisés appartenant aux situations d'un groupe
C           donné.
C
C Etape 2 : on calcule le SALT pour les situations non combinables
C
C Etape 3 : traitement des situations de passage
C           on calcule le SALT(I,J)  
C              - avec I appartenant au premier groupe
C              - avec J appartenant au deuxieme groupe
C              - on lui associe le nombre d'occurrences de la
C                situation de passage
C
C
C IN  : NCNCIN : CONNECTIVITE INVERSE
C IN  : CHINDI : CHAM_ELEM DES INDICES DE CONTRAINTES
C IN  : CHCARA : CHAM_ELEM DES CARACTERISTIQUES ELEMENTAIRES
C IN  : NBMA   : NOMBRE DE MAILLES D'ANALYSE
C IN  : LISTMA : LISTE DES MAILLES D'ANALYSE
C OUT : CHRESU : CHAM_ELEM RESULTAT
C     ------------------------------------------------------------------
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
      CHARACTER*32     JEXNOM, JEXNUM, JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      IG, NBGR, NBSIGR, JNSG, IS, IS1, IOC1, NOCC, NUMGR, 
     +             JCOMBI, JPRESA, JPRESB, JMOMEA, JMOMEB, JNBOCC,
     +             NBTH1, JTH1, NBTH2, JTH2, NBCRS, NBCIN, NBCCA,
     +             JCHMAT, JMAT, JCESD, JCESV, JCESL, JCINV, JCIND,
     +             JCCAV, JCCAD, IM, IMA, NBPT, DECRS, DECIN, DECCA,
     +             IPT, INO, ADRM, NBM, ICMP, IAD, JCONX1, JCONX2, 
     +             JNUMGR, JPASSA, NPASS, NUM1, NUM2, IFM, NIV,
     +             IOCS, JSEIGR, IOC2, JCINL, JCCAL
      REAL*8       R8B, PPI, PPJ, SNMAX, SAMAX, UTOT, SALTIJ, UG, NADM,
     +             MPI(3), MPJ(3), SM, SN, SP, C(3), K(3), CARA(3),
     +             MATPI(14), MATPJ(14), MSE(3), SNB, SAB, SMM
      LOGICAL      SEISME,ENDUR
      CHARACTER*2  CODRET
      CHARACTER*8  K8B, NOMMAT, NOEUD
      CHARACTER*24 MOMEPI, MOMEPJ, NOMMAI, NOMNOE, CONNEX, 
     +             MATEPI, MATEPJ
      REAL*8       TYPEKE,SPMECA,SPTHER
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL INFNIV ( IFM, NIV )
C
      NOMMAI = NOMA//'.NOMMAI         '
      NOMNOE = NOMA//'.NOMNOE         '
      CONNEX = NOMA//'.CONNEX         '
      CALL JEVEUO ( CONNEX, 'L', JCONX1 )
      CALL JEVEUO ( JEXATR(CONNEX,'LONCUM'), 'L', JCONX2 )
C
      CALL JELIRA ( '&&RC3600.SITU_NUME_GROUP', 'LONMAX', NBGR, K8B )
      CALL JEVEUO ( '&&RC3600.SITU_NUME_GROUP', 'L', JNUMGR )
      CALL JEVEUO ( '&&RC3600.SITU_SEISME'    , 'L', JSEIGR )
C
      CALL JEVEUO ( '&&RC3600.SITU_COMBINABLE', 'L', JCOMBI )
      CALL JEVEUO ( '&&RC3600.SITU_PRES_A'    , 'L', JPRESA )
      CALL JEVEUO ( '&&RC3600.SITU_PRES_B'    , 'L', JPRESB )
      CALL JEVEUO ( '&&RC3600.SITU_MOMENT_A'  , 'L', JMOMEA )
      CALL JEVEUO ( '&&RC3600.SITU_MOMENT_B'  , 'L', JMOMEB )
      CALL JEVEUO ( '&&RC3600.SITU_NB_OCCUR'  , 'L', JNBOCC )
      CALL JEVEUO ( '&&RC3600.SITU_PASSAGE'   , 'L', JPASSA )
C
      CALL JEVEUO ( '&&RC3600.MATERIAU'       , 'L', JCHMAT )
      CALL JEVEUO ( '&&RC3600.NOM_MATERIAU'   , 'L', JMAT   )
C
C --- LE CHAM_ELEM RESULTAT
C
      CALL JEVEUO ( CHRESU(1:19)//'.CESD', 'L', JCESD ) 
      CALL JEVEUO ( CHRESU(1:19)//'.CESV', 'E', JCESV ) 
      CALL JEVEUO ( CHRESU(1:19)//'.CESL', 'E', JCESL )
      NBCRS = ZI(JCESD-1+2)
C
C --- LE CHAMP INDICE DE CONTRAINTES
C
      CALL JEVEUO ( CHINDI(1:19)//'.CESV', 'L', JCINV )
      CALL JEVEUO ( CHINDI(1:19)//'.CESD', 'L', JCIND )
      CALL JEVEUO ( CHINDI(1:19)//'.CESL', 'L', JCINL )
      NBCIN = ZI(JCIND-1+2)
C
C --- LE CHAMP CARACTERISTIQUES
C
      CALL JEVEUO ( CHCARA(1:19)//'.CESV', 'L', JCCAV )
      CALL JEVEUO ( CHCARA(1:19)//'.CESD', 'L', JCCAD )
      CALL JEVEUO ( CHCARA(1:19)//'.CESL', 'L', JCCAL )
      NBCCA = ZI(JCCAD-1+2)
C
C --- IL FAUT CALCULER LE FACTEUR D'USAGE EN CHAQUE NOEUD DE CHAQUE
C     MAILLE 
C
      DO 10 IM = 1 , NBMA
C
        IMA = LISTMA(IM)
        NOMMAT = ZK8(JMAT+IMA-1)
C
        NBPT  = ZI(JCESD-1+5+4*(IMA-1)+1)
        DECRS = ZI(JCESD-1+5+4*(IMA-1)+4)
        DECIN = ZI(JCIND-1+5+4*(IMA-1)+4)
        DECCA = ZI(JCCAD-1+5+4*(IMA-1)+4)
C
        DO 20  IPT = 1 , NBPT
C
C ------- LA CONNECTIVITE INVERSE
C
          INO = ZI(JCONX1-1+ZI(JCONX2+IMA-1)+IPT-1)
          CALL JEVEUO ( JEXNUM(NCNCIN,INO), 'L', ADRM )
          CALL JELIRA ( JEXNUM(NCNCIN,INO), 'LONMAX', NBM, K8B)
          IF ( NIV .GE. 2 ) THEN
            CALL JENUNO ( JEXNUM(NOMMAI,IMA), K8B )
            CALL JENUNO ( JEXNUM(NOMNOE,INO), NOEUD )
            WRITE(IFM,1000)'===>> TRAITEMENT DU NOEUD ', NOEUD,
     +                  ' APPARTENANT A LA MAILLE ', K8B
          ENDIF
C
C ------- LES INDICES DE CONTRAINTES
C
          DO 202 ICMP = 1 , 3
            IAD = DECIN + (IPT-1)*NBCIN + ICMP
            IF ( .NOT. ZL(JCINL-1+IAD) ) THEN
               CALL JENUNO ( JEXNUM(NOMMAI,IMA), K8B )
               CALL JENUNO ( JEXNUM(NOMNOE,INO), NOEUD )
               CALL UTDEBM('F','RC36AC','ERREUR DONNEES ')
               CALL UTIMPK('L','POUR LA MAILLE ',1,K8B)
               CALL UTIMPK('S',' ET LE NOEUD ',1,NOEUD)
               IF ( ICMP .EQ. 1) THEN
            CALL UTIMPK('L','IL MANQUE L''INDICE DE CONTRAINTES',1,'C1')
               ELSEIF ( ICMP .EQ. 2) THEN
            CALL UTIMPK('L','IL MANQUE L''INDICE DE CONTRAINTES',1,'C2')
               ELSE
            CALL UTIMPK('L','IL MANQUE L''INDICE DE CONTRAINTES',1,'C3')
               ENDIF
               CALL UTFINM
            ENDIF
            C(ICMP) = ZR(JCINV-1+IAD)
            IAD = DECIN + (IPT-1)*NBCIN + ICMP + 3
            IF ( .NOT. ZL(JCINL-1+IAD) ) THEN
               CALL JENUNO ( JEXNUM(NOMMAI,IMA), K8B )
               CALL JENUNO ( JEXNUM(NOMNOE,INO), NOEUD )
               CALL UTDEBM('F','RC36AC','ERREUR DONNEES ')
               CALL UTIMPK('L','POUR LA MAILLE ',1,K8B)
               CALL UTIMPK('S',' ET LE NOEUD ',1,NOEUD)
               IF ( ICMP .EQ. 1) THEN
            CALL UTIMPK('L','IL MANQUE L''INDICE DE CONTRAINTES',1,'K1')
               ELSEIF ( ICMP .EQ. 2) THEN
            CALL UTIMPK('L','IL MANQUE L''INDICE DE CONTRAINTES',1,'K2')
               ELSE
            CALL UTIMPK('L','IL MANQUE L''INDICE DE CONTRAINTES',1,'K3')
               ENDIF
               CALL UTFINM
            ENDIF
            K(ICMP) = ZR(JCINV-1+IAD)
 202      CONTINUE
C
C ------- LES CARATERISTIQUES : INERTIE, DIAMETRE, EPAISSEUR
C
          DO 204 ICMP = 2 , 4
            IAD = DECCA + (IPT-1)*NBCCA + ICMP
            IF ( .NOT. ZL(JCCAL-1+IAD) ) THEN
               CALL JENUNO ( JEXNUM(NOMMAI,IMA), K8B )
               CALL JENUNO ( JEXNUM(NOMNOE,INO), NOEUD )
               CALL UTDEBM('F','RC36AC','ERREUR DONNEES ')
               CALL UTIMPK('L','POUR LA MAILLE ',1,K8B)
               CALL UTIMPK('S',' ET LE NOEUD ',1,NOEUD)
         CALL UTIMPK('L','IL MANQUE DES CARATERISTIQUES ',1,'CARA_ELEM')
               CALL UTFINM
            ENDIF
            CARA(ICMP-1) = ZR(JCCAV-1+IAD)
 204      CONTINUE
C
          SM    = 0.D0
          SNMAX = 0.D0
          SAMAX = 0.D0
          UTOT  = 0.D0
C
C ----------------------------------------------------------------------
C                           E T A P E   1   
C ----------------------------------------------------------------------
C
C ------- ON TRAITE LES SITUATIONS COMBINABLES DANS LEUR GROUPE
C         -----------------------------------------------------
C
          DO 100 IG = 1 , NBGR
C
            NUMGR = ZI(JNUMGR+IG-1)
            IOCS  = ZI(JSEIGR+IG-1)
C
            NPASS = 0
            SEISME = .FALSE.
C
C --------- PASSAGE 1 : PRISE EN COMPTE DU SEISME, 
C                       CALCUL DU FACTEUR D'USAGE -> UTOT
C
            IF ( IOCS .NE. 0 ) THEN
              SNB = 0.D0
              SAB = 0.D0
              SEISME = .TRUE.
              CALL RC3601 ( NUMGR, IOCS, SEISME, NPASS, IMA, IPT, NBM,
     +                 ZI(ADRM),C, K, CARA, NOMMAT, SNB, SAB, UTOT, SM )
              SEISME = .FALSE.
            ENDIF
C
C --------- PASSAGE 2 : SANS LE SEISME 
C                       CALCUL SU SN_MAX
C                       CALCUL SU SALT_MAX
C                       CALCUL DU FACTEUR D'USAGE -> UTOT
C
            CALL RC3601 ( NUMGR, IOCS, SEISME, NPASS, IMA, IPT, NBM,
     +            ZI(ADRM), C, K, CARA, NOMMAT, SNMAX, SAMAX, UTOT, SM )
C
 100      CONTINUE
C
C ----------------------------------------------------------------------
C                           E T A P E   2 
C ----------------------------------------------------------------------
C
          MSE(1) = 0.D0
          MSE(2) = 0.D0
          MSE(3) = 0.D0
C
C ------- ON TRAITE LES SITUATIONS NON COMBINABLES
C         ----------------------------------------
C
          DO 200 IG = 1 , NBGR
C
            NUMGR = ZI(JNUMGR+IG-1)
C
            CALL JELIRA (JEXNUM('&&RC3600.LES_GROUPES',NUMGR),
     +                                             'LONMAX',NBSIGR,K8B)
            CALL JEVEUO (JEXNUM('&&RC3600.LES_GROUPES',NUMGR),'L',JNSG)
C
            NPASS = 0
C
            DO 210 IS1 = 1 , NBSIGR
              IOC1 = ZI(JNSG+IS1-1)
              IF ( ZL(JCOMBI+IOC1-1) )  GOTO 210
C
              NOCC = ZI(JNBOCC+2*IOC1-2)
C
              PPI = ZR(JPRESA+IOC1-1)
              MOMEPI = ZK24(JMOMEA+IOC1-1)
              CALL RCMO01 ( MOMEPI, IMA, IPT, MPI )
              MATEPI = ZK24(JCHMAT+2*IOC1-1)
              CALL RCMA01 ( MATEPI, IMA, IPT, NBM, ZI(ADRM), MATPI )
C
              PPJ = ZR(JPRESB+IOC1-1)
              MOMEPJ = ZK24(JMOMEB+IOC1-1)
              CALL RCMO01 ( MOMEPJ, IMA, IPT, MPJ )
              MATEPJ = ZK24(JCHMAT+2*IOC1-2)
              CALL RCMA01 ( MATEPJ, IMA, IPT, NBM, ZI(ADRM), MATPJ )
C
              CALL JELIRA ( JEXNUM('&&RC3600.SITU_THERMIQUE',IOC1),
     +                                           'LONUTI', NBTH1, K8B )
              IF ( NBTH1 .NE. 0 ) THEN
                CALL JEVEUO ( JEXNUM('&&RC3600.SITU_THERMIQUE',IOC1),
     +                                                      'L', JTH1 )
              ELSE
                JTH1 = 1
              ENDIF
C
              NBTH2 = 0
              JTH2  = 1
              IOC2=0
C
C ----------- CALCUL DU SN
C
              SN = 0.D0
              CALL RC36SN ( NBM, ZI(ADRM), IPT, C, CARA, MATPI, PPI, 
     +                      MPI, MATPJ, PPJ, MPJ, MSE, NBTH1, ZI(JTH1),
     +                                 NBTH2, ZI(JTH2),IOC1,IOC2, SN )
              SNMAX = MAX ( SNMAX , SN )
C
C ----------- CALCUL DU SP
C
              TYPEKE=MATPI(14)
              SP = 0.D0
        SPMECA = 0.D0
        SPTHER = 0.D0
              CALL RC36SP ( NBM, ZI(ADRM), IPT, C, K, CARA, MATPI, PPI,
     +                      MPI, MATPJ, PPJ, MPJ, MSE, NBTH1, ZI(JTH1),
     +                            NBTH2, ZI(JTH2),IOC1,IOC2, SP 
     +               ,TYPEKE,SPMECA,SPTHER )
C
C ----------- CALCUL DU SALT
C
              CALL RC36SA ( NOMMAT, MATPI, MATPJ, SN, SP
     +               ,TYPEKE,SPMECA,SPTHER, SALTIJ, SMM )
C
              IF ( SALTIJ .GT. SAMAX ) THEN
                 SAMAX = SALTIJ
                 SM = SMM
              ENDIF
C
C ----------- CALCUL DU FACTEUR D'USAGE
C
              CALL LIMEND( NOMMAT,SALTIJ,ENDUR)
              IF (ENDUR) THEN
                  UG=0.D0
              ELSE
                 CALL RCVALE ( NOMMAT, 'FATIGUE', 1, 'SIGM', SALTIJ, 1,
     +                                   'WOHLER', NADM, CODRET, 'F ' )
C
                 UG = DBLE( NOCC ) / NADM
              ENDIF
              UTOT = UTOT + UG
C
 210        CONTINUE
C
 200      CONTINUE
C
C ----------------------------------------------------------------------
C                           E T A P E   3 
C ----------------------------------------------------------------------
C
C ------- ON TRAITE LES SITUATIONS DE PASSAGE
C         -----------------------------------
C
          DO 310 IG = 1 , NBGR
C
            NUMGR = ZI(JNUMGR+IG-1)
C
            CALL JELIRA (JEXNUM('&&RC3600.LES_GROUPES',NUMGR),
     +                                             'LONMAX',NBSIGR,K8B)
            CALL JEVEUO (JEXNUM('&&RC3600.LES_GROUPES',NUMGR),'L',JNSG)
C
            DO 320 IS = 1 , NBSIGR
              IOC1 = ZI(JNSG+IS-1)
              IF ( ZI(JPASSA+2*IOC1-2).EQ.0 .AND.
     +             ZI(JPASSA+2*IOC1-1).EQ.0 )  GOTO 320
C
              NUM1 = ZI(JPASSA+2*IOC1-2)
              NUM2 = ZI(JPASSA+2*IOC1-2)
C
              NPASS = ZI(JNBOCC+2*IOC1-2)
C
              CALL RC3603 ( NUM1, NUM2, NPASS, IMA, IPT, NBM, ZI(ADRM),
     +                      C, K, CARA, NOMMAT, SNMAX, SAMAX, UTOT, SM )
C
 320        CONTINUE
C
 310      CONTINUE
C
C ----------------------------------------------------------------------
C
C ------- ON STOCKE LES RESULTATS DE CALCUL
C         ---------------------------------
C
C         - LE SM
          ICMP = 1
          IAD = DECRS + (IPT-1)*NBCRS + ICMP
          ZR(JCESV-1+IAD) = SM
C         - LE SN
          ICMP = 2
          IAD = DECRS + (IPT-1)*NBCRS + ICMP
          ZR(JCESV-1+IAD) = SNMAX
C         - LE SN/3SM
          ICMP = 3
          IAD = DECRS + (IPT-1)*NBCRS + ICMP
          IF ( SM .EQ. 0.D0 ) THEN
            ZR(JCESV-1+IAD) = 0.D0
          ELSE
            ZR(JCESV-1+IAD) = SNMAX / ( 3 * SM )
          ENDIF
C         - LE SALT
          ICMP = 4
          IAD = DECRS + (IPT-1)*NBCRS + ICMP
          ZR(JCESV-1+IAD) = SAMAX
C         - LE U_TOTAL
          ICMP = 5
          IAD = DECRS + (IPT-1)*NBCRS + ICMP
          ZR(JCESV-1+IAD) = UTOT
C
 20     CONTINUE
C
 10   CONTINUE
C
 1000 FORMAT(A,A8,A,A8)
 1010 FORMAT(1P,' COMBINAISON P ',I4,' SN =',E12.5,' SP =',E12.5)
 1020 FORMAT(1P,' COMBINAISON P Q ',I4,I4,' SN =',E12.5)
 1031 FORMAT(1P,'                 I I ',' SP =',E12.5)
 1032 FORMAT(1P,'                 I J ',' SP =',E12.5)
 1033 FORMAT(1P,'                 J J ',' SP =',E12.5)
 1034 FORMAT(1P,'                 J I ',' SP =',E12.5)
C
      CALL JEDEMA( )
      END
