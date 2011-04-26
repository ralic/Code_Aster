      SUBROUTINE MESTAT(MODELZ,FOMULZ,LISCHZ,MATE,CARAZ,LTPSZ,SOLVEZ,
     &                  NBPASE,INPSCO,COMPOR  )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
C ---------------------------------------------------------------------
C     BUT:  FAIRE UN CALCUL DE MECANIQUE STATIQUE : K(T)*U = F(T)
C           POUR LES DIFFERENTS INSTANTS "T" DE LTPS.
C     IN: MODELZ : NOM D'1 MODELE
C         FOMULZ : LISTE DES FONCTIONS MULTIPLICATRICES
C         LISCHZ : INFORMATION SUR LES CHARGEMENTS
C         MATE   : NOM DU MATERIAU
C         CARAZ  : NOM D'1 CARAC_ELEM
C         LTPSZ  : LISTE DES INSTANTS DE CALCUL
C         SOLVEZ : METHODE DE RESOLUTION 'LDLT' OU 'GCPC'
C         NBPASE : NOMBRE DE PARAMETRES SENSIBLES
C         INPSCO : SD CONTENANT LA LISTE DES NOMS POUR LA SENSIBILITE
C         COMPOR : COMPOR POUR LES MULTIFIBRE (POU_D_EM)
C
C     OUT: L'EVOL_ELAS  EST REMPLI (POUR SA PARTIE 'DEPL')
C ---------------------------------------------------------------------
      IMPLICIT NONE

C 0.1. ==> ARGUMENTS

      INTEGER       NBPASE
      CHARACTER*(*) MODELZ,FOMULZ,LISCHZ,MATE,CARAZ,LTPSZ,SOLVEZ,INPSCO
      CHARACTER*8   LTPS
      CHARACTER*19  LISCHA, SOLVEU
      CHARACTER*24  COMPOR

C 0.2. ==> COMMUNS

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  -------------------------
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
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  -------------------------

C 0.3. ==> VARIABLES LOCALES

      CHARACTER*6  NOMPRO
      PARAMETER    (NOMPRO = 'MESTAT')
      INTEGER      NBVAL,IBID,IERD,JVAL,ITPS,
     &             ITPS0,IRET,NRPASE,NRORES,NINSTC,ISLVK,NEQ,
     &             VALI, ETAUSR
      REAL*8       TIME,INSTF,R8MAEM,TPS1(4),TPS2(4),TPS3(4),TCPU,
     &             PARTPS(3),RBID, VALR(3)
      CHARACTER*1  BASE
      CHARACTER*8  K8BID,REPK,RESULT
      CHARACTER*14 NUPOSS
      CHARACTER*16 K16BID
      CHARACTER*19 MATASS,MAPREC,VECASS,CHDEPL,K19B
      CHARACTER*24 NUMEDD,METHOD,CRITER,OPT,K24B,
     &             MODELE,CARELE,FOMULT,NOOJB
      LOGICAL      MATCST,ASSMAT,LFETI
      LOGICAL      LBID, LTEMP, LHYDR, LSECH

C DEB------------------------------------------------------------------
C====
C 1. PREALABLES
C====
C
      CALL JEMARQ()

C 1.1. ==> LES ARGUMENTS

      SOLVEU = SOLVEZ
      MODELE = MODELZ
      CARELE   = CARAZ
      FOMULT = FOMULZ
      LISCHA = LISCHZ
      LTPS   = LTPSZ

C 1.2. ==> LES CONSTANTES

C               12   345678   90123456789
      VECASS = '&&'//NOMPRO//'.2NDMBR_ASS'
      MAPREC = '&&'//NOMPRO//'_MAT_PRECON'
      MATASS = '&&'//NOMPRO//'_MATR_ASSEM'

      PARTPS(2) = 0.D0
      PARTPS(3) = 0.D0
      BASE   = 'V'
      CRITER = '&&RESGRA_GCPC'


C 1.3. ==> ALLOCATION DES RESULTATS
      CALL JELIRA(LTPS//'           .VALE','LONMAX',NBVAL,K8BID)
      CALL UTCRRE ( NBPASE, INPSCO, NBVAL )

C 1.4. ==> ON REGARDE SI LE MATERIAU EST UNE FONCTION DU TEMPS
C     (DEPENDANCE AVEC LA TEMPERATURE, HYDRATATION, SECHAGE)

      CALL NMVCD2 ( 'HYDR',MATE,LHYDR, LBID )
      CALL NMVCD2 ( 'SECH',MATE,LSECH, LBID )
      CALL NMVCD2 ( 'TEMP',MATE,LTEMP, LBID )


C     -- LE MATERIAU (ELAS) PEUT-IL CHANGER AU COURS DU TEMPS ?
      CALL DISMOI('F','ELAS_FO',MATE,'CHAM_MATER',IBID,REPK,IERD)
      MATCST=(.NOT.(REPK.EQ.'OUI'))

C 2.2. ==> NUMEROTATION ET CREATION DU PROFIL DE LA MATRICE
C FETI OR NOT FETI ?
      CALL JEVEUO(SOLVEU(1:19)//'.SLVK','L',ISLVK)
      METHOD=ZK24(ISLVK)
C SI FETI, INITIALISATION DES OBJETS TEMPORAIRES DE MONITORING
      IF (METHOD(1:4).EQ.'FETI') THEN
        LFETI=.TRUE.
      ELSE
        LFETI=.FALSE.
      ENDIF

      NUMEDD=  '12345678.NUMED'
      NOOJB='12345678.00000.NUME.PRNO'
      CALL GNOMSD ( NOOJB,10,14 )
      NUMEDD=NOOJB(1:14)

      CALL GETRES (RESULT,K16BID,K16BID)
      CALL RSNUME(RESULT,'DEPL',NUPOSS)

      CALL NUMERO ( NUPOSS,MODELE,LISCHA,SOLVEU,'VG',NUMEDD )

      CALL VTCREB(VECASS,NUMEDD,'V','R',NEQ)

C     ??? IL SERAIT PEUT ETRE BON DE VERIFIER QUE QUELQUE CHOSE BOUGE
C     AVEC LE TEMPS. POUR L'INSTANT ON RECALCULE LE 2EME MEMBRE A CHA
C     QUE FOIS.

      CALL UTTCPU('CPU.OP0046.1', 'INIT',' ')
      CALL UTTCPU('CPU.OP0046.2', 'INIT',' ')
      CALL UTTCPU('CPU.OP0046.3', 'INIT',' ')

      CALL JEVEUO(LTPS//'           .VALE','L',JVAL)
      INSTF=R8MAEM()
      CALL GETVR8(' ','INST_FIN',0,1,1,INSTF,IBID)


C====
C 2. BOUCLE 2 SUR LES PAS DE TEMPS
C====

      NINSTC=0
      DO 2 , ITPS = 1 , NBVAL
        CALL JERECU('V')

C       SI LE PAS DE TEMPS A DEJA ETE CALCULE, ON SAUTE L'ITERATION
        CALL RSEXCH(RESULT,'DEPL',ITPS,CHDEPL,IRET)
        IF (IRET.EQ.0) GO TO 2

C 2.1. ==> L'INSTANT
        ITPS0 = ITPS
        TIME = ZR(JVAL-1+ITPS)

C       -- SI ON A DEPASSE INSTF, ON SORT :
        IF (TIME .GT. INSTF) GO TO 3

        NINSTC=NINSTC+1
        PARTPS(1) = TIME

C 2.2. ==> BOUCLE 22 SUR LES RESOLUTIONS
C          LE PREMIER PASSAGE, 0, EST CELUI DU CALCUL STANDARD
C          LES PASSAGES SUIVANTS SONT CEUX DES DERIVATIONS

        DO 22 , NRORES = 0 , NBPASE

          NRPASE = NRORES

C 2.2.1. ==> Y-A-T'IL ASSEMBLAGE DES MATRICES ?

          IF ( NRORES.EQ.0 ) THEN
            IF ( .NOT.MATCST .OR. NINSTC.EQ.1 ) THEN
              ASSMAT = .TRUE.
            ELSE
              ASSMAT = .FALSE.
            ENDIF
          ELSE
            ASSMAT = .FALSE.
          ENDIF

C 2.2.2. ==> RESOLUTION NUMERO NRPASE
          CALL MERESO ( NRPASE, NBPASE, INPSCO,
     >                  MODELE, MATE, CARELE, FOMULT, LISCHA,
     >                  ITPS0, PARTPS,
     >                  NUMEDD, VECASS,
     >                  ASSMAT, SOLVEU, MATASS, MAPREC,
     >                  BASE, COMPOR )

   22   CONTINUE

C       -- IMPRESSION EVENTUELLE DES MESURES DE TEMPS:
        CALL UTTCPG('IMPR','INCR')
C
C       --- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1
C
        IF ( ETAUSR().EQ.1 ) THEN
           CALL SIGUSR()
        ENDIF

C 2.3. ==> CONTROLE DU TEMPS CPU

        CALL UTTCPR('CPU.OP0046.1', 4, TPS1)
        CALL UTTCPR('CPU.OP0046.2', 4, TPS2)
        CALL UTTCPR('CPU.OP0046.3', 4, TPS3)
        IF ( .NOT.MATCST .OR. NINSTC.EQ.1 ) THEN
          TCPU = TPS1(4) + TPS2(4) + TPS3(4)
        ELSE
          TCPU = TPS3(4)
        ENDIF
        IF ( NBVAL .GT. 1    .AND.
     >       ITPS .LT. NBVAL .AND.
     >       TCPU .GT. .95D0*TPS3(1)  ) THEN
             VALI     = ITPS
             VALR (1) = TIME
             VALR (2) = TCPU
             VALR (3) = TCPU
             CALL UTEXCM(28,'ALGORITH16_88',0,' ',1,VALI,3,VALR)
          CALL U2MESS('F','ALGORITH11_83')
          GOTO 9999
        ENDIF

    2 CONTINUE
    3 CONTINUE

C        -- MENAGE DES OBJETS PROVISOIRES:
 9999 CONTINUE
      CALL DETRSD ( 'CHAMP_GD',  VECASS )
      CALL DETRSD ( 'MATR_ASSE', MATASS )

      IF (LFETI) THEN
C NETTOYAGE DES SD FETI SI NECESSAIRE (SUCCESSION DE CALCULS DECOUPLES)
C ET INITIALISATION NUMERO D'INCREMENT
        OPT='NETTOYAGE_SDT'
        CALL ALFETI(OPT,K19B,K19B,K19B,K19B,IBID,RBID,K24B,
     &              RBID,IBID,K24B,K24B,K24B,K24B,IBID,K24B,K24B,IBID)
      ENDIF

      CALL JEEXIN (CRITER(1:19)//'.CRTI',IRET)
      IF ( IRET .NE. 0 ) THEN
         CALL JEDETR ( CRITER(1:19)//'.CRTI' )
         CALL JEDETR ( CRITER(1:19)//'.CRTR' )
         CALL JEDETR ( CRITER(1:19)//'.CRDE' )
      ENDIF
      CALL JEDEMA()
      END
