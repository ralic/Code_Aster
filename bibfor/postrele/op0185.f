      SUBROUTINE OP0185 ( IER )
C
      IMPLICIT     NONE
      INTEGER      IER
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 11/03/2003   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C
C     COMMANDE DERTUY
C     DER : DIAGRAMME D'EVALUATION DE LA RUPTURE
C     CALCUL D'UN FACTEUR DE CORRECTION ELASTOPLASTIQUE DE KI A PARTIR
C     DE LA REGLE R6.
C
C     FONCTION REALISEE :
C     A. RECUPERATION DE DONNEES MATERIAU DANS UN CONCEPT (MATER)
C     B. CREATION DE LA COURBE KR(LR)
C     C. LECTURE DES DONNEES GEOMETRIQUES ET DU DOMAINE DE VALIDITE
C        DE LA METHODE
C     D. APPEL A LA SOUS ROUTINE DERTUY
C
C     ------------------------------------------------------------------
C
C  OUT  : IER   = 0 => TOUT S EST BIEN PASSE
C         IER   > 0 => NOMBRE D ERREURS RENCONTREES
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C     -------------------- DEBUT DES DECLARATIONS ----------------------
C
      INTEGER      IVALK,IADF1,IADF2,IRET
      INTEGER      IGEOM,NBMAT
      INTEGER      NBVAL1,NBVAL2,NBVAL3,NVF1,NBPOIN,NBPOI1
      INTEGER      NBVALA,NBVALB,NBVALC,NBVALD,NBVALE
      INTEGER      K
C
      REAL*8       VAL1(2),VAL2(1),VAL3(3),SIGH,SIGU,SIGY,E,T0
      REAL*8       EPAI,LFIS,LLIG,DEXT,RINT
      REAL*8       Z1,Z2,Z3,RBID
C
      CHARACTER*2  CODRET(2)
      CHARACTER*8  RESULT,NOMMAT,NOVAL1(2),NOVAL2(1),NOVAL3(3),KBID
      CHARACTER*16 CONCEP,NOMCMD
      CHARACTER*19 NOMF1,NOMF2
      CHARACTER*24 NOBJF0,NOBJF1
C
C     -------------------- FIN DES DECLARATIONS ------------------------
C
      CALL JEMARQ ( )
      CALL INFMAJ()
C
C --------- RECUPERATION DU NOM DU RESULTAT ASSOCIE --------------------
C --------- AU CONCEPT (MATER) ET A LA COMMANDE DEFI_MATERIAU ----------
C
      CALL GETRES(RESULT,CONCEP,NOMCMD)
C
C A------- RECUPERATION DES DONNEES MATERIAU ---------------------------
C
      NOMMAT = ' '
      CALL GETVID(' ', 'MATER', 0, 1, 1, NOMMAT, NBMAT)
C
C --1-- RECUPERE LE MODULE D'YOUNG E
      NBVAL1 = 2
      NOVAL1(1) = 'E'
      NOVAL1(2) = 'NU'
      CALL RCVALE(NOMMAT,'ELAS',0,' ',RBID,NBVAL1,NOVAL1,VAL1,CODRET,
     &             'F ')
      IF ( CODRET(1).NE.'OK' .OR. CODRET(2).NE.'OK' )
     &  CALL UTMESS('F','OP0185_01','MODULE D''YOUNG ABSENT')
      E = VAL1(1)
C
C --2-- TEST LA PRESENCE DE RCCM OU DE RCCM_FO
      NOBJF0 = ' '
C
      NOBJF0 = NOMMAT//'.RCCM_FO  .VALK'
      CALL JEEXIN(NOBJF0,IRET)
      IF ( IRET .GT. 0 ) THEN
C -3.1- L'OPTION RCCM_FO EST UTILISEE
        CALL GETVR8 ('DEF_EQUI', 'TEMP_ANALYSE', 1, 1, 1, T0, NBVALA)
C ----- RECUPERE SY_02, S ET SU
        NBVAL3 = 3
        NOVAL3(1) = 'SY_02'
        NOVAL3(2) = 'SU'
        NOVAL3(3) = 'S'
        CALL RCVALE(NOMMAT,'RCCM_FO',1,'TEMP',T0,NBVAL3,NOVAL3,VAL3,
     &              CODRET,'F ')
        IF ( CODRET(1).NE.'OK' .OR. CODRET(2).NE.'OK' )
     &    CALL UTMESS('F','OP0185_02','CARACTERISTIQUES'
     &                //' MATERIAUX ABSENTES')
        SIGY = VAL3(1)
        SIGU = VAL3(2)
      ELSE
C -3.2- L'OPTION RCCM EST UTILISEE
C ----- RECUPERE SY_02, SH ET SU
        NBVAL3 = 3
        NOVAL3(1) = 'SY_02'
        NOVAL3(2) = 'SU'
        NOVAL3(3) = 'SH'
        CALL RCVALE(NOMMAT,'RCCM',0,' ',RBID,NBVAL3,NOVAL3,VAL3,
     &              CODRET,'F ')
        IF ( CODRET(1).NE.'OK' .OR. CODRET(2).NE.'OK' )
     &    CALL UTMESS('F','OP0185_03','CARACTERISTIQUES'
     &                //' MATERIAUX ABSENTES')
        SIGY = VAL3(1)
        SIGU = VAL3(2)
        SIGH = VAL3(3)
      ENDIF
C
C --4-- DUPLIQUE LA COURBE DE TRACTION
      NOMF1 = ' '
      NOBJF1 = ' '
C
      NOBJF1 = NOMMAT//'.TRACTION  .VALK'
      CALL JEEXIN(NOBJF1,IRET)
      IF (IRET.EQ.0)
     &  CALL UTMESS ('F', 'OP0185_04', 'LA COURBE DE TRACTION'
     &            //' EST ABSENTE DE LA DEFINITION DU MATERIAU')
      CALL JEVEUO(NOBJF1, 'L', IVALK)
      IF ( 'SIGM' .EQ. ZK8(IVALK) ) THEN
        NOMF1 = ZK8(IVALK+1)
      ELSE
        CALL UTMESS ('F', 'OP0185_05', 'LA COURBE DE TRACTION'
     &              //' EST ABSENTE DE LA DEFINITION DU MATERIAU')
      ENDIF
C
      NOMF2 = '&&OP0185.KRLR'
      CALL JEDUPC('G', NOMF1, 1, 'V', NOMF2, .TRUE.)
C
C B------- CREATION DE LA COURBE KR(LR) --------------------------------
C
      CALL JEVEUO(NOMF1//'.VALE', 'L', IADF1)
      CALL JELIRA (NOMF1//'.VALE', 'LONMAX', NVF1, KBID)
C
      NBPOIN = NVF1/2
      NBPOI1 = NVF1/2 + 1
C
      CALL JEDETR (NOMF2//'.VALE')
      CALL WKVECT(NOMF2//'.VALE', 'V V R', 2*NBPOI1, IADF2)
C
      ZR(IADF2) = 0.D0
      ZR(IADF2+NBPOI1) = 1.D0
      DO 100 K=1, NBPOIN
C ----- LR -------------------------------------------------------------
        Z1 = ZR(IADF1+NBPOIN+K-1)/SIGY
C ----- ZR -------------------------------------------------------------
        Z2 = (E*ZR(IADF1+K-1))/ZR(IADF1+NBPOIN+K-1)
        Z3 = (SIGY*Z1**3)/(2*E*ZR(IADF1+K-1))
C
        ZR(IADF2+K) = Z1
        ZR(IADF2+NBPOI1+K) = 1.D0 / SQRT(Z2+Z3)
 100  CONTINUE
C
C
C C-------- RECUPERATION DES DONNEES GEOMETRIQUES ----------------------
C
C
      CALL GETVR8 ('DEF_EQUI', 'EPAIS', 1, 1, 1, EPAI, NBVALB)
C
      CALL GETVR8 ('DEF_EQUI', 'LONG_FISS', 1, 1, 1, LFIS, NBVALC)
C
      CALL GETVR8 ('DEF_EQUI', 'LONG_LIGA_INT', 1, 1, 1, LLIG, NBVALD)
C
      CALL GETVR8 ('DEF_EQUI', 'DEXT', 1, 1, 1, DEXT, NBVALE)
C
C     ----- CALCUL DU RAYON INTERNE ------------------------------------
C
      RINT = (DEXT/2.D0) - EPAI
C
C     ----- TEST DES CAS OU LA METHODE NE FONCTIONNE PAS ---------------
C
      IF ( (LFIS/EPAI) .GT. 0.25D0 ) THEN
        CALL UTMESS ('F', 'OP0185_06', 'LA FISSURE EST TROP LONGUE PAR'
     &               //' RAPPORT A L''EPAISSEUR : 2A/T > 1/4')
      END IF
C
      IF ( (LLIG/LFIS) .LT. 0.2D0 ) THEN
        CALL UTMESS ('F', 'OP0185_07', 'LA FISSURE EST TROP LONGUE PAR'
     &               //' RAPPORT AU LIGAMENT : S/2A < 0.2')
      END IF
C
      IF ( LLIG .LT. 2.D0 ) THEN
        CALL UTMESS ('F', 'OP0185_08', 'LA LONGUEUR DU LIGAMENT EST'
     &               //' INFERIEURE A 2 MM')
      END IF
C
C
C D------- APPEL A LA SOUS ROUTINE DERTUY ------------------------------
C
C
      CALL DERTUY(E, SIGY, SIGU, SIGH, ZR(IADF1), ZR(IADF1+NBPOIN),
     &            ZR(IADF2), ZR(IADF2+NBPOI1), EPAI, LFIS, LLIG, RINT,
     &            NBPOIN, NBPOI1, RESULT)
C
C
      CALL JEDETC('V','&&OP0185',1)
      CALL JEDEMA ( )
C
      END
