      SUBROUTINE MDNEWM (NBPAS,DT,NBMODE,PULSAT,PULSA2,
     &                   MASGEN,RIGGEN,LAMOR,AMOGEN,TYPBAS,BASEMO,
     &                   TINIT,IPARCH,DEPSTO,
     &                   VITSTO,ACCSTO,IORSTO,TEMSTO,NOMRES,NBEXCI,
     &                   IDESCF,NOMFON,COEFM,LIAD,INUMOR)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER      IORSTO(*), IPARCH(*),IDESCF(*)
      REAL*8       PULSAT(*),PULSA2(*),MASGEN(*),RIGGEN(*),
     &             AMOGEN(*),DEPSTO(*),VITSTO(*),ACCSTO(*),TEMSTO(*)
      CHARACTER*8  BASEMO,NOMRES,NOMFON(*)
      CHARACTER*16 TYPBAS
      LOGICAL      LAMOR, LPSTO
      INTEGER      DESCMM,DESCMR,DESCMA,LIAD(*),INUMOR(*)
      REAL*8       R8B,COEFM(*)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C TOLE CRP_21
C
C     ALGORITHME DE NEWMARK
C     ------------------------------------------------------------------
C IN  : NBPAS  : NOMBRE DE PAS
C IN  : DT     : PAS DE TEMPS
C IN  : NBMODE : NOMBRE DE MODES
C IN  : PULSAT : PULSATIONS MODALES
C IN  : PULSA2 : PULSATIONS MODALES AU CARREES
C IN  : MASGEN : MASSES GENERALISEES ( TYPBAS = 'MODE_MECA' )
C                MATRICE DE MASSE GENERALISEE ( TYPBAS = 'BASE_MODA' )
C IN  : RIGGEN : RAIDEURS GENERALISES ( TYPBAS = 'MODE_MECA' )
C                MATRICE DE RAIDEUR GENERALISE ( TYPBAS = 'BASE_MODA' )
C IN  : LAMOR  : AMORTISSEMENT SOUS FORME D'UNE LISTE DE REELS
C IN  : AMOGEN : AMORTISSEMENTS REDUITS ( LAMOR = .TRUE. )
C                MATRICE D'AMORTISSEMENT ( LAMOR = .FALSE. )
C IN  : TYPBAS : TYPE DE LA BASE 'MODE_MECA' OU 'BASE_MODA'
C IN  : NBEXCI : NBRE D'EXCITATIONS (SOUS LE MC EXCIT ET EXCIT_RESU)
C IN  : IDESCF : TYPE D'EXCITATION (VECT_ASSE/NUME_ORDRE,FONC_MULT/
C                COEF_MULT)
C IN  : NOMFON : NOM DES FONC_MULT (QUAND IL Y EN A)
C IN  : COEFM  : VALEUR DU COEF_MULT
C IN  : LIAD   : VALEUR DU VECT_ASSE
C IN  : NUMOR  : NUME_ORDRE DU MODE EXCITE
C ----------------------------------------------------------------------
C
      REAL*8      TPS1(4),VALR(3),BETA,GAMMA,RES,TOL,ACCE
      INTEGER     VALI(2),N1, IFM, NIV
      INTEGER     ETAUSR
      CHARACTER*8 TRAN
      INTEGER      IARG
C     ------------------------------------------------------------------
C
C 1.1. ==> RECUPERATION DU NIVEAU D'IMPRESSION

C-----------------------------------------------------------------------
      INTEGER I ,IA ,IARCHI ,IB ,IBID ,IBID1 ,IBID2 
      INTEGER IF ,IFE ,IM ,IM1 ,IND ,IPAS ,IPM 
      INTEGER IRET ,ISTO1 ,JACCE ,JDEPL ,JFEXT ,JM ,JMASS 
      INTEGER JTRA1 ,JTRA2 ,JTRA3 ,JTRA4 ,JTRA5 ,JTRA6 ,JVITE 
      INTEGER N100 ,NBBLOC ,NBEXCI ,NBMOD1 ,NBMODE ,NBPAS ,NBPASB 
      INTEGER NBPASF ,NBPP ,NDIM ,NDT 
      REAL*8 A0 ,A1 ,A2 ,A3 ,A4 ,A5 ,A6 
      REAL*8 A7 ,DEUX ,DT ,DT2 ,TARCHI ,TEMPS ,TINIT 
      REAL*8 X1 ,X2 ,X3 ,ZERO 
C-----------------------------------------------------------------------
      CALL INFNIV(IFM,NIV)
C
      CALL JEMARQ()
C      
      ZERO   = 0.D0
      DEUX   = 2.D0
      DT2    = DT * DT
C
      CALL GETVR8('SCHEMA_TEMPS','BETA',1,IARG,1,BETA,N1)
      CALL GETVR8('SCHEMA_TEMPS','GAMMA',1,IARG,1,GAMMA,N1)
      RES = 0.25D0* (0.5D0+GAMMA)* (0.5D0*GAMMA)
      TOL = 1.D-8
      IF ( GAMMA.LT.(0.5D0-TOL) .OR. BETA.LT.(RES-TOL) ) THEN
          WRITE (IFM,*) ' >>> NEWMARK <<<'//
     &      'CAS CONDITIONNELLEMENT STABLE.'
      END IF
      IF ( BETA.EQ.0) THEN
        CALL U2MESS('F','ALGORITH9_2')
      ENDIF  
C
      A0 = 1.D0/BETA/DT2
      A1 = GAMMA/BETA/DT
      A2 = 1.D0/BETA/DT
      A3=1.D0/(2*BETA)-1
      A4=GAMMA/BETA-1
      A5=DT/2*(GAMMA/BETA-2)
      A6=DT*(1-GAMMA)
      A7=GAMMA*DT
C      
      ISTO1  = 0
      LPSTO = .FALSE.
      R8B = ZERO
      NBMOD1 = NBMODE - 1
C
C     --- VECTEURS DE TRAVAIL ---
      CALL WKVECT('&&MDNEWM.DEPL','V V R8',NBMODE,JDEPL)
      CALL WKVECT('&&MDNEWM.VITE','V V R8',NBMODE,JVITE)
      CALL WKVECT('&&MDNEWM.ACCE','V V R8',NBMODE,JACCE)
      CALL WKVECT('&&MDNEWM.TRA1','V V R8',NBMODE,JTRA1)
      CALL WKVECT('&&MDNEWM.TRA2','V V R8',NBMODE,JTRA2)
      CALL WKVECT('&&MDNEWM.KTILDA','V V R8',NBMODE*NBMODE,JTRA3)
      CALL WKVECT('&&MDNEWM.FTILD1','V V R8',NBMODE*NBMODE,JTRA4)
      CALL WKVECT('&&MDNEWM.FTILD2','V V R8',NBMODE*NBMODE,JTRA5)
      CALL WKVECT('&&MDNEWM.FTILD3','V V R8',NBMODE*NBMODE,JTRA6)
C
C     --- A-T-ON ASSEZ DE PLACE POUR CREER LE VECTEUR "FEXT" ? ---
      CALL JEDISP(1,IPM)
      NDIM = NBMODE * NBPAS
      IF ( NDIM.LE.IPM ) THEN
C        --- ON ALLOUE LE VECTEUR ---
         NBBLOC = 1
         NBPASB = NBPAS
         NBPASF = NBPAS
         CALL WKVECT('&&MDNEWM.FEXT','V V R8',NDIM,JFEXT)
      ELSE
C        --- DECOUPAGE EN BLOC ---
         NBBLOC = NDIM / IPM
         NBPASB = IPM / NBMODE
         NBPASF = NBPAS - ( NBBLOC * NBPASB )
         NBBLOC = NBBLOC + 1
         NDIM = NBMODE * NBPASB
         CALL WKVECT('&&MDNEWM.FEXT','V V R8',NDIM,JFEXT)
         WRITE(6,*)'--->> MDNEWM: DECOUPAGE, NBBLOC=',NBBLOC
         WRITE(6,*)'--->>                    NBPASB=',NBPASB
         WRITE(6,*)'--->>                    NBPASF=',NBPASF
      ENDIF
C
      IF (TYPBAS.EQ.'MODE_MECA'.OR.TYPBAS.EQ.'MODE_GENE') THEN
         IF ( LAMOR ) THEN
            DO 100 IM = 1 , NBMODE
               AMOGEN(IM) = DEUX * AMOGEN(IM) * PULSAT(IM)
 100        CONTINUE
         ELSE
            DO 110 IM = 1 , NBMODE
               DO 112 JM = 1 , NBMODE
                  IND = JM + NBMODE*(IM-1)
                  ZR(JTRA3+IND-1) = A1 * AMOGEN(IND)
 112           CONTINUE
               IND = IM + NBMODE*(IM-1)
               ZR(JTRA3+IND-1) = ZR(JTRA3+IND-1) + A0*MASGEN(IM)
     &                                           + RIGGEN(IM)
 110        CONTINUE
C           --- FACTORISATION DE LA MATRICE KTILDA ---
            CALL TRLDS(ZR(JTRA3),NBMODE,NBMODE,IRET)
            IF (IRET.NE.0) CALL U2MESS('F','ALGORITH5_61')
         ENDIF
      ELSE
         IF ( LAMOR ) THEN
            DO 120 IM = 1 , NBMODE
               AMOGEN(IM) = DEUX * AMOGEN(IM) * PULSAT(IM)
               DO 122 JM = 1 , NBMODE
                  IND = JM + NBMODE*(IM-1)
                  ZR(JTRA3+IND-1) = A0*MASGEN(IND) + RIGGEN(IND)
                  ZR(JTRA4+IND-1) = A2*MASGEN(IND)
                  ZR(JTRA5+IND-1) = A0*MASGEN(IND)
                  ZR(JTRA6+IND-1) = A3*MASGEN(IND)
 122           CONTINUE
               IND = IM + NBMODE*(IM-1)
               ZR(JTRA3+IND-1) = ZR(JTRA3+IND-1) +
     &                           A1*AMOGEN(IM)*MASGEN(IND)
               ZR(JTRA4+IND-1) = ZR(JTRA4+IND-1) +
     &                           A4*AMOGEN(IM)*MASGEN(IND)
               ZR(JTRA5+IND-1) = ZR(JTRA5+IND-1) +
     &                           A1*AMOGEN(IM)*MASGEN(IND)
               ZR(JTRA6+IND-1) = ZR(JTRA6+IND-1) +
     &                           A5*AMOGEN(IM)*MASGEN(IND)
 120        CONTINUE
         ELSE
            DO 130 IM = 1 , NBMODE
               DO 132 JM = 1 , NBMODE
                  IND = JM + NBMODE*(IM-1)
                  ZR(JTRA3+IND-1) = A0*MASGEN(IND) + RIGGEN(IND)
     &                                         + A1*AMOGEN(IND)
                  ZR(JTRA4+IND-1) = A2*MASGEN(IND) + A4*AMOGEN(IND)
                  ZR(JTRA5+IND-1) = A0*MASGEN(IND) + A1*AMOGEN(IND)
                  ZR(JTRA6+IND-1) = A3*MASGEN(IND) + A5*AMOGEN(IND)
 132           CONTINUE
 130        CONTINUE
         ENDIF
C        --- FACTORISATION DE LA MATRICE MASSE ---
         CALL WKVECT('&&MDNEWM.MASS','V V R8',NBMODE*NBMODE,JMASS)
         CALL DCOPY(NBMODE*NBMODE,MASGEN,1,ZR(JMASS),1)
         CALL TRLDS(ZR(JMASS),NBMODE,NBMODE,IRET)
C         CALL TRLDS(MASGEN,NBMODE,NBMODE,IRET)
         IF (IRET.NE.0) CALL U2MESS('F','ALGORITH5_22')
C        --- FACTORISATION DE LA MATRICE KTILDA ---
         CALL TRLDS(ZR(JTRA3),NBMODE,NBMODE,IRET)
         IF (IRET.NE.0) CALL U2MESS('F','ALGORITH5_61')
      ENDIF
C
C     --- CONDITIONS INITIALES ---
      CALL MDINIT(BASEMO,NBMODE,0,ZR(JDEPL),ZR(JVITE),R8B, 
     &            IRET, TINIT)
      IF (IRET.NE.0) GOTO 9999
C
C     --- FORCES EXTERIEURES ---
      IF (NBEXCI.NE.0) THEN
         CALL MDFEXT(TINIT,DT,NBMODE,NBEXCI,IDESCF,NOMFON,COEFM,
     &               LIAD,INUMOR,1,ZR(JFEXT))
      ENDIF
C
C     --- ACCELERATIONS GENERALISEES INITIALES ---
      CALL MDACCE(TYPBAS,NBMODE,PULSA2,MASGEN,DESCMM,RIGGEN,
     &            DESCMR,ZR(JFEXT),LAMOR,AMOGEN,DESCMA,ZR(JTRA1),
     &            ZR(JDEPL),ZR(JVITE),ZR(JACCE))
C
C     --- ARCHIVAGE DONNEES INITIALES ---
      IBID1 = 0
      IBID2 = 0
      TARCHI = TINIT
      CALL MDARCH(ISTO1,0,TINIT,DT, NBMODE,ZR(JDEPL),ZR(JVITE),
     &            ZR(JACCE),IBID1,0,R8B,0,IBID2,0,R8B,IBID,
     &            DEPSTO,VITSTO,
     &            ACCSTO,R8B,LPSTO,IORSTO,TEMSTO, R8B,R8B,R8B, IBID,
     &            R8B, IBID, R8B )
C
      TEMPS = TINIT + DT
      CALL UTTCPU('CPU.MDNEWM','INIT',' ')
      N100 = NBPAS/100 + 1
      IPAS = 0
C
C     --- BOUCLE TEMPORELLE ---
      DO 10 IB = 1 , NBBLOC
         IA = ( IB - 1 ) * NBPASB
         IF (IB.EQ.NBBLOC) THEN
            NBPP = NBPASF
         ELSE
            NBPP = NBPASB
         ENDIF
         DO 20 IF = 0,NDIM-1
            ZR(JFEXT+IF) = ZERO
 20      CONTINUE
C
C        --- FORCES EXTERIEURES ---
         IF (NBEXCI.NE.0) THEN
             CALL MDFEXT(TEMPS,DT,NBMODE,NBEXCI,IDESCF,NOMFON,COEFM,
     &                   LIAD,INUMOR,NBPP,ZR(JFEXT))
         ENDIF
C
         DO 200 I = 1 , NBPP
C
            IF (MOD(IPAS,N100).EQ.0)
     &           CALL UTTCPU('CPU.MDNEWM','DEBUT',' ')
C
            IFE = ( I - 1 ) * NBMODE
            IF (TYPBAS.EQ.'MODE_MECA'.OR.TYPBAS.EQ.'MODE_GENE') THEN
               IF ( LAMOR ) THEN
                  DO 210 IM = 0,NBMOD1
                     IM1 = IM + 1
                     ZR(JTRA1+IM) = ZR(JDEPL+IM)
                     X1 = ( A2 + A4*AMOGEN(IM1) ) * MASGEN(IM1)
                     X2 = ( A0 + A1*AMOGEN(IM1) ) * MASGEN(IM1)
                     X3 = X2 + RIGGEN(IM1)
                     ZR(JDEPL+IM) = ( ZR(JFEXT+IFE+IM) + X1*ZR(JVITE+IM)
     &                                + A3*MASGEN(IM1)*ZR(JACCE+IM)
     &                                + A5*AMOGEN(IM1)*ZR(JACCE+IM)
     &                                + X2*ZR(JDEPL+IM) ) / X3
 210              CONTINUE
               ELSE
                  DO 212 IM = 0,NBMOD1
                     ZR(JTRA1+IM) = ZR(JDEPL+IM)
                     ZR(JTRA2+IM) = A4*ZR(JVITE+IM) + A1*ZR(JDEPL+IM)
     &                             +A5* ZR(JACCE+IM)               
 212              CONTINUE
                  CALL PMAVEC('ZERO',NBMODE,AMOGEN,ZR(JTRA2),ZR(JDEPL))
                  DO 214 IM = 0,NBMOD1
                     IM1 = IM + 1
                     X1 = A3*ZR(JACCE+IM) + A2*ZR(JVITE+IM)
     &                                   + A0*ZR(JTRA1+IM)
                     ZR(JDEPL+IM) = ZR(JDEPL+IM) + ZR(JFEXT+IFE+IM)
     &                                               + X1*MASGEN(IM1)
 214              CONTINUE
                  CALL RRLDS(ZR(JTRA3),NBMODE,NBMODE,ZR(JDEPL),1)
               ENDIF
            ELSE
               DO 216 IM = 0,NBMOD1
                  ZR(JTRA1+IM) = ZR(JDEPL+IM)
                  ZR(JDEPL+IM) = ZR(JFEXT+IFE+IM)
 216           CONTINUE
               CALL PMAVEC('CUMUL',NBMODE,ZR(JTRA6),ZR(JACCE),ZR(JDEPL))
               CALL PMAVEC('CUMUL',NBMODE,ZR(JTRA4),ZR(JVITE),ZR(JDEPL))
               CALL PMAVEC('CUMUL',NBMODE,ZR(JTRA5),ZR(JTRA1),ZR(JDEPL))
               CALL RRLDS(ZR(JTRA3),NBMODE,NBMODE,ZR(JDEPL),1)
            ENDIF
            DO 218 IM = 0,NBMOD1
               ACCE=ZR(JACCE+IM)
               ZR(JACCE+IM) = -A3*ACCE + A0*( ZR(JDEPL+IM)
     &                               - ZR(JTRA1+IM) - DT*ZR(JVITE+IM) )
C
               ZR(JVITE+IM) = ZR(JVITE+IM) + A6*ACCE + A7*ZR(JACCE+IM)

 218        CONTINUE
C
C           --- ARCHIVAGE ---
            IARCHI = IA + I
            IF (IPARCH(IARCHI) .EQ. 1) THEN
               ISTO1 = ISTO1 + 1
               TEMPS = TINIT + IARCHI*DT
              IBID2 = 0
              IBID1 = 0
              TARCHI = TEMPS
              CALL MDARCH(ISTO1,IARCHI,TEMPS,DT,NBMODE,ZR(JDEPL),
     &                     ZR(JVITE),ZR(JACCE), IBID1,0,R8B,0,
     &                     IBID2,0,R8B,IBID,
     &                     DEPSTO,VITSTO,ACCSTO,R8B,LPSTO,IORSTO,TEMSTO,
     &                     R8B,R8B,R8B,IBID, R8B, IBID,R8B )
            ENDIF
C
C       --- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1 ---
C
            IF ( ETAUSR().EQ.1 ) THEN
             CALL SIGUSR()
            ENDIF
C
C       --- TEST SI LE TEMPS RESTANT EST SUFFISANT POUR CONTINUER ---
C
            IF (MOD(IPAS,N100).EQ.0) THEN
             CALL UTTCPU('CPU.MDNEWM','FIN',' ')
             CALL UTTCPR('CPU.MDNEWM',4,TPS1)
C
             IF (MAX(5.D0,N100*TPS1(4)).GT.0.90D0*TPS1(1)) THEN
              CALL MDSIZE (NOMRES,ISTO1,NBMODE,LPSTO,0,0)
              IF (NOMRES.EQ.'&&OP0074') THEN
C             --- CAS D'UNE POURSUITE ---
                 CALL GETVID('ETAT_INIT','RESULTAT',1,IARG,1,TRAN,NDT)
                 IF (NDT.NE.0) CALL RESU74(TRAN,NOMRES)
              ENDIF
                VALI (1) = IA+I
                VALI (2) = ISTO1
                VALR (1) = TARCHI
                VALR (2) = TPS1(4)
                VALR (3) = TPS1(1)
                CALL UTEXCM(28,'ALGORITH16_77',0,' ',2,VALI,3,VALR)
              GOTO 9999
             ENDIF
C
            ENDIF
            IPAS = IPAS + 1
C
 200     CONTINUE
C
         TEMPS = TINIT + ( IA + NBPASB + 1 )*DT
C
 10   CONTINUE
C
 9999 CONTINUE
      CALL JEDETR('&&MDNEWM.DEPL')
      CALL JEDETR('&&MDNEWM.VITE')
      CALL JEDETR('&&MDNEWM.ACCE')
      CALL JEDETR('&&MDNEWM.TRA1')
      CALL JEDETR('&&MDNEWM.TRA2')
      CALL JEDETR('&&MDNEWM.KTILDA')
      CALL JEDETR('&&MDNEWM.FTILD1')
      CALL JEDETR('&&MDNEWM.FTILD2')
      CALL JEDETR('&&MDNEWM.FTILD3')
      CALL JEDETR('&&MDNEWM.FEXT')
      IF (IRET.NE.0)
     &   CALL U2MESS('F','ALGORITH5_24')
C
      CALL JEDEMA()
      END
