      SUBROUTINE MDNEWM (NBPAS,DT,NBMODE,PULSAT,PULSA2,
     +                   MASGEN,RIGGEN,LAMOR,AMOGEN,TYPBAS,BASEMO,
     +                   TINIT,IPARCH,
     +                   DEPSTO,VITSTO,ACCSTO,IORSTO,TEMSTO,NOMRES) 
      IMPLICIT     REAL*8 (A-H,O-Z)
      INTEGER      IORSTO(*), IPARCH(*)
      REAL*8       PULSAT(*),PULSA2(*),MASGEN(*),RIGGEN(*),
     +             AMOGEN(*),DEPSTO(*),VITSTO(*),ACCSTO(*),TEMSTO(*)
      CHARACTER*8  BASEMO,NOMRES
      CHARACTER*16 TYPBAS
      LOGICAL      LAMOR, LPSTO
      INTEGER      DESCMM,DESCMR,DESCMA
      REAL*8       R8B
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      REAL*8      TPS1(4)
      CHARACTER*8 TRAN
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      ZERO   = 0.D0
      DEUX   = 2.D0
      QUATRE = 4.D0
      DT2    = DT * DT
      DSDT   = DEUX / DT
      QSDT   = QUATRE / DT
      QSDT2  = QUATRE / DT2
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
                  ZR(JTRA3+IND-1) = DSDT * AMOGEN(IND)
 112           CONTINUE
               IND = IM + NBMODE*(IM-1)
               ZR(JTRA3+IND-1) = ZR(JTRA3+IND-1) + QSDT2*MASGEN(IM)
     +                                           + RIGGEN(IM)
 110        CONTINUE
C           --- FACTORISATION DE LA MATRICE KTILDA ---
            CALL TRLDS(ZR(JTRA3),NBMODE,NBMODE,IRET)
            IF (IRET.NE.0) CALL UTMESS('F','MDNEWM',
     +                              'LA MATRICE KTILDA EST SINGULIERE.')
         ENDIF
      ELSE
         IF ( LAMOR ) THEN
            DO 120 IM = 1 , NBMODE
               AMOGEN(IM) = DEUX * AMOGEN(IM) * PULSAT(IM)
               DO 122 JM = 1 , NBMODE
                  IND = JM + NBMODE*(IM-1)
                  ZR(JTRA3+IND-1) = QSDT2*MASGEN(IND) + RIGGEN(IND)
                  ZR(JTRA4+IND-1) = QSDT*MASGEN(IND)
                  ZR(JTRA5+IND-1) = QSDT2*MASGEN(IND)
 122           CONTINUE
               IND = IM + NBMODE*(IM-1)
               ZR(JTRA3+IND-1) = ZR(JTRA3+IND-1) + DSDT*AMOGEN(IM)
               ZR(JTRA4+IND-1) = ZR(JTRA4+IND-1) + AMOGEN(IM)
               ZR(JTRA5+IND-1) = ZR(JTRA5+IND-1) + DSDT*AMOGEN(IM)
 120        CONTINUE
         ELSE
            DO 130 IM = 1 , NBMODE
               DO 132 JM = 1 , NBMODE
                  IND = JM + NBMODE*(IM-1)
                  ZR(JTRA3+IND-1) = QSDT2*MASGEN(IND) + RIGGEN(IND)
     +                                         + DSDT*AMOGEN(IND)
                  ZR(JTRA4+IND-1) = QSDT*MASGEN(IND) + AMOGEN(IND)
                  ZR(JTRA5+IND-1) = QSDT2*MASGEN(IND) + DSDT*AMOGEN(IND)
 132           CONTINUE
 130        CONTINUE
         ENDIF
C        --- FACTORISATION DE LA MATRICE MASSE ---
         CALL WKVECT('&&MDNEWM.MASS','V V R8',NBMODE*NBMODE,JMASS)
         CALL DCOPY(NBMODE*NBMODE,MASGEN,1,ZR(JMASS),1)      
         CALL TRLDS(ZR(JMASS),NBMODE,NBMODE,IRET)
C         CALL TRLDS(MASGEN,NBMODE,NBMODE,IRET)
         IF (IRET.NE.0) CALL UTMESS('F','MDNEWM',
     +                              'LA MATRICE MASSE EST SINGULIERE.')
C        --- FACTORISATION DE LA MATRICE KTILDA ---
         CALL TRLDS(ZR(JTRA3),NBMODE,NBMODE,IRET)
         IF (IRET.NE.0) CALL UTMESS('F','MDNEWM',
     +                              'LA MATRICE KTILDA EST SINGULIERE.')
      ENDIF
C
C     --- CONDITIONS INITIALES ---
      CALL MDINIT(BASEMO,NBMODE,0,ZR(JDEPL),ZR(JVITE),R8B, IRET )
      IF (IRET.NE.0) GOTO 9999
C
C     --- FORCES EXTERIEURES ---
      CALL GETFAC('EXCIT',NBEXCI)
      IF (NBEXCI.NE.0) THEN
         CALL MDFEXT(NBEXCI,BASEMO,TYPBAS,NBMODE,TINIT,1,DT,ZR(JFEXT),
     +               IRET)
         IF (IRET.NE.0) GOTO 9999
      ENDIF
C
C     --- ACCELERATIONS GENERALISEES INITIALES ---
      CALL MDACCE(TYPBAS,NBMODE,PULSA2,MASGEN,DESCMM,RIGGEN,
     +            DESCMR,ZR(JFEXT),LAMOR,AMOGEN,DESCMA,ZR(JTRA1),
     +            ZR(JDEPL),ZR(JVITE),ZR(JACCE))
C
C     --- ARCHIVAGE DONNEES INITIALES ---
      IBID1 = 0
      IBID2 = 0
      TARCHI = TINIT
      CALL MDARCH(ISTO1,0,TINIT,DT, NBMODE,ZR(JDEPL),ZR(JVITE),
     +            ZR(JACCE),IBID1,0,R8B,0,IBID2,0,R8B,IBID,
     +            DEPSTO,VITSTO,
     +            ACCSTO,R8B,LPSTO,IORSTO,TEMSTO, R8B,R8B,R8B, IBID,
     +            R8B, IBID, R8B )
C
      TEMPS = TINIT + DT
      CALL UTTCPU (1,'INIT',4,TPS1)
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
            CALL MDFEXT(NBEXCI,BASEMO,TYPBAS,NBMODE,TEMPS,NBPP,DT,
     +                                        ZR(JFEXT),IRET)
            IF (IRET.NE.0) GOTO 9999
         ENDIF
C
         DO 200 I = 1 , NBPP
C
            IF (MOD(IPAS,N100).EQ.0) CALL UTTCPU (1,'DEBUT',4,TPS1)
C
            IFE = ( I - 1 ) * NBMODE
            IF (TYPBAS.EQ.'MODE_MECA'.OR.TYPBAS.EQ.'MODE_GENE') THEN
               IF ( LAMOR ) THEN
                  DO 210 IM = 0,NBMOD1
                     IM1 = IM + 1
                     ZR(JTRA1+IM) = ZR(JDEPL+IM)
                     X1 = ( QSDT + AMOGEN(IM1) ) * MASGEN(IM1)
                     X2 = ( QSDT2 + DSDT*AMOGEN(IM1) ) * MASGEN(IM1)
                     X3 = X2 + RIGGEN(IM1)
                     ZR(JDEPL+IM) = ( ZR(JFEXT+IFE+IM) + X1*ZR(JVITE+IM)
     +                                + MASGEN(IM1)*ZR(JACCE+IM)
     +                                + X2*ZR(JDEPL+IM) ) / X3
 210              CONTINUE
               ELSE
                  DO 212 IM = 0,NBMOD1
                     ZR(JTRA1+IM) = ZR(JDEPL+IM)
                     ZR(JTRA2+IM) = ZR(JVITE+IM) + DSDT*ZR(JDEPL+IM)
 212              CONTINUE
                  CALL PMAVEC('ZERO',NBMODE,AMOGEN,ZR(JTRA2),ZR(JDEPL))
                  DO 214 IM = 0,NBMOD1
                     IM1 = IM + 1
                     X1 = ZR(JACCE+IM) + QSDT*ZR(JVITE+IM)
     +                                   + QSDT2*ZR(JTRA1+IM)
                     ZR(JDEPL+IM) = ZR(JDEPL+IM) + ZR(JFEXT+IFE+IM)
     +                                               + X1*MASGEN(IM1)
 214              CONTINUE
                  CALL RRLDS(ZR(JTRA3),NBMODE,NBMODE,ZR(JDEPL),1)
               ENDIF
            ELSE
               DO 216 IM = 0,NBMOD1
                  ZR(JTRA1+IM) = ZR(JDEPL+IM)
                  ZR(JDEPL+IM) = ZR(JFEXT+IFE+IM)
 216           CONTINUE
               CALL PMAVEC('CUMUL',NBMODE,MASGEN,ZR(JACCE),ZR(JDEPL))
               CALL PMAVEC('CUMUL',NBMODE,ZR(JTRA4),ZR(JVITE),ZR(JDEPL))
               CALL PMAVEC('CUMUL',NBMODE,ZR(JTRA5),ZR(JTRA1),ZR(JDEPL))
               CALL RRLDS(ZR(JTRA3),NBMODE,NBMODE,ZR(JDEPL),1)
            ENDIF
            DO 218 IM = 0,NBMOD1
               ZR(JACCE+IM) = -ZR(JACCE+IM) + QSDT2*( ZR(JDEPL+IM)
     +                               - ZR(JTRA1+IM) - DT*ZR(JVITE+IM) )
C
               ZR(JVITE+IM) = -ZR(JVITE+IM) + DSDT*( ZR(JDEPL+IM)
     +                                      - ZR(JTRA1+IM) )
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
     +                     ZR(JVITE),ZR(JACCE), IBID1,0,R8B,0,
     +                     IBID2,0,R8B,IBID,
     +                     DEPSTO,VITSTO,ACCSTO,R8B,LPSTO,IORSTO,TEMSTO,
     +                     R8B,R8B,R8B,IBID, R8B, IBID,R8B )
            ENDIF
C
C       --- TEST SI LE TEMPS RESTANT EST SUFFISANT POUR CONTINUER ---
C
            IF (MOD(IPAS,N100).EQ.0) THEN
             CALL UTTCPU (1,'FIN',4,TPS1)
C
             IF (MAX(5.D0,N100*TPS1(4)).GT.0.90D0*TPS1(1)) THEN
              CALL MDSIZE (NOMRES,ISTO1,NBMODE,LPSTO,0,0)
              IF (NOMRES.EQ.'99999') THEN
C             --- CAS D'UNE POURSUITE ---
                 CALL GETVID('ETAT_INIT','RESU_GENE',1,1,1,TRAN,NDT)
                 IF (NDT.NE.0) CALL RESU74(TRAN,NOMRES)
              ENDIF
              CALL UTDEBM ('S','MDNEWM','ARRET PAR MANQUE DE TEMPS CPU')
              CALL UTIMPI ('S',' AU NUMERO D''ORDRE : ',1,IA+I)
              CALL UTIMPR ('L',' DERNIER INSTANT ARCHIVE : ',1,TARCHI)
              CALL UTIMPI ('L',' NUMERO D''ORDRE CORRESPONDANT : ',1,
     &                     ISTO1)
              CALL UTIMPR ('L',' TEMPS MOYEN PAR PAS DE TEMPS : ',1,
     &                     TPS1(4))
              CALL UTIMPR ('L',' TEMPS CPU RESTANT : ',1,TPS1(1))
              CALL UTFINM ()
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
      CALL JEDETR('&&MDNEWM.FEXT')
      IF (IRET.NE.0)
     +   CALL UTMESS('F','DYNA_TRAN_MODAL','DONNEES ERRONEES.')
C
      CALL JEDEMA()
      END
