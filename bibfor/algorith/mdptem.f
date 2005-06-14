      SUBROUTINE MDPTEM (NBMODE,MASGEN,RIGGEN,PULSAT,NBCHOC,DPLMOD,
     +                   PARCHO,NOECHO,DT,TINIT,TFIN,NBPAS,INFO,IER)
      IMPLICIT   REAL*8 (A-H,O-Z)
      INTEGER            NBCHOC, NBPAS, INFO,IER, NBMODE
      REAL*8             MASGEN(*),RIGGEN(*),PULSAT(*),
     +                   PARCHO(NBCHOC,*),DPLMOD(NBCHOC,NBMODE,*)
      REAL*8             DT, TINIT, TFIN
      CHARACTER*8        NOECHO(NBCHOC,*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/03/2000   AUTEUR DURAND C.DURAND 
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
C     VERIFICATION ET CALCUL DU PAS DE TEMPS
C     ------------------------------------------------------------------
C IN  : NBMODE : NOMBRE DE MODES
C IN  : MASGEN : MASSES GENERALISEES
C IN  : RIGGEN : RAIDEURS GENERALISES
C IN  : PULSAT : PULSATIONS MODALES
C IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
C IN  : DPLMOD : TABLEAU DES DEPL MODAUX AUX NOEUDS DE CHOC
C IN  : PARCHO : TABLEAU DES PARAMETRES DE CHOC
C IN  : NOECHO : TABLEAU DES NOMS DES NOEUDS DE CHOC
C OUT : DT     : PAS DE TEMPS
C OUT : TINIT  : TEMPS INITIAL
C OUT : TFIN   : TEMPS FINAL
C OUT : NBPAS  : NOMBRE DE PAS CALCULE (INITIAL NON COMPRIS)
C OUT : IER    : CODE RETOUR
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER       IC,IA,I,J,IVERI,JINST, N1,N2,N3,N4,N5,NBINST,
     +              NR,NT
      REAL*8        KNORM, KTANG, KLOCX, KLOCY, KLOCZ, UNSGA
      REAL*8        ZERO, DEUXPI, DTS, DTU, DTI, R8DEPI, R8GAEM
      CHARACTER*8   METHOD, VERIPA,NOMRES,TRAN
      CHARACTER*16  TYPRES,NOMCMD
      CHARACTER*1   K1BID
C     ------------------------------------------------------------------
C
      CALL GETRES(NOMRES,TYPRES,NOMCMD)
C
      UNSGA = SQRT(1/R8GAEM())
      TINIT = 0.D0
      IER = 0
      IVERI = 0
      ZERO = 0.D0
      DEUXPI = R8DEPI()
      DTS = 1.D10
      DTU = 1.D+10
      CALL GETVTX(' ','METHODE'  ,0,1,1,METHOD,N1)
      CALL GETVR8('INCREMENT','INST_INIT' ,1,1,1,  TINIT,N2)
      IF (N2.EQ.0) THEN
         CALL GETVR8('ETAT_INIT','INST_INIT',1,1,1, TINIT,NT)
         IF (NT.EQ.0) THEN
            CALL GETVID('ETAT_INIT','RESU_GENE',1,1,1, TRAN,NR)
            IF (NR.EQ.0) THEN
               CALL UTMESS('I',NOMCMD,' INSTANT INITIAL NON TROUVE '//
     +                                ' VALEUR PRISE : 0 ')
            ELSE
               CALL JEVEUO(TRAN//'           .INST' ,'E',JINST)
               CALL JELIRA(TRAN//'           .INST' ,'LONUTI',NBINST,
     +                     K1BID)
               TINIT = ZR(JINST+NBINST-1)
            ENDIF
         ENDIF
      ENDIF
      CALL GETVR8('INCREMENT','INST_FIN' ,1,1,1,  TFIN,N3)
      CALL GETVR8('INCREMENT','PAS'      ,1,1,1,   DTU,N4)
      CALL GETVTX('INCREMENT','VERI_PAS' ,1,1,1,VERIPA,N5)
      IF ( VERIPA .EQ. 'OUI' ) IVERI = 1
C
      DO 10 I = 1,NBMODE
         IF (PULSAT(I).NE.ZERO) THEN
            DTI = DEUXPI / PULSAT(I)
            DTS = MIN( DTS , DTI )
         ENDIF
 10   CONTINUE
C
      IF ( NBCHOC.GT.0 ) THEN
         IF (INFO.EQ.2) CALL UTDEBM('I','MDPTEM','  ')
         DO 20 I = 1,NBCHOC
            KNORM = PARCHO(I,2)
            KTANG = PARCHO(I,4)
            IC = 1
            IA = 0
 24         CONTINUE
            IF (INFO.EQ.2)
     +      CALL UTIMPK('L','--- AU NOEUD DE CHOC :',1,NOECHO(I,IC))
            DO 22 J = 1,NBMODE
               KLOCX = ZERO
               KLOCY = ZERO
               KLOCZ = ZERO
               IF (ABS(DPLMOD(I,J,1+IA)).GT.UNSGA)
     +                       KLOCX = RIGGEN(J) / DPLMOD(I,J,1+IA)**2
               IF (ABS(DPLMOD(I,J,2+IA)).GT.UNSGA)
     +                       KLOCY = RIGGEN(J) / DPLMOD(I,J,2+IA)**2
               IF (ABS(DPLMOD(I,J,3+IA)).GT.UNSGA)
     +                       KLOCZ = RIGGEN(J) / DPLMOD(I,J,3+IA)**2
               IF (KLOCX.LE.KNORM .OR. KLOCY.LE.KNORM
     +                                      .OR. KLOCZ.LE.KNORM) THEN
                  IF (INFO.EQ.2) THEN
                     CALL UTIMPI('L',' POUR LE MODE NO :',1,J)
                     CALL UTIMPR('L','RAIDEUR LOCALE DEPX : ',1,KLOCX)
                     CALL UTIMPR('L','RAIDEUR LOCALE DEPY : ',1,KLOCY)
                     CALL UTIMPR('L','RAIDEUR LOCALE DEPZ : ',1,KLOCZ)
                  ENDIF
               ENDIF
               IF (KNORM.NE.ZERO) THEN
                  DTI = DEUXPI / SQRT(PULSAT(J)**2 +
     +                     KNORM * DPLMOD(I,J,1+IA)**2 / MASGEN(J) )
                  DTS = MIN(DTS, DTI)
                  DTI = DEUXPI / SQRT(PULSAT(J)**2 +
     +                     KNORM * DPLMOD(I,J,2+IA)**2 / MASGEN(J) )
                  DTS = MIN(DTS, DTI)
                  DTI = DEUXPI / SQRT(PULSAT(J)**2 +
     +                     KNORM * DPLMOD(I,J,3+IA)**2 / MASGEN(J) )
                  DTS = MIN(DTS, DTI)
               ENDIF
               IF (KTANG.NE.ZERO) THEN
                  DTI = DEUXPI / SQRT(PULSAT(J)**2 +
     +                     KTANG * DPLMOD(I,J,1+IA)**2 / MASGEN(J) )
                  DTS = MIN(DTS, DTI)
                  DTI = DEUXPI / SQRT(PULSAT(J)**2 +
     +                     KTANG * DPLMOD(I,J,2+IA)**2 / MASGEN(J) )
                  DTS = MIN(DTS, DTI)
                  DTI = DEUXPI / SQRT(PULSAT(J)**2 +
     +                     KTANG * DPLMOD(I,J,3+IA)**2 / MASGEN(J) )
                  DTS = MIN(DTS, DTI)
               ENDIF
 22         CONTINUE
            IF (IC.EQ.5) GOTO 20
            IF (NOECHO(I,9)(1:2).EQ.'BI') THEN
               IC = 5
               IA = 3
               GOTO 24
            ENDIF
 20      CONTINUE
         IF (INFO.EQ.2) CALL UTFINM()
      ENDIF
C
      IF ( METHOD .EQ. 'DEVOGE' ) THEN
         DT = MIN( DTS / 10.D0 , DTU )
      ELSEIF ( METHOD .EQ. 'NEWMARK' ) THEN
         DT = MIN( DTS / 10.D0 , DTU )
      ELSEIF ( METHOD .EQ. 'ITMI' ) THEN
         DT = DTU
         GOTO 9999
      ELSE
         DT = MIN( DTS / 20.D0 , DTU )
      ENDIF
      NBPAS = NINT( ( TFIN - TINIT ) / DT )
      IF ( N4 .EQ. 0 ) GOTO 9999
C
      IF ( DT .NE. DTU ) THEN
         IF (METHOD .EQ. 'NEWMARK') THEN
         CALL UTDEBM('A','!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',' ')
         CALL UTIMPR('L','PAS DE TEMPS UTILISATEUR TROP GRAND:',1,DTU)
         CALL UTIMPR('L','PAS DE TEMPS NECESSAIRE POUR LE CALCUL:',1,DT)
         CALL UTIMPK('L','RISQUES DE PROBLEMES DE PRECISION',1,' ')
         CALL UTFINM()
         ELSEIF (IVERI.EQ.1 .AND. METHOD .NE. 'ADAPT') THEN
         IER = IER + 1
         CALL UTDEBM('E','!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',' ')
         CALL UTIMPR('L','PAS DE TEMPS UTILISATEUR TROP GRAND:',1,DTU)
         CALL UTIMPR('L','PAS DE TEMPS NECESSAIRE POUR LE CALCUL:',1,DT)
         CALL UTIMPK('L','!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',1,' ')
         CALL UTIMPK('L','PARAMETRES DE CALCUL DANS CE CAS :   ',1,' ')
         CALL UTIMPI('L','NB DE PAS DE CALCUL : ',1,NBPAS)
         CALL UTFINM()
         ELSE
         CALL UTDEBM('A','!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',' ')
         CALL UTIMPR('L','PAS DE TEMPS UTILISATEUR TROP GRAND:',1,DTU)
         CALL UTIMPR('L','PAS DE TEMPS NECESSAIRE POUR LE CALCUL:',1,DT)
         IF (IVERI.NE.1)
     &    CALL UTIMPK('L','ON PASSE OUTRE CAR VERI_PAS: NON     ',1,' ')
         CALL UTIMPK('L','!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',1,' ')
         CALL UTIMPK('L','PARAMETRES DE CALCUL DANS CE CAS :   ',1,' ')
         CALL UTIMPI('L','NB DE PAS DE CALCUL : ',1,NBPAS)
         CALL UTFINM()
         DT = DTU
         NBPAS = NINT( ( TFIN - TINIT ) / DT )
         ENDIF
      ENDIF
C
 9999 CONTINUE
      END
