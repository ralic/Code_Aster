      SUBROUTINE MDPTEM (NBMODE,MASGEN,PULSAT,NBCHOC,DPLMOD,
     &       PARCHO,NOECHO,DT,DTS,DTU,DTMAX,TINIT,TFIN,NBPAS,INFO,IER,
     &       LISINS)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER            NBCHOC, NBPAS, INFO,IER, NBMODE
      REAL*8             MASGEN(*),PULSAT(*),
     &                   PARCHO(NBCHOC,*),DPLMOD(NBCHOC,NBMODE,*)
      REAL*8             DT, TINIT, TFIN, DTMAX, DTS, DTU
      CHARACTER*8        NOECHO(NBCHOC,*)
      CHARACTER*24       LISINS
C ----------------------------------------------------------------------
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
C     VERIFICATION ET CALCUL DU PAS DE TEMPS
C     ------------------------------------------------------------------
C IN  : NBMODE : NOMBRE DE MODES
C IN  : MASGEN : MASSES GENERALISEES
C IN  : PULSAT : PULSATIONS MODALES
C IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
C IN  : DPLMOD : TABLEAU DES DEPL MODAUX AUX NOEUDS DE CHOC
C IN  : PARCHO : TABLEAU DES PARAMETRES DE CHOC
C IN  : NOECHO : TABLEAU DES NOMS DES NOEUDS DE CHOC
C OUT : DT     : PAS DE TEMPS
C OUT : DTS    : PAS DE TEMPS LIMITE (BASE ET CHOCS)
C OUT : DTU    : PAS DE TEMPS UTILISATEUR
C OUT : TINIT  : TEMPS INITIAL
C OUT : TFIN   : TEMPS FINAL
C OUT : NBPAS  : NOMBRE DE PAS CALCULE (INITIAL NON COMPRIS)
C OUT : IER    : CODE RETOUR
C OUT : LISINS : LISTE DES INSTANTS POUR L'ARCHIVAGE
C ----------------------------------------------------------------------
      INTEGER       IC, IA, I, J, IVERI, IBID  
      INTEGER       JBINT, JLPAS, JVALE, JVALR, JINST, JORDR
      INTEGER       N1, N2, N3, N4, N5, N6, NR, NT, NNI         
      INTEGER       NBGRPA, NBINST, NBINSR, NUMEF, NBORDR
      INTEGER       VALI
      REAL*8        KNORM, KTANG, R8BID
      REAL*8        VALR(3)
      REAL*8        ZERO, DEUXPI, DTI, R8DEPI, DTP, DTA
      CHARACTER*8   VERIPA,NOMRES,TRAN, LI
      CHARACTER*16  TYPRES,NOMCMD,METHOD
      CHARACTER*19  NUMARC
      CHARACTER*1   K1BID
      INTEGER      IARG
C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER K ,N7 ,NUME 
C-----------------------------------------------------------------------
      CALL GETRES(NOMRES,TYPRES,NOMCMD)
C
      TINIT = 0.D0
      IER = 0
      IVERI = 0
      ZERO = 0.D0
      DEUXPI = R8DEPI()
      DTS = 1.D10
      DTU = 1.D+10
      DTMAX = 1.D+10
      LISINS= ' '
      CALL GETVTX('SCHEMA_TEMPS','SCHEMA',1,IARG,1,METHOD,N1)
C      
C     VERIFICATION DE PRESENCE DU PAS SI ADAPT
C     (DEJA FAIT DANS MDVERI POUR ITMI)
      IF ( METHOD(1:5) .EQ. 'ADAPT') THEN
        CALL GETVR8('INCREMENT','PAS',1,IARG,0,DTA,IBID)
        IF (IBID.EQ.0)
     &     CALL U2MESS('F','ALGORITH3_11')
      ENDIF  
C
C     RECUPERATION de TINIT
      CALL GETVID('ETAT_INIT','RESULTAT',1,IARG,1, TRAN,NR)
      IF (NR .NE. 0) THEN
         CALL GETVIS('ETAT_INIT','NUME_ORDRE' ,1,IARG,1,NUME,NNI)
         IF ( NNI .EQ. 0 ) THEN
            CALL GETVR8('ETAT_INIT','INST_INIT',1,IARG,1, TINIT,NT)
            IF (NT .EQ. 0 ) THEN
               CALL JEVEUO(TRAN//'           .INST' ,'L',JINST)
               CALL JELIRA(TRAN//'           .INST' ,'LONUTI',NBINST,
     &                     K1BID)
               TINIT = ZR(JINST+NBINST-1)
            ENDIF   
         ELSE
            CALL JEVEUO(TRAN//'           .ORDR' ,'L',JORDR)
            CALL JELIRA(TRAN//'           .ORDR' ,'LONUTI',NBORDR,
     &                     K1BID)
            DO 100 I=1,NBORDR
               IF (ZI(JORDR+I-1).EQ.NUME) GOTO 101
 100        CONTINUE
            CALL U2MESK('F','ALGORITH3_36',1,TRAN)
 101        CONTINUE
            CALL JEVEUO(TRAN//'           .INST' ,'L',JINST)
            TINIT = ZR(JINST+I-1)
         ENDIF    
      ELSE
         CALL GETVID('INCREMENT','LIST_INST',1,IARG,1,  LI,NT)
         IF (NT.NE.0) THEN
            CALL JEVEUO(LI//'           .BINT','L',JBINT)
            TINIT = ZR (JBINT)
         ELSE
            CALL GETVR8('INCREMENT','INST_INIT',1,IARG,1,  TINIT,N2)
            IF (N2.EQ.0) THEN 
              CALL U2MESS('I','ALGORITH5_62') 
            ENDIF
         ENDIF
      ENDIF      
C
C     RECUPERATION de DT et TFIN 
      CALL GETVID('INCREMENT','LIST_INST',1,IARG,1,  LI,NT)
      IF (NT.NE.0) THEN
            CALL JEVEUO(LI//'           .BINT','L',JBINT)
            CALL JEVEUO(LI//'           .LPAS','L',JLPAS)
            CALL JEVEUO(LI//'           .VALE','L',JVALE)
            CALL JELIRA(LI//'           .VALE','LONUTI',NBINST,K1BID)
            CALL JELIRA(LI//'           .NBPA','LONUTI',NBGRPA,K1BID)
            
            IF (NBGRPA.EQ.1) THEN
               DTU = ZR (JLPAS)
               TFIN = ZR (JBINT+1)
            ELSE 
C              CHOIX DTU PLUS PETIT DE LA LISTE
               DTU = ZR(JLPAS)
               DO 32 J=1,NBGRPA-1
                   DTU =  MIN(DTU,ZR(JLPAS+J))
 32            CONTINUE  
C              TEST PAS DE TEMPS CONSTANT SI PLUSIEURS INTERVALLES
               DO 33 I = 1,NBGRPA-1
                   IF ((ABS(ZR(JLPAS+I)-DTU)).GE.(1.D-6*DTU)) THEN
                      CALL U2MESS('F','ALGORITH3_18')
                   ENDIF   
 33            CONTINUE
               TFIN = ZR (JBINT+NBGRPA)
            ENDIF
            CALL GETVIS('INCREMENT','NUME_FIN',1,IARG,1,NUMEF,N1)
            IF ( N1 .EQ. 0 ) THEN
               CALL GETVR8('INCREMENT','INST_FIN',1,IARG,1,TFIN,N1)
               IF ( N1 .EQ. 0 ) GOTO 99
            ELSE
               CALL JEVEUO(LI//'           .VALE','L',JVALR)
               CALL JELIRA(LI//'           .VALE','LONUTI',NBINSR,K1BID)
               IF (NUMEF.GE.NBINSR) GOTO 99
               TFIN = ZR(JVALR+NUMEF)
            ENDIF
      ELSE      
           CALL GETVR8('INCREMENT','INST_FIN' ,1,IARG,1,  TFIN,N3)
           CALL GETVR8('INCREMENT','PAS'      ,1,IARG,1,   DTU,N4)
           IF (DTU.EQ.0.D0)
     &       CALL U2MESS('F','ALGORITH3_12')
      ENDIF
  99  CONTINUE
      CALL GETVTX('INCREMENT','VERI_PAS' ,1,IARG,1,VERIPA,N5)
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

         DO 20 I = 1,NBCHOC
            KNORM = PARCHO(I,2)
            KTANG = PARCHO(I,4)
            IC = 1
            IA = 0
 24         CONTINUE
            IF (INFO.EQ.2)
     &      CALL U2MESG('I','ALGORITH16_92',1,NOECHO(I,IC),0,0,0,0.D0)
            DO 22 J = 1,NBMODE
               IF (PULSAT(J).EQ.ZERO) GOTO 22
             
               IF (KNORM.NE.ZERO) THEN
                     DTI = DEUXPI / SQRT(PULSAT(J)**2 +
     &                     KNORM * DPLMOD(I,J,1+IA)**2 / MASGEN(J) )
                     DTS = MIN(DTS, DTI)
                     DTI = DEUXPI / SQRT(PULSAT(J)**2 +
     &                     KNORM * DPLMOD(I,J,2+IA)**2 / MASGEN(J) )
                     DTS = MIN(DTS, DTI)
                     DTI = DEUXPI / SQRT(PULSAT(J)**2 +
     &                     KNORM * DPLMOD(I,J,3+IA)**2 / MASGEN(J) )
                     DTS = MIN(DTS, DTI)
               ENDIF
               IF (KTANG.NE.ZERO) THEN
                     DTI = DEUXPI / SQRT(PULSAT(J)**2 +
     &                     KTANG * DPLMOD(I,J,1+IA)**2 / MASGEN(J) )
                     DTS = MIN(DTS, DTI)
                     DTI = DEUXPI / SQRT(PULSAT(J)**2 +
     &                     KTANG * DPLMOD(I,J,2+IA)**2 / MASGEN(J) )
                     DTS = MIN(DTS, DTI)
                     DTI = DEUXPI / SQRT(PULSAT(J)**2 +
     &                     KTANG * DPLMOD(I,J,3+IA)**2 / MASGEN(J) )
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
      ENDIF
C
      IF     ( METHOD .EQ. 'DEVOGE'  ) THEN
         DTP = DTS / 10.D0
         DT = MIN( DTP , DTU )
      ELSEIF ( METHOD .EQ. 'NEWMARK' ) THEN
         DTP = DTS / 10.D0
         DT = MIN( DTP , DTU )
      ELSEIF ( METHOD .EQ. 'ITMI'    ) THEN
         DT = DTU
         GOTO 9999
      ELSE
C      CASE METHOD .EQ. 'EULER' OR OTHER
         DTP = DTS / 20.D0
         DT = MIN( DTP , DTU )
      ENDIF
      NBPAS = NINT( ( TFIN - TINIT ) / DT )
C
      IF ( DTU .GT. DT ) THEN
         IF (METHOD .EQ. 'NEWMARK') THEN
           VALR (1) = DTU
           VALR (2) = DT
           CALL U2MESG('A','ALGORITH16_14',0,' ',0,0,2,VALR)
           DT = DTU
           NBPAS = NINT( ( TFIN - TINIT ) / DT )
         ELSEIF (IVERI.EQ.1 .AND. METHOD(1:5) .NE. 'ADAPT') THEN
           IER = IER + 1
           VALR (1) = DTU
           VALR (2) = DT
           VALI = NBPAS
           CALL U2MESG('E','ALGORITH16_15',0,' ',1,VALI,2,VALR)
         ELSE
           VALR (1) = DTU
           VALR (2) = DT
           CALL U2MESG('A+','ALGORITH16_16',0,' ',0,0,2,VALR)
         IF (IVERI.NE.1) CALL U2MESS('A+','ALGORITH16_89')
           VALI = NBPAS
           CALL U2MESG('A','ALGORITH16_17',0,' ',1,VALI,0,0.D0)
           DT = DTU
           NBPAS = NINT( ( TFIN - TINIT ) / DT )
         ENDIF
      ENDIF
 9999 CONTINUE
C
C     SI LA MÃTHODE N'EST PAS ADAPT OU ITMI: 
C     SI LIST_INST DANS ARCHIVAGE ALORS:
C     BESOIN D'UNE LISTE DES INSTANTS DE CALCUL POUR LA CREATION 
C     DE LA LISTE D'ARCHIVAGE DANS DYARCH.F
      CALL GETVID ( 'ARCHIVAGE', 'LIST_INST', 1,IARG,0, NUMARC, N6 )
      CALL GETVR8 ( 'ARCHIVAGE', 'INST', 1,IARG,0, R8BID, N7 )
      IF ( N6 .NE.0 .OR. N7 .NE.0 )THEN
        IF ( METHOD(1:5) .NE. 'ADAPT' .OR. 
     &       METHOD      .NE. 'ITMI')      THEN
          CALL WKVECT('&&OP0074.MD_JVALE','V V R',NBPAS+1,JVALE)
          J=0
          ZR(JVALE)=TINIT
          DO 30 K = 1,NBPAS
            J = J + 1
            ZR(JVALE+J) = TINIT + K*DT
 30       CONTINUE
          LISINS= '&&OP0074.MD_JVALE'
        ENDIF
      ENDIF    
C       
C     GESTION DU PAS MAXIMAL POUR SCHEMA ADAPT
      IF ( METHOD(1:5) .EQ. 'ADAPT') THEN
        CALL GETVR8('INCREMENT','PAS_MAXI',1,IARG,1, R8BID,N6)
        IF ( N6 .NE. 0 ) THEN
          CALL GETVR8('INCREMENT','PAS_MAXI',1,IARG,1,DTMAX,N6)
          IF ( DTMAX .GT. (DTS / 20.D0) ) THEN
            VALR (1) = DTMAX
            VALR (2) = DT
            CALL U2MESG('A','DYNAMIQUE_17',0,' ',0,0,2,VALR)
          ENDIF
          IF ( ( DTMAX .LT. DTU ) ) THEN
            VALR (1) = DTMAX
            VALR (2) = DTU             
            CALL U2MESG('E','DYNAMIQUE_15',0,' ',0,0,2,VALR)
          ENDIF
        ELSE
          DTMAX = DTS / 20.D0
          IF ( DTS .GE. 1.D10 )
     &      CALL U2MESG('A','DYNAMIQUE_16',0,' ',0,0,1,DTMAX)
        ENDIF
      ENDIF
C

      END
