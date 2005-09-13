      SUBROUTINE IRGENE (IOCC, RESU, FORM, IFI, NBNOSY,NOSY,
     +                   NBCMPG,CMPG, NBPARA,PARA,
     +                   NBORDR,ORDR, NBINST,TEMP,NUME, LHIST )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER         CMPG(*), ORDR(*), NUME(*)
      REAL*8          TEMP(*)
      CHARACTER*(*)   RESU, NOSY(*), PARA(*), FORM
      LOGICAL         LHIST
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 12/09/2005   AUTEUR NICOLAS O.NICOLAS 
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
C TOLE CRP_21
C     IMPRESSION D'UN CONCEPT GENERALISE
C     ------------------------------------------------------------------
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
      CHARACTER*32      JEXNUM, JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      CHARACTER*1  CECR, K1BID
      CHARACTER*8  K8B
      CHARACTER*16 TYPCON
      CHARACTER*19 GENE,  NOCH19
      CHARACTER*24 NOMST
      LOGICAL      LORDR
C     ------------------------------------------------------------------
      CALL JEMARQ()
      NOMST = '&&IRGENE.SOUS_TITRE.TITR'
C
C     --- QUEL TYPE DE CONCEPT ? ---
C
      CALL GETTCO ( RESU , TYPCON )
C
C=======================================================================
C
C               --- IMPRESSION D'UN "VECT_ASSE_GENE" ---
C
C=======================================================================
      IF ( TYPCON .EQ. 'VECT_ASSE_GENE_R' ) THEN
C
         CALL IRVGEN ( RESU, IFI, NBCMPG,CMPG, LHIST )
C
C=======================================================================
C
C         --- IMPRESSION D'UN "MODE_GENE" ET D'UN "HARM_GENE" ---
C
C=======================================================================
      ELSEIF ( TYPCON .EQ. 'HARM_GENE' .OR.
     +         TYPCON .EQ. 'MODE_GENE' ) THEN
         CALL IRPARB (RESU,NBPARA,PARA,'&&IRGENE.PARAMETRE',NPARA)
         CALL JEEXIN('&&IRGENE.PARAMETRE',IRET)
         IF (IRET.GT.0) THEN
            CALL JEVEUO('&&IRGENE.PARAMETRE','E',JPARA)
         ELSE
            JPARA=1
         ENDIF

         CECR = 'L'
         DO 100 IORD = 1,NBORDR
            WRITE(IFI,2000)
            CALL IRPARA (RESU, FORM,IFI, 1,ORDR(IORD),
     +                                      NPARA,ZK16(JPARA), CECR )
            DO 110 ISY = 1,NBNOSY
              CALL RSEXCH(RESU,NOSY(ISY),ORDR(IORD),NOCH19,IRET)
              IF(IRET.EQ.0) THEN
                CALL TITRE2 (RESU,NOCH19,NOMST,'GENE',IOCC)
                WRITE(IFI,2010)
                CALL JEVEUO(NOMST,'L',JTITR)
                CALL JELIRA(NOMST,'LONMAX',NBTITR,K1BID)
                WRITE(IFI,'(1X,A)') (ZK80(JTITR+I-1),I=1,NBTITR)
                CALL IRVGEN ( NOCH19, IFI, NBCMPG,CMPG, LHIST )
              ENDIF
 110        CONTINUE
 100     CONTINUE
         CALL JEDETR ('&&IRGENE.PARAMETRE')
         CALL JEEXIN ( NOMST, IRET )
         IF ( IRET .NE. 0 ) CALL JEDETR ( NOMST )
C
C=======================================================================
C
C                     --- IMPRESSION D'UN "TRAN_GENE" ---
C
C=======================================================================
      ELSEIF ( TYPCON .EQ. 'TRAN_GENE' ) THEN
         GENE = RESU
         LORDR = .FALSE.
         CALL JEEXIN(GENE//'.ORDR',IRET)
         IF ( IRET .NE. 0 ) THEN
            CALL JEVEUO(GENE//'.ORDR','L',JORDR)
            LORDR = .TRUE.
         ENDIF
         CALL JEVEUO(GENE//'.DESC','L',JDESC)
         NBMODE = ZI(JDESC+1)
         CALL JEVEUO(GENE//'.REFD','L',JREFE)
         NOCH19 = '&&IRGENE_VECTEUR'
         CALL WKVECT(NOCH19//'.DESC','V V I'  ,2,KDESC)
         CALL WKVECT(NOCH19//'.REFE','V V K24',2,KREFE)
         CALL WKVECT(NOCH19//'.VALE','V V R'  ,NBMODE,KVALE)
         ZI(KDESC+1) = NBMODE
         ZK24(KREFE) = ZK24(JREFE+5)
         DO 200 I = 1 , NBINST
           IORD = NUME(I)
           WRITE(IFI,2000)
           DO 210 ISY = 1,NBNOSY
              CALL JEEXIN(GENE//'.'//NOSY(ISY)(1:4),IRET)
              IF (IRET.EQ.0) GOTO 210
              WRITE(IFI,2010)
              WRITE(IFI,3010) NOSY(ISY)
              IF ( LORDR ) THEN
                 WRITE(IFI,3020) ZI(JORDR+IORD-1), TEMP(I)
              ELSE
                 WRITE(IFI,3020) IORD, TEMP(I)
              ENDIF
              CALL JEVEUO(GENE//'.'//NOSY(ISY)(1:4),'L',ITRESU)
              DO 220 IM = 1 , NBMODE
                 ZR(KVALE+IM-1) = ZR(ITRESU+(IORD-1)*NBMODE+IM-1)
 220          CONTINUE
              CALL IRVGEN ( NOCH19, IFI, NBCMPG,CMPG, LHIST )
 210       CONTINUE
 200     CONTINUE
         CALL JEDETR ( NOCH19//'.DESC' )
         CALL JEDETR ( NOCH19//'.REFE' )
         CALL JEDETR ( NOCH19//'.VALE' )
C
      ELSE
         CALL UTMESS('F','IRGENE',
     +                   'TYPE DE STRUCTURE NON TRAITE: '//TYPCON )
      ENDIF
C
 9999 CONTINUE
C
 2000 FORMAT(/,1X,'======>')
 2010 FORMAT(/,1X,'------>')
 3010 FORMAT(' VECTEUR GENERALISE DE NOM SYMBOLIQUE  ',A)
 3020 FORMAT(1P,' NUMERO D''ORDRE: ',I8,' INSTANT: ',D12.5)
C
      CALL JEDEMA()
      END
