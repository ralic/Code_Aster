      SUBROUTINE FOC1EN( SORTIE, NBFON , NOMFON , CRITER , BASE )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                    NBFON
      CHARACTER*(*)      SORTIE,         NOMFON(*),CRITER
      CHARACTER*1                                        BASE
C     ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 21/02/96   AUTEUR VABHHTS J.PELLET 
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
C     EFFECTUE LE CALCUL DE L'ENVELOPPE DE FONCTION DE TYPE "FONCTION"
C     ----------------------------------------------------------------
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
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      IP(2) , LTP(2)
      REAL*8       EPS
      CHARACTER*24 PROL , VALE , NOMTEM(2)
      CHARACTER*1 K1BID
C     ----------------------------------------------------------------
      DATA     NOMTEM/'&&FOC1EN.TEMPORAIRE1', '&&FOC1EN.TEMPORAIRE2'/
      DATA     IP    /2,1/
C     ----------------------------------------------------------------
C
C     --- DETERMINATION DE LA LISTE DES INSTANTS PRODUIT ---
      CALL JEMARQ()
      EPS    = 1.D-6
      IPERM  = 1
      NBINST = 0
      VALE(20:24) = '.VALE'
      PROL(20:24) = '.PROL'
      DO 200 IOCC = 1, NBFON
         VALE(1:19) = NOMFON(IOCC)
         CALL JELIRA(VALE,'LONUTI',NBVAL,K1BID)
         CALL JEVEUO(VALE,'L',LVAR)
         NBPTS = NBVAL/2
         ISUITE = IP(IPERM)
         CALL WKVECT(NOMTEM(ISUITE),'V V R',NBPTS+NBINST,LTP(ISUITE))
         DO 210 I = 1, NBPTS
            ZR(LTP(ISUITE)+I-1) = ZR(LVAR+I-1)
 210     CONTINUE
         DO 220 I = 1, NBINST
            ZR(LTP(ISUITE)+I-1+NBPTS) = ZR(LTP(IPERM)+I-1)
 220     CONTINUE
         NBINST =  NBPTS+NBINST
         CALL UTTRIR( NBINST, ZR(LTP(ISUITE)), EPS )
         IF (IOCC.GT.1)  CALL JEDETR(NOMTEM(IPERM))
         IPERM = ISUITE
 200  CONTINUE
C
C        --- CREATION DE L'OBJET DES INSTANTS ET DES VALEURS ---
      VALE(1:19) = SORTIE
      CALL WKVECT(VALE,BASE//' V R',2*NBINST,LRES)
      DO 300 I = 1, NBINST
         ZR(LRES+I-1) = ZR(LTP(IPERM)+I-1)
 300  CONTINUE
      CALL JEDETR(NOMTEM(IPERM))
C
C     --- SUPERPOSITION DES VALEURS ---
      CALL WKVECT(NOMTEM(1),'V V R',NBINST,LTRES)
      LRESF = LRES + NBINST
C
      IOCC = 1
      PROL(1:19) = NOMFON(IOCC)
      CALL JEVEUO(PROL,'L',LPRO)
      VALE(1:19) = NOMFON(IOCC)
      CALL JELIRA(VALE,'LONUTI',NBVAL,K1BID)
      CALL JEVEUO(VALE,'L',LVAR)
      NBPTS = NBVAL/2
      LFON  = LVAR + NBPTS
      CALL FOINTR(NOMFON(IOCC),ZK8(LPRO),NBPTS,ZR(LVAR) ,ZR(LFON),
     +                         NBINST,ZR(LRES),ZR(LTRES),IER)
      DO 500 I = 0, NBINST-1
         ZR(LRESF+I) = ZR(LTRES+I)
 500  CONTINUE
      CALL JELIBE(PROL)
      CALL JELIBE(VALE)
C
      DO 400 IOCC = 2, NBFON
         PROL(1:19) = NOMFON(IOCC)
         CALL JEVEUO(PROL,'L',LPRO)
         VALE(1:19) = NOMFON(IOCC)
         CALL JELIRA(VALE,'LONUTI',NBVAL,K1BID)
         CALL JEVEUO(VALE,'L',LVAR)
         NBPTS = NBVAL/2
         LFON  = LVAR + NBPTS
         CALL FOINTR(NOMFON(IOCC),ZK8(LPRO),NBPTS,ZR(LVAR) ,ZR(LFON),
     +                         NBINST,ZR(LRES),ZR(LTRES),IER)
         IF ( CRITER .EQ. 'SUP' ) THEN
            DO 410 I = 0, NBINST-1
               ZR(LRESF+I) = MAX( ZR(LRESF+I) , ZR(LTRES+I) )
 410        CONTINUE
         ELSEIF ( CRITER .EQ. 'INF' ) THEN
            DO 420 I = 0, NBINST-1
               ZR(LRESF+I) = MIN( ZR(LRESF+I) , ZR(LTRES+I) )
 420        CONTINUE
         ENDIF
         CALL JELIBE(PROL)
         CALL JELIBE(VALE)
 400  CONTINUE
      CALL JEDETR(NOMTEM(1))
C
C     FIN DU REMPLISSAGE
      VALE(1:19) = SORTIE
      CALL JEECRA(VALE,'LONUTI',2*NBINST,' ')
      CALL JELIBE(VALE)
C
C     --- CREATION DU .PROL ---
      PROL(1:19) = SORTIE
      CALL WKVECT(PROL,BASE//' V K8',5,LPROS)
      PROL(1:19) = NOMFON(1)
      CALL JEVEUO(PROL,'L',LPRO)
      ZK8(LPROS  ) = 'FONCTION'
      ZK8(LPROS+1) = 'LIN LIN '
      ZK8(LPROS+2) = ZK8(LPRO+2)
      ZK8(LPROS+3) = ZK8(LPRO+3)
      ZK8(LPROS+4) = 'CC      '
      CALL JELIBE(PROL)
      PROL(1:19) = SORTIE
      CALL JELIBE(PROL)
C
      CALL JEDEMA()
      END
