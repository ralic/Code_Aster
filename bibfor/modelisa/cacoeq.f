      SUBROUTINE CACOEQ(FONREZ, CHARGZ, NOMAZ)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     FONREZ, CHARGZ, NOMAZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 16/05/2001   AUTEUR CIBHHBC N.SELLALI 
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
C IN       : FONREZ : 'REEL' OU 'FONC' OU 'COMP'
C IN/JXVAR : CHARGZ : NOM D'UNE SD CHARGE
C ----------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32      JEXNOM, JEXNUM
C
C---------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
C
      INTEGER       NO(3),II,IRET,JLIST
      COMPLEX*16    BETAC, COEMUC(3)
      CHARACTER*1   K1BID
      CHARACTER*2   TYPLAG
      CHARACTER*4   FONREE
      CHARACTER*4   TYPCOE
      CHARACTER*8   BETAF, DDL(3), NONO(3), CHAR, NOMA
      CHARACTER*8   K8BID
      CHARACTER*16  MOTFAC
      CHARACTER*19  LISREL
      CHARACTER*24  LISNOE
      REAL*8        COEMUR(3), DIRECT(6), BETA
      INTEGER       IDIM(3),NBELQU
      DATA  DIRECT /0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0/
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      FONREE = FONREZ
      CHAR = CHARGZ
      NOMA = NOMAZ
C
      TYPLAG = '12'
      TYPCOE = 'REEL'
C
      LISREL = '&&CACOEQ.RLLISTE'
      LISNOE = CHAR//'.CONTACT.NOEUQU'
      CALL JEEXIN(LISNOE,IRET)
      IF (IRET.EQ.0) GOTO 9999
C
      CALL JEVEUO(LISNOE, 'L', JLIST)
      CALL JELIRA(LISNOE,'LONUTI ',NBELQU,K1BID)
      BETAF =  '&FOZERO'
      BETAC  = (0.0D0,0.0D0)
      BETA   =  0.0D0
      COEMUC(1) =  (1.0D0,0.0D0)
      COEMUC(2) = (-0.5D0,0.0D0)
      COEMUC(2) = (-0.5D0,0.0D0)
      COEMUR(1) =  1.0D0
      COEMUR(2) = -0.5D0
      COEMUR(3) = -0.5D0
      IDIM(1)   =  0
      IDIM(2)   =  0
      IDIM(3)   =  0
C
      NBELQU = NBELQU / 3
      DO 10 II = 1,NBELQU
         NO(1) = ZI(JLIST+3*(II-1)-1+1)
         NO(2) = ZI(JLIST+3*(II-1)-1+2)
         NO(3) = ZI(JLIST+3*(II-1)-1+3)
         CALL JENUNO (JEXNUM(NOMA//'.NOMNOE',NO(1)),NONO(1))
         CALL JENUNO (JEXNUM(NOMA//'.NOMNOE',NO(2)),NONO(2))
         CALL JENUNO (JEXNUM(NOMA//'.NOMNOE',NO(3)),NONO(3))
         DDL(1) = 'DX'
         DDL(2) = 'DX'
         DDL(3) = 'DX'
         CALL AFRELA(COEMUR, COEMUC, DDL ,NONO,
     +                     IDIM,DIRECT,3,BETA,BETAC,BETAF,
     +                     TYPCOE,FONREE,TYPLAG,LISREL)
         DDL(1) = 'DY'
         DDL(2) = 'DY'
         DDL(3) = 'DY'
         CALL AFRELA(COEMUR, COEMUC, DDL ,NONO,
     +                     IDIM,DIRECT,3,BETA,BETAC,BETAF,
     +                       TYPCOE,FONREE,TYPLAG,LISREL)
         DDL(1) = 'DZ'
         DDL(2) = 'DZ'
         DDL(3) = 'DZ'
         CALL AFRELA(COEMUR, COEMUC, DDL ,NONO,
     +                     IDIM,DIRECT,3,BETA,BETAC,BETAF,
     +                     TYPCOE,FONREE,TYPLAG,LISREL)
 10      CONTINUE
C         CALL JEDETR (LISNOE)
C     ---------------------------------------------
        CALL AFLRCH(LISREL,CHAR)
C
 9999 CONTINUE
      CALL JEDEMA()
      END
