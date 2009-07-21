      SUBROUTINE NMCADT(SDDISC,IOCC,NUMINS,VALMOI,VALPLU,DTP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/07/2009   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GENIAUT S.GENIAUT
C
      IMPLICIT NONE
      INTEGER      IOCC,NUMINS
      CHARACTER*24 VALMOI(8),VALPLU(8)
      CHARACTER*19 SDDISC   
      REAL*8       DTP  
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME)
C
C CALCUL DU NOUVEAU PAS DE TEMPS EN CAS D'ADAPTATIONS
C      
C ----------------------------------------------------------------------
C
C
C IN  SDDISC : SD DISCRETISATION TEMPORELLE
C   ...
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C 
      INTEGER     IB,NIT,JITER,ITERAT,NUCMP
      REAL*8      R8B,DTM,PCENT,VALREF,DVAL,VALEUR,R8MAEM,R8PREM,R8VIDE
      CHARACTER*8 K8B
      CHARACTER*16 MODETP,NOCHAM,NOCMP,TYPCH
      CHARACTER*19 DCH,DCHS
      CHARACTER*24 DEPMOI,VARMOI,SIGMOI
      CHARACTER*24 DEPPLU,VARPLU,SIGPLU,CHPLU,CHMOI,K24BID
      INTEGER      JCESD,JCESL,JCESV
      INTEGER      NBMA,IMA,IPT,ISP,ICMP,NBPT,NBSP,NBCMP,IAD
      INTEGER      JCNSV,JCNSL,JCNSD,NBNO,INO
C 
C ----------------------------------------------------------------------
C
      CALL JEMARQ()

C     METHODE DE CALCUL DE DT+
      CALL UTDIDT('L',SDDISC,'ADAP',IOCC,'METHODE',R8B,IB,MODETP)

C     PAS DE TEMPS PAR DEFAUT (LE DERNIER, SAUF SI JALON)
      CALL UTDIDT('L',SDDISC,'LIST',IB,'DT-',DTM,IB,K8B)

C     ------------------------------------------------------------------
      IF (MODETP.EQ.'FIXE') THEN
C     ------------------------------------------------------------------

        CALL UTDIDT('L',SDDISC,'ADAP',IOCC,'PCENT_AUGM',PCENT,IB,K8B)
        DTP = DTM * (1.D0 + PCENT / 100.D0)

C     ------------------------------------------------------------------
      ELSEIF (MODETP.EQ.'DELTA_GRANDEUR') THEN
C     ------------------------------------------------------------------

        CALL UTDIDT('L',SDDISC,'ADAP',IOCC,'NOM_CHAM',R8B,IB,NOCHAM)
        CALL UTDIDT('L',SDDISC,'ADAP',IOCC,'NOM_CMP',R8B,IB,NOCMP)
        CALL UTDIDT('L',SDDISC,'ADAP',IOCC,'VALE_REF',VALREF,IB,K8B)
        CALL UTDIDT('L',SDDISC,'ADAP',IOCC,'NU_CMP',R8B,NUCMP,K8B)

C       DECOMPACTION DES VARIABLES CHAPEAUX
        CALL DESAGG(VALPLU,DEPPLU,SIGPLU,VARPLU,K24BID,
     &              K24BID,K24BID,K24BID,K24BID)  
        CALL DESAGG(VALMOI,DEPMOI,SIGMOI,VARMOI,K24BID,
     &              K24BID,K24BID,K24BID,K24BID)         

        IF (NOCHAM.EQ.'VARI_ELGA') THEN
          CHPLU = VARPLU
          CHMOI = VARMOI
          TYPCH = 'CHAM_ELGA'
        ELSEIF (NOCHAM.EQ.'SIEF_ELGA') THEN
          CHPLU = SIGPLU
          CHMOI = SIGMOI
          TYPCH = 'CHAM_ELGA'
        ELSEIF (NOCHAM.EQ.'DEPL') THEN
          CHPLU = DEPPLU
          CHMOI = DEPMOI
          TYPCH = 'CHAM_NO'
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF

C       DCH = CHPLU - CHMOI
        DCH = '&&NMCADT.DELTACH'
        DCHS = '&&NMCADT.DELTACHS'
        CALL BARYCH(CHPLU,CHMOI,1.D0,-1.D0,DCH,'V')
C       ON APPELLERA MEMAX QUAND CETTE ROUTINE SERA MIEUX PROGRAMMEE
C       MAIS C'EST PAS SUR QUE L'ON Y ARRIVE CAR ON SOUHAITE LE MIN DE
C       VREF / |VALEUR| DONC IL FAUT D'ABORD FAIRE :
C       DCH = VREF / ABS(DCH)
C       PUIS DVAL = MIN ( DCH )
        DVAL = R8MAEM()
        IF (TYPCH(1:7).EQ.'CHAM_EL') THEN
          CALL CELCES(DCH,'V',DCHS)
          CALL CESRED(DCHS,0,IB,1,NOCMP,'V',DCHS)
C          CALL IMPRSD('CHAMP',DCHS,6,'DCHSREDUIT')
          CALL JEVEUO(DCHS//'.CESD','L',JCESD)
          CALL JEVEUO(DCHS//'.CESL','L',JCESL)
          CALL JEVEUO(DCHS//'.CESV','L',JCESV)
          NBMA = ZI(JCESD-1+1)
          DO 40,IMA = 1,NBMA
            NBPT = ZI(JCESD-1+5+4* (IMA-1)+1)
            NBSP = ZI(JCESD-1+5+4* (IMA-1)+2)
            NBCMP = ZI(JCESD-1+5+4* (IMA-1)+3)
            DO 30,IPT = 1,NBPT
              DO 20,ISP = 1,NBSP
                DO 10,ICMP = 1,NBCMP
                  CALL CESEXI('C',JCESD,JCESL,IMA,IPT,ISP,ICMP,IAD)
                  IF (IAD.GT.0) THEN
                    VALEUR = ZR(JCESV-1+IAD)
                    IF (ABS(VALEUR).GT.R8PREM()) THEN
                      DVAL = MIN(DVAL,VALREF/ABS(VALEUR))
                    ENDIF
                  ENDIF
   10           CONTINUE
   20         CONTINUE
   30       CONTINUE
   40     CONTINUE
        ELSEIF (TYPCH.EQ.'CHAM_NO') THEN
          CALL CNOCNS(DCH,'V',DCHS)
          CALL CNSRED(DCHS,0,IB,1,NOCMP,'V',DCHS)
C          CALL IMPRSD('CHAMP',DCHS,6,'DCHSREDUIT')
          CALL JEVEUO(DCHS//'.CNSV','L',JCNSV)
          CALL JEVEUO(DCHS//'.CNSL','L',JCNSL)
          CALL JEVEUO(DCHS//'.CNSD','L',JCNSD)
          NBNO = ZI(JCNSD-1+1)
          DO 60,INO=1,NBNO
            IF (ZL(JCNSL-1+INO)) THEN
              VALEUR = ZR(JCNSV-1+INO)
              IF (ABS(VALEUR).GT.R8PREM()) THEN
                DVAL = MIN(DVAL,VALREF/ABS(VALEUR))
              ENDIF
            ENDIF
 60       CONTINUE
        ENDIF

C       LE CHAMP DE VARIATION EST IDENTIQUEMENT NUL : ON SORT
        IF (DVAL.EQ.R8MAEM()) GOTO 9999

        DTP = DTM * DVAL

C     ------------------------------------------------------------------
      ELSEIF (MODETP.EQ.'ITER_NEWTON') THEN
C     ------------------------------------------------------------------

        CALL UTDIDT('L',SDDISC,'ADAP',IOCC,'NB_ITER_NEWTON_REF',
     &                                                   R8B,NIT,K8B)
        CALL JEVEUO(SDDISC//'.ITER','L',JITER)
        ITERAT = ZI(JITER-1+NUMINS)
        DTP = DTM * SQRT( DBLE(NIT) / DBLE(ITERAT+1) )

C     ------------------------------------------------------------------
      ELSEIF (MODETP.EQ.'FORMULE') THEN
C     ------------------------------------------------------------------

        CALL ASSERT(.FALSE.)

C     ------------------------------------------------------------------
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

 9999 CONTINUE
      CALL JEDEMA()
      END
