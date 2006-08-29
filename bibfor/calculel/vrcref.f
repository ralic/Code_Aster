      SUBROUTINE VRCREF(MODELE,CHMAT,CARELE,CHVREF)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 28/08/2006   AUTEUR CIBHHPD L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT   NONE
      CHARACTER*8 MODELE,CHMAT,CARELE
      CHARACTER*19 CHVREF
C ======================================================================
C   BUT : FABRIQUER LE CHAMP DE VARIABLES DE COMMANDE DE "REFERENCE"
C   ARGUMENTS :
C   MODELE (K8)  IN/JXIN : SD MODELE
C   CHMAT  (K8)  IN/JXIN : SD CHAM_MATER
C   CARELE (K8)  IN/JXIN : SD CARA_ELEM (SOUS-POINTS)
C   CHVREF (K19) IN/JXOUT: SD CHAM_ELEM/ELGA (VARC DE "REFERENCE")
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

      INTEGER N1,IRE2,IAD,ISP,IPT,JCESV
      INTEGER K,K2,NBMA,NCMP,ICMP,JCESL1,JCESV1,JCESD1
      INTEGER JCESD,JCESL,IMA,NBPT,NBSP,NBCVRC,JCVVAR
      INTEGER JDCLD,JDCLL,JDCLV,NNCP,IRET
      CHARACTER*8 KBID,VARC
      CHARACTER*19  DCELI,CELMOD,CART1,CES1,LIGRMO,CSVREF
      REAL*8 VALREF
C ----------------------------------------------------------------------

      CALL JEMARQ()


      CALL JEEXIN(CHMAT//'.CVRCVARC',IRE2)
      IF (IRE2.EQ.0) GOTO 9999

      CALL JEVEUO(CHMAT//'.CVRCVARC','L',JCVVAR)
      CALL JELIRA(CHMAT//'.CVRCVARC','LONMAX',NBCVRC,KBID)
      LIGRMO=MODELE//'.MODELE'


C     1. ALLOCATION DE CSVREF
C     ------------------------------------------
      DCELI='&&VRCREF.DCELI'
      CELMOD='&&VRCREF.CELMOD'
      CSVREF='&&VRCREF.CSVREF'
      CALL CESVAR(CARELE,' ',LIGRMO,DCELI)

C     -- MODIFICATION DE DCELI : TOUTES LES MAILLES ONT
C        NBCVRC COMPOSANTES.
      CALL JEVEUO(DCELI//'.CESD','L',JDCLD)
      CALL JEVEUO(DCELI//'.CESL','L',JDCLL)
      CALL JEVEUO(DCELI//'.CESV','E',JDCLV)
      NBMA = ZI(JDCLD-1+1)

      DO 170,IMA = 1,NBMA
        NBPT = ZI(JDCLD-1+5+4* (IMA-1)+1)
        NBSP = MAX(1,ZI(JDCLD-1+5+4* (IMA-1)+2))
        CALL ASSERT(NBPT.EQ.1)
        CALL ASSERT(NBSP.EQ.1)
        CALL CESEXI('C',JDCLD,JDCLL,IMA,1,1,2,IAD)
        IF (IAD.GT.0) ZI(JDCLV-1+IAD)=NBCVRC
170   CONTINUE

      CALL ALCHML(LIGRMO,'INIT_VARC','PVARCPR','V',CELMOD,IRET,DCELI)
      CALL ASSERT(IRET.EQ.0)
      CALL DETRSD('CHAMP',DCELI)
      CALL CELCES(CELMOD,'V',CSVREF)
      CALL DETRSD('CHAMP',CELMOD)

      CALL JELIRA(CSVREF//'.CESV','LONMAX',N1,KBID)

      CALL JEVEUO(CSVREF//'.CESD','L',JCESD)
      CALL JEVEUO(CSVREF//'.CESL','E',JCESL)
      CALL JEVEUO(CSVREF//'.CESV','E',JCESV)
      CALL JELIRA(CSVREF//'.CESL','LONMAX',N1,KBID)
      DO 777, K=1,N1
        ZL(JCESL-1+K)=.FALSE.
777   CONTINUE



C     2. REMPLISSAGE DE CSVREF.CESV :
C     ------------------------------------------
      VARC=' '
      DO 1, K=1,NBCVRC
        IF (ZK8(JCVVAR-1+K).EQ.VARC) GO TO 1
        VARC=ZK8(JCVVAR-1+K)
        CART1 = CHMAT//'.'//VARC//'.1'
        CES1='&&VRCREF.CES1'
        CALL  CARCES(CART1,'ELEM',' ','V',CES1,IRET)
        CALL ASSERT(IRET.EQ.0)

        CALL JEVEUO(CES1//'.CESD','L',JCESD1)
        CALL JEVEUO(CES1//'.CESV','L',JCESV1)
        CALL JEVEUO(CES1//'.CESL','L',JCESL1)

        NBMA = ZI(JCESD-1+1)
        CALL ASSERT(NBMA.EQ.ZI(JCESD1-1+1))

C       -- CALCUL DE NCMP
        NCMP=0
        DO 69, K2=K,NBCVRC
          IF (ZK8(JCVVAR-1+K2).EQ.VARC) NCMP=NCMP+1
69      CONTINUE

        DO 70,IMA = 1,NBMA
          NBPT = ZI(JCESD-1+5+4* (IMA-1)+1)
          NBSP = MAX(1,ZI(JCESD-1+5+4* (IMA-1)+2))

          CALL CESEXI('C',JCESD1,JCESL1,IMA,1,1,1,IAD)
          IF (IAD.LE.0) GO TO 70
          VALREF=ZR(JCESV1-1+IAD)

          DO 60,IPT = 1,NBPT
            DO 50,ISP = 1,NBSP
              DO 51,ICMP = 1,NCMP
                CALL CESEXI('C',JCESD,JCESL,IMA,IPT,ISP,K-1+ICMP,IAD)
                CALL ASSERT(IAD.LT.0)
                IAD=-IAD
                ZL(JCESL-1+IAD)=.TRUE.
                ZR(JCESV-1+IAD)=VALREF
51          CONTINUE
50          CONTINUE
60        CONTINUE
70      CONTINUE
        CALL DETRSD('CHAMP',CES1)
1     CONTINUE


C     3. RECOPIE DU CHAMP SIMPLE DANS LE CHAMP CHVREF
C     -----------------------------------------------------
      CALL CESCEL(CSVREF,LIGRMO,'INIT_VARC','PVARCPR','NON',NNCP,
     &            'V',CHVREF)
      CALL DETRSD('CHAM_ELEM_S',CSVREF)

9999  CONTINUE
      CALL JEDEMA()
      END
