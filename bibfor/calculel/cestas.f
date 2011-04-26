      SUBROUTINE CESTAS(CESZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE VABHHTS J.PELLET
      IMPLICIT NONE
      CHARACTER*(*) CESZ
C ---------------------------------------------------------------------
C BUT: "TASSER" UN CHAM_ELEM_S LORSQU'IL A ETE ALLOUE TROP GRAND
C ---------------------------------------------------------------------
C     ARGUMENTS:
C CESZ   IN/JXIN  K19 : SD CHAM_ELEM_S A TASSER
C-----------------------------------------------------------------------

C---- COMMUNS NORMALISES  JEVEUX
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
C     ------------------------------------------------------------------
      INTEGER JCESK,JCESD,JCESV,JCESL,JCESC,NBMA
      INTEGER JCE2D,JCE2V,JCE2L
      INTEGER IBID,JNBPT,JNBSP,JNBCMP,ICMP
      INTEGER IMA,IPT,ISP,NBPT,NBSP,IAD,IAD2
      INTEGER NCMP,NBCMP,NBPT2,NBSP2,NBCMP2
      CHARACTER*1 BASE
      CHARACTER*8 MA,NOMGD,TYPCES
      CHARACTER*3 TSCA
      CHARACTER*19 CES,CES2
C     ------------------------------------------------------------------
      CALL JEMARQ()


      CES = CESZ

      CALL JEVEUO(CES//'.CESK','L',JCESK)
      CALL JEVEUO(CES//'.CESD','L',JCESD)
      CALL JEVEUO(CES//'.CESC','L',JCESC)
      CALL JEVEUO(CES//'.CESL','L',JCESL)
      CALL JEVEUO(CES//'.CESV','L',JCESV)

      CALL JELIRA(CES//'.CESK','CLAS',IBID,BASE)

      MA = ZK8(JCESK-1+1)
      NOMGD = ZK8(JCESK-1+2)
      TYPCES = ZK8(JCESK-1+3)

      NBMA = ZI(JCESD-1+1)
      NCMP = ZI(JCESD-1+2)


      CALL DISMOI('F','TYPE_SCA',NOMGD,'GRANDEUR',IBID,TSCA,IBID)



C     1- CALCUL DES OBJETS :
C        '&&CESTAS.NBPT'
C        '&&CESTAS.NBSP'
C        '&&CESTAS.NBCMP'
C     -----------------------------------------------------------
      CALL WKVECT('&&CESTAS.NBPT','V V I',NBMA,JNBPT)
      CALL WKVECT('&&CESTAS.NBSP','V V I',NBMA,JNBSP)
      CALL WKVECT('&&CESTAS.NBCMP','V V I',NBMA,JNBCMP)

      DO 40,IMA = 1,NBMA
        NBPT = ZI(JCESD-1+5+4* (IMA-1)+1)
        NBSP = ZI(JCESD-1+5+4* (IMA-1)+2)
        NBCMP = ZI(JCESD-1+5+4* (IMA-1)+3)
        NBPT2 = 0
        NBSP2 = 0
        NBCMP2 = 0

        DO 30,IPT = 1,NBPT
          DO 20,ISP = 1,NBSP
            DO 10,ICMP = 1,NBCMP
              CALL CESEXI('C',JCESD,JCESL,IMA,IPT,ISP,ICMP,IAD)
              IF (IAD.GT.0) THEN
                NBPT2 = IPT
                NBSP2 = ISP
                NBCMP2 = ICMP
              END IF
   10       CONTINUE
   20     CONTINUE
   30   CONTINUE
        ZI(JNBPT-1+IMA) = NBPT2
        ZI(JNBSP-1+IMA) = NBSP2
        ZI(JNBCMP-1+IMA) = NBCMP2
   40 CONTINUE



C     2- ALLOCATION DE CES2 :
C     --------------------------
      CES2 = '&&CESTAS.CES2'
      CALL CESCRE(BASE,CES2,TYPCES,MA,NOMGD,NCMP,ZK8(JCESC),ZI(JNBPT),
     &            ZI(JNBSP),ZI(JNBCMP))
      CALL JEVEUO(CES2//'.CESD','L',JCE2D)
      CALL JEVEUO(CES2//'.CESV','E',JCE2V)
      CALL JEVEUO(CES2//'.CESL','E',JCE2L)




C     3- RECOPIE DES VALEURS DE CES DANS CES2 :
C     -----------------------------------------------------------
      DO 80,IMA = 1,NBMA
        NBPT = ZI(JCE2D-1+5+4* (IMA-1)+1)
        NBSP = ZI(JCE2D-1+5+4* (IMA-1)+2)
        NBCMP = ZI(JCE2D-1+5+4* (IMA-1)+3)

        DO 70,IPT = 1,NBPT
          DO 60,ISP = 1,NBSP
            DO 50,ICMP = 1,NBCMP
              CALL CESEXI('C',JCESD,JCESL,IMA,IPT,ISP,ICMP,IAD)
              CALL CESEXI('C',JCE2D,JCE2L,IMA,IPT,ISP,ICMP,IAD2)
              IF (IAD.GT.0) THEN
                CALL ASSERT(IAD2.LT.0)
                IAD2 = -IAD2
                ZL(JCE2L-1+IAD2) = .TRUE.

                IF (TSCA.EQ.'R') THEN
                  ZR(JCE2V-1+IAD2) = ZR(JCESV-1+IAD)
                ELSE IF (TSCA.EQ.'I') THEN
                  ZI(JCE2V-1+IAD2) = ZI(JCESV-1+IAD)
                ELSE IF (TSCA.EQ.'C') THEN
                  ZC(JCE2V-1+IAD2) = ZC(JCESV-1+IAD)
                ELSE IF (TSCA.EQ.'L') THEN
                  ZL(JCE2V-1+IAD2) = ZL(JCESV-1+IAD)
                ELSE IF (TSCA.EQ.'K8') THEN
                  ZK8(JCE2V-1+IAD2) = ZK8(JCESV-1+IAD)
                ELSE IF (TSCA.EQ.'K16') THEN
                  ZK16(JCE2V-1+IAD2) = ZK16(JCESV-1+IAD)
                ELSE IF (TSCA.EQ.'K24') THEN
                  ZK24(JCE2V-1+IAD2) = ZK24(JCESV-1+IAD)
                ELSE IF (TSCA.EQ.'K32') THEN
                  ZK32(JCE2V-1+IAD2) = ZK32(JCESV-1+IAD)
                ELSE IF (TSCA.EQ.'K80') THEN
                  ZK80(JCE2V-1+IAD2) = ZK80(JCESV-1+IAD)
                ELSE
                  CALL ASSERT(.FALSE.)
                END IF
              END IF
   50       CONTINUE
   60     CONTINUE
   70   CONTINUE
   80 CONTINUE



      CALL DETRSD('CHAM_ELEM_S',CES)
      CALL COPISD('CHAM_ELEM_S',BASE,CES2,CES)



C     7- MENAGE :
C     -----------
      CALL DETRSD('CHAM_ELEM_S',CES2)
      CALL JEDETR('&&CESTAS.NBPT')
      CALL JEDETR('&&CESTAS.NBSP')
      CALL JEDETR('&&CESTAS.NBCMP')

      CALL JEDEMA()
      END
