      SUBROUTINE CESCES(CESA,TYPCES,CESMOZ,MNOGAZ,CELFPZ,BASE,CESB)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE VABHHTS J.PELLET
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*) CESA,CESB,BASE,CESMOZ,TYPCES,MNOGAZ,CELFPZ
C ------------------------------------------------------------------
C BUT: TRANSFORMER UN CHAM_ELEM_S EN CHAM_ELEM_S D'UN AUTRE TYPE
C      ELNO -> ELGA , ELEM -> ELNO , ...
C ------------------------------------------------------------------
C     ARGUMENTS:
C CESA  IN/JXIN  K19 : SD CHAM_ELEM_S A TRANSFORMER

C TYPCES IN       K4  : TYPE VOULU POUR LE NOUVEAU CHAM_ELEM_S
C                      /'ELEM' /'ELGA' /'ELNO'

C CESMOZ IN/JXIN  K19 :  SD CHAM_ELEM_S "MODELE" POUR CESZ
C       SI TYPCES  ='ELGA' ON SE SERT DE CESMOZ POUR DETERMINER
C          LE NOMBRE DE POINTS DE GAUSS DE CESB

C MNOGAZ IN/JXIN  K19 :
C    SD CHAM_ELEM_S CONTENANT LES MATRICES DE PASSAGE NOEUD -> GAUSS.
C    CET OBJET N'EST UTILISE QUE SI ELNO -> ELGA
C    CET OBJET EST OBTENU PAR LA ROUTINE MANOPG.F
C ATTENTION :  MNOGAZ EST UN CHAM_ELEM_S AVEC UNE CONVENTION
C              TRES PARTICULIERE  (MAILLE DE REFERENCE)
C              (VOIR ROUTINE MANOPG.F)

C CELFPZ IN/JXIN  K24 :
C    NOM DE L'OBJET DECRIVANT LES FAMILLES DE P.G. DE CESA (OU ' ')
C    CET OBJET N'EST UTILISE QUE SI ELGA -> ELNO
C    CET OBJET EST OBTENU PAR LA ROUTINE CELFPG.F


C BASE    IN      K1  : BASE DE CREATION POUR CESB : G/V
C-----------------------------------------------------------------------


C-----------------------------------------------------------------------

C     ------------------------------------------------------------------
      INTEGER IMA,IBID,NCMP,ICMP
      INTEGER JCESD,JCESV,JCESL,NBMA,IRET,NBSP,NBNO,ICO
      INTEGER IAD,JNBPT,INO,ISP,NBPG2,NBNO2,IAD1,JNBCMP
      INTEGER JCEMD,JNBSP,ILCNX1,IACNX1,NBPG,IPG,JBREF
      INTEGER MNOGAL,MNOGAD,MNOGAV,MNOGAK,NBNO1,IMAREF
      INTEGER JCES1K,JCES1D,JCES1L,JCES1V,JCES1C
      INTEGER NBPT,NBPT1,NBSP1,IPT,IPT1,NBV
      CHARACTER*1 KBID
      CHARACTER*4 TYPCE1
      CHARACTER*8 MA,NOMGD
      CHARACTER*3 TSCA
      CHARACTER*19 CES1,CESMOD,CES2,MNOGA
      CHARACTER*24 CELFPG
      REAL*8 VR,V1R
      COMPLEX*16 VC,V1C
C     ------------------------------------------------------------------
      CALL JEMARQ()

      CES1 = CESA
      CES2 = CESB
      CESMOD = CESMOZ
      MNOGA = MNOGAZ
      CELFPG=CELFPZ


C     1. RECUPERATION DE :
C        MA     : NOM DU MAILLAGE
C        NOMGD  : NOM DE LA GRANDEUR
C        TYPCE1 : TYPE DE CES1 : ELGA/ELNO/ELEM
C        NCMP   : NOMBRE DE CMPS DE CES1
C        TSCA   : TYPE SCALAIRE DE LA GRANDEUR : R/C/I ...
C        NBMA   : NOMBRE DE MAILLES DU MAILLAGE
C        ILCNX1,IACNX1   : ADRESSES DE LA CONNECTIVITE DU MAILLAGE
C     --------------------------------------------------------------
      CALL EXISD('CHAM_ELEM_S',CES1,IRET)
      CALL ASSERT(IRET.GT.0)
      CALL JEVEUO(CES1//'.CESK','L',JCES1K)
      CALL JEVEUO(CES1//'.CESC','L',JCES1C)
      CALL JEVEUO(CES1//'.CESD','L',JCES1D)
      CALL JEVEUO(CES1//'.CESV','L',JCES1V)
      CALL JEVEUO(CES1//'.CESL','L',JCES1L)
      MA = ZK8(JCES1K-1+1)
      NOMGD = ZK8(JCES1K-1+2)
      TYPCE1 = ZK8(JCES1K-1+3)
      CALL DISMOI('F','NB_MA_MAILLA',MA,'MAILLAGE',NBMA,KBID,IBID)
      CALL JEVEUO(MA//'.CONNEX','L',IACNX1)
      CALL JEVEUO(JEXATR(MA//'.CONNEX','LONCUM'),'L',ILCNX1)
      CALL JELIRA(CES1//'.CESC','LONMAX',NCMP,KBID)
      CALL DISMOI('F','TYPE_SCA',NOMGD,'GRANDEUR',IBID,TSCA,IBID)


C     2. SI C'EST FACILE, ON LE FAIT ... :
C     ------------------------------------------------
      IF (TYPCE1.EQ.TYPCES) THEN
        CALL COPISD('CHAM_ELEM_S',BASE,CES1,CES2)
        GO TO 180
      END IF
      CALL ASSERT(TSCA.EQ.'R'.OR.TSCA.EQ.'C')



C     3. VERIFICATIONS :
C     ---------------------------
      IF ((TYPCE1.EQ.'ELNO') .AND. (TYPCES.EQ.'ELGA')) THEN
        CALL EXISD('CHAM_ELEM_S',MNOGA,IRET)
        CALL ASSERT(IRET.GT.0)
        CALL JEVEUO(MNOGA//'.CESK','L',JBREF)
        CALL ASSERT(MA.EQ.ZK8(JBREF-1+1))
      END IF


C     4. CALCUL DES OBJETS  '.NBPT','.NBSP' ET '.NBCMP'
C     -----------------------------------------------------------------
      CALL WKVECT('&&CESCES.NBPT','V V I',NBMA,JNBPT)
      CALL WKVECT('&&CESCES.NBSP','V V I',NBMA,JNBSP)
      CALL WKVECT('&&CESCES.NBCMP','V V I',NBMA,JNBCMP)


      IF (TYPCES.EQ.'ELEM') THEN
        DO 10,IMA = 1,NBMA
          ZI(JNBPT-1+IMA) = 1
          ZI(JNBSP-1+IMA) = 1
          ZI(JNBCMP-1+IMA) = ZI(JCES1D-1+5+4* (IMA-1)+3)
   10   CONTINUE

      ELSE IF (TYPCES.EQ.'ELNO') THEN
        DO 20,IMA = 1,NBMA
          ZI(JNBPT-1+IMA) = ZI(ILCNX1+IMA) - ZI(ILCNX1+IMA-1)
          ZI(JNBSP-1+IMA) = ZI(JCES1D-1+5+4* (IMA-1)+2)
          ZI(JNBCMP-1+IMA) = ZI(JCES1D-1+5+4* (IMA-1)+3)
   20   CONTINUE

      ELSE IF (TYPCES.EQ.'ELGA') THEN
        CALL EXISD('CHAM_ELEM_S',CESMOD,IRET)
C       TEST ARGUMENT CESMOD OBLIGATOIRE
        CALL ASSERT(IRET.GT.0)
        CALL JEVEUO(CESMOD//'.CESD','L',JCEMD)
        DO 30,IMA = 1,NBMA
          ZI(JNBPT-1+IMA) = ZI(JCEMD-1+5+4* (IMA-1)+1)
          ZI(JNBSP-1+IMA) = ZI(JCES1D-1+5+4* (IMA-1)+2)
          ZI(JNBCMP-1+IMA) = ZI(JCES1D-1+5+4* (IMA-1)+3)
   30   CONTINUE

      ELSE
        CALL ASSERT(.FALSE.)
      END IF


C     5. CREATION DE CES2 :
C     ---------------------------------------
      CALL CESCRE(BASE,CES2,TYPCES,MA,NOMGD,NCMP,ZK8(JCES1C),ZI(JNBPT),
     &            ZI(JNBSP),ZI(JNBCMP))

      CALL JEVEUO(CES2//'.CESD','L',JCESD)
      CALL JEVEUO(CES2//'.CESV','E',JCESV)
      CALL JEVEUO(CES2//'.CESL','E',JCESL)



C     6- REMPLISSAGE DES OBJETS .CESL ET .CESV :
C     ------------------------------------------


      IF ((TYPCE1.EQ.'ELNO') .AND. (TYPCES.EQ.'ELGA')) THEN
C     ------------------------------------------------------
        MNOGA = MNOGAZ
        CALL JEVEUO(MNOGA//'.CESK','L',MNOGAK)
        CALL JEVEUO(MNOGA//'.CESD','L',MNOGAD)
        CALL JEVEUO(MNOGA//'.CESL','L',MNOGAL)
        CALL JEVEUO(MNOGA//'.CESV','L',MNOGAV)
        CALL ASSERT(ZK8(MNOGAK).EQ.MA)

        DO 90,IMA = 1,NBMA
          CALL CESEXI('C',MNOGAD,MNOGAL,IMA,1,1,1,IAD)
          IF (IAD.LE.0) GO TO 90
          IF (NINT(ZR(MNOGAV-1+IAD)).GT.0) THEN
             IMAREF=IMA
          ELSE
             IMAREF=-NINT(ZR(MNOGAV-1+IAD))
          ENDIF
          CALL CESEXI('C',MNOGAD,MNOGAL,IMAREF,1,1,1,IAD)
          IF (IAD.LE.0) GO TO 90

          NBNO2 = NINT(ZR(MNOGAV-1+IAD))
          NBPG2 = NINT(ZR(MNOGAV-1+IAD+1))

          NBPG = ZI(JCESD-1+5+4* (IMA-1)+1)
          NBSP = ZI(JCESD-1+5+4* (IMA-1)+2)
          NBNO = ZI(ILCNX1+IMA) - ZI(ILCNX1-1+IMA)

          NBNO1 = ZI(JCES1D-1+5+4* (IMA-1)+1)

          CALL ASSERT(NBNO.EQ.NBNO1)
          CALL ASSERT(NBNO.EQ.NBNO2)
          CALL ASSERT(NBPG.EQ.NBPG2)

          DO 80 ICMP = 1,NCMP
            DO 70,ISP = 1,NBSP

C             - ON VERIFIE QUE TOUS LES NOEUDS PORTENT BIEN LA CMP :
              ICO = 0
              DO 40,INO = 1,NBNO
                CALL CESEXI('C',JCES1D,JCES1L,IMA,INO,ISP,ICMP,IAD1)
                IF (IAD1.GT.0) ICO = ICO + 1
   40         CONTINUE
              IF (ICO.NE.NBNO) GO TO 70

              IF (TSCA.EQ.'R') THEN
                DO 60,IPG = 1,NBPG
                  VR = 0.D0
                  DO 50,INO = 1,NBNO
                    CALL CESEXI('C',JCES1D,JCES1L,IMA,INO,ISP,ICMP,IAD1)
                    V1R = ZR(JCES1V-1+IAD1)
                    VR = VR + V1R*ZR(MNOGAV-1+IAD+1+NBNO* (IPG-1)+INO)
   50             CONTINUE

                  CALL CESEXI('C',JCESD,JCESL,IMA,IPG,ISP,ICMP,IAD1)
                  CALL ASSERT(IAD1.LT.0)
                  ZL(JCESL-1-IAD1) = .TRUE.
                  ZR(JCESV-1-IAD1) = VR
   60           CONTINUE

              ELSEIF (TSCA.EQ.'C') THEN
                DO 61,IPG = 1,NBPG
                  VC = DCMPLX(0.D0,0.D0)
                  DO 51,INO = 1,NBNO
                    CALL CESEXI('C',JCES1D,JCES1L,IMA,INO,ISP,ICMP,IAD1)
                    V1C = ZC(JCES1V-1+IAD1)
                    VC = VC + V1C*ZR(MNOGAV-1+IAD+1+NBNO* (IPG-1)+INO)
   51             CONTINUE

                  CALL CESEXI('C',JCESD,JCESL,IMA,IPG,ISP,ICMP,IAD1)
                  CALL ASSERT(IAD1.LT.0)
                  ZL(JCESL-1-IAD1) = .TRUE.
                  ZC(JCESV-1-IAD1) = VC
   61           CONTINUE
              ENDIF
   70       CONTINUE

   80     CONTINUE
   90   CONTINUE


      ELSE IF ((TYPCE1.EQ.'ELEM') .AND. (TYPCES(1:2).EQ.'EL')) THEN
C     ------------------------------------------------------
        DO 130,IMA = 1,NBMA
          NBPT = ZI(JCESD-1+5+4* (IMA-1)+1)
          NBSP1 = ZI(JCES1D-1+5+4* (IMA-1)+2)

          DO 120 ICMP = 1,NCMP
            DO 110,ISP = 1,NBSP1
              CALL CESEXI('C',JCES1D,JCES1L,IMA,1,ISP,ICMP,IAD1)
              IF (IAD1.LE.0) GO TO 110

              DO 100,IPT = 1,NBPT
                CALL CESEXI('C',JCESD,JCESL,IMA,IPT,ISP,ICMP,IAD)
                CALL ASSERT(IAD.LT.0)
                ZL(JCESL-1-IAD) = .TRUE.
                IF (TSCA.EQ.'R') THEN
                  ZR(JCESV-1-IAD) = ZR(JCES1V-1+IAD1)
                ELSEIF (TSCA.EQ.'C') THEN
                  ZC(JCESV-1-IAD) = ZC(JCES1V-1+IAD1)
                ENDIF
  100         CONTINUE
  110       CONTINUE

  120     CONTINUE
  130   CONTINUE


      ELSE IF ((TYPCE1(1:2).EQ.'EL') .AND. (TYPCES.EQ.'ELEM')) THEN
C     ------------------------------------------------------
        DO 170,IMA = 1,NBMA
          NBPT1 = ZI(JCES1D-1+5+4* (IMA-1)+1)
          NBSP1 = ZI(JCES1D-1+5+4* (IMA-1)+2)
          IF ((NBPT1*NBSP1).EQ.0) GOTO 170

          DO 160 ICMP = 1,NCMP
            DO 150,ISP = 1,NBSP1
              IF (TSCA.EQ.'R') THEN
                VR = 0.D0
              ELSEIF (TSCA.EQ.'C') THEN
                VC = DCMPLX(0.D0,0.D0)
              ENDIF
              NBV = 0
              DO 140,IPT1 = 1,NBPT1
                CALL CESEXI('C',JCES1D,JCES1L,IMA,IPT1,ISP,ICMP,IAD1)
                IF (IAD1.LE.0) GO TO 140
                NBV = NBV + 1
                IF (TSCA.EQ.'R') THEN
                  VR = VR + ZR(JCES1V-1+IAD1)
                ELSEIF (TSCA.EQ.'C') THEN
                  VC = VC + ZC(JCES1V-1+IAD1)
                ENDIF
  140         CONTINUE

C             -- ON N'AFFECTE DE VALEUR A LA MAILLE QUE SI TOUS
C                LES POINTS ONT CONTRIBUE :
              IF (NBV.EQ.NBPT1) THEN
                CALL CESEXI('C',JCESD,JCESL,IMA,1,ISP,ICMP,IAD)
                CALL ASSERT(IAD.LT.0)
                ZL(JCESL-1-IAD) = .TRUE.
                IF (TSCA.EQ.'R') THEN
                  ZR(JCESV-1-IAD) = VR/DBLE(NBV)
                ELSEIF (TSCA.EQ.'C') THEN
                  ZC(JCESV-1-IAD) = VC/DBLE(NBV)
                ENDIF
              END IF
  150       CONTINUE
  160     CONTINUE
  170   CONTINUE


      ELSE IF ((TYPCE1.EQ.'ELGA') .AND. (TYPCES.EQ.'ELNO')) THEN
C     ------------------------------------------------------
        CALL CESGNO(CES1,CELFPG,BASE,CES2)


      ELSE
C       CAS NON ENCORE PROGRAMME ...
        CALL ASSERT(.FALSE.)
      END IF


C     7- MENAGE :
C     -----------
      CALL JEDETR('&&CESCES.NBPT')
      CALL JEDETR('&&CESCES.NBSP')
      CALL JEDETR('&&CESCES.NBCMP')

  180 CONTINUE

      CALL JEDEMA()
      END
