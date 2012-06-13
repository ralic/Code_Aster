      SUBROUTINE CESEVA(CESF,NPARA,LPARA,CESR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C A_UTIL
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER NPARA
      CHARACTER*(*) CESF,LPARA(NPARA),CESR
C ---------------------------------------------------------------------
C BUT: EVALUER LE CHAM_ELEM_S DE FONCTIONS CESF EN UTILISANT
C      LES PARAMETRES TROUVES DANS LES CHAM_ELEM_S LPARA
C ---------------------------------------------------------------------
C ARGUMENTS:
C CESF  IN/JXIN  K19 : SD CHAM_ELEM_S A EVALUER
C NPARA IN       I   : NOMBRE DE CHAM_ELEM_S PARAMETRES (LPARA)
C LPARA IN/JXIN  V(K19) : LISTE DES CHAM_ELEM_S PARAMETRES
C CESR  IN/JXOUT K19  : SD CHAM_ELEM RESULTAT DE L'EVALUATION

C REMARQUES :
C  EN CHAQUE POINT DE DISCRETISATION DE CESF, ON FERA "CALL FOINTE"
C  POUR EVALUER LES FONCTIONS AFFECTEES A CE POINT.
C  ON PASSERA EN PARAMETRES DES FONCTIONS, LES VALEURS DES CHAMPS
C  DE LPARA AVEC COMME NOM DE PARAMETRE LE NOM DE LA CMP
C  ON NE TRAITE QUE LES FONCTIONS REELLES F : R * R(* R,...) -> R
C-----------------------------------------------------------------------
C     ------------------------------------------------------------------
      INTEGER JFD,JFC,JFV,JFL,JFK
      INTEGER JPD,JPC,JPV,JPL,JPK
      INTEGER JRD,JRC,JRV,JRL,JRK
      INTEGER NBMA,IB,K,IMA,NCMP,NBPU,IER,NBPUMX
      INTEGER NCMP2,IPARA,JAD1,NCMPMX,NSPMX,NPTMX
      INTEGER K2,IADF,IADR,IADP,NBPT,NBSP,IPT,ISP,JNOMPU,JVALPU
      CHARACTER*8 MA,NOMGDF,NOMGDR,FO
      CHARACTER*8 MA2,NOMGD2,TYPCES
      CHARACTER*3 TSCA
      CHARACTER*19 F,P,R
      CHARACTER*24 VALK
      REAL*8 X
C     ------------------------------------------------------------------
      CALL JEMARQ()

C     -- 2 VECTEURS POUR STOCKER LE NOM ET LES VALEURS DES PARAMETRES
C        DES FONCTIONS :
      NBPUMX=10
      CALL WKVECT('&&CESEVA.NOMPU','V V K8',NBPUMX,JNOMPU)
      CALL WKVECT('&&CESEVA.VALPU','V V R',NBPUMX,JVALPU)

C     1- RECUPERATIONS D'INFOS DANS LE CHAMP DE FONCTIONS :
C     ------------------------------------------------------------
      F = CESF
      CALL JEVEUO(F//'.CESK','L',JFK)
      CALL JEVEUO(F//'.CESD','L',JFD)
      CALL JEVEUO(F//'.CESC','L',JFC)
      CALL JEVEUO(F//'.CESV','L',JFV)
      CALL JEVEUO(F//'.CESL','L',JFL)

      MA = ZK8(JFK-1+1)
      NOMGDF = ZK8(JFK-1+2)
      TYPCES = ZK8(JFK-1+3)
      NBMA = ZI(JFD-1+1)
      NCMP = ZI(JFD-1+2)
      NPTMX = ZI(JFD-1+3)
      NSPMX = ZI(JFD-1+4)
      NCMPMX = ZI(JFD-1+5)

      CALL DISMOI('F','TYPE_SCA',NOMGDF,'GRANDEUR',IB,TSCA,IB)
      IF (TSCA.NE.'K8') CALL U2MESS('F','UTILITAI_16')

C     2- ALLOCATION DU CHAM_ELEM_S RESULTAT ET RECUPERATION
C        DES ADRESSES DE SES OBJETS   :
C     ------------------------------------------------------------
      R = CESR
      NOMGDR = NOMGDF(1:4)//'_R'
      CALL CESCRE('V',R,TYPCES,MA,NOMGDR,NCMP,ZK8(JFC),-NPTMX,-NSPMX,
     &            -NCMPMX)
      CALL JEVEUO(R//'.CESK','L',JRK)
      CALL JEVEUO(R//'.CESD','L',JRD)
      CALL JEVEUO(R//'.CESC','L',JRC)
      CALL JEVEUO(R//'.CESV','E',JRV)
      CALL JEVEUO(R//'.CESL','E',JRL)


C     3- ON MET EN MEMOIRE LES OBJETS UTILES DES CHAMPS PARAMETRES :
C     --------------------------------------------------------------
      CALL WKVECT('&&CESEVA.JAD1','V V I',4*NPARA,JAD1)
      DO 10,IPARA = 1,NPARA
        P = LPARA(IPARA)
        CALL JEVEUO(P//'.CESK','L',JPK)
        CALL JEVEUO(P//'.CESD','L',JPD)
        CALL JEVEUO(P//'.CESC','L',JPC)
        CALL JEVEUO(P//'.CESV','L',JPV)
        CALL JEVEUO(P//'.CESL','L',JPL)
        MA2 = ZK8(JPK-1+1)
        NOMGD2 = ZK8(JPK-1+2)

        CALL DISMOI('F','TYPE_SCA',NOMGD2,'GRANDEUR',IB,TSCA,IB)
        IF (TSCA.NE.'R') CALL U2MESS('F','UTILITAI_17')
        IF (MA2.NE.MA) CALL U2MESS('F','UTILITAI_18')
        ZI(JAD1-1+4* (IPARA-1)+1) = JPC
        ZI(JAD1-1+4* (IPARA-1)+2) = JPD
        ZI(JAD1-1+4* (IPARA-1)+3) = JPL
        ZI(JAD1-1+4* (IPARA-1)+4) = JPV
   10 CONTINUE

C     4- EVALUATION DES FONCTIONS :
C     ---------------------------------------
C     ON BOUCLE D'ABORD SUR LES CMPS POUR AVOIR PLUS DE CHANCES
C     DE FAIRE PLUSIEURS FOINTE SUCCESSIFS AVEC LA MEME FONCTION.

      DO 70,K = 1,NCMP
        DO 60,IMA = 1,NBMA
          NBPT = ZI(JFD-1+5+4* (IMA-1)+1)
          NBSP = ZI(JFD-1+5+4* (IMA-1)+2)
          DO 50,IPT = 1,NBPT
            DO 40,ISP = 1,NBSP
              CALL CESEXI('C',JFD,JFL,IMA,IPT,ISP,K,IADF)
              IF (IADF.LE.0) GO TO 40

              FO = ZK8(JFV-1+IADF)

              CALL CESEXI('C',JRD,JRL,IMA,IPT,ISP,K,IADR)
              CALL ASSERT(IADR.LT.0)
              ZL(JRL-1-IADR) = .TRUE.

              IF (FO.EQ.' ') GO TO 40

C           4.1 FABRICATION DE LA LISTE DES PARAMETRES POUR FOINTE:
C           -------------------------------------------------------
              NBPU = 0
              DO 30,IPARA = 1,NPARA
                JPC = ZI(JAD1-1+4* (IPARA-1)+1)
                JPD = ZI(JAD1-1+4* (IPARA-1)+2)
                JPL = ZI(JAD1-1+4* (IPARA-1)+3)
                JPV = ZI(JAD1-1+4* (IPARA-1)+4)
                NCMP2 = ZI(JPD-1+2)
                DO 20,K2 = 1,NCMP2
                  CALL CESEXI('C',JPD,JPL,IMA,IPT,ISP,K2,IADP)
                  IF (IADP.LE.0) GO TO 20

                  NBPU = NBPU + 1
                  IF (NBPU.GT.NBPUMX) THEN
C                    -- ON AGRANDIT .NOMPU ET .VALPU :
                     NBPUMX=2*NBPUMX
                     CALL JUVECA('&&CESEVA.NOMPU',NBPUMX)
                     CALL JUVECA('&&CESEVA.VALPU',NBPUMX)
                     CALL JEVEUO('&&CESEVA.NOMPU','E',JNOMPU)
                     CALL JEVEUO('&&CESEVA.VALPU','E',JVALPU)
                  ENDIF
                  ZK8(JNOMPU-1+NBPU) = ZK8(JPC-1+K2)
                  ZR(JVALPU-1+NBPU) = ZR(JPV-1+IADP)
   20           CONTINUE
   30         CONTINUE


C           4.2 APPEL A FOINTE :
C           --------------------
              CALL FOINTE('E',FO,NBPU,ZK8(JNOMPU),ZR(JVALPU),X,IER)
              IF (IER.NE.0) THEN
                  CALL U2MESK('F+', 'FONCT0_9', 1, FO)
                  CALL JENUNO(JEXNUM(MA//'.NOMMAI',IMA),VALK)
                  CALL U2MESK('F','FONCT0_10',1, VALK)
              ENDIF

C           4.3 STOCKAGE DU RESULTAT :
C           --------------------------
              ZR(JRV-1-IADR) = X

   40       CONTINUE
   50     CONTINUE
   60   CONTINUE
   70 CONTINUE

      CALL CESTAS ( CESR )

C     5- MENAGE :
C     ---------------------------------------
      CALL JEDETR('&&CESEVA.JAD1')
      CALL JEDETR('&&CESEVA.NOMPU')
      CALL JEDETR('&&CESEVA.VALPU')

      CALL JEDEMA()
      END
