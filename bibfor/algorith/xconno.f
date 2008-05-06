      SUBROUTINE XCONNO(MOX,CHFIS,BASE,CHGLO)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/05/2008   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE
      CHARACTER*1   BASE
      CHARACTER*19  CHGLO
      CHARACTER*11  CHFIS
      CHARACTER*8   MOX

C----------------------------------------------------------------------
C  BUT: CONCATENER LES CHAMPS NODAUX DES SD FISS_XFEM
C       DANS UN CHAMP GLOBAL AFFECTE AU MODELE
C
C----------------------------------------------------------------------
C
C     ARGUMENTS/
C  MOX     IN    K19 : MODELE XFEM
C  CHFIS   IN    K19 : SUFFIXE DU NOM DU CHAMP NODAL A CONCATENER
C  CHGLO   OUT   K19 : CHAMP GLOBAL RESULTANT
C  BASE    IN    K1  : BASE DE CREATION POUR CHGLO : G/V/L
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER NFIS,IFIS,JJ,INO,II
      INTEGER IMA,ICMP,NBNOM,JMACNX,JLCNX
      INTEGER IBID,JINDIC,JG,NMAENR,I
      INTEGER JNFIS,JCNSFD,JCNSFC,JCNSFV,JCNSFK,JCNSFL
      INTEGER NCMP1,JMOFIS,JCNSC,JCNSV,JCNSL
      CHARACTER*3  TSCA
      CHARACTER*19  CNS, CNSF
      CHARACTER*24  INDIC,GRP(3)
      CHARACTER*32  JEXATR
      LOGICAL       COMPCH
      CHARACTER*8   MA,NOMGD,NOMFIS
C     ------------------------------------------------------------------

      CALL JEMARQ()
      CNS  = '&&XCONNO.CNS'
      CNSF = '&&XCONNO.CNSF'

C     1.RECUPERATION D'INFORMATIONS DANS MOX

      CALL JEVEUO(MOX//'.NFIS','L',JNFIS)
      NFIS = ZI(JNFIS)

      CALL JEVEUO(MOX//'.FISS','L',JMOFIS)
      NOMFIS = ZK8(JMOFIS)

      CALL CNOCNS(NOMFIS//CHFIS,'V',CNSF)

      CALL JEVEUO(CNSF//'.CNSK','L',JCNSFK)
      CALL JEVEUO(CNSF//'.CNSD','L',JCNSFD)
      CALL JEVEUO(CNSF//'.CNSC','L',JCNSFC)
      CALL JEVEUO(CNSF//'.CNSV','L',JCNSFV)
      CALL JEVEUO(CNSF//'.CNSL','L',JCNSFL)

      MA = ZK8(JCNSFK-1+1)
      NOMGD = ZK8(JCNSFK-1+2)
      NBNOM = ZI(JCNSFD-1+1)
      NCMP1 = ZI(JCNSFD-1+2)

      CALL JEVEUO(MA//'.CONNEX','L',JMACNX)
      CALL JEVEUO(JEXATR(MA//'.CONNEX','LONCUM'),'L',JLCNX)

      CALL DISMOI('F','TYPE_SCA',NOMGD,'GRANDEUR',IBID,TSCA,IBID)

      CALL CNSCRE(MA,NOMGD,NCMP1,ZK8(JCNSFC),'V',CNS)

      CALL JEVEUO(CNS//'.CNSC','L',JCNSC)
      CALL JEVEUO(CNS//'.CNSV','E',JCNSV)
      CALL JEVEUO(CNS//'.CNSL','E',JCNSL)

      DO 20 IFIS = 1,NFIS

        CALL JEVEUO(MOX//'.FISS','L',JMOFIS)
        NOMFIS = ZK8(JMOFIS-1 + IFIS)
        CALL CNOCNS(NOMFIS//CHFIS,'V',CNSF)

        GRP(1)=NOMFIS//'.MAILFISS  .HEAV'
        GRP(2)=NOMFIS//'.MAILFISS  .CTIP'
        GRP(3)=NOMFIS//'.MAILFISS  .HECT'

        CALL JEVEUO(CNSF//'.CNSV','L',JCNSFV)
        CALL JEVEUO(CNSF//'.CNSL','L',JCNSFL)

        INDIC=NOMFIS//'.MAILFISS .INDIC'
        CALL JEVEUO(INDIC,'L',JINDIC)

        DO 1000, II = 1,3
C--COPIER LE CHAMP 'CHFIS' POUR LES MAILLES '.HEAV','.CTIP' ET '.HECT'
          IF (ZI(JINDIC-1+2*(II-1)+1).EQ.1) THEN
            CALL JEVEUO(GRP(II),'L',JG)
            NMAENR=ZI(JINDIC-1+2*II)

            DO 120 I=1,NMAENR
              IMA   = ZI(JG-1+I)
              NBNOM = ZI(JLCNX+IMA)-ZI(JLCNX-1+IMA)
              DO 1210, JJ = 1,NBNOM
                INO = ZI(JMACNX + ZI(JLCNX-1+IMA)-2+JJ)
                DO 1220, ICMP = 1,NCMP1

C                 POUR CHAQUE TYPE 'R', I', 'L', 'K8', SI LE CHAM_NO
C                 A DEJE ETE REMPLI, ON VERIFIE QUE C'EST AVEC LA MEME
C                 VALEUR ET ON S'ARRETE AU CAS OU, SINON, ON LE REMPLIT

                  IF (TSCA.EQ.'R') THEN
                    IF(ZL(JCNSL-1+(INO-1)*NCMP1+ICMP)) THEN 
                      COMPCH = ZR(JCNSV -1 + (INO-1)*NCMP1+ICMP) .EQ.
     &                         ZR(JCNSFV-1 + (INO-1)*NCMP1+ICMP)
                      IF(.NOT. COMPCH ) CALL U2MESS('F','XFEM_1')
                    ELSE
                      ZR(JCNSV-1+ (INO-1)*NCMP1+ICMP) = ZR(JCNSFV-1+
     &                  (INO-1)*NCMP1+ICMP)
                      ZL(JCNSL-1 + (INO-1)*NCMP1+ICMP) = .TRUE.
                    ENDIF
                  ELSE IF (TSCA.EQ.'I') THEN
                    IF(ZL(JCNSL-1+(INO-1)*NCMP1+ICMP)) THEN 
                      COMPCH = ZI(JCNSV -1 + (INO-1)*NCMP1+ICMP) .EQ.
     &                         ZI(JCNSFV-1 + (INO-1)*NCMP1+ICMP)
                      IF(.NOT. COMPCH ) CALL U2MESS('F','XFEM_1')
                    ELSE
                      ZI(JCNSV-1+ (INO-1)*NCMP1+ICMP) = ZI(JCNSFV-1+
     &                  (INO-1)*NCMP1+ICMP)
                      ZL(JCNSL-1 + (INO-1)*NCMP1+ICMP) = .TRUE.
                    ENDIF
                  ELSE IF (TSCA.EQ.'L') THEN
                    IF(ZL(JCNSL-1+(INO-1)*NCMP1+ICMP)) THEN 
                      COMPCH = ZL(JCNSV -1 + (INO-1)*NCMP1+ICMP) .EQV.
     &                         ZL(JCNSFV-1 + (INO-1)*NCMP1+ICMP)
                      IF(.NOT. COMPCH ) CALL U2MESS('F','XFEM_1')
                    ELSE
                      ZL(JCNSV-1+ (INO-1)*NCMP1+ICMP) = ZL(JCNSFV-1+
     &                  (INO-1)*NCMP1+ICMP)
                      ZL(JCNSL-1 + (INO-1)*NCMP1+ICMP) = .TRUE.
                    ENDIF
                  ELSE IF (TSCA.EQ.'K8') THEN
                    IF(ZL(JCNSL-1+(INO-1)*NCMP1+ICMP)) THEN 
                      COMPCH = ZK8(JCNSV -1 + (INO-1)*NCMP1+ICMP) .EQ.
     &                         ZK8(JCNSFV-1 + (INO-1)*NCMP1+ICMP)
                      IF(.NOT. COMPCH ) CALL U2MESS('F','XFEM_1')
                    ELSE
                      ZK8(JCNSV-1+ (INO-1)*NCMP1+ICMP) = ZK8(JCNSFV-1+
     &                  (INO-1)*NCMP1+ICMP)
                      ZL(JCNSL-1 + (INO-1)*NCMP1+ICMP) = .TRUE.
                    ENDIF
                  ELSE
                    CALL ASSERT(.FALSE.)
                  END IF

 1220           CONTINUE
 1210         CONTINUE

 120        CONTINUE
          ENDIF
 1000   CONTINUE

        CALL DETRSD('CHAM_NO_S',CNSF)
 20   CONTINUE

      CALL CNSCNO(CNS,' ','OUI',BASE,CHGLO,'F',IBID)
      CALL DETRSD('CHAM_NO_S',CNS)
      CALL JEDEMA()
      END
