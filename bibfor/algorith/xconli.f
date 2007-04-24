      SUBROUTINE XCONLI(NEQ,DEPDEL,DDEPLA,DEPMOI,DEPPLU,PREMIE,MODELE,
     &                  MA,DEFICO,MCONEL,SCONEL)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/04/2007   AUTEUR MARKOVIC D.MARKOVIC 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE
      LOGICAL       PREMIE
      INTEGER       NEQ
      CHARACTER*8   MODELE,MA,MCONEL,SCONEL,MABID
      CHARACTER*24  DEPDEL,DDEPLA,DEPMOI,DEPPLU,DEFICO
C ----------------------------------------------------------------------
C                 PR�PARATION DES DONN�ES UTILES POUR LE
C        CALCUL DES MATRICES ELEMENTAIRES ET DES SECONDS MEMBRES
C             DE TYPE CONTACT FROTTANT DANS LE CADRE D'X-FEM

C IN   NEQ    : NOMBRE D'�QUATIONS
C OUT  DEPDEL : INCR�MENT DES INCONNUES DEPUIS L'�QUILIBRE PR�C�DENT
C IN   DDEPLA : INCR�MENT DES INCONNUES DEPUIS L'IT�R� DE NEWT PR�C�DENT
C IN   DEPMOI : INCONNUES A L'�QUILIBRE PRECEDENT
C IN   PREMIE : SI ON EST � LA PREMI�RE IT�RATION DE NEWTON
C IN   MODELE : NOM DE L'OBJET MOD�LE
C IN   MA     : NOM DE L'OBJET MAILLAGE ISSU DU MOD�LE
C IN   DEFICO : SD CONTACT X-FEM

C OUT  DEPPLU : INCONNUES (DEPLACEMENT + LAGRANGE) COURANT
C OUT  MCONEL : MATR_ELEM : MATRICES ELEMENTAIRES
C OUT  SCONEL : VECT_ELEM : S M ELEMENTAIRES

C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL,EXIGEO
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)


C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

      CHARACTER*1  KBID
      CHARACTER*19 LIGREL,CES,CEL1,CEL2,CEL3,CEL4,CEL5
      CHARACTER*24 INDIC,GRP(3)
      INTEGER      JDEPDE,JDDEPL,JCESD,JCESL,JCESV,JCMCF, JECPD, JNFIS
      INTEGER      NBMA,IBID,IMA,IAD,IRET,ISPT,STACO0,NNCP, NFIS,IFIS
      INTEGER      I,NMAENR,JG,JMOFIS,JINDIC,STAGLI,II,NFISMX
      PARAMETER    (NFISMX=100)      
      REAL*8       DELTAT,SEUIL0
      CHARACTER*8  LICMP(2),LICMP2(5), FISS(NFISMX), NOMFIS
      CALL JEMARQ()

      CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)
      CALL JEVEUO(DDEPLA(1:19)//'.VALE','L',JDDEPL)
      DELTAT = 1.D0

C     MISE A ZERO DE DEPDEL
      CALL ZZZERO (NEQ,JDEPDE)

      CALL COPISD('CHAMP_GD','V',DEPMOI,DEPPLU)

C     MISE A ZERO DE DDEPLA
      CALL ZZZERO (NEQ,JDDEPL)

      IF (PREMIE) THEN

C       CR�ATION DES CHAM_ELEM D'ENTR�ES DES TE SI PREMIE
        LIGREL = MODELE//'.MODELE'
        CEL1=DEFICO(1:16)//'.IN'
        CEL2=DEFICO(1:16)//'.DO'
        CEL3=DEFICO(1:16)//'.SE'
        CEL4=DEFICO(1:16)//'.GL'
        CEL5=DEFICO(1:16)//'.MC'

C     RECUPERATION D'INFORMATIONS SUR LES FISSURE DANS MODELE
        CALL JEVEUO(MODELE//'.NFIS','L',JNFIS)
        NFIS = ZI(JNFIS)  
        IF (NFIS .GT. NFISMX) CALL U2MESI ('F', 'XFEM_2', 1, NFISMX)

C       1) CHAM_ELEM INDCO (STATUT DU CONTACT)
C          � 12*5 SOUS-POINTS INITIALIS� � 0
        LICMP(1) = 'NPG_DYN'
        LICMP(2) = 'NCMP_DYN'
        CALL DISMOI('F','NB_MA_MAILLA',MA,'MAILLAGE',NBMA,KBID,IBID)
        CES = '&&XCONLI.CES'
        CALL CESCRE('V',CES,'ELEM',MA,'DCEL_I',2,LICMP,-1,-1,-2)
        CALL JEVEUO(CES//'.CESD','L',JCESD)
        CALL JEVEUO(CES//'.CESL','E',JCESL)
        CALL JEVEUO(CES//'.CESV','E',JCESV)
        DO 100,IMA = 1,NBMA
          CALL CESEXI('C',JCESD,JCESL,IMA,1,1,1,IAD)
          IF (IAD.GE.0) CALL U2MESS('F','CALCULEL_2')
          ZL(JCESL-1-IAD) = .TRUE.
          ZI(JCESV-1-IAD) = 60

          CALL CESEXI('C',JCESD,JCESL,IMA,1,1,2,IAD)
          IF (IAD.GE.0) CALL U2MESS('F','CALCULEL_8')
          ZL(JCESL-1-IAD) = .TRUE.
          ZI(JCESV-1-IAD) = 1
100     CONTINUE

        CALL ALCHML(LIGREL,'RIGI_CONT','PINDCOI','V',CEL1,IRET,CES)

        IF (IRET.EQ.1) CALL U2MESS('F','ALGORITH11_44')

C       TRANSFORMATION DU CHAMP CEL1 EN CES
        CALL CELCES(CEL1,'V',CES)
        CALL JEVEUO(CES//'.CESD','L',JCESD)
        CALL JEVEUO(CES//'.CESL','E',JCESL)
        CALL JEVEUO(CES//'.CESV','E',JCESV)

C       BOUCLE SUR LES FISSURES ET
C       PUIS INITIALISATION DE TOUS LES SOUS-POINTS � STACO_INIT
C       (SI STACO_INIT EST DIFF�RENT DE 0 BIEN �VIDEMMENT)

        DO 110 IFIS=1, NFIS
          CALL JEVEUO(MODELE//'.FISS','L',JMOFIS)
          NOMFIS = ZK8(JMOFIS-1 + IFIS)
          GRP(1)=NOMFIS//'.MAILFISS  .HEAV'
          GRP(2)=NOMFIS//'.MAILFISS  .CTIP'
          GRP(3)=NOMFIS//'.MAILFISS  .HECT'

C         RECUPERATION DE STACO0 ET SEUIL0 POUR LA FISSURE EN COURS
      
          CALL JEVEUO(NOMFIS//'.CONTACT.ECPDON','L',JECPD)
          STACO0=ZI(JECPD-1+6)
C         ON VERIFIE QU'IL Y A DU CONTACT
          IF (STACO0.NE.0) THEN

            INDIC=NOMFIS//'.MAILFISS .INDIC'
            CALL JEVEUO(INDIC,'L',JINDIC)

            DO 1000 II = 1,3

C        ON COPIE LES CHAMPS CORRESP. AUX ELEM. HEAV, CTIP ET HECT
              IF (ZI(JINDIC-1+2*(II-1)+1).EQ.1) THEN
                CALL JEVEUO(GRP(II),'L',JG)
                NMAENR=ZI(JINDIC-1+2*II)
                DO 120 I=1,NMAENR
                  IMA=ZI(JG-1+I)
                  DO 121 ISPT =1,60
                    CALL CESEXI('C',JCESD,JCESL,IMA,1,ISPT,1,IAD)
                    IF (IAD.EQ.0) GOTO 121
                    ZI(JCESV-1+IAD) = STACO0
 121              CONTINUE
 120            CONTINUE
              ENDIF
 1000       CONTINUE   

          ENDIF
 110    CONTINUE   
C       FIN BOUCLE SUR LES FISSURES POUR STATUT

        CALL CESCEL(CES,LIGREL,'RIGI_CONT','PINDCOI','OUI',NNCP,
     &                'V',CEL1)
C      ON DUPLIQUE LE CHAMP INDCO POUR CREER LE CHAMP DE MEMOIRE 
C      DE CONTACT (CEL5). IL SERA INITIALIS� DONC AVEC LES STATUTS
C      CONTACT_INI
       CALL CESCEL(CES,LIGREL,'XCVBCA','PMEMCON','OUI',NNCP,
     &                'V',CEL5) 
        CALL DETRSD('CHAM_ELEM_S',CES)

C       2) CHAM_ELEM DO (DONN�ES DU CONTACT)
        LICMP2(1)='RHON'
        LICMP2(2)='MU'
        LICMP2(3)='RHOTK'
        LICMP2(4)='INTEG'
        LICMP2(5)='COECH'
        CES = '&&XCONLI.CES'
        CALL CESCRE('V',CES,'ELEM',MA,'XCONTAC',5,LICMP2,-1,-1,-5)
        CALL JEVEUO(CES//'.CESD','L',JCESD)
        CALL JEVEUO(CES//'.CESL','E',JCESL)
        CALL JEVEUO(CES//'.CESV','E',JCESV)

C       BOUCLE SUR LES FISSURES POUR REMPLIR LE CHAMP .DO
        DO 200 IFIS = 1,NFIS
          CALL JEVEUO(MODELE//'.FISS','L',JMOFIS)
          NOMFIS = ZK8(JMOFIS-1 + IFIS)    
          GRP(1)=NOMFIS//'.MAILFISS  .HEAV'
          GRP(2)=NOMFIS//'.MAILFISS  .CTIP'
          GRP(3)=NOMFIS//'.MAILFISS  .HECT'

          CALL JEVEUO(NOMFIS//'.CONTACT.CARACF','L',JCMCF)
          INDIC=NOMFIS//'.MAILFISS .INDIC'
          CALL JEVEUO(INDIC,'L',JINDIC)
        
          DO 1100 II = 1,3
C         ON COPIE LES CHAMPS CORRESP. AUX ELEM. HEAV, CTIP ET HECT
            IF (ZI(JINDIC-1+2*(II-1)+1).EQ.1) THEN
              CALL JEVEUO(GRP(II),'L',JG)
              NMAENR=ZI(JINDIC-1+2*II)
C
              DO 210 I=1,NMAENR
                IMA=ZI(JG-1+I)
                CALL CESEXI('C',JCESD,JCESL,IMA,1,1,1,IAD)
                IF (IAD.GE.0) CALL U2MESS('F','ALGORITH_19')
                ZL(JCESL-1-IAD) = .TRUE.
                ZR(JCESV-1-IAD) = ZR(JCMCF-1+3)

                CALL CESEXI('C',JCESD,JCESL,IMA,1,1,2,IAD)
                IF (IAD.GE.0) CALL U2MESS('F','CALCULEL_9')
                ZL(JCESL-1-IAD) = .TRUE.
                ZR(JCESV-1-IAD) = ZR(JCMCF-1+5)

                CALL CESEXI('C',JCESD,JCESL,IMA,1,1,3,IAD)
                IF (IAD.GE.0) CALL U2MESS('F','CALCULEL_10')
                ZL(JCESL-1-IAD) = .TRUE.
                ZR(JCESV-1-IAD) = ZR(JCMCF-1+4)/DELTAT

                CALL CESEXI('C',JCESD,JCESL,IMA,1,1,4,IAD)
                IF (IAD.GE.0) CALL U2MESS('F','CATAELEM_11')
                ZL(JCESL-1-IAD) = .TRUE.
                ZR(JCESV-1-IAD) = ZR(JCMCF-1+2)

                CALL CESEXI('C',JCESD,JCESL,IMA,1,1,5,IAD)
                IF (IAD.GE.0) CALL U2MESS('F','CATAELEM_12')
                ZL(JCESL-1-IAD) = .TRUE.
                ZR(JCESV-1-IAD) = ZR(JCMCF-1+8)
 210         CONTINUE
            ENDIF
 1100    CONTINUE    

 200    CONTINUE
C       FIN BOUCLE SUR LES FISSURES POUR DONN�ES

        CALL CESCEL(CES,LIGREL,'RIGI_CONT','PDONCO','OUI',NNCP,
     &              'V',CEL2)
        CALL DETRSD('CHAM_ELEM_S',CES)

C       3) CHAM_ELEM PSEUIL (SEUIL DE FROTTEMENT DE TRESCA)
C          � 12*5 SOUS-POINTS INITIALIS� AUTOMATIQUEMENT � 0.D0
        LICMP(1) = 'NPG_DYN'
        LICMP(2) = 'NCMP_DYN'
        CALL DISMOI('F','NB_MA_MAILLA',MA,'MAILLAGE',NBMA,KBID,IBID)
        CES = '&&XCONLI.CES'
        CALL CESCRE('V',CES,'ELEM',MA,'DCEL_I',2,LICMP,-1,-1,-2)
        CALL JEVEUO(CES//'.CESD','L',JCESD)
        CALL JEVEUO(CES//'.CESL','E',JCESL)
        CALL JEVEUO(CES//'.CESV','E',JCESV)

        DO 300,IMA = 1,NBMA
          CALL CESEXI('C',JCESD,JCESL,IMA,1,1,1,IAD)
          IF (IAD.GE.0) CALL U2MESS('F','CATAELEM_11')
          ZL(JCESL-1-IAD) = .TRUE.
          ZI(JCESV-1-IAD) = 60
          CALL CESEXI('C',JCESD,JCESL,IMA,1,1,2,IAD)
          IF (IAD.GE.0) CALL U2MESS('F','CATAELEM_12')
          ZL(JCESL-1-IAD) = .TRUE.
          ZI(JCESV-1-IAD) = 1
300     CONTINUE

        CALL ALCHML(LIGREL,'RIGI_CONT','PSEUIL','V',CEL3,IRET,CES)

        CALL CELCES(CEL3,'V',CES)
        CALL JEVEUO(CES//'.CESD','L',JCESD)
        CALL JEVEUO(CES//'.CESL','E',JCESL)
        CALL JEVEUO(CES//'.CESV','E',JCESV)

C       BOUCLE SUR LES FISSURES POUR LE SEUIL TRESCA
C       PUIS INITIALISATION DE TOUS LES SOUS-POINTS � SEUIL_INIT
C       (SI SEUIL_INIT EST DIFF�RENT DE 0 BIEN �VIDEMMENT)
        DO 400 IFIS = 1,NFIS
          CALL JEVEUO(MODELE//'.FISS','L',JMOFIS) 
          NOMFIS = ZK8(JMOFIS-1 + IFIS)    
          GRP(1)=NOMFIS//'.MAILFISS  .HEAV'
          GRP(2)=NOMFIS//'.MAILFISS  .CTIP'
          GRP(3)=NOMFIS//'.MAILFISS  .HECT'
          
          CALL JEVEUO(NOMFIS//'.CONTACT.CARACF','L',JCMCF)
          SEUIL0=ZR(JCMCF-1+7)

          IF (SEUIL0.NE.0) THEN
            INDIC=NOMFIS//'.MAILFISS .INDIC'
            CALL JEVEUO(INDIC,'L',JINDIC)

            DO 1200 II = 1,3
C         ON COPIE LES CHAMPS CORRESP. AUX ELEM. HEAV, CTIP ET HECT
              IF (ZI(JINDIC-1+2*(II-1)+1).EQ.1) THEN
                CALL JEVEUO(GRP(II),'L',JG)
                NMAENR=ZI(JINDIC-1+2*II)
                DO 410 I=1,NMAENR
                  IMA=ZI(JG-1+I)
                  DO 411 ISPT =1,60
                    CALL CESEXI('C',JCESD,JCESL,IMA,1,ISPT,1,IAD)
                    IF (IAD.EQ.0) GOTO 410
                    ZR(JCESV-1+IAD) = - ABS(SEUIL0)
 411              CONTINUE
 410            CONTINUE 
              ENDIF
 1200       CONTINUE    
          ENDIF
 400    CONTINUE
        CALL CESCEL(CES,LIGREL,'RIGI_CONT','PSEUIL','OUI',NNCP,
     &                 'V',CEL3)
      
      CALL DETRSD('CHAM_ELEM_S',CES)
      
C       4) CHAM_ELEM IGLISS (STATUT DU CONTACT GLISSIER)
C          � 12*5 SOUS-POINTS INITIALIS� � 0
        LICMP(1) = 'NPG_DYN'
        LICMP(2) = 'NCMP_DYN'
        CALL DISMOI('F','NB_MA_MAILLA',MA,'MAILLAGE',NBMA,KBID,IBID)
        CES = '&&XCONLI.CES'
        CALL CESCRE('V',CES,'ELEM',MA,'DCEL_I',2,LICMP,-1,-1,-2)
        CALL JEVEUO(CES//'.CESD','L',JCESD)
        CALL JEVEUO(CES//'.CESL','E',JCESL)
        CALL JEVEUO(CES//'.CESV','E',JCESV)

        DO 500,IMA = 1,NBMA
          CALL CESEXI('C',JCESD,JCESL,IMA,1,1,1,IAD)
          IF (IAD.GE.0) CALL U2MESS('F','CALCULEL_2')
          ZL(JCESL-1-IAD) = .TRUE.
          ZI(JCESV-1-IAD) = 60
          CALL CESEXI('C',JCESD,JCESL,IMA,1,1,2,IAD)
          IF (IAD.GE.0) CALL U2MESS('F','CALCULEL_8')
          ZL(JCESL-1-IAD) = .TRUE.
          ZI(JCESV-1-IAD) = 1
500     CONTINUE

        CALL ALCHML(LIGREL,'XCVBCA','PGLISS','V',CEL4,IRET,CES)
        IF (IRET.EQ.1) CALL U2MESS('F','ALGORITH11_44')

C       TRANSFORMATION DU CHAMP CEL1 EN CES
        CALL CELCES(CEL4,'V',CES)
        CALL JEVEUO(CES//'.CESD','L',JCESD)
        CALL JEVEUO(CES//'.CESL','E',JCESL)
        CALL JEVEUO(CES//'.CESV','E',JCESV)

C       BOUCLE SUR LES FISSURES ET
C       PUIS INITIALISATION DE TOUS LES SOUS-POINTS � STAGLI
C       (SI STAGLI EST DIFF�RENT DE 0 BIEN �VIDEMMENT)
        DO 600 IFIS=1, NFIS
          CALL JEVEUO(MODELE//'.FISS','L',JMOFIS) 
          NOMFIS = ZK8(JMOFIS-1 + IFIS)
          GRP(1)=NOMFIS//'.MAILFISS  .HEAV'
          GRP(2)=NOMFIS//'.MAILFISS  .CTIP'
          GRP(3)=NOMFIS//'.MAILFISS  .HECT'

C         RECUPERATION DE STAGLI POUR LA FISSURE EN COURS  
          CALL JEVEUO(NOMFIS//'.CONTACT.ECPDON','L',JECPD)
          STAGLI=ZI(JECPD-1+7)
C         ON VERIFIE QU'IL Y A DU CONTACT GLISSIER
          IF (STAGLI.NE.0) THEN
            INDIC=NOMFIS//'.MAILFISS .INDIC'
            CALL JEVEUO(INDIC,'L',JINDIC)

            DO 1300 II=1,3
C          ON COPIE LES CHAMPS CORRESP. AUX ELEM. HEAV, CTIP ET HECT
              IF (ZI(JINDIC-1+2*(II-1)+1).EQ.1) THEN
                CALL JEVEUO(GRP(II),'L',JG)
                NMAENR=ZI(JINDIC-1+2*II)
                DO 510 I=1,NMAENR
                  IMA=ZI(JG-1+I)
                  DO 511 ISPT =1,60
                    CALL CESEXI('C',JCESD,JCESL,IMA,1,ISPT,1,IAD)
                    IF (IAD.EQ.0) GOTO 510
                    ZI(JCESV-1+IAD) = STAGLI
 511              CONTINUE
 510            CONTINUE
              ENDIF
 1300       CONTINUE
          ENDIF
 600    CONTINUE
C       FIN BOUCLE SUR LES FISSURES POUR STATUT

        CALL CESCEL(CES,LIGREL,'XCVBCA','PGLISS','OUI',NNCP,
     &                'V',CEL4)
        CALL DETRSD('CHAM_ELEM_S',CES)
      ENDIF

      CALL XMMCME(MODELE,DEPMOI,DEPDEL,DEFICO,MCONEL,SCONEL)

      CALL JEDEMA()
      END
