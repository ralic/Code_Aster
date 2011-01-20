      SUBROUTINE XMELE2(NOMA  ,MODELE,DEFICO,LIGREL,NFISS ,
     &                  CHELEM)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/01/2011   AUTEUR MASSIN P.MASSIN 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT      NONE
      CHARACTER*8   NOMA
      CHARACTER*8   MODELE
      INTEGER       NFISS
      CHARACTER*19  CHELEM
      CHARACTER*19  LIGREL
      CHARACTER*24  DEFICO
C
C ----------------------------------------------------------------------
C
C ROUTINE XFEM (METHODE XFEM - CREATION CHAM_ELEM)
C
C CREATION DU CHAM_ELEM PDONCO (DONNEES DU CONTACT)
C
C ----------------------------------------------------------------------
C
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  MODELE : NOM DU MODELE
C IN  NFISS  : NOMBRE TOTAL DE FISSURES
C IN  LIGREL : NOM DU LIGREL DES MAILLES TARDIVES
C IN  CHELEM : NOM DU CHAM_ELEM CREEE
C IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C

      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      CHARACTER*32 JEXNUM
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER       NBCMP
      PARAMETER     (NBCMP = 11)
      CHARACTER*8   LICMP(NBCMP)
C
      INTEGER       IFM,NIV
      INTEGER       IBID,IAD,IGRP,I,IMA,IFIS,IZONE,XXCONI
      INTEGER       NMAENR,MMINFI
      CHARACTER*8   NOMFIS
      INTEGER       JCESL,JCESV,JCESD,JMOFIS
      CHARACTER*24  GRP(3),XINDIC
      INTEGER       JGRP,JINDIC
      CHARACTER*19  CHELSI
      REAL*8        MMINFR,COEFCR,COEFFF,COEFFR,INTEGR,COECHE
      REAL*8        COCAUS,COFAUS,COCAUP,COFAUP,RELATI,CZMFER
      CHARACTER*19  VALK(2)
      INTEGER       VALI(1)
      INTEGER       JMAIL,ITYELE
      CHARACTER*16  TYPELE
C
      DATA LICMP    /'RHON','MU','RHOTK','INTEG','COECH',
     &    'COSTCO','COSTFR','COPECO','COPEFR','RELA','CZMFE'/
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('XFEM',IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<XFEM  > CREATION DU CHAM_ELEM PDONCO'
      ENDIF
C
C --- INITIALISATIONS
C
      CHELSI = '&&XMELE2.CES'
C
C --- CREATION DU CHAM_ELEM_S
C
      CALL CESCRE('V',CHELSI,'ELEM',NOMA,'XCONTAC',NBCMP,LICMP,
     &            -1,-1,-NBCMP)
C
C --- ACCES AU CHAM_ELEM_S
C
      CALL JEVEUO(CHELSI//'.CESD','L',JCESD)
      CALL JEVEUO(CHELSI//'.CESL','E',JCESL)
      CALL JEVEUO(CHELSI//'.CESV','E',JCESV)
C
C --- ACCES AUX FISSURES
C
      CALL JEVEUO(MODELE//'.FISS','L',JMOFIS)

C --- RECUPERATION DES MAILLES DU MODELE
      CALL JEVEUO(MODELE//'.MAILLE','L',JMAIL)
C
C --- ENRICHISSEMENT DU CHAM_ELEM POUR LA MULTIFISSURATION
C
      DO 110 IFIS = 1,NFISS
C
C --- ACCES FISSURE COURANTE
C
        NOMFIS = ZK8(JMOFIS-1 + IFIS)
C
C --- INFORMATIONS SUR LA FISSURE
C
        GRP(1) = NOMFIS(1:8)//'.MAILFISS  .HEAV'
        GRP(2) = NOMFIS(1:8)//'.MAILFISS  .CTIP'
        GRP(3) = NOMFIS(1:8)//'.MAILFISS  .HECT'
C
C --- ZONE DE CONTACT IZONE CORRESPONDANTE
C
        IZONE  = XXCONI(DEFICO,NOMFIS,'MAIT')
C
C --- CARACTERISTIQUES DU CONTACT POUR LA FISSURE EN COURS
C
        COEFCR  = MMINFR(DEFICO,'COEF_REGU_CONT',IZONE )
        COEFFR  = MMINFR(DEFICO,'COEF_REGU_FROT',IZONE ) 
        COEFFF  = MMINFR(DEFICO,'COEF_COULOMB'  ,IZONE )       
        COECHE  = MMINFR(DEFICO,'COEF_ECHELLE'  ,IZONE )
        INTEGR  = MMINFI(DEFICO,'INTEGRATION'   ,IZONE )
        COCAUS  = MMINFR(DEFICO,'COEF_STAB_CONT',IZONE )
        COFAUS  = MMINFR(DEFICO,'COEF_STAB_FROT',IZONE ) 
        COCAUP  = MMINFR(DEFICO,'COEF_PENA_CONT',IZONE )
        COFAUP  = MMINFR(DEFICO,'COEF_PENA_FROT',IZONE )
        RELATI  = MMINFR(DEFICO,'RELATION'      ,IZONE )
        CZMFER  = MMINFR(DEFICO,'CZM_FERMETURE' ,IZONE )        
C
C --- ACCES AU CHAMP INDICATEUR
C
        XINDIC = NOMFIS(1:8)//'.MAILFISS .INDIC'
        CALL JEVEUO(XINDIC,'L',JINDIC)
C
        DO 1000 IGRP = 1,3
C
C --- ON COPIE LES CHAMPS CORRESP. AUX ELEM. HEAV, CTIP ET HECT
C
          IF (ZI(JINDIC-1+2*(IGRP-1)+1).EQ.1) THEN
            CALL JEVEUO(GRP(IGRP),'L',JGRP)
            NMAENR = ZI(JINDIC-1+2*IGRP)
            DO 120 I = 1,NMAENR
              IMA = ZI(JGRP-1+I)
C
C --- ON NE TRAITE QUE LES MAILLES DE CONTACT
C
               ITYELE=ZI(JMAIL-1+IMA)
               CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYELE),TYPELE)
               IF (IGRP.EQ.1) THEN
                 IF (TYPELE(9:11).NE.'XHC'.AND.
     &                TYPELE(6:8).NE.'XHC') GOTO 120
               ELSEIF (IGRP.EQ.2) THEN
                 IF (TYPELE(9:11).NE.'XTC'.AND.
     &                TYPELE(6:8).NE.'XTC') GOTO 120
               ELSEIF (IGRP.EQ.3) THEN
                 IF(TYPELE(9:12).NE.'XHTC'.AND.
     &                TYPELE(6:9).NE.'XHTC')GOTO 120
               ELSE
                 CALL ASSERT(.FALSE.)
               ENDIF

C
              CALL CESEXI('C',JCESD,JCESL,IMA,1,1,1,IAD)
              IF (IAD.GE.0) THEN
                VALI(1) = 1
                VALK(1) = CHELSI(1:19)
                VALK(2) = 'ELEM'
                CALL U2MESG('F','CATAELEM_20',2,VALK,1,VALI,0,0.D0)
              ENDIF
              ZL(JCESL-1-IAD) = .TRUE.
              ZR(JCESV-1-IAD) = COEFCR
C
              CALL CESEXI('C',JCESD,JCESL,IMA,1,1,2,IAD)
              IF (IAD.GE.0) THEN
                VALI(1) = 2
                VALK(1) = CHELSI(1:19)
                VALK(2) = 'ELEM'
                CALL U2MESG('F','CATAELEM_20',2,VALK,1,VALI,0,0.D0)
              ENDIF
              ZL(JCESL-1-IAD) = .TRUE.
              ZR(JCESV-1-IAD) = COEFFF
C
              CALL CESEXI('C',JCESD,JCESL,IMA,1,1,3,IAD)
              IF (IAD.GE.0) THEN
                VALI(1) = 3
                VALK(1) = CHELSI(1:19)
                VALK(2) = 'ELEM'
                CALL U2MESG('F','CATAELEM_20',2,VALK,1,VALI,0,0.D0)
              ENDIF
              ZL(JCESL-1-IAD) = .TRUE.
              ZR(JCESV-1-IAD) = COEFFR
C
              CALL CESEXI('C',JCESD,JCESL,IMA,1,1,4,IAD)
              IF (IAD.GE.0) THEN
                VALI(1) = 4
                VALK(1) = CHELSI(1:19)
                VALK(2) = 'ELEM'
                CALL U2MESG('F','CATAELEM_20',2,VALK,1,VALI,0,0.D0)
              ENDIF
              ZL(JCESL-1-IAD) = .TRUE.
              ZR(JCESV-1-IAD) = INTEGR
C
              CALL CESEXI('C',JCESD,JCESL,IMA,1,1,5,IAD)
              IF (IAD.GE.0) THEN
                VALI(1) = 5
                VALK(1) = CHELSI(1:19)
                VALK(2) = 'ELEM'
                CALL U2MESG('F','CATAELEM_20',2,VALK,1,VALI,0,0.D0)
              ENDIF
              ZL(JCESL-1-IAD) = .TRUE.
              ZR(JCESV-1-IAD) = COECHE
C              
              CALL CESEXI('C',JCESD,JCESL,IMA,1,1,6,IAD)
              IF (IAD.GE.0) THEN
                VALI(1) = 6
                VALK(1) = CHELSI(1:19)
                VALK(2) = 'ELEM'
                CALL U2MESG('F','CATAELEM_20',2,VALK,1,VALI,0,0.D0)
              ENDIF
              ZL(JCESL-1-IAD) = .TRUE.
              ZR(JCESV-1-IAD) = COCAUS
C
              CALL CESEXI('C',JCESD,JCESL,IMA,1,1,7,IAD)
              IF (IAD.GE.0) THEN
                VALI(1) = 6
                VALK(1) = CHELSI(1:19)
                VALK(2) = 'ELEM'
                CALL U2MESG('F','CATAELEM_20',2,VALK,1,VALI,0,0.D0)
              ENDIF
              ZL(JCESL-1-IAD) = .TRUE.
              ZR(JCESV-1-IAD) = COFAUS
C
              CALL CESEXI('C',JCESD,JCESL,IMA,1,1,8,IAD)
              IF (IAD.GE.0) THEN
                VALI(1) = 6
                VALK(1) = CHELSI(1:19)
                VALK(2) = 'ELEM'
                CALL U2MESG('F','CATAELEM_20',2,VALK,1,VALI,0,0.D0)
              ENDIF
              ZL(JCESL-1-IAD) = .TRUE.
              ZR(JCESV-1-IAD) = COCAUP
C
              CALL CESEXI('C',JCESD,JCESL,IMA,1,1,9,IAD)
              IF (IAD.GE.0) THEN
                VALI(1) = 6
                VALK(1) = CHELSI(1:19)
                VALK(2) = 'ELEM'
                CALL U2MESG('F','CATAELEM_20',2,VALK,1,VALI,0,0.D0)
              ENDIF
              ZL(JCESL-1-IAD) = .TRUE.
              ZR(JCESV-1-IAD) = COFAUP
C
              CALL CESEXI('C',JCESD,JCESL,IMA,1,1,10,IAD)
              IF (IAD.GE.0) THEN
                VALI(1) = 6
                VALK(1) = CHELSI(1:19)
                VALK(2) = 'ELEM'
                CALL U2MESG('F','CATAELEM_20',2,VALK,1,VALI,0,0.D0)
              ENDIF
                ZL(JCESL-1-IAD) = .TRUE.
                ZR(JCESV-1-IAD) = RELATI  
                 
              CALL CESEXI('C',JCESD,JCESL,IMA,1,1,11,IAD)
              IF (IAD.GE.0) THEN
                VALI(1) = 6
                VALK(1) = CHELSI(1:19)
                VALK(2) = 'ELEM'
                CALL U2MESG('F','CATAELEM_20',2,VALK,1,VALI,0,0.D0)
              ENDIF
                ZL(JCESL-1-IAD) = .TRUE.
                ZR(JCESV-1-IAD) = CZMFER
                                                             
 120       CONTINUE
          ENDIF
 1000    CONTINUE

 110  CONTINUE
C
C --- CONVERSION CHAM_ELEM_S -> CHAM_ELEM
C
      CALL CESCEL(CHELSI,LIGREL,'RIGI_CONT','PDONCO','OUI',IBID,'V',
     &            CHELEM,'F',IBID)
C
C --- DESTRUCTION DU CHAM_ELEM_S
C
      CALL DETRSD('CHAM_ELEM_S',CHELSI)
C
      CALL JEDEMA()
C
      END
