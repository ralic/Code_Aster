      SUBROUTINE IRDEEC(IFI1,IFI2,IFI3,NBNO,PRNO,NUEQ,NEC,DG,
     &              NCMPMX,NBCMPT,NUCMPU,
     &              VALE,NOMGD,NCMPGD,NOMSDR,NOMSYM,
     &              NUMORD,NUORD1,NORDEN,IORDEN,NBCHAM,ICHAM,
     &              LCHAM1,LRESU)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER       IFI1,IFI2,IFI3,NBNO,PRNO(*),NUEQ(*),NEC,DG(*)
      INTEGER       NCMPMX,       NBCMPT,NUCMPU(*)
      COMPLEX*16    VALE(*)
      CHARACTER*(*)      NOMGD,NCMPGD(*),NOMSDR,NOMSYM
      INTEGER       NUMORD,NUORD1,NORDEN,IORDEN,NBCHAM,ICHAM
      LOGICAL       LCHAM1,LRESU
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE CRP_20 CRP_21
C--------------------------------------------------------------------
C     ECRITURE AU FORMAT ENSIGHT D'UN CHAM_NO A
C     VALEURS COMPLEXES ET PROFIL AU NOEUDS VARIABLE
C   ENTREE:
C     IFI1  : UNITE LOGIQUE DU FICHIER "RESULTS" ENSIGHT
C     IFI2  : UNITE LOGIQUE POUR LES FICHIERS ENSIGHT DE
C             PARTIES REELLES DES VALEURS
C     IFI3  : UNITE LOGIQUE POUR LES FICHIERS ENSIGHT
C             PARTIES IMAGINAIRES DES VALEURS
C     NBNO  : NOMBRE DE NOEUDS DU LIGREL ( DU MAILLAGE)
C     PRNO  : OBJET .PRNO DU PROF_CHNO
C     NUEQ  : OBJET .NUEQ DU PROF_CHNO
C     NEC   : NOMBRE D'ENTIERS-CODES
C     DG    : ENTIERS CODES
C     NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR NOMGD
C     NBCMPT: NOMBRE DE CMP A IMPRIMER (NBCMPT=0 : PAS DE SELECTION)
C     NUCMPU: NUMEROS DES CMP A IMPRIMER SI NBCMPT > 0
C     VALE  : VALEURS DU CHAM_NO
C     NOMGD : NOM DE LA GRANDEUR  DEPL_R, TEMP_R, SIEF_R, EPSI_R,...
C     NCMPGD: NOMS DES CMP DE LA GRANDEUR NOMGD
C     NOMSDR: NOM DE LA SD_RESULTAT DONT EST ISSU LE CHAM_NO
C     NOMSYM: NOM SYMBOLIQUE DU CHAM_NO
C     NUMORD: NUMERO D'ORDRE DU CHAMP
C     NUORD1: NUMERO DU PREMIER DES NUMEROS D'ORDRE A IMPRIMER
C     NORDEN: NOMBRE DE NUMEROS D'ORDRE A IMPRIMER
C     IORDEN: INDICE DE NUMORD DANS LA LISTE DES NUMEROS D'ORDRE
C             A IMPRIMER
C     NBCHAM: NOMBRE DE CHAMPS A IMPRIMER
C     ICHAM : INDICE DU CHAMP DANS LA LISTE DES CHAMPS A IMPRIMER
C     LCHAM1: INDIQUE SI LE CHAMP EST LE PREMIER DES CHAMPS
C             A IMPRIMER POUR LE NUMERO D'ORDRE NUMORD
C     LRESU : =.TRUE. INDIQUE IMPRESSION D'UN CONCEPT RESULTAT
C     ------------------------------------------------------------------
C !! EN CAS DE MODIFICATION DE CE SS-PGME, PENSER A IRDEER,IRDRER,IRDREC
C
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL,EXISDG
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*24 VALK(6)
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C     ------------------------------------------------------------------
      CHARACTER*6  CHNMOD
      CHARACTER*8  NOMSD,NOCMP,NOCMP1,NOCMP2,NOCMP3
      CHARACTER*10 TYPORD
      CHARACTER*16 NOSY16,VKORD
      CHARACTER*19 NOMVAR
      CHARACTER*60 NOFIVA,FIEN
      CHARACTER*80 FICH,FICVAR,DESRFV,DESIFV
      INTEGER      NBVSMX,NBVSCA,NBVVMX,NBVVEC,LGCONC,LGDESC
      INTEGER      LGCH16,LGCOMP,LGCMP1,LGCMP2,LGCMP3
      INTEGER      LGFIVA,LGFICH,LGNUO1,LGNTXT
      INTEGER      ICPRES,NVIDAV,NVIDAP,NBBCL,VIORD
      INTEGER      NBVSCO,NBVVCO,IPVSCA(500),IPVVEC(500,3)
      LOGICAL      IMPSCA(500),IMPVEC(500),EXIFVA
      LOGICAL      LNULSN,LNULSC,LNULVC,LNULV1,LNULV2,LNULV3
      REAL*8       VRORD,VALR8R(6),VALR8I(6)
C
C     --- INITIALISATIONS ----
      CALL JEMARQ()
      NOMSD=NOMSDR
      NOSY16=NOMSYM
      LGCONC=LXLGUT(NOMSD)
      LGCH16=LXLGUT(NOSY16)
      LNULSC=.FALSE.
      LNULSN=.FALSE.
      LNULVC=.FALSE.
      LNULV1=.FALSE.
      LNULV2=.FALSE.
      LNULV3=.FALSE.
      NBVSMX=0
      NBVVMX=0
      NBVSCA=0
      NBVVEC=0
      ICPRES=-1
      NVIDAV=0
      NVIDAP=0
C
C     - PARCOURS DES COMPOSANTES DU CHAM_NO, DEFINITION DES VARIABLES
C       ENSIGHT SCALAIRES ET VECTORIELLES A PARTIR DES COMPOSANTES ET
C       DE LEURS POSITIONS DANS LE CHAM_NO
      CALL IRGAGE(NCMPMX,NCMPGD,NBCMPT,NUCMPU,
     &            NBVSMX,IPVSCA,NBVVMX,IPVVEC)
      IF (NBVSMX.GT.500.OR.NBVVMX.GT.500) CALL U2MESS('F','PREPOST2_19')
      NBVSCA=NBVSMX
      NBVVEC=NBVVMX
C     - ON VERIFIE QUE LES VARIABLES SCALAIRES (COMPOSANTES) SONT
C       PORTEES PAR AU MOINS UN NOEUD. SINON ON NE LES IMPRIMERA PAS.
      DO 70 ISCA=1,NBVSMX
        IMPSCA(ISCA)=.FALSE.
        ICMPAS=IPVSCA(ISCA)
        DO 72 INO=1,NBNO
          DO 73 IEC=1,NEC
            DG(IEC)=PRNO((INO-1)*(NEC+2)+2+IEC)
  73      CONTINUE
C         - NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
          NCMP = PRNO((INO-1)* (NEC+2)+2)
          IF(NCMP.EQ.0) GO TO 72
          IF(EXISDG(DG,ICMPAS)) THEN
            IMPSCA(ISCA)=.TRUE.
            GO TO 70
          ENDIF
  72    CONTINUE
        NBVSCA=NBVSCA-1
        IF(NBCMPT.GT.0) THEN
          NOCMP=NCMPGD(ICMPAS)
          LGCOMP=LXLGUT(NOCMP)
          IF(.NOT.LRESU) THEN
             VALK(1) = NOCMP(1:LGCOMP)
             VALK(2) = NOSY16(1:LGCH16)
             CALL U2MESK('A','PREPOST2_20', 2 ,VALK)
          ELSE
             VALK(1) = NOCMP(1:LGCOMP)
             VALK(2) = NOSY16(1:LGCH16)
             VALK(3) = NOMSD(1:LGCONC)
             CALL U2MESK('A','PREPOST2_21', 3 ,VALK)
          ENDIF
        ENDIF
  70  CONTINUE
C      - ON VERIFIE QU'AU MOINS UNE DES COMPOSANTES DES VECTEURS
C        EST PORTEE PAR AU MOINS UN NOEUD. SINON ON NE DOIT PAS
C        IMPRIMER LA VARIABLE VECTORIELLE ASSOCIEE
      DO 80 IVEC=1,NBVVMX
        IMPVEC(IVEC)=.FALSE.
        ICMPA1=IPVVEC(IVEC,1)
        ICMPA2=IPVVEC(IVEC,2)
        ICMPA3=IPVVEC(IVEC,3)
        DO 82 INO=1,NBNO
          DO 83 IEC=1,NEC
            DG(IEC)=PRNO((INO-1)*(NEC+2)+2+IEC)
  83      CONTINUE
C         - NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
          NCMP = PRNO((INO-1)* (NEC+2)+2)
          IF(NCMP.EQ.0) GO TO 82
          IF(EXISDG(DG,ICMPA1).OR.
     &         EXISDG(DG,ICMPA2).OR.EXISDG(DG,ICMPA3)) THEN
            IMPVEC(IVEC)=.TRUE.
            GO TO 80
          ENDIF
  82    CONTINUE
        NBVVEC=NBVVEC-1
        IF(NBCMPT.GT.0) THEN
          NOCMP1=NCMPGD(ICMPA1)
          NOCMP2=NCMPGD(ICMPA2)
          NOCMP3=NCMPGD(ICMPA3)
          LGCMP1=LXLGUT(NOCMP1)
          LGCMP2=LXLGUT(NOCMP2)
          LGCMP3=LXLGUT(NOCMP3)
          IF(.NOT.LRESU) THEN
              VALK(1) = NOCMP1(1:LGCMP1)
              VALK(2) = NOCMP2(1:LGCMP2)
              VALK(3) = NOCMP3(1:LGCMP3)
              VALK(4) = NOSY16(1:LGCH16)
              CALL U2MESK('A','PREPOST2_22', 4 ,VALK)
          ELSE
              VALK(1) = NOCMP1(1:LGCMP1)
              VALK(2) = NOCMP2(1:LGCMP2)
              VALK(3) = NOCMP3(1:LGCMP3)
              VALK(4) = NOSY16(1:LGCH16)
              VALK(5) = NOMSD(1:LGCONC)
              CALL U2MESK('A','PREPOST2_23', 5 ,VALK)
          ENDIF
        ENDIF
  80  CONTINUE
      IF(LRESU) THEN
C     --- CAS D'UNE SD_RESULTAT: ON RECUPERE LES OBJETS DESCRIPTIFS ---
C       * ICPRES=0 INDIQUE QUE LE CHAMP EST PRESENT POUR LA 1ERE FOIS,
C         AUQUEL CAS IL FAUDRA RAJOUTER DES NOMS GENERIQUES DE FICHIERS
C         ET DE VARIABLES DANS LE FICHIER "RESULTS" ENSIGHT
C       * NVIDAV DONNE LE NOMBRE DE FICHIERS VIDES DE VALEURS A ECRIRE
C         AVANT LES FICHIERS DU CHAMP COURANT
C       * NVIDAP DONNE LE NOMBRE DE FICHIERS VIDES DE VALEURS A ECRIRE
C         APRES LES FICHIERS DU CHAMP COURANT
        CALL JEVEUO('&&IRECRI.CHPRES','L',JCPRES)
        CALL JEVEUO('&&IRECRI.FVIDAV','L',JVIDAV)
        CALL JEVEUO('&&IRECRI.FVIDAP','L',JVIDAP)
        ICPRES=ZI(JCPRES-1+(IORDEN-1)*NBCHAM+ICHAM)
        NVIDAV=ZI(JVIDAV-1+(IORDEN-1)*NBCHAM+ICHAM)
        NVIDAP=ZI(JVIDAP-1+(IORDEN-1)*NBCHAM+ICHAM)
      ENDIF
C
C     - LECTURE ET MODIFICATION DU DEBUT DU FICHIER "RESULTS" ENSIGHT
C       AVANT INSERTION DE NOUVELLES VARIABLES SCALAIRES ET VECTORIELLES
      CALL ECRFRE(IFI1,NOMSDR,NOMSYM,NUMORD,NUORD1,
     &            NORDEN,IORDEN,2*NBVSCA,2*NBVVEC,ICPRES,LCHAM1,LRESU,
     &            NBVSCO,NBVVCO,VIORD,VRORD,VKORD,TYPORD,LGNUO1,
     &            FICH,LGFIVA)
      NOFIVA=FICH(1:LGFIVA)
C
C     - INSERTION DES NOUVELLES VARIABLES SCALAIRES ET DES FICHIERS
C       DE VALEURS ASSOCIES
      IF(NBVSCA.NE.0) THEN
C       - ECRITURE DES NOMS DES NOUVELLES VARIABLES SCALAIRES
C         (FICHIER "RESULTS" ENSIGHT) PUIS DES FICHIERS DE VALEURS
        DO 20 ISCA=1,NBVSMX
          IF(.NOT.IMPSCA(ISCA)) GO TO 20
          ICMPAS=IPVSCA(ISCA)
          NOCMP=NCMPGD(ICMPAS)
          LGCOMP=LXLGUT(NOCMP)
C         - COMPOSANTE AJOUTEE AU NOM GENERIQUE DU FICHIER DE VALEURS
          NOFIVA=NOFIVA(1:LGFIVA)//NOCMP(1:LGCOMP)
C         - ON ECRIT LES FICHIERS VIDES DE VALEURS SI LE CHAMP
C           N'ETAIT PAS PRESENT A DES INDICES PRECEDENTS
          IF(LRESU.AND.(NVIDAV.GT.0)) THEN
            DO 84 IVIDE=IORDEN-NVIDAV,IORDEN-1
C             - ON DEFINIT LE NOM DU FICHIER VIDE DE VALEURS
              CALL DEFIEN(NOFIVA,NOMSD,LGCONC,NOMGD,NOSY16,LGCH16,
     &                    NOCMP,LGCOMP,LGNUO1,NORDEN,IVIDE,LRESU,
     &                    'SCA_VIDE',1,FICH,LGFICH,NOMVAR,LGNOVA,
     &                    LGDESC,DESRFV,DESIFV)
              FIEN=FICH(1:LGFICH)
              FIEN=FIEN(1:LGFICH)//'.R'
              INQUIRE(FILE=FIEN,ERR=200,IOSTAT=IOS1,EXIST=EXIFVA)
 200          CONTINUE
              IF(IOS1.NE.0) THEN
                IF(.NOT.LRESU) GO TO 101
                GOTO 102
              ENDIF
              IF(EXIFVA) GO TO 998
              OPEN(UNIT=IFI2,ERR=85,STATUS='NEW',FILE=FIEN,IOSTAT=IOS2)
  85          CONTINUE
              IF(IOS2.NE.0) THEN
                IF(.NOT.LRESU) GO TO 101
                GOTO 102
              ENDIF
              CLOSE(IFI2)
              FIEN=FIEN(1:LGFICH)//'.I'
              OPEN(UNIT=IFI3,ERR=86,STATUS='NEW',FILE=FIEN,IOSTAT=IOS3)
  86          CONTINUE
              IF(IOS3.NE.0) THEN
                IF(.NOT.LRESU) GO TO 103
                GOTO 104
              ENDIF
              CLOSE(IFI3)
  84        CONTINUE
          ENDIF
C         --- FICHIER DE VALEURS COURANT ---
C         - ON DEFINIT LE NOM DU FICHIER CONTENANT LES VALEURS,
C           LE NOM DE LA VARIABLE ENSIGHT ASSOCIEE AINSI QUE
C           LA DESCRIPTION DU CONTENU DU FICHIER DE VALEURS
          CALL DEFIEN(NOFIVA,NOMSD,LGCONC,NOMGD,NOSY16,LGCH16,
     &                NOCMP,LGCOMP,LGNUO1,NORDEN,IORDEN,LRESU,
     &                'SCALAIRE',1,FICH,LGFICH,NOMVAR,LGNOVA,
     &                LGDESC,DESRFV,DESIFV)
          IF((.NOT.LRESU) .OR. (ICPRES.EQ.0)) THEN
            WRITE(IFI1,'(A,1X,A)') NOFIVA(1:LGFIVA+LGCOMP)//'.R',
     &                             NOMVAR(1:LGNOVA)
            WRITE(IFI1,'(A,1X,A)') NOFIVA(1:LGFIVA+LGCOMP)//'.I',
     &                             NOMVAR(1:LGNOVA-2)//'.I'
          ENDIF
          FIEN=FICH(1:LGFICH)
          FIEN=FIEN(1:LGFICH)//'.R'
          INQUIRE(FILE=FIEN,ERR=201,IOSTAT=IOS1,EXIST=EXIFVA)
 201      CONTINUE
          IF(IOS1.NE.0) THEN
            IF(.NOT.LRESU) GO TO 101
            GOTO 102
          ENDIF
          IF(EXIFVA) GO TO 998
          OPEN(UNIT=IFI2,ERR=21,STATUS='NEW',FILE=FIEN,IOSTAT=IOS2)
  21      CONTINUE
          IF(IOS2.NE.0) THEN
            IF(.NOT.LRESU) GO TO 101
            GOTO 102
          ENDIF
          FIEN=FIEN(1:LGFICH)//'.I'
          OPEN(UNIT=IFI3,ERR=22,STATUS='NEW',FILE=FIEN,IOSTAT=IOS3)
  22      CONTINUE
          IF(IOS3.NE.0) THEN
            IF(.NOT.LRESU) GO TO 103
            GOTO 104
          ENDIF
          IF(TYPORD.EQ.'INST') THEN
            WRITE(IFI2,'(A,A,1PE10.3)') DESRFV(1:LGDESC),' INST=',VRORD
            WRITE(IFI3,'(A,A,1PE10.3)') DESIFV(1:LGDESC),' INST=',VRORD
          ELSEIF(TYPORD.EQ.'FREQ') THEN
            WRITE(IFI2,'(A,A,1PE10.3)') DESRFV(1:LGDESC),' FREQ=',VRORD
            WRITE(IFI3,'(A,A,1PE10.3)') DESIFV(1:LGDESC),' FREQ=',VRORD
          ELSEIF(TYPORD.EQ.'NUME_MODE') THEN
            CALL CODENT(VIORD,'G',CHNMOD)
            LGNTXT=LXLGUT(CHNMOD)
            WRITE(IFI2,'(A,1PE10.3)') DESRFV(1:LGDESC)//
     &           ' MODE='//CHNMOD(1:LGNTXT)//' F=',VRORD
            WRITE(IFI3,'(A,1PE10.3)') DESIFV(1:LGDESC)//
     &           ' MODE='//CHNMOD(1:LGNTXT)//' F=',VRORD
          ELSEIF(TYPORD.EQ.'NUME_CAS') THEN
            LGNTXT=LXLGUT(VKORD)
            WRITE(IFI2,'(A)') DESRFV(1:LGDESC)//
     &           ' CAS='//VKORD(1:LGNTXT)
            WRITE(IFI3,'(A)') DESIFV(1:LGDESC)//
     &           ' CAS='//VKORD(1:LGNTXT)
          ELSE
             WRITE(IFI2,'(A)') DESRFV(1:LGDESC)
             WRITE(IFI3,'(A)') DESIFV(1:LGDESC)
          ENDIF
          INUVAL=0
          DO 24 INO=1,NBNO
            DO 25 IEC=1,NEC
              DG(IEC)=PRNO((INO-1)*(NEC+2)+2+IEC)
  25        CONTINUE
C           - NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
C           - IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
            IVAL = PRNO((INO-1)*(NEC+2)+1)
            NCMP = PRNO((INO-1)*(NEC+2)+2)
            IF(NCMP.EQ.0) THEN
              IF(.NOT.LNULSN) LNULSN=.TRUE.
              IF(.NOT.LNULSC) LNULSC=.TRUE.
            ELSE
              IF(EXISDG(DG,ICMPAS)) THEN
C               - LA COMPOSANTE EST PORTEE PAR LE NOEUD, IL FAUT
C                 CHERCHER SON RANG DANS LE .NUEQ DU PROFIL AUX NOEUDS
                IRGCMP=0
                NBBCL=ICMPAS/30
                IF(NBBCL.GT.0) THEN
                  DO 26 IEC=1,NBBCL
                    DO 27 I=1,30
                      IF(IAND(DG(IEC),2**I).GT.0) IRGCMP=IRGCMP+1
  27                CONTINUE
  26              CONTINUE
                ENDIF
                IF((ICMPAS-30*NBBCL).GT.0) THEN
                  DO 28 I=1,ICMPAS-30*NBBCL
                    IF(IAND(DG(NBBCL+1),2**I).GT.0) IRGCMP=IRGCMP+1
  28              CONTINUE
                ENDIF
              ELSE
                IF(.NOT.LNULSN) LNULSN=.TRUE.
                IF(.NOT.LNULSC) LNULSC=.TRUE.
              ENDIF
            ENDIF
            INUVAL=INUVAL+1
            IF(.NOT.LNULSN) THEN
              VALR8R(INUVAL)=DBLE(VALE(NUEQ(IVAL-1+IRGCMP)))
              VALR8I(INUVAL)=DIMAG(VALE(NUEQ(IVAL-1+IRGCMP)))
            ELSE
              VALR8R(INUVAL)=0.0D0
              VALR8I(INUVAL)=0.0D0
              LNULSN=.FALSE.
            ENDIF
            IF(INUVAL.EQ.6) THEN
              WRITE(IFI2,'(6(1PE12.5))') (VALR8R(I),I=1,6)
              WRITE(IFI3,'(6(1PE12.5))') (VALR8I(I),I=1,6)
              INUVAL=0
            ELSEIF(INO.EQ.NBNO) THEN
              WRITE(IFI2,'(6(1PE12.5))') (VALR8R(I),I=1,INUVAL)
              WRITE(IFI3,'(6(1PE12.5))') (VALR8I(I),I=1,INUVAL)
            ENDIF
  24      CONTINUE
          CLOSE(IFI2)
          CLOSE(IFI3)
          IF(LNULSC) THEN
             VALK(1) = NOCMP(1:LGCOMP)
             VALK(2) = NOSY16(1:LGCH16)
             VALK(3) = NOMVAR(1:LGNOVA)
             VALK(4) = NOCMP(1:LGCOMP)
             VALK(5) = NOFIVA(1:LGFIVA+LGCOMP)
             CALL U2MESK('A','PREPOST2_24', 5 ,VALK)
            LNULSC=.FALSE.
          ENDIF
          IF(LRESU.AND.(NVIDAP.GT.0)) THEN
C           - ON ECRIT LES FICHIERS VIDES DE VALEURS QUI SUIVENT
            DO 88 IVIDE=IORDEN+1,IORDEN+NVIDAP
C             - ON DEFINIT LE NOM DU FICHIER VIDE DE VALEURS
              CALL DEFIEN(NOFIVA,NOMSD,LGCONC,NOMGD,NOSY16,LGCH16,
     &                    NOCMP,LGCOMP,LGNUO1,NORDEN,IVIDE,LRESU,
     &                    'SCA_VIDE',1,FICH,LGFICH,NOMVAR,LGNOVA,
     &                    LGDESC,DESRFV,DESIFV)
              FIEN=FICH(1:LGFICH)
              FIEN=FIEN(1:LGFICH)//'.R'
              INQUIRE(FILE=FIEN,ERR=202,IOSTAT=IOS1,EXIST=EXIFVA)
 202          CONTINUE
              IF(IOS1.NE.0) THEN
                IF(.NOT.LRESU) GO TO 101
                GOTO 102
              ENDIF
              IF(EXIFVA) GO TO 998
              OPEN(UNIT=IFI2,ERR=89,STATUS='NEW',FILE=FIEN,IOSTAT=IOS2)
  89          CONTINUE
              IF(IOS2.NE.0) THEN
                IF(.NOT.LRESU) GO TO 101
                GOTO 102
              ENDIF
              CLOSE(IFI2)
              FIEN=FIEN(1:LGFICH)//'.I'
              OPEN(UNIT=IFI3,ERR=90,STATUS='NEW',FILE=FIEN,IOSTAT=IOS3)
  90          CONTINUE
              IF(IOS3.NE.0) THEN
                IF(.NOT.LRESU) GO TO 103
                GOTO 104
              ENDIF
              CLOSE(IFI3)
  88        CONTINUE
          ENDIF
  20    CONTINUE
      ENDIF

      IF((NBVSCO+NBVVCO).GT.0) THEN
C       - ON RECRIT LES NOMS DES FICHIERS DE VALEURS ET DES VARIABLES
C         QUI ETAIENT DEJA PRESENTS DANS LE FICHIER "RESULTS" ENSIGHT
        CALL JEVEUO('&&ECRFRE.NOMS_FIVA','L',JFIVA)
        DO 29 I=1,NBVSCO+NBVVCO
          FICVAR=ZK80(JFIVA-1+I)
          LGFICH=LXLGUT(FICVAR)
          WRITE(IFI1,'(A)') FICVAR(1:LGFICH)
  29    CONTINUE
      ENDIF

      IF(NBVVEC.NE.0) THEN
C       - CETTE FOIS ON TRAITE LES VARIABLES VECTORIELLES ENSIGHT
        DO 30 IVEC=1,NBVVMX
          IF(.NOT.IMPVEC(IVEC)) GO TO 30
          ICMPA1=IPVVEC(IVEC,1)
          ICMPA2=IPVVEC(IVEC,2)
          ICMPA3=IPVVEC(IVEC,3)
          NOCMP=' '
          LGCOMP=0
          IF(LRESU.AND.(NVIDAV.GT.0)) THEN
            DO 92 IVIDE=IORDEN-NVIDAV,IORDEN-1
              CALL DEFIEN(NOFIVA,NOMSD,LGCONC,NOMGD,NOSY16,LGCH16,
     &                    NOCMP,LGCOMP,LGNUO1,NORDEN,IVIDE,LRESU,
     &                    'VEC_VIDE',1,FICH,LGFICH,NOMVAR,LGNOVA,
     &                    LGDESC,DESRFV,DESIFV)
              FIEN=FICH(1:LGFICH)
              FIEN=FIEN(1:LGFICH)//'.R'
              INQUIRE(FILE=FIEN,ERR=203,IOSTAT=IOS1,EXIST=EXIFVA)
 203          CONTINUE
              IF(IOS1.NE.0) THEN
                IF(.NOT.LRESU) GO TO 101
                GOTO 102
              ENDIF
              IF(EXIFVA) GO TO 998
              OPEN(UNIT=IFI2,ERR=93,STATUS='NEW',FILE=FIEN,IOSTAT=IOS2)
  93          CONTINUE
              IF(IOS2.NE.0) THEN
                IF(.NOT.LRESU) GO TO 101
                GOTO 102
              ENDIF
              CLOSE(IFI2)
              FIEN=FIEN(1:LGFICH)//'.I'
              OPEN(UNIT=IFI3,ERR=94,STATUS='NEW',FILE=FIEN,IOSTAT=IOS3)
  94          CONTINUE
              IF(IOS3.NE.0) THEN
                IF(.NOT.LRESU) GO TO 103
                GOTO 104
              ENDIF
              CLOSE(IFI3)
  92        CONTINUE
          ENDIF
C         --- FICHIER DE VALEURS COURANT ---
          CALL DEFIEN(NOFIVA,NOMSD,LGCONC,NOMGD,NOSY16,LGCH16,
     &                NOCMP,LGCOMP,LGNUO1,NORDEN,IORDEN,LRESU,
     &                'VECTEUR ',1,FICH,LGFICH,NOMVAR,LGNOVA,
     &                LGDESC,DESRFV,DESIFV)
          IF((.NOT.LRESU) .OR. (ICPRES.EQ.0)) THEN
            WRITE(IFI1,'(A,1X,A)') NOFIVA(1:LGFIVA-1)//'.R',
     &                             NOMVAR(1:LGNOVA)
            WRITE(IFI1,'(A,1X,A)') NOFIVA(1:LGFIVA-1)//'.I',
     &                             NOMVAR(1:LGNOVA-2)//'.I'
          ENDIF
          FIEN=FICH(1:LGFICH)
          FIEN=FIEN(1:LGFICH)//'.R'
          INQUIRE(FILE=FIEN,ERR=204,IOSTAT=IOS1,EXIST=EXIFVA)
 204      CONTINUE
          IF(IOS1.NE.0) THEN
            IF(.NOT.LRESU) GO TO 101
            GOTO 102
          ENDIF
          IF(EXIFVA) GO TO 998
          OPEN(UNIT=IFI2,ERR=31,STATUS='NEW',FILE=FIEN,IOSTAT=IOS2)
  31      CONTINUE
          IF(IOS2.NE.0) THEN
            IF(.NOT.LRESU) GO TO 101
            GOTO 102
          ENDIF
          FIEN=FIEN(1:LGFICH)//'.I'
          OPEN(UNIT=IFI3,ERR=32,STATUS='NEW',FILE=FIEN,IOSTAT=IOS3)
  32      CONTINUE
          IF(IOS3.NE.0) THEN
            IF(.NOT.LRESU) GO TO 103
            GOTO 104
          ENDIF
          IF(TYPORD.EQ.'INST') THEN
            WRITE(IFI2,'(A,A,1PE10.3)') DESRFV(1:LGDESC),' INST=',VRORD
            WRITE(IFI3,'(A,A,1PE10.3)') DESIFV(1:LGDESC),' INST=',VRORD
          ELSEIF(TYPORD.EQ.'FREQ') THEN
            WRITE(IFI2,'(A,A,1PE10.3)') DESRFV(1:LGDESC),' FREQ=',VRORD
            WRITE(IFI3,'(A,A,1PE10.3)') DESIFV(1:LGDESC),' FREQ=',VRORD
          ELSEIF(TYPORD.EQ.'NUME_MODE') THEN
            CALL CODENT(VIORD,'G',CHNMOD)
            LGNTXT=LXLGUT(CHNMOD)
            WRITE(IFI2,'(A,1PE10.3)') DESRFV(1:LGDESC)//
     &           ' MODE='//CHNMOD(1:LGNTXT)//' F=',VRORD
            WRITE(IFI3,'(A,1PE10.3)') DESIFV(1:LGDESC)//
     &           ' MODE='//CHNMOD(1:LGNTXT)//' F=',VRORD
          ELSEIF(TYPORD.EQ.'NUME_CAS') THEN
            LGNTXT=LXLGUT(VKORD)
            WRITE(IFI2,'(A)') DESRFV(1:LGDESC)//
     &           ' CAS='//VKORD(1:LGNTXT)
            WRITE(IFI3,'(A)') DESIFV(1:LGDESC)//
     &           ' CAS='//VKORD(1:LGNTXT)
          ELSE
             WRITE(IFI2,'(A)') DESRFV(1:LGDESC)
             WRITE(IFI3,'(A)') DESIFV(1:LGDESC)
          ENDIF
          INUVAL=0
          DO 34 INO=1,NBNO
            DO 35 IEC=1,NEC
              DG(IEC)=PRNO((INO-1)*(NEC+2)+2+IEC)
  35        CONTINUE
C           - NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
C           - IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
            IVAL = PRNO((INO-1)*(NEC+2)+1)
            NCMP = PRNO((INO-1)*(NEC+2)+2)
            IF(NCMP.EQ.0) THEN
              IF(.NOT.LNULV1) LNULV1=.TRUE.
              IF(.NOT.LNULV2) LNULV2=.TRUE.
              IF(.NOT.LNULV3) LNULV3=.TRUE.
              IF(.NOT.LNULVC) LNULVC=.TRUE.
            ELSE
              IF(EXISDG(DG,ICMPA1)) THEN
                IRGCP1=0
                NBBCL=ICMPA1/30
                IF(NBBCL.GT.0) THEN
                  DO 36 IEC=1,NBBCL
                    DO 37 I=1,30
                      IF(IAND(DG(IEC),2**I).GT.0) IRGCP1=IRGCP1+1
  37                CONTINUE
  36              CONTINUE
                ENDIF
                IF((ICMPA1-30*NBBCL).GT.0) THEN
                  DO 38 I=1,ICMPA1-30*NBBCL
                    IF(IAND(DG(NBBCL+1),2**I).GT.0) IRGCP1=IRGCP1+1
  38              CONTINUE
                ENDIF
              ELSE
                IF(.NOT.LNULV1) LNULV1=.TRUE.
                IF(.NOT.LNULVC) LNULVC=.TRUE.
              ENDIF
              IF(EXISDG(DG,ICMPA2)) THEN
                IRGCP2=0
                NBBCL=ICMPA2/30
                IF(NBBCL.GT.0) THEN
                  DO 39 IEC=1,NBBCL
                    DO 40 I=1,30
                      IF(IAND(DG(IEC),2**I).GT.0) IRGCP2=IRGCP2+1
  40                CONTINUE
  39              CONTINUE
                ENDIF
                IF((ICMPA2-30*NBBCL).GT.0) THEN
                  DO 41 I=1,ICMPA2-30*NBBCL
                    IF(IAND(DG(NBBCL+1),2**I).GT.0) IRGCP2=IRGCP2+1
  41              CONTINUE
                ENDIF
              ELSE
                IF(.NOT.LNULV2) LNULV2=.TRUE.
                IF(.NOT.LNULVC) LNULVC=.TRUE.
              ENDIF
              IF(EXISDG(DG,ICMPA3)) THEN
                IRGCP3=0
                NBBCL=ICMPA3/30
                IF(NBBCL.GT.0) THEN
                  DO 42 IEC=1,NBBCL
                    DO 43 I=1,30
                      IF(IAND(DG(IEC),2**I).GT.0) IRGCP3=IRGCP3+1
  43                CONTINUE
  42              CONTINUE
                ENDIF
                IF((ICMPA3-30*NBBCL).GT.0) THEN
                  DO 44 I=1,ICMPA3-30*NBBCL
                    IF(IAND(DG(NBBCL+1),2**I).GT.0) IRGCP3=IRGCP3+1
  44              CONTINUE
                ENDIF
              ELSE
                IF(.NOT.LNULV3) LNULV3=.TRUE.
                IF(.NOT.LNULVC) LNULVC=.TRUE.
              ENDIF
            ENDIF
            INUVAL=INUVAL+1
            IF(.NOT.LNULV1) THEN
              VALR8R(3*(INUVAL-1)+1)=DBLE(VALE(NUEQ(IVAL-1+IRGCP1)))
              VALR8I(3*(INUVAL-1)+1)=DIMAG(VALE(NUEQ(IVAL-1+IRGCP1)))
            ELSE
              VALR8R(3*(INUVAL-1)+1)=0.0D0
              VALR8I(3*(INUVAL-1)+1)=0.0D0
              LNULV1=.FALSE.
            ENDIF
            IF(.NOT.LNULV2) THEN
              VALR8R(3*(INUVAL-1)+2)=DBLE(VALE(NUEQ(IVAL-1+IRGCP2)))
              VALR8I(3*(INUVAL-1)+2)=DIMAG(VALE(NUEQ(IVAL-1+IRGCP2)))
            ELSE
              VALR8R(3*(INUVAL-1)+2)=0.0D0
              VALR8I(3*(INUVAL-1)+2)=0.0D0
              LNULV2=.FALSE.
            ENDIF
            IF(.NOT.LNULV3) THEN
              VALR8R(3*(INUVAL-1)+3)=DBLE(VALE(NUEQ(IVAL-1+IRGCP3)))
              VALR8I(3*(INUVAL-1)+3)=DIMAG(VALE(NUEQ(IVAL-1+IRGCP3)))
            ELSE
              VALR8R(3*(INUVAL-1)+3)=0.0D0
              VALR8I(3*(INUVAL-1)+3)=0.0D0
              LNULV3=.FALSE.
            ENDIF
            IF(INUVAL.EQ.2) THEN
              WRITE(IFI2,'(6(1PE12.5))') (VALR8R(I),I=1,6)
              WRITE(IFI3,'(6(1PE12.5))') (VALR8I(I),I=1,6)
              INUVAL=0
            ELSEIF(INO.EQ.NBNO) THEN
              WRITE(IFI2,'(3(1PE12.5))') (VALR8R(I),I=1,3)
              WRITE(IFI3,'(3(1PE12.5))') (VALR8I(I),I=1,3)
            ENDIF
  34      CONTINUE
          CLOSE(IFI2)
          CLOSE(IFI3)
          IF(LNULVC) THEN
            NOCMP1=NCMPGD(ICMPA1)
            NOCMP2=NCMPGD(ICMPA2)
            NOCMP3=NCMPGD(ICMPA3)
            LGCMP1=LXLGUT(NOCMP1)
            LGCMP2=LXLGUT(NOCMP2)
            LGCMP3=LXLGUT(NOCMP3)
             VALK(1) = NOCMP1(1:LGCMP1)
             VALK(2) = NOCMP2(1:LGCMP2)
             VALK(3) = NOCMP3(1:LGCMP3)
             VALK(4) = NOSY16(1:LGCH16)
             VALK(5) = NOMVAR(1:LGNOVA)
             VALK(6) = NOFIVA(1:LGFIVA-1)
             CALL U2MESK('A','PREPOST2_25', 6 ,VALK)
            LNULVC=.FALSE.
          ENDIF
          IF(LRESU.AND.(NVIDAP.GT.0)) THEN
C           - ON ECRIT LES FICHIERS VIDES DE VALEURS QUI SUIVENT
            DO 96 IVIDE=IORDEN+1,IORDEN+NVIDAP
              CALL DEFIEN(NOFIVA,NOMSD,LGCONC,NOMGD,NOSY16,LGCH16,
     &                    NOCMP,LGCOMP,LGNUO1,NORDEN,IVIDE,LRESU,
     &                    'VEC_VIDE',1,FICH,LGFICH,NOMVAR,LGNOVA,
     &                    LGDESC,DESRFV,DESIFV)
              FIEN=FICH(1:LGFICH)
              FIEN=FIEN(1:LGFICH)//'.R'
              INQUIRE(FILE=FIEN,ERR=205,IOSTAT=IOS1,EXIST=EXIFVA)
 205          CONTINUE
              IF(IOS1.NE.0) THEN
                IF(.NOT.LRESU) GO TO 101
                GOTO 102
              ENDIF
              IF(EXIFVA) GO TO 998
              OPEN(UNIT=IFI2,ERR=97,STATUS='NEW',FILE=FIEN,IOSTAT=IOS2)
  97          CONTINUE
              IF(IOS2.NE.0) THEN
                IF(.NOT.LRESU) GO TO 101
                GOTO 102
              ENDIF
              CLOSE(IFI2)
              FIEN=FIEN(1:LGFICH)//'.I'
              OPEN(UNIT=IFI3,ERR=98,STATUS='NEW',FILE=FIEN,IOSTAT=IOS3)
  98          CONTINUE
              IF(IOS3.NE.0) THEN
                IF(.NOT.LRESU) GO TO 103
                GOTO 104
              ENDIF
              CLOSE(IFI3)
  96        CONTINUE
          ENDIF
  30    CONTINUE
      ENDIF
      CLOSE(IFI1)
      GOTO 999
 101  CONTINUE
       VALK(1) = FIEN(1:LGFICH)
       VALK(2) = NOSY16(1:LGCH16)
       CALL U2MESK('E','PREPOST2_26', 2 ,VALK)
 102  CONTINUE
       VALK(1) = FIEN(1:LGFICH)
       VALK(2) = NOSY16(1:LGCH16)
       VALK(3) = NOMSD(1:LGCONC)
       CALL U2MESK('E','PREPOST2_27', 3 ,VALK)
      GOTO 999
 103  CONTINUE
       VALK(1) = FIEN(1:LGFICH)
       VALK(2) = NOSY16(1:LGCH16)
       CALL U2MESK('E','PREPOST2_28', 2 ,VALK)
 104  CONTINUE
       VALK(1) = FIEN(1:LGFICH)
       VALK(2) = NOSY16(1:LGCH16)
       VALK(3) = NOMSD(1:LGCONC)
       CALL U2MESK('E','PREPOST2_29', 3 ,VALK)
      GOTO 999
 998  CONTINUE
      CALL U2MESK('E','PREPOST2_30',1,FIEN(1:LGFICH))
 999  CONTINUE
      CALL JEDETR('&&ECRFRE.NOMS_FIVA')
      CALL JEDEMA()
      END
