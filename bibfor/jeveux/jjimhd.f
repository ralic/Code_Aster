      SUBROUTINE JJIMHD (IDFIC,INAT,CRNOM,NGRP,KATTR,IADMI,GENRI,
     &                   TYPEI,LT,LONOI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_18 CRS_508 CRS_512
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
      IMPLICIT NONE
      INTEGER            IDFIC,INAT,IADMI,LT,LONOI
      CHARACTER*(*)      CRNOM,NGRP,GENRI,TYPEI
      CHARACTER*24       KATTR(*)
C ----------------------------------------------------------------------
C ROUTINE UTILISATEUR : ECRIT UN SEGMENT DE VALEURS DANS UN FICHIER HDF
C
C IN  IDFIC  : IDENTIFICATEUR DU FICHIER HDF
C IN  INAT   : TYPE DE L'OBJET JEVEUX : 1 OBJET SIMPLE, 2 COLLECTION
C                                       3 OBJET DE COLLECTION
C IN  CRNOM  : NOM DU DATASET (NOM DE L'OBJET JEVEUX)
C IN  NGRP   : NOM DU GROUPE SOUS LEQUEL LE DATASET EST CREE
C IN  KATTR  : LISTE D'ATTRIBUTS A STOCKER AVEC LE DATASET
C IN  IADMI  : ADRESSE DU PREMIER MOT DU SEGMENT DE VALEUR
C IN  GENRI  : GENRE DE L'OBJET
C IN  TYPEI  : TYPE DE L'OBJET
C IN  LT     : LONGUEUR DU TYPE
C IN  LONOI  : LONGEUR EN ENTIER DU SEGMENT
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IPS ,JDOCU ,JGENR ,JI ,JITAB ,JORIG ,JRNOM 
      INTEGER JTYPE ,KITAB ,N 
C-----------------------------------------------------------------------
      PARAMETER      ( N = 5 )
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      REAL*8           R8ZON(1)
      LOGICAL          LSZON(1)
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) , R8ZON(1) , LSZON(1) )
C ----------------------------------------------------------------------
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
C ----------------------------------------------------------------------
      INTEGER          ILOREP , IDENO , ILNOM , ILMAX , ILUTI , IDEHC
      PARAMETER      ( ILOREP=1,IDENO=2,ILNOM=3,ILMAX=4,ILUTI=5,IDEHC=6)
C ----------------------------------------------------------------------
      INTEGER          LG,IRET,IDDAT,KADM,LADM,IDG
      INTEGER          HDFWSV,HDFOPD,HDFWAT,HDFCLD
      INTEGER          HDFCRG,HDFCLG
      CHARACTER*6      PGME
      PARAMETER      ( PGME = 'JJIMHD' )
      CHARACTER*24     NOMATR
      PARAMETER      ( NOMATR = 'ATTRIBUTS JEVEUX' )
C DEB ------------------------------------------------------------------
      KADM = IADMI
      LADM = ISZON(JISZON + KADM - 3)
      IPS  = KADM - 4
C
      IF ( INAT .EQ. 0 ) THEN
        KATTR(1)='OBJET SYSTEME'
      ELSE IF ( INAT .EQ. 1 ) THEN
        KATTR(1)='OBJET SIMPLE'
      ELSE IF ( INAT .EQ. 2 ) THEN
        KATTR(1)='OBJ. SYSTEME COLLECTION'
      ELSE IF ( INAT .EQ. 3 ) THEN
        KATTR(1)='OBJET DE COLLECTION'
      ELSE IF ( INAT .EQ. -1 ) THEN
        KATTR(1)='COLLECTION'
      ENDIF
      KATTR(3)(3:3)=GENRI
      IF ( GENRI .NE. 'N') THEN
        IF ( TYPEI .EQ. 'S' ) THEN
          JI =   1 + (JISZON +KADM - 1) *LOIS+LADM
          LG=LONOI/(LOR8/2)
          IRET  = HDFWSV (IDFIC,NGRP,CRNOM,TYPEI,LT,K1ZON(JI),LG)
          KATTR(3)(4:24)=' I4'
          KATTR(4)='INTEGER*4'
        ELSE IF ( TYPEI .EQ. 'I' ) THEN
          JI =   1 + (JISZON +KADM - 1) *LOIS+LADM
          LG=LONOI/LOIS
          IRET  = HDFWSV (IDFIC,NGRP,CRNOM,TYPEI,LT,K1ZON(JI),LG)
          KATTR(3)(4:24)=' I'
          KATTR(4)='INTEGER'
        ELSE IF ( TYPEI .EQ. 'R' ) THEN
          JI =   1 + (JISZON +KADM - 1) *LOIS+LADM
          LG=LONOI/LOR8
          IRET  = HDFWSV (IDFIC,NGRP,CRNOM,TYPEI,LT,K1ZON(JI),LG)
          KATTR(3)(4:24)=' R'
          KATTR(4)='REAL*8'
        ELSE IF ( TYPEI .EQ. 'C' ) THEN
          JI =   1 + (JISZON +KADM - 1) *LOIS+LADM
          LG=LONOI/LOR8
          IRET  = HDFWSV (IDFIC,NGRP,CRNOM,TYPEI,LT,K1ZON(JI),LG)
          KATTR(3)(4:24)=' C'
          KATTR(4)='COMPLEX*16'
        ELSE IF ( TYPEI .EQ. 'L' ) THEN
          JI = 1 + ( JISZON + KADM - 1) * LOIS + LADM
          LG=LONOI/LOLS
          IRET  = HDFWSV (IDFIC,NGRP,CRNOM,TYPEI,LT,K1ZON(JI),LG)
          KATTR(3)(4:24)=' L'
          KATTR(4)='LOGICAL'
        ELSE IF ( TYPEI .EQ. 'K' ) THEN
          JI = 1 + ( JISZON + KADM - 1) * LOIS + LADM
          LG=LONOI/LT
          IRET  = HDFWSV (IDFIC,NGRP,CRNOM,TYPEI,LT,K1ZON(JI),LG)
          KATTR(3)(4:24)=' K'
          KATTR(4)='CHARACTER*'
          IF(LT.GT.9) THEN
            WRITE(KATTR(3)(6:7),'(I2)') LT
            WRITE(KATTR(4)(11:12),'(I2)') LT
          ELSE
            WRITE(KATTR(3)(6:6),'(I1)') LT
            WRITE(KATTR(4)(11:11),'(I1)') LT
          ENDIF
        ELSE
          CALL U2MESK('F','JEVEUX1_43',1,TYPEI)
        ENDIF
        IDDAT = HDFOPD (IDFIC,NGRP,CRNOM)
        IRET  = HDFWAT (IDDAT,NOMATR,5,KATTR)
        IF (IRET.LT.0) THEN
          CALL U2MESK('A','JEVEUX1_48',1,CRNOM)
        ENDIF
        IRET  = HDFCLD (IDDAT)
      ELSE
        IDG = HDFCRG (IDFIC,NGRP,CRNOM)
        JITAB = JISZON + KADM - 1
        KITAB = JK1ZON + (KADM - 1) * LOIS
        JI    = KITAB+ISZON(JITAB+IDENO)+ 1
        LG    = ISZON(JITAB+ILMAX)
        LT    = ISZON(JITAB+ILNOM)
        KATTR(3)(2:24)=' N K'
        KATTR(4)='CHARACTER*'
        IF(LT.GT.9) THEN
          WRITE(KATTR(3)(6:7),'(I2)') LT
          WRITE(KATTR(4)(11:12),'(I2)') LT
        ELSE
          WRITE(KATTR(3)(6:6),'(I1)') LT
          WRITE(KATTR(4)(11:11),'(I1)') LT
        ENDIF
        IRET  = HDFWAT (IDG,NOMATR,5,KATTR)
        IF (IRET.LT.0) THEN
          CALL U2MESK('A','JEVEUX1_48',1,CRNOM)
        ENDIF
        IRET  = HDFWSV (IDFIC,CRNOM,'T_NOM',TYPEI,LT,K1ZON(JI),LG)
        TYPEI = 'I'
        JI    = 1 + (JISZON +KADM - 1) *LOIS+LADM
        LG    = ISZON(JITAB + ILOREP)+ IDEHC
        LT    = LOIS
        IRET  = HDFWSV (IDFIC,CRNOM,'T_HCOD',TYPEI,LT,K1ZON(JI),LG)
        IRET  = HDFCLG (IDG)
      ENDIF
C FIN ------------------------------------------------------------------
      END
