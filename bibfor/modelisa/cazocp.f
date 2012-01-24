      SUBROUTINE CAZOCP(CHAR  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 23/01/2012   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*8  CHAR
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE XFEM - LECTURE DONNEES)
C
C LECTURE DES PARAMETRES PRINCIPAUX QUI NE DEPENDENT PAS DE LA ZONE
C DE CONTACT
C
C ----------------------------------------------------------------------
C
C
C IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
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
      CHARACTER*24 DEFICO
      CHARACTER*24 PARACR,PARACI
      INTEGER      JPARCR,JPARCI
      INTEGER      NBREAC,LGBLOC,GCPMAX,PREMAX
      INTEGER      REACCA,REACBS,REACBG
      CHARACTER*16 RECH,PREC,REAC,TYPCON,ISTO
      CHARACTER*16 ALGOCO,ALGOFR
      INTEGER      NOC
      REAL*8       PRECIS,COEFRS
      REAL*8       RESIGE,RESIFR
      LOGICAL      CFDISL,LGCP
      LOGICAL      LCTCD,LCTCC,LXFCM,LFROT,LMAIL
      CHARACTER*16 LISSA
      INTEGER      IARG
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ
C
C --- INITIALISATIONS
C
      DEFICO = CHAR(1:8)//'.CONTACT'
      REAC   = 'AUTOMATIQUE'
      ALGOCO = ' '
      ALGOFR = ' '
      NBREAC = 2
      LGBLOC = 10
      RESIGE = 1.D-2
      RESIFR = 1.D-2
C
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C
      PARACR = DEFICO(1:16)//'.PARACR'
      PARACI = DEFICO(1:16)//'.PARACI'
      CALL JEVEUO(PARACR,'E',JPARCR)
      CALL JEVEUO(PARACI,'E',JPARCI)
C
C --- DRAPEAUX
C
      LCTCD  = CFDISL(DEFICO,'FORMUL_DISCRETE')
      LCTCC  = CFDISL(DEFICO,'FORMUL_CONTINUE')
      LMAIL  = LCTCD.OR.LCTCC
      LXFCM  = CFDISL(DEFICO,'FORMUL_XFEM')
      LGCP   = CFDISL(DEFICO,'CONT_GCP'  )
      LFROT  = CFDISL(DEFICO,'FROTTEMENT')
C
C --- PARAMETRES BOUCLE GEOMETRIQUE
C
      CALL GETVTX(' ','REAC_GEOM',1 ,IARG,1,REAC,NOC)
      IF (REAC .EQ. 'SANS') THEN
        ZI(JPARCI+1-1) = 0
        ZR(JPARCR+1-1) = RESIGE
      ELSEIF (REAC .EQ. 'AUTOMATIQUE') THEN
        ZI(JPARCI+1-1) = -1
        CALL GETVIS(' ','ITER_GEOM_MAXI',1 ,IARG,1,REACBG,NOC)
        ZI(JPARCI+6-1) = REACBG
        CALL GETVR8(' ','RESI_GEOM',1 ,IARG,1,RESIGE,NOC)
        ZR(JPARCR+1-1) = RESIGE
      ELSEIF (REAC .EQ. 'CONTROLE') THEN
        CALL GETVIS(' ','NB_ITER_GEOM',1 ,IARG,1,NBREAC,NOC)
        ZI(JPARCI+1-1) = NBREAC
        ZR(JPARCR+1-1) = RESIGE
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- PARAMETRES BOUCLE CONTACT
C
      IF (LCTCD) THEN
        CALL GETVIS(' ','ITER_CONT_MULT',1 ,IARG,1,REACCA,NOC)
        ZI(JPARCI+5-1)  = REACCA
        ZI(JPARCI+10-1) = -1
      ELSEIF (LCTCC.OR.LXFCM) THEN
        CALL GETVTX(' ','ITER_CONT_TYPE',1 ,IARG,1,TYPCON,NOC)
        IF (TYPCON.EQ.'MULT') THEN
          REACCA = 4
          CALL GETVIS(' ','ITER_CONT_MULT',1 ,IARG,1,REACCA,NOC)
          ZI(JPARCI+5-1)  = REACCA
          ZI(JPARCI+10-1) = -1
        ELSEIF (TYPCON.EQ.'MAXI') THEN
          REACCA = 30
          CALL GETVIS(' ','ITER_CONT_MAXI',1 ,IARG,1,REACCA,NOC)
          ZI(JPARCI+10-1)  = REACCA
          ZI(JPARCI+5-1) = -1
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        IF (LCTCC) THEN
          CALL GETVTX(' ','ALGO_RESO_CONT',1 ,IARG,1,ALGOCO,NOC)
          IF (ALGOCO.EQ.'POINT_FIXE') THEN
            ZI(JPARCI+27-1) = 0
          ELSEIF (ALGOCO.EQ.'NEWTON') THEN
            ZI(JPARCI+27-1) = 1
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        ENDIF
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- PARAMETRES BOUCLE FROTTEMENT
C
      IF (LFROT) THEN
        IF (LCTCC.OR.LXFCM) THEN
          CALL GETVTX(' ','REAC_FROT',1 ,IARG,1,REAC,NOC)
          IF (REAC .EQ. 'AUTOMATIQUE') THEN
            ZI(JPARCI+20-1) = -1
            CALL GETVIS(' ','ITER_FROT_MAXI',1 ,IARG,1,REACBS,NOC)
            ZI(JPARCI+7-1) = REACBS
            CALL GETVR8(' ','RESI_FROT',1 ,IARG,1,RESIFR,NOC)
            ZR(JPARCR+2-1) = RESIFR
          ELSEIF (REAC .EQ. 'CONTROLE') THEN
            CALL GETVIS(' ','NB_ITER_FROT',1 ,IARG,1,NBREAC,NOC)
            ZI(JPARCI+20-1) = NBREAC
            ZR(JPARCR+2-1) = RESIFR
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
          IF (LCTCC) THEN
            CALL GETVTX(' ','ALGO_RESO_FROT',1 ,IARG,1,ALGOFR,NOC)
            IF (ALGOFR.EQ.'POINT_FIXE') THEN
              ZI(JPARCI+28-1) = 0
            ELSEIF (ALGOFR.EQ.'NEWTON') THEN
              ZI(JPARCI+28-1) = 1
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF
          IF (LXFCM) THEN
            IF (ZI(JPARCI+1-1).EQ.0) THEN
              ZI(JPARCI+28-1) = 0
            ELSE
              ZI(JPARCI+28-1) = 1
            ENDIF
          ENDIF
        ENDIF
      ELSE
        ZI(JPARCI+20-1) = 0
      ENDIF
C
C --- FORMULATION DISCRETE
C
      IF (LCTCD) THEN
C ---   ARRET OU PAS SI MATRICE DE CONTACT SINGULIERE
        CALL GETVTX(' ','STOP_SINGULIER',1 ,IARG,1,ISTO,NOC)
        IF (ISTO .EQ. 'OUI') THEN
          ZI(JPARCI+2-1) = 0
        ELSEIF (ISTO .EQ. 'NON') THEN
          ZI(JPARCI+2-1) = 1
        ELSE
          CALL ASSERT(.FALSE.)
        END IF
C ---   NOMBRE DE PAQUETS POUR LA RESOLUTION DES SYSTEMES LINEAIRES
        CALL GETVIS(' ','NB_RESOL',1 ,IARG,1,LGBLOC,NOC)
        ZI(JPARCI+3-1) = LGBLOC
C
C --- PARAMETRE GCP
C
        IF (LGCP) THEN
          CALL GETVR8(' ','RESI_ABSO',1 ,IARG,1,PRECIS,NOC)
          IF (NOC.EQ.0) THEN
            CALL U2MESS('F','CONTACT_4')
          ENDIF
          ZR(JPARCR+4-1) = PRECIS

C ---     NON UTILISE
          ZI(JPARCI+11-1) = 0

          CALL GETVIS(' ','ITER_GCP_MAXI',1 ,IARG,1,GCPMAX,NOC)
          ZI(JPARCI+12-1) = GCPMAX

          CALL GETVTX(' ','PRE_COND',1 ,IARG,1,PREC,NOC)
          IF (PREC.EQ.'SANS') THEN
            ZI(JPARCI+13-1) = 0
          ELSE IF (PREC.EQ.'DIRICHLET') THEN
            ZI(JPARCI+13-1) = 1
            CALL GETVR8(' ','COEF_RESI',1 ,IARG,1,COEFRS,NOC)
            ZR(JPARCR+5-1)  = COEFRS
            CALL GETVIS(' ','ITER_PRE_MAXI',1 ,IARG,1,PREMAX,NOC)
            ZI(JPARCI+14-1) = PREMAX
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF

          CALL GETVTX(' ','RECH_LINEAIRE',1 ,IARG,1,RECH,NOC)
          IF (RECH.EQ.'ADMISSIBLE') THEN
            ZI(JPARCI+15-1) = 0
          ELSE IF (RECH.EQ.'NON_ADMISSIBLE') THEN
            ZI(JPARCI+15-1) = 1
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        ENDIF
      ENDIF
C
C --- LISSAGE
C
      IF (LMAIL) THEN
        CALL GETVTX(' ','LISSAGE',1 ,IARG,1,LISSA,NOC)
        IF (LISSA(1:3) .EQ. 'NON') THEN
          ZI(JPARCI+19-1) = 0
        ELSEIF (LISSA(1:3) .EQ. 'OUI') THEN
          ZI(JPARCI+19-1) = 1
        ELSE
          CALL ASSERT(.FALSE.)
        END IF
      ENDIF
C
C --- METHODE VERIF
C
      IF (LMAIL) THEN
        CALL GETVTX(' ','STOP_INTERP',1 ,IARG,1,ISTO,NOC)
        IF (ISTO.EQ.'OUI') THEN
          ZI(JPARCI+25-1) = 1
        ELSEIF (ISTO.EQ.'NON') THEN
          ZI(JPARCI+25-1) = 0
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ENDIF
C
      CALL JEDEMA()
      END
