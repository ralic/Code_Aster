      SUBROUTINE OP0027 ( IER )
C RESPONSABLE CAMBIER S.CAMBIER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/03/2008   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     GENE_MATR_ALEA : GENERATEUR DE MATRICES GENERALISEE ALEATOIRE
C     CONSTRUITE EN UTILISANT LE PRINCIPE DU MAXIMUM D'ENTROPIE ET
C     L'INFORMATION DISPONIBLE POUR DES MATRICES GENERALISEES
C     SYMETRIQUES DEFINIES POSITIVES.
C
C ----------------------------------------------------------------------
      IMPLICIT   NONE
      INTEGER    IER
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      IRET, N1, N, I, M, IAK,IADR,IADR1,IADR2,IBID
      INTEGER      IDESC, IALIME, IACONL, JREFA2, JREFA1
      INTEGER      JUMP, IRET2
      REAL*8       DELTA
      CHARACTER*8  NOMRES, NOMMAT, K8B
      CHARACTER*16 NOMCMD, CONCEP
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
C
      IER=0
C
      CALL GETRES ( NOMRES, CONCEP, NOMCMD )
C

      CALL GETVID ( ' ', 'MATR_MOYEN', 1,1,1, NOMMAT, N1 )
C
      CALL GETVIS ( ' ', 'INIT_ALEA'    , 0,1,1, JUMP , N1 )
      IF (N1 .NE. 0) CALL INIRAN(JUMP)

      IF (CONCEP.EQ.'MATR_ASSE_GENE_R') THEN

C===================================================
C --- GENERATION D UNE MATRICE GENERALISEE ALEATOIRE
C===================================================

        CALL GETVR8 ( ' ', 'COEF_VAR'     , 1,1,1, DELTA,  N1 )

        CALL JEVEUO(NOMMAT//'           .DESC','L',IDESC)

C --- VERIF PROFIL=PLEIN
        IF (ZI(IDESC+2).NE.2) THEN
      CALL U2MESS('F','ALGORITH9_18')
        ENDIF

        N = ZI(IDESC+1)
        M = N*(N+1)/2

        CALL JEEXIN(NOMRES//'           .VALM',IRET)
C
        IF (IRET.EQ.0) THEN
          CALL JEVEUO(NOMMAT//'           .REFA','L',JREFA1)
C
C ------ CREATION DES BASES DE DONNEES DE LA MATRICE A GENERER.
C        SUIVANT LE MODELE DE OP0071
C
          CALL JECREC(NOMRES//'           .VALM','G V R','NU',
     &                           'DISPERSE', 'CONSTANT',1)
          CALL JECROC(JEXNUM(NOMRES//'           .VALM',1))
          CALL JEECRA(NOMRES//'           .VALM','LONMAX',M,K8B)
C

          CALL WKVECT(NOMRES//'           .DESC','G V I',3,IDESC)
          ZI(IDESC)   = 2
          ZI(IDESC+1) = N
          ZI(IDESC+2) = 2
C
          CALL WKVECT ( NOMRES//'           .LIME', 'G V K24',1, IALIME)
          ZK24(IALIME) = '                        '
C
          CALL WKVECT ( NOMRES//'           .CONL', 'G V R', N, IACONL)
          DO 10 I = 1 , N
            ZR(IACONL+I-1) = 1.0D0
 10       CONTINUE
C
          CALL WKVECT(NOMRES//'           .REFA','G V K24',11,JREFA2)
          ZK24(JREFA2-1+11)='MPI_COMPLET'
          ZK24(JREFA2-1+1) = ZK24(JREFA1-1+1)
          ZK24(JREFA2-1+2) = ZK24(JREFA1-1+2)
          ZK24(JREFA2-1+9) = ZK24(JREFA1-1+9)
          ZK24(JREFA2-1+10) = ZK24(JREFA1-1+10)
C
        ENDIF
C
        CALL JEVEUO(JEXNUM(NOMMAT//'           .VALM',1),'L',IAK)
        CALL JEVEUO(JEXNUM(NOMRES//'           .VALM',1),'E',IADR)
        DO 20 I=1,M
          ZR(IADR-1+I) = 0.D0
 20     CONTINUE

        CALL WKVECT ( '&&OP0027.VECTTRA1', 'V V R', M, IADR1 )
        CALL WKVECT ( '&&OP0027.VECTTRA2', 'V V R', M, IADR2 )

        CALL GEMATG (N, DELTA, ZR(IAK), ZR(IADR), ZR(IADR1), ZR(IADR2))


      ELSE
C===================================================
C --- GENERATION D UN MACRO-ELEMENT DYNAMIQUE ALEATOIRE
C===================================================
C
        CALL JEVEUO (NOMMAT//'.MAEL_RAID_DESC','L',IDESC)

        N = ZI(IDESC+1)
        M = N*(N+1)/2
        CALL WKVECT ( '&&OP0027.VECTTRA1', 'V V R', M, IADR1 )
        CALL WKVECT ( '&&OP0027.VECTTRA2', 'V V R', M, IADR2 )

C -- EXISTENCE DES MATRICES, PRESENCE AMORTISSEMENT, COPIE STUCTURES
        CALL JEEXIN(NOMRES//'.MAEL_RAID_VALE',IRET)
        IF (IRET.EQ.0) THEN
          CALL COPISD(' ','G',NOMMAT,NOMRES)
        ELSE
          CALL JEEXIN(NOMMAT//'.MAEL_AMOR_VALE',IRET2)
          IF (IRET2.NE.0) THEN
            CALL JEEXIN(NOMRES//'.MAEL_AMOR_VALE',IRET2)
            IF (IRET2.EQ.0) THEN
              CALL COPISD(' ','G',NOMMAT,NOMRES)
            ENDIF
          ENDIF
        ENDIF

C -- RAIDEUR
        CALL GETVR8 ( ' ', 'COEF_VAR_RIGI'     , 1,1,1, DELTA,  N1 )
        IF (DELTA.GT.0.D0) THEN
C GENRRATION RAIDEUR

          CALL JEVEUO(JEXNUM(NOMMAT//'.MAEL_RAID_VALE',1),'L',IAK)
          CALL JEVEUO(JEXNUM(NOMRES//'.MAEL_RAID_VALE',1),'E',IADR)
          DO 30 I=1,M
            ZR(IADR-1+I) = 0.D0
 30       CONTINUE

          CALL GEMATG (N,DELTA, ZR(IAK), ZR(IADR), ZR(IADR1), ZR(IADR2))

        ELSE
C COPIE VALEURS RAIDEUR
          IF (IRET.NE.0) THEN
           CALL JEVEUO(JEXNUM(NOMMAT//'.MAEL_RAID_VALE',1),'L',IAK)
           CALL JEVEUO(JEXNUM(NOMRES//'.MAEL_RAID_VALE',1),'E',IADR)
           DO 35 I=1,M
             ZR(IADR-1+I) = ZR(IAK-1+I)
 35        CONTINUE
           ENDIF

        ENDIF

C -- MASSE
        CALL GETVR8 ( ' ', 'COEF_VAR_MASS'     , 1,1,1, DELTA,  N1 )

        IF (DELTA.GT.0.D0) THEN
C GENRRATION MASSE

          CALL JEVEUO(JEXNUM(NOMMAT//'.MAEL_MASS_VALE',1),'L',IAK)
          CALL JEVEUO(JEXNUM(NOMRES//'.MAEL_MASS_VALE',1),'E',IADR)
          DO 40 I=1,M
            ZR(IADR-1+I) = 0.D0
 40       CONTINUE

          CALL GEMATG (N,DELTA, ZR(IAK), ZR(IADR), ZR(IADR1), ZR(IADR2))

        ELSE
C COPIE VALEURS MASSE
          IF (IRET.NE.0) THEN
           CALL JEVEUO(JEXNUM(NOMMAT//'.MAEL_MASS_VALE',1),'L',IAK)
           CALL JEVEUO(JEXNUM(NOMRES//'.MAEL_MASS_VALE',1),'E',IADR)
           DO 45 I=1,M
               ZR(IADR-1+I) = ZR(IAK-1+I)
 45        CONTINUE
          ENDIF

        ENDIF

C -- AMORTISSEMNT
        CALL GETVR8 ( ' ', 'COEF_VAR_AMOR'     , 1,1,1, DELTA,  N1 )

        IF (DELTA.GT.0.D0) THEN
C GENRRATION AMORTISSEMENT
          CALL JEEXIN(NOMMAT//'.MAEL_AMOR_VALE',IRET)
          IF (IRET.EQ.0) THEN
            CALL U2MESS('A','ALGORITH9_19')
          ELSE

          CALL JEVEUO(JEXNUM(NOMMAT//'.MAEL_AMOR_VALE',1),'L',IAK)
          CALL JEVEUO(JEXNUM(NOMRES//'.MAEL_AMOR_VALE',1),'E',IADR)
            DO 50 I=1,M
              ZR(IADR-1+I) = 0.D0
 50         CONTINUE

          CALL GEMATG (N,DELTA, ZR(IAK), ZR(IADR), ZR(IADR1), ZR(IADR2))

          END IF

        ELSE
C COPIE VALEURS AMORTISSEMENT
          IF (IRET.NE.0) THEN
            CALL JEEXIN(NOMMAT//'.MAEL_AMOR_VALE',IRET)
            IF (IRET.NE.0) THEN
           CALL JEVEUO(JEXNUM(NOMMAT//'.MAEL_AMOR_VALE',1),'L',IAK)
           CALL JEVEUO(JEXNUM(NOMRES//'.MAEL_AMOR_VALE',1),'E',IADR)
              DO 55 I=1,M
                ZR(IADR-1+I) = ZR(IAK-1+I)
 55           CONTINUE
            ENDIF
          ENDIF
        ENDIF

      ENDIF

C
      CALL JEDEMA()
      END
