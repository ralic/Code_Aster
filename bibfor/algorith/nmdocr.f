      SUBROUTINE NMDOCR(CARCRZ,MODELE,NBMO1,MOCLEF,IRET)
C RESPONSABLE PROIX J-M.PROIX
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/03/2012   AUTEUR PROIX J-M.PROIX 
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
C     SAISIE ET STOCKAGE DES PARAMETRES LOCAUX DE COMPOREMENT
C
C IN MODELE  : LE MODELE
C IN NBMO1   : NOMBRE DE MOTS-CLES (1 OU 2) COMP_INCR / COMP_ELAS
C IN MOCLEF  : LISTE DES MOTS-CLES (COMP_INCR / COMP_ELAS)
C OUT IRET   : CODE RETOUR
C OUT CARCRI : CARTE DECRIVANT LES CRITERES LOCAUX DE CONVERGENCE
C                     0 : ITER_INTE_MAXI
C                     1 : COMPOSANTE INUTILISEE
C                     2 : RESI_INTE_RELA
C                     3 : THETA (POUR THM)
C                     4 : ITER_INTE_PAS
C                     5 : ALGO_INTE
C                     .............
C                     13 PARM_ALPHA  -> ALPHA DE SUSHI (DÉFAUT 1)
C ----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*1  K1BID
      CHARACTER*8  NOMA,K8B,TYPMCL(2)
      CHARACTER*16 TYMATG,COMP,ALGO,MOCLES(2),MOCLEF(2)
      CHARACTER*16 TEXTE(3),COMCOD,METHOD,K16BID,NOMCMD
      CHARACTER*19 CARCRI
      CHARACTER*24 CARCRZ
      CHARACTER*24 MESMAI,MODELE
      INTEGER IRET,JNCMP,JVALV,NUMGD,JACMP,NBCRIT,ICMP,K,JMA,NBMA,IRETT
      INTEGER ITEPAS,I,ITDEBO,IBID,TYPTGT,N1,NBMO1,NBOCC
      REAL*8  RESI,RESID,TSAMPL,TSRETU,TSEUIL,PERT,THETA,ITEINT
      REAL*8  ALPHA,ALGOR,TOLRAD
      INTEGER EXITS,GETEXM
      LOGICAL CPLAN,EXICP
C
      INTEGER DIMAKI,DIMANV
C    DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
      PARAMETER (DIMAKI=9)
C    DIMANV = DIMENSION MAX DE LA LISTE DU NOMBRE DE VAR INT EN THM
      PARAMETER (DIMANV=4)
      INTEGER NBKIT,NBNVI(DIMANV),NCOMEL,NVMETA,NUMLC
      CHARACTER*16 NOMKIT(DIMAKI),LCOMEL(5),MECA,MECACO

C      
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
      INTEGER      IARG
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C ----------------------------------------------------------------------
      CALL JEMARQ()
      CARCRI = CARCRZ
      CALL DISMOI('C','NOM_MAILLA',MODELE(1:8),'MODELE',I,NOMA,IRETT)
      CALL GETRES(K8B,K16BID,NOMCMD)

C
C CARTE DES CRITERES DE CONVERGENCE LOCAUX
      CALL JEEXIN(CARCRI(1:19)//'.VALV',IRET)
      IF (IRET.EQ.0) THEN
          CALL ALCART('V',CARCRI,NOMA,'CARCRI')
      ENDIF
      CALL JEVEUO(CARCRI(1:19)//'.NCMP','E',JNCMP)
      CALL JEVEUO(CARCRI(1:19)//'.VALV','E',JVALV)
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD' ,'CARCRI'),NUMGD)
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',NUMGD),'L',JACMP)
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',NUMGD),'LONMAX',
     &            NBCRIT,K1BID)
      DO 95 ICMP = 1,NBCRIT
        ZK8(JNCMP+ICMP-1) = ZK8(JACMP+ICMP-1)
   95 CONTINUE
C ----------------------------------------------------------------------
C     CARTE PAR DEFAUT SI ON OUBLIE COMP_INCR SUR DES MAILLES
      ZR(JVALV-1+1)  = 10
      ZR(JVALV-1+2)  = 0
      ZR(JVALV-1+3)  = 1.D-6
      ZR(JVALV-1+4)  = 1.D0
      DO 96 I=5,NBCRIT
         ZR(JVALV-1+I)  = 0.D0
 96   CONTINUE
      CALL NOCART(CARCRI,1,K8B,K8B,0,K8B,IBID,K8B,NBCRIT)
C ----------------------------------------------------------------------
      MOCLES(1) = 'GROUP_MA'
      MOCLES(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
      MESMAI = '&&NMDOCR'//'.LISTE_MAILLES'

C     LECTURE DES PARAMETRES DE CONVERGENCE A STOCKER DANS CARCRI
C     MOTS CLES FACTEUR
      DO 160 I = 1,NBMO1

        CALL GETFAC(MOCLEF(I),NBOCC)

C       NOMBRE D'OCCURRENCES
        DO 150 K = 1,NBOCC
        
          NCOMEL=1

          CALL RELIEM(MODELE,NOMA,'NU_MAILLE',MOCLEF(I),K,2,MOCLES,
     &                TYPMCL,MESMAI,NBMA)

          CALL GETVTX(MOCLEF(I),'ALGO_INTE',K,IARG,1,ALGO,IRET)
          CALL GETVTX(MOCLEF(I),'RELATION',K,IARG,1,COMP,N1)
C         CREATION DE L'OBJET COMPORTEMENT A PARTIR DU CATALOGUE
          CALL LCCREE(1, COMP, COMCOD)

          IF (IRET.NE.0) THEN
C           SI ALGO_INTE EST RENSEIGNE : VERIF QUE CET ALGO EST POSSIBLE
C           AVEC LA LOI DE COMPORTEMENT
            CALL LCTEST(COMCOD,'ALGO_INTE',ALGO,IRETT)
            IF (IRETT.EQ.0) THEN
                IF((COMP(1:6).EQ.'KIT_HM')
     &         .OR.(COMP(1:7).EQ.'KIT_THM'))THEN
                  CALL NMDOKI(MOCLEF(I),MODELE,COMP,K,DIMAKI,NBKIT,
     &                        NOMKIT,NBNVI,NCOMEL,LCOMEL,NUMLC,NVMETA)
C --- LE COMPORTEMENT MECANIQUE EST STOCKE AU 5EME RANG DE LCOMEL
                  MECA = LCOMEL(NCOMEL)
                  CALL LCCREE(1, MECA, MECACO)
                  CALL LCTEST(MECACO,'ALGO_INTE',ALGO,IRETT)
                  IF (IRETT.EQ.0) THEN
                    TEXTE(1)=ALGO
                    TEXTE(2)='ALGO_INTE'
                    TEXTE(3)=COMP
                    CALL U2MESK('F','COMPOR1_45',3,TEXTE)
                  ENDIF
                ELSE
                    TEXTE(1)=ALGO
                    TEXTE(2)='ALGO_INTE'
                    TEXTE(3)=COMP
                    CALL U2MESK('F','COMPOR1_45',3,TEXTE)
                ENDIF
            ENDIF
          ELSE
C           RECUP DE ALGO_INTE D'APRES LE CATALOGUE DE LA
C           LOI DE COMPORTEMENT (1ERE VALEUR DE LA LISTE)
            CALL LCALGO(COMCOD,ALGO)
          ENDIF

C         CERTAINES LOIS EN CPLAN CHANGENT L'ALGO D'INTEGRATION 
          CPLAN = EXICP(MODELE(1:8),MESMAI,NBMA)
                    
          IF (CPLAN) THEN
            IF (COMP.EQ.'VMIS_ECMI_LINE'.OR.
     &          COMP.EQ.'VMIS_ECMI_TRAC'.OR.
     &          COMP.EQ.'VMIS_ISOT_LINE'.OR.
     &          COMP.EQ.'VMIS_ISOT_TRAC') THEN
              ALGO = 'SECANTE'
            ENDIF
          ENDIF
          
C         PASSAGE NOM ALGO -> IDENTIFICATEUR (VALEUR REELLE)
          CALL UTLCAL('NOM_VALE',ALGO,ALGOR)

C         RECUP DES DONNEES LIEES A LA CONVERGENCE LOCALE
          CALL NMDOCV(MOCLEF(I),K,ALGO,'RESI_INTE_RELA',RESI)
          CALL NMDOCV(MOCLEF(I),K,ALGO,'ITER_INTE_MAXI',ITEINT)

          ITEPAS = 0
          IF (MOCLEF(I).EQ.'COMP_INCR') THEN
            CALL GETVIS(MOCLEF(I),'ITER_INTE_PAS',K,IARG,1,ITEPAS,IRET)
          ENDIF
C
C         CPLAN DEBORST  ET COMP1D DEBORST SEULEMENT EN COMP_INCR
          RESID=1.D-6
          PERT=0.D0
          ITDEBO=1
          TSEUIL=-1.D0
          TSAMPL=-1.D0
          TSRETU=-1.D0
          TYPTGT = 0
          TYMATG=' '
          IF ( MOCLEF(I).EQ. 'COMP_INCR') THEN
             CALL GETVIS(MOCLEF(I),'ITER_CPLAN_MAXI',
     &                   K,IARG,1,ITDEBO,IRET)
             CALL GETVR8(MOCLEF(I),'RESI_CPLAN_MAXI',K,IARG,1,
     &                   RESID,IRET)
             IF (IRET.NE.0) THEN
                RESID=-RESID
             ELSE
                CALL GETVR8(MOCLEF(I),'RESI_CPLAN_RELA',K,IARG,1,
     &                      RESID,IRET)
             ENDIF
             EXITS = GETEXM(MOCLEF(I),'TYPE_MATR_TANG')
             IF (EXITS .EQ. 1) THEN
C               DANS ZR(JVALV+1) ON STOCKE LE TYPE DE MATRICE TGTE
                CALL GETVTX(MOCLEF(I),'TYPE_MATR_TANG',K,IARG,1,TYMATG,
     &                      IRET)
                IF (IRET.EQ.0) THEN
                   TYPTGT = 0
                ELSE
                   IF (TYMATG.EQ.'PERTURBATION') THEN
                      TYPTGT = 1
                      CALL GETVR8(MOCLEF(I),'VALE_PERT_RELA',K,IARG,1,
     &                            PERT,IRET)
                   ELSEIF (TYMATG.EQ.'VERIFICATION') THEN
                      TYPTGT = 2
                      CALL GETVR8(MOCLEF(I),'VALE_PERT_RELA',K,IARG,1,
     &                            PERT,IRET)
                   ELSEIF (TYMATG(1:16).EQ.'TANGENTE_SECANTE') THEN
C                     MATRICE EVOLUTIVE TANGENTE/SECANTE
                      CALL GETVR8(MOCLEF(I),'SEUIL',K,IARG,1,
     &                            TSEUIL,IRET)
                      CALL GETVR8(MOCLEF(I),'AMPLITUDE',K,IARG,1,TSAMPL,
     &                            IRET)
                      CALL GETVR8(MOCLEF(I),'TAUX_RETOUR',K,IARG,1,
     &                            TSRETU,
     &                            IRET)
                   ENDIF
C                  VERIF QUE TYMATG EST POSSIBLE POUR COMP
                   CALL LCTEST(COMCOD,'TYPE_MATR_TANG',TYMATG,IRETT)
                   IF (IRETT.EQ.0) THEN
                      TEXTE(1)=TYMATG
                      TEXTE(2)=COMP
                      CALL U2MESG('F','COMPOR1_46',2,TEXTE,0,0,0,0.D0)
                   ENDIF
                ENDIF
             ENDIF
C GLUTE POUR IMPLEX
             IF (NOMCMD(1:13).EQ.'STAT_NON_LINE') THEN
                CALL GETVTX(' ','METHODE',0,IARG,1,METHOD,IRET)
                IF (IRET.NE.0) THEN
                   IF (METHOD.EQ.'IMPLEX') THEN
                      IF ((TYPTGT.NE.0).AND.(COMP.NE.'SANS')) THEN
                         TEXTE(1)=TYMATG
                         TEXTE(2)='IMPLEX'
                        CALL U2MESG('F','COMPOR1_46',2,TEXTE,0,0,0,0.D0)
                      ELSE
                         TYPTGT=9
                      ENDIF
C                     VERIF QUE TYMATG EST POSSIBLE POUR COMP
                     CALL LCTEST(COMCOD,'TYPE_MATR_TANG','IMPLEX',IRETT)
                      IF ((IRETT.EQ.0).AND.(COMP.NE.'SANS')) THEN
                         TEXTE(1)='IMPLEX'
                         TEXTE(2)=COMP
                        CALL U2MESG('F','COMPOR1_46',2,TEXTE,0,0,0,0.D0)
                      ENDIF
                   ENDIF
                ENDIF
             ENDIF
C FIN GLUTE POUR IMPLEX

          ENDIF
C
          IF (MOCLEF(I) .EQ. 'COMP_INCR') THEN
            CALL GETVR8(MOCLEF(I),'PARM_THETA',K,IARG,1,THETA,IRET)
            CALL GETVR8(MOCLEF(I),'PARM_ALPHA',K,IARG,1,ALPHA ,IRET)
          ELSE
            THETA=1.D0
            ALPHA=1.D0
          ENDIF
C         TOLERANCE POUR LE CRITERE DE RADIALITE
          IF (MOCLEF(I) .EQ. 'COMP_INCR') THEN
             IF (TYPTGT.EQ.0 .AND.
     &           TYMATG(1:16).NE.'TANGENTE_SECANTE') THEN
                CALL GETVR8(MOCLEF(I),'RESI_RADI_RELA',K,IARG,1,TOLRAD,
     &                      IRET)
                IF (IRET.NE.0) THEN 
                   TSEUIL=TOLRAD
                ELSE
                   TSEUIL=-10.D0
                ENDIF
             ENDIF
          ENDIF          
          
C         STOCKAGE DE LA CARTE CARCRI

          ZR(JVALV-1+1)  = ITEINT
          ZR(JVALV-1+2)  = TYPTGT
          ZR(JVALV-1+3)  = RESI
          ZR(JVALV-1+4)  = THETA
          ZR(JVALV-1+5)  = ITEPAS
          ZR(JVALV-1+6)  = ALGOR
          ZR(JVALV-1+7)  = PERT
          ZR(JVALV-1+8)  = RESID
          ZR(JVALV-1+9)  = ITDEBO
          ZR(JVALV-1+10) = TSEUIL
          ZR(JVALV-1+11) = TSAMPL
          ZR(JVALV-1+12) = TSRETU
          ZR(JVALV-1+13) = ALPHA

          IF (NBMA.NE.0) THEN
            CALL JEVEUO(MESMAI,'L',JMA)
            CALL NOCART(CARCRI,3,K8B,'NUM',NBMA,K8B,ZI(JMA),' ',NBCRIT)
            CALL JEDETR(MESMAI)
          ELSE
C           PAR DEFAUT C'EST TOUT='OUI'
            CALL NOCART(CARCRI,1,K8B,K8B,0,K8B,IBID,K8B,NBCRIT)
          ENDIF

  150   CONTINUE

  160 CONTINUE

C FIN ------------------------------------------------------------------
      CALL JEDEMA()
      END
