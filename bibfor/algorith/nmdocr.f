      SUBROUTINE NMDOCR(CARCRZ,MODELE,NBMO1,MOCLEF,IRET)
C RESPONSABLE PROIX J-M.PROIX
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/09/2008   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                     5 : RESO_INTE (0: EULER_1, 1: RK_2, 2: RK_4)
C ----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*1  K1BID
      CHARACTER*8  NOMA,K8B,TYPMCL(2)
      CHARACTER*16 TYMATG,COMP,TXCP,RESO,MOCLES(2),MOCLEF(2)
      CHARACTER*16 TEXTE(2),COMCOD
      CHARACTER*19 CARCRI, CARCR0
      CHARACTER*24 CARCRZ
      CHARACTER*24 MESMAI,MODELE
      INTEGER IRET,JCRIT,JVALC,NUMGD,JACMP,NBCRIT,ICMP,K,JMA,NBMA,IRETT
      INTEGER ITEINT,ITEPAS,IMPEXP,I,ITDEBO,IBID,TYPTGT,N1,NBMO1,NBOCC
      REAL*8  RESI,R8VIDE,RESID,TSAMPL,TSRETU,TSEUIL,PERT,THETA
      REAL*8  VALCMP(12)
      COMPLEX*16   CBID
      LOGICAL EXITS,GETEXM
      
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C ----------------------------------------------------------------------
      CALL JEMARQ()
     
      CARCRI = CARCRZ
      CALL DISMOI('I','NOM_MAILLA',MODELE(1:8),'MODELE',I,NOMA,IRETT)

C CARTE DES CRITERES DE CONVERGENCE LOCAUX
      CALL JEEXIN(CARCRI(1:19)//'.VALV',IRET)
      IF (IRET.EQ.0) THEN
          CALL ALCART('V',CARCRI,NOMA,'CARCRI')
      ENDIF
      
      CALL JEVEUO(CARCRI(1:19)//'.NCMP','E',JCRIT)
      CALL JEVEUO(CARCRI(1:19)//'.VALV','E',JVALC)
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD' ,'CARCRI'),NUMGD)
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',NUMGD),'L',JACMP)
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',NUMGD),'LONMAX',
     &            NBCRIT,K1BID)
      DO 95 ICMP = 1,NBCRIT
        ZK8(JCRIT+ICMP-1) = ZK8(JACMP+ICMP-1)
   95 CONTINUE

C ----------------------------------------------------------------------
C     CARTE PAR DEFAUT SI ON OUBLIE COMP_INCR SUR DES MAILLES
      CARCR0 = '&&NMDOCR.CARCR0'
      VALCMP(1) = 10
      VALCMP(2) = 0
      VALCMP(3) = 1.D-6
      VALCMP(4) = 1.D0
      DO 96 I=5,NBCRIT
         VALCMP(I) = 0.D0
 96   CONTINUE      
      CALL MECACT('V',CARCR0,'MAILLA',NOMA,'CARCRI',NBCRIT,ZK8(JCRIT),
     &              IBID,VALCMP,CBID,K1BID)
C ----------------------------------------------------------------------

   
      MOCLES(1) = 'GROUP_MA'
      MOCLES(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
      MESMAI = '&&NMDOCR'//'.LISTE_MAILLES'
C  LECTURE DES PARAMETRES DE CONVERGENCE A STOCKER DANS CARCRI

C     MOTS CLES FACTEUR
      DO 160 I = 1,NBMO1
        CALL GETFAC(MOCLEF(I),NBOCC)
C       NOMBRE D'OCCURRENCES
        DO 150 K = 1,NBOCC
          CALL GETVTX(MOCLEF(I),'RESO_INTE',K,1,1,RESO,IRET) 
C         Verif que SCHEMA est possible pour COMP
          CALL GETVTX(MOCLEF(I),'RELATION',K,1,1,COMP,N1)
C         CREATION DE L'OBJET COMPORTEMENT A PARTIR DU CATALOGUE
          CALL LCCREE(1, COMP, COMCOD) 
          CALL LCTEST(COMCOD,'SCHEMA',RESO,IRETT)
          IF (IRETT.EQ.0) THEN
              TEXTE(1)=RESO
              TEXTE(2)=COMP
              CALL U2MESG('F','COMPOR1_45',2,TEXTE,0,0,0,0.D0)
          ENDIF
 
          CALL GETVR8(MOCLEF(I),'RESI_INTE_RELA',K,1,1,RESI,IRET)
          CALL GETVIS(MOCLEF(I),'ITER_INTE_MAXI',K,1,1,ITEINT,
     &              IRET)
          IF (RESI.NE.R8VIDE()  .AND. RESI.GT.1.0001D-6)
     &      CALL U2MESS('A','ALGORITH7_60')
          ITEPAS = 0
          CALL GETVIS('COMP_INCR','ITER_INTE_PAS' ,1,1,1,ITEPAS,
     &                 IRET)
          IMPEXP=-999
          IF(RESO(1:9) .EQ.'IMPLICITE')     IMPEXP = 0
          IF(RESO(1:13).EQ.'RUNGE_KUTTA_2') IMPEXP = 1
          IF(RESO(1:13).EQ.'RUNGE_KUTTA_4') IMPEXP = 2

C         CPLAN DEBORST  ET COMP1D DEBORST SEULEMENT EN COMP_INCR
          RESID=1.D-6
          PERT=0.D0
          ITDEBO=1
          TSEUIL=-1.D0
          TSAMPL=-1.D0
          TSRETU=-1.D0
          TYPTGT = 0
          IF ( MOCLEF(I).EQ. 'COMP_INCR') THEN
                CALL GETVIS(MOCLEF(I),'ITER_MAXI_DEBORST',
     &                  K,1,1,ITDEBO,IRET)
                CALL GETVR8(MOCLEF(I),'RESI_DEBORST',K,1,1,
     &                     RESID,IRET)
             EXITS = GETEXM(MOCLEF(I),'TYPE_MATR_TANG')
             IF (EXITS) THEN
C            dans ZR(JVALC+1) on stocke le type de matrice tgte
             CALL GETVTX(MOCLEF(I),'TYPE_MATR_TANG',K,1,1,TYMATG,IRET)
                IF (IRET.EQ.0) THEN
                   TYPTGT = 0
                ELSE
                   IF (TYMATG.EQ.'PERTURBATION') THEN
                      TYPTGT = 1
                      CALL GETVR8(MOCLEF(I),'VALE_PERT_RELA',K,1,1,
     &                            PERT,IRET)
                   ELSEIF (TYMATG.EQ.'VERIFICATION') THEN
                      TYPTGT = 2
                      CALL GETVR8(MOCLEF(I),'VALE_PERT_RELA',K,1,1,
     &                            PERT,IRET)
                   ELSEIF (TYMATG(1:16).EQ.'TANGENTE_SECANTE') THEN
C                     MATRICE EVOLUTIVE TANGENTE/SECANTE
                      CALL GETVR8(MOCLEF(I),'SEUIL',K,1,1,TSEUIL,IRET)
                      CALL GETVR8(MOCLEF(I),'AMPLITUDE',K,1,1,TSAMPL,
     &                            IRET)
                      CALL GETVR8(MOCLEF(I),'TAUX_RETOUR',K,1,1,TSRETU,
     &                            IRET)
                   ENDIF
C                  Verif que TYMATG est possible pour COMP
                   CALL LCTEST(COMCOD,'TYPE_MATR_TANG',TYMATG,IRETT)
                   IF (IRETT.EQ.0) THEN
                      TEXTE(1)=TYMATG
                      TEXTE(2)=COMP
                      CALL U2MESG('F','COMPOR1_46',2,TEXTE,0,0,0,0.D0)
                   ENDIF
                ENDIF
             ENDIF
          ENDIF

          IF (MOCLEF(I) .EQ. 'COMP_INCR') THEN
              CALL GETVR8(MOCLEF(I),'PARM_THETA',K,1,1,THETA,IRET)
          ELSE
              THETA=1.D0
          ENDIF

C         STOCKAGE DE LA CARTE CARCRI

          CALL RELIEM(MODELE,NOMA,'NU_MAILLE',MOCLEF(I),K,2,MOCLES,
     &                 TYPMCL,MESMAI,NBMA)
          IF (NBMA.NE.0) THEN
             CALL JEVEUO(MESMAI,'L',JMA)
               ZR(JVALC)    = ITEINT
               ZR(JVALC+1)  = TYPTGT
               ZR(JVALC+2)  = RESI
               ZR(JVALC+3)  = THETA
               ZR(JVALC+4)  = ITEPAS
               ZR(JVALC+5)  = IMPEXP
               ZR(JVALC+6)  = PERT
               ZR(JVALC+7)  = RESID
               ZR(JVALC+8)  = ITDEBO
               ZR(JVALC+9)  = TSEUIL
               ZR(JVALC+10) = TSAMPL
               ZR(JVALC+11) = TSRETU
               CALL NOCART(CARCRI,3,K8B,'NUM',NBMA,K8B,ZI(JMA),' ',
     &                   NBCRIT)
             CALL JEDETR(MESMAI)

          ELSE
C ----    PAR DEFAUT C'EST TOUT='OUI'
               ZR(JVALC)    = ITEINT
               ZR(JVALC+1)  = TYPTGT
               ZR(JVALC+2)  = RESI
               ZR(JVALC+3)  = THETA
               ZR(JVALC+4)  = ITEPAS
               ZR(JVALC+5)  = IMPEXP
               ZR(JVALC+6)  = PERT
               ZR(JVALC+7)  = RESID
               ZR(JVALC+8)  = ITDEBO
               ZR(JVALC+9)  = TSEUIL
               ZR(JVALC+10) = TSAMPL
               ZR(JVALC+11) = TSRETU
               CALL NOCART(CARCRI,1,K8B,K8B,0,K8B,IBID,K8B,NBCRIT)
          ENDIF
  150   CONTINUE
  160 CONTINUE
C     SURCHARGE DE LA CARTE CARCR0 PAR CARCRI
      CALL NMDOFU(CARCR0,CARCRI)
      

C FIN ------------------------------------------------------------------
      CALL JEDEMA()
      END
