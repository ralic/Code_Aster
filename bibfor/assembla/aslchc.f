      SUBROUTINE ASLCHC(BASE,MATAS,STOC,NSTOC,MOTC)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) MATAS
      CHARACTER*1 BASE
      CHARACTER*4 MOTC
      INTEGER NSTOC,STOC(*)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C OBJET :
C        TRAITEMENT DES CHARGES CINEMATIQUES DANS LES MATRICE ASSEMBLEES
C        CONSTRUCTION DU .LLIG ET DU .VALI DANS LE CAS D'UNE MATRICE
C        STOCKEE SOUS FORME LIGN_CIEL
C-----------------------------------------------------------------------
C IN   BASE    K*1     : 'G','V' BASE SUR LAQUELLE EST MATAS
C VAR  MATAS   K*19    : NOM DE LA MATR_ASSE
C IN   STOC    I(*)    : TABLEAU ENTIER DE DIM = NEQ DONNANT LES
C                        LES LIGNES A STOCKES ET A ELIMINER ET LEUR
C                        NUMERO DE STOCKAGE
C IN   NSTOC   I       : NOMBRE DE LIGNES DE LA MATRICE A ELIMINER
C IN   MOTC    K*4     : 'ZERO' OU 'CUMU'
C                        'ZERO': SI .VALI OU .LLIG EXISTE DEJA ON
C                        S'ARRETE EN ERREUR 'F'.
C                        'CUMU': SI .LLIG ET .VALI EXISTENT ON ENRICHI
C-----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C-----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      CHARACTER*8 KBID
      CHARACTER*19 MAT,SMAT
      INTEGER TYPMAT,DLLIG,DALIG,DBLI,NBLI
C----------------------------------------------------------------------
C                DEBUT DES INSTRUCTIONS
      CALL JEMARQ()
C----------------------------------------------------------------------
      MAT = MATAS
      IF (NSTOC.EQ.0) GO TO 180
      CALL MTDSCR(MAT)
      CALL JEVEUO(MAT(1:19)//'.&INT','E',LMAT)
      NEQ = ZI(LMAT+2)
      CALL JEVEUO(ZK24(ZI(LMAT+1)) (1:19)//'.REFA','L',IDREFE)
      CALL JEVEUO(ZK24(IDREFE+2) (1:19)//'.HCOL','L',IDHCOL)
      LONBLO = ZI(LMAT+14)
      CALL JEEXIN(MAT//'.LLIG',IERL)
      CALL JEEXIN(MAT//'.VALI',IERI)
      CALL JEEXIN(MAT//'.ALIG',IERA)
      CALL JEEXIN(MAT//'.ABLI',IERB)
      IF (IERL.NE.0) THEN
        IF (MOTC.EQ.'ZERO') THEN
C     QUAND ON RECONSTRUIRA LA MATRICE
          CALL UTMESS('F','ASLCHC_0','LA MATRICE POSSEDE DEJA .LLIG ')
        ELSE IF (MOTC.EQ.'CUMU') THEN
          IF (IERB.EQ.0) CALL UTMESS('F','ASLCHC_1',
     &                               '.ABLI INEXISTANT INCOHERENCE')
          IF (IERI.EQ.0) CALL UTMESS('F','ASLCHC_2',
     &                               '.VALI INEXISTANT INCOHERENCE')
          IF (IERA.EQ.0) CALL UTMESS('F','ASLCHC_3',
     &                               '.ALIG INEXISTANT INCOHERENCE')
          CALL JEVEUO(MAT//'.LLIG','E',IDLLIG)
          NSTOCO = ZI(IDLLIG)
          CALL JEVEUO(MAT//'.ALIG','E',IDALIG)
          CALL JELIRA(MAT//'.ALIG','LONMAX',DALIG,KBID)
          IF (DALIG.NE.NSTOCO) CALL UTMESS('F','ASLCHC_4',
     &                              '.ALIG MAUVAISE DIM INCOHERENCE')
          DLLIG = 1 + 3* (NSTOCO+NSTOC)
          CALL JUVECA(MAT//'.LLIG',DLLIG)
          CALL JEVEUO(MAT//'.LLIG','E',IDLLIG)
          DALIG = NSTOCO + NSTOC
          CALL JUVECA(MAT//'.ALIG',DALIG)
          CALL JEVEUO(MAT//'.ALIG','E',IDALIG)
        ELSE
          CALL UTMESS('F','ASLCHC_5',' LES ARGUMENTS POSSIBLES DE MOTC'
     &                //' SONT ZERO OU CUMU')
        END IF
      ELSE
        NSTOCO = 0
        DLLIG = 1 + 3*NSTOC
        CALL WKVECT(MAT//'.LLIG',BASE//' V I ',DLLIG,IDLLIG)
        DALIG = NSTOC
        CALL WKVECT(MAT//'.ALIG',BASE//' V I ',DALIG,IDALIG)
      END IF
      ZI(IDLLIG) = NSTOC + NSTOCO
      IDLLIG = IDLLIG + NSTOCO*3 + 1
      IDALIG = IDALIG + NSTOCO
      DO 20 IEQ = 1,NEQ
        KSTOC = STOC(IEQ)
        IHCOL = ZI(IDHCOL-1+IEQ)
        IF (KSTOC.NE.0) THEN
          ZI(IDLLIG+3* (KSTOC-1)) = IEQ
          ZI(IDLLIG+3* (KSTOC-1)+1) = IEQ - IHCOL + 1
          ZI(IDLLIG+3* (KSTOC-1)+2) = IEQ
        END IF
        DO 10 JEQ = IEQ - IHCOL + 1,IEQ - 1
          JSTOC = STOC(JEQ)
          IF (JSTOC.NE.0) ZI(IDLLIG+3*JSTOC-1) = IEQ
   10   CONTINUE
   20 CONTINUE

C --- CALCUL DE .ALIG ET DE .ABLI
      IF (IERI.NE.0) THEN
        CALL JELIRA(MAT//'.VALI','NUTIOC',NBLIO,KBID)
        CALL JEVEUO(MAT//'.ABLI','E',IDABLI)
        CALL JELIRA(JEXNUM(MAT//'.VALI',NBLIO),'LONUTI',IVALI,KBID)
      ELSE
        IVALI = 0
        NBLIO = 1
      END IF
      NBLI = NBLIO
      CALL WKVECT('&ASLCHC.TAMPON.ABLI','V V I',NSTOC+1,ITABLI)
      DO 30 I = 1,NSTOC
        JDEB = ZI(IDLLIG-1+3*I-1)
        JFIN = ZI(IDLLIG-1+3*I)
        LVALI = JFIN - JDEB + 1
        IF (LVALI.GT.LONBLO) THEN
          CALL UTMESS('F','ASLCHC_7','TAILLE DE BLOC TROP PETITE ')
        END IF
        IF (IVALI+LVALI.GT.LONBLO) THEN
CCC       WRITE(6,*) 'PASSAGE DU BLOC :',NBLI,' AU SUIVANT ISTOC=',I
          NBLI = NBLI + 1
          IVALI = 0
        END IF
        ZI(ITABLI+NBLI) = I
        ZI(IDALIG-1+I) = IVALI + 1
        IVALI = IVALI + LVALI
   30 CONTINUE

C --- MISE A JOUR .ABLI
      IF (IERB.NE.0) THEN
        CALL JUVECA(MAT//'.ABLI',NBLI+1)
        CALL JEVEUO(MAT//'.ABLI','E',IDABLI)
      ELSE
        CALL WKVECT(MAT//'.ABLI',BASE//' V I',NBLI+1,IDABLI)
      END IF
      DO 40 I = NBLIO,NBLI
        ZI(IDABLI+I) = ZI(ITABLI+I)
CCC     WRITE(6,*) ' ABLI(',I,')=',ZI(IDABLI+I)
   40 CONTINUE
      CALL JEDETR('&ASLCHC.TAMPON.ABLI')
CCCCCCC
C     DO 200 I = NBLIO,NBLI
C       ISTO1= ZI(IDABLI+I-1)+1
C       ISTO2= ZI(IDABLI+I)










C200  CONTINUE

C --- MISE A JOUR DE .VALI
      TYPMAT = ZI(LMAT+3)
      IF (IERI.NE.0) THEN
        CALL UTMESS('F','ASLCHC_!!','PAS FAIT')
      ELSE
        IF (TYPMAT.EQ.1) CALL JECREC(MAT//'.VALI',BASE//' V R','NU',
     &                               'DISPERSE','CONSTANT',NBLI)
        IF (TYPMAT.EQ.2) CALL JECREC(MAT//'.VALI',BASE//' V C','NU',
     &                               'DISPERSE','CONSTANT',NBLI)
      END IF

C --- REMPLISSAGE DE .VALI
      NBBLOC = ZI(LMAT+13)
      CALL MTDSC2(ZK24(ZI(LMAT+1)),'ABLO','L',IDABLO)
      CALL JEVEUO(ZK24(ZI(LMAT+1)) (1:19)//'.REFA','L',IDREFE)
      CALL JEVEUO(ZK24(IDREFE+2) (1:19)//'.HCOL','L',IDHCOL)
      CALL MTDSC2(ZK24(ZI(LMAT+1)),'ADIA','L',IDADIA)
      CALL JEEXIN(JEXNUM(MAT//'.VALI',1),IERI)
      IF (IERI.NE.0) THEN
        IBLIC = NBLIO
        CALL JEVEUO(JEXNUM(MAT//'.VALI',IBLIC),'E',IDVALI)
      ELSE
        IBLIC = 1
        CALL JECROC(JEXNUM(MAT//'.VALI',IBLIC))
        CALL JEECRA(JEXNUM(MAT//'.VALI',IBLIC),'LONMAX',LONBLO,KBID)
        CALL JEVEUO(JEXNUM(MAT//'.VALI',IBLIC),'E',IDVALI)
      END IF
      IBLLIF = ZI(IDABLI+IBLIC)
      DO 170 IBLOC = 1,NBBLOC
        IDEB = ZI(IDABLO+IBLOC-1) + 1
        IFIN = ZI(IDABLO+IBLOC)
        CALL JEVEUO(JEXNUM(MAT//'.VALE',IBLOC),'E',IDBLOC)

C---  SAUVEGARDE DES COLONNES A ELIMINEES DE LA TRIANGULEE SUPERIEURE

        IVALED = 0
        IVALEF = 0
        CALL JEVEUO(JEXNUM(MAT//'.VALI',IBLIC),'E',IDVALI)
        JMIN = ISMAEM()
        DO 70 IEQ = IDEB,IFIN
          KSTOCI = STOC(IEQ)
          IVALED = IVALEF + 1
          IVALEF = ZI(IDADIA-1+IEQ)
          JDEB = IEQ - ZI(IDHCOL-1+IEQ) + 1
          JMIN = MIN(JMIN,JDEB)
          IVALI = 0
          IF (KSTOCI.NE.0) THEN
CCC         WRITE(6,*) ' KSTOCI = ',KSTOCI,' IEQ = ',IEQ
            IF (KSTOCI.GT.IBLLIF) THEN
              CALL JELIBE(JEXNUM(MAT//'.VALI',IBLIC))
              IBLIC = IBLIC + 1
              IBLLIF = ZI(IDABLI+IBLIC)
              IF (KSTOCI.GT.IBLLIF) CALL UTMESS('F','ASLCHC_9',
     &                                   'ERREUR DE PROGRAMMATION')
              CALL JEEXIN(JEXNUM(MAT//'.VALI',IBLIC),IERI)
              IF (IERI.NE.0) THEN
                CALL JEVEUO(JEXNUM(MAT//'.VALI',IBLIC),'E',IDVALI)
              ELSE
                CALL JECROC(JEXNUM(MAT//'.VALI',IBLIC))
                CALL JEVEUO(JEXNUM(MAT//'.VALI',IBLIC),'E',IDVALI)
              END IF
            END IF
CCC     WRITE(6,*)'COPIE DE LA COLONNE : ',IEQ,' DANS IBLIC : ',IBLIC
CCC         CALL JEIMPA('MESSAGE',JEXNUM(MAT//'.VALI',IBLIC),'IBLIC')
            IVALI = ZI(IDALIG-1+KSTOCI)
            IF (TYPMAT.EQ.1) THEN
              DO 50 J = JDEB,IEQ
                ZR(IDVALI-1+IVALI+J-JDEB) = ZR(IDBLOC-1+IVALED+J-JDEB)
   50         CONTINUE
            ELSE
              DO 60 IVALE = IVALED,IVALEF
                ZC(IDVALI-1+IVALI) = ZC(IDBLOC-1+IVALE)
   60         CONTINUE
            END IF
          END IF
   70   CONTINUE
        CALL JELIBE(JEXNUM(MAT//'.VALI',IBLIC))

C---  SAUVEGARDE DES LIGNES A ELIMINEES DE LA TRIANGULEE INFERIEURE
C     ET MISE A ZERO
        DO 80 J = 1,IBLIC
          IF (JMIN.LE.ZI(IDLLIG+3*ZI(IDABLI+J)-3)) THEN
            IBLICD = J
            GO TO 90
          END IF
   80   CONTINUE
   90   CONTINUE
        DO 130 JBLIC = IBLICD,IBLIC
          ISTO1 = ZI(IDABLI+JBLIC-1) + 1
          ISTO2 = ZI(IDABLI+JBLIC)
          JDEB = ZI(IDLLIG+3*ISTO1-3)
          JFIN = ZI(IDLLIG+3*ISTO2-3)
          CALL JEVEUO(JEXNUM(MAT//'.VALI',JBLIC),'E',IDBLIC)
          DO 120 IEQ = IDEB,IFIN
            IHCOL = ZI(IDHCOL-1+IEQ)
            JEQDEB = MAX(IEQ-IHCOL+1,JDEB)
            JEQFIN = MIN(IEQ,JFIN)
            IF (TYPMAT.EQ.1) THEN
              DO 100 JEQ = JEQDEB,JEQFIN
                KSTOCJ = STOC(JEQ)
                IF (KSTOCJ.GT.0) THEN
                  JVALI = ZI(IDALIG-1+KSTOCJ) + ZI(IDHCOL-1+JEQ) + IEQ -
     &                    JEQ - 1
                  JVALE = ZI(IDADIA-1+IEQ) - IEQ + JEQ
                  ZR(IDBLIC-1+JVALI) = ZR(IDBLOC-1+JVALE)
                  ZR(IDBLOC-1+JVALE) = 0.D0
                END IF
  100         CONTINUE
            ELSE IF (TYPMAT.EQ.2) THEN
              DO 110 JEQ = JEQDEB,JEQFIN
                KSTOCJ = STOC(JEQ)
                IF (KSTOCJ.GT.0) THEN
                  JVALI = ZI(IDALIG-1+KSTOCJ) + ZI(IDHCOL-1+JEQ) + IEQ -
     &                    JEQ - 1
                  JVALE = ZI(IDADIA-1+IEQ) - IEQ + JEQ
                  ZC(IDBLIC-1+JVALI) = ZC(IDBLOC-1+JVALE)
                  ZC(IDBLOC-1+JVALE) = DCMPLX(0.D0,0.D0)
                END IF
  110         CONTINUE
            END IF
  120     CONTINUE
          CALL JELIBE(JEXNUM(MAT//'.VALI',JBLIC))
  130   CONTINUE

C---  MISE A ZERO DES COLONNES ELIMINEES DE LA TRIANGULEE SUPERIEURE

        DO 160 IEQ = IDEB,IFIN
          KSTOCI = STOC(IEQ)
          IVALEF = ZI(IDADIA-1+IEQ)
          JDEB = IEQ - ZI(IDHCOL-1+IEQ) + 1
          IF (KSTOCI.NE.0) THEN
            IF (TYPMAT.EQ.1) THEN
              DO 140 J = JDEB,IEQ - 1
                ZR(IDBLOC-1+IVALEF-IEQ+J) = 0.D0
  140         CONTINUE
              ZR(IDBLOC-1+IVALEF) = 1.D0
            ELSE
              DO 150 J = JDEB,IEQ - 1
                ZC(IDBLOC-1+IVALEF-IEQ+J) = DCMPLX(0.D0,0.D0)
  150         CONTINUE
              ZC(IDBLOC-1+IVALEF) = DCMPLX(1.D0,0.D0)
            END IF
          END IF
  160   CONTINUE
        CALL JELIBE(JEXNUM(MAT//'.VALE',IBLOC))
  170 CONTINUE
      CALL JELIBE(MAT//'.ALIG')
      CALL JELIBE(MAT//'.ABLI')
      CALL JELIBE(MAT//'.LLIG')
      CALL JELIBE(ZK24(IDREFE+2) (1:19)//'.HCOL')
  180 CONTINUE
      CALL JEDEMA()
      END
