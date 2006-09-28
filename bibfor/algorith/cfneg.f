      SUBROUTINE CFNEG(RESOCO,DEFICO,NOMA,NDIM,
     &                 INDIC,NBLIAI,NBLIAC,AJLIAI,SPLIAI,
     &                 LLF,LLF1,LLF2)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT     NONE
      CHARACTER*24 RESOCO
      CHARACTER*24 DEFICO
      CHARACTER*8  NOMA
      INTEGER      NDIM
      INTEGER      INDIC
      INTEGER      NBLIAC
      INTEGER      AJLIAI
      INTEGER      SPLIAI
      INTEGER      LLF
      INTEGER      LLF1
      INTEGER      LLF2
      INTEGER      NBLIAI
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : ALGOCL/FRO2GD/FROLGD/FROPGD
C ----------------------------------------------------------------------
C
C  VERIFICATION QUE LES MULTIPLICATEURS DE LAGRANGE SONT A VALEURS
C   POSITIVES (PRESSION DE CONTACT POSITIVE)
C
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C                'E': RESOCO(1:14)//'.LIAC'
C                'E': RESOCO(1:14)//'.CONVEC'
C                'E': RESOCO(1:14)//'.MU'
C IN  NOMA   : NOM DU MAILLAGE
C IN  NDIM   : DIMENSION DU PROBLEME
C OUT INDIC  :+1 ON A RAJOUTE UNE LIAISON
C             -1 ON A ENLEVE UNE LIAISON
C IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
C I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
C I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON CORRECTE DU CALCUL
C              DE LA MATRICE DE CONTACT ACM1AT
C I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
C I/O LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
C              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
C               DIRECTIONS SIMULTANEES (EN 3D)
C I/O LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               PREMIERE DIRECTION (EN 3D)
C I/O LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               SECONDE DIRECTION (EN 3D)
C
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
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      IFM,NIV
      INTEGER      DEKLAG, DEKLN, DEKLF0, DEKLF1
      INTEGER      DEKLF2, POSIT, JSPNBL, JSPLF0, JSPLF1, JSPLF2
      INTEGER      JJ, KK, LL, MM, ILIAC, LLIAC, LLJAC, JSPLIA
      INTEGER      COMPT0, COMPT1, COMPT2, NBINI, IDEBUT, IFIN, JSTO
      REAL*8       LAMBDA
      CHARACTER*1  TYPESP
      CHARACTER*2  TYPEC0, TYPEF0, TYPEF1, TYPEF2
      CHARACTER*19 LIAC,MU,CONVEC
      INTEGER      JLIAC,JMU,JVECC
      CHARACTER*24 APPARI,CONTNO,CONTMA
      INTEGER      JAPPAR,JNOCO,JMACO
C
C ======================================================================
C
      CALL INFNIV(IFM,NIV)
      CALL JEMARQ()
C
      IF (NBLIAC.EQ.0) THEN
        GOTO 999
      ENDIF
C ======================================================================
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C ======================================================================
      APPARI = RESOCO(1:14)//'.APPARI'
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      CONTMA = DEFICO(1:16)//'.MAILCO'
      LIAC   = RESOCO(1:14)//'.LIAC'
      CONVEC = RESOCO(1:14)//'.CONVEC'
      MU     = RESOCO(1:14)//'.MU'
C ======================================================================
      CALL JEVEUO(CONTNO,'L',JNOCO)
      CALL JEVEUO(CONTMA,'L',JMACO)
      CALL JEVEUO(APPARI,'L',JAPPAR)
      CALL JEVEUO(LIAC,  'E',JLIAC )
      CALL JEVEUO(CONVEC,'E',JVECC)
      CALL JEVEUO(MU,    'E',JMU   )
C ======================================================================
C --- INITIALISATION DES VARIABLES
C ======================================================================
      DEKLAG = 0
      DEKLN  = 0
      DEKLF0 = 0
      DEKLF1 = 0
      DEKLF2 = 0
      TYPESP = 'S'
      TYPEC0 = 'C0'
      TYPEF0 = 'F0'
      TYPEF1 = 'F1'
      TYPEF2 = 'F2'
C ======================================================================
C --- CREATION D'OBJETS TEMPORAIRES
C ======================================================================
      CALL WKVECT ('&&CPNOTE.SUPNBL','V V I',NBLIAC ,JSPNBL)
      CALL WKVECT ('&&CPNOTE.SPLIAC','V V I',NBLIAC ,JSPLIA)
      IF (LLF.NE.0) THEN
         CALL WKVECT ('&&CPNOTE.SUPLF0','V V I',LLF ,JSPLF0)
      ENDIF
      IF (LLF1.NE.0) THEN
         CALL WKVECT ('&&CPNOTE.SUPLF1','V V I',LLF1,JSPLF1)
      ENDIF
      IF (LLF2.NE.0) THEN
         CALL WKVECT ('&&CPNOTE.SUPLF2','V V I',LLF2,JSPLF2)
      ENDIF
C ======================================================================
C --- LES VALEURS DU VECTEUR SUPNBL SONT NECESSAIREMENT CROISSANTES
C --- ATTENTION CE N'EST PAS NECESSAIREMENT LE CAS DU VECTEUR SUPLLF
C ======================================================================
      NBINI  = 1
      DO 10 ILIAC = 1, NBLIAC
         LAMBDA = ZR(JMU-1+ILIAC)
         IF ( LAMBDA.LT.0.0D0 ) THEN
            DEKLN = DEKLN + 1
            DO 20 JJ = NBINI, NBLIAC + LLF + LLF1 + LLF2
               IF (ZK8(JVECC-1+JJ).EQ.TYPEC0) THEN
                  DEKLAG = DEKLAG + 1
                  IF (DEKLAG.EQ.ILIAC) THEN
                     ZI(JSPNBL-1+DEKLN) = ILIAC
                     ZI(JSPLIA-1+DEKLN) = JJ
                     NBINI  = JJ + 1
                     LLIAC  = ZI(JLIAC-1+JJ)
                     COMPT0 = 0
                     COMPT1 = 0
                     COMPT2 = 0
                     DO 30 KK = 1, NBLIAC + LLF + LLF1 + LLF2
                        LLJAC = ZI(JLIAC-1+KK)
                        IF (ZK8(JVECC-1+KK).EQ.TYPEC0) THEN
                           GOTO 30
                        ELSE IF (ZK8(JVECC-1+KK).EQ.TYPEF0) THEN
                           COMPT0 = COMPT0 + 1
                           POSIT  = 2
                        ELSE IF (ZK8(JVECC-1+KK).EQ.TYPEF1) THEN
                           COMPT1 = COMPT1 + 1
                           POSIT  = 3
                        ELSE IF (ZK8(JVECC-1+KK).EQ.TYPEF2) THEN
                           COMPT2 = COMPT2 + 1
                           POSIT = 4
                        ENDIF
                        IF (LLJAC.EQ.LLIAC) THEN
                           GOTO (1000, 2000, 3000, 4000) POSIT
C ======================================================================
C --- CAS IMPOSSIBLE
C ======================================================================
 1000                      CONTINUE
                           CALL U2MESS('F','ALGORITH_92')
C ======================================================================
C --- CAS DU FROTTEMENT ADHERENT SUIVANT LES DEUX DIRECTIONS EN 3D
C --- OU CAS GENERAL EN 2D
C ======================================================================
 2000                      CONTINUE
                           DEKLF0 = DEKLF0 + 1
                           ZI(JSPLF0-1+DEKLF0) = COMPT0
                           GOTO 10
C ======================================================================
C --- CAS DU FROTTEMENT ADHERENT SUIVANT LA PREMIERE DIRECTION
C ======================================================================
 3000                      CONTINUE
                           DO 3100 LL = 0, DEKLF1-1
                              IF (COMPT1.GT.ZI(JSPLF1-1+DEKLF1-LL)) THEN
                                 DEKLF1 = DEKLF1 + 1
                                 DO 3200 MM = DEKLF1 - LL, DEKLF1
                                    ZI(JSPLF1-1+MM) = ZI(JSPLF1-1+MM-1)
 3200                            CONTINUE
                                 ZI(JSPLF1-1+DEKLF1-LL-1) = COMPT1
                                 GOTO 10
                              ENDIF
 3100                      CONTINUE
                           DEKLF1 = DEKLF1 + 1
                           ZI(JSPLF1-1+DEKLF1) = COMPT1
                           GOTO 10
C ======================================================================
C --- CAS DU FROTTEMENT ADHERENT SUIVANT LA SECONDE DIRECTION
C ======================================================================
 4000                      CONTINUE
                           DO 4100 LL = 0, DEKLF2-1
                              IF (COMPT2.GT.ZI(JSPLF2-1+DEKLF2-LL)) THEN
                                 DEKLF2 = DEKLF2 + 1
                                 DO 4200 MM = DEKLF2 - LL, DEKLF2
                                    ZI(JSPLF2-1+MM) = ZI(JSPLF2-1+MM-1)
 4200                            CONTINUE
                                 ZI(JSPLF2-1+DEKLF2-LL-1) = COMPT2
                                 GOTO 10
                              ENDIF
 4100                      CONTINUE
                           DEKLF2 = DEKLF2 + 1
                           ZI(JSPLF2-1+DEKLF2) = COMPT2
                           GOTO 10
C ======================================================================
                        ENDIF
 30                  CONTINUE
                     GOTO 10
                  ENDIF
               ENDIF
 20         CONTINUE
C            CALL UTMESS ('F','CPNOTE_2','CAS IMPOSSIBLE')
         ENDIF
 10   CONTINUE
      IF (DEKLN.EQ.0) THEN
         GOTO 999
      ENDIF
C ======================================================================
C --- MISE A JOUR DE MU POUR LE CONTACT ET DU VECTEUR DES LIAISONS
C --- DE CONTACT ET DE FROTTEMENT ADHERENT
C ======================================================================
      JSTO = ZI(JSPNBL) - 1
      DO 100 ILIAC = 1, DEKLN-1
         IDEBUT = JSTO + 1
         IFIN   = IDEBUT+ZI(JSPNBL-1+ILIAC+1)-ZI(JSPNBL-1+ILIAC)-1-1
         DO 110 JJ = IDEBUT, IFIN
            JSTO = JSTO + 1
            ZR(JMU-1+JSTO) = ZR(JMU-1+JJ+ILIAC)
 110     CONTINUE
 100  CONTINUE
      IDEBUT = JSTO + 1
      IFIN   = NBLIAC - DEKLN
      DO 120 JJ = IDEBUT, IFIN
         JSTO = JSTO + 1
         ZR(JMU-1+JSTO) = ZR(JMU-1+JJ+DEKLN)
 120  CONTINUE
      DO 111 JJ = 1, DEKLN
         ILIAC = DEKLN - JJ + 1
         POSIT = ZI(JSPLIA-1+ILIAC)
         LLIAC = ZI(JLIAC -1+POSIT)
         ZR(JMU+3*NBLIAI-1+LLIAC) = 0.0D0
         CALL CFTABL( INDIC, NBLIAC, AJLIAI, SPLIAI, LLF,
     &                 LLF1, LLF2, RESOCO, TYPESP, POSIT, LLIAC, TYPEC0)
         IF (NIV.GE.2) THEN
            CALL CFIMP2(IFM,NOMA,LLIAC,TYPEC0,TYPESP,'NEG',LAMBDA,
     &                  JAPPAR,JNOCO,JMACO)
         ENDIF
 111  CONTINUE
C ======================================================================
C --- MISE A JOUR DE MU POUR LE FROTTEMENT
C ======================================================================
C --- FROTTEMENT ADHERENT DE TYPE LLF
C ======================================================================
      IF (LLF.NE.0) THEN
         IF (DEKLF0.NE.0) THEN
            DO 200 ILIAC = 1, ZI(JSPLF0-1+1) - 1
               JSTO = JSTO + 1
               ZR(JMU-1+JSTO) = ZR(JMU-1+NBLIAC+DEKLN+ILIAC)
 200        CONTINUE
            DO 210 ILIAC = 1, DEKLF0 - 1
               IDEBUT = JSTO + 1
               IFIN   = IDEBUT + ZI(JSPLF0-1+ILIAC+1) -
     &                                       ZI(JSPLF0-1+ILIAC) - 1 - 1
               DO 220 JJ = IDEBUT, IFIN
                  JSTO = JSTO + 1
                  ZR(JMU-1+JSTO) = ZR(JMU-1+DEKLN+JJ+ILIAC)
 220           CONTINUE
 210        CONTINUE
         ENDIF
         IDEBUT = JSTO + 1
         IFIN   = NBLIAC + LLF
         DO 230 JJ = IDEBUT, IFIN
            JSTO = JSTO + 1
            ZR(JMU-1+JSTO) = ZR(JMU-1+DEKLN+DEKLF0+JJ)
 230     CONTINUE
C ======================================================================
C --- CAS DE LA SECONDE DIRECTION EN 3D
C ======================================================================
         IF (NDIM.EQ.3) THEN
            IF (DEKLF0.NE.0) THEN
               DO 240 ILIAC = 1, ZI(JSPLF0-1+1) - 1
                  JSTO = JSTO + 1
                  ZR(JMU-1+JSTO) =
     &                           ZR(JMU-1+NBLIAC+DEKLN+LLF+DEKLF0+ILIAC)
 240           CONTINUE
               DO 250 ILIAC = 1, DEKLF0 - 1
                  IDEBUT = JSTO + 1
                  IFIN   = IDEBUT + ZI(JSPLF0-1+ILIAC+1) -
     &                                        ZI(JSPLF0-1+ILIAC) - 1 - 1
                  DO 260 JJ = IDEBUT, IFIN
                     JSTO = JSTO + 1
                     ZR(JMU-1+JSTO) = ZR(JMU-1+DEKLN+DEKLF0+JJ)
 260              CONTINUE
 250           CONTINUE
            ENDIF
            IDEBUT = JSTO + 1
            IFIN   = NBLIAC + (NDIM-1)*LLF
            DO 270 JJ = IDEBUT, IFIN
               JSTO = JSTO + 1
               ZR(JMU-1+JSTO) = ZR(JMU-1+DEKLN+(NDIM-1)*DEKLF0+JJ)
 270        CONTINUE
         ENDIF
      ENDIF
C ======================================================================
C --- FROTTEMENT ADHERENT DE TYPE LLF1
C ======================================================================
      IF (LLF1.NE.0) THEN
         IF (DEKLF1.NE.0) THEN
            DO 300 ILIAC = 1, ZI(JSPLF1-1+1) - 1
               JSTO = JSTO + 1
               ZR(JMU-1+JSTO) =
     &                ZR(JMU-1+NBLIAC+DEKLN+(NDIM-1)*(LLF+DEKLF0)+ILIAC)
 300        CONTINUE
            DO 310 ILIAC = 1, DEKLF1 - 1
               IDEBUT = ZI(JSPLF1-1+ILIAC  )
               IFIN   = ZI(JSPLF1-1+ILIAC+1) - 1
               DO 320 JJ = IDEBUT, IFIN
                  JSTO = JSTO + 1
                  ZR(JMU-1+JSTO) = ZR(JMU-1+DEKLN+(NDIM-1)*DEKLF0+JJ)
 320           CONTINUE
 310        CONTINUE
         ENDIF
         IDEBUT = JSTO + 1
         IFIN   = NBLIAC + (NDIM-1)*LLF + LLF1
         DO 330 JJ = IDEBUT, IFIN
            JSTO  = JSTO + 1
            ZR(JMU-1+JSTO) = ZR(JMU-1+DEKLN+(NDIM-1)*DEKLF0+DEKLF1+JJ)
 330     CONTINUE
      ENDIF
C ======================================================================
C --- FROTTEMENT ADHERENT DE TYPE LLF2
C ======================================================================
      IF (LLF2.NE.0) THEN
         IF (DEKLF2.NE.0) THEN
            DO 400 ILIAC = 1, ZI(JSPLF2-1+1) - 1
               JSTO = JSTO + 1
               ZR(JMU-1+JSTO) =
     &    ZR(JMU-1+NBLIAC+DEKLN+(NDIM-1)*(LLF+DEKLF0)+LLF1+DEKLF1+ILIAC)
 400        CONTINUE
            DO 410 ILIAC = 1, DEKLF2 - 1
               IDEBUT = ZI(JSPLF2-1+ILIAC  )
               IFIN   = ZI(JSPLF2-1+ILIAC+1) - 1
               DO 420 JJ = IDEBUT, IFIN
                  JSTO = JSTO + 1
                  ZR(JMU-1+JSTO) =
     &                         ZR(JMU-1+DEKLN+(NDIM-1)*DEKLF0+DEKLF1+JJ)
 420           CONTINUE
 410        CONTINUE
         ENDIF
         IDEBUT = JSTO + 1
         IFIN   = NBLIAC + (NDIM-1)*LLF + LLF1 + LLF2
         DO 430 JJ = IDEBUT, IFIN
            JSTO = JSTO + 1
            ZR(JMU-1+JSTO) =
     &                  ZR(JMU-1+DEKLN+(NDIM-1)*DEKLF0+DEKLF1+DEKLF2+JJ)
 430     CONTINUE
      ENDIF
C ======================================================================
 999  CONTINUE
C ======================================================================
      CALL JEDETC('V','&&CPNOTE',1)
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
