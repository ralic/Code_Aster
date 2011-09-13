      SUBROUTINE CFACA2(NDIM  ,NBLIAC,SPLIAI,LLF   ,LLF1  ,
     &                  LLF2  ,INDFAC,NESMAX,RESOCO,LMAT  , 
     &                  NBLIAI,XJVMAX)
C 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/09/2011   AUTEUR ABBAS M.ABBAS 
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
C TOLE CRP_20
C
      IMPLICIT     NONE
      INTEGER      NBLIAI,NBLIAC,LLF,LLF1,LLF2      
      INTEGER      SPLIAI,INDFAC
      INTEGER      NDIM  ,NESMAX
      INTEGER      LMAT
      REAL*8       XJVMAX
      CHARACTER*24 RESOCO
C        
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION - A.C-1.AT)
C
C CALCUL DE -A.C-1.AT (REDUITE AUX LIAISONS ACTIVES)
C STOCKAGE DE LA MOITIE UNIQUEMENT (PROBLEME SYMETRIQUE)
C
C ----------------------------------------------------------------------
C
C
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  RESOCO : SD DE RESOLUTION DU CONTACT
C IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
C IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT POSSIBLES
C IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
C I/O XJVMAX : VALEUR DU PIVOT MAX
C I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
C IN  NESMAX : NOMBRE MAX DE NOEUDS ESCLAVES
C              (SERT A DECALER LES POINTEURS POUR LE FROTTEMENT 3D)
C IN  LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
C              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
C               DIRECTIONS SIMULTANEES (EN 3D)
C IN  LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               PREMIERE DIRECTION (EN 3D)
C IN  LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               SECONDE DIRECTION (EN 3D)
C I/O INDFAC : INDICE DE DEBUT DE LA FACTORISATION
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*32       JEXNUM
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER       JDECAL
      INTEGER       NBDDL, JVA, JVALE, DEKLAG, NEQ, POSIT
      INTEGER       ILIAC, JJ, LLIAC, LLJAC, II, DERCOL, BLOC
      INTEGER       NBBLOC
      REAL*8        VAL
      CHARACTER*2   TYPEF0
      CHARACTER*19  LIAC  ,CM1A  ,TYPL
      INTEGER       JLIAC ,JCM1A ,JTYPL
      CHARACTER*19  STOC
      INTEGER       JSCBL,JSCIB,JSCDE
      CHARACTER*19  OUVERT,MACONT
      INTEGER       JOUV
      CHARACTER*24  APPOIN,APDDL ,APCOEF,APCOFR
      INTEGER       JAPPTR,JAPDDL,JAPCOE,JAPCOF
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C 
C --- RECUPERATION D'OBJETS JEVEUX
C 
      CM1A   = RESOCO(1:14)//'.CM1A'
      APPOIN = RESOCO(1:14)//'.APPOIN'
      APDDL  = RESOCO(1:14)//'.APDDL'
      LIAC   = RESOCO(1:14)//'.LIAC'
      APCOEF = RESOCO(1:14)//'.APCOEF'
      APCOFR = RESOCO(1:14)//'.APCOFR'
      MACONT = RESOCO(1:14)//'.MATC'
      TYPL   = RESOCO(1:14)//'.TYPL'
      STOC   = RESOCO(1:14)//'.SLCS'
C
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(APDDL, 'L',JAPDDL)
      CALL JEVEUO(LIAC,  'L',JLIAC )
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      IF (LLF+LLF1+LLF2.NE.0) THEN
        CALL JEVEUO(APCOFR,'L',JAPCOF)
      ENDIF
      CALL JEVEUO(TYPL,'L',JTYPL )
      CALL JEVEUO(STOC//'.SCIB','L',JSCIB)
      CALL JEVEUO(STOC//'.SCBL','L',JSCBL)
      CALL JEVEUO(STOC//'.SCDE','L',JSCDE)
C 
C --- INITIALISATIONS 
C 
      TYPEF0 = 'F0'
      NEQ    = ZI(LMAT+2)
      DEKLAG = 0
      NBBLOC = ZI(JSCDE-1+3)
      OUVERT = '&CFACA2.TRAV'
C
      CALL WKVECT (OUVERT,'V V L',NBBLOC,JOUV)
      IF (NDIM.EQ.3) THEN
         DO 100 ILIAC = 1, SPLIAI
            IF ( ZK8(JTYPL-1+ILIAC).EQ.TYPEF0 ) THEN
               DEKLAG = DEKLAG + 1
            ENDIF
 100     CONTINUE
      ENDIF
C ======================================================================
C --- CALCUL DE -A.C-1.AT (REDUITE AUX LIAISONS ACTIVES) ---------------
C --- (STOCKAGE DE LA MOITIE PAR SYMETRIE) -----------------------------
C ======================================================================
      INDFAC = MIN(INDFAC, SPLIAI+DEKLAG+1)
      DO 210 ILIAC = SPLIAI+1, NBLIAC + LLF + LLF1 + LLF2
         LLIAC  = ZI(JLIAC-1+ILIAC)
         CALL CFTYLI(RESOCO, ILIAC, POSIT)
         GOTO (1000, 2000, 4000, 5000) POSIT
C ======================================================================
C --- AJOUT D'UNE LIAISON DE CONTACT -----------------------------------
C ======================================================================
 1000    CONTINUE
         CALL JEVEUO(JEXNUM(CM1A,LLIAC),'L',JCM1A)
         II = ZI(JSCIB-1+ILIAC+DEKLAG)
         DERCOL=ZI(JSCBL+II-1)
         BLOC=DERCOL*(DERCOL+1)/2
         IF (.NOT.ZL(JOUV-1+II)) THEN
            IF (II.GT.1) THEN
               CALL JELIBE(JEXNUM(MACONT//'.UALF',(II-1)))
               ZL(JOUV-2+II)=.FALSE.
            ENDIF
            CALL JEVEUO (JEXNUM(MACONT//'.UALF',II),'E',JVALE)
            ZL(JOUV-1+II)=.TRUE.
         ENDIF
         JVA = JVALE-1 + (ILIAC+DEKLAG-1)*(ILIAC+DEKLAG)/2-BLOC
         DO 10 JJ = 1, ILIAC
            LLJAC   = ZI(JLIAC-1+JJ)
            JDECAL  = ZI(JAPPTR+LLJAC-1)
            NBDDL   = ZI(JAPPTR+LLJAC) - ZI(JAPPTR+LLJAC-1)
            JVA     = JVA + 1
            ZR(JVA) = 0.0D0
            CALL CFTYLI(RESOCO, JJ, POSIT)
            GOTO (1100, 1200, 1300, 1400) POSIT
C ======================================================================
C --- LIAISON DE CONTACT -----------------------------------------------
C ======================================================================
 1100       CONTINUE
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
            ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
            GOTO 10
C ======================================================================
C --- LIAISON DE FROTTEMENT --------------------------------------------
C ======================================================================
 1200       CONTINUE
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
            ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
C ======================================================================
C --- DANS LE CAS 3D ---------------------------------------------------
C ======================================================================
            IF (NDIM.EQ.3) THEN
               JVA = JVA + 1
               ZR(JVA) = 0.0D0
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
               ZR(JVA) = ZR(JVA) - VAL
               IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
            ENDIF
            GOTO 10
C ======================================================================
C --- LIAISON DE FROTTEMENT SUIVANT LA PREMIERE DIRECTION --------------
C ======================================================================
 1300       CONTINUE
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
            ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
            GOTO 10
C ======================================================================
C --- LIAISON DE FROTTEMENT SUIVANT LA SECONDE DIRECTION ---------------
C ======================================================================
 1400       CONTINUE
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
C ======================================================================
            ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
 10      CONTINUE
         CALL JELIBE(JEXNUM(CM1A,LLIAC))
         GOTO 210
C ======================================================================
C --- AJOUT D'UNE LIAISON DE FROTTEMENT --------------------------------
C ======================================================================
 2000    CONTINUE
         CALL JEVEUO(JEXNUM(CM1A,LLIAC+NBLIAI),'L',JCM1A)
         II = ZI(JSCIB-1+ILIAC+DEKLAG)
         DERCOL=ZI(JSCBL+II-1)
         BLOC=DERCOL*(DERCOL+1)/2
         IF (.NOT.ZL(JOUV-1+II)) THEN
            IF (II.GT.1) THEN
               CALL JELIBE(JEXNUM(MACONT//'.UALF',(II-1)))
               ZL(JOUV-2+II)=.FALSE.
            ENDIF
            CALL JEVEUO (JEXNUM(MACONT//'.UALF',II),'E',JVALE)
            ZL(JOUV-1+II)=.TRUE.
         ENDIF
         JVA = JVALE-1 + (ILIAC+DEKLAG-1)*(ILIAC+DEKLAG)/2-BLOC
         DO 20 JJ = 1, ILIAC - 1
            LLJAC   = ZI(JLIAC-1+JJ)
            JDECAL  = ZI(JAPPTR+LLJAC-1)
            NBDDL   = ZI(JAPPTR+LLJAC) - ZI(JAPPTR+LLJAC-1)
            JVA     = JVA + 1
            ZR(JVA) = 0.0D0
            CALL CFTYLI(RESOCO, JJ, POSIT)
            GOTO (2100, 2200, 2300, 2400) POSIT
C ======================================================================
C --- LIAISON DE CONTACT -----------------------------------------------
C ======================================================================
 2100       CONTINUE
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
            ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
            GOTO 20
C ======================================================================
C --- LIAISON DE FROTTEMENT --------------------------------------------
C ======================================================================
 2200       CONTINUE
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
            ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
C ======================================================================
C --- DANS LE CAS 3D ---------------------------------------------------
C ======================================================================
            IF (NDIM.EQ.3) THEN
               JVA = JVA + 1
               ZR(JVA) = 0.0D0
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
               ZR(JVA) = ZR(JVA) - VAL
               IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
            ENDIF
            GOTO 20
C ======================================================================
C --- LIAISON DE FROTTEMENT SUIVANT LA PREMIERE DIRECTION --------------
C ======================================================================
 2300       CONTINUE
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
            ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
            GOTO 20
C ======================================================================
C --- LIAISON DE FROTTEMENT SUIVANT LA SECONDE DIRECTION ---------------
C ======================================================================
 2400       CONTINUE
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
C ======================================================================
            ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
 20      CONTINUE
         LLJAC   = ZI(JLIAC-1+ILIAC)
         JDECAL  = ZI(JAPPTR+LLJAC-1)
         NBDDL   = ZI(JAPPTR+LLJAC) - ZI(JAPPTR+LLJAC-1)
         JVA     = JVA + 1
         ZR(JVA) = 0.0D0
         CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
         ZR(JVA) = ZR(JVA) - VAL
         IF (ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
         CALL JELIBE(JEXNUM(CM1A,LLIAC+NBLIAI))
         IF (NDIM.EQ.3) THEN
C ======================================================================
C --- DANS LE CAS 3D ---------------------------------------------------
C ======================================================================
            CALL JEVEUO(JEXNUM(CM1A,LLIAC+(NDIM-1)*NBLIAI),'L',JCM1A)
            DEKLAG = DEKLAG + 1
            II = ZI(JSCIB-1+ILIAC+DEKLAG)
            DERCOL=ZI(JSCBL+II-1)
            BLOC=DERCOL*(DERCOL+1)/2
            IF (.NOT.ZL(JOUV-1+II)) THEN
               IF (II.GT.1) THEN
                  CALL JELIBE(JEXNUM(MACONT//'.UALF',(II-1)))
                  ZL(JOUV-2+II)=.FALSE.
               ENDIF
               CALL JEVEUO (JEXNUM(MACONT//'.UALF',II),'E',JVALE)
               ZL(JOUV-1+II)=.TRUE.
            ENDIF
            JVA = JVALE-1 + (ILIAC+DEKLAG-1)*(ILIAC+DEKLAG)/2-BLOC
            DO 30 JJ = 1, ILIAC
               LLJAC   = ZI(JLIAC-1+JJ)
               JDECAL  = ZI(JAPPTR+LLJAC-1)
               NBDDL   = ZI(JAPPTR+LLJAC) - ZI(JAPPTR+LLJAC-1)
               JVA     = JVA + 1
               ZR(JVA) = 0.0D0
               CALL CFTYLI(RESOCO, JJ, POSIT)
               GOTO (3100, 3200, 3300, 3400) POSIT
C ======================================================================
C --- LIAISON DE CONTACT -----------------------------------------------
C ======================================================================
 3100          CONTINUE
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
               ZR(JVA) = ZR(JVA) - VAL
               IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
               GOTO 30
C ======================================================================
C --- LIAISON DE FROTTEMENT --------------------------------------------
C ======================================================================
 3200          CONTINUE
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
               ZR(JVA) = ZR(JVA) - VAL
               IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
C ======================================================================
C --- DANS LE CAS 3D ---------------------------------------------------
C ======================================================================
               JVA = JVA + 1
               ZR(JVA) = 0.0D0
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
               ZR(JVA) = ZR(JVA) - VAL
               IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
               GOTO 30
C ======================================================================
C --- LIAISON DE FROTTEMENT SUIVANT LA PREMIERE DIRECTION --------------
C ======================================================================
 3300          CONTINUE
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
               ZR(JVA) = ZR(JVA) - VAL
               IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
               GOTO 30
C ======================================================================
C --- LIAISON DE FROTTEMENT SUIVANT LA SECONDE DIRECTION ---------------
C ======================================================================
 3400          CONTINUE
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
C ======================================================================
               ZR(JVA) = ZR(JVA) - VAL
               IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
 30         CONTINUE
C            DEKLAG = DEKLAG + 1
            CALL JELIBE(JEXNUM(CM1A,LLIAC+(NDIM-1)*NBLIAI))
         ENDIF
         GOTO 210
C ======================================================================
C --- AJOUT D'UNE LIAISON DE FROTTEMENT SUIVANT LA PREMIERE ------------
C --- DIRECTION UNIQUEMENT ---------------------------------------------
C ======================================================================
 4000    CONTINUE
         CALL JEVEUO(JEXNUM(CM1A,LLIAC+NBLIAI),'L',JCM1A)
         II = ZI(JSCIB-1+ILIAC+DEKLAG)
         DERCOL=ZI(JSCBL+II-1)
         BLOC=DERCOL*(DERCOL+1)/2
         IF (.NOT.ZL(JOUV-1+II)) THEN
            IF (II.GT.1) THEN
               CALL JELIBE(JEXNUM(MACONT//'.UALF',(II-1)))
               ZL(JOUV-2+II)=.FALSE.
            ENDIF
            CALL JEVEUO (JEXNUM(MACONT//'.UALF',II),'E',JVALE)
            ZL(JOUV-1+II)=.TRUE.
         ENDIF
         JVA = JVALE-1 + (ILIAC+DEKLAG-1)*(ILIAC+DEKLAG)/2-BLOC
         DO 40 JJ = 1, ILIAC
            LLJAC   = ZI(JLIAC-1+JJ)
            JDECAL  = ZI(JAPPTR+LLJAC-1)
            NBDDL   = ZI(JAPPTR+LLJAC) - ZI(JAPPTR+LLJAC-1)
            JVA     = JVA + 1
            ZR(JVA) = 0.0D0
            CALL CFTYLI(RESOCO, JJ, POSIT)
            GOTO (4100, 4200, 4300, 4400) POSIT
C ======================================================================
C --- LIAISON DE CONTACT -----------------------------------------------
C ======================================================================
 4100       CONTINUE
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
            ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
            GOTO 40
C ======================================================================
C --- LIAISON DE FROTTEMENT --------------------------------------------
C ======================================================================
 4200       CONTINUE
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
            ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
C ======================================================================
C --- DANS LE CAS 3D ---------------------------------------------------
C ======================================================================
            IF (NDIM.EQ.3) THEN
               JVA = JVA + 1
               ZR(JVA) = 0.0D0
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
               ZR(JVA) = ZR(JVA) - VAL
               IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
            ENDIF
            GOTO 40
C ======================================================================
C --- LIAISON DE FROTTEMENT SUIVANT LA PREMIERE DIRECTION --------------
C ======================================================================
 4300       CONTINUE
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
            ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
            GOTO 40
C ======================================================================
C --- LIAISON DE FROTTEMENT SUIVANT LA SECONDE DIRECTION ---------------
C ======================================================================
 4400       CONTINUE
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
C ======================================================================
           ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
 40      CONTINUE
         CALL JELIBE(JEXNUM(CM1A,LLIAC+NBLIAI))
         GOTO 210
C ======================================================================
C --- AJOUT D'UNE LIAISON DE CONTACT -----------------------------------
C ======================================================================
 5000    CONTINUE
         CALL JEVEUO(JEXNUM(CM1A,LLIAC+(NDIM-1)*NBLIAI),'L',JCM1A)
         II = ZI(JSCIB-1+ILIAC+DEKLAG)
         DERCOL=ZI(JSCBL+II-1)
         BLOC=DERCOL*(DERCOL+1)/2
         IF (.NOT.ZL(JOUV-1+II)) THEN
            IF (II.GT.1) THEN
               CALL JELIBE(JEXNUM(MACONT//'.UALF',(II-1)))
               ZL(JOUV-2+II)=.FALSE.
            ENDIF
            CALL JEVEUO (JEXNUM(MACONT//'.UALF',II),'E',JVALE)
            ZL(JOUV-1+II)=.TRUE.
         ENDIF
         JVA = JVALE-1 + (ILIAC+DEKLAG-1)*(ILIAC+DEKLAG)/2-BLOC
         DO 50 JJ = 1, ILIAC
            LLJAC   = ZI(JLIAC-1+JJ)
            JDECAL  = ZI(JAPPTR+LLJAC-1)
            NBDDL   = ZI(JAPPTR+LLJAC) - ZI(JAPPTR+LLJAC-1)
            JVA     = JVA + 1
            ZR(JVA) = 0.0D0
            CALL CFTYLI(RESOCO, JJ, POSIT)
            GOTO (5100, 5200, 5300, 5400) POSIT
C ======================================================================
C --- LIAISON DE CONTACT -----------------------------------------------
C ======================================================================
 5100       CONTINUE
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
            ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
            GOTO 50
C ======================================================================
C --- LIAISON DE FROTTEMENT --------------------------------------------
C ======================================================================
 5200       CONTINUE
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
            ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
C ======================================================================
C --- DANS LE CAS 3D ---------------------------------------------------
C ======================================================================
            IF (NDIM.EQ.3) THEN
               JVA = JVA + 1
               ZR(JVA) = 0.0D0
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
               ZR(JVA) = ZR(JVA) - VAL
               IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
            ENDIF
            GOTO 50
C ======================================================================
C --- LIAISON DE FROTTEMENT SUIVANT LA PREMIERE DIRECTION --------------
C ======================================================================
 5300       CONTINUE
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
            ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
            GOTO 50
C ======================================================================
C --- LIAISON DE FROTTEMENT SUIVANT LA SECONDE DIRECTION ---------------
C ======================================================================
 5400       CONTINUE
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                                  ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
C ======================================================================
            ZR(JVA) = ZR(JVA) - VAL
            IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
 50      CONTINUE
         CALL JELIBE(JEXNUM(CM1A,LLIAC+(NDIM-1)*NBLIAI))
 210  CONTINUE

C ======================================================================
      SPLIAI = NBLIAC + LLF + LLF1 + LLF2
      CALL JEDETR (OUVERT)
      CALL JEDEMA ()
C ======================================================================

C     -- ON CREE L'OBJET .VALM DE LA MATRICE "MORSE" :
C     CALL UALFVA(MACONT,'V')


      END
