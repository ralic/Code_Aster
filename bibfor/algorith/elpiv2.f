      SUBROUTINE ELPIV2(XJVMAX,NDIM  ,INDIC ,NBLIAC,AJLIAI,
     &                  SPLIAI,LLF   ,LLF1  ,LLF2  ,NOMA  ,
     &                  DEFICO,RESOCO)
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      INTEGER      NDIM
      INTEGER      INDIC
      INTEGER      NBLIAC,LLF,LLF1,LLF2
      INTEGER      AJLIAI,SPLIAI
      REAL*8       XJVMAX
      CHARACTER*8  NOMA
      CHARACTER*24 RESOCO,DEFICO
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - UTILITAIRE)
C
C ELIMINATION DES PIVOTS NULS DANS LA MATRICE DE CONTACT/FROTTEMENT
C
C ----------------------------------------------------------------------
C
C
C IN  XJVMAX : VALEUR DU PIVOT MAX
C IN  NDIM   : DIMENSION DU PROBLEME
C OUT INDIC  : +1 ON A RAJOUTE UNE LIAISON
C              -1 ON A ENLEVE UNE LIAISON
C I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
C I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON CORRECTE DU CALCUL
C              DE LA MATRICE DE CONTACT ACM1AT
C I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
C IN  NOMA   : NOM DU MAILLAGE
C I/O LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
C              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
C               DIRECTIONS SIMULTANEES (EN 3D)
C I/O LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               PREMIERE DIRECTION (EN 3D)
C I/O LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               SECONDE DIRECTION (EN 3D)
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C                'E': RESOCO(1:14)//'.LIAC'
C                'E': RESOCO(1:14)//'.LIOT'
C
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------
C
      CHARACTER*32 JEXNUM
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER       KK1,KK2,JVA,NIV,ILIAC,IFM
      INTEGER       NOTE2,NOTE1,NOTE12,NOTE,NBLIAI,LLF0
      INTEGER       PIVOT2,LLIAC,IBID,POSIT,CFDISD
      INTEGER       BTOTAL,DEKLAG,PIVOT,JSCDE,NBBLOC,JOUV
      REAL*8        COPMAX
      CHARACTER*1   TYPEAJ, TYPESP
      CHARACTER*2   TYPEC0, TYPEF0, TYPEF1, TYPEF2
      CHARACTER*19  LIAC,LIOT,MACONT,STOC,OUVERT
      INTEGER       JLIAC,JLIOT,JVALE,ISCIB,II,DERCOL,BLOC,JSCBL
C
C ----------------------------------------------------------------------
C
      CALL INFNIV(IFM,NIV)
      CALL JEMARQ()
C 
C --- LECTURE DES STRUCTURES DE DONNEES
C 
      LIAC   = RESOCO(1:14)//'.LIAC'
      LIOT   = RESOCO(1:14)//'.LIOT'
      MACONT = RESOCO(1:14)//'.MATC'
      STOC   = RESOCO(1:14)//'.SLCS'
      CALL JEVEUO(LIAC  ,'E',JLIAC )
      CALL JEVEUO(LIOT  ,'E',JLIOT )
      CALL JEVEUO(STOC//'.SCIB','L',ISCIB)
      CALL JEVEUO(STOC//'.SCBL','L',JSCBL)
      CALL JEVEUO(STOC//'.SCDE','L',JSCDE)
C 
C --- INITIALISATIONS
C 
      NBBLOC = ZI(JSCDE-1+3)
      NBLIAI = CFDISD(RESOCO,'NBLIAI')
      IBID   = 0
      PIVOT  = 0
      DEKLAG = 0
      TYPEAJ = 'A'
      TYPESP = 'S'
      TYPEC0 = 'C0'
      TYPEF0 = 'F0'
      TYPEF1 = 'F1'
      TYPEF2 = 'F2'
      OUVERT='&&ELPIV2.TRAV'
      CALL WKVECT (OUVERT,'V V L',NBBLOC,JOUV)
      COPMAX = XJVMAX * 1.0D-08
      IF (NDIM.EQ.3) THEN
         LLF0 = LLF
      ELSE
         LLF0 = 0
      ENDIF
C ======================================================================
C --- VERIFICATION DE LA PRESENCE OU NON D'UN PIVOT NUL
C --- SUR L'ENSEMBLE DES LIAISONS EN CONTACT ET ADHERENTES
C ======================================================================
      BTOTAL = NBLIAC + LLF + LLF1 + LLF2 - 1
      DO 90 KK1 = 0, BTOTAL
         ILIAC  = BTOTAL + 1 - KK1
         LLIAC  = ZI(JLIAC-1+ILIAC)
         CALL CFTYLI(RESOCO, ILIAC, POSIT)
         GOTO (1000, 2000, 3000, 4000) POSIT
 1000    CONTINUE
C ======================================================================
C --- ON SE TROUVE DANS LE CAS D'UNE LIAISON DE CONTACT
C ======================================================================
         II = ZI(ISCIB-1+ILIAC+LLF0-DEKLAG)
         DERCOL=ZI(JSCBL+II-1)
         BLOC=DERCOL*(DERCOL+1)/2
         IF (.NOT.ZL(JOUV-1+II)) THEN
            IF ((II.LT.NBBLOC).AND.(ILIAC.NE.(BTOTAL+1))) THEN
               CALL JELIBE(JEXNUM(MACONT//'.UALF',(II+1)))
               ZL(JOUV+II)=.FALSE.
            ENDIF
            CALL JEVEUO (JEXNUM(MACONT//'.UALF',II),'E',JVALE)
            ZL(JOUV-1+II)=.TRUE.
         ENDIF
         JVA=JVALE-1+(ILIAC+LLF0-DEKLAG-1)*(ILIAC+LLF0-DEKLAG)/2-BLOC
         DO 10 KK2 = 1, ILIAC + LLF0 - DEKLAG
            JVA = JVA + 1
            IF (ABS(ZR(JVA)).LT.COPMAX) THEN
               PIVOT = 1
            ELSE
C ======================================================================
C --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE
C ======================================================================
               PIVOT = 0
               GOTO 90
            END IF
 10      CONTINUE
C ======================================================================
C --- ON INCREMENTE LE VECTEUR DES LIAISONS OTEES LIOT
C ======================================================================
         ZI(JLIOT+4*NBLIAI) = ZI(JLIOT+4*NBLIAI) + 1
         NOTE = ZI(JLIOT+4*NBLIAI)
         ZI(JLIOT-1+NOTE) = LLIAC
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE CONTACT
C ======================================================================
         CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     &               RESOCO,TYPESP,ILIAC,LLIAC,TYPEC0)
         CALL CFIMP2(DEFICO,RESOCO,NOMA  ,LLIAC,TYPEC0,
     &               'PIV' )
         GOTO 100
 2000    CONTINUE
C ======================================================================
C --- ON SE TROUVE DANS LE CAS D'UNE LIAISON DE FROTTEMENT ADHERENT
C --- (DANS LE CAS GENERAL EN 2D/SUIVANT LES DEUX DIRECTIONS EN 3D)
C ======================================================================
         IF (NDIM.EQ.3) THEN
            DEKLAG = DEKLAG + 1
            II = ZI(ISCIB-1+ILIAC+LLF0-DEKLAG)
            DERCOL=ZI(JSCBL+II-1)
            BLOC=DERCOL*(DERCOL+1)/2
            IF (.NOT.ZL(JOUV-1+II)) THEN
               IF ((II.LT.NBBLOC).AND.(ILIAC.NE.(BTOTAL+1))) THEN
                  CALL JELIBE(JEXNUM(MACONT//'.UALF',(II+1)))
                  ZL(JOUV+II)=.FALSE.
               ENDIF
               CALL JEVEUO (JEXNUM(MACONT//'.UALF',II),'E',JVALE)
               ZL(JOUV-1+II)=.TRUE.
            ENDIF
            JVA=JVALE-1+(ILIAC+LLF0-DEKLAG-1)*(ILIAC+LLF0-DEKLAG)/2-BLOC
            DO 20 KK2 = 1, ILIAC + LLF0 - DEKLAG
               JVA = JVA + 1
               IF (ABS(ZR(JVA)).LT.COPMAX) THEN
                  PIVOT = 1
               ELSE
C ======================================================================
C --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE
C ======================================================================
                  PIVOT = 0
               ENDIF
 20         CONTINUE
            DO 30 KK2 = 1, ILIAC + LLF0 - DEKLAG + 1
               JVA = JVA + 1
               IF (ABS(ZR(JVA)).LT.COPMAX) THEN
                  PIVOT2 = 1
               ELSE
C ======================================================================
C --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE
C ======================================================================
                  PIVOT2 = 0
               END IF
 30         CONTINUE
            IF (PIVOT.EQ.0) THEN
               IF (PIVOT2.EQ.0) THEN
C ======================================================================
C --- PAS D'ELIMINATION DE PIVOT
C ======================================================================
                  GOTO 90
               ELSE
C ======================================================================
C --- ELIMINATION DU PIVOT NUL SUIVANT LA SECONDE DIRECTION
C ======================================================================
                  ZI(JLIOT+4*NBLIAI+3) = ZI(JLIOT+4*NBLIAI+3) + 1
                  NOTE2 = ZI(JLIOT+4*NBLIAI+3)
                  ZI(JLIOT-1+NOTE2+3*NBLIAI) = LLIAC
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT
C --- ON SUPPRIME LA DOUBLE (F0), ON AJOUTE LA F1 -> LA F2 EST SUPPRIMEE
C ======================================================================
                  CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     &                                 RESOCO,TYPESP,ILIAC,LLIAC,TYPEF0)
                  POSIT = NBLIAC + LLF + LLF1 + LLF2 + 1
                  CALL CFTABL(IBID,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     &                                 RESOCO,TYPEAJ,POSIT,LLIAC,TYPEF1)
                  CALL CFIMP2(DEFICO,RESOCO,NOMA  ,LLIAC,TYPEF2,
     &                        'PIV' )
                  GOTO 100
               ENDIF
            ELSE
               IF (PIVOT2.EQ.0) THEN
C ======================================================================
C --- ELIMINATION DU PIVOT NUL SUIVANT LA SECONDE DIRECTION
C ======================================================================
                  ZI(JLIOT+4*NBLIAI+2) = ZI(JLIOT+4*NBLIAI+2) + 1
                  NOTE1 = ZI(JLIOT+4*NBLIAI+2)
                  ZI(JLIOT-1+NOTE1+2*NBLIAI) = LLIAC
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT ----------------
C ======================================================================
                  CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     +                                 RESOCO,TYPESP,ILIAC,LLIAC,TYPEF0)
                  POSIT = NBLIAC + LLF + LLF1 + LLF2 + 1
                  CALL CFTABL(IBID,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     +                                 RESOCO,TYPEAJ,POSIT,LLIAC,TYPEF2)
                  CALL CFIMP2(DEFICO,RESOCO,NOMA  ,LLIAC,TYPEF1,
     &                        'PIV' )
                  GOTO 100
               ELSE
C ======================================================================
C --- ELIMINATION DU PIVOT NUL SUIVANT LES DEUX DIRECTIONS
C ======================================================================
                  ZI(JLIOT+4*NBLIAI+1) = ZI(JLIOT+4*NBLIAI+1) + 1
                  NOTE12 = ZI(JLIOT+4*NBLIAI+1)
                  ZI(JLIOT-1+NOTE12+NBLIAI) = LLIAC
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT
C ======================================================================
                  CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     +                         RESOCO,TYPESP,ILIAC,LLIAC,TYPEF0)
                  GOTO 100
               ENDIF
            ENDIF
         ELSE
            II = ZI(ISCIB-1+ILIAC+LLF0-DEKLAG)
            DERCOL=ZI(JSCBL+II-1)
            BLOC=DERCOL*(DERCOL+1)/2
            IF (.NOT.ZL(JOUV-1+II)) THEN
               IF ((II.LT.NBBLOC).AND.(ILIAC.NE.(BTOTAL+1))) THEN
                  CALL JELIBE(JEXNUM(MACONT//'.UALF',(II+1)))
                  ZL(JOUV+II)=.FALSE.
               ENDIF
               CALL JEVEUO (JEXNUM(MACONT//'.UALF',II),'E',JVALE)
               ZL(JOUV-1+II)=.TRUE.
            ENDIF
            JVA=JVALE-1+(ILIAC+LLF0-DEKLAG-1)*(ILIAC+LLF0-DEKLAG)/2-BLOC
            DO 40 KK2 = 1, ILIAC + LLF0 - DEKLAG
               JVA = JVA + 1
               IF (ABS(ZR(JVA)).LT.COPMAX) THEN
                  PIVOT = 1
               ELSE
C ======================================================================
C --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE
C ======================================================================
                  PIVOT = 0
                  GOTO 90
               ENDIF
 40         CONTINUE
            ZI(JLIOT+4*NBLIAI+1) = ZI(JLIOT+4*NBLIAI+1) + 1
            NOTE12 = ZI(JLIOT+4*NBLIAI+1)
            ZI(JLIOT-1+NOTE12+NBLIAI) = LLIAC
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT
C ======================================================================
            CALL CFTABL(INDIC, NBLIAC, AJLIAI, SPLIAI, LLF, LLF1, LLF2,
     +                             RESOCO, TYPESP, ILIAC, LLIAC, TYPEF0)
            CALL CFIMP2(DEFICO,RESOCO,NOMA  ,LLIAC,'F3'  ,
     &                  'PIV' )
            GOTO 100
         ENDIF
 3000    CONTINUE
C ======================================================================
C --- ON SE TROUVE DANS LE CAS D'UNE LIAISON DE FROTTEMENT ADHERENT
C --- SUIVANT LA PREMIERE DIRECTION EN 3D
C ======================================================================
         II = ZI(ISCIB-1+ILIAC+LLF0-DEKLAG)
         DERCOL=ZI(JSCBL+II-1)
         BLOC=DERCOL*(DERCOL+1)/2
         IF (.NOT.ZL(JOUV-1+II)) THEN
            IF ((II.LT.NBBLOC).AND.(ILIAC.NE.(BTOTAL+1))) THEN
               CALL JELIBE(JEXNUM(MACONT//'.UALF',(II+1)))
               ZL(JOUV+II)=.FALSE.
            ENDIF
            CALL JEVEUO (JEXNUM(MACONT//'.UALF',II),'E',JVALE)
            ZL(JOUV-1+II)=.TRUE.
         ENDIF
         JVA=JVALE-1+(ILIAC+LLF0-DEKLAG-1)*(ILIAC+LLF0-DEKLAG)/2-BLOC
         DO 50 KK2 = 1, ILIAC + LLF0 - DEKLAG
            JVA = JVA + 1
            IF (ABS(ZR(JVA)).LT.COPMAX) THEN
               PIVOT = 1
            ELSE
C ======================================================================
C --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE
C ======================================================================
               PIVOT = 0
               GOTO 90
            ENDIF
 50      CONTINUE
         ZI(JLIOT+4*NBLIAI+1) = ZI(JLIOT+4*NBLIAI+1) + 1
         NOTE12 = ZI(JLIOT+4*NBLIAI+1)
         ZI(JLIOT-1+NOTE12+NBLIAI) = LLIAC
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT
C ======================================================================
         CALL CFTABL(INDIC, NBLIAC, AJLIAI, SPLIAI, LLF, LLF1, LLF2,
     +               RESOCO, TYPESP, ILIAC, LLIAC, TYPEF1)
         CALL CFIMP2(DEFICO,RESOCO,NOMA  ,LLIAC,TYPEF1,
     &               'PIV' )
         GOTO 100
 4000    CONTINUE
C ======================================================================
C --- ON SE TROUVE DANS LE CAS D'UNE LIAISON DE FROTTEMENT ADHERENT
C --- SUIVANT LA SECONDE DIRECTION EN 3D
C ======================================================================
         II = ZI(ISCIB-1+ILIAC+LLF0-DEKLAG)
         DERCOL=ZI(JSCBL+II-1)
         BLOC=DERCOL*(DERCOL+1)/2
         IF (.NOT.ZL(JOUV-1+II)) THEN
            IF ((II.LT.NBBLOC).AND.(ILIAC.NE.(BTOTAL+1))) THEN
               CALL JELIBE(JEXNUM(MACONT//'.UALF',(II+1)))
               ZL(JOUV+II)=.FALSE.
            ENDIF
            CALL JEVEUO (JEXNUM(MACONT//'.UALF',II),'E',JVALE)
            ZL(JOUV-1+II)=.TRUE.
         ENDIF
         JVA=JVALE-1+(ILIAC+LLF0-DEKLAG-1)*(ILIAC+LLF0-DEKLAG)/2-BLOC
         DO 60 KK2 = 1, ILIAC + LLF0 - DEKLAG
            JVA = JVA + 1
            IF (ABS(ZR(JVA)).LT.COPMAX) THEN
               PIVOT = 1
            ELSE
C ======================================================================
C --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE
C ======================================================================
               PIVOT = 0
               GOTO 90
            ENDIF
 60      CONTINUE
         ZI(JLIOT+4*NBLIAI+1) = ZI(JLIOT+4*NBLIAI+1) + 1
         NOTE12 = ZI(JLIOT+4*NBLIAI+1)
         ZI(JLIOT-1+NOTE12+NBLIAI) = LLIAC
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT
C ======================================================================
         CALL CFTABL(INDIC, NBLIAC, AJLIAI, SPLIAI, LLF, LLF1, LLF2,
     +               RESOCO, TYPESP, ILIAC, LLIAC, TYPEF2)
         CALL CFIMP2(DEFICO,RESOCO,NOMA  ,LLIAC,TYPEF2,
     &               'PIV' )
         GOTO 100
C ======================================================================
 90   CONTINUE
C ======================================================================
 100  CONTINUE
C ======================================================================
      CALL JEDETR(OUVERT)
      CALL JEDEMA()
C ======================================================================
      END
