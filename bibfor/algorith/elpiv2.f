      SUBROUTINE ELPIV2(XJVMAX, NDIM, INDIC, NBLIAC, AJLIAI, SPLIAI,
     +                    LLF, LLF1, LLF2, NOMA, DEFICO, RESOCO)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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
      IMPLICIT      NONE
      INTEGER       NDIM, INDIC, NBLIAC, AJLIAI, SPLIAI, LLF, LLF1, LLF2
      REAL*8        XJVMAX
      CHARACTER*8   NOMA
      CHARACTER*24  RESOCO, DEFICO
C ======================================================================
C     BUT         : ELIMINATION DES PIVOTS NULS
C     APPELEE PAR : FRO2GD, FROLGD
C ======================================================================
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------
C ======================================================================
      CHARACTER*32 JEXNUM,JEXNOM
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
C ======================================================================
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C ======================================================================
      INTEGER       JVALE,KK1,KK2,KK1F,KK2F,JVA,NIV,ILIAC,JLIAC,IFM,KK
      INTEGER       NOTE2,NOTE1,NOTE12,NOTE,JLIOT,NBLIAI,JAPPAR,LLF0
      INTEGER       PIVOT2,ICONTA,II,POS1,NUM1,JNOCO,LLIAC,IBID,POSIT
      INTEGER       BTOTAL,DEKLAG,PIVOT
      REAL*8        COPMAX
      CHARACTER*1   TYPEAJ, TYPESP
      CHARACTER*2   TYPEC0, TYPEF0, TYPEF1, TYPEF2
      CHARACTER*8   NOM1
      CHARACTER*19  LIAC, LIOT, MATR
      CHARACTER*24  APPARI, CONTNO
C ======================================================================
      CALL INFNIV(IFM,NIV)
      CALL JEMARQ()
C ======================================================================
C --- LECTURE DES STRUCTURES DE DONNEES --------------------------------
C ======================================================================
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      APPARI = RESOCO(1:14)//'.APPARI'
      LIAC   = RESOCO(1:14)//'.LIAC'
      LIOT   = RESOCO(1:14)//'.LIOT'
      MATR   = RESOCO(1:14)//'.MATR'
C ======================================================================
      CALL JEVEUO(CONTNO,'L',JNOCO )
      CALL JEVEUO(APPARI,'L',JAPPAR)
      CALL JEVEUO(LIAC  ,'E',JLIAC )
      CALL JEVEUO(LIOT  ,'E',JLIOT )
      CALL JEVEUO(JEXNUM(MATR//'.VALE',1),'L',JVALE)
C ======================================================================
C --- FIN DE LECTURE DES STRUCTURES DE DONNEES -------------------------
C ======================================================================
C --- INITIALISATION ---------------------------------------------------
C ======================================================================
      NBLIAI = ZI(JAPPAR )
      IBID   = 0
      PIVOT  = 0
      DEKLAG = 0
      TYPEAJ = 'A'
      TYPESP = 'S'
      TYPEC0 = 'C0'
      TYPEF0 = 'F0'
      TYPEF1 = 'F1'
      TYPEF2 = 'F2'
      COPMAX = XJVMAX * 1.0D-08
      IF (NDIM.EQ.3) THEN
         LLF0 = LLF
      ELSE
         LLF0 = 0
      ENDIF
C ======================================================================
C --- VERIFICATION DE LA PRESENCE OU NON D'UN PIVOT NUL ----------------
C --- SUR L'ENSEMBLE DES LIAISONS EN CONTACT ET ADHERENTES -------------
C ======================================================================
      BTOTAL = NBLIAC + LLF + LLF1 + LLF2 - 1
      DO 90 KK1 = 0, BTOTAL
         ILIAC  = BTOTAL + 1 - KK1
         LLIAC  = ZI(JLIAC-1+ILIAC)
         CALL CFTYLI(RESOCO, ILIAC, POSIT)
         GOTO (1000, 2000, 3000, 4000) POSIT
 1000    CONTINUE
C ======================================================================
C --- ON SE TROUVE DANS LE CAS D'UNE LIAISON DE CONTACT ----------------
C ======================================================================
         JVA = JVALE-1 + (ILIAC+LLF0-DEKLAG-1)*(ILIAC+LLF0-DEKLAG)/2
         DO 10 KK2 = 1, ILIAC + LLF0 - DEKLAG
            JVA = JVA + 1
            IF (ABS(ZR(JVA)).LT.COPMAX) THEN
               PIVOT = 1
            ELSE
C ======================================================================
C --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE ----------
C ======================================================================
               PIVOT = 0
               GOTO 90
            END IF
 10      CONTINUE
C ======================================================================
C --- ON INCREMENTE LE VECTEUR DES LIAISONS OTEES LIOT ET ON PREPARE ---
C --- L'IMPRESSION -----------------------------------------------------
C ======================================================================
         POS1 = ZI(JAPPAR+3*(LLIAC-1)+1)
         NUM1 = ZI(JNOCO+ABS(POS1)-1)
         CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
         WRITE (IFM,9000) 'CONTACT : NOEUD ',NOM1
         IF (NIV.EQ.2) THEN
            WRITE (IFM,9001) 'CONTACT : LIAISON ',LLIAC
         END IF
         ZI(JLIOT+4*NBLIAI) = ZI(JLIOT+4*NBLIAI) + 1
         NOTE = ZI(JLIOT+4*NBLIAI)
         ZI(JLIOT-1+NOTE) = LLIAC
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE CONTACT -------------------
C ======================================================================
         CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     +                                 RESOCO,TYPESP,ILIAC,LLIAC,TYPEC0)
         GOTO 100
 2000    CONTINUE
C ======================================================================
C --- ON SE TROUVE DANS LE CAS D'UNE LIAISON DE FROTTEMENT ADHERENT ----
C --- (DANS LE CAS GENERAL EN 2D/SUIVANT LES DEUX DIRECTIONS EN 3D) ----
C ======================================================================
         IF (NDIM.EQ.3) THEN
            DEKLAG = DEKLAG + 1
            JVA = JVALE-1 + (ILIAC+LLF0-DEKLAG-1)*(ILIAC+LLF0-DEKLAG)/2
            DO 20 KK2 = 1, ILIAC + LLF0 - DEKLAG
               JVA = JVA + 1
               IF (ABS(ZR(JVA)).LT.COPMAX) THEN
                  PIVOT = 1
               ELSE
C ======================================================================
C --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE ----------
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
C --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE ----------
C ======================================================================
                  PIVOT2 = 0
               END IF
 30         CONTINUE
            IF (PIVOT.EQ.0) THEN
               IF (PIVOT2.EQ.0) THEN
C ======================================================================
C --- PAS D'ELIMINATION DE PIVOT ---------------------------------------
C ======================================================================
                  GOTO 90
               ELSE
C ======================================================================
C --- ELIMINATION DU PIVOT NUL SUIVANT LA PREMIERE DIRECTION -----------
C ======================================================================
                  POS1 = ZI(JAPPAR+3*(LLIAC-1)+1)
                  NUM1 = ZI(JNOCO+ABS(POS1)-1)
                  CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
                 WRITE (IFM,9100) 'FROTTEMENT DIRECTION 2 : NOEUD ',NOM1
                  IF (NIV.EQ.2) THEN
              WRITE (IFM,9101) 'FROTTEMENT DIRECTION 2 : LIAISON ',LLIAC
                  END IF
                  ZI(JLIOT+4*NBLIAI+3) = ZI(JLIOT+4*NBLIAI+3) + 1
                  NOTE2 = ZI(JLIOT+4*NBLIAI+3)
                  ZI(JLIOT-1+NOTE2+3*NBLIAI) = LLIAC
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT ----------------
C ======================================================================
                  CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     +                                 RESOCO,TYPESP,ILIAC,LLIAC,TYPEF0)
                  POSIT = NBLIAC + LLF + LLF1 + LLF2 + 1
                  CALL CFTABL(IBID,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     +                                 RESOCO,TYPEAJ,POSIT,LLIAC,TYPEF1)
                  GOTO 100
               ENDIF
            ELSE
               IF (PIVOT2.EQ.0) THEN
C ======================================================================
C --- ELIMINATION DU PIVOT NUL SUIVANT LA PREMIERE DIRECTION -----------
C ======================================================================
                  POS1 = ZI(JAPPAR+3*(LLIAC-1)+1)
                  NUM1 = ZI(JNOCO+ABS(POS1)-1)
                  CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
                 WRITE (IFM,9100) 'FROTTEMENT DIRECTION 1 : NOEUD ',NOM1
                  IF (NIV.EQ.2) THEN
              WRITE (IFM,9101) 'FROTTEMENT DIRECTION 1 : LIAISON ',LLIAC
                  END IF
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
                  GOTO 100
               ELSE
C ======================================================================
C --- ELIMINATION DU PIVOT NUL SUIVANT LES DEUX DIRECTIONS -------------
C ======================================================================
                  POS1 = ZI(JAPPAR+3*(LLIAC-1)+1)
                  NUM1 = ZI(JNOCO+ABS(POS1)-1)
                  CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
               WRITE (IFM,9110) 'FROTTEMENT DIRECTIONS 1 ET 2 : NOEUD ',
     &                                                              NOM1
                  IF (NIV.EQ.2) THEN
             WRITE (IFM,9111) 'FROTTEMENT DIRECTIONS 1 ET 2 : LIAISON ',
     &                                                             LLIAC
                  END IF
                  ZI(JLIOT+4*NBLIAI+1) = ZI(JLIOT+4*NBLIAI+1) + 1
                  NOTE12 = ZI(JLIOT+4*NBLIAI+1)
                  ZI(JLIOT-1+NOTE12+NBLIAI) = LLIAC
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT ----------------
C ======================================================================
                  CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     +                                 RESOCO,TYPESP,ILIAC,LLIAC,TYPEF0)
                  GOTO 100
               ENDIF
            ENDIF
         ELSE
            JVA = JVALE-1 + (ILIAC+LLF0-DEKLAG-1)*(ILIAC+LLF0-DEKLAG)/2
            DO 40 KK2 = 1, ILIAC + LLF0 - DEKLAG
               JVA = JVA + 1
               IF (ABS(ZR(JVA)).LT.COPMAX) THEN
                  PIVOT = 1
               ELSE
C ======================================================================
C --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE ----------
C ======================================================================
                  PIVOT = 0
                  GOTO 90
               ENDIF
 40         CONTINUE
            POS1 = ZI(JAPPAR+3*(LLIAC-1)+1)
            NUM1 = ZI(JNOCO+ABS(POS1)-1)
            CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
            WRITE (IFM,9010) 'FROTTEMENT : NOEUD ', NOM1
            IF (NIV.EQ.2) THEN
               WRITE (IFM,9011) 'FROTTEMENT : LIAISON ', LLIAC
            END IF
            ZI(JLIOT+4*NBLIAI+1) = ZI(JLIOT+4*NBLIAI+1) + 1
            NOTE12 = ZI(JLIOT+4*NBLIAI+1)
            ZI(JLIOT-1+NOTE12+NBLIAI) = LLIAC
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT ----------------
C ======================================================================
            CALL CFTABL(INDIC, NBLIAC, AJLIAI, SPLIAI, LLF, LLF1, LLF2,
     +                             RESOCO, TYPESP, ILIAC, LLIAC, TYPEF0)
            GOTO 100
         ENDIF
 3000    CONTINUE
C ======================================================================
C --- ON SE TROUVE DANS LE CAS D'UNE LIAISON DE FROTTEMENT ADHERENT ----
C --- SUIVANT LA PREMIERE DIRECTION EN 3D ------------------------------
C ======================================================================
         JVA = JVALE-1 + (ILIAC+LLF0-DEKLAG-1)*(ILIAC+LLF0-DEKLAG)/2
         DO 50 KK2 = 1, ILIAC + LLF0 - DEKLAG
            JVA = JVA + 1
            IF (ABS(ZR(JVA)).LT.COPMAX) THEN
               PIVOT = 1
            ELSE
C ======================================================================
C --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE ----------
C ======================================================================
               PIVOT = 0
               GOTO 90
            ENDIF
 50      CONTINUE
         POS1 = ZI(JAPPAR+3*(LLIAC-1)+1)
         NUM1 = ZI(JNOCO+ABS(POS1)-1)
         CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
         WRITE (IFM,9100) 'FROTTEMENT DIRECTION 1 : NOEUD ', NOM1
         IF (NIV.EQ.2) THEN
            WRITE (IFM,9101) 'FROTTEMENT DIRECTION 1 : LIAISON ', LLIAC
         END IF
         ZI(JLIOT+4*NBLIAI+1) = ZI(JLIOT+4*NBLIAI+1) + 1
         NOTE12 = ZI(JLIOT+4*NBLIAI+1)
         ZI(JLIOT-1+NOTE12+NBLIAI) = LLIAC
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT ----------------
C ======================================================================
         CALL CFTABL(INDIC, NBLIAC, AJLIAI, SPLIAI, LLF, LLF1, LLF2,
     +                             RESOCO, TYPESP, ILIAC, LLIAC, TYPEF1)
         GOTO 100
 4000    CONTINUE
C ======================================================================
C --- ON SE TROUVE DANS LE CAS D'UNE LIAISON DE FROTTEMENT ADHERENT ----
C --- SUIVANT LA SECONDE DIRECTION EN 3D -------------------------------
C ======================================================================
         JVA = JVALE-1 + (ILIAC+LLF0-DEKLAG-1)*(ILIAC+LLF0-DEKLAG)/2
         DO 60 KK2 = 1, ILIAC + LLF0 - DEKLAG
            JVA = JVA + 1
            IF (ABS(ZR(JVA)).LT.COPMAX) THEN
               PIVOT = 1
            ELSE
C ======================================================================
C --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE ----------
C ======================================================================
               PIVOT = 0
               GOTO 90
            ENDIF
 60      CONTINUE
         POS1 = ZI(JAPPAR+3*(LLIAC-1)+1)
         NUM1 = ZI(JNOCO+ABS(POS1)-1)
         CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
         WRITE (IFM,9100) 'FROTTEMENT DIRECTION 2 : NOEUD ', NOM1
         IF (NIV.EQ.2) THEN
            WRITE (IFM,9101) 'FROTTEMENT DIRECTION 2 : LIAISON ', LLIAC
         END IF
         ZI(JLIOT+4*NBLIAI+1) = ZI(JLIOT+4*NBLIAI+1) + 1
         NOTE12 = ZI(JLIOT+4*NBLIAI+1)
         ZI(JLIOT-1+NOTE12+NBLIAI) = LLIAC
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT ----------------
C ======================================================================
         CALL CFTABL(INDIC, NBLIAC, AJLIAI, SPLIAI, LLF, LLF1, LLF2,
     +                             RESOCO, TYPESP, ILIAC, LLIAC, TYPEF2)
         GOTO 100
C ======================================================================
 90   CONTINUE
C ======================================================================
 100  CONTINUE
C ======================================================================
      CALL JEDEMA()
C ======================================================================
 9000 FORMAT (' PIVOT NUL OTE ',A16,A8)
 9001 FORMAT (' PIVOT NUL OTE ',A18,I5)
 9010 FORMAT (' PIVOT NUL OTE ',A19,A8)
 9011 FORMAT (' PIVOT NUL OTE ',A21,I5)
 9100 FORMAT (' PIVOT NUL OTE ',A31,A8)
 9101 FORMAT (' PIVOT NUL OTE ',A33,I5)
 9110 FORMAT (' PIVOT NUL OTE ',A37,A8)
 9111 FORMAT (' PIVOT NUL OTE ',A39,I5)
C ======================================================================
      END
