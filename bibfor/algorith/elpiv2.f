      SUBROUTINE ELPIV2(XJVMAX,MATR,NOMA,DEFICO,RESOCO,IDEBUT,LLF,
     &                  LLF1,LLF2,NBLIAC,NDIM,KKMIN,PIVOT,INDIC)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/10/2002   AUTEUR CIBHHBC R.FERNANDES 
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
      INTEGER IDEBUT,LLF,LLF1,LLF2,NBLIAC,INDIC,PIVOT,NDIM,KKMIN
      REAL*8 XJVMAX
      CHARACTER*8 NOMA
      CHARACTER*19 MATR
      CHARACTER*24 RESOCO,DEFICO
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
      INTEGER JVALE,KK1,KK2,KK1F,KK2F,JVA,NIV,ILIAC,JLIAC,IFM,KK
      INTEGER NOTE2,NOTE1,NOTE12,NOTE,JLIOT,NBLIAI,JAPPAR
      INTEGER PIVOT2,ICONTA,II,POS1,NUM1,JNOCO,LLIAC
      CHARACTER*8  NOM1
      CHARACTER*19 LIAC,LIOT
      CHARACTER*24 APPARI,CONTNO
C ======================================================================
      CALL INFNIV(IFM,NIV)
      CALL JEMARQ()
C ======================================================================
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      CALL JEVEUO(CONTNO,'L',JNOCO)
      APPARI = RESOCO(1:14)//'.APPARI'
      CALL JEEXIN(APPARI,ICONTA)
      IF (ICONTA.NE.0) THEN
        CALL JEVEUO(APPARI,'L',JAPPAR)
        NBLIAI = ZI(JAPPAR)
      END IF
C ======================================================================
      LIAC = RESOCO(1:14)//'.LIAC'
      LIOT = RESOCO(1:14)//'.LIOT'
C ======================================================================
      CALL JEVEUO(LIAC,'E',JLIAC)
      CALL JEVEUO(LIOT,'E',JLIOT)
      CALL JEVEUO(JEXNUM(MATR//'.VALE',1),'L',JVALE)
      PIVOT = 0
      XJVMAX = XJVMAX*1.D-08
C ======================================================================
C --- VERIFICATION DE LA PRESENCE OU NON D'UN PIVOT NUL ----------------
C --- SUR L'ENSEMBLE DES LIAISONS EN CONTACT ET ADHERENTES -------------
C ======================================================================
      DO 90 KK1 = 1,NBLIAC + LLF* (NDIM-1) + LLF1 + LLF2
        DO 10 KK2 = 1,KK1
          JVA = JVALE - 1 + (KK1-1)*KK1/2 + KK2
          IF (ABS(ZR(JVA)).LT.XJVMAX) THEN
            PIVOT = 1
          ELSE
C ======================================================================
C --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE ----------
C ======================================================================
            PIVOT = 0
            GO TO 90
          END IF
   10   CONTINUE
C ======================================================================
C --- PIVOT NUL A OTER -------------------------------------------------
C ======================================================================
        IF (KK1.LE.NBLIAC) THEN
C ======================================================================
C --- CAS D'UNE LIAISON DE CONTACT -------------------------------------
C ======================================================================
          LLIAC = ZI(JLIAC-1+KK1)
          POS1 = ZI(JAPPAR+3* (LLIAC-1)+1)
          NUM1 = ZI(JNOCO+ABS(POS1)-1)
          CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
          WRITE (IFM,1000) 'CONTACT : NOEUD ',NOM1
C          IF (NIV.EQ.2) THEN
            WRITE (IFM,1010) 'CONTACT:LIAISON ',LLIAC
C          END IF
          ZI(JLIOT+4*NBLIAI) = ZI(JLIOT+4*NBLIAI) + 1
          NOTE = ZI(JLIOT+4*NBLIAI)
          ZI(JLIOT-1+NOTE) = ZI(JLIAC-1+KK1)
C ======================================================================
C --- MISE A JOUR DU NOMBRE DE LIAISONS DE CONTACT ---------------------
C ======================================================================
          NBLIAC = NBLIAC - 1
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE CONTACT -------------------
C ======================================================================
          DO 20 ILIAC = KK1,NBLIAC + LLF + LLF1 + LLF2
             ZI(JLIAC-1+ILIAC) = ZI(JLIAC+ILIAC)
   20     CONTINUE
C ======================================================================
C --- PRISE EN COMPTE DE LA SUPPRESSION DE LA LIAISON DE CONTACT -------
C --- AU NIVEAU DU FROTTEMENT ADHERENT ---------------------------------
C ======================================================================
C --- FROTTEMENT ADHERENT SUIVANT LES DEUX DIRECTIONS ------------------
C ======================================================================
          DO 40 ILIAC = NBLIAC + 1,NBLIAC + LLF
             IF (ZI(JLIOT-1+NOTE).EQ.ZI(JLIAC-1+ILIAC)) THEN
                LLF = LLF - 1
                ZI(JLIOT+4*NBLIAI+1) = ZI(JLIOT+4*NBLIAI+1) + 1
                NOTE12 = ZI(JLIOT+4*NBLIAI+1)
                ZI(JLIOT-1+NOTE12+NBLIAI) = ZI(JLIAC-1+ILIAC)
                DO 30 II = ILIAC,NBLIAC + LLF + LLF1 + LLF2
                   ZI(JLIAC-1+II) = ZI(JLIAC+II)
   30           CONTINUE
             END IF
   40     CONTINUE
C ======================================================================
C --- FROTTEMENT ADHERENT SUIVANT LA PREMIERE DIRECTION ----------------
C ======================================================================
          DO 60 ILIAC = NBLIAC + LLF + 1,NBLIAC + LLF + LLF1
             IF (ZI(JLIOT-1+NOTE).EQ.ZI(JLIAC-1+ILIAC)) THEN
                LLF1 = LLF1 - 1
                DO 50 II = ILIAC,NBLIAC + LLF + LLF1 + LLF2
                   ZI(JLIAC-1+II) = ZI(JLIAC+II)
   50           CONTINUE
             END IF
   60     CONTINUE
C ======================================================================
C --- FROTTEMENT ADHERENT SUIVANT LA SECONDE DIRECTION -----------------
C ======================================================================
          DO 80 ILIAC = NBLIAC+LLF+LLF1+1, NBLIAC+LLF+LLF1+LLF2
             IF (ZI(JLIOT-1+NOTE).EQ.ZI(JLIAC-1+ILIAC)) THEN
                LLF2 = LLF2 - 1
                DO 70 II = ILIAC,NBLIAC + LLF + LLF1 + LLF2
                   ZI(JLIAC-1+II) = ZI(JLIAC+II)
 70             CONTINUE
             END IF
 80       CONTINUE
C ======================================================================
C --- VOIR UTILITE DE KKMIN ET DE INDIC EN DIMENSION 3 -----------------
C --- PAS D'UTILITE EN DIMENSION 2 -------------------------------------
C ======================================================================
          KKMIN = KK1
          INDIC = -1
          GO TO 100
C ======================================================================
        ELSE IF ( KK1 .LE. (NBLIAC+LLF) ) THEN
C ======================================================================
C --- CAS D'UNE LIAISON ADHERENTE SUIVANT LA PREMIERE DIRECTION --------
C ======================================================================
C --- ON VERIFIE SI LE PIVOT NUL EN DIRECTION 1 EST AUSSI --------------
C --- UN PIVOT NUL EN DIRECTION 2 --------------------------------------
C ======================================================================
          KK = KK1 + LLF
          PIVOT2 = 0
          DO 91 KK2 = 1, KK
             JVA = JVALE - 1 + ( KK - 1 ) * KK /2 + KK2
             IF (ABS(ZR(JVA)).LT.XJVMAX) THEN
                PIVOT2 = 1
             ELSE
                PIVOT2 = 0
                GO TO 92
             END IF
 91       CONTINUE
 92       CONTINUE
C ======================================================================
C --- MISE A JOUR DU NOMBRE DE LIAISONS ADHERENTES ---------------------
C ======================================================================
          LLF = LLF - 1
C ======================================================================
          IF (PIVOT2.EQ.1) THEN
C ======================================================================
C --- ELIMINATION DU PIVOT NUL SUIVANT LES DEUX DIRECTIONS -------------
C ======================================================================
             LLIAC = ZI(JLIAC-1+KK1)
             POS1 = ZI(JAPPAR+3* (LLIAC-1)+1)
             NUM1 = ZI(JNOCO+ABS(POS1)-1)
             CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
             WRITE (IFM,1100) 'FROTTEMENT DIRECTIONS 1 ET 2 : NOEUD ',
     &        NOM1
            IF (NIV.EQ.2) THEN
              WRITE (IFM,1110) 'FROTTEMENT DIRECTIONS 1 ET 2:LIAISON ',
     &          LLIAC
            END IF
            ZI(JLIOT+4*NBLIAI+1) = ZI(JLIOT+4*NBLIAI+1) + 1
            NOTE12 = ZI(JLIOT+4*NBLIAI+1)
            ZI(JLIOT-1+NOTE12+NBLIAI) = LLIAC
            DO 94 ILIAC = KK1,NBLIAC + LLF + LLF1 + LLF2
               ZI(JLIAC-1+ILIAC) = ZI(JLIAC+ILIAC)
 94         CONTINUE
          ELSE
C ======================================================================
C --- ELIMINATION DU PIVOT NUL SUIVANT LA PREMIERE DIRECTION -----------
C ======================================================================
            LLIAC = ZI(JLIAC-1+KK1)
            POS1 = ZI(JAPPAR+3* (LLIAC-1)+1)
            NUM1 = ZI(JNOCO+ABS(POS1)-1)
            CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
            WRITE (IFM,1100) 'FROTTEMENT DIRECTION 1 : NOEUD ',NOM1
            IF (NIV.EQ.2) THEN
              WRITE (IFM,1110) 'FROTTEMENT DIRECTION 1:LIAISON ',LLIAC
            END IF
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE CONTACT -------------------
C ======================================================================
            ZI(JLIOT+4*NBLIAI+2) = ZI(JLIOT+4*NBLIAI+2) + 1
            NOTE1 = ZI(JLIOT+4*NBLIAI+2)
            ZI(JLIOT-1+NOTE1+2*NBLIAI) = LLIAC
            LLF2 = LLF2 + 1
            ZI(JLIAC-1+NBLIAC+LLF+1+LLF1+LLF2) = LLIAC
            DO 102 ILIAC = KK1,NBLIAC + LLF + LLF1 + LLF2
               ZI(JLIAC-1+ILIAC) = ZI(JLIAC+ILIAC)
 102        CONTINUE
          END IF
C ======================================================================
C --- VOIR UTILITE DE KKMIN ET DE INDIC EN DIMENSION 3 -----------------
C --- PAS D'UTILITE EN DIMENSION 2 -------------------------------------
C ======================================================================
          KKMIN = KK1
          INDIC = -1
          GO TO 100
C ======================================================================
        ELSE IF (KK1.LE. (NBLIAC+2*LLF)) THEN
C ======================================================================
C --- CAS D'UNE LIAISON ADHERENTE SUIVANT LA DEUXIEME DIRECTION --------
C ======================================================================
          LLIAC = ZI(JLIAC-1+KK1-LLF)
          POS1 = ZI(JAPPAR+3* (LLIAC-1)+1)
          NUM1 = ZI(JNOCO+ABS(POS1)-1)
          CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
          WRITE (IFM,1100) 'FROTTEMENT DIRECTION 2 : NOEUD ',NOM1
          IF (NIV.EQ.2) THEN
            WRITE (IFM,1110) 'FROTTEMENT DIRECTION 2:LIAISON ',LLIAC
          END IF
C ======================================================================
C --- MISE A JOUR DU NOMBRE DE LIAISONS DE FROTTEMENT ADHERENTES -------
C ======================================================================
          LLF = LLF - 1
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE CONTACT -------------------
C ======================================================================
            ZI(JLIOT+4*NBLIAI+3) = ZI(JLIOT+4*NBLIAI+3) + 1
            NOTE2 = ZI(JLIOT+4*NBLIAI+3)
            ZI(JLIOT-1+NOTE2+3*NBLIAI) = LLIAC
            DO 96 ILIAC = 0,LLF2-1
               ZI(JLIAC  +NBLIAC+LLF+1+LLF1+LLF2-ILIAC) =
     &                          ZI(JLIAC-1+NBLIAC+LLF+1+LLF1+LLF2-ILIAC)
 96         CONTINUE
            LLF1 = LLF1 + 1
            ZI(JLIAC-1+NBLIAC+LLF+1+LLF1) = LLIAC
            DO 98 ILIAC = KK1-LLF-1,NBLIAC + LLF + LLF1 + LLF2
               ZI(JLIAC-1+ILIAC) = ZI(JLIAC+ILIAC)
 98         CONTINUE
C ======================================================================
C --- VOIR UTILITE DE KKMIN ET DE INDIC EN DIMENSION 3 -----------------
C --- PAS D'UTILITE EN DIMENSION 2 -------------------------------------
C ======================================================================
          KKMIN = KK1
          INDIC = -1
          GO TO 100
C ======================================================================
        ELSE IF (KK1.LE. (NBLIAC+2*LLF+LLF1)) THEN
C ======================================================================
C --- CAS D'UNE LIAISON ADHERENTE SUIVANT LA PREMIERE DIRECTION --------
C ======================================================================
          LLIAC = ZI(JLIAC-1+KK1-LLF)
          POS1 = ZI(JAPPAR+3* (LLIAC-1)+1)
          NUM1 = ZI(JNOCO+ABS(POS1)-1)
          CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
          WRITE (IFM,1100) 'FROTTEMENT DIRECTION 1 : NOEUD ',NOM1
          IF (NIV.EQ.2) THEN
            WRITE (IFM,1110) 'FROTTEMENT DIRECTION 1:LIAISON ',LLIAC
          END IF
C ======================================================================
C --- MISE A JOUR DU NOMBRE DE LIAISONS DE FROTTEMENT ADHERENTES -------
C ======================================================================
          LLF1 = LLF1 - 1
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE CONTACT -------------------
C ======================================================================
          ZI(JLIOT+4*NBLIAI+1) = ZI(JLIOT+4*NBLIAI+1) + 1
          NOTE12 = ZI(JLIOT+4*NBLIAI+1)
          ZI(JLIOT-1+NOTE12+NBLIAI) = LLIAC
          DO 104 ILIAC = KK1-LLF,NBLIAC + LLF + LLF1 + LLF2
             ZI(JLIAC-1+ILIAC) = ZI(JLIAC+ILIAC)
 104      CONTINUE
C ======================================================================
C --- VOIR UTILITE DE KKMIN ET DE INDIC EN DIMENSION 3 -----------------
C --- PAS D'UTILITE EN DIMENSION 2 -------------------------------------
C ======================================================================
          KKMIN = KK1
          INDIC = -1
          GO TO 100
C ======================================================================
        ELSE
C ======================================================================
C --- CAS D'UNE LIAISON ADHERENTE SUIVANT LA DEUXIEME DIRECTION --------
C ======================================================================
          LLIAC = ZI(JLIAC-1+KK1-LLF)
          POS1 = ZI(JAPPAR+3* (LLIAC-1)+1)
          NUM1 = ZI(JNOCO+ABS(POS1)-1)
          CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
          WRITE (IFM,1100) 'FROTTEMENT DIRECTION 2 : NOEUD ',NOM1
          IF (NIV.EQ.2) THEN
            WRITE (IFM,1110) 'FROTTEMENT DIRECTION 2:LIAISON ',LLIAC
          END IF
C ======================================================================
C --- MISE A JOUR DU NOMBRE DE LIAISONS DE FROTTEMENT ADHERENTES -------
C ======================================================================
          LLF2 = LLF2 - 1
C ======================================================================
C --- MISE A JOUR DU VECTEUR DES LIAISONS DE CONTACT -------------------
C ======================================================================
          ZI(JLIOT+4*NBLIAI+1) = ZI(JLIOT+4*NBLIAI+1) + 1
          NOTE12 = ZI(JLIOT+4*NBLIAI+1)
          ZI(JLIOT-1+NOTE12+NBLIAI) = LLIAC
          DO 106 ILIAC = KK1-LLF,NBLIAC + LLF + LLF1 + LLF2
             ZI(JLIAC-1+ILIAC) = ZI(JLIAC+ILIAC)
 106      CONTINUE
C ======================================================================
C --- VOIR UTILITE DE KKMIN ET DE INDIC EN DIMENSION 3 -----------------
C --- PAS D'UTILITE EN DIMENSION 2 -------------------------------------
C ======================================================================
          KKMIN = KK1
          INDIC = -1
          GO TO 100
C ======================================================================
        END IF
C ======================================================================
   90 CONTINUE
C ======================================================================
  100 CONTINUE
C ======================================================================
      CALL JEDEMA()
C ======================================================================
 1000 FORMAT (' PIVOT NUL OTE',A17,A8)
 1010 FORMAT (' PIVOT NUL OTE',A17,I5)
 1100 FORMAT (' PIVOT NUL OTE',A37,A8)
 1110 FORMAT (' PIVOT NUL OTE',A37,I5)
      END
