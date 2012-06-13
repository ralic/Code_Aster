      SUBROUTINE LECTVL (ZCMPLX,ITYPE,NBABS,INATUR,IDEAS,NBMESU,
     &                   LABS,AMIN,APAS,LVALC,LVALR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C
C     LECTVL : LECTURE VALEURS SUR FICHIER FORMAT UNIVERSEL DATASET 58
C
C     IN : ZCMPLX : DONNEES COMPLEXE OU REELLE (BOOLEEN)
C     IN : ITYPE : MODE DE RANGEMENT DES DONNEES (EVEN/UNEVEN)
C     IN : NBABS : NOMBRE DE VALEURS CONTENUES DANS LE DATASET
C     IN : INATUR : NATURE DU CHAMP (SIMPLE/DOUBLE;REEL/COMPLEXE)
C     IN : IDEAS : NUMERO LOGIQUE DU FICHIER UNV
C     IN : NBMESU : NUMERO DE LA MESURE COURANTE
C     IN : LABS : ADRESSE DU VECTEUR DES ABSCISSES
C     IN : AMIN : ABSCISSE MIN
C     IN : APAS : PAS (ABSCISSE)
C     IN : LVALC : ADRESSE DU VECTEUR CONTENANT LES VALEURS COMPLEXES
C     IN : LVALR : ADRESSE DU VECTEUR CONTENANT LES VALEURS REELLES
C     ------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      LOGICAL ZCMPLX
      INTEGER ITYPE,NBABS,INATUR,IDEAS,NBMESU,LABS,LVALC,LVALR
      REAL*8 AMIN,APAS
C
C
C
C
C
      INTEGER I,NBLI,REST,NBVAL,LIG,INCR,IABS
      INTEGER ICMPR,ICMPI,ICMPA
      REAL*8 VAL(6)
C
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      IABS = 0
      IF (ZCMPLX) THEN
C COMPLEX EVEN
        IF (ITYPE .EQ. 1) THEN
          NBVAL = NBABS*2
          IF (INATUR .EQ. 5) THEN
C COMPLEXE EVEN SIMPLE PRECISION
            NBLI = INT ( NBVAL / 6 )
            REST  = NBVAL - ( NBLI * 6 )
            DO 151 LIG = 1,NBLI
              READ (IDEAS,'(6E13.5)',ERR=160) (VAL(I),I=1,6)
              DO 100 INCR = 1,3
                IABS = IABS + 1
                ICMPR = (INCR-1)*2 + 1
                ICMPI = (INCR-1)*2 + 2
                IF (NBMESU .LE. 1) THEN
                  ZR(LABS-1 + IABS) = AMIN + (IABS -1)*APAS
                END IF
                ZC(LVALC-1 + (NBMESU-1)*NBABS + IABS) =
     &              DCMPLX(VAL(ICMPR),VAL(ICMPI))
  100         CONTINUE
  151       CONTINUE
            IF (REST .GE. 1) THEN
              READ (IDEAS,'(6E13.5)',ERR=160) (VAL(I),I=1,REST)
              DO 152 INCR = 1,INT(REST/2)
                IABS = IABS + 1
                ICMPR = (INCR-1)*2 + 1
                ICMPI = (INCR-1)*2 + 2
                IF (NBMESU .LE. 1) THEN
                  ZR(LABS-1 + IABS) = AMIN + (IABS -1)*APAS
                END IF
                ZC(LVALC-1 + (NBMESU-1)*NBABS + IABS) =
     &              DCMPLX(VAL(ICMPR),VAL(ICMPI))
  152         CONTINUE
            END IF
          ELSE
C COMPLEXE EVEN DOUBLE PRECISION
            NBLI = INT ( NBVAL / 4 )
            REST  = NBVAL - ( NBLI * 4 )
            DO 154 LIG = 1,NBLI
              READ (IDEAS,'(4E20.12)',ERR=160) (VAL(I),I=1,4)
              DO 153 INCR = 1,2
                IABS = IABS + 1
                ICMPR = (INCR-1)*2 + 1
                ICMPI = (INCR-1)*2 + 2
                IF (NBMESU .LE. 1) THEN
                  ZR(LABS-1 + IABS) = AMIN + (IABS -1)*APAS
                END IF
                ZC(LVALC-1 + (NBMESU-1)*NBABS + IABS) =
     &              DCMPLX(VAL(ICMPR),VAL(ICMPI))
  153         CONTINUE
  154       CONTINUE
            IF (REST .GE. 1) THEN
              READ (IDEAS,'(4E20.12)',ERR=160) (VAL(I),I=1,REST)
              DO 155 INCR = 1,INT(REST/2)
                IABS = IABS + 1
                ICMPR = (INCR-1)*2 + 1
                ICMPI = (INCR-1)*2 + 2
                IF (NBMESU .LE. 1) THEN
                  ZR(LABS-1 + IABS) = AMIN + (IABS -1)*APAS
                END IF
                ZC(LVALC-1 + (NBMESU-1)*NBABS + IABS) =
     &              DCMPLX(VAL(ICMPR),VAL(ICMPI))
  155         CONTINUE
            END IF
          END IF
        ELSE
C COMPLEX UNEVEN
          NBVAL = NBABS*3
          IF (INATUR .EQ. 5) THEN
C COMPLEXE UNEVEN SIMPLE PRECISION
            NBLI = INT ( NBVAL / 6 )
            REST  = NBVAL - ( NBLI * 6 )
            DO 161 LIG = 1,NBLI
              READ (IDEAS,'(6E13.5)',ERR=160) (VAL(I),I=1,6)
              DO 162 INCR = 1,2
                IABS = IABS + 1
                ICMPA = (INCR-1)*3 + 1
                ICMPR = (INCR-1)*3 + 2
                ICMPI = (INCR-1)*3 + 3
                IF (NBMESU .LE. 1) THEN
                  ZR(LABS-1 + IABS) = VAL(ICMPA)
                END IF
                ZC(LVALC-1 + (NBMESU-1)*NBABS + IABS) =
     &              DCMPLX(VAL(ICMPR),VAL(ICMPI))
  162         CONTINUE
  161       CONTINUE
            IF (REST .GE. 1) THEN
              READ (IDEAS,'(6E13.5)',ERR=160) (VAL(I),I=1,REST)
              IABS = IABS + 1
              IF (NBMESU .LE. 1) THEN
                ZR(LABS-1 + IABS) = VAL(1)
              END IF
              ZC(LVALC-1 + (NBMESU-1)*NBABS + IABS) =
     &              DCMPLX(VAL(2),VAL(3))
            END IF
          ELSE
C COMPLEX UNEVEN DOUBLE PRECISION
            NBLI = INT ( NBVAL / 3 )
            DO 164 LIG = 1,NBLI
        READ (IDEAS,'(E13.5,2E20.12)',ERR=160) (VAL(I),I=1,3)
              IABS = IABS + 1
              ICMPA =  1
              ICMPR =  2
              ICMPI =  3
              IF (NBMESU .LE. 1) THEN
                ZR(LABS-1 + IABS) = VAL(ICMPA)
              END IF
              ZC(LVALC-1 + (NBMESU-1)*NBABS + IABS) =
     &              DCMPLX(VAL(ICMPR),VAL(ICMPI))
  164       CONTINUE
          END IF
        END IF

      ELSE
C REAL EVEN
        IF (ITYPE .EQ. 1) THEN
          NBVAL = NBABS
          IF (INATUR .EQ. 2) THEN
C REAL EVEN SIMPLE PRECISION
            NBLI = INT ( NBVAL / 6 )
            REST  = NBVAL - ( NBLI * 6 )
            DO 171 LIG = 1,NBLI
              READ (IDEAS,'(6E13.5)',ERR=160) (VAL(I),I=1,6)
              DO 172 INCR = 1,6
                IABS = IABS + 1
                IF (NBMESU .LE. 1) THEN
                  ZR(LABS-1 + IABS) = AMIN + (IABS -1)*APAS
                END IF
                ZR(LVALR-1 + (NBMESU-1)*NBABS + IABS) = VAL(INCR)
  172         CONTINUE
  171       CONTINUE
            IF (REST .GE. 1) THEN
              READ (IDEAS,'(6E13.5)',ERR=160) (VAL(I),I=1,REST)
              DO 173 INCR = 1,REST
                IABS = IABS + 1
                IF (NBMESU .LE. 1) THEN
                  ZR(LABS-1 + IABS) = AMIN + (IABS -1)*APAS
                END IF
                ZR(LVALR-1 + (NBMESU-1)*NBABS + IABS) = VAL(INCR)
  173         CONTINUE
            END IF
          ELSE
C REAL EVEN DOUBLE PRECISION
            NBLI = INT ( NBVAL / 4 )
            REST  = NBVAL - ( NBLI * 4 )
            DO 184 LIG = 1,NBLI
              READ (IDEAS,'(4E20.12)',ERR=160) (VAL(I),I=1,4)
              DO 183 INCR = 1,4
                IABS = IABS + 1
                IF (NBMESU .LE. 1) THEN
                  ZR(LABS-1 + IABS) = AMIN + (IABS -1)*APAS
                END IF
                ZR(LVALR-1 + (NBMESU-1)*NBABS + IABS) = VAL(INCR)
  183         CONTINUE
  184       CONTINUE
            IF (REST .GE. 1) THEN
              READ (IDEAS,'(4E20.12)',ERR=160) (VAL(I),I=1,REST)
              DO 185 INCR = 1,REST
                IABS = IABS + 1
                IF (NBMESU .LE. 1) THEN
                  ZR(LABS-1 + IABS) = AMIN + (IABS -1)*APAS
                END IF
                ZR(LVALR-1 + (NBMESU-1)*NBABS + IABS) = VAL(INCR)
  185         CONTINUE
            END IF
          END IF
        ELSE
C REAL UNEVEN
          NBVAL = NBABS*2
          IF (INATUR .EQ. 2) THEN
C REAL UNEVEN SIMPLE PRECISION
            NBLI = INT ( NBVAL / 6 )
            REST  = NBVAL - ( NBLI * 6 )
            DO 191 LIG = 1,NBLI
              READ (IDEAS,'(6E13.5)',ERR=160) (VAL(I),I=1,6)
              DO 190 INCR = 1,3
                IABS = IABS + 1
                ICMPA = (INCR-1)*2 + 1
                ICMPR = (INCR-1)*2 + 2
                IF (NBMESU .LE. 1) THEN
                  ZR(LABS-1 + IABS) = VAL(ICMPA)
                END IF
                ZR(LVALR-1 + (NBMESU-1)*NBABS + IABS) = VAL(ICMPR)
  190         CONTINUE
  191       CONTINUE
            IF (REST .GE. 1) THEN
              READ (IDEAS,'(6E13.5)',ERR=160) (VAL(I),I=1,REST)
              DO 192 INCR = 1,INT(REST/2)
                IABS = IABS + 1
                ICMPA = (INCR-1)*2 + 1
                ICMPR = (INCR-1)*2 + 2
                IF (NBMESU .LE. 1) THEN
                  ZR(LABS-1 + IABS) = VAL(ICMPA)
                END IF
                ZR(LVALR-1 + (NBMESU-1)*NBABS + IABS) = VAL(ICMPR)
  192         CONTINUE
            END IF
          ELSE
C REAL UNEVEN DOUBLE PRECISION
            NBLI = INT ( NBVAL / 4 )
            REST  = NBVAL - ( NBLI * 4 )
            DO 144 LIG = 1,NBLI
        READ (IDEAS,'(2(E13.5,E20.12))',ERR=160) (VAL(I),I=1,4)
              DO 143 INCR = 1,2
                IABS = IABS + 1
                ICMPA = (INCR-1)*2 + 1
                ICMPR = (INCR-1)*2 + 2
                IF (NBMESU .LE. 1) THEN
                  ZR(LABS-1 + IABS) = VAL(ICMPA)
                END IF
                ZR(LVALR-1 + (NBMESU-1)*NBABS + IABS) = VAL(ICMPR)
  143         CONTINUE
  144       CONTINUE
            IF (REST .GE. 1) THEN
        READ (IDEAS,'(2(E13.5,E20.12))',ERR=160) (VAL(I),I=1,REST)
              DO 145 INCR = 1,INT(REST/2)
                IABS = IABS + 1
                ICMPA = (INCR-1)*2 + 1
                ICMPR = (INCR-1)*2 + 2
                IF (NBMESU .LE. 1) THEN
                  ZR(LABS-1 + IABS) = VAL(ICMPA)
                END IF
                ZR(LVALR-1 + (NBMESU-1)*NBABS + IABS) = VAL(ICMPR)
  145         CONTINUE
            END IF
          END IF
        END IF
      END IF
C FIN LECTURE DU DATASET
      GO TO 9999

  160 CONTINUE
C EN CAS D ERREUR DE LECTURE DU FICHIER UNV
      CALL U2MESS('F','ALGORITH5_5')

 9999 CONTINUE

      CALL JEDEMA()
C
      END
