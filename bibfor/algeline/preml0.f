      SUBROUTINE PREML0(N1,N2,DIAG,COL,DELG,PRNO,DEEQ,NEC,P,Q,LBD1,LBD2,
     &                  RL,RL1,RL2,NRL,LT,LMAT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 31/08/2004   AUTEUR VABHHTS J.PELLET 
C RESPONSABLE JFBHHUC C.ROSE
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER N1,DIAG(0:*),COL(*)
      INTEGER DELG(*),PRNO(*),DEEQ(*),NEC,LBD1(N1),LBD2(N1)
      INTEGER RL(4,*),RL1(*),RL2(*)
      INTEGER P(*),Q(*)
C     VARIABLES LOCALES
      INTEGER NRL,L1,LT,N2,INO,NUM,NOBL,I,J,LMAT,I2,IDDL,IER,IFM,NIV
      INTEGER NCOUNT,IDIAI,IDIAI1,II,LI,ICONNE,NFOIS
      NFOIS = 0
      ICONNE = 0
      CALL INFNIV(IFM,NIV)

C---------------------------------------------INITIALISATIONS
      DIAG(0) = 0
      DO 10 I = 1,N1
        P(I) = 0
        LBD1(I) = 0
        LBD2(I) = 0
        Q(I) = 0
        RL1(I) = 0
        RL2(I) = 0
   10 CONTINUE
C---------------------------------------------CALCUL DE ADJNC1
      LMAT = DIAG(N1)
      N2 = 0
      NRL = 0
      DO 30 IDDL = 1,N1
        IF (DELG(IDDL).EQ.0) THEN
          N2 = N2 + 1
          P(IDDL) = N2
          Q(N2) = IDDL
        ELSE
          INO = DEEQ(2*IDDL-1)
          IF (INO.NE.0) THEN
C     IDDL EST UN LAGRANGE DE BLOCAGE
            NUM = -DEEQ(2*IDDL)
            IF (NUM.EQ.0) THEN
              CALL UTDEBM('F','PREML0',' INCOHERENCE DANS DEEQ ')
              CALL UTIMPI('L','I = ',1,IDDL)
              CALL UTIMPI('L','DEEQ(2*I-1) = ',1,INO)
              CALL UTIMPI('L','DEEQ(2*I) = ',1,NUM)
              CALL UTFINM()
            END IF
            NOBL = PRNO((NEC+2)* (INO-1)+1)
C     RECHERCHE DE NOBL : NUMERO DU DDL BLOQUE
C     DO WHILE (DEEQ(2*NOBL).NE.NUM)
   20       CONTINUE
            IF (DEEQ(2*NOBL).NE.NUM) THEN
              NOBL = NOBL + 1
              CALL ASSERT(NOBL.LE.N1)
              GO TO 20
C     FIN DO WHILE
            END IF
            IF (DELG(IDDL).EQ.-1) THEN
              IF (LBD1(NOBL).NE.0) NFOIS = NFOIS + 1
              LBD1(NOBL) = IDDL
            ELSE IF (DELG(IDDL).EQ.-2) THEN
              IF (LBD2(NOBL).NE.0) NFOIS = NFOIS + 1
              LBD2(NOBL) = IDDL
            ELSE
              CALL UTDEBM('F','PREML0',' ERREUR DE TYPE ')
              CALL UTIMPI('L','DELG(IDDL) DIFF DE -1 OU -2 ',1,
     &                    DELG(IDDL))
              CALL UTFINM()
            END IF
            IF (NFOIS.GT.0) THEN
              CALL UTDEBM('F','PREML0','UN DDL BLOQUE A AU MOINS ')
              CALL UTIMPK('L','2 LAMBDA1 OU 2 LAMBDA2',1,' ')
              CALL UTIMPI('L','LE DDL BLOQUE EST ',1,NOBL)
              CALL UTFINM()
            END IF
          ELSE
C     IDDL EST UN LAGRANGE DE RELATION LINEAIRE
C     POUR CHQE REL. LIN. I,ON A
C     RL(2,I) = LAMBDA2 ET RL(1,I) = LAMBDA1
            IF (DELG(IDDL).EQ.-2) THEN
              NRL = NRL + 1
              RL(2,NRL) = IDDL
C     RL(1,NRL) SERA DEFINI DANS PREMLC, COMME LE NO DE COLONNE
C     DU 1ER TERME DE LA LIGNE RL(2,NRL).
            END IF
          END IF
        END IF
   30 CONTINUE
C     CALCUL DE LA TAILLE DE LA LISTE
      LT = 0
      DO 40 I = 1,NRL
        I2 = RL(2,I)
        LT = LT + (DIAG(I2)-DIAG(I2-1))
   40 CONTINUE
C     ON MAJORE LT POUR LES PETITS CAS-TESTS
      IF (LT.LE.10) THEN
        LT = LT**2
      ELSE
        LT = LT*10
      END IF

C     VERIFICATION DES CONNEXIONS DES LAGRANGES

      DO 80 I = 1,N1
        LI = LBD1(I)
        IF (LI.NE.0) THEN
          IDIAI1 = DIAG(LI-1) + 1
          IDIAI = DIAG(LI)
          IF (IDIAI1.LT.IDIAI) THEN

C WRITE(IFM,*)'LE DDL BLOQUE: ',I,' A POUR LAMBDA1: ',LBD1(I)
C WRITE(IFM,*)'LE DDL BLOQUE: ',I,' A POUR LAMBDA2: ',LBD2(I)
C WRITE(IFM,*)'LE LAMBDA1 ',LBD1(I),
C  &               ' A POUR VOISIN INATTENDUS '
            DO 50 J = IDIAI1,IDIAI - 1
C     WRITE(IFM,*) 'LE DDL ', COL(J)
              ICONNE = ICONNE + 1
   50       CONTINUE
          END IF
          DO 70 II = LI + 1,N1
            IDIAI1 = DIAG(II-1) + 1
            IDIAI = DIAG(II)
            DO 60 J = IDIAI1,IDIAI
              IF (COL(J).EQ.LI) THEN
                IF (II.NE.I .AND. II.NE.LBD2(I)) THEN
C     WRITE(IFM,*)'LE DDL BLOQUE: ',I,
C     &                        ' A POUR LAMBDA1: ',LBD1(I)
C     WRITE(IFM,*)'LE DDL BLOQUE: ',I,
C     &                        ' A POUR LAMBDA2: ',LBD2(I)
C     WRITE(IFM,*)'LE LAMBDA1 ',LBD1(I),
C     &                        ' A POUR VOISIN INATTENDU',II
                  ICONNE = ICONNE + 1
                END IF
              END IF
   60       CONTINUE

   70     CONTINUE
        END IF
   80 CONTINUE
C     IF(ICONNE.GT.0) THEN
C     CALL UTDEBM('A','NUME_DDL.PREML0',
C     +         'SUR-CONNEXION  DES LAGRANGES LAMBDA1')
C     CALL UTFINM()
C     WRITE(IFM,*) 2*ICONNE ,' TERMES SUPPLEMENTAIRES DANS
C     +    LA MATRICE INITIALE'
C     ENDIF

      IF (NIV.EQ.2) THEN
        IER = 0
        DO 90 I = 1,N1
          IF (LBD1(I).NE.0) THEN
            WRITE (IFM,*) 'LE DDL BLOQUE: ',I,' A POUR LAMBDA1: ',
     &        LBD1(I)
            WRITE (IFM,*) 'LE DDL BLOQUE: ',I,' A POUR LAMBDA2: ',
     &        LBD2(I)
            IF (LBD2(I).EQ.0) IER = 1
          ELSE IF (LBD2(I).NE.0) THEN
            IER = 1
          END IF
          IF (IER.EQ.1) THEN
            CALL UTDEBM('F','NUME_DDL.PREML0',
     &                  'INCOHERENCE DES LAGRANGES')
            CALL UTIMPI('L','DDL',1,I)
            CALL UTIMPI('L','LAMBDA1',1,LBD1(I))
            CALL UTIMPI('L','LAMBDA1',1,LBD1(I))

            CALL UTFINM()
          END IF

   90   CONTINUE
      END IF
      END
