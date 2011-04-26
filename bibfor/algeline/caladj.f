      SUBROUTINE CALADJ
     +           (COL,DIAG,XADJ,ADJNCY,N,NNZ,DEB,TAB,SUIV,LMAT,LADJN,
     +            NRL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C RESPONSABLE JFBHHUC C.ROSE
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
C------------------------------------------------------------
      IMPLICIT NONE
C      CALCUL DES VOISINS DE TOUS LES NOEUDS ( VERSION ASTER )
C      DONNEES
C      -------
C             DIAG(1:N)        : POINTEURS DE LA MATRICE RANGEE
C             COL(1:LMAT )         SYMETRIQUE INFERIEURE PAR LIGNES
C      RESULTATS
C      ---------
C             XADJ(1:N+1)     POINTEURS ,LES VOISINS DE I SONT ENTRE
C             ADJNCY(1:NADJ)     LES ADRESSES XADJ(I) ET XADJ(I+1)-1
C                                PAR CONTINUITE ON XADJ(N+1) = NADJ+1
C      TAB DE TRAVAIL
C      --------------
C             TAB(1:LT),SUIV(1:LT)
C             NNZ(1:N)
C ATTENTION : XADJ SERT DE TAB DE TRAVAIL DANS LA 1ERE PARTIE (DO 1 )
C-----------
      INTEGER LMAT,N,COL(LMAT),DIAG(0:N),ADJNCY(*)
      INTEGER XADJ(N+1),NNZ(N),SUIV(*),TAB(*)
      INTEGER NRL
      INTEGER DEB(N)
C--------------------------------------------------------------
C     VAR. LOCALES
      INTEGER I,J,K,II,IT,LADJN,IAD
      INTEGER VALI(2)
      IF(NRL.EQ.0) THEN
C     PAS DE RELATION LINEAIRE ENTRE PLUSIEURS DDL
         DO  110  J = 1, N

            NNZ(J) = DIAG(J) - DIAG(J-1 ) - 1
 110     CONTINUE
         DO  120  K = 1, DIAG(N)
C     PARTIE TRIANGULAIRE SUPERIEURE
            NNZ(COL(K)) = NNZ(COL(K)) + 1
 120     CONTINUE
C
         XADJ(1) = 1
         DO  130  J = 1, N
C     ON DIMINUE DE 1 CAR ON NE VEUT PAS LE TERME
C     DIAGONAL DANS ADJNCY
            XADJ(J+1) = XADJ(J) + NNZ(J) - 1
            NNZ(J) = 0
 130     CONTINUE
C
         DO  150  J = 1, N
            DO  140  II = DIAG(J-1)+1, DIAG(J)-1
               I = COL(II)
               ADJNCY(XADJ(J)+NNZ(J)) = I
               NNZ(J) = NNZ(J) + 1
               ADJNCY(XADJ (I)+NNZ(I)) = J
               NNZ(I) = NNZ(I) + 1
 140        CONTINUE
 150     CONTINUE

C     ---------------------------
      ELSE
C     AVEC RELATION LINEAIRE
C     CALCUL DES LISTES DE NOEUDS A AJOUTER ( FAIT DANS PREMLA)
C
         DO 190 I=1,N
C            DEB(I) =0
            NNZ(I) =0
 190     CONTINUE
C     INITIALISATION DE NNZ : NBRE DE TERMES A AJOUTER
C     POUR CHAQUE LIGNE
         DO 220 J=1,N
            IT = DEB(J)
 219        CONTINUE
            IF(IT.GT.0) THEN
               NNZ(J) = NNZ(J) + 1
               IT = SUIV(IT)
               GOTO 219
            ENDIF
 220     CONTINUE
C     VERIFICATION
         DO  310  J = 1, N
C     TERMES A AJOUTER PARTIE INFERIEURE
            NNZ(J) = NNZ(J) + DIAG(J) - DIAG(J-1 ) - 1
 310     CONTINUE
         DO  320  K = 1, DIAG(N)
C     PARTIE TRIANGULAIRE SUPERIEURE
            NNZ(COL(K)) = NNZ(COL(K)) + 1
 320     CONTINUE
         DO  325  J = 1, N
C     TERMES A AJOUTER PARTIE SUPERIEURE
            IT = DEB(J)
 324        CONTINUE
            IF(IT.GT.0) THEN
               NNZ(TAB(IT)) = NNZ(TAB(IT)) + 1
               IT = SUIV(IT)
               GOTO 324
            ENDIF
 325     CONTINUE
C
         XADJ(1) = 1
         DO  330  J = 1, N
C     ON DIMINUE DE 1 CAR ON NE VEUT PAS LE TERME
C     DIAGONAL DANS ADJNCY
            XADJ(J+1) = XADJ(J) + NNZ(J) - 1
            NNZ(J) = 0
 330     CONTINUE
        IF( (XADJ(N+1)-1).GT.LADJN) THEN
C       TEST D'ESPACE SUFFISANT DANS ADJNCY
               VALI (1) = LADJN
               VALI (2) = XADJ(N+1)-1
               CALL U2MESG('F', 'ALGELINE4_4',0,' ',2,VALI,0,0.D0)
         ENDIF
C
        IAD=0
         DO  350  J = 1, N
            DO  340  II = DIAG(J-1)+1, DIAG(J)-1
               I = COL(II)
               ADJNCY(XADJ(J)+NNZ(J)) = I
               NNZ(J) = NNZ(J) + 1
               ADJNCY(XADJ (I)+NNZ(I)) = J
               NNZ(I) = NNZ(I) + 1
            IAD=MAX(IAD,(XADJ (I)+NNZ(I)))
            IAD=MAX(IAD,(XADJ (J)+NNZ(J)))
 340        CONTINUE
            IT = DEB(J)
 344        CONTINUE
            IF(IT.GT.0) THEN
               ADJNCY(XADJ(J)+NNZ(J)) = TAB(IT)
               NNZ(J) = NNZ(J) + 1
               ADJNCY(XADJ(TAB(IT))+NNZ(TAB(IT))) = J
               NNZ(TAB(IT)) = NNZ(TAB(IT)) + 1
               IT = SUIV(IT)
               GOTO 344
            ENDIF
 350     CONTINUE

      ENDIF
      END
