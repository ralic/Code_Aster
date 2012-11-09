      SUBROUTINE COPMAT(MATR,NUMDDL,MAT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT NONE
C
C***********************************************************************
C 15/03/91    G.JACQUART AMV/P61 47 65 49 41
C***********************************************************************
C
C     FONCTION : COPIE MATR_ASSE DANS MATRICE PLEINE
C
C-----------------------------------------------------------------------
C    MATR   /I/ : NOM DE LA MATRICE
C    NUMDDL /I/ : NUMEROTATION SI MATRICE STOCKEE LIGNE DE CIEL
C    MAT   /O/ : VECTEUR CONTENANT LA MATONALE DE MATR
C-----------------------------------------------------------------------
C
C
C
C
C
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      CHARACTER*8 KBID,MATR
      CHARACTER*14 NUMDDL
      REAL*8      MAT(*),PIJ
      LOGICAL LSYM
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,IB ,ISCDI ,ISCHC ,J ,JBLO2 ,JBLOC
      INTEGER JREFA ,JSCBL ,JSCDE ,N1BLOC ,N2BLOC ,NBBLOC ,NEQ

      REAL*8 PJI
C-----------------------------------------------------------------------
      DATA KBID /'        '/
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
      IF (NUMDDL(1:8).EQ.KBID) THEN

          CALL U2MESK('F','UTILITAI_43',1,MATR)
C
      ELSE
        CALL JEVEUO(NUMDDL(1:8)//'      .SLCS.SCDE','L',JSCDE)
        NEQ    = ZI(JSCDE-1+1)
        NBBLOC = ZI(JSCDE-1+3)
        CALL JELIBE(NUMDDL(1:8)//'      .SLCS.SCDE')
C
C
        CALL JEVEUO(NUMDDL(1:8)//'      .SLCS.SCBL','L',JSCBL)
        CALL JEVEUO(NUMDDL(1:8)//'      .SLCS.SCDI','L',ISCDI)
        CALL JEVEUO(NUMDDL(1:8)//'      .SLCS.SCHC','L',ISCHC)
        CALL JEVEUO(MATR//'           .REFA','L',JREFA)
        LSYM=ZK24(JREFA-1+9) .EQ. 'MS'
        IF (LSYM) THEN
          DO 20 IB=1,NBBLOC
            CALL JEVEUO(JEXNUM(MATR//'           .VALM',IB),'L',JBLOC)
            N1BLOC=ZI(JSCBL+IB-1)+1
            N2BLOC=ZI(JSCBL+IB)
C
C           BOUCLE SUR LES COLONNES DU BLOC
C
            DO 30 J=N1BLOC,N2BLOC
C
C           BOUCLE SUR LES LIGNES DANS LA COLONNE
C
              DO 30 I= (J-ZI(ISCHC+J-1)+1),J
                PIJ = ZR(JBLOC+ZI(ISCDI+J-1)+I-J-1)
                MAT(I+ (J-1)*NEQ) = PIJ
                MAT(J+ (I-1)*NEQ) = PIJ
30          CONTINUE
            CALL JELIBE(JEXNUM(MATR//'           .VALM',IB))
20        CONTINUE
        ELSE
           CALL ASSERT(NBBLOC.EQ.1)
C          TRIANGULAIRE SUPERIEURE
            CALL JEVEUO(JEXNUM(MATR//'           .VALM',1),'L',JBLOC)
C          TRIANGULAIRE INFERIEURE
            CALL JEVEUO(JEXNUM(MATR//'           .VALM',2),'L',JBLO2)
C            N1BLOC=ZI(JSCBL+IB-1)+1
C            N2BLOC=ZI(JSCBL+IB)
C
C           BOUCLE SUR LES COLONNES DU BLOC
C
            DO 50 J=1,NEQ
C
C           BOUCLE SUR LES LIGNES DANS LA COLONNE
C
              DO 50 I= (J-ZI(ISCHC+J-1)+1),J
                PIJ = ZR(JBLOC+ZI(ISCDI+J-1)+I-J-1)
                PJI = ZR(JBLO2+ZI(ISCDI+J-1)+I-J-1)
                MAT(I+ (J-1)*NEQ) = PIJ
                MAT(J+ (I-1)*NEQ) = PJI
50          CONTINUE
            CALL JELIBE(JEXNUM(MATR//'           .VALM',1))
            CALL JELIBE(JEXNUM(MATR//'           .VALM',2))
        ENDIF
        CALL JELIBE(NUMDDL(1:8)//'      .SLCS.SCBL')
        CALL JELIBE(NUMDDL(1:8)//'      .SLCS.SCDI')
C
      ENDIF
C
      CALL JEDEMA()
      END
