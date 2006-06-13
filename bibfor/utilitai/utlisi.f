      SUBROUTINE UTLISI(MOTCLE,A,NA,B,NB,C,NC,NTROU)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 06/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C     ARGUMENTS:
C     ----------
      CHARACTER*(*) MOTCLE
      INTEGER A(*),B(*),C(*),NA,NB,NC,NTROU
C ----------------------------------------------------------------------
C     BUT :
C
C     UTILITAIRE D'OPERATION LOGIQUE SUR DES LISTES D'ENTIERS:
C          C = SINGLETON(A)
C          C = INTERSECTION (A,B)
C          C = UNION        (A,B)
C          C = A - B
C
C     ATTENTION AUX EFFETS DE BORD :
C     -------------------------------
C     LA PROGRAMMATION SUPPOSE QUE LES TABLEAUX A, B, C SONT DISTINCTS.
C
C     ATTENTION AUX DOUBLONS :
C     ------------------------
C     (SI LES LISTES A ET B CONTIENNENT DES ELEMENTS MULTIPLES) :
C     ALGORITHMES :
C       * SINGL :  ON RECOPIE DANS C LES ELEMENTS DE A DANS L'ORDRE
C                  DE A EN RECOPIANT PAS PLUSIEURS FOIS UN MEME
C                  ELEMENT. LES ELEMENTS DE C SONT DONC TOUS DIFF.
C       * UNION :  ON RECOPIE A DANS C, PUIS ON RECOPIE LES ELEMENTS
C                  DE B QUI NE SONT PAS DANS A.
C                  (ON A DONC OTE LES DOUBLONS (A,B) MAIS PAS
C                    CEUX DE (A,A) NI (B,B)!)
C       * INTER :  ON RECOPIE DANS C LES ELEMENTS DE A QUI EXISTENT
C                  DANS B. LES DOUBLONS (A,A) PEUVENT PERSISTER.
C       * A - B :  ON RECOPIE DANS C LES ELEMENTS DE A QUI N'EXISTENT
C                  PAS DANS B. LES DOUBLONS (A,A) PEUVENT PERSISTER.
C
C
C     ENTREES:
C       MOTCLE : ACTION DEMANDEE :  / 'SINGL'(ETON)
C                                   / 'UNION'
C                                   / 'INTER'(SECTION)
C                                   / 'DIFFE'(RENCE)
C       A,B    : LISTES D'ENTIER.
C       NA,NB  : DIMENSIONS DES LISTES A ET B
C       NC     : DIMENSION DE LA LISTE C.
C
C     SORTIES:
C       C  : LISTE D'ENTIERS TROUVEE.
C     NTROU: "+" NB D'ENTIERS DS LA LISTE C (SI NC "IN" SUFFISANT).
C     NTROU: "-" NB D'ENTIERS DS LA LISTE C (SI NC "IN" INSUFFISANT).
C
C ----------------------------------------------------------------------
      CHARACTER*5 MOTCL2
C DEB-------------------------------------------------------------------
C
      MOTCL2=MOTCLE
C
C
      IF (MOTCL2.EQ.'SINGL') THEN
C     ---------------------------
        IC=0
        DO 1, IA=1,NA
          II= INDIIS(A,A(IA),1,IA-1)
          IF (II.EQ.0) THEN
            IC=IC+1
            IF (IC.LE.NC) C(IC)=A(IA)
          END IF
 1      CONTINUE
        NTROU=IC
        IF (IC.GT.NC) NTROU= -NTROU
C
C
      ELSE IF (MOTCL2.EQ.'UNION') THEN
C     ---------------------------------
        IC=0
        DO 21, IA=1,NA
          IC=IC+1
          IF (IC.LE.NC) C(IC)=A(IA)
 21     CONTINUE
        DO 22, IB=1,NB
          II= INDIIS(A,B(IB),1,NA)
          IF (II.EQ.0) THEN
            IC=IC+1
            IF (IC.LE.NC) C(IC)=B(IB)
          END IF
 22     CONTINUE
        NTROU=IC
        IF (IC.GT.NC) NTROU= -NTROU
C
C
      ELSE IF (MOTCL2.EQ.'INTER') THEN
C     ---------------------------------
        IC=0
        DO 31, IA=1,NA
          II= INDIIS(B,A(IA),1,NB)
          IF (II.GT.0) THEN
            IC=IC+1
            IF (IC.LE.NC) C(IC)=A(IA)
          END IF
 31     CONTINUE
        NTROU=IC
        IF (IC.GT.NC) NTROU= -NTROU
C
C
      ELSE IF (MOTCL2(1:5).EQ.'DIFFE') THEN
C     ---------------------------------
        IC=0
        DO 41, IA=1,NA
          II= INDIIS(B,A(IA),1,NB)
          IF (II.EQ.0) THEN
            IC=IC+1
            IF (IC.LE.NC) C(IC)=A(IA)
          END IF
 41     CONTINUE
        NTROU=IC
        IF (IC.GT.NC) NTROU= -NTROU
C
C
      ELSE
C     -----
        CALL UTMESS('F','UTLISI','MOT-CLEF : '//MOTCL2//' INCONNU.')
      END IF
C
 9999 CONTINUE
      END
