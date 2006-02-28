      SUBROUTINE COPMA2(MATR,MAT1,MAT2)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 28/02/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C
C     FONCTION : COPIE MATR_ASSE VERSION MULT_FRONT DANS MATRICE PLEINE
C
C-----------------------------------------------------------------------
C    IN : MATR  NOM DE LA MATRICE
C    OUT: MAT1/MAT2 VECTEUR CONTENANT LA MATRICE PLEINE
C     ------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       03/06/04 (OB): CREATION POUR FETI.
C-----------------------------------------------------------------------
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*19 MATR
      REAL*8       MAT1(*),MAT2(*)

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32  JEXNUM,JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------

C DECLARATION VARIABLES LOCALES
      INTEGER IVALE,JREFA,JSMDE,NEQ,ISMDI,ISMHC,I,J,IAUX,IAUXO,K,L
      CHARACTER*8  KBID
      CHARACTER*14 NUMDDL
      REAL*8       PIJ

C-----------------------------------------------------------------------
      DATA KBID /'        '/
C-----------------------------------------------------------------------

      CALL JEMARQ()
      CALL JEVEUO(MATR//'.REFA','L',JREFA)
      NUMDDL=ZK24(JREFA-1+2)(1:14)

      IF (NUMDDL(1:8).EQ.KBID) THEN
          CALL UTMESS('F','COPMA2',
     +                'NUMEROTATION ABSENTE '
     +                //' PROBLEME DANS LA MATRICE '//MATR )

      ELSE
        CALL JEVEUO(NUMDDL(1:14)//'.SMOS.SMDE','L',JSMDE)
        NEQ=ZI(JSMDE-1+1)
        CALL JEVEUO(NUMDDL(1:14)//'.SMOS.SMDI','L',ISMDI)
        CALL JEVEUO(NUMDDL(1:14)//'.SMOS.SMHC','L',ISMHC)

        CALL JEVEUO(JEXNUM(MATR//'.VALM',1),'L',IVALE)

        IAUXO=0
        L=IVALE-1
        DO 50 J=1,NEQ
          IAUX=ZI(ISMDI+J-1)
          DO 40 K=IAUXO+1,IAUX
            L=L+1
            I=ZI(ISMHC+K-1)
            PIJ=ZR(L)
            MAT1(I+(J-1)*NEQ) = PIJ
            MAT1(J+(I-1)*NEQ) = PIJ
            MAT2(I+(J-1)*NEQ) = PIJ
            MAT2(J+(I-1)*NEQ) = PIJ
   40     CONTINUE
          IAUXO=IAUX
   50   CONTINUE
      ENDIF

      CALL JEDEMA()
      END
