      SUBROUTINE IRGME2 (NUMOLD,IMA,CONNEX,NBORD2,TABD,TABL,TABV,
     &                   PARTIE,JTYPE,NBNO,LISTNO,NBCMP,IFI,IADMAX)
      IMPLICIT NONE
      INTEGER   NUMOLD(*),TABD(*),TABL(*),TABV(*),NBNO
      INTEGER   LISTNO(*),NBCMP,IFI,IMA,NBORD2,IADMAX,JTYPE
      CHARACTER*24  CONNEX
      CHARACTER*(*) PARTIE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     BUT: ECRITURE DES CMP D'UN CHAMP TENSORIEL PAR ELEMENT
C     POUR UN TYPE D'ELEMENT AU FORMAT GMSH
C
C     ENTREE:
C     NUMOLD : I   : TABLEAU DE CORRESPONDANCE NOUV MAILLE ANC. MAILLE
C     IMA    : I   : NUMERO NOUVELLE MAILLE
C     CONNEX : I   : CONNECTIVITE ANCIEN MAILLAGE
C     NBORD2 : I   : NOMBRE DE NUM D'ORDRE
C     TABD   : I   : DESCRIPTEURS DU CHAMP SIMPLE A IMPRIMER
C     TABL   : I   : DESCRIPTEURS DU CHAMP SIMPLE A IMPRIMER
C     TABV   : I   : DESCRIPTEURS DU CHAMP SIMPLE A IMPRIMER
C     PARTIE : K4  : IMPRESSION DE LA PARTIE COMPLEXE OU REELLE DU CHAMP
C     JTYPE  : I   : ADRESSE DU TYPE DU CHAMP ( REEL OU COMPLEXE )
C     NBNO   : I   : NOMBRE NOEUD DE LA NOUVELLE MAILLE
C     LISTNO : I   : LISTE DES NOEUDS DE LA NOUVELLE MAILLE
C     NBCMP  : I   : NOMBRE DE COMPOSANTES DU CHAMP
C     IFI    : I   : NUMERO D'UNITE LOGIQUE DU FICHIER GMSH
C     SORTIE
C     IADMAX  : I   : MAX DES IAD SI >0 LE CHAMP EXISTE POUR LA MAILLE
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNUM, JEXNOM, JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER       IMAOLD,JCNOLD,IOR,JCESD,JCESL,JCESV,NBPT,NBSP,J,INO
      INTEGER       ITROU,IPT,INOLD,ISP,JNUMOL,JTABD,JTABL,JTABV,IAD,K
      REAL*8        VALE
      REAL*8        VAL2(6)
C
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      IMAOLD = NUMOLD(IMA)
      CALL JEVEUO (JEXNUM(CONNEX,IMAOLD), 'L', JCNOLD )
C
C --- ON NE TRAITE QUE LES CHAMPS A 1 SOUS-POINT,
C     ET UNE SEULE VALEUR SCALAIRE (COMPOSANTE K DE LA BOUCLE 51)
C
      ISP=1
      IADMAX=0
      K = 0
      DO 11 IOR = 1 , NBORD2
        JCESD = TABD(IOR)
        JCESL = TABL(IOR)
        JCESV = TABV(IOR)
        NBPT = ZI(JCESD-1+5+4*(IMAOLD-1)+1)
        NBSP = ZI(JCESD-1+5+4*(IMAOLD-1)+2)
        IF ( NBSP .NE. 1 ) THEN
          CALL U2MESS('F','PREPOST2_57')
        ENDIF
C
        ITROU=0
        IF (ZK8(JTYPE-1+IOR).EQ.'R') THEN
          DO 14 J=1,NBNO
            INO=LISTNO(J)
            ITROU=0
            DO 13 IPT = 1,NBPT
              INOLD=ZI(JCNOLD-1+IPT)
              IF (INO.EQ.INOLD) THEN
                ITROU=1
                DO 16 K = 1, NBCMP
                  CALL CESEXI('C',JCESD,JCESL,IMAOLD,IPT,ISP,K,IAD)
                  IF (IAD.GT.0) THEN
                    VALE = ZR(JCESV-1+IAD)
                    IF (ABS(VALE).LE.1.D-99) VALE = 0.D0
                    VAL2(K) = VALE
                    IADMAX=IAD
                  ELSE
                    VALE = 0.D0
                    VAL2(K) = VALE
                  ENDIF
 16             CONTINUE
              ENDIF
              WRITE(IFI,1010) VAL2(1),VAL2(4),VAL2(5),VAL2(4),VAL2(2),
     &                    VAL2(6),VAL2(5),VAL2(6),VAL2(3)
              GOTO 15
 13         CONTINUE
            IF (ITROU.EQ.0) THEN
              CALL U2MESS('F','PREPOST2_58')
            ENDIF
 15         CONTINUE
 14       CONTINUE
        ELSE IF (ZK8(JTYPE-1+IOR).EQ.'C') THEN
          DO 24 J=1,NBNO
            INO=LISTNO(J)
            ITROU=0
            DO 23 IPT = 1,NBPT
              INOLD=ZI(JCNOLD-1+IPT)
              IF (INO.EQ.INOLD) THEN
                ITROU=1
                DO 26 K = 1, NBCMP
                  CALL CESEXI('C',JCESD,JCESL,IMAOLD,IPT,ISP,K,IAD)
                  IF (IAD.GT.0) THEN
                    IF (PARTIE.EQ.'REEL') THEN
                      VALE = DBLE(ZC(JCESV-1+IAD))
                    ELSEIF (PARTIE.EQ.'IMAG') THEN
                      VALE = DIMAG(ZC(JCESV-1+IAD))
                    ENDIF
                    IF (ABS(VALE).LE.1.D-99) VALE = 0.D0
                    VAL2(K) = VALE
                    IADMAX=IAD
                  ELSE
                    VALE = 0.D0
                    VAL2(K) = VALE
                  ENDIF
 26             CONTINUE
              ENDIF
              WRITE(IFI,1010) VAL2(1),VAL2(4),VAL2(5),VAL2(4),VAL2(2),
     &                    VAL2(6),VAL2(5),VAL2(6),VAL2(3)
              GOTO 25
 23         CONTINUE
            IF (ITROU.EQ.0) THEN
              CALL U2MESS('F','PREPOST2_58')
            ENDIF
 25         CONTINUE
 24       CONTINUE
        ENDIF
 11   CONTINUE
C
      CALL JEDEMA()
C
 1010 FORMAT(1P,9(E15.7E3))
C
      END
