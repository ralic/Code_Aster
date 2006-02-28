      SUBROUTINE EXTDIA(MATR,NUMDDL,ICODE,DIAG)
      IMPLICIT REAL*8 (A-H,O-Z)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 28/02/2006   AUTEUR VABHHTS J.PELLET 
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
C***********************************************************************
C 15/03/91    G.JACQUART AMV/P61 47 65 49 41
C***********************************************************************
C
C     FONCTION : EXTRACTION DE LA DIAGONALE D'UNE MATRICE
C
C-----------------------------------------------------------------------
C    MATR   /I/ : NOM DE LA MATRICE
C    NUMDDL /I/ : NUMEROTATION ASSOCIEE A MATR
C    ICODE  /I/ : 2 SI CALCUL TRANSITOIRE DIRECT
C                 1 SI SOUS-STRUCTURATION DYNAMIQUE TRANSITOIRE
C                   SANS DOUBLE PROJECTION
C                 0 SINON
C    DIAG   /O/ : VECTEUR CONTENANT LA DIAGONALE DE MATR
C-----------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C
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
C
      CHARACTER*32  JEXNUM,JEXNOM
C
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      CHARACTER*24 NUMDDL
      CHARACTER*8  MATR
      REAL*8       DIAG(*)
      INTEGER      ICODE, STOCKA
C
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL MTDSCR(MATR)
      CALL JEVEUO(MATR//'           .&INT','L',LMAT)

      CALL JEVEUO(NUMDDL(1:8)//'      .SMOS.SMDE','L',JSMDE)
      NEQ    = ZI(JSMDE-1+1)

      CALL WKVECT('&&EXTDIA.TYPDDL','V V I',NEQ,JTYP)
      CALL TYPDDL('ACTI',NUMDDL(1:8),NEQ,ZI(JTYP),NBACTI,NBBLOQ,
     &             NBLAGR,NBLIAI)
      IF (ICODE.EQ.2) THEN
        IF (NBLIAI.GT.0) THEN
          CALL UTMESS ('F','EXTDIA_01','LES CONDITIONS AUX LIMITES'
     &                 //' AUTRES QUE DES DDLS BLOQUES NE SONT PAS'
     &                 //' ADMISES')
        ENDIF
      ENDIF

      CALL JEVEUO(NUMDDL(1:8)//'      .SMOS.SMDI','L',JADIA)
      K = 0
      L = 0
      CALL JEVEUO(JEXNUM(MATR//'           .VALM',1),'L',JBLOC)
      DO 40 J=1,NEQ
        K = K + 1
        IF (ZI(JTYP-1+K).NE.0) THEN
          IDIA=ZI(JADIA+K-1)
          L=L+1
          DIAG(L)=ZR(JBLOC-1+IDIA)
        ELSEIF (ICODE.EQ.0.OR.ICODE.EQ.2) THEN
          L=L+1
          DIAG(L)=0.D0
        ENDIF
40    CONTINUE
      CALL JEDETR('&&EXTDIA.TYPDDL')

 9999 CONTINUE
      CALL JEDEMA()
      END
