      SUBROUTINE ARLNIM(UNIT,DIME,NNORM,NTANG)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 10/07/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*10  NNORM,NTANG
      INTEGER       UNIT,DIME
C
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C IMPRESSION DE LA SD POUR LES NORMALES ET TANGENTES LISSEES
C
C ----------------------------------------------------------------------
C
C
C IN  UNIT   : UNITE D'IMPRESSION
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  NNORM  : NOM DE L'OBJET NORMALES LISSEES
C IN  NTANG  : NOM DE L'OBJET TANGENTES LISSEES
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXNUM
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      IRET
      CHARACTER*8  NOMO,NOMA,NOMNOE
      INTEGER      IOCC,JNOMA
      REAL*8       NORM(3),TANG(6)
      INTEGER      INO
      INTEGER      JDIME,JNORM,JTANG
      INTEGER      NNO
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- RECUPERATION MAILLAGE
C
      CALL GETVID(' ','MODELE',0,1,1,NOMO,IOCC)
      IF (IOCC.EQ.0) THEN
        WRITE(UNIT,*) '<NORMALES> AFFICHAGE IMPOSSIBLE (VOIR ARLNIM) '
        GOTO 999
      ELSE
        WRITE(UNIT,*) '<NORMALES> NORMALES ET TANGENTES LISSEES...'
      ENDIF
      CALL JEVEUO(NOMO(1:8)//'.MODELE    .LGRF','L',JNOMA)
      NOMA = ZK8(JNOMA)
      CALL JEVEUO(NOMA(1:8)//'.DIME','L',JDIME)
      NNO  = ZI(JDIME)
C
      CALL JEEXIN(NNORM(1:10),IRET)
      IF (IRET.EQ.0) THEN
        WRITE(UNIT,*) '<NORMALES> SD NORMALES N''EXISTE PAS'
      ELSE
        CALL JEVEUO(NNORM(1:10),'L',JNORM)
        CALL JEEXIN(NTANG(1:10),IRET)
        IF (IRET.EQ.0) THEN
          WRITE(UNIT,*) '<NORMALES> SD TANGENTES N''EXISTE PAS'
          GOTO 999
        ELSE
          CALL JEVEUO(NTANG(1:10),'L',JTANG)
        ENDIF
C
        DO 70 INO = 1 , NNO
          CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMNOE',INO),NOMNOE)
          IF (DIME.EQ.2) THEN
            NORM(1) = ZR(JNORM+2*(INO-1))
            NORM(2) = ZR(JNORM+2*(INO-1)+1)
            TANG(1) = ZR(JTANG+2*(INO-1))
            TANG(2) = ZR(JTANG+2*(INO-1)+1)
            WRITE(UNIT,*) '<NORMALES> ...NOEUD ',NOMNOE,' NORMALE  = (',
     &                 NORM(1),',',
     &                 NORM(2),')'
            WRITE(UNIT,*) '<NORMALES> ................. TANGENTE = (',
     &                 TANG(1),',',
     &                 TANG(2),')'
          ELSEIF (DIME.EQ.3) THEN
            NORM(1) = ZR(JNORM+3*(INO-1))
            NORM(2) = ZR(JNORM+3*(INO-1)+1)
            NORM(3) = ZR(JNORM+3*(INO-1)+2)
            TANG(1) = ZR(JTANG+6*(INO-1))
            TANG(2) = ZR(JTANG+6*(INO-1)+1)
            TANG(3) = ZR(JTANG+6*(INO-1)+2)
            TANG(4) = ZR(JTANG+6*(INO-1)+3)
            TANG(5) = ZR(JTANG+6*(INO-1)+4)
            TANG(6) = ZR(JTANG+6*(INO-1)+5)
            WRITE(UNIT,*) '<NORMALES> ...NOEUD ',NOMNOE,'NORMALE  = (',
     &                 NORM(1),',',
     &                 NORM(2),',',
     &                 NORM(3),')'
            WRITE(UNIT,*) '<NORMALES> ................. TANGENTE = (',
     &                 TANG(1),',',
     &                 TANG(2),',',
     &                 TANG(3),')'
            WRITE(UNIT,*) '<NORMALES> ................. TANGENTE = (',
     &                 TANG(4),',',
     &                 TANG(5),',',
     &                 TANG(6),')'
          ELSE
            WRITE(UNIT,*) '<NORMALES> DIMENSION INCORRECTE'
            GOTO 999
          ENDIF
 70     CONTINUE
      ENDIF
C
  999 CONTINUE
C
      CALL JEDEMA()
      END
