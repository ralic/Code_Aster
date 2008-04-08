      SUBROUTINE ARBRIM(UNIT,NOMARB)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/04/2008   AUTEUR MEUNIER S.MEUNIER 
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
C RESPONSABLE MEUNIER S.MEUNIER
C
      IMPLICIT NONE
      CHARACTER*16  NOMARB
      INTEGER       UNIT
C
C ----------------------------------------------------------------------
C
C CREATION D'UN ARBRE BSP POUR APPARIEMENT DES MAILLES (EN BOITE)
C
C IMPRESSION DE LA STRUCTURE DE DONNEES
C
C ----------------------------------------------------------------------
C
C
C IN  UNIT   : UNITE D'IMPRESSION
C IN  NOMARB : NOM DE LA SD ARBRE
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
      INTEGER      IPAN
      INTEGER      IRET
      LOGICAL      DEBUG
      CHARACTER*8  K8BID
      INTEGER      NLIMA,ICELL,NCELL
      INTEGER      PPAN,ICG,ICD
      INTEGER      JLIMA,JCELL
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      DEBUG  = .TRUE.
C
      CALL JEEXIN(NOMARB(1:16)//'.CELL'  ,IRET)
      IF (IRET.EQ.0) THEN
        WRITE(UNIT,*) '<ARBREBSP> SD ARBRE INCORRECTE: <',
     &           NOMARB(1:16)//'.CELL','> N''EXISTE PAS'
      ELSE
        WRITE(UNIT,*) '<ARBREBSP> ARBRE DE PARTITION BINAIRE '//
     &                'DE L''ESPACE...'
        CALL JEVEUO(NOMARB(1:16)//'.CELL','L',JCELL)
        CALL JELIRA(NOMARB(1:16)//'.CELL','LONMAX',NCELL,K8BID)
        NCELL = NCELL/3
        WRITE(UNIT,*) '<ARBREBSP> ... NOMBRE DE CELLULES :',
     &                 NCELL
        IF (DEBUG) THEN
          DO 40 ICELL = 1 , NCELL
            IPAN = ZI(JCELL+3*(ICELL-1))
            IF (IPAN.GT.0) THEN
              WRITE(UNIT,*) '<ARBREBSP> ... CELLULE ( ',ICELL,' ):'
              ICG = ZI(JCELL+3*(ICELL-1)+1)
              ICD = ZI(JCELL+3*(ICELL-1)+2)
              WRITE(UNIT,*) '<ARBREBSP> ...... PAN ASSOCIE    : ',
     &                       IPAN
              WRITE(UNIT,*) '<ARBREBSP> ...... CELLULE GAUCHE : ',
     &                       ICG
              WRITE(UNIT,*) '<ARBREBSP> ...... CELLULE DROITE : ',
     &                       ICD
            ELSE
              PPAN = ZI(JCELL+3*(ICELL-1)+1)
              IPAN = - IPAN
              IF (IPAN.EQ.0) THEN
                WRITE(UNIT,*) '<ARBREBSP> ... CELLULE VIDE : ',
     &                         ICELL
              ELSE
                WRITE(UNIT,*) '<ARBREBSP> ... CELLULE ( ',ICELL,' ):'
                WRITE(UNIT,*) '<ARBREBSP> ...... NB MAILLES : ',
     &                         IPAN
                WRITE(UNIT,*) '<ARBREBSP> ...... POINT PAN  : ',
     &                         PPAN
              ENDIF
            ENDIF
 40       CONTINUE
        ENDIF
      ENDIF
C
      CALL JEEXIN(NOMARB(1:16)//'.LIMA'  ,IRET)
      IF (IRET.EQ.0) THEN
        WRITE(UNIT,*) '<ARBREBSP> SD ARBRE INCORRECTE: <',
     &           NOMARB(1:16)//'.LIMA','> N''EXISTE PAS'
      ELSE
        CALL JEVEUO(NOMARB(1:16)//'.LIMA','L',JLIMA)
        CALL JELIRA(NOMARB(1:16)//'.LIMA','LONMAX',NLIMA,K8BID)
        WRITE(UNIT,*) '<ARBREBSP> ... NBRE DE GRPE DE MAILLES :',
     &                 NLIMA
      ENDIF
C
      CALL JEDEMA()
      END
