      SUBROUTINE TBIMFO ( TABLE, FICHIE)
      IMPLICIT   NONE
      CHARACTER*(*)       TABLE, FICHIE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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
C     OPERATEUR   IMPR_TABLE, IMPRESSIONS DE FONCTION
C     ------------------------------------------------------------------
C
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      IMPR, IER1, IER2, IER3, JTBNP, NBPARA, NBLIGN, I, J,
     +             JTBLP, JVALE, JLOGQ, IRET, IOC, NBFONC, JFONC
      CHARACTER*3  TYPVAL
      CHARACTER*8  TYPE
      CHARACTER*19 NOMTAB, NOMFON
      CHARACTER*24 NOPARA, NOMOBJ, NOMJV, NOMJVL
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      NOMTAB = TABLE
      IMPR   = 3
C
      CALL JEEXIN ( NOMTAB//'.TBBA', IER1 )
      CALL JEEXIN ( NOMTAB//'.TBNP', IER2 )
      CALL JEEXIN ( NOMTAB//'.TBLP', IER3 )
      IF ( IER1.EQ.0 .OR. IER2.EQ.0 .OR. IER3.EQ.0 ) THEN
         CALL UTMESS('A','TBIMPR','LA TABLE N''EXISTE PAS')
         GOTO 9999
      ENDIF
C
      CALL JEVEUO ( NOMTAB//'.TBNP' , 'L', JTBNP )
      NBPARA = ZI(JTBNP  )
      NBLIGN = ZI(JTBNP+1)
      IF ( NBPARA .EQ. 0 ) THEN
         CALL UTMESS('A','TBIMPR','PAS DE PARAMETRES DEFINIS') 
         GOTO 9999
      ENDIF
      IF ( NBLIGN .EQ. 0 ) THEN
         CALL UTMESS('A','TBIMPR','PAS DE LIGNES DEFINIS') 
         GOTO 9999
      ENDIF
      CALL JEVEUO ( NOMTAB//'.TBLP' , 'L', JTBLP )
C
      DO 10 I = 1 , NBPARA
         NOPARA = ZK24(JTBLP+4*(I-1)  )
         TYPE   = ZK24(JTBLP+4*(I-1)+1)
         NOMJV  = ZK24(JTBLP+4*(I-1)+2)
         NOMJVL = ZK24(JTBLP+4*(I-1)+3)
         CALL JEVEUO ( NOMJV , 'L', JVALE )
         CALL JEVEUO ( NOMJVL, 'L', JLOGQ )
         IF ( TYPE(1:3) .EQ. 'K80' ) THEN
            DO 20 J = 1 , NBLIGN
               IF ( ZL(JLOGQ+J-1) ) THEN
                  NOMFON = ZK80(JVALE+J-1)(1:19)
                  CALL JEEXIN ( NOMFON//'.PROL', IRET )
                  IF ( IRET .NE. 0 ) GOTO 100
                  GOTO 10
               ENDIF
 20         CONTINUE
            GOTO 10
         ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
            DO 22 J = 1 , NBLIGN
               IF ( ZL(JLOGQ+J-1) ) THEN
                  NOMFON = ZK32(JVALE+J-1)(1:19)
                  CALL JEEXIN ( NOMFON//'.PROL', IRET )
                  IF ( IRET .NE. 0 ) GOTO 100
                  GOTO 10
               ENDIF
 22         CONTINUE
            GOTO 10
         ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
            DO 24 J = 1 , NBLIGN
               IF ( ZL(JLOGQ+J-1) ) THEN
                  NOMFON = ZK24(JVALE+J-1)(1:19)
                  CALL JEEXIN ( NOMFON//'.PROL', IRET )
                  IF ( IRET .NE. 0 ) GOTO 100
                  GOTO 10
               ENDIF
 24         CONTINUE
            GOTO 10
         ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
            DO 26 J = 1 , NBLIGN
               IF ( ZL(JLOGQ+J-1) ) THEN
                  NOMFON = ZK16(JVALE+J-1)
                  CALL JEEXIN ( NOMFON//'.PROL', IRET )
                  IF ( IRET .NE. 0 ) GOTO 100
                  GOTO 10
               ENDIF
 26         CONTINUE
            GOTO 10
         ELSEIF ( TYPE(1:2) .EQ. 'K8' ) THEN
            DO 28 J = 1 , NBLIGN
               IF ( ZL(JLOGQ+J-1) ) THEN
                  NOMFON = ZK8(JVALE+J-1)
                  CALL JEEXIN ( NOMFON//'.PROL', IRET )
                  IF ( IRET .NE. 0 ) GOTO 100
                  GOTO 10
               ENDIF
 28         CONTINUE
            GOTO 10
         ELSE
            GOTO 10
         ENDIF
C
C        --- TRAITEMENT DES FONCTIONS ---
C
 100     CONTINUE
         NOMOBJ = '&&TBINFO.NOM_FONC'
         CALL TBEXVE ( TABLE, NOPARA, NOMOBJ, 'V', NBFONC, TYPVAL )
         CALL JEVEUO ( NOMOBJ, 'L', JFONC )
         DO 200 IOC = 1 , NBFONC
            IF ( TYPVAL(1:3) .EQ. 'K80' ) THEN
               NOMFON = ZK80(JFONC-1+IOC)
            ELSEIF ( TYPVAL(1:3) .EQ. 'K32' ) THEN
               NOMFON = ZK32(JFONC-1+IOC)
            ELSEIF ( TYPVAL(1:3) .EQ. 'K24' ) THEN
               NOMFON = ZK24(JFONC-1+IOC)
            ELSEIF ( TYPVAL(1:3) .EQ. 'K16' ) THEN
               NOMFON = ZK16(JFONC-1+IOC)
            ELSEIF ( TYPVAL(1:2) .EQ. 'K8'  ) THEN
               NOMFON = ZK8(JFONC-1+IOC)
            ENDIF
            CALL FOIMPR ( NOMFON, IMPR, FICHIE, 0, ' ' )
 200     CONTINUE
C
 10   CONTINUE
C
 9999 CONTINUE
C
      CALL JEDEMA()
      END
