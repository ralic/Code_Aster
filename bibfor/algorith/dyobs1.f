      SUBROUTINE DYOBS1 ( MAILLA, NBOCC, NTOBS )
      IMPLICIT   NONE
      INTEGER             NBOCC, NTOBS
      CHARACTER*8         MAILLA
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/05/2000   AUTEUR VABHHTS J.PELLET 
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
C ----------------------------------------------------------------------
C     SAISIE DU MOT CLE FACTEUR "OBSERVATION"
C            VERIFICATION DES DONNEES
C            COMPTAGE DES FONCTIONS
C ----------------------------------------------------------------------
C     --- DEBUT DECLARATIONS NORMALISEES JEVEUX ------------------------
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
      CHARACTER*32       JEXNUM , JEXNOM
C     --- FIN DECLARATIONS NORMALISEES JEVEUX --------------------------
      INTEGER      N1, N2, N3, N4, INO, IMA, IGNO, IOCC, NBN,
     +             IBID, IRET, NCHP, NCMP, I, NBNO, NBMA, NBGN,
     +             JNOE, JGRN, JMAI, NBPT
      LOGICAL      CHAMNO, CHAMES, CHAMEV
      CHARACTER*8  K8B
      CHARACTER*16 NOCHP(50)
      CHARACTER*24 GRPNO, NOMNOE, NOMMAI
C DEB------------------------------------------------------------------
C
      CALL JEMARQ()
C
      GRPNO  = MAILLA//'.GROUPENO'
      NOMNOE = MAILLA//'.NOMNOE'
      NOMMAI = MAILLA//'.NOMMAI'
C
      NTOBS = 0
      DO 10 IOCC = 1 , NBOCC
C
C ------ VERIFICATION DES CHAMPS ---------------------------------------
C
         CALL GETVTX ( 'OBSERVATION', 'NOM_CHAM', IOCC,1,0, K8B, N1 )
         NCHP = -N1
         CALL GETVTX ( 'OBSERVATION','NOM_CHAM',IOCC,1,NCHP,NOCHP, N1 )
         CHAMNO = .FALSE.
         CHAMES = .FALSE.
         CHAMEV = .FALSE.
         DO 12 I = 1 , NCHP
            IF ( NOCHP(I)(1:4) .EQ. 'DEPL' ) THEN
               CHAMNO = .TRUE.
            ELSEIF ( NOCHP(I)(1:4) .EQ. 'VITE' ) THEN
               CHAMNO = .TRUE.
            ELSEIF ( NOCHP(I)(1:4) .EQ. 'ACCE' ) THEN
               CHAMNO = .TRUE.
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'SIEF_ELGA' ) THEN
               CHAMES = .TRUE.
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'VARI_ELGA' ) THEN
               CHAMEV = .TRUE.
            ENDIF
 12      CONTINUE
         IF ( CHAMES .AND. CHAMEV ) THEN
            CALL UTMESS('F','DYOBS1','ERREUR NOM_CHAM')
         ENDIF
         IF ( CHAMNO .AND. ( CHAMES .OR. CHAMEV ) ) THEN
            CALL UTMESS('F','DYOBS1','ERREUR NOM_CHAM')
         ENDIF
C
C ------ VERIFICATION DES COMPOSANTES ----------------------------------
C
         CALL GETVTX ( 'OBSERVATION', 'NOM_CMP' , IOCC,1,0, K8B , N2 )
         NCMP = -N2
C
C ------ VERIFICATION DES NOEUDS ET MAILLES ----------------------------
C
         CALL GETVID ( 'OBSERVATION','NOEUD'   , IOCC,1,0, K8B ,N1 )
         CALL GETVID ( 'OBSERVATION','GROUP_NO', IOCC,1,0, K8B ,N2 )
         CALL GETVID ( 'OBSERVATION','MAILLE'  , IOCC,1,0, K8B ,N3 )
         CALL GETVIS ( 'OBSERVATION','POINT'   , IOCC,1,0, IBID,N4 )
         IF ( N1 .NE. 0 ) THEN
            NBNO = -N1
            CALL WKVECT ('&&DYOBS1.LIST_NOEU','V V K8',NBNO,JNOE)
            CALL GETVID ( 'OBSERVATION','NOEUD', IOCC,1,NBNO,
     +                                                    ZK8(JNOE),N1)
            DO 20 INO = 0 , NBNO-1
               CALL JEEXIN ( JEXNOM(NOMNOE,ZK8(JNOE+INO)) , IRET )
               IF ( IRET .EQ. 0 ) THEN
                  CALL UTDEBM('F','DYOBS1','ERREUR DANS LES DONNEES')
                  CALL UTIMPK('L','LE NOEUD ',1,ZK8(JNOE+INO))
                  CALL UTIMPK('S',' N''EXISTE PAS DANS ',1,MAILLA)
                  CALL UTFINM()
               ENDIF
 20         CONTINUE
         ENDIF
         IF ( N2 .NE. 0 ) THEN
            NBGN = -N2
            CALL WKVECT ('&&DYOBS1.LIST_GRNO','V V K8',NBGN,JGRN)
            CALL GETVID ( 'OBSERVATION','GROUP_NO', IOCC,1,NBGN,
     +                                                    ZK8(JGRN),N2)
            DO 22 IGNO = 0 , NBGN-1
               CALL JEEXIN ( JEXNOM(GRPNO,ZK8(JGRN+IGNO)) , IRET )
               IF ( IRET .EQ. 0 ) THEN
                  CALL UTDEBM('F','DYOBS1','ERREUR DANS LES DONNEES')
                  CALL UTIMPK('L','LE GROUP_NO ',1,ZK8(JGRN+IGNO))
                  CALL UTIMPK('S',' N''EXISTE PAS DANS ',1,MAILLA)
                  CALL UTFINM()
               ENDIF
 22         CONTINUE
         ENDIF
         IF ( N3 .NE. 0 ) THEN
            NBMA = -N3
            CALL WKVECT ('&&DYOBS1.LIST_MAIL','V V K8',NBMA,JMAI)
            CALL GETVID ( 'OBSERVATION','MAILLE', IOCC,1,NBMA,
     +                                                    ZK8(JMAI),N3)
            DO 24 IMA = 0 , NBMA-1
               CALL JEEXIN ( JEXNOM(NOMMAI,ZK8(JMAI+IMA)) , IRET )
               IF ( IRET .EQ. 0 ) THEN
                  CALL UTDEBM('F','DYOBS1','ERREUR DANS LES DONNEES')
                  CALL UTIMPK('L','LA MAILLE ',1,ZK8(JMAI+IMA))
                  CALL UTIMPK('S',' N''EXISTE PAS DANS ',1,MAILLA)
                  CALL UTFINM()
               ENDIF
 24         CONTINUE
         ENDIF
C
         NBPT = 0
         DO 16 I = 1 , NCHP
            IF ( NOCHP(I)(1:4) .EQ. 'DEPL' .OR.
     +           NOCHP(I)(1:4) .EQ. 'VITE' .OR.
     +           NOCHP(I)(1:4) .EQ. 'ACCE' ) THEN
               IF ( N1 .NE. 0 ) THEN
                  NBPT = NBPT + NBNO
               ELSEIF ( N2 .NE. 0 ) THEN
                  DO 18 IGNO = 0 , NBGN-1
                     CALL JELIRA ( JEXNOM(GRPNO,ZK8(JGRN+IGNO)),
     +                                              'LONMAX', NBN, K8B )
                     NBPT = NBPT + NBN
 18               CONTINUE
               ELSE
                  CALL UTDEBM('F','DYOBS1','ERREUR DANS LES DONNEES')
                  CALL UTIMPK('L','POUR "NOM_CHAM" ',1,NOCHP(I)(1:4))
                  CALL UTIMPK('S',' IL FAUT RENSEIGNER ',1,'NOEUD')
                  CALL UTIMPK('S',' OU ',1,'GROUP_NO')
                  CALL UTFINM()
               ENDIF
C
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'SIEF_ELGA' .OR.
     +               NOCHP(I)(1:9) .EQ. 'VARI_ELGA' ) THEN
               IF ( N3*N4 .EQ. 0 ) THEN
                  CALL UTDEBM('F','DYOBS1','ERREUR DANS LES DONNEES')
                  CALL UTIMPK('L','POUR "NOM_CHAM" ',1,NOCHP(I))
                  CALL UTIMPK('S',' IL FAUT RENSEIGNER ',1,'MAILLE')
                  CALL UTIMPK('S',' ET ',1,'POINT')
                  CALL UTFINM()
               ELSE
                  NBPT = NBPT + ABS( N3*N4 )
               ENDIF
            ENDIF
C
 16      CONTINUE
         IF ( N1 .NE. 0 )  CALL JEDETR ( '&&DYOBS1.LIST_NOEU' )
         IF ( N2 .NE. 0 )  CALL JEDETR ( '&&DYOBS1.LIST_GRNO' )
         IF ( N3 .NE. 0 )  CALL JEDETR ( '&&DYOBS1.LIST_MAIL' )
C
C ------ NOMBRE DE FONCTIONS CREEES ------------------------------------
C
         NTOBS  = NTOBS + ( NCHP * NCMP * NBPT )
C
 10   CONTINUE
C
C
      CALL JEDEMA()
C
      END
