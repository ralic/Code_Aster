      SUBROUTINE SUILEC(SUIVCO,MAILLA,MOTCLE,
     &                  NBOCC,NBSUIV)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/02/2006   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE MABBAS M.ABBAS

      IMPLICIT     NONE
      CHARACTER*24 SUIVCO
      CHARACTER*8  MAILLA
      CHARACTER*16 MOTCLE
      INTEGER      NBOCC
      INTEGER      NBSUIV
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : SUIINI
C ----------------------------------------------------------------------
C
C SAISIE DU MOT CLE FACTEUR "SUIVI"
C
C IN  SUIVCO : NOM DE LA SD CONTENANT INFOS DE SUIVIS DDL
C IN  MAILLA : NOM DU MAILLAGE
C IN  MOTCLE : MOT-CLEF POUR LIRE INFOS DANS CATALOGUE
C OUT NBOCC  : NOMBRE OCCURRENCES MOT-CLEF SUIVI
C OUT NBSUIV : NOMBRE DE SUIVIS A FAIRE
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      CHARACTER*32 JEXNOM
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      N1,N2,N3,N4,INO,IMA,IOCC
      INTEGER      IBID,IRET,NCHP,NCMP,I,NBNO,NBMA
      INTEGER      JNOE,JMAI,NBPT
      LOGICAL      CHAMNO,CHAMES,CHAMEV
      CHARACTER*8  K8B
      CHARACTER*16 NOCHP(50)
      CHARACTER*24 NOMNOE,NOMMAI      
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- NOMBRE DE SUIVIS DANS LE MOT-CLEF FACTEUR: NBOCC
C
      NBSUIV = 0
      CALL GETFAC(MOTCLE,NBOCC)

      IF (NBOCC.EQ.0) THEN 
        GOTO 999
      ENDIF
      IF (NBOCC.GT.4) THEN
        CALL UTMESS('F','SUILEC',
     &              'LE NOMBRE DE SUIVI DDL EST LIMITE A 4 !')      
      ENDIF
C 
C --- VERIFICATION DES DONNEES ET COMPTAGE DES FONCTIONS 
C
      NOMNOE = MAILLA//'.NOMNOE'
      NOMMAI = MAILLA//'.NOMMAI'
C
      DO 10 IOCC = 1 , NBOCC
C
C ------ VERIFICATION DES CHAMPS ---------------------------------------
C
         CALL GETVTX(MOTCLE,'NOM_CHAM',IOCC,1,0,K8B,N1)
         NCHP = -N1
         CALL GETVTX(MOTCLE,'NOM_CHAM',IOCC,1,NCHP,NOCHP,N1)
         
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
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'VALE_CONT' ) THEN
               CHAMNO = .TRUE.
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'FORC_NODA' ) THEN
               CHAMNO = .TRUE.
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'SIEF_ELGA' ) THEN
               CHAMES = .TRUE.
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'VARI_ELGA' ) THEN
               CHAMEV = .TRUE.
            ENDIF
 12      CONTINUE
         IF ( CHAMES .AND. CHAMEV ) THEN
            CALL UTMESS('F','SUILEC',
     &                  'MELANGE DE CHAMPS DE NATURE '//
     &      'DIFFERENTE DANS LE MEME MOT-CLEF FACTEUR SUIVI')
         ENDIF
         IF ( CHAMNO .AND. ( CHAMES .OR. CHAMEV ) ) THEN
            CALL UTMESS('F','SUILEC',
     &                  'MELANGE DE CHAMPS DE NATURE '//
     &      'DIFFERENTE DANS LE MEME MOT-CLEF FACTEUR SUIVI')
         ENDIF
C
C ------ VERIFICATION DES COMPOSANTES ----------------------------------
C
         CALL GETVTX (MOTCLE,'NOM_CMP' ,IOCC,1,0,K8B ,N2 )
         NCMP = -N2
C
C ------ VERIFICATION DES NOEUDS ET MAILLES ----------------------------
C
         CALL GETVID(MOTCLE,'NOEUD'   ,IOCC,1,0,K8B ,N1 )
         CALL GETVID(MOTCLE,'MAILLE'  ,IOCC,1,0,K8B ,N3 )
         CALL GETVIS(MOTCLE,'POINT'   ,IOCC,1,0,IBID,N4 )
         IF ( N1 .NE. 0 ) THEN
            NBNO = -N1
            CALL WKVECT ('&&SUILEC.LIST_NOEU','V V K8',NBNO,JNOE)
            CALL GETVID (MOTCLE,'NOEUD',IOCC,1,NBNO,
     &                   ZK8(JNOE),N1)
            DO 20 INO = 0 , NBNO-1
               CALL JEEXIN ( JEXNOM(NOMNOE,ZK8(JNOE+INO)) , IRET )
               IF ( IRET .EQ. 0 ) THEN
                  CALL UTDEBM('F','SUILEC','ERREUR DANS LES DONNEES '//
     &                         'DE SUIVI')
                  CALL UTIMPK('L','LE NOEUD ',1,ZK8(JNOE+INO))
                  CALL UTIMPK('S',' N''EXISTE PAS DANS ',1,MAILLA)
                  CALL UTFINM()
               ENDIF
 20         CONTINUE
         ENDIF
         IF ( N3 .NE. 0 ) THEN
            NBMA = -N3
            CALL WKVECT ('&&SUILEC.LIST_MAIL','V V K8',NBMA,JMAI)
            CALL GETVID (MOTCLE,'MAILLE',IOCC,1,NBMA,
     &                   ZK8(JMAI),N3)
            DO 24 IMA = 0 , NBMA-1
               CALL JEEXIN ( JEXNOM(NOMMAI,ZK8(JMAI+IMA)) , IRET )
               IF ( IRET .EQ. 0 ) THEN
                  CALL UTDEBM('F','SUILEC','ERREUR DANS LES DONNEES '//
     &                         'DE SUIVI')
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
     &           NOCHP(I)(1:4) .EQ. 'VITE' .OR.
     &           NOCHP(I)(1:4) .EQ. 'ACCE' .OR.
     &           NOCHP(I)(1:9) .EQ. 'FORC_NODA' .OR.
     &           NOCHP(I)(1:9) .EQ. 'VALE_CONT'   ) THEN
               IF ( N1 .NE. 0 ) THEN
                  NBPT = NBPT + NBNO
               ELSE
                  CALL UTDEBM('F','SUILEC','ERREUR DANS LES DONNEES '//
     &                         'DE SUIVI')
                  CALL UTIMPK('L','POUR "NOM_CHAM" ',1,NOCHP(I)(1:4))
                  CALL UTIMPK('S',' IL FAUT RENSEIGNER ',1,'NOEUD')
                  CALL UTFINM()
               ENDIF
C
            ELSEIF ( NOCHP(I)(1:9) .EQ. 'SIEF_ELGA' .OR.
     &               NOCHP(I)(1:9) .EQ. 'VARI_ELGA' ) THEN
               IF ( N3*N4 .EQ. 0 ) THEN
                  CALL UTDEBM('F','SUILEC','ERREUR DANS LES DONNEES '//
     &                         'DE SUIVI')
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
C
C --- NETTOYAGE
C 
         IF ( N1 .NE. 0 )  CALL JEDETR ( '&&SUILEC.LIST_NOEU' )
         IF ( N3 .NE. 0 )  CALL JEDETR ( '&&SUILEC.LIST_MAIL' )
C
C --- NOMBRE DE DDL A SUIVRE 
C
         NBSUIV  = NBSUIV + ( NCHP * NCMP * NBPT )
C
 10   CONTINUE
C
C ----------------------------------------------------------------------
C
  999 CONTINUE
      CALL JEDEMA()

      END
