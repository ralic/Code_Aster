      SUBROUTINE TBIMPA ( TABLE, PARIM, NPARIM, PARPG, NPARPG )
      IMPLICIT   NONE
      INTEGER             NPARIM, NPARPG
      CHARACTER*19        TABLE
      CHARACTER*24        PARIM, PARPG
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/10/2000   AUTEUR CIBHHLV L.VIVAN 
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
C     OPERATEUR  IMPR_TABLE , TRAITEMENT DU MOT CLE "TOUT_PARA"
C                             TRAITEMENT DU MOT CLE "NOM_PARA"
C                             TRAITEMENT DU MOT CLE "PAGINATION"
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
      INTEGER      ITBNP, ITBLP, NBPARA, N1, JPAIM, JPAPG, I, J, IRET
      LOGICAL      ARRET
      CHARACTER*8  K8B
      CHARACTER*16 INPAR, JNPAR
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL JEVEUO ( TABLE//'.TBNP' , 'L', ITBNP )
      NBPARA = ZI(ITBNP)
      CALL JEVEUO ( TABLE//'.TBLP' , 'L', ITBLP )
C     ------------------------------------------------------------------
C
C     --- NOM_PARA , TOUT_PARA ---
C
C     ------------------------------------------------------------------
C
      CALL GETVTX ( ' ', 'NOM_PARA'  , 1,1,0, K8B, N1 )
      IF ( N1 .NE. 0 ) THEN
         NPARIM = -N1
         CALL WKVECT ( PARIM, 'V V K24', NPARIM, JPAIM )
         CALL GETVTX ( ' ', 'NOM_PARA', 1,1,NPARIM, ZK24(JPAIM), N1 )
         ARRET = .FALSE.
         DO 10 I = 1 , NPARIM
            INPAR = ZK24(JPAIM+I-1)
            DO 12 J = 1 , NBPARA
               JNPAR = ZK24(ITBLP+4*(J-1))
               IF ( INPAR .EQ. JNPAR ) THEN
                  GOTO 10
               ENDIF
 12         CONTINUE
            ARRET = .TRUE.
            CALL UTDEBM('A','IMPR_TABLE','ERREUR DANS LES DONNEES') 
            CALL UTIMPK('L',' LE PARAMETRE N''EXISTE PAS : ',1,INPAR)
            CALL UTFINM( )
 10      CONTINUE
         IF ( ARRET ) THEN
            CALL UTDEBM('F','IMPR_TABLE',' MAUVAISE DEFINITION DES PAR')
            CALL UTIMPK('S','AMETRES D''IMPRESSION ',NPARIM,ZK24(JPAIM))
            CALL UTFINM( )
         ENDIF
      ELSE
         NPARIM = NBPARA
         CALL WKVECT ( PARIM, 'V V K24', NPARIM, JPAIM )
         DO 14 I = 1 , NBPARA
            ZK24(JPAIM+I-1) = ZK24(ITBLP+4*(I-1))
 14      CONTINUE
      ENDIF
C     ------------------------------------------------------------------
C
C     --- PAGINATION ---
C
C     ------------------------------------------------------------------
C
      CALL GETVTX ( ' ', 'PAGINATION'  , 1,1,0, K8B, N1 )
      IF ( N1 .NE. 0 ) THEN
         NPARPG = -N1
         CALL WKVECT ( PARPG, 'V V K24', NPARPG, JPAPG )
         CALL GETVTX ( ' ', 'PAGINATION', 1,1,NPARPG, ZK24(JPAPG), N1 )
         ARRET = .FALSE.
         DO 20 I = 1 , NPARPG
            INPAR = ZK24(JPAPG+I-1)
            DO 22 J = 1 , NBPARA
               JNPAR = ZK24(ITBLP+4*(J-1))
               IF ( INPAR .EQ. JNPAR ) THEN
                  GOTO 20
               ENDIF
 22         CONTINUE
            ARRET = .TRUE.
            CALL UTDEBM('A','IMPR_TABLE','ERREUR DANS LES DONNEES') 
            CALL UTIMPK('L',' LE PARAMETRE N''EXISTE PAS : ',1,INPAR)
            CALL UTFINM( )
 20      CONTINUE
         IF ( ARRET ) THEN
            CALL UTDEBM('F','IMPR_TABLE',' MAUVAISE DEFINITION DES PAR')
            CALL UTIMPK('S','AMETRES DE PAGINATION ',NPARPG,ZK24(JPAPG))
            CALL UTFINM( )
         ENDIF
C
C ------ ZK24(JPAPG) DOIT ETRE INCLUS DANS ZK24(JPAIM)
C
         CALL KNINCL ( 24, ZK24(JPAPG),NPARPG, ZK24(JPAIM),NPARIM, IRET)
         IF ( IRET .NE. 0 ) THEN
            CALL UTDEBM('F','IMPR_TABLE',' MAUVAISE DEFINITION DES PAR')
            CALL UTIMPK('S','AMETRES DE PAGINATION ',NPARPG,ZK24(JPAPG))
            CALL UTIMPK('L','LE PARAMETRE ',1,ZK24(JPAPG+IRET-1))
            CALL UTIMPK('S','N''EST PAS DEFINI DANS ',1,'NOM_PARA')
            CALL UTFINM( )
         ENDIF
C
      ELSE
         NPARPG = 0
         CALL WKVECT ( PARPG, 'V V K24', 1, JPAPG )
      ENDIF
C
      CALL JEDEMA()
      END
