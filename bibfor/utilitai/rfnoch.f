      SUBROUTINE RFNOCH
      IMPLICIT NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 21/06/2000   AUTEUR CIBHHLV L.VIVAN 
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
C
C     OPERATEUR "RECU_FONCTION"   MOT CLE "NOEUD_CHOC"
C     ------------------------------------------------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       IBID, N, NC, NG, INT, IND, NSST, IRET, JREFE 
      CHARACTER*8   NOMA, SST, BASEMO, RAIDE, NOEUD, INTITU, NOGNO
      CHARACTER*16  PARAX,PARAY, NOMCMD, TYPCON
      CHARACTER*19  LISTR, NOMFON, RESU
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL GETRES ( NOMFON, TYPCON, NOMCMD )
C
      CALL GETVTX ( ' ', 'INTITULE'  , 0,1,1, INTITU, INT  )
      CALL GETVID ( ' ', 'RESU_GENE' , 0,1,1, RESU  , N    )
      CALL GETVTX ( ' ', 'PARA_X'    , 0,1,1, PARAX , N    )
      CALL GETVTX ( ' ', 'PARA_Y'    , 0,1,1, PARAY , N    )
      CALL GETVID ( ' ', 'LIST_PARA' , 0,1,1, LISTR , IND  )
      CALL GETVTX ( ' ', 'SOUS_STRUC', 0,1,1, SST   , NSST )
C
      CALL GETVID ( ' ', 'NOEUD_CHOC'   , 0,1,1, NOEUD, NC )
      CALL GETVID ( ' ', 'GROUP_NO_CHOC', 0,1,1, NOGNO, NG )
C
      IF ( NC .NE. 0 ) THEN
C
        CALL FOCRCH ( NOMFON, RESU, NOEUD, PARAX, PARAY, 'G', INT,
     +                INTITU, IND, LISTR, SST, NSST, IRET )
C
      ELSE
C
         CALL JEVEUO ( RESU//'.REFE', 'L', JREFE )
         BASEMO = ZK24(JREFE)(1:8)
         CALL JEVEUO ( BASEMO//'           .REFE', 'L', JREFE )
         RAIDE = ZK24(JREFE+2)(1:8)
         CALL DISMOI('F','NOM_MAILLA',RAIDE,'MATR_ASSE',IBID,NOMA,IRET)
C
         CALL UTNONO ( ' ', NOMA, 'NOEUD', NOGNO, NOEUD, IRET )
         IF (IRET.EQ.10) THEN
            CALL UTMESS('F','OP0090','LE GROUP_NO : '//NOGNO//
     +                  'N''EXISTE PAS.')
         ELSEIF (IRET.EQ.1) THEN
            CALL UTDEBM('A','OP0090','TROP DE NOEUDS DANS LE GROUP_NO')
            CALL UTIMPK('L','  NOEUD UTILISE: ',1,NOEUD)
            CALL UTFINM()
         ENDIF
C
         CALL FOCRCH ( NOMFON, RESU, NOEUD, PARAX, PARAY, 'G', INT,
     +                 INTITU, IND, LISTR, SST, NSST, IRET )
      ENDIF
C
      CALL JEDEMA()
      END
