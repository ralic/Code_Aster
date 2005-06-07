      SUBROUTINE OP0130 ( IERR )
      IMPLICIT   NONE
      INTEGER             IERR
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 14/05/2002   AUTEUR DURAND C.DURAND 
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
C     OPERATEUR "POST_DYNA_MODA_T"
C
C ----------------------------------------------------------------------
C     ---- DEBUT DES COMMUNS JEVEUX ------------------------------------
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
C     ---- FIN DES COMMUNS JEVEUX --------------------------------------
      INTEGER      NBBLOC, NBCLAS, N1, N2, I, JDESC, NBIND
      REAL*8       TDEBUT, TFIN, OFFSET, TREPOS
      CHARACTER*8  TRANGE, NOEU, CMP, NOMRES
      CHARACTER*16 NOMCMD, CONCEP, KOPTIO
      LOGICAL LOPTIO
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      NOMRES = ' '
      CALL GETRES(NOMRES,CONCEP,NOMCMD)
      CALL INFMAJ
C
      CALL GETVID(' ','RESU_GENE',0,1,1,TRANGE,N1)
      CALL JEVEUO(TRANGE//'           .DESC','L',JDESC)
C
      CALL GETFAC('CHOC',NBIND)
      IF (NBIND.NE.0) THEN
         DO 10 I = 1,NBIND
            CALL GETVIS('CHOC','NB_BLOC'    ,I,1,1,NBBLOC,N1)
            CALL GETVR8('CHOC','INST_INIT'  ,I,1,1,TDEBUT,N1)
            CALL GETVR8('CHOC','INST_FIN'   ,I,1,1,TFIN,  N1)
            CALL GETVR8('CHOC','SEUIL_FORCE',I,1,1,OFFSET,N1)
            CALL GETVR8('CHOC','DUREE_REPOS',I,1,1,TREPOS,N1)
            CALL GETVTX('CHOC','OPTION'     ,I,1,1,KOPTIO,N1)
            CALL GETVIS('CHOC','NB_CLASSE'  ,I,1,1,NBCLAS,N1)
            IF (KOPTIO(1:6).EQ.'USURE') THEN
              LOPTIO = .TRUE.
            ELSE
              LOPTIO = .FALSE.
            ENDIF
            IF (ZI(JDESC) .EQ. 2) THEN
              CALL POCHOC (TRANGE, NBBLOC, TDEBUT, TFIN, OFFSET, TREPOS,
     +                     NBCLAS, NOMRES, LOPTIO )
            ELSE IF (ZI(JDESC) .EQ. 3) THEN
              CALL POCHPV (TRANGE, NBBLOC, TDEBUT, TFIN, OFFSET, TREPOS,
     +                     NBCLAS, NOMRES, LOPTIO )
            ENDIF
 10      CONTINUE
      ENDIF
C
      CALL GETFAC('RELA_EFFO_DEPL',NBIND)
      IF (NBIND.NE.0 .AND. ZI(JDESC+3).NE.0 ) THEN
         DO 20 I = 1,NBIND
            CALL GETVID('RELA_EFFO_DEPL','NOEUD'  ,I,1,1,NOEU,N2)
            CALL GETVTX('RELA_EFFO_DEPL','NOM_CMP',I,1,1,CMP ,N2)
C
            CALL POREFD (TRANGE, NOEU, CMP, NOMRES )
 20      CONTINUE
      ENDIF
C
      CALL TITRE
C
      CALL JEDEMA()
      END
