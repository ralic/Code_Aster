      SUBROUTINE OP0027 ( IER )
C RESPONSABLE CAMBIER S.CAMBIER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/10/2004   AUTEUR REZETTE C.REZETTE 
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
C     GENE_MATR_ALEA : GENERATEUR DE MATRICES GENERALISEE ALEATOIRE
C     CONSTRUITE EN UTILISANT LE PRINCIPE DU MAXIMUM D'ENTROPIE ET
C     L'INFORMATION DISPONIBLE POUR DES MATRICES GENERALISEES 
C     SYMETRIQUES DEFINIES POSITIVES.
C
C ----------------------------------------------------------------------
      IMPLICIT   NONE
      INTEGER    IER
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
      CHARACTER*32 JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      IRET, N1, NBLOC, N, I, M, IAK,IADR,IADR1,IADR2,IBID
      INTEGER      IBLO, IDESC, IALIME, IACONL, IAREFE, IAREFA, IADESC
      INTEGER      JUMP
      
      REAL*8       DELTA
      CHARACTER*8  NOMRES, NOMMAT, K8B
      CHARACTER*16 NOMCMD, CONCEP
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
C
      IER=0
C
      CALL GETRES ( NOMRES, CONCEP, NOMCMD )
C      
      CALL GETVID ( ' ', 'MATR_MOYEN', 1,1,1, NOMMAT, N1 )
      CALL GETVR8 ( ' ', 'COEF_VAR'     , 1,1,1, DELTA,  N1 )
C
      CALL GETVIS ( ' ', 'INIT_ALEA'    , 0,1,1, JUMP , N1 )       
      IF (N1 .NE. 0) CALL INIRAN(JUMP)
      
      CALL JEVEUO (NOMMAT//'           .DESC','L',IDESC)
      N = ZI(IDESC+1)     
      M = N*(N+1)/2
      
      CALL JEEXIN(NOMRES//'           .VALE',IRET)
C
      IF (IRET.EQ.0) THEN
         NBLOC = 1
         IBLO  = 1
         CALL JEVEUO(NOMMAT//'           .REFA','L',IAREFA)
C
C ------ CREATION DES BASES DE DONNEES DE LA MATRICE A GENERER.
C        SUIVANT LE MODELE DE OP0071
C
        CALL JECREC(NOMRES//'           .VALE','G V R','NU','DISPERSE',
     &                                         'CONSTANT',NBLOC)
        CALL JECROC(JEXNUM(NOMRES//'           .VALE',IBLO))
        CALL JEECRA(NOMRES//'           .VALE','LONMAX',M,K8B)
        CALL JEECRA(NOMRES//'           .VALE','DOCU',IBID,'MS')
C
         CALL WKVECT(NOMRES//'           .DESC','G V I',3,IADESC)
         ZI(IADESC)   = 2
         ZI(IADESC+1) = N
         ZI(IADESC+2) = 2
C
         CALL WKVECT ( NOMRES//'           .LIME', 'G V K8',1, IALIME)
         ZK8(IALIME) = '        '
C
         CALL WKVECT ( NOMRES//'           .CONL', 'G V R', N, IACONL)
         DO 10 I = 1 , N
            ZR(IACONL+I-1) = 1.0D0
 10      CONTINUE
C
         CALL WKVECT(NOMRES//'           .REFA', 'G V K24', 4, IAREFE)
         CALL JEECRA(NOMRES//'           .REFA', 'DOCU', IBID, 'SLCS')
         ZK24(IAREFE)   = ZK24(IAREFA)
         ZK24(IAREFE+1) = ZK24(IAREFA+1)
         ZK24(IAREFE+2) = ZK24(IAREFA+2)
C
      ENDIF
C
      IBLO = 1
      CALL JEVEUO(JEXNUM(NOMMAT//'           .VALE',IBLO),'L',IAK)
      CALL JEVEUO(JEXNUM(NOMRES//'           .VALE',IBLO),'E',IADR)
      DO 20 I=1,M
         ZR(IADR-1+I) = 0.D0
 20   CONTINUE

      CALL WKVECT ( '&&OP0027.VECTTRA1', 'V V R', M, IADR1 )
      CALL WKVECT ( '&&OP0027.VECTTRA2', 'V V R', M, IADR2 )

      CALL GEMATG ( N, DELTA, ZR(IAK), ZR(IADR), ZR(IADR1), ZR(IADR2) )

C
      CALL JEDEMA()
      END
