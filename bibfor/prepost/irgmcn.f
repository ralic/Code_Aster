      SUBROUTINE IRGMCN ( CHAMSY, IFI, NOMCON, ORDR, NBORDR, COORD,
     +                    CONNX, POINT, NOBJ, NBEL,
     +                    NBCMPI, NOMCMP, LRESU, PARA,
     +                    VERSIO )
      IMPLICIT NONE
      INTEGER        IFI, NBORDR, NBCMPI, VERSIO
      INTEGER        ORDR(*), CONNX(*), POINT(*)
      REAL*8         COORD(*), PARA(*)
      LOGICAL        LRESU
      CHARACTER*(*)  NOMCON,CHAMSY,NOMCMP(*)
C     NBRE, NOM D'OBJET POUR CHAQUE TYPE D'ELEMENT
      INTEGER    NELETR
      PARAMETER (NELETR =  8)
      INTEGER      TORD(NELETR)
      INTEGER      NBEL(*)
      CHARACTER*24 NOBJ(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/02/2004   AUTEUR MCOURTOI M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C
C        IMPRESSION D'UN CHAM_NO AU FORMAT GMSH
C
C        CHAMSY : NOM SYMBOLIQUE DU CHAM_NO A ECRIRE
C        IFI    : NUMERO D'UNITE LOGIQUE DU FICHIER DE SORTIE GMSH
C        NOMCON : NOM DU CONCEPT A IMPRIMER
C        ORDR   : LISTE DES NUMEROS D'ORDRE A IMPRIMER
C        NBORDR : NOMBRE DE NUMEROS D'ORDRE DANS LE TABLEAU ORDR
C        COORD  : VECTEUR COORDONNEES DES NOEUDS DU MAILLAGE
C        CONNX  : VECTEUR CONNECTIVITES DES NOEUDS DU MAILLAGE
C        POINT  : VECTEUR DU NOMBRE DE NOEUDS DES MAILLES DU MAILLAGE
C        NOBJ(i): NOM JEVEUX DEFINISSANT LES ELEMENTS DU MAILLAGE
C        NBEL(i): NOMBRE D'ELEMENTS DU MAILLAGE DE TYPE i
C        NBCMPI : NOMBRE DE COMPOSANTES DEMANDEES A IMPRIMER
C        NOMCMP : NOMS DES COMPOSANTES DEMANDEES A IMPRIMER
C        LRESU  : LOGIQUE INDIQUANT SI NOMCON EST UNE SD RESULTAT
C        PARA   : VALEURS DES VARIABLES D'ACCES (INST, FREQ)
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      I,INE
      INTEGER      IOR, IBID, K, NCMP, IRET, NBORD2, JNCMP, NCMPU
      INTEGER      JTABC, JTABV, JTABL, JTABD, JCNSK
      LOGICAL      SCAL, VECT, TENS
      CHARACTER*1  TSCA
      CHARACTER*8  K8B, NOMGD, NOCMP, TBCMP(3)
      CHARACTER*19 NOCH19, CHAMPS
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- ORDRE D'IMPRESSION DES VALEURS
      CALL IRGMOR(TORD,VERSIO)
C
      NBORD2 = MAX(1,NBORDR)

      CALL WKVECT ( '&&IRGMCN.CNSD', 'V V I', NBORD2, JTABD )
      CALL WKVECT ( '&&IRGMCN.CNSC', 'V V I', NBORD2, JTABC )
      CALL WKVECT ( '&&IRGMCN.CNSV', 'V V I', NBORD2, JTABV )
      CALL WKVECT ( '&&IRGMCN.CNSL', 'V V I', NBORD2, JTABL )
C
C
      DO 100 IOR = 1 , NBORD2  
         IF ( LRESU ) THEN 
           CALL RSEXCH ( NOMCON, CHAMSY, ORDR(IOR), NOCH19, IRET )
           IF( IRET .NE. 0 ) GOTO 100
         ELSE
           NOCH19 = NOMCON
         ENDIF
         CALL CODENT ( IOR, 'D0', K8B )
         CHAMPS = '&&IRGMCN.CH'//K8B
         CALL CNOCNS ( NOCH19, 'V', CHAMPS )
         CALL JEVEUO ( CHAMPS//'.CNSK', 'L', JCNSK )
         CALL JEVEUO ( CHAMPS//'.CNSD', 'L', ZI(JTABD+IOR-1) )
         CALL JEVEUO ( CHAMPS//'.CNSC', 'L', ZI(JTABC+IOR-1) )
         CALL JEVEUO ( CHAMPS//'.CNSV', 'L', ZI(JTABV+IOR-1) )
         CALL JEVEUO ( CHAMPS//'.CNSL', 'L', ZI(JTABL+IOR-1) )
C
         NOMGD = ZK8(JCNSK-1+2)
         CALL DISMOI('F','TYPE_SCA',NOMGD,'GRANDEUR',IBID,TSCA,IBID)
         IF ( TSCA .NE. 'R' ) THEN
            CALL UTMESS('F','IRGMCN','ON IMPRIME QUE DES CHAMPS REELS')
         ENDIF
C
 100  CONTINUE
C
C --- 1/ ON RECHERCHE LES COMPOSANTES DX, DY, DZ
C        ==> IMPRESSION D'UN CHAMP VECTORIEL
C --- 2/ POUR LES AUTRES COMPOSANTES
C        ==> IMPRESSION D'UN CHAMP SCALAIRE PAR COMPOSANTE
C
      SCAL = .FALSE.
      VECT = .FALSE.
      TENS = .FALSE.
      NCMP = ZI(ZI(JTABD)-1+2)
      NCMPU = 0
      CALL WKVECT ( '&&IRGMCN.NOCMP', 'V V K8', NCMP, JNCMP )
      IF ( NBCMPI .EQ. 0 ) THEN
         DO 200 K = 1 , NCMP
            NOCMP = ZK8(ZI(JTABC)-1+K)
            IF ( NOCMP .EQ. 'DX' ) THEN
               VECT = .TRUE.
            ELSEIF ( NOCMP .EQ. 'DY' ) THEN
               VECT = .TRUE.
            ELSEIF ( NOCMP .EQ. 'DZ' ) THEN
               VECT = .TRUE.
            ELSEIF ( CHAMSY(1:2) .EQ. 'SI' ) THEN
               IF (VERSIO.EQ.2) THEN
                 TENS = .TRUE.
               ENDIF
            ELSEIF ( CHAMSY(1:2) .EQ. 'EP' ) THEN
               IF (VERSIO.EQ.2) THEN
                 TENS = .TRUE.
               ENDIF
            ELSE
               SCAL = .TRUE.
               NCMPU = NCMPU + 1
               ZK8(JNCMP+NCMPU-1) = NOCMP
            ENDIF
 200     CONTINUE
      ELSE
         DO 210 K = 1 , NBCMPI
            NOCMP = NOMCMP(K)
            SCAL = .TRUE.
            NCMPU = NCMPU + 1
            ZK8(JNCMP+NCMPU-1) = NOCMP
 210     CONTINUE
      ENDIF
C
C ----------------------------------------------------------------------
C          IMPRESSION D'UN CHAMP TENSORIEL 
C ----------------------------------------------------------------------
      IF ( TENS ) THEN
         IF (VERSIO.EQ.2) THEN
            CALL UTMESS('A','IMPR_RESU','ATTENTION, IL FAUT SPECIFIER '
     &     //'LES NOMS DES COMPOSANTES DU TENSEUR POUR POUVOIR LES '
     &     //'VISUALISER SEPAREMENT AVEC GMSH')
         ENDIF
C
C        ECRITURE DE L'ENTETE DE View
C        **************************** 
         NOCMP = 'TENSEUR '
         CALL IRGMPV ( IFI, LRESU, NOMCON, CHAMSY, NBORD2, PARA, NOCMP,
     +                 NBEL, .FALSE., .FALSE., TENS, VERSIO )

C ---    BOUCLE SUR LES TYPES D'ELEMENTS SI NBEL>0
C        ON A RECUPERE L'ORDRE D'IMPRESSION PAR IRGMOR
         DO 101 INE=1,NELETR
            I=TORD(INE)
            IF(NBEL(I).NE.0)THEN
               CALL IRGNTE ( IFI, NBORD2, COORD, CONNX, POINT,
     +                       NOBJ(I), NBEL(I),
     +                       ZI(JTABV), ZI(JTABD) )
            ENDIF
 101     CONTINUE
C
C        FIN D'ECRITURE DE View
C        ********************** 
         WRITE(IFI,1000) '$EndView'
C
      ENDIF
C
C
C ----------------------------------------------------------------------
C          IMPRESSION D'UN CHAMP VECTORIEL ( CMP = DX, DY, DZ )
C ----------------------------------------------------------------------
      IF ( VECT ) THEN
C
C        ECRITURE DE L'ENTETE DE View
C        **************************** 
C
         NOCMP = 'VECTEUR '
         CALL IRGMPV ( IFI, LRESU, NOMCON, CHAMSY, NBORD2, PARA, NOCMP,
     +                 NBEL, .FALSE., VECT, TENS, VERSIO )
C
C        LISTE DES COMPOSANTES
         TBCMP(1)='DX      '
         TBCMP(2)='DY      '
         TBCMP(3)='DZ      '
C
C ---    BOUCLE SUR LES TYPES D'ELEMENTS SI NBEL>0
C        ON A RECUPERE L'ORDRE D'IMPRESSION PAR IRGMOR
         DO 102 INE=1,NELETR
            I=TORD(INE)
            IF(NBEL(I).NE.0)THEN
               CALL IRGNAL(IFI, NBORD2, COORD, CONNX, POINT,
     +                     TBCMP, 3, I, NOBJ(I), NBEL(I),
     +                     ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
            ENDIF
 102     CONTINUE
C
C        FIN D'ECRITURE DE View
C        ********************** 
C      
         WRITE(IFI,1000) '$EndView'
C
      ENDIF
C
C ----------------------------------------------------------------------
C           IMPRESSION D'UN CHAMP SCALAIRE ( AUTRE CMP )
C ----------------------------------------------------------------------
C
      IF ( SCAL ) THEN
         DO 300 K = 1 , NCMPU
            NOCMP = ZK8(JNCMP+K-1)
C
C        ECRITURE DE L'ENTETE DE View
C        **************************** 
C      
         CALL IRGMPV ( IFI, LRESU, NOMCON, CHAMSY, NBORD2, PARA, NOCMP,
     +                 NBEL, SCAL, .FALSE., TENS, VERSIO )
C
C        LISTE DES COMPOSANTES
         TBCMP(1)=NOCMP
C
C ---    BOUCLE SUR LES TYPES D'ELEMENTS SI NBEL>0
C        ON A RECUPERE L'ORDRE D'IMPRESSION PAR IRGMOR
         DO 103 INE=1,NELETR
            I=TORD(INE)
            IF(NBEL(I).NE.0)THEN
               CALL IRGNAL(IFI, NBORD2, COORD, CONNX, POINT,
     +                     TBCMP, 1, I, NOBJ(I), NBEL(I),
     +                     ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
            ENDIF
 103     CONTINUE
C
C        FIN D'ECRITURE DE View
C        ********************** 
C      
         WRITE(IFI,1000) '$EndView'
C
 300     CONTINUE
      ENDIF
C 
      CALL JEDETR ( '&&IRGMCN.CNSD'  )
      CALL JEDETR ( '&&IRGMCN.CNSC'  )
      CALL JEDETR ( '&&IRGMCN.CNSV'  )
      CALL JEDETR ( '&&IRGMCN.CNSL'  )
      CALL JEDETR ( '&&IRGMCN.NOCMP' )
C
      CALL JEDEMA()
C
 1000 FORMAT(A8)
C
      END
