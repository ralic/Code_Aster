      SUBROUTINE IRGMCN ( CHAMSY, IFI, NOMCON, ORDR, NBORDR, COORD,
     +                    CONNX, POINT, NJVPOI, NJVSEG, NJVTRI, NJVQUA,
     +                    NJVTET, NJVPYR, NJVPRI, NJVHEX,
     +                    NBPOI, NBSEG, NBTRI, NBQUA, NBTET, NBPYR, 
     +                    NBPRI, NBHEX, NBCMPI, NOMCMP, LRESU, PARA,
     +                    VERSIO )
      IMPLICIT NONE
      INTEGER        NBPOI, NBSEG, NBTRI, NBTET, IFI, NBORDR, NBCMPI
      INTEGER        NBQUA, NBPYR, NBPRI, NBHEX, VERSIO
      INTEGER        ORDR(*), CONNX(*), POINT(*)
      REAL*8         COORD(*), PARA(*)
      LOGICAL        LRESU
      CHARACTER*(*)  NOMCON,CHAMSY,NJVPOI,NJVSEG,NJVTRI,NJVTET,NOMCMP(*)
     +              ,NJVQUA,NJVPYR,NJVPRI,NJVHEX
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 07/01/2003   AUTEUR JMBHH01 J.M.PROIX 
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
C TOLE CRP_21
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
C        NJVPOI : NOM JEVEUX DEFINISSANT LES ELEMENTS POI1 DU MAILLAGE
C        NJVSEG : NOM JEVEUX DEFINISSANT LES ELEMENTS SEG2 DU MAILLAGE
C        NJVTRI : NOM JEVEUX DEFINISSANT LES ELEMENTS TRI3 DU MAILLAGE
C        NJVQUA : NOM JEVEUX DEFINISSANT LES ELEMENTS QUA4 DU MAILLAGE
C        NJVTET : NOM JEVEUX DEFINISSANT LES ELEMENTS TET4 DU MAILLAGE
C        NJVPYR : NOM JEVEUX DEFINISSANT LES ELEMENTS PYR5 DU MAILLAGE
C        NJVPRI : NOM JEVEUX DEFINISSANT LES ELEMENTS PRI6 DU MAILLAGE
C        NJVHEX : NOM JEVEUX DEFINISSANT LES ELEMENTS HEX8 DU MAILLAGE
C        NBPOI  : NOMBRE D'ELEMENTS POI1 DU MAILLAGE
C        NBSEG  : NOMBRE D'ELEMENTS SEG2 DU MAILLAGE
C        NBTRI  : NOMBRE D'ELEMENTS TRI3 DU MAILLAGE
C        NBQUA  : NOMBRE D'ELEMENTS QUA4 DU MAILLAGE
C        NBTET  : NOMBRE D'ELEMENTS TET4 DU MAILLAGE
C        NBPYR  : NOMBRE D'ELEMENTS PYR5 DU MAILLAGE
C        NBPRI  : NOMBRE D'ELEMENTS PRI6 DU MAILLAGE
C        NBHEX  : NOMBRE D'ELEMENTS HEXA8 DU MAILLAGE
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
      INTEGER      IOR, IBID, K, NCMP, IRET, NBORD2, JNCMP, NCMPU
      INTEGER      JTABC, JTABV, JTABL, JTABD, JCNSK, JCNSD
      LOGICAL      SCAL, VECT, TENS
      CHARACTER*1  TSCA
      CHARACTER*8  K8B, NOMGD, NOCMP
      CHARACTER*19 NOCH19, CHAMPS
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
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
     &     //'VISUALISER SEPAREMENT AVEC GMSH (VERSION 1.36)')
         ENDIF
C
C        ECRITURE DE L'ENTETE DE View
C        **************************** 
C      
         NOCMP = 'TENSEUR '
         CALL IRGMPV ( IFI, LRESU, NOMCON, CHAMSY, NBORD2, PARA, NOCMP,
     +                 NBPOI, NBSEG, NBTRI, NBQUA, NBTET, NBPYR, NBPRI,
     +                 NBHEX, .FALSE., .FALSE., TENS, VERSIO )

         IF ( NBPOI .NE. 0 ) THEN
          CALL IRGNTE ( IFI, NBORD2, COORD, CONNX, POINT, NJVPOI, NBPOI,
     +                  ZI(JTABV), ZI(JTABD) )
         ENDIF
C
         IF ( NBSEG .NE. 0 ) THEN
          CALL IRGNTE ( IFI, NBORD2, COORD, CONNX, POINT, NJVSEG, NBSEG,
     +                  ZI(JTABV), ZI(JTABD) )
         ENDIF

         IF ( NBTRI .NE. 0 ) THEN
          CALL IRGNTE ( IFI, NBORD2, COORD, CONNX, POINT, NJVTRI, NBTRI,
     +                  ZI(JTABV), ZI(JTABD) )
         ENDIF

         IF ( NBQUA .NE. 0 ) THEN
          CALL IRGNTE ( IFI, NBORD2, COORD, CONNX, POINT, NJVQUA, NBQUA,
     +                  ZI(JTABV), ZI(JTABD) )
         ENDIF

         IF ( NBTET .NE. 0 ) THEN
          CALL IRGNTE ( IFI, NBORD2, COORD, CONNX, POINT, NJVTET, NBTET,
     +                  ZI(JTABV), ZI(JTABD) )
         ENDIF

         IF ( NBPYR .NE. 0 ) THEN
          CALL IRGNTE ( IFI, NBORD2, COORD, CONNX, POINT, NJVPYR, NBPYR,
     +                  ZI(JTABV), ZI(JTABD) )
         ENDIF

         IF ( NBPRI .NE. 0 ) THEN
          CALL IRGNTE ( IFI, NBORD2, COORD, CONNX, POINT, NJVPRI, NBPRI,
     +                  ZI(JTABV), ZI(JTABD) )
         ENDIF

         IF ( NBHEX .NE. 0 ) THEN
          CALL IRGNTE ( IFI, NBORD2, COORD, CONNX, POINT, NJVHEX, NBHEX,
     +                  ZI(JTABV), ZI(JTABD) )
         ENDIF
C
C        FIN D'ECRITURE DE View
C        ********************** 
C      
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
     +                 NBPOI, NBSEG, NBTRI, NBQUA, NBTET, NBPYR, NBPRI,
     +                 NBHEX, .FALSE., VECT, TENS, VERSIO )
C
C
C        BOUCLE SUR LES ELEMENTS DE TYPES POI1
C        -------------------------------------
         IF ( NBPOI .NE. 0 ) THEN
          CALL IRGNP1 ( IFI, NBORD2, COORD, CONNX, POINT, NJVPOI, NBPOI,
     +                  ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
         ENDIF
C
C        BOUCLE SUR LES ELEMENTS DE TYPES SEG2
C        -------------------------------------
         IF ( NBSEG .NE. 0 ) THEN
          CALL IRGNS1 ( IFI, NBORD2, COORD, CONNX, POINT, NJVSEG, NBSEG,
     +                 ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
         ENDIF
C
C        BOUCLE SUR LES ELEMENTS DE TYPES TRIA3
C        --------------------------------------
         IF ( NBTRI .NE. 0 ) THEN
          CALL IRGNT1 ( IFI, NBORD2, COORD, CONNX, POINT, NJVTRI, NBTRI,
     +                 ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
         ENDIF
C
C        BOUCLE SUR LES ELEMENTS DE TYPES QUA4
C        --------------------------------------
         IF ( NBQUA .NE. 0 ) THEN
          CALL IRGNQ1 ( IFI, NBORD2, COORD, CONNX, POINT, NJVQUA, NBQUA,
     +                 ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
         ENDIF
C
C        BOUCLE SUR LES ELEMENTS DE TYPES TETRA4
C        ---------------------------------------
         IF ( NBTET .NE. 0 ) THEN
          CALL IRGNE1 ( IFI, NBORD2, COORD, CONNX, POINT, NJVTET, NBTET,
     +                 ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
         ENDIF
C
C        BOUCLE SUR LES ELEMENTS DE TYPES PYRAM5
C        ---------------------------------------
         IF ( NBPYR .NE. 0 ) THEN
          CALL IRGNY1 ( IFI, NBORD2, COORD, CONNX, POINT, NJVPYR, NBPYR,
     +                 ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
         ENDIF
C
C        BOUCLE SUR LES ELEMENTS DE TYPES PRI6
C        -------------------------------------
         IF ( NBPRI .NE. 0 ) THEN
          CALL IRGNR1 ( IFI, NBORD2, COORD, CONNX, POINT, NJVPRI, NBPRI,
     +                 ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
         ENDIF
C
C        BOUCLE SUR LES ELEMENTS DE TYPES HEXA8
C        --------------------------------------
         IF ( NBHEX .NE. 0 ) THEN
          CALL IRGNH1 ( IFI, NBORD2, COORD, CONNX, POINT, NJVHEX, NBHEX,
     +                 ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
         ENDIF
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
     +                 NBPOI, NBSEG, NBTRI ,NBQUA, NBTET, NBPYR, NBPRI,
     +                 NBHEX, SCAL, .FALSE., TENS, VERSIO )
C
C
C        BOUCLE SUR LES ELEMENTS DE TYPES POI1
C        -------------------------------------
         IF ( NBPOI .NE. 0 ) THEN
          CALL IRGNP2 ( IFI, NBORD2, COORD, CONNX, POINT, NOCMP, NJVPOI,
     +              NBPOI, ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
         ENDIF
C
C        BOUCLE SUR LES ELEMENTS DE TYPES SEG2
C        -------------------------------------
         IF ( NBSEG .NE. 0 ) THEN
          CALL IRGNS2 ( IFI, NBORD2, COORD, CONNX, POINT, NOCMP, NJVSEG,
     +              NBSEG, ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
         ENDIF
C
C        BOUCLE SUR LES ELEMENTS DE TYPES TRIA3
C        --------------------------------------
         IF ( NBTRI .NE. 0 ) THEN
          CALL IRGNT2 ( IFI, NBORD2, COORD, CONNX, POINT, NOCMP, NJVTRI,
     +              NBTRI, ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
         ENDIF
C
C        BOUCLE SUR LES ELEMENTS DE TYPES QUA4
C        --------------------------------------
         IF ( NBQUA .NE. 0 ) THEN
          CALL IRGNQ2 ( IFI, NBORD2, COORD, CONNX, POINT, NOCMP, NJVQUA,
     +              NBQUA, ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
         ENDIF
C
C        BOUCLE SUR LES ELEMENTS DE TYPES TETRA4
C        ---------------------------------------
         IF ( NBTET .NE. 0 ) THEN
          CALL IRGNE2 ( IFI, NBORD2, COORD, CONNX, POINT, NOCMP, NJVTET,
     +              NBTET, ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
         ENDIF
C
C        BOUCLE SUR LES ELEMENTS DE TYPES PYRAM5
C        ---------------------------------------
         IF ( NBPYR .NE. 0 ) THEN
          CALL IRGNY2 ( IFI, NBORD2, COORD, CONNX, POINT, NOCMP, NJVPYR,
     +              NBPYR, ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
         ENDIF
C
C        BOUCLE SUR LES ELEMENTS DE TYPES PRI6
C        -------------------------------------
         IF ( NBPRI .NE. 0 ) THEN
          CALL IRGNR2 ( IFI, NBORD2, COORD, CONNX, POINT, NOCMP, NJVPRI,
     +              NBPRI,ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
         ENDIF
C
C        BOUCLE SUR LES ELEMENTS DE TYPES HEXA8
C        --------------------------------------
         IF ( NBHEX .NE. 0 ) THEN
          CALL IRGNH2 ( IFI, NBORD2, COORD, CONNX, POINT, NOCMP, NJVHEX,
     +              NBHEX, ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
         ENDIF

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
