      SUBROUTINE IRGMCN ( CHAMSY, IFI, NOMCON, ORDR, NBORDR, COORD,
     +                    CONNX, POINT, NJVPOI, NJVSEG, NJVTRI, 
     +                    NJVTET, NBPOI, NBSEG, NBTRI, NBTET,
     +                    NBCMPI, NOMCMP, LRESU, PARA )
      IMPLICIT NONE
      INTEGER        NBPOI, NBSEG, NBTRI, NBTET, IFI, NBORDR, NBCMPI
      INTEGER        ORDR(*), CONNX(*), POINT(*)
      REAL*8         COORD(*), PARA(*)
      LOGICAL        LRESU
      CHARACTER*(*)  NOMCON,CHAMSY,NJVPOI,NJVSEG,NJVTRI,NJVTET,NOMCMP(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 05/02/2002   AUTEUR JMBHH01 J.M.PROIX 
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
C        NJVPOI : NOM JEVEUX DEFINISSANT LES ELEMENTS POI1 DU MAILLAGE
C        NJVSEG : NOM JEVEUX DEFINISSANT LES ELEMENTS SEG2 DU MAILLAGE
C        NJVTRI : NOM JEVEUX DEFINISSANT LES ELEMENTS TRI3 DU MAILLAGE
C        NJVTET : NOM JEVEUX DEFINISSANT LES ELEMENTS TET4 DU MAILLAGE
C        NBPOI  : NOMBRE D'ELEMENTS POI1 DU MAILLAGE
C        NBSEG  : NOMBRE D'ELEMENTS SEG2 DU MAILLAGE
C        NBTRI  : NOMBRE D'ELEMENTS TRI3 DU MAILLAGE
C        NBTET  : NOMBRE D'ELEMENTS TET4 DU MAILLAGE
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
            ELSE
               SCAL = .TRUE.
               NCMPU = NCMPU + 1
               ZK8(JNCMP+NCMPU-1) = NOCMP
            ENDIF
 200     CONTINUE
      ELSE
         DO 210 K = 1 , NBCMPI
            NOCMP = NOMCMP(K)
            IF ( NOCMP .EQ. 'DX' ) THEN
               VECT = .TRUE.
            ELSEIF ( NOCMP .EQ. 'DY' ) THEN
               VECT = .TRUE.
            ELSEIF ( NOCMP .EQ. 'DZ' ) THEN
               VECT = .TRUE.
            ELSE
               SCAL = .TRUE.
               NCMPU = NCMPU + 1
               ZK8(JNCMP+NCMPU-1) = NOCMP
            ENDIF
 210     CONTINUE
      ENDIF
C
C ----------------------------------------------------------------------
C          IMPRESSION D'UN CHAMP VECTORIEL ( CMP = DX, DY, DZ )
C ----------------------------------------------------------------------
C
      IF ( VECT ) THEN
C
C        ECRITURE DE L'ENTETE DE View
C        **************************** 
C      
         NOCMP = 'VECTEUR '
         CALL IRGMPV ( IFI, LRESU, NOMCON, CHAMSY, NBORD2, PARA, NOCMP,
     +                 NBPOI, NBSEG, NBTRI, NBTET, .FALSE., VECT, TENS )
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
C        BOUCLE SUR LES ELEMENTS DE TYPES TETRA4
C        ---------------------------------------
         IF ( NBTET .NE. 0 ) THEN
          CALL IRGNE1 ( IFI, NBORD2, COORD, CONNX, POINT, NJVTET, NBTET,
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
     +                 NBPOI, NBSEG, NBTRI, NBTET, SCAL, .FALSE., TENS )
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
C        BOUCLE SUR LES ELEMENTS DE TYPES TETRA4
C        ---------------------------------------
         IF ( NBTET .NE. 0 ) THEN
          CALL IRGNE2 ( IFI, NBORD2, COORD, CONNX, POINT, NOCMP, NJVTET,
     +              NBTET, ZI(JTABC), ZI(JTABL), ZI(JTABV), ZI(JTABD) )
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
