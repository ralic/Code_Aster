      SUBROUTINE MTDSCR(NOMMAT)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       NOMMAT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 01/02/2000   AUTEUR VABHHTS J.PELLET 
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
C     ------------------------------------------------------------------
C     ALLOCATION / DESALLOCATION DES DESCRIPTEURS D'UNE MATRICE
C     ------------------------------------------------------------------
C
C IN  NOMMAT  : K19 : NOM DE LA MATRICE
C     ------------------------------------------------------------------
C     CETTE ROUTINE CREE 2 OBJETS DE TRAVAIL SUR LA BASE VOLATILE
C
C     DE NOM  NOMMAT//'.&INT'   VECTEUR D'ENTIER
C             NOMMAT//'.&IN2'   VECTEUR DE K24
C
C     ZI(+0) : NOMBRE DE VALEUR DANS LE DESCRIPTEUR
C     ZK24(ZI(+1) : NOM DEVELOPPEUR DE LA MATRICE + 4 BLANCS
C     ZI(+2) : NOMBRE D'EQUATIONS
C     ZI(+3) : TYPE DE VALEURS
C                1 : REELLE
C                2 : COMPLEXE
C     ZI(+4) : PROPRIETE DE SYMETRIE DE LA MATRICE
C                0 : QUELCONQUE
C                1 : SYMETRIQUE
C                2 : HERMITIENNE
C     ZI(+5) : INUTILISE
C     ZI(+6) : TYPE DE STOCKAGE
C                1 : PROFIL PAR BLOC
C                2 : MORSE
C     ZI(+7) : NOMBRE DE DDLS IMPOSES PAR DES CHARGES CINEMATIQUES DANS
C              LA MATRICE ASSEMBLEE = NIMPO
C
C     ZI(+10) : INUTILISE
C     ZI(+11) : INUTILISE
C     ZI(+12) : INUTILISE
C     ZI(+13) : NOMBRE DE BLOC POUR LA MATRICE ASSEMBLEE
C     ZI(+14) : LONGUEUR D'UN BLOC
C     ZI(+15) : INUTILISE
C     ZI(+16) : INUTILISE
C     ZI(+17) : INUTILISE
C     ZI(+18) : NOMBRE DE BLOC POUR .VALI ,LES BLOCS SONT DE LA MEME
C               LONGUEUR QUE CEUX DE .VALE
C     ------------------------------------------------------------------
C
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
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     ----- PARAMETRES DE DEFINITION DES MATRICES ----------------------
      PARAMETER ( NBVINT = 19 )
      CHARACTER*2   TYMA
      CHARACTER*4   CBID, DOCU
      CHARACTER*8   CH8
      CHARACTER*19  NOMREF, CH19
      CHARACTER*24  NOMINT, NOMIN2, REF2
      CHARACTER*24  REFA,ADIA,ABLO,DESC,VALE,LLIG,ALIG,ABLI
      CHARACTER*32  JEXNUM
C     ------------------------------------------------------------------
C     --- GENERALITES ---
      DATA  REFA  / '                   .REFA'/
C
C     --- STOCKAGE ---
      DATA  REF2  / '                   .REFE'/
      DATA  DESC  / '                   .DESC'/
      DATA  ADIA  / '                   .ADIA'/
      DATA  ABLO  / '                   .ABLO'/
      DATA  LLIG  / '                   .LLIG'/
      DATA  ALIG  / '                   .ALIG'/
      DATA  ABLI  / '                   .ABLI'/
C
C     --- LISTE DES VALEURS ---
      DATA  VALE  / '                   .VALE'/
C
C     --- NOMS INTERNES ---
      DATA  NOMINT/ '                   .&INT'/
      DATA  NOMIN2/ '                   .&IN2'/
C     ------------------------------------------------------------------
C
C
      CALL JEMARQ()
      NOMINT(1:19) = NOMMAT
      NOMIN2(1:19) = NOMMAT
C
C
C        ------ ALLOCATION DES OBJETS SI NECESSAIRE :
         CALL JEEXIN(NOMINT, IER )
         IF ( IER .EQ. 0 ) THEN
            CALL JECREO(NOMINT,' V V I')
            CALL JEECRA(NOMINT,'LONMAX',NBVINT,'  ')
         ENDIF
         CALL JEVEUO(NOMINT,'E',LMAT)
C
         CALL JEEXIN(NOMIN2, IER )
         IF ( IER .EQ. 0 ) THEN
            CALL JECREO(NOMIN2,' V E K24')
         ENDIF
         CALL JEVEUT(NOMIN2,'E',LNOM)
         ZK24(LNOM) = NOMMAT


C
C        +0 : NOMBRE DE VALEURS DANS LE DESCRIPTEUR
         ZI(LMAT      ) = NBVINT
C
C        +1 :ADRESSE DU NOM UTILISATEUR DE LA MATRICE
         ZI(LMAT+1 ) = LNOM
C
         REFA(1:19) = NOMMAT
         CALL JEVEUO(REFA,'L',LREFE)
C
C
         NOMREF = ZK24(LREFE+2)
         REF2(1:19) = NOMREF
         DESC(1:19) = NOMREF
         ADIA(1:19) = NOMREF
         ABLO(1:19) = NOMREF
         CALL JEEXIN(DESC,IER)
         IF ( IER.NE.0 ) CALL JEEXIN(ABLO,IER)
         IF ( IER.NE.0 ) CALL JEEXIN(ADIA,IER)
         IF ( IER.NE.0 ) THEN
            CALL JEVEUO(DESC,'L',LDESC)
C
C            --- +2 : NOMBRE D'EQUATION
             ZI(LMAT+2) = ZI(LDESC)
C
C           ---  +3 : TYPE DE VALEUR
            VALE(1:19) = NOMMAT
            CALL JELIRA(JEXNUM(VALE,1),'TYPE',ZI(LMAT+14),CBID)
            IF ( CBID(1:1) .EQ. 'R' ) ZI(LMAT+3) = 1
            IF ( CBID(1:1) .EQ. 'C' ) ZI(LMAT+3) = 2
C
C         ---  +4 : SYMETRIE
C
            CALL JELIRA(NOMINT(1:19)//'.VALE','DOCU',IBID,TYMA)
            IF ( TYMA.EQ.'MS' ) THEN
               ZI(LMAT+4) = 1
            ELSE IF ( TYMA.EQ.'MR' ) THEN
               ZI(LMAT+4) = 0
            ENDIF
C
C           ---  +5 : PROPRIETES SUR LA COTE D'AZUR
            CALL JELIRA(REF2,'DOCU',IBID,DOCU)
            ZI(LMAT+5) = 0
C
C           ---  +6  : TYPE DE STOCKAGE = 1 <==> LIGN_CIEL
C           ---                         = 2 <==> MORSE
            IF ( DOCU(2:3).EQ.'LC' ) ZI(LMAT+ 6) = 1
            IF ( DOCU(2:3).EQ.'MO' ) ZI(LMAT+ 6) = 2
C
C           ---  +7  : NOMBRE DE DDLS IMPOSES ELIMINES NIMPO
C           ---
            LLIG(1:19) = NOMMAT
            ALIG(1:19) = NOMMAT
            ABLI(1:19) = NOMMAT
            ZI(LMAT+7) = 0
            ZI(LMAT+15) = 0
            ZI(LMAT+16) = 0
            ZI(LMAT+17) = 0
            ZI(LMAT+18) = 0

            ZI(LMAT+10)=ISMAEM()
            ZI(LMAT+11)=ISMAEM()
            ZI(LMAT+12)=ISMAEM()
            ZI(LMAT+15)=ISMAEM()
            ZI(LMAT+16)=ISMAEM()
            ZI(LMAT+17)=ISMAEM()

            CALL JEEXIN(LLIG,IER)
            IF (IER.NE.0) THEN
              CALL JEEXIN(ALIG,IERA)
              IF (IERA.EQ.0) THEN
                CALL UTMESS('F','MTDSCR_1',' .LLIG EXISTE ET LE .ALIG'
     +          //' N"EXISTE PAS INCOHERENCE DANS LA MATR_ASSE')
              ENDIF
              CALL JEEXIN(ABLI,IERA)
              IF (IERA.EQ.0) THEN
                CALL UTMESS('F','MTDSCR_1',' .LLIG EXISTE ET LE .ABLI'
     +          //' N"EXISTE PAS INCOHERENCE DANS LA MATR_ASSE')
              ENDIF
C
              CALL JEVEUO(LLIG,'L',IBID)
              ZI(LMAT+7) = ZI(IBID)
C
C           ---  +18 : NOMBRE DE BLOC DE .VALI
              CALL JELIRA(ABLI,'LONMAX',NB,CBID)
              ZI(LMAT+18) = NB-1
            ENDIF
C
C           ---  +13 : NOMBRE DE BLOC
            ZI(LMAT+13) = ZI(LDESC+2)
C
C           ---  +14 : LONGUEUR D'UN BLOC
            ZI(LMAT+14) = ZI(LDESC+1)
C
         ELSE
           CH19 = NOMMAT
           CALL UTMESS('F','MTDSCR','LE TYPE DE STOCKAGE DE "'//CH19//
     +                                '" EST INCONNU.')
         ENDIF
9999  CONTINUE
      CALL JEDEMA()
      END
