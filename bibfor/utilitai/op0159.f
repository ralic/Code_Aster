      SUBROUTINE OP0159 ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/03/2003   AUTEUR DURAND C.DURAND 
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
C     OPERATEUR   IMPR_MATRICE
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
      CHARACTER*32      JEXNOM, JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      VERSIO, NBELEM
      CHARACTER*8  K8B, FORMAT, MATRIC, MODEL1, NOMMAI, GRAIN
      CHARACTER*9  TYPSD
      CHARACTER*16 OPTION, OPTIO2, OPTIO3, FICHIE
      CHARACTER*19 MATR, LIGRE1, LIGREL
      CHARACTER*24 NOLI, DESC, RESU
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
      CALL INFMAJ()
C
      EPS    = 0.0D0
      NBEL   = 0
      NBNOEU = 0
      NBCMP  = 0
      GRAIN  = 'VALEUR'
      NBCHIF =  4
      OPTIO3 = 'SOUS_MATRICE'
C
      CALL GETFAC('MATR_ELEM',NBMAEL)
C
C     --- TRAITEMENT MAILLE TARDIVE ---
C
      IF ( NBMAEL .NE. 0 ) THEN
        CALL GETVID ('MATR_ELEM','MATRICE',1,1,1,MATRIC,N4)
        CALL GETVTX ('MATR_ELEM','FORMAT' ,1,1,1,FORMAT,N1)
        CALL GETVIS ('MATR_ELEM','VERSION',1,1,1,VERSIO,N3)
        FICHIE = FORMAT
        CALL GETVTX ('MATR_ELEM','FICHIER',1,1,1,FICHIE,N2)
        IFC = IUNIFI(FICHIE)
        IF ( FORMAT .EQ. 'IDEAS'  .AND.  VERSIO .EQ. 5 ) THEN
        CALL DISMOI('F','NOM_MODELE',MATRIC,'MATR_ELEM',IBID,MODEL1,IE)
        CALL DISMOI('F','NOM_LIGREL',MODEL1,'MODELE'   ,IBID,LIGRE1,IE)
        CALL DISMOI('F','NB_MA_SUP'   ,LIGRE1,'LIGREL',NMSUP ,K8B,IE)
        CALL DISMOI('F','NB_MA_MAILLA',LIGRE1,'LIGREL',NBELET,K8B,IE)
        JNSUP1 = 1
        JNSUP2 = 1
        IF ( NMSUP .NE. 0 ) THEN
          CALL WKVECT('&&OP0159.NUME_MAILLE ','V V I',NMSUP,JNSUP1)
          CALL WKVECT('&&OP0159.NUME_ELEMENT','V V I',NMSUP,JNSUP2)
            WRITE (IFC,'(A)') '    -1'
            WRITE (IFC,'(A)') '   780'
          K = 0
          CALL JEVEUO(MATRIC//'.LISTE_RESU','L',JLRESU)
          CALL JELIRA(MATRIC//'.LISTE_RESU','LONMAX',NBRESU,K8B)
          CALL JEVEUO(MATRIC//'.REFE_RESU','L',JRRESU)
          OPTION = ZK24(JRRESU+1)
          DO 12 IMEL = 1 , NBRESU
            MATR = ZK24(JLRESU+IMEL-1)
            DESC = MATR//'.DESC'
            NOLI = MATR//'.NOLI'
            CALL JEEXIN(DESC,IRET)
            IF ( IRET .EQ. 0 ) GOTO 12
            CALL JEVEUO(DESC,'L',JDESC)
            CALL JEVEUO(NOLI,'L',JNOLI)
            LIGREL = ZK24(JNOLI)
            OPTIO2 = ZK24(JNOLI+1)
            IF ( OPTIO2 .NE. OPTION ) GOTO 12
            CALL JELIRA(LIGREL//'.LIEL','NMAXOC',NBGREL,K8B)
            DO 14 IGREL = 1 , NBGREL
              MODE = ZI(JDESC-1+2+IGREL)
              IF (MODE.EQ.0) GOTO 14
              CALL JEVEUO(JEXNUM(LIGREL//'.LIEL',IGREL),'L',JGR)
              NEL = NBELEM(LIGREL,IGREL)
              DO 16 IEL = 1 , NEL
                IMAIL = ZI(JGR+IEL-1)
                IF ( IMAIL .LT. 0 ) THEN
                  DO 20 J = 1 , K
                     IF ( ZI(JNSUP1+J-1) .EQ. IMAIL ) GOTO 16
 20               CONTINUE
                  IMA = -IMAIL
                  CALL JEVEUO(JEXNUM(LIGREL//'.NEMA',IMA),'L',JNEM)
             CALL JELIRA(JEXNUM(LIGREL//'.NEMA',IMA),'LONMAX',NEL,K8B)
             CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(JNEM+NEL-1)),NOMMAI)
                  CALL UTIDEA ( NOMMAI , ITYPM )
                  NBELET = NBELET + 1
                  K = K + 1
                  ZI(JNSUP1+K-1) = IMAIL
                  ZI(JNSUP2+K-1) = NBELET
                  IELM = NEL -1
                  IF ( FORMAT.EQ.'IDEAS' .AND. VERSIO.EQ.5 ) THEN
                    WRITE (IFC,'(8I10)') NBELET,ITYPM,1,2,1,1,7,IELM
                    WRITE (IFC,'(8I10)') ( ZI(JNEM+J-1) , J=1,IELM )
                  ENDIF
                ENDIF
 16           CONTINUE
 14         CONTINUE
 12       CONTINUE
          IF ( FORMAT .EQ. 'IDEAS'  .AND.  VERSIO .EQ. 5 ) THEN
            WRITE (IFC,'(A)') '    -1'
          ENDIF
        ENDIF
       ENDIF
      ENDIF
C
      DO 100 I = 1 , NBMAEL
         CALL GETVTX ('MATR_ELEM','FORMAT' ,I,1,1,FORMAT,N1)
         FICHIE = FORMAT
         CALL GETVTX ('MATR_ELEM','FICHIER',I,1,1,FICHIE,N2)
         CALL GETVIS ('MATR_ELEM','VERSION',I,1,1,VERSIO,N3)
         CALL GETVID ('MATR_ELEM','MATRICE',I,1,1,MATRIC,N4)
C
C ---   VALEURS PAR DEFAUT :
C       ------------------
         EPS    = 0.0D0
         NBEL   = 0
         NBNOEU = 0
         NBCMP  = 0
         GRAIN  = 'VALEUR'
         NBCHIF =  4
         OPTIO3 = 'SOUS_MATRICE'
C
C ---   CREATION DE LA LISTE DES MAILLES POUR-LESQUELLES
C ---   ON VEUT IMPRIMER LE MATR_ELEM :
C       -----------------------------
         CALL CRLIMA ('MATR_ELEM',MATRIC,I,'&OP0159.LISMAI',NBEL)
C
C ---   CREATION DE LA LISTE DES COMPOSANTES POUR-LESQUELLES
C ---   ON VEUT IMPRIMER LE MATR_ELEM :
C       -----------------------------
         CALL GETVTX ('MATR_ELEM','NOM_CMP' ,I,1,0,K8B,NBCM)
         IF (NBCM.NE.0) THEN
           NBCM = -NBCM
           CALL WKVECT ('&OP0159.CMP','V V K8',NBCM,IDNBCM)
           CALL GETVTX ('MATR_ELEM','NOM_CMP' ,I,1,NBCM,ZK8(IDNBCM),
     +                   NBCMP)
         ENDIF
C
C ---   RECUPERATION DU GRAIN DE L'IMPRESSION :
C       -------------------------------------
         CALL GETVTX ('MATR_ELEM','GRAIN' ,I,1,0,K8B,NBGRAI)
         IF (NBGRAI.NE.0) THEN
           CALL GETVTX ('MATR_ELEM','GRAIN' ,I,1,1,GRAIN,NBGRA)
         ENDIF
C
C ---   RECUPERATION DU NOMBRE DE CHIFFRES SIGNIFICATIFS :
C       ------------------------------------------------
         CALL GETVIS ('MATR_ELEM','NB_CHIFFRE' ,I,1,0,IBID,NBCVIG)
         IF (NBCVIG.NE.0) THEN
           CALL GETVIS ('MATR_ELEM','NB_CHIFFRE' ,I,1,1,NBCHIF,NB)
         ENDIF
C
         IF ( FORMAT .EQ. 'IDEAS' ) THEN
           CALL IRMEID(MATRIC,FICHIE,VERSIO,NMSUP,ZI(JNSUP1),ZI(JNSUP2))
         ELSE
C
C ---     RECUPERATION DU TYPE 'MATR_ELEM' OU 'VECT_ELEM' DE LA S.D.
C ---     A IMPRIMER :
C         ----------
           CALL JEEXIN (MATRIC//'.LISTE_RESU',IRET)
           IF (IRET.EQ.0) THEN
             CALL UTMESS('F','OP0159','ERREUR DANS LA DONNEE DE LA '//
     +                   'S.D. '//MATRIC//' A IMPRIMER, IL NE S''AGIT'
     +                 //' NI D''UN MATR_ELEM, NI D''UN VECT_ELEM '//
     +                   'CAR LE .LISTE_RESU N''EXISTE PAS.')
           ELSE
             CALL JEVEUO (MATRIC//'.LISTE_RESU','L',IDLRES)
             RESU = ZK24(IDLRES)
             IF (RESU(10:10).EQ.'M') THEN
                TYPSD = 'MATR_ELEM'
             ELSEIF (RESU(10:10).EQ.'V') THEN
                TYPSD = 'VECT_ELEM'
             ELSE
               CALL UTMESS('F','OP0159','ERREUR DANS LA DONNEE DE LA '
     +                 //'S.D. '//MATRIC//' A IMPRIMER, IL NE S''AGIT'
     +                 //' NI D''UN MATR_ELEM, NI D''UN VECT_ELEM.')
             ENDIF
             CALL IMPMAT(FICHIE,MATRIC,TYPSD,GRAIN,OPTIO3,NBNOEU,
     +                  '&OP0159.LISNOE',NBEL,'&OP0159.LISMAI',
     +                   NBCMP,'&OP0159.CMP',NBCHIF,EPS)
           ENDIF
         ENDIF
C
         CALL JEDETR ( '&OP0159.LISMAI' )
         CALL JEDETR ( '&OP0159.LISNOE' )
         CALL JEDETR ( '&OP0159.CMP' )
 100  CONTINUE
C
      CALL GETFAC('MATR_ASSE',NBASSE)
      DO 200 I = 1 , NBASSE
         CALL GETVTX ('MATR_ASSE','FORMAT' ,I,1,1,FORMAT,N1)
         FICHIE = '        '
         CALL GETVTX ('MATR_ASSE','FICHIER',I,1,1,FICHIE,N2)
         IF ( FICHIE .EQ. '        ' ) FICHIE = FORMAT
         CALL GETVIS ('MATR_ASSE','VERSION',I,1,1,VERSIO,N3)
         CALL GETVID ('MATR_ASSE','MATRICE',I,1,1,MATRIC,N4)
C
C ---   VALEURS PAR DEFAUT :
C       ------------------
         EPS    = 0.0D0
         NBEL   = 0
         NBNOEU = 0
         NBCMP  = 0
         GRAIN  = 'VALEUR'
         NBCHIF =  4
         OPTIO3 = 'SOUS_MATRICE'
C
C ---   CREATION DE LA LISTE DES NOEUDS POUR-LESQUELS
C ---   ON VEUT IMPRIMER LES LIGNES DE LA MATR_ASSE :
C       -------------------------------------------
         CALL CRLINO ('MATR_ASSE',MATRIC,I,'&OP0159.LISNOE',NBNOEU)
C
C ---   RECUPERATION DE L'OPTION DE L'IMPRESSION :
C       ----------------------------------------
        CALL GETVTX ('MATR_ASSE','OPTION' ,I,1,0,K8B,NOPT)
        IF (NOPT.NE.0) THEN
          CALL GETVTX ('MATR_ASSE','OPTION' ,I,1,1,OPTIO3,NPO)
        ENDIF
C
C ---   CREATION DE LA LISTE DES COMPOSANTES POUR-LESQUELLES
C ---   ON VEUT IMPRIMER LA MATR_ASSE :
C       -----------------------------
        CALL GETVTX ('MATR_ASSE','NOM_CMP' ,I,1,0,K8B,NBCM)
        IF (NBCM.NE.0) THEN
          NBCM = -NBCM
          CALL WKVECT ('&OP0159.CMP','V V K8',NBCM,IDNBCM)
          CALL GETVTX ('MATR_ASSE','NOM_CMP' ,I,1,NBCM,ZK8(IDNBCM),
     +                  NBCMP)
        ENDIF
C
C ---   RECUPERATION DU GRAIN DE L'IMPRESSION :
C       -------------------------------------
        CALL GETVTX ('MATR_ASSE','GRAIN' ,I,1,0,K8B,NBGRAI)
        IF (NBGRAI.NE.0) THEN
          CALL GETVTX ('MATR_ASSE','GRAIN' ,I,1,1,GRAIN,NBGRA)
        ENDIF
C
C ---   RECUPERATION DU NOMBRE DE CHIFFRES SIGNIFICATIFS :
C       ------------------------------------------------
        CALL GETVIS ('MATR_ASSE','NB_CHIFFRE' ,I,1,0,IBID,NBCVIG)
        IF (NBCVIG.NE.0) THEN
          CALL GETVIS ('MATR_ASSE','NB_CHIFFRE' ,I,1,1,NBCHIF,NB)
        ENDIF
C
C ---   RECUPERATION DU 'ZERO' DE L'IMPRESSION :
C       --------------------------------------
        CALL GETVR8 ('MATR_ASSE','VALE_ZERO' ,I,1,0,R8B,NBZERO)
        IF (NBZERO.NE.0) THEN
          CALL GETVR8 ('MATR_ASSE','VALE_ZERO' ,I,1,1,EPS,NBZ)
        ENDIF
C
         IF ( FORMAT .EQ. 'IDEAS' ) THEN
            CALL IRMAID ( MATRIC, FICHIE, VERSIO )
         ELSE
           CALL IMPMAT(FICHIE,MATRIC,'MATR_ASSE',GRAIN,OPTIO3,NBNOEU,
     +                  '&OP0159.LISNOE',NBEL,'&OP0159.LISMAI',
     +                   NBCMP,'&OP0159.CMP',NBCHIF,EPS)
         ENDIF
C
         CALL JEDETR ( '&OP0159.LISMAI' )
         CALL JEDETR ( '&OP0159.LISNOE' )
         CALL JEDETR ( '&OP0159.CMP' )
 200  CONTINUE
C
      CALL JEDETR ( '&&OP0159.NUME_MAILLE' )
      CALL JEDETR ( '&&OP0159.NUME_ELEMENT' )
      CALL JEDEMA( )
C
      END
