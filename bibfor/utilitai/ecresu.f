      SUBROUTINE ECRESU(RESIN,VECTOT,NBVA,GRAND,RESOU,IER)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER NPARA,NBVA
      CHARACTER*(*) RESIN,RESOU,GRAND
      CHARACTER*19 VECTOT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 05/02/2013   AUTEUR ALARCON A.ALARCON 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     REALISATION N.GREFFET
C     OPERATEUR "ECRIRE RESULTAT"
C     IN:
C       RESIN : SD_RESULTAT INITIALE HARMONIQUE
C               (VENANT DE DYNA_LINE_HARM)
C       NPARA : POINTEUR DU TABLEAU DE DONNEE VENANT DE LA FFT
C       NBVA  : NOMBRE D'INSTANTS
C       GRAND : GRANDEUR PHYSIQUE (DEPL, VITE, ACCE)
C
C     OUT:
C       RESOU   : SD_RESULTAT FINALE TRANSITOIRE
C
C
C
C
C
C     ------------------------------------------------------------------
      INTEGER      NBORDR,LTPS,JORDR,IBID,I, NBSYM, IARG
      INTEGER      LTPS2,IEQ,IER,NEQ,LVAL,LVALS,JREFE,IRET,NBVA2
      INTEGER      NBSAUV,IARCHI,ISTO1,ISTO2,ISTO3,ISTO4
      INTEGER      JDEPS,JVITS,JACCS,JPASS,JINST
      INTEGER      JFCHO,JDCHO,JVCHO,JICHO,JREDC,JREDD,JREVC,JREVD
      INTEGER      IRES,N1,JDESC,NBMODE,LVALV,LVALA,J,LV1,LV2,LV3
      INTEGER      JREFAM,JVINT, JFREQ
      REAL*8       R1,RBID
      REAL*8       DT
      CHARACTER*1  K1B,KTYP
      CHARACTER*4  GRANDE, K4BID, NOMSYM(3)
      CHARACTER*8  K8B
      CHARACTER*8  MASGEN,RIGGEN,AMOGEN,BASEMO
      CHARACTER*16 TYPOUT
      CHARACTER*19 CHDEP,CHDEPS,KREFE
      CHARACTER*24 TYPRES,CHDEP2,REFE
      CHARACTER*24 RAIDE
      COMPLEX*16   R1C
C
      DATA  REFE  /'                   .REFD'/
C     ------------------------------------------------------------------
      CALL JEMARQ()
      GRANDE = GRAND
      CALL JEVEUO(VECTOT,'L',NPARA)
      IER = 0
C   Recuperation type RESU
      CALL GETTCO(RESIN,TYPRES)
      IF (TYPRES(1:10).EQ.'DYNA_HARMO') THEN
         TYPOUT='DYNA_TRANS'
         NBVA2=NBVA
      ELSEIF (TYPRES(1:9).EQ.'HARM_GENE') THEN
         TYPOUT='TRAN_GENE'
         NBVA2=NBVA
      ELSEIF (TYPRES(1:10).EQ.'DYNA_TRANS') THEN
         TYPOUT='DYNA_HARMO'
         NBVA2=2*NBVA
      ELSEIF (TYPRES(1:9).EQ.'TRAN_GENE') THEN
         TYPOUT='HARM_GENE'
         NBVA2=2*NBVA
         CALL JEVEUO(RESIN(1:8)//'           .REFD','L',JREFE)
         RIGGEN = ZK24(JREFE)(1:8)
         MASGEN = ZK24(JREFE+1)(1:8)
         AMOGEN = ZK24(JREFE+2)(1:8)
      ENDIF
C
C  Creation objet de stockage en LTPS pour les valeurs d'instants
C
C  Champs
      IF ( TYPRES(6:9).NE.'GENE') THEN
         CALL RSEXCH('F',RESIN,GRANDE,1,CHDEP,IRET)
         CALL JEVEUO(CHDEP//'.VALE','L',LVAL)
C  Nombre d'equations : NEQ
         CHDEP2 = CHDEP(1:19)//'.VALE'
         CALL JELIRA(CHDEP2,'LONMAX',NEQ,K1B)
      ELSE
         CALL JELIRA(RESIN(1:19)//'.ORDR','LONUTI',NBORDR,K1B)
         CALL JEVEUO(RESIN(1:19)//'.'//GRANDE ,'L',LVAL)
         CHDEP2 = RESIN(1:19)//'.'//GRANDE
         CALL JELIRA(CHDEP2,'LONMAX',NEQ,K1B)
         NEQ = NEQ / NBORDR
      ENDIF
      NBORDR = NBVA
      CALL WKVECT('&&ECRESU.PARAMACC','V V R',NBVA,LTPS)
C
C  Creation objet resultat en sortie si non existence
C
C      NBORDR = NBVA
      CALL JEEXIN ( RESOU(1:8)//'           .DESC', IRES )
      IF ( (IRES.EQ.0).AND.(TYPOUT(6:9).NE.'GENE'))
     &     CALL RSCRSD('G',RESOU,TYPOUT,NBORDR)
C
      REFE(1:8) = RESIN
      CALL JEVEUO(REFE, 'L', JREFE )
      RAIDE = ZK24(JREFE)
C
      IF (TYPOUT(1:10).EQ.'DYNA_HARMO') THEN
C        --- CAS OU RESULTAT EST HARMO SUR BASE PHYSIQUE
C        --- RECUPERER LA LISTE DES FREQUENCES DE LA TABLE FFT
         DO  10 I = 0,NBVA-1
C           --- LES FREQUENCES SONT DECALEES PAR (NEQ*NBVA) DS LA TBLE
            ZR(LTPS+I) =  DBLE(ZC(NPARA+(NEQ*NBVA)+I))
 10      CONTINUE
C
C        --- BOUCLE SUR LES FREQUENCES A SAUVEGARDER (NUM ORDRE RESU)
         DO 20 I = 0,NBORDR-1
            CALL RSADPA(RESOU,'E',1,'FREQ',I+1,0,LTPS2,K8B)
            ZR(LTPS2) = ZR(LTPS+I)
            CALL RSEXCH (' ',RESOU,GRANDE,I+1,CHDEPS,IRET)

            IF (RAIDE(1:1).NE.' ') THEN
C              --- CREATION D'UNE STRUCTURE CHAM_NO BASEE SUR MATRICE
C                  DE RAIDEUR
              CALL VTCREM (CHDEPS, RAIDE, 'G', 'C' )
            ELSE
C              --- CREATION D'UNE STRUCTURE CHAM_NO "CHAMP" BASEE
C                  SUR BASE MODALE (.REFD[3])
              CALL VTCREB(CHDEPS,ZK24(JREFE+3),'G','C',N1)
              CALL ASSERT(N1.EQ.NEQ)
            ENDIF
C           -------------------------------------------------------
C
C           --- REMPLIR LE .VALE PAR LES RESULTATS DANS LA TABLE FFT
            CALL JEVEUO(CHDEPS//'.VALE', 'E', LVALS )
            DO 30 IEQ = 0,NEQ-1
               ZC(LVALS+IEQ) = ZC(NPARA+NBVA*IEQ+I)
 30         CONTINUE
            CALL RSNOCH(RESOU,GRANDE,I+1)
 20      CONTINUE
      ELSEIF ( TYPOUT(1:10).EQ.'DYNA_TRANS' ) THEN
         DO  100 I = 1,NBVA
            ZR(LTPS+I-1) =  ZR(NPARA+(NEQ*NBVA2)+I-1)
 100     CONTINUE
         DO 200 I = 1,NBORDR
C  Temps
            CALL RSADPA(RESOU,'E',1,'INST',(I-1),0,LTPS2,K8B)
            ZR(LTPS2) = ZR(LTPS+I-1)
            CALL RSEXCH (' ',RESOU,GRANDE,(I-1),CHDEPS,IRET)
            IF (RAIDE(1:1).NE.' ') THEN
              CALL VTCREM (CHDEPS, RAIDE, 'G', 'R' )
            ELSE
              CALL VTCREB(CHDEPS,ZK24(JREFE+3),'G','R',N1)
              CALL ASSERT(N1.EQ.NEQ)
            ENDIF

            CALL JEVEUO(CHDEPS//'.VALE', 'E', LVALS )
            CALL JELIRA(CHDEPS//'.VALE', 'LONMAX', N1,K1B )
            CALL ASSERT(N1.EQ.NEQ)
            CALL JELIRA(CHDEPS//'.VALE', 'TYPE', IBID,KTYP)
            CALL ASSERT(KTYP.EQ.'R')
            DO 300 IEQ = 1,NEQ
               R1 = ZR(NPARA+NBVA*(IEQ-1)+I-1)
               ZR(LVALS+IEQ-1) = R1
 300        CONTINUE
            CALL RSNOCH(RESOU,GRANDE,(I-1))
 200     CONTINUE
      ELSEIF ( TYPOUT(1:9).EQ.'TRAN_GENE' ) THEN
C        --- SI NOUVEAU RESULTAT TRAN_GENE 
         IF ( IRES .EQ. 0 ) THEN
C           --- BOUCLE SUR LES INSTANTS A ARCHIVER
            DO  400 I = 0,NBVA-1
C              --- INSTANTS SAUVEGARDEES DANS LA TABLE FFT MAIS
C                  DECALEES PAR (NEQ*NBVA2)
               ZR(LTPS+I) =  ZR(NPARA+(NEQ*NBVA2)+I)
 400        CONTINUE
C           --- INITIALISATION DES INDICES D'ARCHIVAGE POUR MDARCH
            ISTO1 = 0
            ISTO2 = 0
            ISTO3 = 0
            ISTO4 = 0
C
            JVINT = 1
C           --- INFORMATIONS POUR LE .REFD TRANSMIS DIRECTEMENT DE
C               HARM_GENE A TRAN_GENE
            RIGGEN = ZK24(JREFE)(1:8)
            MASGEN = ZK24(JREFE+1)(1:8)
            AMOGEN = ZK24(JREFE+2)(1:8)
C
            NBSAUV = NBORDR
C           --- RECUPERATION DU PAS DE TEMPS, NOMBRE DE MODES ET 
C               ENFIN LA BASE MODALE
            DT = ZR(LTPS+1) - ZR(LTPS)
            CALL JEVEUO(MASGEN(1:8)//'           .DESC','L',JDESC)
            NBMODE = ZI(JDESC+1)
            CALL JEVEUO(MASGEN(1:8)//'           .REFA','L',JREFAM)
            BASEMO = ZK24(JREFAM)(1:8)
C
            K8B = '        '
C
C           --- ALLOCATION DE LA SD DYNA_GENE RESULTAT
            CALL MDALLO (RESOU(1:8),BASEMO,MASGEN,RIGGEN,AMOGEN,NBMODE,
     &                   DT,NBSAUV, 0,K8B,K8B,
     &                   0,K8B,0,K8B,
     &                   JDEPS,JVITS,JACCS,JPASS,JORDR,JINST,
     &                   JFCHO,JDCHO,JVCHO, JICHO,
     &                   JREDC,JREDD, JREVC, JREVD, 'EULER           ',
     &                   IBID,K4BID,'TRAN','GLOB')
C
C           --- CREATION DES VECTEURS DE TRAVAIL TEMPORAIRES
            CALL WKVECT('&&ECRESU.DEPL','V V R',NEQ,LVALS)
            CALL WKVECT('&&ECRESU.VITE','V V R',NEQ,LVALV)
            CALL WKVECT('&&ECRESU.ACCE','V V R',NEQ,LVALA)
            LV1 = LVALS
            LV2 = LVALV
            LV3 = LVALA
C           --- SWITCHER L'ORDRE DE LV(X) SELON LE TYPE DE GRANDEUR
            IF (GRANDE.EQ.'VITE') THEN
               LV1 = LVALV
               LV2 = LVALS
            ELSEIF (GRANDE.EQ.'ACCE') THEN
               LV1 = LVALA
               LV3 = LVALS
            ENDIF
C           --- BOUCLE SUR LES INSTANTS D'ARCHIVAGE (NUM ORDRE)
            DO 500 J = 0,NBORDR-1
               IARCHI = J
               ISTO1 = J
C              --- BOUCLE SUR LES MODES
               DO 600 IEQ = 0,NEQ-1
C                 --- RECUPERER LA VALEUR DANS LA TABLE FFT
                  R1 = ZR(NPARA+NBVA*IEQ+J)
C                 --- SAUVER LA VALEUR DANS LE VECT DE TRAVAIL ASSOCIE
                  ZR(LV1+IEQ) = R1
                  ZR(LV2+IEQ) = 0.D0
                  ZR(LV3+IEQ) = 0.D0
 600           CONTINUE
C
C              --- ARCHIVER LES RESULTATS POUR L'INSTANT EN COURS
               CALL MDARNL (ISTO1,IARCHI,ZR(LTPS+J),DT,NEQ,
     &             ZR(LVALS),ZR(LVALV),ZR(LVALA),
     &             ISTO2,0,0.D0,0,
     &             ISTO3,0,0.D0,0,
     &             ISTO4,0,0.D0,0,
     &             ZR(JDEPS),ZR(JVITS),ZR(JACCS),ZR(JPASS),ZI(JORDR),
     &             ZR(JINST),ZR(JFCHO),ZR(JDCHO),ZR(JVCHO),ZI(JICHO),
     &             ZR(JVINT),ZI(JREDC),ZR(JREDD),ZI(JREVC),ZR(JREVD))
 500        CONTINUE
         ELSE
C           --- SI LE RESULTAT TRAN_GENE N'EST PAS UN NOUVEAU CONCEPT,
C               ALORS MODIFIER DIRECTEMENT LES VALEURS DANS LA SD
C               (FONCTION AVEC APPELS RECURSIFS)
            CALL JEVEUO(RESOU(1:8)//'           .'//GRANDE ,'E',LVALS)
            CALL JELIRA(RESOU(1:8)//'           .'//GRANDE,'LONMAX',
     &              IBID,K1B)
            DO 700 J = 0,NBORDR-1
               IARCHI = J
               ISTO1 = J
               DO 800 IEQ = 0,NEQ-1
                  R1 = ZR(NPARA+NBVA*IEQ+J)
                  ZR(LVALS+(NEQ*ISTO1)+IEQ) = R1
 800           CONTINUE
 700        CONTINUE
         ENDIF
      ELSEIF (TYPOUT(1:9).EQ.'HARM_GENE' ) THEN

C        --- CAS OU LE RESULTAT EST HARMO SUR BASE GENERALISEE
C        --- RECUPERER LA LISTE DES FREQUENCES DE LA TABLE FFT
         IF ( IRES .EQ. 0 ) THEN

            DO  11 I = 0,NBVA-1
C             --- LES FREQUENCES SONT DECALEES PAR (NEQ*NBVA) DS LA TBLE
              ZR(LTPS+I) =  DBLE(ZC(NPARA+(NEQ*NBVA)+I))
 11         CONTINUE
C
C           --- INITIALISATION DES INDICES D'ARCHIVAGE POUR MDARCH
            ISTO1 = 0
C
C           --- INFORMATIONS POUR LE .REFD TRANSMIS DIRECTEMENT DE
C               TRAN_GENE A HARM_GENE
            RIGGEN = ZK24(JREFE)(1:8)
            MASGEN = ZK24(JREFE+1)(1:8)
            AMOGEN = ZK24(JREFE+2)(1:8)
C
            NBSAUV = NBORDR
C           --- RECUPERATION DU NOMBRE DE MODES ET LA BASE MODALE
            CALL JEVEUO(MASGEN(1:8)//'           .DESC','L',JDESC)
            NBMODE = ZI(JDESC+1)
            CALL JEVEUO(MASGEN(1:8)//'           .REFA','L',JREFAM)
            BASEMO = ZK24(JREFAM)(1:8)
C
            K8B = '        '
C           --- ALLOCATION DE LA SD DYNA_GENE RESULTAT
C           ON RECHERCHE LES CHAMPS A REMPLIR POUR LE CAS HARMONIQUE
            CALL GETVTX(' ','NOM_CHAM',1,IARG,3,NOMSYM,NBSYM)
            IF (NBSYM .EQ. 0) THEN
              NBSYM     = 3
              NOMSYM(1) = 'DEPL'
              NOMSYM(2) = 'VITE'
              NOMSYM(3) = 'ACCE'
            ENDIF
C
            CALL MDALLO (RESOU(1:8),BASEMO,MASGEN,RIGGEN,AMOGEN,NBMODE,
     &                   RBID,NBSAUV, 0,K8B,K8B,
     &                   0,K8B,0,K8B,
     &                   JDEPS,JVITS,JACCS,JPASS,JORDR,JFREQ,
     &                   IBID,IBID,IBID, IBID,
     &                   IBID,IBID, IBID, IBID, 'EULER           ',
     &                   NBSYM,NOMSYM,'HARM','GLOB')
C
C           --- CREATION DES VECTEURS DE TRAVAIL TEMPORAIRES
            CALL WKVECT('&&ECRESU.DEPLC','V V C',NEQ,LVALS)
            CALL WKVECT('&&ECRESU.VITEC','V V C',NEQ,LVALV)
            CALL WKVECT('&&ECRESU.ACCEC','V V C',NEQ,LVALA)
            LV1 = LVALS
            LV2 = LVALV
            LV3 = LVALA
C           --- SWITCHER L'ORDRE DE LV(X) SELON LE TYPE DE GRANDEUR
            IF (GRANDE.EQ.'VITE') THEN
               LV1 = LVALV
               LV2 = LVALS
            ELSEIF (GRANDE.EQ.'ACCE') THEN
               LV1 = LVALA
               LV3 = LVALS
            ENDIF
C
C           --- BOUCLE SUR LES FREQUENCES A SAUVEGARDER (NUM ORDRE RESU)
            DO 21 J = 0,NBORDR-1
               IARCHI = J
               ISTO1 = J
C              --- BOUCLE SUR LES MODES
               DO 22 IEQ = 0,NEQ-1
C                 --- RECUPERER LA VALEUR DANS LA TABLE FFT
                  R1C = ZC(NPARA+NBVA*IEQ+J)
C                 --- SAUVER LA VALEUR DANS LE VECT DE TRAVAIL ASSOCIE
                  ZC(LV1+IEQ) = R1C
                  ZC(LV2+IEQ) = 0.D0
                  ZC(LV3+IEQ) = 0.D0
 22            CONTINUE
               NBSYM     = 3
               NOMSYM(1) = 'DEPL'
               NOMSYM(2) = 'VITE'
               NOMSYM(3) = 'ACCE'
C
C              --- ARCHIVER LES RESULTATS POUR LA FREQUENCE EN COURS
               CALL MDARCH (ISTO1,IARCHI,ZR(LTPS+J),RBID,NEQ,'HARM',
     &                      NBSYM,NOMSYM,
     &                      RBID,RBID,RBID,RBID,RBID,RBID, 
     &                      ZC(LVALS),ZC(LVALV),ZC(LVALA),
     &                      ZC(JDEPS),ZC(JVITS),ZC(JACCS),
     &                      RBID,ZI(JORDR),ZR(JFREQ))
 21        CONTINUE
         ELSE
C           --- SI LE RESULTAT HARM_GENE N'EST PAS UN NOUVEAU CONCEPT,
C               ALORS MODIFIER DIRECTEMENT LES VALEURS DANS LA SD
C               (FONCTION AVEC APPELS RECURSIFS)
            CALL JEVEUO(RESOU(1:8)//'           .'//GRANDE ,'E',LVALS)
            CALL JELIRA(RESOU(1:8)//'           .'//GRANDE,'LONMAX',
     &              IBID,K1B)
            DO 23 J = 0,NBORDR-1
               IARCHI = J
               ISTO1 = J
               DO 24 IEQ = 0,NEQ-1
                  R1C = ZC(NPARA+NBVA*IEQ+J)
                  ZC(LVALS+(NEQ*ISTO1)+IEQ) = R1C
 24            CONTINUE
 23        CONTINUE
         ENDIF
      ENDIF
C
C     --- FINALISER LE .REFD POUR LES CAS AVEC RESU SUR BASE PHYSIQUE
      IF ( (IRES.EQ.0) .AND. (TYPOUT(6:9).NE.'GENE') ) THEN
         KREFE = RESOU(1:8)
         CALL WKVECT(KREFE//'.REFD','G V K24',7,JORDR)
         ZK24(JORDR) = ZK24(JREFE)
         ZK24(JORDR+1) = ZK24(JREFE+1)
         ZK24(JORDR+2) = ZK24(JREFE+2)
         ZK24(JORDR+3) = ZK24(JREFE+3)
         ZK24(JORDR+4) = ZK24(JREFE+4)
         ZK24(JORDR+5) = ZK24(JREFE+5)
         ZK24(JORDR+6) = ZK24(JREFE+6)
         CALL JELIBE(KREFE//'.REFD')
      ENDIF

      CALL JEDETR('&&ECRESU.PARAMACC')
      CALL JEDEMA()
      END
