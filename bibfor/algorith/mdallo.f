      SUBROUTINE MDALLO (NOMRES,BASEMO,MASGEN,RIGGEN,AMOGEN,NBMODE,DT,
     +                   NBSAUV, NBCHOC,NOECHO,INTITU,
     +                   NBREDE,FONRED, NBREVI,
     +                   JDEPL,JVITE,JACCE,JPTEM,JORDR,JINST,
     +                   JFCHO,JDCHO,JVCHO, JADCHO,
     +                   JREDC,JREDD, LPSTO, METHOD)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*) BASEMO
      CHARACTER*8 NOMRES,MASGEN,RIGGEN,AMOGEN,INTITU(*),KBID,KB
      CHARACTER*8 NOECHO(NBCHOC,*),FONRED(NBREDE,*)
      CHARACTER*16 METHOD
      LOGICAL LPSTO
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ALLOCATION DES VECTEURS DE SORTIE
C     ------------------------------------------------------------------
C IN  : NOMRES : NOM DU RESULTAT
C IN  : METHOD : METHODE EMPLOYE
C IN  : BASEMO : NOM DU CONCEPT BASE MODALE
C IN  : MASGEN : NOM DU CONCEPT MASSE GENERALISEE
C IN  : RIGGEN : NOM DU CONCEPT RAIDEUR GENERALISEE
C IN  : AMOGEN : NOM DU CONCEPT AMORTISSEMENT GENERALISE
C IN  : NBMODE : NOMBRE DE MODES
C IN  : DT     : PAS DE TEMPS
C IN  : NBPAS  : NOMBRE DE PAS CALCULE (INITIAL COMPRIS)
C IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
C IN  : NOECHO : TABLEAU DES NOMS DES NOEUDS DE CHOC
C IN  : INTITU : TABLEAU DES NOMS DES LIAISONS
C IN  : NBREDE : NOMBRE DE RELATION EFFORT DEPLACEMENT (RED)
C IN  : FONRED : TABLEAU DES FONCTIONS DE RED
C IN  : NBREVI : NOMBRE DE RELATION EFFORT VITESSE (REV)
C IN  : LPSTO  : LOGIQUE =VRAI SI STOCKAGE DU PAS DE TEMPS VARIABLE
C IN  : METHOD : ALGORITHME UTILISE (DEVOGE, EULER, ...)
C                DANS LE CAS ITMI, UN OBJET EST DIFFERENT
C ----------------------------------------------------------------------
      INTEGER      NBSAUV, NBSTOC,J1REFE
      CHARACTER*8  NUMGEN,BLANC
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,IC ,IRET ,JACCE ,JADCHO ,JDCHO ,JDEPL 
      INTEGER JDESC ,JFCHO ,JINST ,JINTI ,JNCHO ,JORDR ,JPTEM 
      INTEGER JREDC ,JREDD ,JREDN ,JREFE ,JSST ,JVCHO ,JVINT 
      INTEGER JVITE ,NBCHOC ,NBMODE ,NBREDE ,NBREVI ,NBSTO1 
      REAL*8 DT 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      NBSTOC = NBMODE * NBSAUV
      JFCHO = 1
      JDCHO = 1
      JVCHO = 1
      JADCHO= 1
      JREDC = 1
      JREDD = 1
      BLANC =  '        '
C
      CALL JEEXIN(NOMRES//'           .REFD',IRET)
      IF (IRET.EQ.0) THEN
C On recupere la numerotation generalisee
         CALL JEEXIN(RIGGEN//'           .REFA',IRET)
         IF (IRET.NE.0) THEN
           CALL JEVEUO(RIGGEN//'           .REFA','L',J1REFE)
           NUMGEN = ZK24(J1REFE+1)(1:8)
         ELSE
           NUMGEN = BLANC
         ENDIF
         CALL WKVECT(NOMRES//'           .REFD','G V K24',7,JREFE)
         ZK24(JREFE) = RIGGEN
         ZK24(JREFE+1) = MASGEN
         ZK24(JREFE+2) = AMOGEN
         ZK24(JREFE+3) = NUMGEN
         ZK24(JREFE+4) = ' '
         ZK24(JREFE+5) = BASEMO(1:8)
         ZK24(JREFE+6) = ' '
      ENDIF
C
      CALL JEEXIN(NOMRES//'           .DESC',IRET)
      IF (IRET.EQ.0) THEN
         CALL WKVECT(NOMRES//'           .DESC','G V I',5,JDESC)
         ZI(JDESC) = 1
         IF ( NBCHOC.NE.0 ) ZI(JDESC) = 2
C        --- DANS LE CAS ITMI ET ADAPT (METHODES A PAS VARIABLE),
C            ON MET LA VALEUR 3 QUI SERVIRA DE TEST
C             A LA COMMANDE POST_DYNA_MODA_T
         IF (METHOD.EQ.'ITMI' .OR. METHOD(1:5).EQ.'ADAPT') ZI(JDESC) = 3
C        ---
         ZI(JDESC+1) = NBMODE
         ZI(JDESC+2) = NBCHOC
         ZI(JDESC+3) = NBREDE
         ZI(JDESC+4) = NBREVI
      ENDIF
C
      IF (NBSAUV.NE.0) THEN
        CALL JECREO(NOMRES//'           .DEPL' ,'G V R')
        CALL JEECRA(NOMRES//'           .DEPL' ,'LONMAX',NBSTOC,KBID)
        CALL JEECRA(NOMRES//'           .DEPL' ,'LONUTI',NBSTOC,KBID)
        CALL JEVEUT(NOMRES//'           .DEPL' ,'E',JDEPL)
        CALL JECREO(NOMRES//'           .VITE' ,'G V R')
        CALL JEECRA(NOMRES//'           .VITE' ,'LONMAX',NBSTOC,KBID)
        CALL JEECRA(NOMRES//'           .VITE' ,'LONUTI',NBSTOC,KBID)
        CALL JEVEUT(NOMRES//'           .VITE' ,'E',JVITE)
        CALL JECREO(NOMRES//'           .ACCE' ,'G V R')
        CALL JEECRA(NOMRES//'           .ACCE' ,'LONMAX',NBSTOC,KBID)
        CALL JEECRA(NOMRES//'           .ACCE' ,'LONUTI',NBSTOC,KBID)
        CALL JEVEUT(NOMRES//'           .ACCE' ,'E',JACCE)
        CALL JECREO(NOMRES//'           .ORDR' ,'G V I')
        CALL JEECRA(NOMRES//'           .ORDR' ,'LONMAX',NBSAUV,KBID)
        CALL JEECRA(NOMRES//'           .ORDR' ,'LONUTI',NBSAUV,KBID)
        CALL JEVEUT(NOMRES//'           .ORDR' ,'E',JORDR)
        CALL JECREO(NOMRES//'           .INST' ,'G V R')
        CALL JEECRA(NOMRES//'           .INST' ,'LONMAX',NBSAUV,KBID)
        CALL JEECRA(NOMRES//'           .INST' ,'LONUTI',NBSAUV,KBID)
        CALL JEVEUT(NOMRES//'           .INST' ,'E',JINST)
        IF (LPSTO) THEN
          CALL JECREO(NOMRES//'           .PTEM' ,'G V R')
          CALL JEECRA(NOMRES//'           .PTEM' ,'LONMAX',NBSAUV,KBID)
          CALL JEECRA(NOMRES//'           .PTEM' ,'LONUTI',NBSAUV,KBID)
          CALL JEVEUT(NOMRES//'           .PTEM' ,'E',JPTEM)
        ELSE
          CALL JECREO(NOMRES//'           .PTEM' ,'G V R')
          CALL JEECRA(NOMRES//'           .PTEM' ,'LONMAX',1,KBID)
          CALL JEECRA(NOMRES//'           .PTEM' ,'LONUTI',1,KBID)
          CALL JEVEUT(NOMRES//'           .PTEM' ,'E',JPTEM)
        ENDIF
        ZR(JPTEM) = DT
      ENDIF
C
C     --- CREATION DES VECTEURS DE STOCKAGE DES FORCES DE CHOC ---
      IF ( NBCHOC.NE.0 ) THEN
        NBSTOC = 3 * NBCHOC * NBSAUV
        NBSTO1 = NBCHOC * NBSAUV
        CALL JEEXIN(NOMRES//'           .NCHO',IRET)
        IF (IRET.EQ.0)
     &   CALL WKVECT(NOMRES//'           .NCHO','G V K8',2*NBCHOC,JNCHO)
        CALL JEEXIN(NOMRES//'           .SST',IRET)
        IF (IRET.EQ.0)
     &   CALL WKVECT(NOMRES//'           .SST' ,'G V K8',2*NBCHOC,JSST)
        IF (NBSAUV.NE.0) THEN
          CALL JECREO(NOMRES//'           .FCHO','G V R')
          CALL JEECRA(NOMRES//'           .FCHO','LONMAX',NBSTOC,KBID)
          CALL JEECRA(NOMRES//'           .FCHO','LONUTI',NBSTOC,KBID)
          CALL JEVEUT(NOMRES//'           .FCHO','E',JFCHO)
          CALL JECREO(NOMRES//'           .DLOC','G V R')
          CALL JEECRA(NOMRES//'           .DLOC','LONMAX',2*NBSTOC,KBID)
          CALL JEECRA(NOMRES//'           .DLOC','LONUTI',2*NBSTOC,KBID)
          CALL JEVEUT(NOMRES//'           .DLOC','E',JDCHO)
          CALL JECREO(NOMRES//'           .VCHO','G V R')
          CALL JEECRA(NOMRES//'           .VCHO','LONMAX',NBSTOC,KBID)
          CALL JEECRA(NOMRES//'           .VCHO','LONUTI',NBSTOC,KBID)
          CALL JEVEUT(NOMRES//'           .VCHO','E',JVCHO)
          CALL JECREO(NOMRES//'           .ICHO','G V I')
          CALL JEECRA(NOMRES//'           .ICHO','LONMAX',NBSTO1,KBID)
          CALL JEECRA(NOMRES//'           .ICHO','LONUTI',NBSTO1,KBID)
          CALL JEVEUT(NOMRES//'           .ICHO','E',JADCHO)
C          --- OBJET POUR LE FLAMBEMENT : VARIABLE INTERNE ---
          CALL JECREO(NOMRES//'           .VINT','G V R')
          CALL JEECRA(NOMRES//'           .VINT','LONMAX',NBSTO1,KB)
          CALL JEECRA(NOMRES//'           .VINT','LONUTI',NBSTO1,KB)
C              INITIALISATION
         CALL JEVEUO(NOMRES//'           .VINT','E',JVINT)
         CALL R8INIR(NBSTO1,0.D0,ZR(JVINT),1)
        ENDIF
        CALL JEEXIN(NOMRES//'           .INTI',IRET)
        IF (IRET.EQ.0) THEN
          CALL WKVECT(NOMRES//'           .INTI','G V K8',NBCHOC,JINTI)
          DO 10 IC = 1,NBCHOC
             ZK8(JINTI+IC-1) = INTITU(IC)
             ZK8(JNCHO+IC-1) = NOECHO(IC,1)
             ZK8(JNCHO+NBCHOC+IC-1) = NOECHO(IC,5)
             ZK8(JSST+IC-1) = NOECHO(IC,2)
             ZK8(JSST+NBCHOC+IC-1) = NOECHO(IC,6)
 10       CONTINUE
        ENDIF
      ENDIF
C
C     --- CREATION DES VECTEURS DE STOCKAGE DES RELA_EFFO_DEPL ---
      IF ( NBREDE.NE.0 ) THEN
        NBSTOC = NBREDE * NBSAUV
        IF (NBSAUV.NE.0) THEN
          CALL JECREO(NOMRES//'           .REDC','G V I')
          CALL JEECRA(NOMRES//'           .REDC','LONMAX',NBSTOC,KBID)
          CALL JEECRA(NOMRES//'           .REDC','LONUTI',NBSTOC,KBID)
          CALL JEVEUT(NOMRES//'           .REDC','E',JREDC)
          CALL JECREO(NOMRES//'           .REDD','G V R')
          CALL JEECRA(NOMRES//'           .REDD','LONMAX',NBSTOC,KBID)
          CALL JEECRA(NOMRES//'           .REDD','LONUTI',NBSTOC,KBID)
          CALL JEVEUT(NOMRES//'           .REDD','E',JREDD)
        ENDIF
        CALL JEEXIN(NOMRES//'           .REDN',IRET)
        IF (IRET.EQ.0) THEN
          CALL WKVECT(NOMRES//'           .REDN','G V K24',NBREDE,JREDN)
          DO 20 I = 1,NBREDE
             ZK24(JREDN+I-1) = FONRED(I,1)//FONRED(I,2)//FONRED(I,3)
 20       CONTINUE
        ENDIF
      ENDIF
C
      CALL JEDEMA()
      END
