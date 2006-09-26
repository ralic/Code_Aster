      SUBROUTINE MDALLC (NOMRE0,BASEMO,MASGEN,RIGGEN,AMOGEN,
     &                   NBMODE,NBSAUV,NBCHOC,
     &                   NBREDE,FONRED,NBREVI,
     &                   JDEPL,JVITE,JACCE,JORDR,JABS,
     &                   JREDC,JREDD,METHOD)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/09/2006   AUTEUR A3BHHAE H.ANDRIAMBOLOLONA 
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
C     ALLOCATION DES VECTEURS DE SORTIE (DONNEEES COMPLEXES)
C     ------------------------------------------------------------------
C IN  : NOMRE0 : NOM DU CONCEPT RESULTAT
C IN  : BASEMO : NOM DU CONCEPT BASE MODALE
C IN  : MASGEN : NOM DU CONCEPT MASSE GENERALISEE
C IN  : RIGGEN : NOM DU CONCEPT RAIDEUR GENERALISEE
C IN  : AMOGEN : NOM DU CONCEPT AMORTISSEMENT GENERALISE
C IN  : NBMODE : NOMBRE DE MODES
C IN  : NBSAUV : NOMBRE DE PAS CALCULE (INITIAL COMPRIS)
C IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
C IN  : NBREDE : NOMBRE DE RELATION EFFORT DEPLACEMENT (RED)
C IN  : FONRED : TABLEAU DES FONCTIONS DE RED
C IN  : NBREVI : NOMBRE DE RELATION EFFORT VITESSE (REV)
C OUT : JDEPL  : ADRESSE DE NOMRES.DEPL
C OUT : JVITE  : ADRESSE DE NOMRES.VITE
C OUT : JACCE  : ADRESSE DE NOMRES.ACCE
C OUT : JORDR  : ADRESSE DE NOMRES.ORDR
C OUT : JABS   : ADRESSE DE NOMRES.FREQ
C OUT : JREDC  : ADRESSE DE NOMRES.REDC
C OUT : JREDD  : ADRESSE DE NOMRES.REDD
C IN  : METHOD : ALGORITHME UTILISE (DEVOGE, EULER, ...)
C                DANS LE CAS ITMI, UN OBJET EST DIFFERENT
C ----------------------------------------------------------------------
      IMPLICIT    NONE
      INTEGER NBMODE,JREDC,JREDD,JABS
      INTEGER NBCHOC,NBSAUV,NBREDE,NBREVI,JDEPL,JVITE,JACCE,JORDR
      CHARACTER*8 BASEMO,NOMRE0,MASGEN,RIGGEN,AMOGEN
      CHARACTER*8 NOMRES,KBID
      CHARACTER*8 FONRED(NBREDE,*),METHOD
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER NBSTOC,IRET,JREFE,JDESC
      INTEGER I,JREDN,J1REFE

      CHARACTER*8 NUMGEN,BLANC
C
C     ------------------------------------------------------------------
      CALL JEMARQ()

      NBSTOC = NBMODE * NBSAUV
      NOMRES = NOMRE0
C
      JREDC = 1
      JREDD = 1
      BLANC =  '        '

      CALL JEEXIN(NOMRES//'           .REFE',IRET)
      IF (IRET.EQ.0) THEN
         CALL WKVECT(NOMRES//'           .REFE','G V K24',4,JREFE)
         ZK24(JREFE  ) = BASEMO(1:8)
         ZK24(JREFE+1) = MASGEN(1:8)
         ZK24(JREFE+2) = RIGGEN(1:8)
         ZK24(JREFE+3) = AMOGEN(1:8)
      ENDIF
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
         CALL WKVECT(NOMRES//'           .REFD','G V K24',6,JREFE)
         ZK24(JREFE) = RIGGEN
         ZK24(JREFE+1) = MASGEN
         ZK24(JREFE+2) = AMOGEN
         ZK24(JREFE+3) = NUMGEN
         ZK24(JREFE+4) = ' '
         ZK24(JREFE+5) = BASEMO(1:8)
      ENDIF
C



C
      CALL JEEXIN(NOMRES//'           .DESC',IRET)
      IF (IRET.EQ.0) THEN
         CALL WKVECT(NOMRES//'           .DESC','G V I',5,JDESC)
         ZI(JDESC) = 1
         IF ( NBCHOC.NE.0 ) ZI(JDESC) = 2
C        --- DANS LE CAS ITMI ET ADAPT (METHODES A PAS VARIABLE),
C            ON MET LA VALEUR 3 QUI SERVIRA DE TEST
C             A LA COMMANDE POST_DYNA_MODA_T
         IF (METHOD.EQ.'ITMI' .OR. METHOD.EQ.'ADAPT') ZI(JDESC) = 3
C        ---
         ZI(JDESC+1) = NBMODE
         ZI(JDESC+2) = NBCHOC
         ZI(JDESC+3) = NBREDE
         ZI(JDESC+4) = NBREVI
      ENDIF
C
      IF (NBSAUV.NE.0) THEN
        CALL JECREO(NOMRES//'           .DEPL' ,'G V C')
        CALL JEECRA(NOMRES//'           .DEPL' ,'LONMAX',NBSTOC,KBID)
        CALL JEECRA(NOMRES//'           .DEPL' ,'LONUTI',NBSTOC,KBID)
        CALL JEVEUT(NOMRES//'           .DEPL' ,'E',JDEPL)
        CALL JECREO(NOMRES//'           .VITE' ,'G V C')
        CALL JEECRA(NOMRES//'           .VITE' ,'LONMAX',NBSTOC,KBID)
        CALL JEECRA(NOMRES//'           .VITE' ,'LONUTI',NBSTOC,KBID)
        CALL JEVEUT(NOMRES//'           .VITE' ,'E',JVITE)
        CALL JECREO(NOMRES//'           .ACCE' ,'G V C')
        CALL JEECRA(NOMRES//'           .ACCE' ,'LONMAX',NBSTOC,KBID)
        CALL JEECRA(NOMRES//'           .ACCE' ,'LONUTI',NBSTOC,KBID)
        CALL JEVEUT(NOMRES//'           .ACCE' ,'E',JACCE)
        CALL JEEXIN(NOMRES//'           .ORDR',IRET)
        IF (IRET.EQ.0) THEN
          CALL JECREO(NOMRES//'           .ORDR' ,'G V I')
          CALL JEECRA(NOMRES//'           .ORDR' ,'LONMAX',NBSAUV,KBID)
        ENDIF
        CALL JEECRA(NOMRES//'           .ORDR' ,'LONUTI',NBSAUV,KBID)
        CALL JEVEUT(NOMRES//'           .ORDR' ,'E',JORDR)
        CALL JEEXIN(NOMRES//'           .FREQ',IRET)
        IF (IRET.EQ.0) THEN
          CALL JECREO(NOMRES//'           .FREQ' ,'G V R')
          CALL JEECRA(NOMRES//'           .FREQ' ,'LONMAX',NBSAUV,KBID)
        ENDIF
        CALL JEECRA(NOMRES//'           .FREQ' ,'LONUTI',NBSAUV,KBID)
        CALL JEVEUT(NOMRES//'           .FREQ' ,'E',JABS)
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
 9999 CONTINUE
      CALL JEDEMA()
      END
