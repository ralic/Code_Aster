      SUBROUTINE REHAEC(NOMRES,RESGEN,NOMSST)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/06/2005   AUTEUR VABHHTS J.PELLET 
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
C***********************************************************************
C    T. KERBER     DATE 14/05/93
C-----------------------------------------------------------------------
C  BUT:      < RESTITUTION HARMONIQUE ECLATEE >
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      RESTITUER EN BASE PHYSIQUE SUR UNE SOUS-STRUCTURE LES RESULTATS
C                ISSU DE LA SOUS-STRUCTURATION GENERALE
C  LE CONCEPT RESULTAT EST UN RESULTAT COMPOSE "DYNA_HARMO"
C
C-----------------------------------------------------------------------
C
C NOMRES   /I/: NOM K8 DU CONCEPT DYNA HARMO RESULTAT
C RESGEN   /I/: NOM K8 DU HARM_GENE AMONT
C NOMSST   /I/: NOM K8 DE LA SOUS-STRUCTURE SUR LAQUELLE ON RESTITUE
C
C-------- DEBUT COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
      CHARACTER*32 JEXNUM,JEXNOM
C
C----------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*6 PGC
      CHARACTER*8 CHSYMB,SYMB(3)
      CHARACTER*8 NOMRES,BASMOD,MAILLA,LINT
      CHARACTER*8 NOMSST,MODGEN,RESGEN,SOUTR
      CHARACTER*19 RAID,NUMDDL
      CHARACTER*19 NUMGEN,CHAMNE,NUME(3)
      CHARACTER*24 CREFE(2),CHAMOL,CHAMBA
      CHARACTER*24 CHAMNO,CHAREF
      CHARACTER*16 NOMPAR(1)
      INTEGER IAFREQ(1)
      CHARACTER*8 KBID
      COMPLEX*16  CBID
C
C-----------------------------------------------------------------------
      DATA PGC/'REHAEC'/
      DATA SOUTR/'&SOUSSTR'/
      DATA NOMPAR/'FREQ'/
C-----------------------------------------------------------------------
C
C-----------------ECRITURE DU TITRE-------------------------------------
C
      CALL JEMARQ()
      CALL TITRE
C
C-----DETERMINATION DES CHAMPS CALCULES LORS DES CALCULS HARMONIQUES----
C
C     LES NOMS SYMBOLIQUES DES CHAMPS CALCULES, PARMI DEPL, VITE ET ACCE
C     SONT STOCKES DANS LE VECTEUR SYMB. LEURS NUME_DDL SONT ECRITS
C     DANS LE VECTEUR NUM.
C
      ISYMB = 0
C
      CALL RSEXCH(RESGEN,'DEPL',1,CHAMNO,IRET)
C
      IF (IRET.EQ.0) THEN
        ISYMB = ISYMB + 1
        SYMB(ISYMB) = 'DEPL'
        CHAREF = CHAMNO(1:19)//'.REFE'
        CALL JEVEUO(CHAREF,'L',LREF)
        NUME(ISYMB) = ZK24(LREF+1)
        CALL JELIBE(CHAREF)
      END IF
C
C
      CALL RSEXCH(RESGEN,'VITE',1,CHAMNO,IRET)
C
      IF (IRET.EQ.0) THEN
        ISYMB = ISYMB + 1
        SYMB(ISYMB) = 'VITE'
        CHAREF = CHAMNO(1:19)//'.REFE'
        CALL JEVEUO(CHAREF,'L',LREF)
        NUME(ISYMB) = ZK24(LREF+1)
        CALL JELIBE(CHAREF)
      END IF
C
C
      CALL RSEXCH(RESGEN,'ACCE',1,CHAMNO,IRET)
C
      IF (IRET.EQ.0) THEN
        ISYMB = ISYMB + 1
        SYMB(ISYMB) = 'ACCE'
        CHAREF = CHAMNO(1:19)//'.REFE'
        CALL JEVEUO(CHAREF,'L',LREF)
        NUME(ISYMB) = ZK24(LREF+1)
        CALL JELIBE(CHAREF)
      END IF
C
C     NOMBRE DE CHAMPS SYMBOLIQUES CALCULES.
C     ON S'ASSURE QUE LEUR NOMBRE EST NON NUL.
C
      NBSYMB = ISYMB
C
      IF (NBSYMB.EQ.0) THEN
        CALL UTDEBM('F',PGC,'AUCUN CHAMP N''EST CALCULE')
        CALL UTIMPK('L','DANS LA STRUCTURE DE DONNEES ',1,RESGEN)
        CALL UTFINM
      END IF
C
C     VERIFICATION DE LA COHERENCE DES NUM_DDL
C
      NUMGEN = NUME(1)
      IF (NBSYMB.GT.1) THEN
        DO 10 I = 2,NBSYMB
          IF (NUME(I).NE.NUMGEN) THEN
            CALL UTDEBM('F',PGC,
     +                 'LES NUMEROTATIONS DES CHAMPS NE COINCIDENT PAS '
     +                  )
            CALL UTIMPK('L','CELUI DE ',1,SYMB(I))
            CALL UTIMPK('S',' EST : ',1,NUME(I))
            CALL UTIMPK('L','ET CELUI DE ',1,SYMB(1))
            CALL UTIMPK('S',' EST : ',1,NUMGEN)
            CALL UTFINM
          END IF

   10   CONTINUE
      END IF
C
C
C-------------------RECUPERATION DU MODELE GENERALISE-------------------
C
      NUMGEN(15:19) = '.NUME'
      CALL JEVEUO(NUMGEN//'.REFN','L',LLREF)
      MODGEN = ZK24(LLREF)
C
C--------------------RECUPERATION NUMERO DE SOUS-STRUCTURE--------------
C                 ET DU NOEUD TARDIF CORRESPONDANT
C
      CALL JENONU(JEXNOM(MODGEN//'      .MODG.SSNO',NOMSST),NUSST)
      IF (NUSST.EQ.0) THEN
        CALL UTDEBM('F',PGC,
     +            'SOUS-STRUCTURE INEXISTANTE DANS LE MODELE GENERALISE'
     +              )
        CALL UTIMPK('L','MODELE GENERALISEE',1,MODGEN)
        CALL UTIMPK('L','SOUS-STRUCTURE',1,NOMSST)
        CALL UTFINM
      END IF
C
      CALL JENONU(JEXNOM(NUMGEN//'.LILI',SOUTR),IBID)
      CALL JEVEUO(JEXNUM(NUMGEN//'.ORIG',IBID),'L',LLORS)
      CALL JENONU(JEXNOM(NUMGEN//'.LILI',SOUTR),IBID)
      CALL JELIRA(JEXNUM(NUMGEN//'.ORIG',IBID),'LONMAX',NBSST,KBID)
C
      DO 20 I = 1,NBSST
        IF (ZI(LLORS+I-1).EQ.NUSST) NUTARS = I
   20 CONTINUE
C
C
C     NOMBRE DE MODES ET NUMERO DU PREMIER D.D.L. DE LA SOUS-STRUCTURE
C
      CALL JENONU(JEXNOM(NUMGEN//'.LILI',SOUTR),IBID)
      CALL JEVEUO(JEXNUM(NUMGEN//'.PRNO',IBID),'L',LLPRS)
      NBDDG = ZI(LLPRS+ (NUTARS-1)*2+1)
      IEQ = ZI(LLPRS+ (NUTARS-1)*2)
C
C----------------------RECUPERATION DE LA BASE MODALE-------------------
C   ET VERIFICATION COHERENCE NOMBRE DE CHAMPS DE LA BASE
C    (QUI NORMALEMENT NE DOIT PAS DECLENCHER MAIS ON EST PRUDENT)
C
      CALL MGUTDM(MODGEN,NOMSST,IBID,'NOM_BASE_MODALE',IBID,BASMOD)
C
      CALL BMNBMD(BASMOD,'TOUT',NBBAS)
      IF (NBBAS.NE.NBDDG) THEN
        CALL UTDEBM('F',PGC,
     +              'PROBLEME COHERENCE NOMBRE DE CHAMPS BASE MODALE')
        CALL UTIMPK('L','BASE MODALE',1,BASMOD)
        CALL UTIMPI('L','NOMBRE DE CHAMPS DE LA BASE',1,NBBAS)
        CALL UTIMPI('L','NOMBRE DE DGRES GENERALISES',1,NBDDG)
        CALL UTFINM
      END IF
C
      CALL JEVEUO(BASMOD//'           .REFD','L',LLREF)
      LINT = ZK24(LLREF)
      CALL JELIBE(BASMOD//'           .REFD')
      CALL DISMOI('F','NOM_MAILLA',LINT,'INTERF_DYNA',IBID,MAILLA,IRET)
      CALL DISMOI('F','NOM_NUME_DDL',LINT,'INTERF_DYNA',IBID,NUMDDL,
     +            IRET)
      CALL DISMOI('F','NB_EQUA',NUMDDL,'NUME_DDL',NEQ,KBID,IRET)
C
      CREFE(1) = MAILLA
      CREFE(2) = NUMDDL
C
C--------------RECUPERATION NOMBRE DE FREQUENCES D'EXCITATION ETUDIEES--
C
      CALL RSORAC(RESGEN,'LONUTI',IBID,RBID,KBID,CBID,RBID,KBID,
     &            NBFREQ,1,IBID)
C
      WRITE (6,*) 'NBFREQ = ',NBFREQ
C
C--------------------ALLOCATION STRUCTURE DE DONNEES RESULTAT-----------
C
      CALL RSCRSD(NOMRES,'DYNA_HARMO',NBFREQ)
C
C----------------------RESTITUTION PROPREMENT DITE----------------------
C
      CALL JEVEUO(NUMGEN//'.NUEQ','L',LLNUEQ)
C
C     BOUCLE SUR LES FREQUENCES ETUDIEES
C
      DO 30 I = 1,NBFREQ
C
C     BOUCLE SUR LES CHAMPS A RESTITUER
C
        DO 40 ISYMB = 1,NBSYMB
C
          CHSYMB = SYMB(ISYMB)
C
C  REQUETTE NOM ET ADRESSE CHAMNO GENERALISE
          CALL DCAPNO(RESGEN,CHSYMB,I,CHAMOL)
          CALL JEVEUO(CHAMOL,'L',LLCHOL)
C
C  REQUETTE NOM ET ADRESSE NOUVEAU CHAMNO
          CALL RSEXCH(NOMRES,CHSYMB,I,CHAMNE,IER)
C  CREATION DU .REFE
          CALL VTCREA(CHAMNE,CREFE,'G','C',NEQ)
          CALL JEVEUO(CHAMNE//'.VALE','E',LDNEW)
C
C  BOUCLE SUR LES MODES PROPRES DE LA BASE
C
          DO 50 J = 1,NBDDG
C
            CALL DCAPNO(BASMOD,'DEPL',J,CHAMBA)
            CALL JEVEUO(CHAMBA,'L',LLCHAB)
CALL JEIMPO('MESSAGE',CHAMBA,' ','BIDON')
C
            IAD = LLCHOL + ZI(LLNUEQ+IEQ+J-2) - 1
C
C     BOUCLE SUR LES EQUATIONS PHYSIQUES
C
            DO 60 K = 1,NEQ
C
              ZC(LDNEW+K-1) = ZC(LDNEW+K-1) + ZR(LLCHAB+K-1)*ZC(IAD)
   60       CONTINUE
C
            CALL JELIBE(CHAMBA)
   50     CONTINUE
          CALL RSNOCH(NOMRES,CHSYMB,I,' ')
C
          CALL JELIBE(CHAMOL)
   40   CONTINUE
C
C     ENREGISTREMENT DES FREQUENCES DANS NOMRES
C
        CALL RSADPA(RESGEN,'L',1,NOMPAR,I,0,IAFREQ,KBID)
        FREQ = ZR(IAFREQ(1))
        WRITE (6,*) 'TOUT VA BIEN, FREQ=',FREQ
        CALL RSADPA(NOMRES,'E',1,NOMPAR,I,0,IAFREQ,KBID)
        ZR(IAFREQ(1)) = FREQ
C
   30 CONTINUE
C
      CALL JELIBE(NUMGEN//'.NUEQ')
C
      GOTO 9999

 9999 CONTINUE
      CALL JEDEMA()
      END
