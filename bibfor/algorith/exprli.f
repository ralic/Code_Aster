      SUBROUTINE EXPRLI(BASMDZ,LINTFZ,NMINTZ,NUMINT,FAMPRZ,II,ORDO)
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
C    P. RICHARD     DATE 23/05/91
C-----------------------------------------------------------------------
C  BUT:  < DETERMINATION PROFNO  INTERFACE >
      IMPLICIT NONE
C
C  CONSISTE A DETERMINER UN MINI PROFNO POUR UNE INTERFACE
C
C  PROFLI(1,I)=NUMERO DU PREMIER DDL DU IEME NOEUD DE
C              L'INTERFACE DANS LA MATRICE DE LIAISON
C               (1 DDL = 1 LIGNE)
C  PROFLI(2,I)= ENTIER CODE DES TYPE DDL ACTIF AU NOEUD DANS
C              L'INTERFACE
C
C C'EST A DIRE POUR CHAQUE NOEUDS DE L'INTERFACE LE RANG DE SON PREMIER
C  DDL ACTIF DANS LA MATRICE DE LIAISON ET L'ENTIER CODE DES DDL ACTIFS
C A LA LIAISON
C
C-----------------------------------------------------------------------
C
C BASMDZ   /I/: NOM UT DE LA BASE_MODALE
C LINTFZ   /I/: NOM UT DE LA LIST_INTERFACE
C NMINTZ   /I/: NOM DE L'INTERFACE
C NUMINT   /I/: NUMERO DE L'INTERFACE
C FAMPRZ   /I/: FAMILLE DES MINI-PROFNO
C II       /I/: NUMERO DU PROFNO A CREER DANS LA FAMILLE
C
C-------- DEBUT COMMUNS NORMALISES  JEVEUX  ----------------------------
C
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
C
      CHARACTER*32 JEXNOM, JEXNUM
C
C----------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER     IDEC(30),LLINT4,NBCMPM,LLREF,NBEC,IERD,NBCMP,
     &            NBDDL,ORDO,II,LLACT,IEC,LDMAP,NUMINT,NBDEF,IBID,
     &            NBNOE,ICOMP,I,J
      PARAMETER   (NBCMPM=10)
      CHARACTER*(*) BASMDZ,NMINTZ,LINTFZ,FAMPRZ
      CHARACTER*1 K1BID
      CHARACTER*4 NLIAI
      CHARACTER*8 BASMOD,NOMINT,LINTF,KBID,BLANC,NOMG,TEMP
      CHARACTER*24 ORDOD
      CHARACTER*32 NOMMAT,NOMPRL,FAMPRL
C
C-----------------------------------------------------------------------
      DATA BLANC /'        '/
C-----------------------------------------------------------------------
C
C-------------RECUPERATION LIST_INTERFACE AMONT SI BASE MODALE----------
C
      CALL JEMARQ()
C
      BASMOD = BASMDZ
      NOMINT = NMINTZ
      LINTF  = LINTFZ
      FAMPRL = FAMPRZ
C
      IF(BASMOD.NE.BLANC) THEN
        CALL JEVEUO(BASMOD//'           .REFD','L',LLREF)
        LINTF=ZK24(LLREF)
      ENDIF
C
C-----RECUPERATION DU NOMBRE DU NOMBRE D'ENTIERS CODES ASSOCIE A DEPL_R
C
      NOMG = 'DEPL_R'
      CALL DISMOI('F','NB_EC',NOMG,'GRANDEUR',NBEC,KBID,IERD)
      IF (NBEC.GT.10) THEN
         CALL UTMESS('F','EXPRLI',
     +                   'LE DESCRIPTEUR_GRANDEUR DES DEPLACEMENTS'//
     +                    ' NE TIENT PAS SUR DIX ENTIERS CODES')
      ENDIF
      CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOMG),'LONMAX',NBCMP,K1BID)
C
C----------------RECUPERATION EVENTUELLE DU NUMERO INTERFACE------------
C
      IF(NOMINT.NE.'             ') THEN
        CALL JENONU(JEXNOM(LINTF//'      .INTD.NOMS',NOMINT),NUMINT)
      ENDIF
C
C----------------RECUPERATION DU NOMBRE DE DDL GENERALISES--------------
C
      CALL BMNBMD(BASMOD,'TOUT',NBDEF)
C
C----RECUPERATION DU NOMBRE DE DDL  ET NOEUDS ASSOCIES A L'INTERFACE----
C
      KBID=' '
      CALL BMRDDA(BASMOD,KBID,NOMINT,NUMINT,0,IBID,NBDDL,ORDO,II)
      KBID=' '
      CALL BMNOIN(BASMOD,KBID,NOMINT,NUMINT,0,IBID,NBNOE)
C
C-------ALLOCATION DU MINI PROFNO LIAISON INTERFACE COURANTE------------
C
      CALL JEECRA(JEXNUM(FAMPRL,II),'LONMAX',NBNOE*(1+NBEC),' ')
      CALL JEVEUO(JEXNUM(FAMPRL,II),'E',LDMAP)
C
C--------------------------DETERMINATION DU PRNO------------------------
C
      CALL JEVEUO(JEXNUM(LINTF//'      .INTD.DDAC',NUMINT),'L',LLACT)
C
      ICOMP=0
      DO 10 I=1,NBNOE
        DO 20 IEC = 1, NBEC
          IF (ORDO.EQ.0) THEN
            ZI(LDMAP+(1+NBEC)*(I-1)+IEC)=ZI(LLACT+(I-1)*NBEC+IEC-1)
          ELSE
            TEMP='REORDRE_'
            CALL CODENT(II,'D',NLIAI)
            ORDOD=TEMP//'      .LDAC.'//NLIAI
            CALL JEVEUO(ORDOD,'L',LLINT4)
            ZI(LDMAP+(1+NBEC)*(I-1)+IEC)=ZI(LLINT4+(I-1)*NBEC+IEC-1)
          ENDIF
  20    CONTINUE
        ZI(LDMAP+(1+NBEC)*(I-1))=ICOMP+1
        IF (ORDO.EQ.0) THEN
          CALL ISDECO(ZI(LLACT+(I-1)*NBEC+1-1),IDEC,NBCMPM)
        ELSE
            TEMP='REORDRE_'
            CALL CODENT(II,'D',NLIAI)
            ORDOD=TEMP//'      .LDAC.'//NLIAI
            CALL JEVEUO(ORDOD,'L',LLINT4)
            CALL ISDECO(ZI(LLINT4+(I-1)*NBEC+1-1),IDEC,NBCMPM)
        ENDIF
        DO 30 J=1,6
          ICOMP=ICOMP+IDEC(J)
 30     CONTINUE
 10   CONTINUE
C
 9999 CONTINUE
      CALL JEDEMA()
      END
