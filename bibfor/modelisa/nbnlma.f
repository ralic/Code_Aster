      SUBROUTINE NBNLMA ( NOMA, NBM, LIMANU, LIMANO, NBTYP, LITYP, NBN )
      IMPLICIT   NONE
      INTEGER        LIMANU(*), NBM, NBN, NBTYP
      CHARACTER*8    LITYP(*), NOMA
      CHARACTER*(*)  LIMANO(*)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/05/2000   AUTEUR CIBHHLV L.VIVAN 
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
C
C BUT : TROUVER LE NOMBRE DE NOEUDS QUI APPARTIENNENT A UNE LISTE DE
C       MAILLES, ET EVENTUELLEMENT LEURS NUMEROS
C       VERIFIER QUE LES MAILLES DE CETTE LISTE SONT D'UN TYPE CORRECT
C
C ARGUMENTS D'ENTREE:
C      NOMA : NOM DU MAILLAGE
C      NBM  : NOMBRE DE MAILLES DANS LA LISTE.
C              SI >0 LA LISTE EST NUMEROTEE ==> LIMANU
C              SI <0 LA LISTE EST NOMMEE    ==> LIMANO
C      NBTYP: NOMBRE DE TYPE_MAILLES DANS LA LISTE LITYP.
C ARGUMENTS DE SORTIE:
C      NBN  : NOMBRE DE NOEUDS
C OBJETS JEVEUX CREES
C      &&NBNLMA.LN   : NUMEROS DES NOEUDS (CREE SI ICOD=1)
C      &&NBNLMA.NBN  : NOMBRES D'OCCURENCES DES NOEUDS (CREE SI ICOD=1)
C-----------------------------------------------------------------------
C --------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNOM, JEXNUM
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      IATYMA, IBID, IRET, IT, ITROU, J, JDES, JLN, JNBN,
     +             JTYP, M, MI, N, NBNA, NBNM, NN, NUMTYP
      CHARACTER*8  MK
      CHARACTER*1  K1BID
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      NBNM=0
      DO 1 M=1,ABS(NBM)
         IF (NBM.GT.0) THEN
            MI=LIMANU(M)
            CALL JEVEUO(NOMA//'.TYPMAIL','L',IATYMA)
            JTYP=IATYMA-1+MI
         ELSE IF (NBM.LT.0) THEN
            MK=LIMANO(M)
            CALL JENONU(JEXNOM(NOMA//'.NOMMAI',MK),IBID)
            CALL JEVEUO(NOMA//'.TYPMAIL','L',IATYMA)
            JTYP=IATYMA-1+IBID
         END IF
         NN=0
         DO 2 IT=1,NBTYP
            CALL JENONU(JEXNOM('&CATA.TM.NBNO',LITYP(IT)),NUMTYP)
            IF (ZI(JTYP).EQ.NUMTYP) THEN
               IF(NBM.GT.0) THEN
                 CALL JELIRA(JEXNUM(NOMA//'.CONNEX',MI),'LONMAX',
     +                       NN,K1BID)
               ELSE IF(NBM.LT.0) THEN
                 CALL JENONU(JEXNOM(NOMA//'.NOMMAI',MK),IBID)
                 CALL JELIRA(JEXNUM(NOMA//'.CONNEX',IBID),'LONMAX',
     +                       NN,K1BID)
               END IF
            END IF
    2    CONTINUE
         IF (NN.EQ.0) THEN
            IF (NBM.GT.0) THEN
               CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',MI),MK)
            ELSE
               CALL JENONU(JEXNOM(NOMA//'.NOMMAI',MK),MI)
            END IF
            CALL UTDEBM('F','NBNLMA',' LA MAILLE ' )
            CALL UTIMPI('S',' DE NUM ',1,MI)
            CALL UTIMPK('L',' DE NOM ',1,MK//' A UN TYPE NON '//
     &                                  'CONFORME AU CALCUL ENVISAGE')
            CALL UTFINM()
         ELSE
            NBNM=NBNM+NN
         END IF
    1 CONTINUE
      CALL JEEXIN('&&NBNLMA.LN',IRET)
      IF (IRET.NE.0) CALL JEDETR('&&NBNLMA.LN')
      CALL JECREO('&&NBNLMA.LN','V V I')
      CALL JEECRA('&&NBNLMA.LN','LONMAX',NBNM,' ')
      CALL JEVEUO('&&NBNLMA.LN','E',JLN)
      CALL JEEXIN('&&NBNLMA.NBN',IRET)
      IF (IRET.NE.0) CALL JEDETR('&&NBNLMA.NBN')
      CALL JECREO('&&NBNLMA.NBN','V V I')
      CALL JEECRA('&&NBNLMA.NBN','LONMAX',NBNM,' ')
      CALL JEVEUO('&&NBNLMA.NBN','E',JNBN)
      DO 3 M=1,ABS(NBM)
         IF (NBM.GT.0) THEN
            MI=LIMANU(M)
            CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',MI),'L',JDES)
            CALL JELIRA(JEXNUM(NOMA//'.CONNEX',MI),'LONMAX',NN,K1BID)
         ELSE IF (NBM.LT.0) THEN
            MK=LIMANO(M)
            CALL JENONU(JEXNOM(NOMA//'.NOMMAI',MK),IBID)
            CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',IBID),'L',JDES)
            CALL JELIRA(JEXNUM(NOMA//'.CONNEX',IBID),'LONMAX',NN,K1BID)
         END IF
         DO 4 N=1,NN
C
C        NBNA EST LE NOMBRE DE NOEUDS ACTUELLEMENT STOCKES
C
            CALL JELIRA('&&NBNLMA.LN','LONUTI',NBNA,K1BID)
            ITROU=0
C
C        SI LE NUMERO DE NOEUD (ZI(JDES-1+N)) EXISTE DEJA DANS .LN
C        ON INCREMENTE A LA PLACE J  DANS LE TABLEAU &&NBNLMA.NBN
C
            DO 5 J=1,NBNA
               IF (ZI(JDES-1+N).EQ.ZI(JLN-1+J)) THEN
                  ZI(JNBN-1+J)=ZI(JNBN-1+J)+1
                  ITROU=1
               END IF
    5       CONTINUE
C
C        SI LE NUMERO DE NOEUD (ZI(JDES-1+N)) N'EXISTE PAS,
C        ON LE STOCKE A LA PLACE NBNA (A LA FIN ) DANS LE TABLEAU .LN
C        ET ON STOCKE 1 A LA PLACE NBNA LE TABLEAU &&NBNLMA.NBN
C
            IF (ITROU.EQ.0) THEN
               NBNA=NBNA+1
               ZI(JLN-1+NBNA)=ZI(JDES-1+N)
               ZI(JNBN-1+NBNA)=1
               CALL JEECRA('&&NBNLMA.LN','LONUTI',NBNA,' ')
               CALL JEECRA('&&NBNLMA.NBN','LONUTI',NBNA,' ')
            END IF
    4    CONTINUE
    3 CONTINUE
      NBN=NBNA
      CALL JEDEMA()
      END
