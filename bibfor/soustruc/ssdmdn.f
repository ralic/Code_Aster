      SUBROUTINE SSDMDN ( MAG )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
      IMPLICIT NONE
C     ARGUMENTS:
C     ----------
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      CHARACTER*8 MAG
C ----------------------------------------------------------------------
C     BUT:
C        - TRAITER LE MOT CLEF "DEFI_NOEUD"
C          DE LA COMMANDE DEFI_MAILLAGE.
C        - CREER LES OBJETS :
C            BASE VOLATILE: .NOMNOE_2
C
C     IN:
C        MAG : NOM DU MAILLAGE QUE L'ON DEFINIT.
C
      CHARACTER*8  NOMACR,NOSMA,KBID,MAL,PREF,NOMNOL,NOMNOG
      INTEGER INDI(4)
      CHARACTER*24 VALK(2)
      INTEGER      IARG
C ----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,I1 ,IACONX ,IADIM2 ,IADIME ,IALINO ,IANCNF
      INTEGER IANMCR ,IANON2 ,IASUPM ,IBID ,IED ,INO ,INO1
      INTEGER INOL ,IOCC ,ISMA ,KK ,LMAIL ,LNOEU ,LONGT
      INTEGER LPREF ,N1 ,N2 ,N3 ,NBNOE ,NBNOET ,NBNOEX
      INTEGER NBNOL ,NBSMA ,NNNOE ,NOCC,INDIIS
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL JEVEUO(MAG//'.DIME','L',IADIME)
      CALL JEVEUO(MAG//'.DIME_2','L',IADIM2)
      CALL JEVEUO(MAG//'.NOEUD_CONF','L',IANCNF)
      CALL JEVEUO(MAG//'.NOMACR','L',IANMCR)
      NNNOE= ZI(IADIME-1+1)
      NBSMA= ZI(IADIME-1+4)
C
      CALL WKVECT(MAG//'.NOMNOE_2','V V K8',NNNOE,IANON2)
C
C
C     -- BOUCLE SUR LES OCCURENCES DU MOT-CLEF:
C     -----------------------------------------
      CALL GETFAC('DEFI_NOEUD',NOCC)
      DO 1, IOCC=1,NOCC
        CALL GETVTX('DEFI_NOEUD','TOUT',IOCC,IARG,1,KBID,N1)
        IF (N1.EQ.1) THEN
C
C       -- CAS : TOUT: 'OUI'
C       --------------------
          LPREF=0
          CALL GETLTX('DEFI_NOEUD','PREFIXE',IOCC,8,1,LPREF,N2)
          CALL GETVIS('DEFI_NOEUD','INDEX',IOCC,IARG,4,INDI,N3)
          LMAIL=INDI(2)-INDI(1)+1
          LNOEU=INDI(4)-INDI(3)+1
          LMAIL=MAX(LMAIL,0)
          LNOEU=MAX(LNOEU,0)
          LONGT= LPREF+LMAIL+LNOEU
          IF (LONGT.GT.8) CALL U2MESS('F','SOUSTRUC_57')
          IF (LPREF.GT.0)
     &    CALL GETVTX('DEFI_NOEUD','PREFIXE',IOCC,IARG,1,PREF,N2)
C
          DO 2 , ISMA=1,NBSMA
            CALL JEVEUO(JEXNUM(MAG//'.SUPMAIL',ISMA),'L',IASUPM)
            CALL JENUNO(JEXNUM(MAG//'.SUPMAIL',ISMA),NOSMA)
            NOMACR= ZK8(IANMCR-1+ISMA)
            CALL JEVEUO(NOMACR//'.CONX','L',IACONX)
            CALL DISMOI('F','NOM_MAILLA',NOMACR,'MACR_ELEM_STAT'
     &                    ,IBID,MAL,IED)
            NBNOE=ZI(IADIM2-1+4*(ISMA-1)+1)
            NBNOL=ZI(IADIM2-1+4*(ISMA-1)+2)
            NBNOET=NBNOE+NBNOL
            DO 3 , I=1,NBNOET
              INO= ZI(IASUPM-1+I)
              IF (INO.GT.NNNOE) GO TO 3
              INO1= ZI(IACONX-1+3*(I-1)+2)
              CALL JENUNO(JEXNUM(MAL//'.NOMNOE',INO1),NOMNOL)
              I1=1
              IF (LPREF.GT.0) ZK8(IANON2-1+INO)(I1:I1-1+LPREF)
     &                         = PREF(1:LPREF)
              I1= I1+LPREF
              IF (LMAIL.GT.0) ZK8(IANON2-1+INO)(I1:I1-1+LMAIL)
     &                         = NOSMA(INDI(1):INDI(2))
              I1= I1+LMAIL
              IF (LNOEU.GT.0) ZK8(IANON2-1+INO)(I1:I1-1+LNOEU)
     &                         = NOMNOL(INDI(3):INDI(4))
   3        CONTINUE
   2      CONTINUE
        ELSE
C
C
C       -- CAS : MAILLE, NOEUD_FIN, NOEUD_INIT :
C       ---------------------------------------
          CALL GETVTX('DEFI_NOEUD','SUPER_MAILLE',
     &               IOCC,IARG,1,NOSMA,N1)
          CALL GETVTX('DEFI_NOEUD','NOEUD_FIN',
     &                  IOCC,IARG,1,NOMNOG,N2)
          CALL GETVTX('DEFI_NOEUD','NOEUD_INIT',
     &                   IOCC,IARG,1,NOMNOL,N3)
          IF((N1*N2*N3).EQ.0) CALL U2MESS('F','SOUSTRUC_58')
C
          CALL JENONU(JEXNOM(MAG//'.SUPMAIL',NOSMA),ISMA)
          NOMACR= ZK8(IANMCR-1+ISMA)
          CALL JEVEUO(NOMACR//'.LINO','L',IALINO)
          CALL JELIRA(NOMACR//'.LINO','LONUTI',NBNOEX,KBID)
          CALL DISMOI('F','NOM_MAILLA',NOMACR,'MACR_ELEM_STAT'
     &                    ,IBID,MAL,IED)
          CALL JENONU(JEXNOM(MAL//'.NOMNOE',NOMNOL),INOL)
          KK= INDIIS(ZI(IALINO),INOL,1,NBNOEX)
          IF (KK.EQ.0) THEN
             VALK(1) = NOMNOL
             VALK(2) = NOSMA
             CALL U2MESK('A','SOUSTRUC_59', 2 ,VALK)
            GO TO 1
          END IF
C
          INO=ZI(IADIM2-1+4*(ISMA-1)+3)+KK
          IF (ZI(IANCNF-1+INO).EQ.INO) THEN
            ZK8(IANON2-1+INO)= NOMNOG
          ELSE
             VALK(1) = NOMNOL
             VALK(2) = NOSMA
             CALL U2MESK('A','SOUSTRUC_60', 2 ,VALK)
          END IF
        END IF
 1    CONTINUE
C
C

      CALL JEDEMA()
      END
