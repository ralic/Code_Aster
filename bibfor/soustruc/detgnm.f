      SUBROUTINE DETGNM ( MA )
      IMPLICIT   NONE
      CHARACTER*8 MA

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 08/06/2009   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     BUT: - TRAITE LES MOTS CLES FACTEURS DETR_GROUP_MA ET
C            DETR_GROUP_NO DE L'OPERATEUR DEFI_GROUP.
C          - PERMET DE DETRUIRE DES GROUPES (NOEUDS OU MAILLES)
C
C     IN : MA : NOM DU MAILLAGE
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
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER N1,IOCC,MAXVAL,NBVAL,JGMDET,IRET,NBTGP,NUMGM,NBGMDE
      INTEGER NBGP,NBMAGP,JGM,JMAGP,I,J,J1,J2,NGP,IG
      PARAMETER(NGP=2)
      CHARACTER*8 K8B,KBID,NOMGP
      CHARACTER*16 DETR(2),GROUP(2)
      CHARACTER*24 GRP
      DATA DETR  / 'DETR_GROUP_MA','DETR_GROUP_NO'/
      DATA GROUP / '.GROUPEMA','.GROUPENO'/
C
      CALL JEMARQ()
C
      DO 100 IG = 1 , NGP
         CALL GETFAC(DETR(IG),N1)
         IF(N1.NE.0)THEN
            CALL JEEXIN(MA//GROUP(IG),IRET)
            IF(IRET.EQ.0) GOTO 100
            CALL JELIRA(MA//GROUP(IG),'NUTIOC',NBTGP,KBID)
            CALL WKVECT('&&DETGNM.GROUP','V V I',NBTGP,JGM)
            DO 5 I=1,NBTGP
               ZI(JGM+I-1)=0
 5          CONTINUE
            DO 10 IOCC= 1 , N1
               MAXVAL=0
               CALL GETVID(DETR(IG),'NOM',IOCC,1,MAXVAL,K8B,NBVAL)
               NBVAL=-NBVAL
               CALL WKVECT('&&DETGNM.GROUP_DETR','V V K8',NBVAL,
     &              JGMDET)
               CALL GETVID(DETR(IG),'NOM',IOCC,1,NBVAL,
     &              ZK8(JGMDET),IRET)
C              ON RECUPERE LES NUMEROS DES GROUPES A DETRUIRE
               DO 15 I = 1 , NBVAL
                  CALL JENONU(JEXNOM(MA//GROUP(IG),ZK8(JGMDET+I-1)),
     &                 NUMGM)
                  IF(NUMGM.NE.0)THEN
                     ZI(JGM+NUMGM-1)=NUMGM
                  ENDIF
 15            CONTINUE
               CALL JEDETR('&&DETGNM.GROUP_DETR')
 10         CONTINUE
C           ON COMPTE LE NOMBRE DE GROUPES A DETRUIRE
            NBGMDE=0
            DO 20 I=1,NBTGP
               IF(ZI(JGM+I-1).NE.0)THEN
                  NBGMDE=NBGMDE+1
               ENDIF
 20         CONTINUE
C           REACTUALISATION DE L'OBJET .GROUPEMA (OU .GROUPENO)
            GRP='&&TMP   .GROUP          '
            NBGP=NBTGP-NBGMDE
            IF(NBGP.EQ.0)THEN
               CALL JEDETR(MA//GROUP(IG))
               GOTO 100
            ENDIF
            CALL JECREC(GRP,'V V I','NO','DISPERSE','VARIABLE',NBGP)
            DO 25 I=1,NBTGP
               IF(ZI(JGM+I-1).EQ.0)THEN
                  CALL JENUNO(JEXNUM(MA//GROUP(IG),I),NOMGP)
                  CALL JECROC(JEXNOM(GRP,NOMGP))
                  CALL JELIRA(JEXNOM(MA//GROUP(IG),NOMGP),'LONMAX',
     &                 NBMAGP,KBID)
                  CALL JEECRA(JEXNOM(GRP,NOMGP),'LONMAX',NBMAGP,KBID)
                  CALL JEVEUO(JEXNOM(GRP,NOMGP),'E',J2)
                  CALL JEVEUO(JEXNOM(MA//GROUP(IG),NOMGP),'L',J1)
                  DO 30 J=1,NBMAGP
                     ZI(J2+J-1)=ZI(J1+J-1)
 30               CONTINUE
               ENDIF
 25         CONTINUE
            CALL JEDETR(MA//GROUP(IG))
            CALL JEDUPO(GRP,'G',MA//GROUP(IG),.FALSE.)
            CALL JEDETC('V','&&',1)
         ENDIF
 100  CONTINUE

      CALL JEDEMA()

      END
