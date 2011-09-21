      SUBROUTINE TFVEGR(CMD,NOMMCF,OCGRIL)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C     APPELANT : TFVERI, OP0143 , OPERATEUR DEFI_FLUI_STRU
C     VERIFICATIONS DE PREMIER NIVEAU : MOT-CLE FACTEUR FAISCEAU_AXIAL,
C     OPERANDES CARACTERISTIQUES DES GRILLES
C-----------------------------------------------------------------------
C  IN   : CMD    : NOM DE LA COMMANDE
C  IN   : NOMMCF : NOM DU MOT-CLE FACTEUR UTILISE (FAISCEAU_AXIAL)
C  IN   : OCGRIL : OCCURENCE DU MOT-CLE FACTEUR POUR LAQUELLE ON
C                  VERIFIE LES ARGUMENTS FOURNIS SOUS LES OPERANDES
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*16  CMD, NOMMCF
      INTEGER       OCGRIL, NTYPG
      INTEGER      IARG
C
C    ------------------------------------------------------------------
      CALL JEMARQ()
C
           CALL GETVR8(NOMMCF,'LONG_TYPG',OCGRIL,IARG,0,RBID,NTYPG)
           NTYPG = ABS(NTYPG)
           CALL GETVR8(NOMMCF,'COOR_GRILLE',OCGRIL,IARG,0,RBID,NBGTOT)
           NBGTOT = ABS(NBGTOT)
           IF (NBGTOT.LT.NTYPG) THEN
              CALL U2MESS('E','MODELISA7_15')
           ENDIF
           CALL GETVIS(NOMMCF,'TYPE_GRILLE',OCGRIL,IARG,0,IBID,NTOT2)
           IF (ABS(NTOT2).NE.NBGTOT) THEN
              CALL U2MESS('E','MODELISA7_16')
           ENDIF
           CALL WKVECT('&&TFVEGR.TEMP.VECI','V V I',NBGTOT,IVECI)
           CALL GETVIS(NOMMCF,'TYPE_GRILLE',OCGRIL,IARG,NBGTOT,
     &                 ZI(IVECI),
     &                 IBID)
           DO 100 IGRIL = 1, NBGTOT
              IF ((ZI(IVECI+IGRIL-1).LT.1).OR.
     &            (ZI(IVECI+IGRIL-1).GT.NTYPG)) THEN
                 CALL U2MESS('E','MODELISA7_17')
              ENDIF
 100       CONTINUE
           CALL GETVR8(NOMMCF,'LARG_TYPG',OCGRIL,IARG,0,RBID,NTYPG2)
           CALL GETVR8(NOMMCF,'EPAI_TYPG',OCGRIL,IARG,0,RBID,NTYPG3)
           CALL GETVR8(NOMMCF,'RUGO_TYPG',OCGRIL,IARG,0,RBID,NTYPG4)
           CALL GETVR8(NOMMCF,'COEF_TRAI_TYPG',OCGRIL,IARG,0,
     &                 RBID,NTYPG5)
           CALL GETVR8(NOMMCF,'COEF_DPOR_TYPG',OCGRIL,IARG,0,
     &                 RBID,NTYPG6)
           IF ((ABS(NTYPG2).NE.NTYPG).OR.(ABS(NTYPG3).NE.NTYPG).OR.
     &         (ABS(NTYPG4).NE.NTYPG).OR.(ABS(NTYPG5).NE.NTYPG).OR.
     &         (ABS(NTYPG6).NE.NTYPG)) THEN
              CALL U2MESS('E','MODELISA7_18')
           ENDIF

C
      CALL JEDETC('V','&&TFVEGR',1)
      CALL JEDEMA()
      END
