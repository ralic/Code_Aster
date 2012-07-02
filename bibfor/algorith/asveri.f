      SUBROUTINE ASVERI(KNOMSY,NBOPT,MECA,PSMO,STAT,TRONC,MONOAP,NBSUP,
     &                                  NSUPP,NOMSUP,NDIR,NORDR,NBMODE)
      IMPLICIT NONE
      INTEGER           NDIR(*),NORDR(*),NSUPP(*)
      INTEGER VALI
      CHARACTER*(*)     KNOMSY(*),MECA,PSMO,STAT,NOMSUP(NBSUP,*)
      LOGICAL           TRONC,MONOAP
C     ------------------------------------------------------------------
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
C     ------------------------------------------------------------------
C     COMMANDE : COMB_SISM_MODAL
C        VERIFICATION DES OPTIONS DE CALCUL ET DES MODES
C     ------------------------------------------------------------------
C IN  : KNOMSY : VECTEUR DES OPTIONS DE CALCUL
C IN  : NBOPT  : NOMBRE D'OPTIONS DE CALCUL
C IN  : MECA   : MODES MECANIQUES
C IN  : STAT   : MODES STATIQUES
C IN  : TRONC  : PRISE EN COMPTE DE LA TRONCATURE
C IN  : MONOAP : = .TRUE.  , STRUCTURE MONO-SUPPORT
C                = .FALSE. , STRUCTURE MULTI-SUPPORT
C IN  : NBSUP  : NOMBRE DE SUPPORTS
C IN  : NOMSUP : VECTEUR DES NOMS DES SUPPORTS
C IN  : NDIR   : VECTEUR DES DIRECTIONS DE CALCUL
C IN  : NORDR  : NUMERO D'ORDRE DES MODES MECANIQUES
C IN  : NBMODE : NOMBRE DE MODES MECANIQUES
C     ------------------------------------------------------------------
      CHARACTER*4  CTYP
      CHARACTER*8  K8B, RESU, NOEU, CMP, NOMCMP(3)
      CHARACTER*16 NOMSY, CONCEP, NOMCMD, ACCES(3), MONACC, MONPAR
      CHARACTER*19 CHEXTR, CHEXT2
      CHARACTER*24 VALK(3)
      COMPLEX*16   CBID
      INTEGER      IARG
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IB ,IBID ,ID ,IER ,IM ,IN ,INUM 
      INTEGER IORDR ,IRET ,IRT ,IRT1 ,IRT2 ,IS ,NBMODE 
      INTEGER NBOPT ,NBSUP ,NBTROU ,NS 
      REAL*8 R8B ,RB 
C-----------------------------------------------------------------------
      DATA  NOMCMP / 'DX' , 'DY' , 'DZ' /
      DATA  ACCES  / 'ACCE    X       ' , 'ACCE    Y       ',
     &               'ACCE    Z       ' /
C     ------------------------------------------------------------------
C
      CALL GETRES(RESU,CONCEP,NOMCMD)
      IER = 0
      CALL GETVTX('DEPL_MULT_APPUI','NOM_CAS',1,IARG,0,K8B,NS)
C
C     --- VERIFICATION DES CHAMPS DONNES ---
      IF ( MONOAP ) THEN
         IF ( TRONC ) THEN
         DO 10 ID = 1,3
            IF (NDIR(ID).EQ.1) THEN
               CALL RSORAC(PSMO,'NOEUD_CMP',IBID,R8B,ACCES(ID),CBID,R8B,
     &                                              K8B,IORDR,1,NBTROU)
               IF (NBTROU.NE.1) THEN
                  IER = IER + 1
               VALK (1) = PSMO
               VALK (2) = ACCES(ID)
                  CALL U2MESG('E', 'ALGORITH12_12',2,VALK,0,0,0,0.D0)
                  GOTO 10
               ENDIF
               MONPAR = 'ACCE_IMPO'
               CALL RSVPAR(PSMO,IORDR,'TYPE_DEFO',IB,RB,MONPAR,IRET)
               IF (IRET.NE.100) THEN
                  IER = IER + 1
               VALK (1) = PSMO
               VALK (2) = ACCES(ID)
               VALK (3) = MONPAR
                  CALL U2MESG('E', 'ALGORITH12_13',3,VALK,0,0,0,0.D0)
               ENDIF
            ENDIF
 10      CONTINUE
         ENDIF
      ELSE
         DO 12 ID = 1,3
            IF (NDIR(ID).EQ.1) THEN
               DO 14 IS = 1,NSUPP(ID)
                  NOEU = NOMSUP(IS,ID)
                  CMP = NOMCMP(ID)
                  MONACC = NOEU//CMP
                  IF (NS.NE.0) THEN
                  CALL RSORAC(STAT,'NOEUD_CMP',IBID,R8B,MONACC,CBID,
     &                                        R8B,K8B,IORDR,1,NBTROU)
                  IF (NBTROU.NE.1) THEN
                     IER = IER + 1
                     VALK (1) = STAT
                     VALK (2) = MONACC
                     CALL U2MESG('E', 'ALGORITH12_14',2,VALK,0,0,0,0.D0)
                     GOTO 16
                  ENDIF
                  MONPAR = 'DEPL_IMPO'
                  CALL RSVPAR(STAT,IORDR,'TYPE_DEFO',IB,RB,MONPAR,IRET)
                  IF (IRET.NE.100) THEN
                     IER = IER + 1
                     VALK (1) = STAT
                     VALK (2) = MONACC
                     VALK (3) = MONPAR
                     CALL U2MESG('E', 'ALGORITH12_15',3,VALK,0,0,0,0.D0)
                  ENDIF
 16               CONTINUE
                  ENDIF
                  IF ( TRONC ) THEN
                     CALL RSORAC(PSMO,'NOEUD_CMP',IBID,R8B,MONACC,CBID,
     &                                        R8B,K8B,IORDR,1,NBTROU)
                     IF (NBTROU.NE.1) THEN
                        IER = IER + 1
                        VALK (1) = PSMO
                        VALK (2) = MONACC
                        CALL U2MESG('E', 'ALGORITH12_12',2,VALK,
     &                                                     0,0,0,0.D0)
                        GOTO 14
                      ENDIF
                      MONPAR = 'ACCE_DDL_IMPO'
                      CALL RSVPAR(PSMO,IORDR,'TYPE_DEFO',IB,RB,
     &                                 MONPAR,IRET)
                  IF (IRET.NE.100) THEN
                     IER = IER + 1
                VALK (1) = PSMO
               VALK (2) = MONACC
               VALK (3) = MONPAR
                   CALL U2MESG('E', 'ALGORITH12_13',3,VALK,0,0,0,0.D0)
                  ENDIF
                  ENDIF
 14            CONTINUE
            ENDIF
 12      CONTINUE
      ENDIF
C
C     --- VERIFICATION DES OPTIONS DE CALCUL ---
      DO 20 IN = 1,NBOPT
         NOMSY = KNOMSY(IN)
         IF (NOMSY(1:4).EQ.'VITE' .AND. .NOT.MONOAP) THEN
               VALK (1) = NOMSY
            CALL U2MESG('E', 'ALGORITH12_18',1,VALK,0,0,0,0.D0)
            IER = IER + 1
         ENDIF
         IF (NOMSY(1:4).EQ.'VITE') GOTO 20
         IF (NOMSY(1:4).EQ.'ACCE') GOTO 20
         CALL RSUTNC(MECA,NOMSY,0,K8B,IBID,NBTROU)
         IF (NBTROU.EQ.0) THEN
            IER = IER + 1
               VALK (1) = MECA
               VALK (2) = NOMSY
            CALL U2MESG('E', 'ALGORITH12_7',2,VALK,0,0,0,0.D0)
            GOTO 20
         ENDIF
         DO 22 IM = 1,NBMODE
            CALL RSEXCH(MECA,NOMSY,NORDR(IM),CHEXT2,IRET)
            IF (IRET.NE.0) THEN
               INUM = NORDR(IM)
               IER = IER + 1
               VALK (1) = MECA
               VALK (2) = NOMSY
               VALI = INUM
               CALL U2MESG('E', 'ALGORITH12_20',2,VALK,1,VALI,0,0.D0)
            ENDIF
 22      CONTINUE
         IF ( TRONC ) THEN
            CALL RSUTNC(PSMO,NOMSY,0,K8B,IBID,NBTROU)
            IF (NBTROU.EQ.0) THEN
               IER = IER + 1
               VALK (1) = PSMO
               VALK (2) = NOMSY
               CALL U2MESG('E', 'ALGORITH12_21',2,VALK,0,0,0,0.D0)
            ENDIF
         ENDIF
         IF (( .NOT.MONOAP ).AND.(NS.NE.0)) THEN
            CALL RSUTNC(STAT,NOMSY,0,K8B,IBID,NBTROU)
            IF (NBTROU.EQ.0) THEN
               IER = IER + 1
               VALK (1) = STAT
               VALK (2) = NOMSY
               CALL U2MESG('E', 'ALGORITH12_22',2,VALK,0,0,0,0.D0)
            ENDIF
         ENDIF
 20   CONTINUE
C
C     --- ON VERIFIE QUE LES CHAM_NOS ET CHAM_ELEMS SONT IDENTIQUES ---
      DO 30 IN = 1,NBOPT
         NOMSY = KNOMSY(IN)
         IF (NOMSY(1:4).EQ.'VITE') GOTO 30
         IF (NOMSY(1:4).EQ.'ACCE') GOTO 30
C
C        --- ON RECUPERE LE PREMIER CHAMP ---
         CALL RSEXCH(MECA,NOMSY,NORDR(1),CHEXTR,IRET)
         CALL DISMOI('F','TYPE_CHAMP',CHEXTR,'CHAMP',IBID,CTYP,IRET)
C
C        --- ON VERIFIE QUE LES SUIVANTS SONT IDENTIQUES ---
         DO 32 IM = 2,NBMODE
            CALL RSEXCH(MECA,NOMSY,NORDR(IM),CHEXT2,IRET)
            IF (CTYP(1:2).EQ.'NO') THEN
               CALL VRREFE(CHEXTR,CHEXT2,IRT)
            ELSEIF (CTYP(1:2).EQ.'EL') THEN
               CALL VRDESC(CHEXTR,CHEXT2,IRT1)
               CALL VRNOLI(CHEXTR,CHEXT2,IRT2)
               IRT = IRT1 + IRT2
            ENDIF
            IF (IRT.NE.0) THEN
               IER = IER + 1
                VALK(1) = CHEXTR
                VALK(2) = CHEXT2
                CALL U2MESK('E','ALGORITH_35', 2 ,VALK)
            ENDIF
 32      CONTINUE
         IF ( MONOAP ) THEN
            IF ( TRONC ) THEN
               DO 34 ID = 1,3
                  IF (NDIR(ID).EQ.1) THEN
                     CALL RSORAC(PSMO,'NOEUD_CMP',IBID,R8B,ACCES(ID),
     &                                     CBID,R8B,K8B,IORDR,1,NBTROU)
                     IF (NBTROU.EQ.1) THEN
                        CALL RSEXCH(PSMO,NOMSY,IORDR,CHEXT2,IRET)
                        IF (CTYP(1:2).EQ.'NO') THEN
                           CALL VRREFE(CHEXTR,CHEXT2,IRT)
                        ELSEIF (CTYP(1:2).EQ.'EL') THEN
                           CALL VRDESC(CHEXTR,CHEXT2,IRT1)
                           CALL VRNOLI(CHEXTR,CHEXT2,IRT2)
                           IRT = IRT1 + IRT2
                        ENDIF
                        IF (IRT.NE.0) THEN
                           IER = IER + 1
                       VALK(1) = CHEXTR
                       VALK(2) = CHEXT2
                       CALL U2MESK('E','ALGORITH_35', 2 ,VALK)
                        ENDIF
                     ENDIF
                  ENDIF
 34            CONTINUE
            ENDIF
         ELSE
            
            DO 36 ID = 1,3
               IF (NDIR(ID).EQ.1) THEN
                  DO 38 IS = 1,NSUPP(ID)
                     NOEU = NOMSUP(IS,ID)
                     CMP = NOMCMP(ID)
                     MONACC = NOEU//CMP
                     IF (NS.NE.0) THEN
                     CALL RSORAC(STAT,'NOEUD_CMP',IBID,R8B,MONACC,CBID,
     &                                          R8B,K8B,IORDR,1,NBTROU)
                     IF (NBTROU.EQ.1) THEN
                        CALL RSEXCH(STAT,NOMSY,IORDR,CHEXT2,IRET)
                        IF (CTYP(1:2).EQ.'NO') THEN
                           CALL VRREFE(CHEXTR,CHEXT2,IRT)
                        ELSEIF (CTYP(1:2).EQ.'EL') THEN
                           CALL VRDESC(CHEXTR,CHEXT2,IRT1)
                           CALL VRNOLI(CHEXTR,CHEXT2,IRT2)
                           IRT = IRT1 + IRT2
                        ENDIF
                        IF (IRT.NE.0) THEN
                           IER = IER + 1
                       VALK(1) = CHEXTR
                       VALK(2) = CHEXT2
                       CALL U2MESK('E','ALGORITH_35', 2 ,VALK)
                        ENDIF
                     ENDIF
                     ENDIF
                     IF ( TRONC ) THEN
                        CALL RSORAC(PSMO,'NOEUD_CMP',IBID,R8B,MONACC,
     &                              CBID,R8B,K8B,IORDR,1,NBTROU)
                        IF (NBTROU.EQ.1) THEN
                           CALL RSEXCH(PSMO,NOMSY,IORDR,CHEXT2,IRET)
                           IF (CTYP(1:2).EQ.'NO') THEN
                              CALL VRREFE(CHEXTR,CHEXT2,IRT)
                           ELSEIF (CTYP(1:2).EQ.'EL') THEN
                              CALL VRDESC(CHEXTR,CHEXT2,IRT1)
                              CALL VRNOLI(CHEXTR,CHEXT2,IRT2)
                              IRT = IRT1 + IRT2
                           ENDIF
                           IF (IRT.NE.0) THEN
                              IER = IER + 1
                       VALK(1) = CHEXTR
                       VALK(2) = CHEXT2
                       CALL U2MESK('E','ALGORITH_35', 2 ,VALK)
                           ENDIF
                        ENDIF
                     ENDIF
 38               CONTINUE
               ENDIF
 36         CONTINUE
         ENDIF
 30   CONTINUE
C
      IF (IER.NE.0) CALL U2MESS('F','ALGORITH_25')
C
      END
