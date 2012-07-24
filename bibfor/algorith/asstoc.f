      SUBROUTINE ASSTOC ( MOME, RESU, NOMSY, NEQ, REPDIR, NDIR,
     &                    COMDIR, TYPCDI, GLOB, PRIM )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER           NEQ, NDIR(*)
      REAL*8            REPDIR(NEQ,*)
      LOGICAL           COMDIR, GLOB, PRIM
      CHARACTER*16      NOMSY
      CHARACTER*(*)     MOME, RESU, TYPCDI
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/07/2012   AUTEUR PELLET J.PELLET 
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
C        STOCKAGE DES CHAMPS CALCULES
C     ------------------------------------------------------------------
C IN  : MOME   : MODES MECANIQUES
C IN  : RESU   : NOM UTILISATEUR DE LA COMMANDE
C IN  : NOMSY  : OPTION DE CALCUL
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : REPDIR : VECTEUR DES RECOMBINAISONS
C IN  : NDIR   : VECTEUR DES DIRECTIONS
C IN  : COMDIR : =.TRUE.  , COMBINAISON DES DIRECTIONS
C                =.FALSE. , PAS DE COMBINAISON DES DIRECTIONS
C IN  : TYPCDI : TYPE DE COMBINAISON DES DIRECTIONS
C     ------------------------------------------------------------------
      INTEGER       IBID,I,  ID, IEQ, IER, IN, IORDR, JDEF, JDIR, JVAL,
     &              LVALE, NBMODE, NBTROU
      REAL*8        R8B, R1, R10, R11, R12, R13, R14, R15, R16, R17,
     &              R18, R19, R2, R20, R21, R22, R23, R24, R3, R4, R5,
     &              R6, R7, R8, R9, RX, RY, RZ, XXX
      CHARACTER*8   K8B, COMP(5)
      CHARACTER*16  NOMS2, CONCEP, NOMCMD, DEF
      CHARACTER*19  MONCHA, CHAMP
      CHARACTER*24  VALE
      CHARACTER*24  VALK(3)
      COMPLEX*16    C16B
C     ------------------------------------------------------------------
      DATA  COMP / 'X' , 'Y' , 'Z' , 'QUAD' , 'NEWMARK' /
      DATA  VALE / '                   .VALE' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETRES(K8B,CONCEP,NOMCMD)
      CALL GETFAC('DEPL_MULT_APPUI',NBMODE)

      DO 10 I = 1,3
         IF (NDIR(I).EQ.1) NBMODE = NBMODE + 1
 10   CONTINUE
      IF ( COMDIR ) NBMODE = NBMODE + 1
C
C     --- CREATION DE LA STRUCTURE D'ACCUEIL ---
      CALL RSEXIS(RESU,IER)
      IF (IER.EQ.0) CALL RSCRSD('G',RESU,CONCEP,NBMODE)
      NOMS2 = NOMSY
      IF (NOMSY(1:4).EQ.'VITE') NOMS2 = 'DEPL'
      IF (NOMSY(1:4).EQ.'ACCE') NOMS2 = 'DEPL'
      CALL RSORAC(MOME,'TOUT_ORDRE',IBID,R8B,K8B,C16B,R8B,K8B,
     &                                                  IORDR,1,NBTROU)
      CALL RSEXCH('F',MOME,NOMS2,IORDR,MONCHA,IER)
C
      IORDR = 0

      IF (GLOB) THEN
         DEF = 'GLOBALE'
      ELSE IF (PRIM) THEN
         DEF = 'PRIMAIRE'
      ENDIF
      DO 20 ID = 1,3
       IF (NDIR(ID).EQ.1) THEN
          IORDR = IORDR + 1

C           --- CHAMP RECOMBINE ---
            CALL RSEXCH(' ',RESU,NOMSY,IORDR,CHAMP,IER)
            IF ( IER .EQ. 100 ) THEN
               CALL VTDEFS(CHAMP,MONCHA,'G','R')
            ELSE
               VALK(1) = NOMSY
               VALK(2) = COMP(ID)
               VALK(3) = CHAMP
               CALL U2MESK('F', 'SEISME_26',3,VALK)
            ENDIF
            VALE(1:19) = CHAMP
            CALL JEEXIN(VALE(1:19)//'.VALE',IBID)
            IF (IBID.GT.0) THEN
              VALE(20:24)='.VALE'
            ELSE
              VALE(20:24)='.CELV'
            END IF
            CALL JEVEUO(VALE,'E',JVAL)

            DO 30 IN = 1, NEQ
               ZR(JVAL+IN-1) = SQRT( ABS ( REPDIR(IN,ID) ) )
 30         CONTINUE
            CALL JELIBE(VALE)
            CALL RSNOCH(RESU,NOMSY,IORDR)
C
C           --- PARAMETRE ---
            CALL RSADPA(RESU,'E',1,'NOEUD_CMP' ,IORDR,0,JDIR,K8B)
            ZK16(JDIR) = 'DIR     '//COMP(ID)
            CALL RSADPA(RESU,'E',1,'TYPE_DEFO',IORDR,0,JDEF,K8B)
            ZK16(JDEF) = DEF
         ENDIF
 20   CONTINUE
C
      IF ( COMDIR ) THEN
         IORDR = IORDR + 1
C
C        --- CHAMP RECOMBINE ---
         CALL RSEXCH(' ',RESU,NOMSY,IORDR,CHAMP,IER)
         IF ( IER .EQ. 100 ) THEN
            CALL VTDEFS(CHAMP,MONCHA,'G','R')
         ELSE
            VALK(1) = NOMSY
            VALK(2) = COMP(ID)
            VALK(3) = CHAMP
            CALL U2MESK('F', 'SEISME_26', 3, VALK)
         ENDIF
         VALE(1:19) = CHAMP
         CALL JEEXIN(VALE(1:19)//'.VALE',IBID)
         IF (IBID.GT.0) THEN
           VALE(20:24)='.VALE'
         ELSE
           VALE(20:24)='.CELV'
         END IF
         CALL JEVEUO(VALE,'E',LVALE)

         IF ( TYPCDI(1:4).EQ.'QUAD') THEN
            DO 40 IEQ = 1, NEQ
               XXX = ABS ( REPDIR(IEQ,1) ) + ABS ( REPDIR(IEQ,2) )
     &                                     + ABS ( REPDIR(IEQ,3) )
               ZR(LVALE+IEQ-1) = SQRT( XXX )
 40         CONTINUE
         ELSEIF (TYPCDI(1:4).EQ.'NEWM') THEN
            DO 42 IEQ = 1, NEQ
               RX = SQRT( ABS ( REPDIR(IEQ,1) ) )
               RY = SQRT( ABS ( REPDIR(IEQ,2) ) )
               RZ = SQRT( ABS ( REPDIR(IEQ,3) ) )
               R1  =      RX + 0.4D0*RY + 0.4D0*RZ
               R2  =  0.4D0*RX +     RY + 0.4D0*RZ
               R3  =  0.4D0*RX + 0.4D0*RY +     RZ
               R4  =      RX - 0.4D0*RY + 0.4D0*RZ
               R5  =  0.4D0*RX -     RY + 0.4D0*RZ
               R6  =  0.4D0*RX - 0.4D0*RY +     RZ
               R7  =      RX - 0.4D0*RY - 0.4D0*RZ
               R8  =  0.4D0*RX -     RY - 0.4D0*RZ
               R9  =  0.4D0*RX - 0.4D0*RY -     RZ
               R10 =      RX + 0.4D0*RY - 0.4D0*RZ
               R11 =  0.4D0*RX +     RY - 0.4D0*RZ
               R12 =  0.4D0*RX + 0.4D0*RY -     RZ
               R13 =     -RX + 0.4D0*RY + 0.4D0*RZ
               R14 = -0.4D0*RX +     RY + 0.4D0*RZ
               R15 = -0.4D0*RX + 0.4D0*RY +     RZ
               R16 =     -RX - 0.4D0*RY + 0.4D0*RZ
               R17 = -0.4D0*RX -     RY + 0.4D0*RZ
               R18 = -0.4D0*RX - 0.4D0*RY +     RZ
               R19 =     -RX - 0.4D0*RY - 0.4D0*RZ
               R20 = -0.4D0*RX -     RY - 0.4D0*RZ
               R21 = -0.4D0*RX - 0.4D0*RY -     RZ
               R22 =     -RX + 0.4D0*RY - 0.4D0*RZ
               R23 = -0.4D0*RX +     RY - 0.4D0*RZ
               R24 = -0.4D0*RX + 0.4D0*RY -     RZ
               XXX = MAX(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14)
               XXX = MAX(XXX,R15,R16,R17,R18,R19,R20,R21,R22,R23,R24)
               ZR(LVALE+IEQ-1) = XXX
 42         CONTINUE
         ENDIF
         CALL JELIBE(VALE)
         CALL RSNOCH(RESU,NOMSY,IORDR)
C
C        --- PARAMETRE ---
         CALL RSADPA(RESU,'E',1,'NOEUD_CMP',IORDR,0,JDIR,K8B)
         IF (TYPCDI(1:4).EQ.'QUAD') THEN
            ZK16(JDIR) = 'COMBI   '//COMP(4)
         ELSEIF (TYPCDI(1:4).EQ.'NEWM') THEN
            ZK16(JDIR) = 'COMBI   '//COMP(5)
         ENDIF
         CALL RSADPA(RESU,'E',1,'TYPE_DEFO',IORDR,0,JDEF,K8B)
         ZK16(JDEF) = DEF
      ENDIF
C
      CALL JEDEMA()
      END
