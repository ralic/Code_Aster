      SUBROUTINE RCMO02 ( ETAT, NUMSIT, VALE )
      IMPLICIT   NONE
      INTEGER             NUMSIT
      REAL*8              VALE(*)
      CHARACTER*1         ETAT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C     RECUPERATION DES MOMENTS POUR UN ETAT STABILISE
C
C IN  : ETAT   : ETAT STABILISE "A" OU "B"
C              : OU "S" SI SEISME
C IN  : NUMSIT : NUMERO DE LA SITUATION
C OUT : VALE   : ON SOMME LES CHARGEMENTS
C                VALE(1)  = FX  OU _TUBU
C                VALE(2)  = FY  OU _TUBU
C                VALE(3)  = FZ  OU _TUBU
C                VALE(4)  = MX  OU _TUBU
C                VALE(5)  = MY  OU _TUBU
C                VALE(6)  = MZ  OU _TUBU
C                VALE(7)  = FX_CORP
C                VALE(8)  = FY_CORP
C                VALE(9)  = FZ_CORP
C                VALE(10) = MX_CORP
C                VALE(11) = MY_CORP
C                VALE(12) = MZ_CORP
C     ------------------------------------------------------------------
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
      CHARACTER*32     JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      I, J, NUMCHA, JLCHA, NBCHAR, JCHAR, IRET
      CHARACTER*1         ETATS
      CHARACTER*8  K8B, KNUMES, KNUMEC
C DEB ------------------------------------------------------------------
C
      DO 10 I = 1 , 12
         VALE(I) = 0.D0
 10   CONTINUE
C
      KNUMES = 'S       '
      CALL CODENT ( NUMSIT , 'D0' , KNUMES(2:8)  )
C
C --- LISTE DES CHARGEMENTS POUR LE NUMERO DE SITUATION
C
      IF ((ETAT.EQ.'S').OR.(ETAT.EQ.'A')) THEN
        ETATS = 'A'
      ELSE
        ETATS = 'B'
      ENDIF

      CALL JEEXIN (JEXNOM('&&RC3200.SITU_ETAT_'//ETATS,KNUMES), IRET )
      IF ( IRET .EQ. 0 ) GOTO 9999
C
      CALL JELIRA (JEXNOM('&&RC3200.SITU_ETAT_'//ETATS,KNUMES),
     +                                          'LONUTI', NBCHAR, K8B )
      CALL JEVEUO (JEXNOM('&&RC3200.SITU_ETAT_'//ETATS,KNUMES),
     +                                                     'L', JLCHA )
C
C
      DO 100 I = 1 , NBCHAR
C
         NUMCHA = ZI(JLCHA-1+I)
         KNUMEC = 'C       '
         CALL CODENT ( NUMCHA , 'D0' , KNUMEC(2:8)  )
C
         CALL JEVEUO (JEXNOM('&&RC3200.VALE_CHAR',KNUMEC), 'L', JCHAR )
C
         IF ( ETAT .EQ. 'S' ) THEN
            DO 102 J = 1 , 12
               VALE(J) = VALE(J) + ZR(JCHAR-1+J)**2
 102        CONTINUE
         ELSE
            DO 104 J = 1 , 12
               VALE(J) = VALE(J) + ZR(JCHAR-1+J)
 104        CONTINUE
         ENDIF
C
 100  CONTINUE
C
       IF ( ETAT .EQ. 'S' ) THEN
          DO 106 J = 1 , 12
             VALE(J) = SQRT ( VALE(J) )
 106      CONTINUE
       ENDIF
C
 9999 CONTINUE
C
      END
