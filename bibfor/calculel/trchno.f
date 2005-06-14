      SUBROUTINE TRCHNO ( IFIC, NOCC )
      IMPLICIT   NONE
      INTEGER    IFIC, NOCC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 13/06/2005   AUTEUR CIBHHLV L.VIVAN 
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
C ----------------------------------------------------------------------
C     COMMANDE:  TEST_RESU
C                MOT CLE FACTEUR "CHAM_NO"
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------

      INTEGER      VALI, REFI, IOCC, IBID, IRET, NBCMP,JCMP,N1,N2,N3,N4
      REAL*8       VALR, REFR, EPSI, PREC
      COMPLEX*16   VALC, REFC
      CHARACTER*1  TYPRES
      CHARACTER*3  SSIGNE
      CHARACTER*4  TYPCH, TESTOK
      CHARACTER*8  CRIT, NODDL, NOGRNO, NOMMA, NOCMP, TYPTES
      CHARACTER*11 MOTCLE
      CHARACTER*19 CHAM19
      CHARACTER*17 NONOEU, LABEL
C     ------------------------------------------------------------------
      CALL JEMARQ()

      MOTCLE = 'CHAM_NO:'

      DO 100 IOCC = 1,NOCC
        TESTOK = 'NOOK'
        NONOEU = ' '
        NODDL = ' '
        CALL GETVID('CHAM_NO','CHAM_GD',IOCC,1,1,CHAM19,N1)
        WRITE (IFIC,*) '---- ',MOTCLE,CHAM19(1:8)
        CALL UTEST3(IFIC,'CHAM_NO',IOCC)

        CALL GETVTX ( 'CHAM_NO', 'NOM_CMP',   IOCC,1,1, NODDL, N1 )
        CALL GETVTX ( 'CHAM_NO', 'VALE_ABS',  IOCC,1,1, SSIGNE,N1 )
        CALL GETVR8 ( 'CHAM_NO', 'PRECISION', IOCC,1,1, EPSI,  N1 )
        CALL GETVTX ( 'CHAM_NO', 'CRITERE',   IOCC,1,1, CRIT,  N1 )

        CALL GETVR8 ( 'CHAM_NO', 'VALE',   IOCC,1,1, REFR, N1 )
        CALL GETVIS ( 'CHAM_NO', 'VALE_I', IOCC,1,1, REFI, N2 )
        CALL GETVC8 ( 'CHAM_NO', 'VALE_C', IOCC,1,1, REFC, N3 )
        TYPRES = 'R'
        IF (N2.NE.0) TYPRES = 'I'
        IF (N3.NE.0) TYPRES = 'C'

        CALL GETVTX('CHAM_NO','TYPE_TEST',IOCC,1,1,TYPTES,N1)
        IF (N1.NE.0) THEN
          CALL GETVTX('CHAM_NO','NOM_CMP',IOCC,1,0,NODDL,N4)
          IF (N4.EQ.0) THEN
            CALL UTEST1(CHAM19,TYPTES,TYPRES,REFI,REFR,REFC,EPSI,CRIT,
     &                  IFIC,SSIGNE)
          ELSE
            NBCMP = -N4
            CALL WKVECT('&&TRCHNO.NOM_CMP','V V K8',NBCMP,JCMP)
            CALL GETVTX('CHAM_NO','NOM_CMP',IOCC,1,NBCMP,ZK8(JCMP),N4)
            CALL UTEST4(CHAM19,TYPTES,TYPRES,REFI,REFR,REFC,EPSI,CRIT,
     &                  IFIC,NBCMP,ZK8(JCMP),SSIGNE)
            CALL JEDETR('&&TRCHNO.NOM_CMP')
          END IF

        ELSE

          CALL GETVTX('CHAM_NO','NOM_CMP',IOCC,1,1,NODDL,N1)
          CALL DISMOI('F','NOM_MAILLA',CHAM19,'CHAMP',IBID,NOMMA,IRET)
          CALL GETVEM(NOMMA,'NOEUD','CHAM_NO','NOEUD',IOCC,1,1,
     &                NONOEU(1:8),N1)
          CALL GETVEM(NOMMA,'GROUP_NO','CHAM_NO','GROUP_NO',IOCC,1,1,
     &                NOGRNO,N2)
          IF (N1.EQ.1) THEN
C            RIEN A FAIRE.
          ELSE
            CALL UTNONO('E',NOMMA,'NOEUD',NOGRNO,NONOEU(1:8),IRET)
            IF (IRET.NE.0) THEN
              WRITE (IFIC,*) TESTOK
              GO TO 100
            END IF
            NONOEU(10:17) = NOGRNO
          END IF
          CALL UTESTR(CHAM19,NONOEU,NODDL,REFI,REFR,REFC,TYPRES,EPSI,
     &                CRIT,IFIC,SSIGNE)
        END IF
 100  CONTINUE

      CALL JEDEMA()
      END
