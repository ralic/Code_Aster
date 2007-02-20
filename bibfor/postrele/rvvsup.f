      SUBROUTINE RVVSUP ( )
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C     ------------------------------------------------------------------
C     VERIFICATION SUPPLEMENTAIRE OP0051
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      N1, N2, N3, N4, IOCC, NBPOST
      INTEGER VALI(3)
      REAL*8       R8B
      CHARACTER*8  K8B, RESU, NOMRES
      CHARACTER*24 VALK(4)
      CHARACTER*16 NOMCMD, CONCEP, TYPRES
C
C=======================================================================
C
      CALL JEMARQ()
      CALL GETRES(RESU,CONCEP,NOMCMD)
C
C     --- VERIFICATION SUR "OPERATION" ---
C
      CALL GETFAC ( 'ACTION', NBPOST )
C
      DO 10, IOCC = 1, NBPOST, 1
C
C     /* QUANTITE (IE : SOMME) */
         CALL GETVTX('ACTION','RESULTANTE',IOCC,1,0,K8B,N1)
         N1 = -N1
         IF ( N1 .GT. 0 ) THEN
            CALL GETVTX('ACTION','RESULTANTE',IOCC,1,0,K8B,N1)
            CALL GETVTX('ACTION','MOMENT    ',IOCC,1,0,K8B,N2)
            N1 = -N1
            N2 = -N2
            IF ( N2 .NE. 0 ) THEN
               IF (((N1.NE.2).AND.(N1.NE.3)) .OR. (N1.NE.N2)) THEN
                  VALI (1) = IOCC
                  VALI (2) = 2
                  VALI (3) = 3
                  VALK (1) = ' '
                CALL U2MESG('E', 'POSTRELE1_75',1,VALK,3,VALI,0,0.D0)
               ENDIF
               CALL GETVR8('ACTION','POINT',IOCC,1,0,R8B,N1)
               N1 = -N1
               IF ( (N1.NE.2) .AND. (N1.NE.3) ) THEN
                  VALI (1) = IOCC
                  VALI (2) = 2
                  VALI (3) = 3
                CALL U2MESG('E', 'POSTRELE1_76',0,' ',3,VALI,0,0.D0)
               ENDIF
            ENDIF
         ENDIF
C
C     /* COHERENCE ACCES DANS RESULTAT */
         CALL GETVID('ACTION','RESULTAT',IOCC,1,0,NOMRES,N1)
         N1 = -N1
         IF ( N1 .GT. 0 ) THEN
            CALL GETVID('ACTION','RESULTAT',IOCC,1,1,NOMRES,N1)
            CALL GETTCO(NOMRES,TYPRES)
            CALL GETVID('ACTION','LIST_FREQ',IOCC,1,0,K8B,N1)
            CALL GETVR8('ACTION','FREQ'     ,IOCC,1,0,ZR, N2)
            N1 = MAX(-N1,-N2)
            CALL GETVID('ACTION','LIST_INST',IOCC,1,0,K8B,N2)
            CALL GETVR8('ACTION','INST'     ,IOCC,1,0,ZR, N3)
            N2 = MAX(-N3,-N2)
            CALL GETVID('ACTION','LIST_MODE',IOCC,1,0,K8B,N3)
            CALL GETVIS('ACTION','NUME_MODE',IOCC,1,0,ZI, N4)
            N3 = MAX(-N3,-N4)
            N4 = MAX(N1,N2,N3)
            IF ( N4 .GT. 0 ) THEN
               IF ( ((N1 .NE. 0).OR.(N3 .NE. 0)) .AND.
     +             ((TYPRES(1:4) .EQ. 'EVOL') .OR.
     +              (TYPRES(6:10) .EQ. 'TRANS') .OR.
     +              (TYPRES(11:15) .EQ. 'TRANS') ) ) THEN
                  VALI (1) = IOCC
                  VALK (1) = NOMRES
                  VALK (2) = TYPRES
                  VALK (3) = 'FREQ'
                  VALK (4) = 'MODE'
                  CALL U2MESG('E', 'POSTRELE1_77',4,VALK,1,VALI,0,0.D0)
               ENDIF
               IF ( (N2 .NE. 0) .AND.
     +             ((TYPRES(1:4)   .EQ. 'MODE' ) .OR.
     +              (TYPRES(1:4)   .EQ. 'BASE' ) .OR.
     +              (TYPRES(6:10)  .EQ. 'HARMO') .OR.
     +              (TYPRES(11:15) .EQ. 'HARMO') ) ) THEN
                  VALI (1) = IOCC
                  VALK (1) = NOMRES
                  VALK (2) = TYPRES
                  VALK (3) = 'INSTANT'
                  CALL U2MESG('E', 'POSTRELE1_78',3,VALK,1,VALI,0,0.D0)
               ENDIF
            ENDIF
         ENDIF
C
 10   CONTINUE
C
      CALL JEDEMA()
      END
