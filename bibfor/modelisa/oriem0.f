      SUBROUTINE ORIEM0 ( NOMAZ, NUMA1, NUMA2, PREC, IER )
      IMPLICIT   NONE
      INTEGER                    NUMA1, NUMA2, IER
      REAL*8              PREC
      CHARACTER*(*)       NOMAZ
C.======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 24/10/2005   AUTEUR CIBHHLV L.VIVAN 
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
C ======================================================================
C     ORIEM0  --  ROUTINE APPELEE PAR ORIEMA
C     SI LES MAILLES ONT LES MEMES SUPPORTS (DUPLICATION DES MAILLES)
C     COORDONNEES DES NOEUDS IDENTIQUES : IER = 0
C                                 SINON : IER = 1
C
C IN  : NOMAZ  : NOM DU MAILLAGE
C IN  : NUMA1  : NUMERO DE LA MAILLE 1
C IN  : NUMA2  : NUMERO DE LA MAILLE 2
C IN  : PREC   : PRECISION
C OUT : IER    : = 0  MAILLE IDENTIQUE
C                = 1  MAILLE DIFFERENTE
C
C.========================= DEBUT DES DECLARATIONS ====================
C ----- COMMUNS NORMALISES  JEVEUX
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR,DDOT
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
      CHARACTER*32     JEXNUM, JEXNOM, JEXATR
C
      INTEGER      JDES1, NBNO1, JDES2, NBNO2, JCOOR, INO, N1, N2, IC
      REAL*8       X1, X2
      CHARACTER*1  K1BID
      CHARACTER*8  NOMA
C
C ========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     ---------------
      NOMA = NOMAZ
      IER  = 1
C
C --- COORDONNEES DES CONNECTIVITES :
C     -----------------------------
      CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMA1),'L',JDES1)
      CALL JELIRA(JEXNUM(NOMA//'.CONNEX',NUMA1),'LONMAX',NBNO1,K1BID)
C
      CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMA2),'L',JDES2)
      CALL JELIRA(JEXNUM(NOMA//'.CONNEX',NUMA2),'LONMAX',NBNO2,K1BID)
C
      IF ( NBNO1.NE.NBNO2 ) GOTO 9999
C
C --- RECUPERATION DU TABLEAU DES COORDONNEES :
C     ---------------------------------------
      CALL JEVEUO (NOMA//'.COORDO    .VALE','L',JCOOR)
C
C --- VERIFICATION DES COORDONNEES :
C     ----------------------------
      DO 10 INO = 1 , NBNO1
         N1 = ZI(JDES1+INO-1)
         N2 = ZI(JDES2+INO-1)
         DO 12 IC = 1 , 3
            X1 = ZR(JCOOR+3*(N1-1)+IC-1)
            X2 = ZR(JCOOR+3*(N2-1)+IC-1)
            IF ( X2 .EQ. 0.D0 ) THEN
               IF ( ABS(X1) .GT. PREC ) GOTO 9999
            ELSE
               IF ( ABS((X1-X2)/X2) .GT. PREC ) GOTO 9999
            ENDIF
 12      CONTINUE
 10   CONTINUE
C
      IER = 0
C
 9999 CONTINUE
C
      END
