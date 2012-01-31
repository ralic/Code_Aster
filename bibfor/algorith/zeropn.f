      SUBROUTINE ZEROPN( KSTOP, DEGRE, AI, RACINE, IER )
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 31/01/2012   AUTEUR REZETTE C.REZETTE 
C TOLE CRS_1404
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C =====================================================================
      IMPLICIT NONE
      INTEGER     DEGRE
      REAL*8      AI(DEGRE),RACINE(2*DEGRE)
      CHARACTER*1 KSTOP
C =====================================================================
C --- RECHERCHE DES RACINES D'UN POLYNOME PAR LA METHODE --------------
C --- COMPANION MATRIX POLYNOMIAL -------------------------------------
C --- LE POLYNOME ETANT DE LA FORME : ---------------------------------
C --- P(X) = X^N+A_(N-1).X^(N-1)+...A_K.X^K+...+A_1.X+A_0 -------------
C
C KSTOP : COMPORTEMENT EN CAS DE PROBLEME :
C        / 'A' : ON EMET UNE ALARME
C        / 'F' : ON EMET UNE ERREUR FATALE
C        / ' ' : ON N'EMET PAS DE MESSAGE
C DEGRE : DEGRE DU POLYNOME
C AI : COEFFICIENTS DU POLYNOME
C RACINE : RACINES DU POLYNOME
C OUT IER : PARAMETRE  D'ERREUR
C           IER = 0 OK
C           IER = J > 0 , NON CONVERGENCE POUR LA J-IEME RACINE
C                LES J PREMIERES RACINES NE SONT PAS CALCULEES
C =====================================================================
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------
C =====================================================================
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
C =====================================================================
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX ----------------
C =====================================================================
      INTEGER      II, IER, ICODE, IBID,DEGMAX
      PARAMETER   (DEGMAX=4)
      REAL*8       BIDON(2*DEGRE), VBID(2*DEGRE)
      REAL*8       COMAPO(DEGMAX*DEGMAX)
C =====================================================================
C --- INITIALISATIONS ET COHERENCES -----------------------------------
C =====================================================================
      IER    = 0
      DO 2 II=1,2*DEGRE
         RACINE(II) = 0.0D0
 2    CONTINUE
C =====================================================================
C --- INITIALISATION DE LA MATRICE ------------------------------------
C =====================================================================
      CALL ASSERT(DEGRE.LE.DEGMAX)
      DO 10 II = 1, DEGMAX*DEGMAX
         COMAPO(II) = 0.D0
 10   CONTINUE
C =====================================================================
C --- CREATION DE LA MATRICE DE TYOPE HESSENBERG ----------------------
C =====================================================================
C --- REMPLISSAGE DE LA PREMIERE SOUS DIAGONALE -----------------------
C =====================================================================
      DO 20 II = 1, DEGRE - 1
         COMAPO(II*(DEGRE+1)) = 1.0D0
 20   CONTINUE
      DO 30 II = 1, DEGRE
         COMAPO(II*DEGRE) = - AI(II)
 30   CONTINUE
      ICODE = 0
      CALL VPHQRP(COMAPO,DEGRE,DEGRE,ICODE,RACINE,BIDON,
     &                                              1,VBID,30,IER,IBID)
C =====================================================================
      IF (KSTOP.EQ.' ') GOTO 40

      IF (IER .NE. 0) THEN
        CALL U2MESI(KSTOP,'ALGORITH17_6',1,IER)
      ENDIF

 40   CONTINUE
C =====================================================================
      END
