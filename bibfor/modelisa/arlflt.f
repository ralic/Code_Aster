      SUBROUTINE ARLFLT(NOM0,BC,NOM1,N1)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C        METHODE ARLEQUIN : FILTRAGE DES MAILLES SITUEES DANS 
C                LA BOITE DE LA ZONE DE RECOUVREMENT
C ----------------------------------------------------------------------
C VARIABLE D'ENTREE
C CHARACTER*10  NOM0       : NOM SD DOMAINE A FILTRER
C REAL*8        BC(2,DIM)  : BOITE ENGLOBANT LA ZONE DE RECOUVREMENT
C
C VARIABLE DE SORTIE
C CHARACTER*10  NOM1       : NOM SD DOMAINE FILTRE
C INTEGER       N1         : NOUVEAU NOMBRE DE MAILLES DE NOMC
C
C SD D'ENTREE
C NOM0.BOITE               : BOITES ENGLOBANTES (CF. BOITE)
C
C SD DE SORTIE
C NOM3.GROUPEMA            : LISTE DES MAILLES DE NOMC (MA1,MA2,...)  
C NOM3.BOITE               : BOITES ENGLOBANTES (CF. BOITE)
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
C --- VARIABLES
      CHARACTER*10 NOM0,NOM1
      INTEGER      N1,DIME,NMA,DD,DP,LCPAN,LCSOM,P0,P1,Q,I,J,K,N
      INTEGER      A0,A1,A2,A3,A4,A5,A6,B0,B1,B2,B3,B4,B5,B6
      REAL*8       R,BC(2,*),MN(3),MX(3)

      CALL JEMARQ()

C --- LECTURE DES DONNEES

      CALL JEVEUO(NOM0//'.GROUPEMA','L',A0)
      CALL JEVEUO(NOM0//'.BOITE.DIME','L',A1)
      CALL JEVEUO(NOM0//'.BOITE.MINMAX','L',A2)
      CALL JEVEUO(NOM0//'.BOITE.PAN','L',A3)
      CALL JEVEUO(NOM0//'.BOITE.SOMMET','L',A4)
      CALL JEVEUO(NOM0//'.BOITE.MMGLOB','L',A5)
      CALL JEVEUO(NOM0//'.BOITE.H','L',A6)

      DIME = ZI(A1)
      NMA  = ZI(A1+1)
      DD   = 2*DIME
      DP   = DIME+2 

C --- FILTRAGE DES MAILLES UTILES

      CALL WKVECT('&&ARLFLT.FILTRE','V V L',NMA,Q)
      
      DO 10 I = 1, NMA
        ZL(Q-1+I) = .FALSE.
 10   CONTINUE

      N1 = 0
      P0 = A2 - DD

      DO 20 I = 1, NMA

        P0 = P0 + DD

        P1 = P0
        DO 30 J = 1, DIME
          IF ((ZR(P1).GT.BC(2,J)).OR.(ZR(P1+1).LT.BC(1,J))) GOTO 20
          P1 = P1 + 2
 30     CONTINUE

        N1 = N1 + 1
        ZL(Q-1+I) = .TRUE.

 20   CONTINUE

C --- COMPTE NOUVEAUX NOMBRES DE PANS ET DE SOMMETS

      LCPAN = 0
      LCSOM = 0

      P0 = A1
      DO 40 I = 1, NMA

        P0 = P0 + 2
        IF (.NOT.ZL(Q-1+I)) GOTO 40

        LCPAN = LCPAN + ZI(P0+2) - ZI(P0)
        LCSOM = LCSOM + ZI(P0+3) - ZI(P0+1)

 40   CONTINUE

C --- ALLOCATION NOUVELLE SD GROUPEMA ET BOITE

      CALL WKVECT(NOM1//'.GROUPEMA','V V I',N1,B0)
      CALL WKVECT(NOM1//'.BOITE.DIME','V V I',4+2*N1,B1)
      CALL WKVECT(NOM1//'.BOITE.MINMAX','V V R',DD*N1,B2)
      CALL WKVECT(NOM1//'.BOITE.PAN','V V R',LCPAN*DP,B3)
      CALL WKVECT(NOM1//'.BOITE.SOMMET','V V R',LCSOM*DIME,B4)
      CALL WKVECT(NOM1//'.BOITE.MMGLOB','V V R',DD,B5)
      CALL WKVECT(NOM1//'.BOITE.H','V V R',N1,B6)

C --- RECOPIE

      LCPAN = 1
      LCSOM = 1
      ZI(B1) = DIME
      ZI(B1+1) = N1

      DO 50 I = 1, DIME
        MX(I) = BC(1,I)
        MN(I) = BC(2,I)
 50   CONTINUE

      P0 = A2 - DD
      DO 60 I = 1, NMA

        A1 = A1 + 2
        P0 = P0 + DD
        IF (.NOT.ZL(Q-1+I)) GOTO 60

C ----- DIME

        B1 = B1 + 2
        ZI(B1) = LCPAN
        ZI(B1+1) = LCSOM

C ----- MINMAX

        P1 = P0
        DO 70 J = 1, DIME

          R = ZR(P1)
          ZR(B2) = R
          IF (R.LT.MN(J)) MN(J) = R

          R = ZR(P1+1)
          ZR(B2+1) = R
          IF (R.GT.MX(J)) MX(J) = R

          P1 = P1 + 2
          B2 = B2 + 2

 70     CONTINUE

C ----- PAN

        P1 = A3 + DP*(ZI(A1)-1)
        N = ZI(A1+2) - ZI(A1)

        DO 80 J = 1, N

          LCPAN = LCPAN + 1

          DO 80 K = 1, DP

            ZR(B3) = ZR(P1)
            B3 = B3 + 1
            P1 = P1 + 1

 80     CONTINUE

C ----- SOMMET

        P1 = A4 + DIME*(ZI(A1+1)-1)
        N = ZI(A1+3) - ZI(A1+1) 

        DO 90 J = 1, N

          LCSOM = LCSOM + 1

          DO 90 K = 1, DIME

            ZR(B4) = ZR(P1)
            P1 = P1 + 1
            B4 = B4 + 1

 90     CONTINUE
              
C ----- GROUPEMA ET H
  
        ZI(B0) = ZI(A0-1+I)
        ZR(B6) = ZR(A6-1+I)
        B0 = B0 + 1
        B6 = B6 + 1

 60   CONTINUE

      ZI(B1+2) = LCPAN
      ZI(B1+3) = LCSOM

C --- MMGLOB

      DO 100 I = 1, DIME
        ZR(B5) = MN(I)
        ZR(B5+1) = MX(I)
        B5 = B5 + 2
 100  CONTINUE

C --- DESALLOCATION

      CALL JEDETR('&&ARLFLT.FILTRE')
      CALL JEDETR(NOM0//'.GROUPEMA')
      CALL JEDETR(NOM0//'.BOITE.DIME')
      CALL JEDETR(NOM0//'.BOITE.MINMAX')
      CALL JEDETR(NOM0//'.BOITE.PAN')
      CALL JEDETR(NOM0//'.BOITE.SOMMET')
      CALL JEDETR(NOM0//'.BOITE.MMGLOB')
      CALL JEDETR(NOM0//'.BOITE.H')
      
      CALL JEDEMA()

      END
