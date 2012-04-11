      SUBROUTINE AVETPR( NBORDR, VWORK, TDISP, KWORK, SOMMW, TSPAQ, I,
     &                   VETPR, VSITN )

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 10/04/2012   AUTEUR TRAN V-X.TRAN 
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
      IMPLICIT   NONE
      INTEGER    NBORDR, TDISP, KWORK, SOMMW, TSPAQ, I
      REAL*8     VWORK(TDISP), VETPR(NBORDR), VSITN(NBORDR)
C ----------------------------------------------------------------------
C BUT: CALCULER LA CONTRAINTE PRINCIPALE ET DEFORMATION NOMRMALE ASSOCIE
C ----------------------------------------------------------------------
C ARGUMENTS :
C  NBORDR : IN   I  : NOMBRE DE NUMEROS D'ORDRE.
C  VWORK  : IN   R  : VECTEUR DE TRAVAIL CONTENANT
C                     L'HISTORIQUE DES TENSEURS DES CONTRAINTES
C                     ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
C                     DU <<PAQUET>> DE MAILLES.
C  TDISP  : IN   I  : TAILLE DU VECTEUR DE TRAVAIL.
C  KWORK  : IN   I  : KWORK = 0 ON TRAITE LA 1ERE MAILLE DU PAQUET
C                               MAILLES OU LE 1ER NOEUD DU PAQUET DE
C                               NOEUDS;
C                     KWORK = 1 ON TRAITE LA IEME (I>1) MAILLE DU PAQUET
C                               MAILLES OU LE IEME NOEUD DU PAQUET
C                               DE NOEUDS.
C  SOMMW  : IN   I  : SOMME DES POINTS DE GAUSS OU DES NOEUDS DES N
C                     MAILLES PRECEDANT LA MAILLE COURANTE.
C  TSPAQ  : IN   I  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
C                     OU DE NOEUDS COURANT.
C  I      : IN   I  : IEME POINT DE GAUSS OU IEME NOEUD.
C  VPHYDR : OUT  R  : VECTEUR CONTENANT LA PRESSION HYDROSTATIQUE A
C                     TOUS LES INSTANTS.
C ----------------------------------------------------------------------
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
C     ------------------------------------------------------------------
      INTEGER    IORDR, ADRS, NVP, NPERM, NITJAC, J, IORDRE, ITYPE
      REAL*8     SIG(6), EPS(6), TOL, TOLDYN,AR(6), BR(6) 
      REAL*8     VECPRO(3,3), VALPRO(3), NM1X, NM1Y, NM1Z,EXM, EYM, EZM
      REAL*8     JACAUX(3)
C     ------------------------------------------------------------------
C
C234567                                                              012
C
      CALL JEMARQ()
C
      DO 10 IORDR=1, NBORDR
         ADRS = (IORDR-1)*TSPAQ + KWORK*SOMMW*12 + (I-1)*12

         DO 35 J = 1, 6 
            SIG(J) = VWORK(ADRS + J )        
            EPS(J) =  VWORK(ADRS + J + 6)
35       CONTINUE 

         NVP = 3
         NPERM = 12
         TOL = 1.D-10
         TOLDYN = 1.D-2
         ITYPE = 0
         IORDRE = 1
         AR(1) = EPS(1)
         AR(2) = EPS(4)
         AR(3) = EPS(5)
         AR(4) = EPS(2)
         AR(5) = EPS(6)
         AR(6) = EPS(3)
         BR(1) = 1.D0
         BR(2) = 0.D0
         BR(3) = 0.D0
         BR(4) = 1.D0
         BR(5) = 0.D0
         BR(6) = 1.D0      
         
         CALL JACOBI(NVP,NPERM,TOL,TOLDYN,AR,BR,VECPRO,VALPRO,
     &                     JACAUX,NITJAC,ITYPE,IORDRE)

         VETPR(IORDR) = VALPRO(1)
         NM1X = VECPRO (1,1)
         NM1Y = VECPRO (2,1)
         NM1Z = VECPRO (3,1)
C CALCvect_F = [SIG].vect_n

         EXM = SIG(1)*NM1X + SIG(4)*NM1Y + SIG(5)*NM1Z
         EYM = SIG(4)*NM1X + SIG(2)*NM1Y + SIG(6)*NM1Z
         EZM = SIG(5)*NM1X + SIG(6)*NM1Y + SIG(3)*NM1Z

C CALCNORM = vect_F.vect_n

         VSITN(IORDR) = ABS(EXM*NM1X + EYM*NM1Y + EZM*NM1Z)
C
 10   CONTINUE
C
      CALL JEDEMA()
C
      END
