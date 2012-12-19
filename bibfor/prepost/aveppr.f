      SUBROUTINE AVEPPR( NBORDR, VWORK, TDISP, KWORK, SOMMW, TSPAQ, I,
     &                   VEPPR, VSIPN )

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
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
      INCLUDE 'jeveux.h'

      INTEGER    NBORDR, TDISP, KWORK, SOMMW, TSPAQ, I
      REAL*8     VWORK(TDISP), VEPPR(NBORDR), VSIPN(NBORDR)
C ----------------------------------------------------------------------
C BUT: CALCULER LA DEF PLAS PRINCIPALE ET CONT NOMRMALE ASSOCIE
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
      INTEGER    IORDR, ADRS, NVP, NPERM, NITJAC, J, IORDRE, ITYPE
      INTEGER    DECAL
      REAL*8     SIG(6), TOL, TOLDYN,AR(6), BR(6)
      REAL*8     VECPRO(3,3), VALPRO(3), NM1X, NM1Y, NM1Z,SXM, SYM, SZM
      REAL*8     JACAUX(3), EPSP(6)
C     ------------------------------------------------------------------
C
C234567                                                              012
C
      CALL JEMARQ()
C
C ---------------------------------------------------------------
C RECUPER LES CONTRAINTES ET DEFORMATION

      DECAL = 18
      DO 10 IORDR=1, NBORDR
         ADRS = (IORDR-1)*TSPAQ + KWORK*SOMMW*DECAL + (I-1)*DECAL

         DO 35 J = 1, 6
            SIG(J) = VWORK(ADRS + J )
            EPSP(J) =  VWORK(ADRS + J + 6 + 6)
35       CONTINUE

         NVP = 3
         NPERM = 12
         TOL = 1.D-10
         TOLDYN = 1.D-2
         ITYPE = 0
         IORDRE = 1
         AR(1) =  EPSP(1)
         AR(2) =  EPSP(4)
         AR(3) =  EPSP(5)
         AR(4) =  EPSP(2)
         AR(5) =  EPSP(6)
         AR(6) =  EPSP(3)
         BR(1) = 1.D0
         BR(2) = 0.D0
         BR(3) = 0.D0
         BR(4) = 1.D0
         BR(5) = 0.D0
         BR(6) = 1.D0

         CALL JACOBI(NVP,NPERM,TOL,TOLDYN,AR,BR,VECPRO,VALPRO,
     &                     JACAUX,NITJAC,ITYPE,IORDRE)

         VEPPR(IORDR) = VALPRO(1)

         NM1X = VECPRO (1,1)
         NM1Y = VECPRO (2,1)
         NM1Z = VECPRO (3,1)
C CALCvect_F = [SIG].vect_n

         SXM = SIG(1)*NM1X + SIG(4)*NM1Y + SIG(5)*NM1Z
         SYM = SIG(4)*NM1X + SIG(2)*NM1Y + SIG(6)*NM1Z
         SZM = SIG(5)*NM1X + SIG(6)*NM1Y + SIG(3)*NM1Z

C CALCNORM = vect_F.vect_n

         VSIPN(IORDR) = ABS(SXM*NM1X + SYM*NM1Y + SZM*NM1Z)
C
 10   CONTINUE
C
      CALL JEDEMA()
C
      END
