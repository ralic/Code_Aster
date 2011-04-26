      SUBROUTINE XRMES2(NDIM,NBNASE,CPT,IN,IVOIS,JSIGSE,NNO,NBCMP,
     &                  JCNSET,DSG11,DSG22,DSG12)
      IMPLICIT NONE
      INTEGER NDIM,NBNASE,CPT,IN,IVOIS,JSIGSE,NNO,NBCMP,JCNSET
      REAL*8 DSG11(NBNASE),DSG22(NBNASE),DSG12(NBNASE)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
C TOLE CRS_1404
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
C =====================================================================
C  XFEM - ERREUR EN MECANIQUE - TERME DE SAUT - DIMENSION 2
C  *       *        **                   *                *
C =====================================================================
C
C     BUT:
C         CALCUL DU SAUT DE CONTRAINTE (AUX NOEUDS) ENTRE LE
C         SOUS-ELEMENT XFEM COURANT ET LE SOUS-ELEMENT VOISIN PAR
C         RAPPORT AU BORD (ARETE) COURANT.
C
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   NDIM   : DIMENSION
C IN   NBNASE : NOMBRE DE NOEUDS PAR ARR�TE DU SOUS-ELEMENT
C IN   CPT    : NUMERO DU SOUS-ELEMENT
C IN   IN     : NUMERO DU NOEUD (I.E. DE L'ARETE)
C IN   IVOIS  : ADRESSE DANS ZI DES VOISINS DU SOUS-ELEMENT
C IN   JSIGSE : ADRESSE DES CONTRAINTES AUX NOEUDS DU SOUS-ELEMENT
C IN   NBCMP  : NOMBRE DE COMPOSANTE DE SIGMA
C IN   JCNSET : ADRESSE DANS ZI DE LA CONNECTIVITE
C
C      SORTIE :
C-------------
C OUT  DSG11  : SAUT DE CONTRAINTE AUX NOEUDS COMPOSANTE 11
C OUT  DSG22  : SAUT DE CONTRAINTE AUX NOEUDS COMPOSANTE 22
C OUT  DSG12  : SAUT DE CONTRAINTE AUX NOEUDS COMPOSANTE 12
C
C ......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      REAL*8  SIG11(NBNASE),SIG22(NBNASE),SIG12(NBNASE)
      REAL*8  SIGV11(NBNASE),SIGV22(NBNASE),SIGV12(NBNASE)
      INTEGER J,K,INSUI,INV,INVSUI,INX,INXV,INSUX
C
C ----------------------------------------------------------------------
C
      IF(IN.EQ.NNO) THEN
        INSUI=1
      ELSE
        INSUI=IN+1
      END IF
C
C --- ON ETABLIT LA CORRESPONDANCE ENTRE LA NUMEROTATION DES NOEUDS
C --- LOCALE � LA MAILLE COURANTE ET LA NUMEROTATION DES NOEUDS LOCALE
C --- � LA MAILLE VOISINE. POUR CELA ON UTILISE LA NUMEROTATION XFEM
C
      INX=ZI(JCNSET-1+(NDIM+1)*(CPT-1)+IN)
      INSUX=ZI(JCNSET-1+(NDIM+1)*(CPT-1)+INSUI)

      DO 100 J=1,NNO
        INXV=ZI(JCNSET-1+(NDIM+1)*(IVOIS-1)+J)
        IF (INXV.EQ.INX) THEN
          INV=J
        END IF
        IF (INXV.EQ.INSUX) THEN
          INVSUI=J
        END IF
 100  CONTINUE
C
C --- LE CAS QUADRATIQUE N'EST PAS PR�VU CAR LES ELEMENTS SOUS DECOUPE
C --- SONT TOUJOURS LINEAIRE !
C
C --- RECUPERATION DES CONTRAINTES AUX NOEUDS DE L'ARR�TE COURANTE
C --- POUR LE SOUS-ELEMENT COURANT
C
      SIG11(1)=ZR(JSIGSE-1+NBCMP*NNO*(CPT-1)+NBCMP*(IN-1)+1)
      SIG11(2)=ZR(JSIGSE-1+NBCMP*NNO*(CPT-1)+NBCMP*(INSUI-1)+1)

      SIG22(1)=ZR(JSIGSE-1+NBCMP*NNO*(CPT-1)+NBCMP*(IN-1)+2)
      SIG22(2)=ZR(JSIGSE-1+NBCMP*NNO*(CPT-1)+NBCMP*(INSUI-1)+2)

      SIG12(1)=ZR(JSIGSE-1+NBCMP*NNO*(CPT-1)+NBCMP*(IN-1)+4)
      SIG12(2)=ZR(JSIGSE-1+NBCMP*NNO*(CPT-1)+NBCMP*(INSUI-1)+4)
C
C --- RECUPERATION DES CONTRAINTES AUX NOEUDS DE L'ARR�TE COURANTE
C --- POUR LE SOUS-ELEMENT VOISIN PAR RAPPORT � CETTE ARR�TE
C
      SIGV11(1)=ZR(JSIGSE-1+NBCMP*NNO*(IVOIS-1)+NBCMP*(INV-1)+1)
      SIGV11(2)=ZR(JSIGSE-1+NBCMP*NNO*(IVOIS-1)+NBCMP*(INVSUI-1)+1)

      SIGV22(1)=ZR(JSIGSE-1+NBCMP*NNO*(IVOIS-1)+NBCMP*(INV-1)+2)
      SIGV22(2)=ZR(JSIGSE-1+NBCMP*NNO*(IVOIS-1)+NBCMP*(INVSUI-1)+2)

      SIGV12(1)=ZR(JSIGSE-1+NBCMP*NNO*(IVOIS-1)+NBCMP*(INV-1)+4)
      SIGV12(2)=ZR(JSIGSE-1+NBCMP*NNO*(IVOIS-1)+NBCMP*(INVSUI-1)+4)
C
C --- CALCUL DU SAUT DE CONTRAINTES
C
      DO 200 K=1,NBNASE
        DSG11(K)=SIG11(K)-SIGV11(K)
        DSG22(K)=SIG22(K)-SIGV22(K)
        DSG12(K)=SIG12(K)-SIGV12(K)
 200  CONTINUE

      END
