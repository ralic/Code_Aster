      SUBROUTINE DPDEDI( MATERF, TD, TF, TR, DEPST, DEPS)
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/12/2003   AUTEUR GRANET S.GRANET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT      NONE
      REAL*8        MATERF(4,2), TD, TF, TR, DEPST(6), DEPS(6)
C =====================================================================
C --- RETRAIT DE LA DEFORMATION DUE A LA DILATATION THERMIQUE ---------
C =====================================================================
C --- IN --- : MATERF  COEFFICIENTS MATERIAU --------------------------
C ---------- : TD      TEMPERATURE DEBUT INCREMENT --------------------
C ---------- : TF      TEMPERATURE FIN INCREMENT ----------------------
C ---------- : TR      TEMPERATURE DE REFERENCE -----------------------
C ---------- : DEPST   INCREMENT DE DEFORMATION TOTALE ----------------
C --- OUT -- : DEPSM   INCREMENT DE DEFORMATION MECANIQUE -------------
C =====================================================================
      INTEGER     II, NDT, NDI
      REAL*8      ALPHA
C =====================================================================
      COMMON /TDIM/   NDT, NDI
C =====================================================================
C --- LES PARAMETRES MATERIAUX SONT SUPPOSES CONSTANT -----------------
C =====================================================================
      ALPHA = MATERF(3,1)
      DO 10 II = 1, NDI
         DEPS(II) = DEPST(II) - ( ALPHA*(TF-TR) - ALPHA*(TD-TR))
C         EPSD(II) = EPSDT(II) - ( ALPHA*(TD-TR) )
 10   CONTINUE
      DO 20 II = NDI+1, NDT
         DEPS(II) = DEPST(II)
C         EPSD(II) = EPSDT(II)
 20   CONTINUE
C =====================================================================
      END
