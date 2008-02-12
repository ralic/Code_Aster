      SUBROUTINE ECHMC2(NSOM  ,NOEARE,NARE  ,NECH  ,OFFSOM,
     &                  SEG   ,NSEG)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 12/02/2008   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER     NSOM
      INTEGER     NOEARE(*)    
      INTEGER     NARE   
      INTEGER     NECH  
      INTEGER     OFFSOM 
      INTEGER     SEG(2,*)
      INTEGER     NSEG         
C      
C ----------------------------------------------------------------------
C
C APPARIEMENT DE DEUX GROUPES DE MAILLE PAR LA METHODE
C BOITES ENGLOBANTES + ARBRE BSP
C
C CONNECTIVITE DE L'ECHANTILLONNAGE PRODUIT PAR LA ROUTINE ECHMAP (2D)
C
C ----------------------------------------------------------------------
C
C CHAQUE ARETE EST DECOUPEE EN SEGMENTS      
C
C IN  NSOM   : NOMBRE DE SOMMETS SANS LES POINTS D'ECHANTILLONNAGE
C IN  NOEARE : NOEUDS DEFINISSANT LES ARETES DE LA MAILLE 
C              VOIR NOARE/NBARE
C                           ( NOMBRE DE NOEUDS ARETES 1, N1, N2, ...
C                             NOMBRE DE NOEUDS ARETES 2, ... )
C              INDICE DES NOEUDS: ORDRE DU NOEUD DANS LA MAILLE 
C                                 (EX: POUR HEXA8 NOEUDS 1 A 8)
C IN  NARE   : NOMBRE D'ARETES
C IN  NECH   : NOMBRE DE POINTS D'ECHANTILLONNAGE 
C IN  OFFSOM : DECALAGE INDICE DANS LE TABLEAU DES SOMMETS
C OUT NSEG   : NOMBRE DE SEGMENTS NSEG = NARE*NECH  
C OUT SEG    : NOEUDS DEFINISSANT LES SEGMENTS
C                (SEG1.ND1,SEG1.ND2,
C                 SEG2.ND1,SEG2.ND2,...)
C              INDICE DES NOEUDS: SE REFERE AU TABLEAU NOH DANS ECHMAP
C
C ----------------------------------------------------------------------
C
      INTEGER NNP,N0,N1,IARE,J,P0
C
C ----------------------------------------------------------------------
C
      P0   = 1
      NSEG = 0
      N0   = NSOM + OFFSOM
C   
      DO 10 IARE = 1, NARE                  
        NNP = NOEARE(P0)
        N1  = NOEARE(P0+1) + OFFSOM
        DO 20 J = 2, NECH  
          N0          = N0 + 1
          NSEG        = NSEG + 1
          SEG(1,NSEG) = N1
          SEG(2,NSEG) = N0
          N1          = N0
 20     CONTINUE
        NSEG        = NSEG + 1
        SEG(1,NSEG) = N1
        SEG(2,NSEG) = NOEARE(P0+2) + OFFSOM
        P0          = P0 + NNP + 1
 10   CONTINUE
      END
