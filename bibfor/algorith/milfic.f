      SUBROUTINE MILFIC(NDIM,GEOM,XG)
      IMPLICIT NONE 

      INCLUDE 'jeveux.h'
      INTEGER       NDIM
      REAL*8        XG(NDIM),GEOM(*)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C                      COORDONNEES RELLES DU POINT MILIEU D'UNE ARETE
C                      QUADRATIQUE
C                    
C     ENTREE
C       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
C       GEOM   : COORDONNEES DES 3 NOEUDS DE L'ARETE
C
C     SORTIE
C       XG       : COORDONNES RELLES DU PT MILIEU DE L'ARETE
C
C......................................................................
C
      REAL*8        S,S1,XE1,XE
      INTEGER       NNO
      CHARACTER*8   ELP
      PARAMETER     (ELP='SE3')
      PARAMETER     (NNO=3)
C
C......................................................................
C
      CALL JEMARQ()
C     DANS COORSG : 1. C ---> ETA(C)=-1
C                   2. 101 ---> ETA(101)= 1
C                   3. F ---> ETA(F)= 0

C     CALCUL DE L'ABSCISSE CURVILIGNE DE 101
      XE1=1
      CALL ABSCVF(NDIM,GEOM,XE1,S1)

C --- COORDONNEES DU POINT DANS L'ELEMENT DE REFERENCE
C     ABSCURV(M)=[ABSCURV(A)]/2 
      S=S1/2
      CALL XINVAC(ELP,NDIM,GEOM,S,XE)
      CALL ASSERT(XE.GE.-1 .AND. XE.LE.1)

C --- COORDONNES DU POINT DANS L'ELEMENT REEL
      CALL REEREL(ELP,NNO,NDIM,GEOM,XE,XG)

      CALL JEDEMA()
      END
