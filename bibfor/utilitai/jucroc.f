      SUBROUTINE JUCROC( NOMC, NOOC, NUOC, DIM, LDEC )
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      CHARACTER*(*)      NOMC, NOOC
      INTEGER                        NUOC, DIM, LDEC
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C     ------------------------------------------------------------------
C     CREATION D'UN OBJET DE COLLECTION
C     ------------------------------------------------------------------
C IN  NOMC  : CH*24 : NOM (COMPLET)  DE LA COLLECTION
C IN  NOOC  : CH*8  : NOM  DE L'OBJET (SI NUM <=0)
C IN  NUOC  : IS    : OU NUM  DE L'OBJET (>0)
C IN  DIM   : IS    : TAILLE DE L'OBJET
C OUT LDEC  : IS    : DECALAGE
C     ------------------------------------------------------------------
      CHARACTER*4  CBID
      CHARACTER*32 NOM
C     ------------------------------------------------------------------
      IF ( NUOC .GT. 0 ) THEN
         NOM = JEXNUM(NOMC,NUOC)
      ELSE
         NOM = JEXNOM(NOMC,NOOC)
      ENDIF
      CALL JECROC ( NOM )
      CALL JEECRA ( NOM, 'LONMAX', DIM , CBID )
      CALL JEECRA ( NOM, 'LONUTI', DIM , CBID )
      CALL JEVEUO ( NOM, 'E' , LDEC  )
      END
