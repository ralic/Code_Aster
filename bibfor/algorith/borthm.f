      SUBROUTINE BORTHM(NOMTE,AXI,TYPMOD,NDIM,NDLNO,NDLNM)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/08/2005   AUTEUR ROMEO R.FERNANDES 
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
      IMPLICIT      NONE
      LOGICAL       AXI
      INTEGER       NDIM,NDLNO,NDLNM
      CHARACTER*8   TYPMOD(2)
      CHARACTER*16  NOMTE
C ======================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
      AXI       = .FALSE.
      TYPMOD(2) = '        '
C ======================================================================
C --- TYPE DE MODELISATION? AXI DPLAN OU 3D ----------------------------
C ======================================================================
      CALL TYPTHM(NOMTE,AXI,TYPMOD,NDIM)
C ======================================================================
C --- MISE A JOUR DES DIMENSIONS POUR ELEMENTS DE BORD -----------------
C ======================================================================
      CALL DIMTHM(NOMTE,NDLNO,NDLNM,NDIM)
C ======================================================================
      END
