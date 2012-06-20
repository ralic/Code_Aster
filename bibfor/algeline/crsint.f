      SUBROUTINE CRSINT(SOLVEU)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 20/06/2012   AUTEUR BOITEAU O.BOITEAU 
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
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*19 SOLVEU
C-----------------------------------------------------------------------
C     CREATION D'UNE SD SOLVEUR MUMPS POUR LA ROUTINE CONINT (OPERATEUR
C     MODE_STATIQUE
C     ATTENTION A LA COHERENCE AVEC CRSVMU ET CRSMSP
C-----------------------------------------------------------------------
C IN  K19  SOLVEUR    : NOM DE LA SD SOLVEUR MUMPS BIDON
C-----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      INTEGER ISLVK,ISLVR,ISLVI
C----------------------------------------------------------------------
      CALL JEMARQ()
      CALL WKVECT(SOLVEU//'.SLVK','V V K24',12,ISLVK)
      CALL WKVECT(SOLVEU//'.SLVR','V V R',4,ISLVR)
      CALL WKVECT(SOLVEU//'.SLVI','V V I',7,ISLVI)

      ZK24(ISLVK-1+1)  = 'MUMPS                   '
      ZK24(ISLVK-1+2)  = 'AUTO                    '
      ZK24(ISLVK-1+3)  = 'AUTO                    '
      ZK24(ISLVK-1+4)  = 'AUTO                    '
      ZK24(ISLVK-1+5)  = 'NON                     '
      ZK24(ISLVK-1+6)  = 'OUI                     '
      ZK24(ISLVK-1+7)  = 'NON                     '
      ZK24(ISLVK-1+8)  = 'NON                     '
      ZK24(ISLVK-1+9)  = 'IN_CORE                 '
      ZK24(ISLVK-1+10) = 'NON                     '
      ZK24(ISLVK-1+11) = 'AUTO                    '
      ZK24(ISLVK-1+12) = 'XXXX                    '

      ZR(ISLVR-1+1) = -1.D0
      ZR(ISLVR-1+2) = -1.D0
      ZR(ISLVR-1+3) = 0.D0
      ZR(ISLVR-1+4) = 0.D0

      ZI(ISLVI-1+1) = 9
      ZI(ISLVI-1+2) = 50
      ZI(ISLVI-1+3) = 0
      ZI(ISLVI-1+4) = -9999
      ZI(ISLVI-1+5) = -9999
      ZI(ISLVI-1+6) = -9999
      ZI(ISLVI-1+7) = -9999
      CALL JEDEMA()
      END
