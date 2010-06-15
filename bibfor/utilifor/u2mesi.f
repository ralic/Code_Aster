      SUBROUTINE U2MESI (CH1, IDMESS, NI, VALI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 26/09/2006   AUTEUR D6BHHJP J.P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     ROUTINE CHAPEAU D'APPEL A U2MESG POUR NE PASSER QUE DES ENTIERS
C
C     ------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER* (*)   CH1,IDMESS
      INTEGER          NI,VALI(*)
C     ------------------------------------------------------------------
      CHARACTER*8      VALK(1)
      INTEGER          NK,NR
      REAL*8           VALR(1),R8NNEM
C     ------------------------------------------------------------------
      NR = 0
      NK = 0
      VALK(1) = ' '
      VALR(1) = R8NNEM()
      CALL U2MESG (CH1, IDMESS, NK, VALK, NI, VALI, NR, VALR)
C     ------------------------------------------------------------------
      END
