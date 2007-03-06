      SUBROUTINE UTEXCM( NUM, IDMESS, NK, VALK, NI, VALI, NR, VALR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 21/02/2007   AUTEUR TARDIEU N.TARDIEU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ROUTINE QUI LEVE UNE EXCEPTION AVEC IMPRESSION DE MESSAGES 
C     ------------------------------------------------------------------
C     ARGUMENTS :
C        NUM    = NUMERO DE L'EXCEPTION
C        IDMESS = IDENTIFIANT DU MESSAGE
C        NK     = LONGUEUR DU TABLEAU DE CHAINES VALK
C        VALK   = TABLEAU DE CHAINES A PASSER AU MESSAGE
C        NI     = LONGUEUR DU TABLEAU D'ENTIERS VALI
C        VALI   = TABLEAU D'ENTIERS A PASSER AU MESSAGE
C        NR     = LONGUEUR DU TABLEAU DE REELS VALR
C        VALK   = TABLEAU DE REELS A PASSER AU MESSAGE
C        TEXTE  = MESSAGE EXPLIQUANT POURQUOI L'EXCEPTION EST LEVEE.
C     ------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER            NUM
      CHARACTER* (*)     IDMESS,VALK(*)
      INTEGER            NK,NI,VALI(*),NR
      REAL*8             VALR(*)
      INTEGER          NEXCEP
      COMMON /UTEXC /  NEXCEP
C
      NEXCEP = NUM
      CALL U2MESG('Z',IDMESS, NK, VALK, NI, VALI, NR, VALR)
      END
