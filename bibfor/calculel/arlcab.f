      SUBROUTINE ARLCAB(NOMARL,NOMARB,BASE  ,NMA   )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*8  NOMARL      
      CHARACTER*1  BASE
      CHARACTER*16 NOMARB
      INTEGER      NMA
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C CREATION DE LA SD ARBRE
C
C ----------------------------------------------------------------------
C
C
C IN  NOMARL : NOM DE LA SD PRINCIPALE ARLEQUIN
C IN  NOMARB : NOM DE LA SD ARBRE
C IN  BASE   : TYPE DE BASE ('V' OU 'G')
C IN  NMA    : NOMBRE DE MAILLES
C      
C ----------------------------------------------------------------------
C
      INTEGER      ARLGEI
      REAL*8       ARLGER
      REAL*8       GAMMA0,GAMMA1,PRECAR
      INTEGER      IMAX,NMIN   
C      
C ----------------------------------------------------------------------
C
      GAMMA0 = ARLGER(NOMARL,'GAMMA0')
      GAMMA1 = ARLGER(NOMARL,'GAMMA1')
      PRECAR = ARLGER(NOMARL,'PRECAR')
      IMAX   = ARLGEI(NOMARL,'IMAX  ')
      NMIN   = ARLGEI(NOMARL,'NMIN  ')
      CALL ARBRCR(NOMARB,BASE  ,NMA   ,NMIN  ,GAMMA0,
     &            GAMMA1,PRECAR,IMAX)
      END
