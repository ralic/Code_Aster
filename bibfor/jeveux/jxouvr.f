      SUBROUTINE JXOUVR ( ICLAS , IDN )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 06/03/2012   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C TOLE CRP_6
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             ICLAS , IDN 
C     ==================================================================
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      INTEGER          N
      PARAMETER      ( N = 5 )
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     +                 DN2(N)
      CHARACTER*8      NOMBAS
      COMMON /KBASJE/  NOMBAS(N)
      CHARACTER*128    REPGLO,REPVOL
      COMMON /BANVJE/  REPGLO,REPVOL
      INTEGER          LREPGL,LREPVO
      COMMON /BALVJE/  LREPGL,LREPVO
C     ------------------------------------------------------------------
      CHARACTER*8      NOM
      CHARACTER*128    NOM128
      INTEGER          INDX(1) , NBL
C DEB ------------------------------------------------------------------
      NBL = 1
      IF ( KSTINI(ICLAS) .NE. 'DUMMY   ' ) THEN
        IERR = 0
        NOM = NOMFIC(ICLAS)(1:4)//'.   '
        CALL CODENT(IDN,'G',NOM(6:7))
        IF ( NOM(1:4) .EQ. 'glob' ) THEN
          NOM128=REPGLO(1:LREPGL)//'/'//NOM
        ELSE IF ( NOM(1:4) .EQ. 'vola' ) THEN
          NOM128=REPVOL(1:LREPVO)//'/'//NOM
        ELSE
          NOM128='./'//NOM
        ENDIF
        CALL OPENDR ( NOM128 , INDX , NBL , 0 , IERR )
        IF ( IERR .NE. 0 ) THEN
          CALL U2MESG ( 'F' ,'JEVEUX_43',1,NOMBAS(ICLAS),1,IERR,0,R8BID)
        ENDIF
      ENDIF
      END
