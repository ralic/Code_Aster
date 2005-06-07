      SUBROUTINE JXFERM ( ICLAS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX DATE 28/02/96 AUTEUR D6BHHJP J.P.LEFEBVRE
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER             ICLAS
C     ------------------------------------------------------------------
      INTEGER          N
      PARAMETER      ( N = 5 )
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     +                 DN2(N)
      CHARACTER*8      NOMBAS
      COMMON /KBASJE/  NOMBAS(N)
      INTEGER          IDN    , IEXT    , NBENRG
      COMMON /IEXTJE/  IDN(N) , IEXT(N) , NBENRG(N)
C     ------------------------------------------------------------------
      CHARACTER*8     NOM
      INTEGER         IER
C DEB ------------------------------------------------------------------
      IER = 0
      NOM = NOMFIC(ICLAS)(1:4)//'.   '
      DO 1 K = 1,IEXT(ICLAS)
        CALL CODENT(K,'G',NOM(6:7))
        CALL CLOSDR ( NOM , IER )
        IF ( IER .NE. 0 ) THEN
          CALL JVMESS ( 'S','JXFERM_01' ,'ERREUR LORS DE LA FERMETURE'//
     &                      ' DE LA BASE '//NOMBAS(ICLAS))
        ENDIF
 1    CONTINUE
C
      END
