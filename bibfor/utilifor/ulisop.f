      INTEGER FUNCTION ULISOP (UNIT, NAME)
      IMPLICIT NONE
      INTEGER                  UNIT
      CHARACTER*16                   NAME 
C     ------------------------------------------------------------------
C     RETOURNE LE NOM LOCAL NAME (DDNAME) ASSOCIE A L'UNITE LOGIQUE
C     ------------------------------------------------------------------
C
C OUT  NAME     : CH*16 : NOM "LOCALE" ASSOCIE AU NUMERO D'UNITE LOGIQUE
C                         FORTRAN 
C      ULISOP   : IS    : 0 L'UNITE LOGIQUE N'EST PAS OUVERTE
C                         UNIT SINON
C IN   UNIT     : IS    : NUMERO D'UNITE LOGIQUE ASSOCIE A "NAME"
C                    
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 10/10/2003   AUTEUR D6BHHJP J.P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE D6BHHJP J.P.LEFEBVRE
C
      INTEGER          MXF
      PARAMETER       (MXF=100)
      CHARACTER*1      TYPEFI(MXF),ACCEFI(MXF),ETATFI(MXF),MODIFI(MXF)
      CHARACTER*16     DDNAME(MXF)
      CHARACTER*255    NAMEFI(MXF)
      INTEGER          FIRST, UNITFI(MXF) , NBFILE
      COMMON/ ASGFI1 / FIRST, UNITFI      , NBFILE
      COMMON/ ASGFI2 / NAMEFI,DDNAME,TYPEFI,ACCEFI,ETATFI,MODIFI
C
      CHARACTER*8      K8BID
      INTEGER          I,IVAL
C
      IF ( FIRST .NE. 17111990 ) CALL ULINIT
C
      IF ( UNIT .LT. 0 ) THEN
        WRITE(K8BID,'(I4)') UNIT 
        CALL UTMESS ('F','ULISOP01','ARGUMENT D''APPEL INVALIDE : '
     &               //' UNIT = '//K8BID)
      ENDIF
      NAME = '?'
      IVAL = 0
      DO 1 I = 1, NBFILE
        IF( UNITFI(I) .EQ. UNIT .AND. ETATFI(I) .EQ. 'O') THEN
          NAME = DDNAME(I)
          IVAL = I
          GOTO 2
        ENDIF
   1  CONTINUE
   2  CONTINUE
      ULISOP = IVAL
      END
