      SUBROUTINE IBFHDF ( IER , FICHDF )
      IMPLICIT NONE
      INTEGER             IER
      CHARACTER*80              FICHDF
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 08/06/2004   AUTEUR CIBHHLV L.VIVAN 
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
C
C     LECTURE DU MOT CLE HDF DANS LA COMMANDE DEBUT
C
C ----------------------------------------------------------------------
      CHARACTER*16    CBID,NOMCMD,FHDF
      INTEGER         NFHDF
C
      FICHDF = ' '
      CALL GETRES(CBID,CBID,NOMCMD)
      IF ( NOMCMD .EQ. 'DEBUT' .OR. NOMCMD .EQ. 'POURSUITE') THEN
        CALL GETVTX(' ','FORMAT_HDF',1,1,1,FHDF,NFHDF)
        IF (NFHDF .GT. 0) THEN
          IF ( FHDF .EQ. 'OUI' ) THEN
            FICHDF = 'bhdf.1'
          ENDIF
        ENDIF
      ENDIF
C
      END
