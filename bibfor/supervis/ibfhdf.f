      SUBROUTINE IBFHDF ( IER , FICHDF )
      IMPLICIT NONE
      INTEGER             IER
      CHARACTER*80              FICHDF
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 06/09/2003   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
      CHARACTER*16    CBID,NOMCMD
      INTEGER         NBOCC,K,ILONG
C
      FICHDF = ' '
      CALL GETRES(CBID,CBID,NOMCMD)
      IF ( NOMCMD .EQ. 'DEBUT' .OR. NOMCMD .EQ. 'POURSUITE') THEN
        CALL GETFAC('HDF',NBOCC)
        IF (NBOCC .GT. 0) THEN
           CALL GETVTX('HDF','FICHIER',1,1,1,FICHDF,K)
           CALL GETLTX('HDF','FICHIER',1,1,1,ILONG ,K)
           IF ( ILONG .GT. 80 ) CALL UTMESS('A',NOMCMD,'L''ARGUMENT '
     &          //'DU MOT CLE "FICHIER" SOUS LE MOT CLE FACTEUR "HDF" '
     &          //'EST TRONQUE A 80 CARACTERES. LE NOM DE FICHIER '
     &          //'EST DONC "'//FICHDF//'".' )
        ENDIF
      ENDIF
C
      END
