      SUBROUTINE OP0160 ( IER )
      IMPLICIT   NONE
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 14/05/2002   AUTEUR DURAND C.DURAND 
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
C     OPERATEUR   IMPR_MACR_ELEM
C     ------------------------------------------------------------------
      INTEGER      VERSIO, N1
      CHARACTER*8  FORMAT, FICHIE, MACREL
C     ------------------------------------------------------------------
C
      CALL GETVTX (' ','FORMAT',1,1,1,FORMAT,N1)
C
      FICHIE = '        '
      CALL GETVTX (' ','FICHIER',1,1,1,FICHIE,N1)
      IF ( FICHIE .EQ. '        ' ) FICHIE = FORMAT
C
      CALL GETVIS (' ','VERSION',1,1,1,VERSIO,N1)
C
      CALL GETVID (' ','MACR_ELEM_DYNA',1,1,1,MACREL,N1)
C
C     ------------------------------------------------------------------
      IF ( FORMAT .EQ. 'IDEAS' ) THEN
         CALL IREDSU ( MACREL, FORMAT, FICHIE , VERSIO )
C
      ELSEIF ( FORMAT .EQ. 'MISS_3D' ) THEN
         CALL IREDMI ( MACREL, FICHIE )
C
      ELSEIF ( FORMAT .EQ. 'CADYRO' ) THEN
         CALL IREDCA ( MACREL )
C
      ELSEIF ( FORMAT .EQ. 'PLEXUS' ) THEN
         CALL IREDPL ( MACREL,FICHIE, VERSIO )
      ELSE
         CALL UTMESS('F','OP0160','ERREUR 1')
      ENDIF
C
      END
