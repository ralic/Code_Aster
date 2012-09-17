      SUBROUTINE NMDOIM(SDIMPR)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/09/2012   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      CHARACTER*24 SDIMPR
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (STRUCTURES DE DONNEES)
C
C LECTURE DES DONNNES IMPRESSION
C
C ----------------------------------------------------------------------
C
C
C OUT SDIMPR : SD IMPRESSION
C
C ----------------------------------------------------------------------
C
      INTEGER      IFM,NIV
      CHARACTER*16 MOTFAC,REPK
      LOGICAL      LTCVFI,LINFRE,LINFTP
      INTEGER      IARG,NOC,UTCVFI
C
C ----------------------------------------------------------------------
C
      CALL INFNIV(IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> ... LECTURE INFO AFFICHAGE'
      ENDIF
C
C --- INITIALISATIONS
C
      MOTFAC = 'AFFICHAGE'
      LINFRE = .FALSE.
      LINFTP = .FALSE.
      UTCVFI  = 0
      LTCVFI = .FALSE.
C
C --- INFO SUR LES RESIDUS
C
      CALL GETVTX(MOTFAC,'INFO_RESIDU',1,IARG,1,REPK,NOC)
      IF (NOC.EQ.0) THEN
        LINFRE = .FALSE.
      ELSE
        IF (REPK.EQ.'OUI') THEN
          LINFRE = .TRUE.
        ELSEIF (REPK.EQ.'NON') THEN
          LINFRE = .FALSE.
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ENDIF
C
C --- INFO SUR LE TEMPS
C
      CALL GETVTX(MOTFAC,'INFO_TEMPS',1,IARG,1,REPK,NOC)
      IF (NOC.EQ.0) THEN
        LINFTP = .FALSE.
      ELSE
        IF (REPK.EQ.'OUI') THEN
          LINFTP = .TRUE.
        ELSEIF (REPK.EQ.'NON') THEN
          LINFTP = .FALSE.
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ENDIF
C
C --- SORTIE TABLEAU CONVERGENCE DANS FICHIER CSV ?
C
      CALL GETVIS(MOTFAC,'UNITE',1,IARG,1,UTCVFI,NOC)
      IF (NOC.EQ.0) THEN
        LTCVFI = .FALSE.
      ELSE
        IF (UTCVFI.EQ.0) THEN
          LTCVFI = .FALSE.
        ELSE
          LTCVFI = .TRUE.
        ENDIF
      ENDIF
C
C --- SAUVEGARDE DONNEES
C
      CALL OBSETB(SDIMPR,'INFO_RESIDU'  ,LINFRE)
      CALL OBSETB(SDIMPR,'INFO_TEMPS '  ,LINFTP)
      CALL OBSETB(SDIMPR,'TABL_CONV_CSV',LTCVFI)
      CALL OBSETI(SDIMPR,'UNIT_CONV_CSV',UTCVFI)
C
      END
