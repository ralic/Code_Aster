      SUBROUTINE LISCCP(PHENOM,LISCHA)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16 PHENOM
      CHARACTER*19 LISCHA
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE (LISTE_CHARGES)
C
C VERIFICATION COMPATIBILITE CHARGE/PHENOMENE
C
C ----------------------------------------------------------------------
C
C
C IN  PHENOM : TYPE DE PHENOMENE (MECANIQUE, THERMIQUE, ACOUSTIQUE)
C IN  LISCHA : SD LISTE DES CHARGES
C
C
C
C
      INTEGER      ICHAR,NBCHAR
      INTEGER      IBID,IRET,CODCHA
      CHARACTER*8  PHECHA,CHARGE
      LOGICAL      LOK
      LOGICAL      LISICO,LVEAC,LVEAG,LVEAS
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- NOMBRE DE CHARGES
C
      CALL LISNNB(LISCHA,NBCHAR)
      IF (NBCHAR.EQ.0) GOTO 999
C
C --- BOUCLE SUR LES CHARGES
C
      DO 10 ICHAR = 1,NBCHAR
        LOK  = .FALSE.
C
C ----- CODE DU GENRE DE LA CHARGE
C
        CALL LISLCO(LISCHA,ICHAR ,CODCHA)
        LVEAC  = LISICO('VECT_ASSE_CHAR',CODCHA)
        LVEAG  = LISICO('VECT_ASSE_GENE',CODCHA)
        LVEAS  = LISICO('VECT_ASSE'     ,CODCHA)
C
C ----- PHENOMENE DE LA CHARGE
C
        IF (LVEAC.OR.LVEAS.OR.LVEAG) THEN
          PHECHA = ' '
        ELSE
          CALL LISLCH(LISCHA,ICHAR ,CHARGE)
          CALL DISMOI('F','TYPE_CHARGE',CHARGE,'CHARGE',IBID,
     &                PHECHA,IRET)
        ENDIF        
C
        IF (PHENOM.EQ.'MECANIQUE') THEN
          IF ((PHECHA(1:4).EQ.'MECA').OR.
     &        (PHECHA(1:4).EQ.'CIME').OR.
     &        (LVEAC.OR.LVEAS.OR.LVEAG)) THEN
            LOK = .TRUE.
          ENDIF
        ELSEIF (PHENOM.EQ.'THERMIQUE') THEN
          IF ((PHECHA(1:4).EQ.'THER').OR.
     &        (PHECHA(1:4).EQ.'CITH')) THEN
            LOK = .TRUE.
          ENDIF
        ELSEIF (PHENOM.EQ.'ACOUSTIQUE') THEN
          IF ((PHECHA(1:4).EQ.'ACOU').OR.
     &        (PHECHA(1:4).EQ.'CIAC')) THEN
            LOK = .TRUE.
          ENDIF
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
C
        IF (.NOT.LOK) THEN
          CALL U2MESK('F','CHARGES5_4',1,CHARGE)
        ENDIF
  10  CONTINUE
C
  999 CONTINUE
C
      CALL JEDEMA()
      END
