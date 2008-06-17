      SUBROUTINE DILCAR(OPTION,ICOMPO,ICONTM,IDEPLM,IDEPLP,IGEOM,IMATE,
     +                  IMATUU,IVECTU,ICONTP,IVARIP,ICHG,ICHN,JCRET,
     +                  IDEFO)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/05/2007   AUTEUR FERNANDES R.FERNANDES 
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
C ======================================================================
      IMPLICIT      NONE
      INTEGER       ICOMPO,ICONTM,IDEPLM,IDEPLP,IGEOM,IMATE,JCRET,IDEFO
      INTEGER       IMATUU,IVECTU,ICONTP,ICHG,ICHN,IVARIP
      CHARACTER*16  OPTION
C ======================================================================
C --- BUT : RECUPERATION DES ADRESSES DES CHAMPS DE L'ELEMENT ----------
C -------   POUR LES MODELES SECOND GRADIENT ---------------------------
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER ISMAEM,IVARIM
C ======================================================================
C --- INITIALISATION DE TOUTES LES ADRESSES A L'ENTIER MAXIMAL ---------
C ======================================================================
      ICOMPO=ISMAEM()
      ICONTM=ISMAEM()
      IDEPLM=ISMAEM()
      IDEPLP=ISMAEM()
      IGEOM =ISMAEM()
      IMATE =ISMAEM()
      IVARIM=ISMAEM()
      IMATUU=ISMAEM()
      IVECTU=ISMAEM()
      ICONTP=ISMAEM()
      IVARIP=ISMAEM()
      ICHG  =ISMAEM()
      ICHN  =ISMAEM()
      JCRET =ISMAEM()
      IDEFO =ISMAEM()
C ======================================================================
      IF (OPTION.EQ.'EPSI_ELNO_DEPL'.OR.
     +    OPTION.EQ.'CHAR_MECA_PESA_R') THEN
         CALL U2MESS('F','ELEMENTS3_33')
      ENDIF
C ======================================================================
C --- RECUPERATION DES CHAMPS D'ENTREE DE L'ELEMENT --------------------
C ======================================================================
      IF (OPTION(1:9).EQ.'RIGI_MECA') THEN
         CALL JEVECH('PCOMPOR','L',ICOMPO )
         CALL JEVECH('PCONTMR','L',ICONTM )
         CALL JEVECH('PDEPLMR','L',IDEPLM )
         CALL JEVECH('PDEPLPR','L',IDEPLP )
         CALL JEVECH('PGEOMER','L',IGEOM  )
         CALL JEVECH('PMATERC','L',IMATE  )
         CALL JEVECH('PVARIMR','L',IVARIM )
         CALL JEVECH('PMATUNS','E',IMATUU )
      ELSE IF (OPTION(1:9).EQ.'RAPH_MECA' ) THEN
         CALL JEVECH('PCOMPOR','L',ICOMPO )
         CALL JEVECH('PCONTMR','L',ICONTM )
         CALL JEVECH('PDEPLMR','L',IDEPLM )
         CALL JEVECH('PDEPLPR','L',IDEPLP )
         CALL JEVECH('PGEOMER','L',IGEOM  )
         CALL JEVECH('PMATERC','L',IMATE  )
         CALL JEVECH('PVARIMR','L',IVARIM )
         CALL JEVECH('PVECTUR','E',IVECTU )
         CALL JEVECH('PCONTPR','E',ICONTP )
         CALL JEVECH('PVARIPR','E',IVARIP )
         CALL JEVECH('PCODRET','E',JCRET  )
      ELSE IF (OPTION(1:9).EQ.'FULL_MECA' ) THEN
         CALL JEVECH('PCOMPOR','L',ICOMPO )
         CALL JEVECH('PCONTMR','L',ICONTM )
         CALL JEVECH('PDEPLMR','L',IDEPLM )
         CALL JEVECH('PDEPLPR','L',IDEPLP )
         CALL JEVECH('PGEOMER','L',IGEOM  )
         CALL JEVECH('PMATERC','L',IMATE  )
         CALL JEVECH('PVARIMR','L',IVARIM )
         CALL JEVECH('PMATUNS','E',IMATUU )
         CALL JEVECH('PVECTUR','E',IVECTU )
         CALL JEVECH('PCONTPR','E',ICONTP )
         CALL JEVECH('PVARIPR','E',IVARIP )
         CALL JEVECH('PCODRET','E',JCRET  )
      ELSE IF (OPTION(1:9).EQ.'FORC_NODA' ) THEN
         CALL JEVECH('PCOMPOR','L',ICOMPO )
         CALL JEVECH('PCONTMR','L',ICONTM )
         CALL JEVECH('PDEPLMR','L',IDEPLM )
         CALL JEVECH('PGEOMER','L',IGEOM  )
         CALL JEVECH('PMATERC','L',IMATE  )
         CALL JEVECH('PVECTUR','E',IVECTU )
      ELSE IF (OPTION.EQ.'SIEF_ELNO_ELGA' ) THEN
         CALL JEVECH('PCOMPOR', 'L',ICOMPO)
         CALL JEVECH('PCONTRR', 'L',ICHG  )
         CALL JEVECH('PSIEFNOR','E',ICHN  )
      ELSE IF (OPTION.EQ.'VARI_ELNO_ELGA' ) THEN
         CALL JEVECH('PCOMPOR', 'L',ICOMPO)
         CALL JEVECH('PVARIGR', 'L',ICHG  )
         CALL JEVECH('PVARINR', 'E',ICHN  )
      ELSE IF (OPTION.EQ.'EPSI_ELGA_DEPL' ) THEN
         CALL JEVECH('PGEOMER','L',IGEOM  )
         CALL JEVECH('PDEPLAR','L',IDEPLP )
         CALL JEVECH('PDEFORR','E',IDEFO  )
      ENDIF
C ======================================================================
      END
