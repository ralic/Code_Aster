        SUBROUTINE LIRITM(IFL,ICL,IV,RV,CV,CNL,DEBLIG,ILEC)
        IMPLICIT REAL*8 (A-H,O-Z)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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
C       ----------------------------------------------------------------
C       LECTURE DE L ITEM SUIVANT
C       EN IGNORANT                     - LES SEPARATEURS
C                                       - LES LIGNES BLANCHES
C                                       - LES CARACTERES DERIERE %
C                                         JUSQU'EN FIN DE LIGNE
C       ----------------------------------------------------------------
C       IN  DEBLIG      = -1    >  LIRE ITEM EN DEBUT DE LIGNE SUIVANTE
C           IFL                 >  NUMERO LOGIQUE FICHIER MAILLAGE
C           ILEC        = 1     >  PREMIERE LECTURE DU FICHIER
C                       = 2     >  SECONDE  LECTURE DU FICHIER
C       OUT DEBLIG      = 0     >  ITEM LUT DANS LA LIGNE
C           DEBLIG      = 1     >  ITEM LUT EN DEBUT DE LIGNE
C           ICL         =-1     >  FIN DE LIGNE
C                       = 0     >  ERREUR DE LECTURE
C                       = 1     >  LECTURE ENTIER
C                       = 2     >  LECTURE REEL
C                       = 3     >  LECTURE IDENTIFICATEUR
C                       = 4     >  LECTURE CONSTANTE DE TEXTE
C                       = 5     >  LECTURE SEPARATEUR
C           IV                  >  ENTIER LU
C           RV                  >  REEL LU
C           CV                  >  CHAINE LUE
C           CNL                 >  NUMERO LIGNE (CHAINE)
C       ----------------------------------------------------------------
        INTEGER         IFL , ICL , IV , IDEB  , DEBLIG,  ILEC
        REAL*8          RV
        CHARACTER*(*)   CV
        CHARACTER*80    LIG
        CHARACTER*14    CNL
        SAVE            LIG , IDEB
        CHARACTER*16    NOP
        COMMON          /OPMAIL/        NOP
C
        IF(DEBLIG.EQ.-1)THEN
        CALL LIRLIG(IFL,CNL,LIG,ILEC)
        IDEB   = 1
        DEBLIG = 1
        ELSE
        DEBLIG = 0
        ENDIF
C
C - LECTURE ITEM SUIVANT
C
 1      CONTINUE
        CALL LXSCAN(LIG,IDEB,ICL,IV,RV,CV)
        IF(ICL.EQ.3.OR.ICL.EQ.4)CALL LXCAPS(CV(1:IV))
C
C - FIN DE LIGNE OU COMMENTAIRE
C
        IF(ICL.EQ.-1.OR.(ICL.EQ.5.AND.CV(1:1).EQ.'%'))THEN
        CALL LIRLIG(IFL,CNL,LIG,ILEC)
        IDEB   = 1
        DEBLIG = 1
        GOTO 1
        ENDIF
C
C - SEPARATEUR SAUF %
C
        IF(ICL.EQ.5.AND.CV(1:1).NE.'%')GOTO 1
C
 9999   CONTINUE
        END
