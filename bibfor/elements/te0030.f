      SUBROUTINE TE0030(OPTION,NOMTE)
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =====================================================================
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C =====================================================================
C    - FONCTION REALISEE:  CALCUL DE L'OPTIONS INDI_LOCA_ELGA
C                          QUI EST UN INDICATEUR SUR LA LOCALISATION
C                          AUX POINTS DE GAUSS
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C =====================================================================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX --------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX --------------------
C =====================================================================
      INTEGER      IMATE, ICOMPO, IVARIM, IVARIP, ICONTP, ILOCAL
      INTEGER      NBVARI, NBRAC4, RINDIC, KPG, II, NBSIG
      INTEGER      NBSIGM, ICODE
      INTEGER      NDIM, NNO, NNOS, NPG, IPOIDS, IVF, IDFDE, JGANO
      REAL*8       VBIFUR, RACINE(4), DSDE(6,6)
      CHARACTER*8  MOD, MODELI
      CHARACTER*16 RELCOM
C =====================================================================
C --- RINDIC EST LE NOMBRE DE PARAMETRE DE LOCALISATION DEFINIT -------
C --- SOUS LE MOT-CLE IND_LOCA DANS GRANDEUR_SIMPLE.CATA --------------
C =====================================================================
      PARAMETER ( RINDIC  = 5 )
C =====================================================================
      IF ( OPTION.EQ.'INDI_LOCA_ELGA' ) THEN
C =====================================================================
C --- VERIFICATION DE COHERENCE ---------------------------------------
C --- LE TENSEUR ACOUSTIQUE EST DEVELOPPE EN 2D UNIQUEMENT ------------
C =====================================================================
         MODELI(1:2) = NOMTE(3:4)
         NBSIG = NBSIGM(MODELI)
         IF (MODELI(1:2).EQ.'DP') THEN
            MOD(1:6) = 'D_PLAN'
         ELSE IF (MODELI(1:2).EQ.'CP') THEN
            MOD(1:6) = 'C_PLAN'
         ELSE IF (MODELI(1:2).EQ.'AX') THEN
            MOD(1:4) = 'AXIS'
         ELSE
            CALL UTMESS('F','TE0030','LA MODELISATION : '//MODELI//
     +               'N''EST PAS TRAITEE.')
         ENDIF
C =====================================================================
C --- RECUPERATION DU ELREFE ------------------------------------------
C =====================================================================
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C =====================================================================
C --- PARAMETRES EN ENTREE --------------------------------------------
C =====================================================================
         CALL JEVECH('PMATERC','L',IMATE )
         CALL JEVECH('PCOMPOR','L',ICOMPO)
         CALL JEVECH('PVARIMR','L',IVARIM)
         CALL JEVECH('PVARIPR','L',IVARIP)
         CALL JEVECH('PCONTPR','L',ICONTP)
         RELCOM = ZK16(ICOMPO-1+1)
C =====================================================================
C --- NOMBRE DE VARIABLES INTERNES ASSOCIE A LA LOI DE COMPORTEMENT ---
C =====================================================================
         READ (ZK16(ICOMPO-1+2),'(I16)') NBVARI
C =====================================================================
C --- PARAMETRES EN SORTIE --------------------------------------------
C =====================================================================
         CALL JEVECH('PINDLOC','E',ILOCAL)
C =====================================================================
C --- BOUCLE SUR LES POINTS DE GAUSS ----------------------------------
C =====================================================================
         DO 10 KPG = 1, NPG
C =====================================================================
C --- CALCUL DE LA MATRICE TANGENTE -----------------------------------
C --- (FONCTION DE LA RELATION DE COMPORTEMENT) -----------------------
C =====================================================================
            IF (RELCOM.EQ.'DRUCKER_PRAGER') THEN
C =====================================================================
C --- LOI DE TYPE DRUCKER_PRAGER --------------------------------------
C =====================================================================
               CALL REDRPR(MOD,ZI(IMATE),ZR(ICONTP-1+(KPG-1)*NBSIG+1 ),
     +                                   ZR(IVARIM-1+(KPG-1)*NBVARI+1),
     +                                   ZR(IVARIP-1+(KPG-1)*NBVARI+1),
     +                                                      DSDE,ICODE)
               IF (ICODE.EQ.0) GO TO 10
            ELSE
               CALL UTMESS('F','TE0030',
     +                           'RELATION DE COMPORTEMENT NON TRAITE')
            ENDIF
C =====================================================================
C --- CALCUL DU TENSEUR ACOUSTIQUE ------------------------------------
C =====================================================================
            CALL CRIBIF( MOD, DSDE, VBIFUR , NBRAC4, RACINE )
C =====================================================================
C --- SURCHARGE DE L'INDICATEUR DE LOCALISATION -----------------------
C =====================================================================
            ZR(ILOCAL-1+1+(KPG-1)*RINDIC) = VBIFUR
            DO 20 II=1, NBRAC4
               ZR(ILOCAL-1+1+II+(KPG-1)*RINDIC) = RACINE(II)
 20         CONTINUE
 10      CONTINUE
      ELSE
         CALL UTMESS('F','TE0030', 'OPTION NON VALIDE')
      END IF
C =====================================================================
      END
