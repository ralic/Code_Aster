      SUBROUTINE TE0511(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 15/01/2013   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE FOUCAULT A.FOUCAULT
C =====================================================================
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16 OPTION,NOMTE
C =====================================================================
C    - FONCTION REALISEE:  CALCUL DU MODULE DE RIGIDITE
C                          DE MICRO-DILTATION CONTENU
C                          AUX POINTS DE GAUSS
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C =====================================================================
C =====================================================================
      LOGICAL      LOGTHM
      INTEGER      IMATE, ICOMPO, IVARIP, ICONTP, ILOCAL, IC, IV
      INTEGER      NBVARI, RINDIC, KPG, NBSIG, IMAT,IBID
      INTEGER      NBSIGM, ICODE, IRET, TABTHM(3), DIMMAX, NPGU
      INTEGER      NDIM, NNO, NNOS, NPG, IPOIDS, IVF, IDFDE, JGANO
      REAL*8       MODULE, SIG(6)
      REAL*8       VIN(100)
      CHARACTER*8  MOD, ALIAS8
      CHARACTER*16 RELCOM
C =====================================================================
C --- RINDIC EST LE NOMBRE DE PARAMETRE DE LOCALISATION DEFINIT -------
C --- SOUS LE MOT-CLE INDL_R DANS GRANDEUR_SIMPLE.CATA --------------
C =====================================================================
      PARAMETER ( RINDIC  = 1 )
C =====================================================================
      CALL TEATTR(' ','S','ALIAS8',ALIAS8,IBID)
      IF ( OPTION.EQ.'PDIL_ELGA' ) THEN
C =====================================================================
C --- VERIFICATION DE COHERENCE ---------------------------------------
C --- LE TENSEUR ACOUSTIQUE EST DEVELOPPE EN 2D UNIQUEMENT ------------
C =====================================================================
C --- CAS D'UN POST-TRAITEMENT EN MECANIQUE DRAINE --------------------
C =====================================================================
         LOGTHM  = .FALSE.
         IF ((ALIAS8(3:5).EQ.'DPL').OR.(ALIAS8(3:5).EQ.'DPS')) THEN
            MOD(1:6) = 'D_PLAN'
            NBSIG = NBSIGM()
         ELSE IF (ALIAS8(3:5).EQ.'CPL') THEN
            MOD(1:6) = 'C_PLAN'
            NBSIG = NBSIGM()
         ELSE IF (ALIAS8(3:5).EQ.'AX_') THEN
            MOD(1:4) = 'AXIS'
            NBSIG = NBSIGM()
         ELSE
C =====================================================================
C --- CAS D'UN POST-TRAITEMENT EN MECANIQUE THM -----------------------
C =====================================================================
            LOGTHM  = .TRUE.
            IF (ALIAS8(3:5).EQ.'AH2')THEN
               MOD(1:4) = 'AXIS'
            ELSE IF ((ALIAS8(3:5).EQ.'DH2').OR.
     &               (ALIAS8(3:5).EQ.'DR1').OR.
     &               (ALIAS8(3:5).EQ.'DM1'))THEN
               MOD(1:6) = 'D_PLAN'
            ELSE
C =====================================================================
C --- CAS NON TRAITE --------------------------------------------------
C =====================================================================
               CALL U2MESK('F','ELEMENTS_11',1,NOMTE)
            ENDIF
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
         CALL JEVECH('PVARIPR','L',IVARIP)
         IF (LOGTHM) THEN
C =====================================================================
C --- DANS LE CADRE THM ON FAIT UN TECACH PLUTOT QU'UN JEVECH POUR ----
C --- RECUPERER EGALEMENT LA DIMENSION DU VECTEUR QUI DIFFERE SUIVANT -
C --- LA MODELISATION THM ---------------------------------------------
C =====================================================================
            CALL TECACH('OOO','PCONTPR','L',3,TABTHM,IRET)
            ICONTP = TABTHM(1)
            DIMMAX = TABTHM(2)
            NPGU   = TABTHM(3)
C =====================================================================
C --- ON TESTE LA COHERENCE DES RECUPERATIONS ELREF4 ET TECACH SUR ----
C --- LE NOMBRE DE POINTS DE GAUSS ------------------------------------
C =====================================================================
            CALL ASSERT(NPGU.EQ.NPG)
            NBSIG = DIMMAX / NPG
C =====================================================================
C --- DANS LE CADRE DE LA THM ON RECUPERE DIRECTEMENT LA RELATION -----
C --- DE COMPORTEMENT DE TYPE MECANIQUE -------------------------------
C =====================================================================
            RELCOM = ZK16(ICOMPO-1+11)
         ELSE
            CALL JEVECH('PCONTPR','L',ICONTP)
            RELCOM = ZK16(ICOMPO-1+ 1)
         ENDIF
C =====================================================================
C --- NOMBRE DE VARIABLES INTERNES ASSOCIE A LA LOI DE COMPORTEMENT ---
C =====================================================================
         READ (ZK16(ICOMPO-1+2),'(I16)') NBVARI
C =====================================================================
C --- PARAMETRES EN SORTIE --------------------------------------------
C =====================================================================
         CALL JEVECH('PPDIL','E',ILOCAL)
C =====================================================================
C --- BOUCLE SUR LES POINTS DE GAUSS ----------------------------------
C =====================================================================
         DO 10 KPG = 1, NPG
C =====================================================================
C --- INITIALISATIONS -------------------------------------------------
C =====================================================================
            MODULE    = 0.0D0
C =====================================================================
C --- CALCUL DU MODULE DE RIGIDITE DE MICRO-DILTATION -----------------
C =====================================================================
            DO 20 IC = 1, NBSIG
              SIG(IC) = ZR(ICONTP-1+(KPG-1)*NBSIG+IC )
 20         CONTINUE
            DO 30 IV = 1, NBVARI
              VIN(IV) = ZR(IVARIP-1+(KPG-1)*NBVARI+IV )
 30         CONTINUE
            IMAT = ZI(IMATE)
            CALL EVALA1( MOD, RELCOM, SIG, VIN, IMAT, MODULE, ICODE)
C =====================================================================
C --- SURCHARGE DE L'INDICATEUR DE LOCALISATION -----------------------
C =====================================================================
            ZR(ILOCAL-1+1+(KPG-1)*RINDIC) = MODULE
 10      CONTINUE
      ELSE
CC OPTION DE CALCUL INVALIDE
        CALL ASSERT(.FALSE.)
      END IF
C =====================================================================
      END
