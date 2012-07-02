      SUBROUTINE VDREP2 ( ALPHA, BETA, ZILZI, ZRLZR,
     &                    MATEVN, MATEVG )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C.======================================================================
      IMPLICIT NONE
C
C      VDREP2   -- DETERMINATION DES MATRICES DE PASSAGE
C                  DES REPERES INTRINSEQUES AUX NOEUDS  DE L'ELEMENT
C                  AU REPERE UTILISATEUR (MATRICE MATEVN)
C                  ET DES REPERES INTRINSEQUES AUX POINTS D'INTEGRATION
C                  DE L'ELEMENT AU REPERE UTILISATEUR (MATRICE MATEVG)
C                  POUR LES ELEMENTS DE COQUE EPAISSE 3D .
C
C   ARGUMENT        E/S   TYPE         ROLE
C    ALPHA, BETA    IN     R    ANGLES DETERMINANT LE REPERE UTILISATEUR
C    MATEVN(2,2,10) OUT    R        MATRICES DE PASSAGE DES REPERES
C                                   INTRINSEQUES AUX NOEUDS  DE
C                                   L'ELEMENT AU REPERE UTILISATEUR
C    MATEVG(2,2,10) OUT    R        MATRICES DE PASSAGE DES REPERES
C                                   INTRINSEQUES AUX POINTS
C                                   D'INTEGRATION DE L'ELEMENT AU
C                                   REPERE UTILISATEUR
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           REAL*8            MATEVN(2,2,1), MATEVG(2,2,1)
C -----  VARIABLES LOCALES
           REAL*8            PGL(3,3), NORM, ZRLZR(*)
           INTEGER           ZILZI(*)
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- NOMBRE DE NOEUDS DE L'ELEMENT  :
C     -----------------------------
C-----------------------------------------------------------------------
      INTEGER I ,IDEC ,IGAU ,INO ,J ,K ,NB2 
      INTEGER NPGSN 
      REAL*8 ALPHA ,BETA ,C ,DX ,DY ,DZ ,PJDX 
      REAL*8 PJDY ,PJDZ ,PS ,R8DGRD ,R8PREM ,S 
C-----------------------------------------------------------------------
      NB2  = ZILZI(2)
C
C --- NOMBRE DE POINTS D'INTEGRATION DE L'ELEMENT (SOUS-INTEGRE) :
C     ----------------------------------------------------------
      NPGSN= ZILZI(4)
C
C --- RECUPERATION DES ANGLES DETERMINANT LE REPERE UTILISATEUR
C --- PAR RAPPORT AU REPERE GLOBAL :
C     ============================
      ALPHA = ALPHA * R8DGRD()
      BETA  = BETA * R8DGRD()
C
      DX = COS(BETA)*COS(ALPHA)
      DY = COS(BETA)*SIN(ALPHA)
      DZ = SIN(BETA)
      NORM = SQRT (DX*DX + DY*DY + DZ*DZ)
      DX = DX/NORM
      DY = DY/NORM
      DZ = DZ/NORM
C
C --- DETERMINATION DES MATRICES DE PASSAGE DES REPERES INTRINSEQUES
C --- AUX NOEUDS DE L'ELEMENT AU REPERE UTILISATEUR :
C     =============================================
C
C --- ADRESSE DES MATRICES DE PASSAGE DU REPERE GLOBAL AUX REPERES
C --- INTRINSEQUES AUX NOEUDS DE L'ELEMENT DANS LE TABLEAU .DESR :
C     ----------------------------------------------------------
      IDEC = 1090
C
C --- BOUCLE SUR LES NOEUDS DE L'ELEMENT :
C     ----------------------------------
      DO 10 INO = 1, NB2
C
C ---   RECUPERATION DE LA MATRICE DE PASSAGE AU NOEUD COURANT :
C       ------------------------------------------------------
        K = 0
        DO 20 J = 1, 3
          DO 30 I = 1, 3
            K  = K + 1
            PGL(I,J) = ZRLZR(IDEC+(INO-1)*9+K)
  30      CONTINUE
  20    CONTINUE
C
C ---   DETERMINATION DE LA PROJECTION DU VECTEUR X DU REPERE
C ---   UTILISATEUR SUR LE FEUILLET TANGENT A LA COQUE AU NOEUD
C ---   COURANT :
C       -------
        PS = DX*PGL(3,1) + DY*PGL(3,2) + DZ*PGL(3,3)
        PJDX = DX - PS*PGL(3,1)
        PJDY = DY - PS*PGL(3,2)
        PJDZ = DZ - PS*PGL(3,3)
        NORM = SQRT (PJDX*PJDX + PJDY*PJDY + PJDZ*PJDZ)
        IF ( NORM .LE. R8PREM() ) THEN
          CALL U2MESS('F','ELEMENTS_49')
        ENDIF
C
        PJDX = PJDX/NORM
        PJDY = PJDY/NORM
        PJDZ = PJDZ/NORM
C
        C = PJDX*PGL(1,1) + PJDY*PGL(1,2) + PJDZ*PGL(1,3)
        S = PJDX*PGL(2,1) + PJDY*PGL(2,2) + PJDZ*PGL(2,3)
C
        MATEVN(1,1,INO) =  C
        MATEVN(2,1,INO) =  S
        MATEVN(1,2,INO) = -S
        MATEVN(2,2,INO) =  C
C
  10  CONTINUE
C
C --- DETERMINATION DES MATRICES DE PASSAGE DES REPERES INTRINSEQUES
C --- AUX POINTS D'INTEGRATION DE L'ELEMENT AU REPERE UTILISATEUR :
C     ===========================================================
C
C --- ADRESSE DES MATRICES DE PASSAGE DU REPERE GLOBAL AUX REPERES
C --- INTRINSEQUES AUX POINTS D'INTEGRATION DE L'ELEMENT
C --- DANS LE TABLEAU .DESR :
C     ---------------------
      IDEC = 2000
C
C --- BOUCLE SUR LES POINTS D'INTEGRATION DE L'ELEMENT (SOUS-INTEGRE) :
C     --------------------------------------------------------------
      DO 40 IGAU = 1, NPGSN
C
C ---   RECUPERATION DE LA MATRICE DE PASSAGE AU POINT D'INTEGRATION
C ---   COURANT :
C       -------
        K = 0
        DO 50 J = 1, 3
          DO 60 I = 1, 3
            K  = K + 1
            PGL(I,J) = ZRLZR(IDEC+(IGAU-1)*9+K)
  60      CONTINUE
  50    CONTINUE
C
C ---   DETERMINATION DE LA PROJECTION DU VECTEUR X DU REPERE
C ---   UTILISATEUR SUR LE FEUILLET TANGENT A LA COQUE AU POINT
C ---   D'INTEGRATION COURANT :
C       ---------------------
        PS = DX*PGL(3,1) + DY*PGL(3,2) + DZ*PGL(3,3)
        PJDX = DX - PS*PGL(3,1)
        PJDY = DY - PS*PGL(3,2)
        PJDZ = DZ - PS*PGL(3,3)
        NORM = SQRT (PJDX*PJDX + PJDY*PJDY + PJDZ*PJDZ)
        IF ( NORM .LE. R8PREM() ) THEN
          CALL U2MESS('F','ELEMENTS_49')
        ENDIF
C
        PJDX = PJDX/NORM
        PJDY = PJDY/NORM
        PJDZ = PJDZ/NORM
C
        C = PJDX*PGL(1,1) + PJDY*PGL(1,2) + PJDZ*PGL(1,3)
        S = PJDX*PGL(2,1) + PJDY*PGL(2,2) + PJDZ*PGL(2,3)
C
        MATEVG(1,1,IGAU) =  C
        MATEVG(2,1,IGAU) =  S
        MATEVG(1,2,IGAU) = -S
        MATEVG(2,2,IGAU) =  C
C
  40  CONTINUE
C
C.============================ FIN DE LA ROUTINE ======================
      END
