      SUBROUTINE I2GCCL (DEBCCL,TVOIS1,TVOIS2,TPLACE,SCHM,ACHM,PTS,PTA)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C
C******************************************************************
C
C         REPERAGE DANS LE GROUPE DE MAILLE D' UN CHEMIN
C         SIMPLE CONNAISSANT SA MAILLE DE DEPART.
C
C         DEBCCL (IN)      : MAILLE DE DEPART
C
C         TVOISI (IN)      : TABLES DES VOISINS
C
C         TPLACE (IN-OUT)  : TABLE DES MAILLES DEJA PLACEES
C
C         SCHM   (OUT)     : TABLE DE STRUCTURATION DES CHEMINS
C
C         ACHM   (OUT)     : TABLE D 'ACCES A SCHM
C
C         PTS    (IN-OUT)  : POINTEUR SUR SCHM
C
C         PTA    (IN-OUT)  : POINTEUR SUR ACHM
C
C******************************************************************
C
      LOGICAL TPLACE(*)
      INTEGER DEBCCL,TVOIS1(*),TVOIS2(*)
      INTEGER SCHM(*),ACHM(*),PTS,PTA
C
      LOGICAL FINI
C
      INTEGER S,S1,S2
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      FINI = .FALSE.
C
      S1 = 0
C
      S2 = 0
C
      S = DEBCCL
C
      SCHM(PTS) = S
      ACHM(PTA) = PTS
C
      TPLACE(S) = .TRUE.
C
      PTS = PTS + 1
      PTA = PTA + 1
C
10    CONTINUE
      IF ( .NOT. FINI ) THEN
C
         S1 = TVOIS1(S)
         S2 = TVOIS2(S)
C
         IF ( .NOT. TPLACE(S1) ) THEN
C
            S = S1
C
            TPLACE(S) = .TRUE.
C
         ELSE IF ( .NOT. TPLACE(S2) ) THEN
C
            S = S2
C
            TPLACE(S) = .TRUE.
C
         ELSE
C
            S = SCHM(ACHM(PTA-1))
C
            FINI = .TRUE.
C
         ENDIF
C
         SCHM(PTS) = S
C
         PTS = PTS + 1
C
         GOTO 10
C
      ENDIF
C
      END
