      SUBROUTINE THMEVC(OPTION,NOMTE,AXI  ,NNO,NPG,IPOIDS,
     &                  IVF   ,IDFDE,NDDLS,
     &                  NNOS  ,NDDLM,NNOM  )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      LOGICAL      AXI
      INTEGER      NNO,NPG,IPOIDS,IVF,IDFDE
      INTEGER      NNOS,NDDLS,NNOM,NDDLM
      CHARACTER*16 OPTION,NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE MEUNIER S.MEUNIER
C =====================================================================
C    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
C                          ELEMENTS THHM, HM ET HH
C =====================================================================
C  THERMO-HYDRO-MECANIQUE - CALCUL DES EVOL_CHAR
C  *      *     *                      **   *
C =====================================================================
C OPTION    NOM DE L'OPTION
C NOMTE     NOM DE L'OPTION
C NNO       NOMBRE DE NOEUDS TOTAL SUR L'ELEMENT
C NPG       NOMBRE DE POINTS DE GAUSS
C IPOIDS    ADRESSE DU VECTEUR POIDS DES POINTS DE GAUSS
C IVF       FONCTIONS DE FORMES QUADRATIQUES
C IDFDE     ADRESSE DES DERIVEES DES FONCTIONS DE FORME
C NDDLS     NB DE DDL SUR LES SOMMETS
C NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT
C NDDLM     NB DE DDL SUR LES MILIEUX
C NNOM      NB DE NOEUDS MILIEUX DE L'ELEMENT
C =====================================================================
C
C
C
C
C =====================================================================
      REAL*8      POIDS
      INTEGER     I,IFORC,IGEOM,IVECTU,II,KP,K
      REAL*8      DFDBID(27),FX,FY,FZ
      REAL*8      RX
C =====================================================================
C --- 1. OPTION : CHAR_MECA_FR3D3D ------------------------------------
C =====================================================================
      IF (OPTION.EQ.'CHAR_MECA_FR3D3D') THEN
         CALL JEVECH('PGEOMER','L',IGEOM)
         CALL TEFREP(OPTION,NOMTE,'PFR3D3D',IFORC)
         CALL JEVECH('PVECTUR','E',IVECTU)

         DO 10 KP = 1,NPG
            K = (KP-1)*NNO
            CALL DFDM3D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDBID,DFDBID,
     &                                                     DFDBID,POIDS)

C      --- CALCUL DE LA FORCE AUX PG (A PARTIR DES NOEUDS) ---

            FX = 0.D0
            FY = 0.D0
            FZ = 0.D0

            DO 20 I=1,NNO
              II = 3 * (I-1)
              FX = FX + ZR(IVF+K+I-1) * ZR(IFORC+II  )
              FY = FY + ZR(IVF+K+I-1) * ZR(IFORC+II+1)
              FZ = FZ + ZR(IVF+K+I-1) * ZR(IFORC+II+2)
 20         CONTINUE

            DO 30 I = 1,NNOS
               II = NDDLS * (I-1)
               ZR(IVECTU+II) = ZR(IVECTU+II) +
     +                         POIDS * FX * ZR(IVF+K+I-1)
               ZR(IVECTU+II+1) = ZR(IVECTU+II+1) +
     +                         POIDS * FY * ZR(IVF+K+I-1)
               ZR(IVECTU+II+2) = ZR(IVECTU+II+2) +
     +                         POIDS * FZ * ZR(IVF+K+I-1)
 30         CONTINUE

            DO 40 I = 1,NNOM
               II = NNOS*NDDLS+NDDLM*(I-1)
               ZR(IVECTU+II)= ZR(IVECTU+II) +
     +                         POIDS * FX * ZR(IVF+K+I+NNOS-1)
               ZR(IVECTU+II+1)= ZR(IVECTU+II+1) +
     +                         POIDS * FY * ZR(IVF+K+I+NNOS-1)
               ZR(IVECTU+II+2)= ZR(IVECTU+II+2) +
     +                         POIDS * FZ * ZR(IVF+K+I+NNOS-1)
 40         CONTINUE
 10      CONTINUE
      END IF
C ======================================================================
C --- 2. OPTION : CHAR_MECA_FR2D2D -------------------------------------
C ======================================================================
      IF (OPTION.EQ.'CHAR_MECA_FR2D2D') THEN
         CALL JEVECH('PGEOMER','L',IGEOM)
         CALL TEFREP(OPTION,NOMTE,'PFR2D2D',IFORC)
         CALL JEVECH('PVECTUR','E',IVECTU)

         DO 100 KP = 1,NPG
            K = (KP-1)*NNO
            CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDBID,DFDBID,
     &                                                           POIDS)

C      --- CALCUL DE LA FORCE AUX PG (A PARTIR DES NOEUDS) ---
            FX = 0.D0
            FY = 0.D0
            DO 110 I=1,NNO
              II = 2 * (I-1)
              FX = FX + ZR(IVF+K+I-1) * ZR(IFORC+II  )
              FY = FY + ZR(IVF+K+I-1) * ZR(IFORC+II+1)
 110        CONTINUE

            IF (AXI) THEN
               RX = 0.D0
               DO 120 I = 1,NNO
                  RX = RX + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
 120           CONTINUE
               POIDS = POIDS*RX
            END IF

            DO 130 I = 1,NNOS
               II = NDDLS* (I-1)
               ZR(IVECTU+II) = ZR(IVECTU+II) +
     +                         POIDS * FX * ZR(IVF+K+I-1)
               ZR(IVECTU+II+1) = ZR(IVECTU+II+1) +
     +                         POIDS * FY * ZR(IVF+K+I-1)
 130        CONTINUE

            DO 140 I = 1,NNOM
               II = NNOS*NDDLS+NDDLM*(I-1)
               ZR(IVECTU+II)= ZR(IVECTU+II) +
     +                         POIDS * FX * ZR(IVF+K+I+NNOS-1)
               ZR(IVECTU+II+1)= ZR(IVECTU+II+1) +
     +                         POIDS * FY * ZR(IVF+K+I+NNOS-1)
 140        CONTINUE
 100     CONTINUE
      END IF

      END
