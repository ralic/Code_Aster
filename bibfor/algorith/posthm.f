      SUBROUTINE POSTHM(OPTION,MODINT,JGANO,NCMP,NVIM,VPG,VNO)
      IMPLICIT     NONE
      INTEGER      JGANO,NCMP,NVIM
      REAL*8       VNO(*),VPG(*)
      CHARACTER*3  MODINT
      CHARACTER*16 OPTION
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/08/2008   AUTEUR KHAM M.KHAM 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C --- ROUTINE DE POST-TRAITEMENT POUR LA THM --------------------------
C --- ON DEFINIT LES VECTEURS SUIVANT : -------------------------------
C =====================================================================
C --- * VPG1 DE DIMENSION NPGMAX*NVMAX - OU NVMAX EST LE --------------
C ---   NOMBRE MAXIMAL DE VARIABLE INTERNE (A CE JOUR : 20 : 16 POUR --
C ---   LA LOI CJS ET 4 POUR LES VINT LIEES A LA THM NON MECANIQUE) ---
C ---   NPGMAX ETANT LE NOMBRE DE POINTS DE GAUSS MAXIMAL -------------
C ---   (A CE JOUR : 8 : 8 POINTS DE GAUSS POUR LES HEXA20) -----------
C =====================================================================
C --- * VPG2 DE DIMENSION NNOSMA*NVMAX - OU ---------------------------
C ---   NNOSMA ETANT LE NOMBRE DE NOEUDS SOMMETS MAXIMAL --------------
C ---   (A CE JOUR : 8 : 8 SOMMETS POUR LES HEXA20) -------------------
C =====================================================================
C --- * SPG1 DE DIMENSION NPGMAX*DIMMAX - OU DIMMAX -------------------
C ---   EST LA DIMENSION MAXIMAL DE DIMCON (A CE JOUR : 31) -----------
C =====================================================================
C --- * SPG2 DE DIMENSION NNOSMA*DIMMAX -------------------------------
C =====================================================================
C --- * VARIPG ET VARISO DE DIMENSION NNOMAX*NVMAX - OU NNOMAX EST LE -
C ---   NOMBRE MAXIMAL DE NOEUDS (A CE JOUR : 20 : 20 SOMMETS POUR ----
C ---   LES HEXA20) ---------------------------------------------------
C =====================================================================
C --- * SIEFPG ET SIEFSO DE DIMENSION NNOMAX*DIMMAX -------------------
C =====================================================================
      INTEGER    I,J,IBID1,IBID2,IBID3,JGANPG,JGANSO
      INTEGER    NDIM,NNO,NNOS,NPG,NDIM2,NNO2,NNOS2,NPG2
      INTEGER    NVMAX,NPGMAX,NNOSMA,DIMMAX,NNOMAX
      PARAMETER (NVMAX  = 60)
      PARAMETER (NPGMAX = 8 )
      PARAMETER (NNOSMA = 8 )
      PARAMETER (DIMMAX = 31)
      PARAMETER (NNOMAX = 20)
      REAL*8     VPG1(NPGMAX*NVMAX),VPG2(NNOSMA*NVMAX)
      REAL*8     SPG1(NPGMAX*DIMMAX),SPG2(NNOSMA*DIMMAX)
      REAL*8     VARIPG(NNOMAX*NVMAX),VARISO(NNOMAX*NVMAX)
      REAL*8     SIEFPG(NNOMAX*DIMMAX),SIEFSO(NNOMAX*DIMMAX)
C =====================================================================
      IF (MODINT .NE. 'RED' ) THEN
         CALL PPGAN2(JGANO,NCMP,VPG,VNO)
      ELSE
C =====================================================================
C --- MATRICE DE PASSAGE POINTS DE GAUSS -> SOMMETS JGANPG ------------
C =====================================================================
         CALL ELREF4(' ','MASS',NDIM,NNO,NNOS,NPG,IBID1,IBID2,
     +                                                    IBID3,JGANPG)
C =====================================================================
C --- MATRICE DE PASSAGE SOMMETS -> SOMMETS : JGANSO ------------------
C =====================================================================
         CALL ELREF4(' ','NOEU_S',NDIM2,NNO2,NNOS2,NPG2,IBID1,IBID2,
     +                                                    IBID3,JGANSO)
C =====================================================================
C --- ON VERIFIE QUE LES DIMENSIONNEMENTS SONT A JOUR -----------------
C =====================================================================
         CALL ASSERT(NNO  .LE. NNOMAX)
         CALL ASSERT(NPG  .LE. NPGMAX)
         CALL ASSERT(NNOS .LE. NNOSMA)
         IF (OPTION .EQ. 'SIEF_ELNO_ELGA  ') THEN
C =====================================================================
C --- ON VERIFIE QUE LES DIMENSIONNEMENTS SONT A JOUR -----------------
C =====================================================================
            CALL ASSERT(NCMP .LE. DIMMAX)
            DO 100 I=1,NCMP*NPG
               SPG1(I) = VPG(I)
 100        CONTINUE
            DO 200 I=1,NCMP*NPG2
               SPG2(I) = VPG(NCMP*NPG+I)
 200        CONTINUE
            CALL PPGAN2(JGANPG,NCMP,SPG1,SIEFPG)
            CALL PPGAN2(JGANSO,NCMP,SPG2,SIEFSO)
            DO 10 I=1,NNO
               DO 20 J=1,NVIM
                  VNO((I-1)*NCMP+J) = SIEFPG((I-1)*NCMP+J)
 20            CONTINUE
               DO 30 J=NVIM+1,NCMP
                  VNO((I-1)*NCMP+J) = SIEFSO((I-1)*NCMP+J)
 30            CONTINUE
 10         CONTINUE
         ENDIF
         IF (OPTION .EQ. 'VARI_ELNO_ELGA  ') THEN
C =====================================================================
C --- ON VERIFIE QUE LES DIMENSIONNEMENTS SONT A JOUR -----------------
C =====================================================================
            CALL ASSERT(NCMP .LE. NVMAX)
            DO 300 I=1,NCMP*NPG
               VPG1(I) = VPG(I)
 300        CONTINUE
            DO 400 I=1,NCMP*NPG2
               VPG2(I) = VPG(NCMP*NPG+I)
 400        CONTINUE
            CALL PPGAN2(JGANPG,NCMP,VPG1,VARIPG)
            CALL PPGAN2(JGANSO,NCMP,VPG2,VARISO)
            DO 40 I=1,NNO
               DO 50 J=1,NVIM
                  VNO((I-1)*NCMP+J) = VARIPG((I-1)*NCMP+J)
 50            CONTINUE
               DO 60 J=NVIM+1,NCMP
                  VNO((I-1)*NCMP+J) = VARISO((I-1)*NCMP+J)
 60            CONTINUE
 40         CONTINUE
         ENDIF
      ENDIF
C =====================================================================
      END
